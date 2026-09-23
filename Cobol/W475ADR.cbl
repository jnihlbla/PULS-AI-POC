000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W475ADR.                                                 
000400 AUTHOR.         JAN-ERIK FRANTZEN.                                       
000500 DATE-WRITTEN.   APRIL 1994                                               
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        PROGRAMMET ÄR ETT SUBPROGRAM TILL W4053900 OCH                   
001100*        SKRIVER BLANKETT ADR FÖR FARLIGT GODS MED HJÄLP                  
001200*        AV PRINTPROGRAM W006PRR1.                                        
001300*                                                                         
001400*        PROGRAMMET LÄSER      WL4513 (WDR4)                              
001500*        PROGRAMMET LÄSER      WLGMTA (WDB2)                              
001700*        PROGRAMMET LÄSER              WDE6                               
001800*        PROGRAMMET LÄSER      WL1165 (WDR2 HTYP 1165/1168)               
001900*                                                                         
002000*    UTDATA.                                                              
002100*        BLANKETTER:  ADR/ADR-S                                           
002200*                                                                         
002210*    IMPORTANT.                                                           
002220*    4633590 - SEND SEAL,UN NUM TO FLS/MIC                                
002230*    IF PSN IS ADDED TO GODK-PSN,IT IS IMPORTANT TO ADD IN                
002240*    GOOD-PSN-ADR IN W4768000.                                            
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600 DATA DIVISION.                                                           
002700     EJECT                                                                
002800 WORKING-STORAGE SECTION.                                                 
002900                                                                          
003000*    -- CHECKED BY WY2000                                                 
003100 77  IDPGM                       PIC X(08)   VALUE 'W475ADR '.            
003200 77  WS-ADRESS-DP                PIC X(50)                                
003300         VALUE 'CARPARTS.DAP.DISTRDOC'.                                   
003400 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
003500 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
003600 77  KDRC-DISPLAY                PIC Z(5).                                
003700 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
003800 77  WZ04-001-IDCOM              PIC S9(9)   COMP VALUE +0.               
003900 77  JA                          PIC X       VALUE 'J'.                   
004000 77  NEJ                         PIC X       VALUE 'N'.                   
004100 77  IX                          PIC S9(9)   VALUE +0   COMP SYNC.        
004200 77  IX1                         PIC S9(9)   VALUE +0   COMP SYNC.        
004300 77  IX2                         PIC S9(9)   VALUE +0   COMP SYNC.        
004400 77  IX4                         PIC S9(3)   VALUE +0   COMP-3.           
004500 77  PSN-IX                      PIC S9(9)   VALUE +0   COMP SYNC.        
004600 77  TAB2-IX                     PIC S9(9)   VALUE +0   COMP SYNC.        
004700 77  TAB2-PSN-IX                 PIC S9(9)   VALUE +0   COMP SYNC.        
004800 77  TAB3-IX                     PIC S9(9)   VALUE +0   COMP SYNC.        
004900 77  TAB3-PSN-IX                 PIC S9(9)   VALUE +0   COMP SYNC.        
005000 77  KDKOLLI-IX                  PIC S9(9)   VALUE +0   COMP SYNC.        
005100 77  SPAR-IX                     PIC S9(9)   VALUE +0   COMP SYNC.        
005200 77  MAX-IX                      PIC S9(9)   VALUE +999 COMP SYNC.        
005300 77  RAD-IX                      PIC S9(9)   VALUE +0   COMP SYNC.        
005400 77  MAX-RAD-IX                  PIC S9(9)   VALUE +18  COMP SYNC.        
005500 77  WS-IDPSN-IX                 PIC S9(3)   VALUE +0   COMP-3.           
005600 77  WS-ANTAL-PSN                PIC S9(3)   VALUE +0   COMP-3.           
005700 77  WS-ANTAL-KOLLIN-TOT         PIC S9(5)   VALUE +0   COMP-3.           
005800 77  WS-VKORDBTO-TOT             PIC S9(6)V9(1) VALUE +0 COMP-3.          
005900 77  WS-KDKOLLI-KOLLI            PIC X(7)    VALUE SPACE.                 
006000 77  WS-KDKOLLI-TAB              PIC X(7)    VALUE SPACE.                 
006100 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006200 77  WS-SIDA-TOT                 PIC S9(3)   VALUE +0  COMP-3.            
006300 77  WS-RADER                    PIC S9(3)   VALUE +0  COMP-3.            
006402 77  WS-AKT-IDSPRAK              PIC X(2)    VALUE SPACE.                 
006500                                                                          
006600 77  W-KDFORMS                   PIC X       VALUE SPACE.                 
006700                                                                          
006800 01  WS-DATUM-AAMMDD             PIC 9(6).                                
006900 01  FILLER REDEFINES WS-DATUM-AAMMDD.                                    
007000     03  WS-AA                   PIC 9(2).                                
007100     03  WS-MM                   PIC 9(2).                                
007200     03  WS-DD                   PIC 9(2).                                
007300                                                                          
007400 01  WS-DATUM-DDMMAA             PIC 9(6).                                
007500 01  FILLER REDEFINES WS-DATUM-DDMMAA.                                    
007600     03  WS-MM-NA                PIC 9(2).                                
007700     03  WS-DD-NA                PIC 9(2).                                
007800     03  WS-AA-NA                PIC 9(2).                                
007900                                                                          
008000 01  WS-ORT-DATUM.                                                        
008100     03   ADR-SIGN-ORT            PIC X(20) VALUE SPACE.                  
008200     03   ADR-SIGN-DATUM          PIC X(6).                               
008300                                                                          
008400*01  FILLER   -COPY WWDC99                                                
008500     EJECT                                                                
008600 01  WS-VLFG                     PIC 9(4)V9(3).                           
008700 01  FILLER REDEFINES WS-VLFG.                                            
008800     03  WS-VLFG-1-4             PIC 9(4).                                
008900     03  WS-VLFG-5               PIC 9.                                   
009000     03  WS-VLFG-6-7             PIC 99.                                  
009100                                                                          
009200 01  GENERELLA-SUBPROGRAM.                                                
009300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009400     03  W006PRR1                PIC X(8)    VALUE 'W006PRR1'.            
009500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
009700     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
009800     03  WL10WBDC                PIC X(8)    VALUE 'WL10WBDC'.            
009900                                                                          
010000*    --- PARAMETERS FOR SUBPROGRAM WL10WBDC                               
010100                                                                          
010200 01  FILLER                      PIC X(16)  VALUE 'WL10WBDC AREA'.        
010300*01  -COPY WL10WBDC                                                       
010400                                                                          
010500 77  RKOD-ABEND-MED-DUMP        PIC S9(4)   VALUE +33 COMP SYNC.          
010600                                                                          
010700     EJECT                                                                
010800 01  TEST-IDDISTR                 PIC 9(5)   COMP-3.                      
010900*01  FILLER   -COPY WWDIST66    -RED TEST-IDDISTR.                        
011000                                                                          
011100 01  FILLER                       PIC X(16)  VALUE 'WWLNDSPR   '.         
011200*01  -COPY WWLNDSPR                                                       
011300                                                                          
011402*    --- tabell emb.benämningar per typ och språk                         
011501 01  FILLER                       PIC X(16)  VALUE 'W476FEMB   '.         
011601*01  -COPY W476FEMB                                                       
011701                                                                          
011801*    --- POSTBESKRIVNINGAR TILL D&P                                       
011901 01  DOC-HEAD-AREA.                                                       
012001*    03 -COPY W476FG1  -PRE HUVUD-                                        
012101 01  DOC-LINE-AREA.                                                       
012201*    03 -COPY W476FG2  -PRE RAD-                                          
012301 01  DOC-FOOT-AREA.                                                       
012401*    03 -COPY W476FG3  -PRE FOT-                                          
012501                                                                          
012601 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
012701*01  -COPY WZ01SEND                                                       
012801     EJECT                                                                
012901 01  PRINTAREA-START             PIC X(24)   VALUE                        
013001                                 'PRINTAREA-START'.                       
013101 01  HDR-AREA.                                                            
013201*    03  -COPY WZ01REQU  -PRE HDR-                                        
013301*    03  -COPY WZ04HDR                                                    
013401                                                                          
013501                                                                          
013601                                                                          
013701     EJECT                                                                
013801*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013901*                                                                         
014001 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014101                                                                          
014201 01  NYCKLAR-TILL-DLI.                                                    
014301     03  W-4513-X.                                                        
014401         05  W-IDHTYP-4513       PIC X(4)    VALUE '4513'.                
014501         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
014601                                                                          
014701     03  W-DASKEPPN-X.                                                    
014801         05  W-DASKEPPN          PIC 9(8)    VALUE ZERO.                  
014901                                                                          
015001     03  W-4516-X.                                                        
015101         05  W-IDDC-4516         PIC X(2)    VALUE SPACE.                 
015201         05  W-IDSKEPPN-4516     PIC S9(7)   VALUE ZERO COMP-3.           
015301         05  W-IDDISTR-4516      PIC S9(5)   VALUE ZERO COMP-3.           
015401         05  W-IDKUNDNR-4516     PIC S9(7)   VALUE ZERO COMP-3.           
015501                                                                          
015601     03  W-IDGMT-X.                                                       
015701         05  W-IDDISTR-WDB2      PIC S9(5)   VALUE ZERO COMP-3.           
015801         05  W-IDKUNDNR-WDB2     PIC S9(7)   VALUE ZERO COMP-3.           
015901                                                                          
016301     03  W-WDGXKEY-X.                                                     
016401         05  W-IDHTYP            PIC X(4)    VALUE '1165'.                
016501         05  W-IDPSN             PIC 9(3).                                
016601         05  W-IDSPRAK           PIC X(2).                                
016701         05  W-LOW-VALUE         PIC X(21)   VALUE LOW-VALUE.             
016801                                                                          
016901     03  W-KDFGTRP-X.                                                     
017001         05  W-KDFGTRP           PIC 9(2)    VALUE 0.                     
017101                                                                          
017201     03    W-IDPRODNR-X.                                                  
017301         05    W-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
017401                                                                          
017501     03    W-IDKOLLI-X.                                                   
017601         05    W-IDKOLLI         PIC S9(5)   VALUE ZERO  COMP-3.          
017701                                                                          
017801*--- Till WDB6                                                            
017901                                                                          
018001     03  W-IDDC-B6-X.                                                     
018101         05 W-IDDC-B6                  PIC X(2).                          
018201                                                                          
018301     EJECT                                                                
018401*    --- PRINTPARAMETRAR (PRINTNING MED ÅTERSTART)                        
018501*01  FILLER   -COPY W006PRAR                                              
018601     EJECT                                                                
018701                                                                          
018801 01  PRT-AREA.                                                            
018901     03 WS-PRT-IDPRTLST          PIC X(8)  VALUE SPACE.                   
019001     03 WS-PRT-IDLIST.                                                    
019101        05 WS-IDLIST             PIC X(4)  VALUE 'ADR '.                  
019201        05 WS-PRT-IDDISTR        PIC 9(4).                                
019301        05 WS-PRT-KDFRAKT        PIC 9(2).                                
019401     03 WS-WEB-IDLIST.                                                    
019501        05 WS-WEB-IDLIST         PIC X(3)  VALUE 'ADR'.                   
019601        05 WS-WEB-IDDISTR        PIC 9(4).                                
019701        05 WS-WEB-IDTRPTNR       PIC 9(3).                                
019801     03 WS-PRT-LISTRAD.                                                   
019901        05 FILLER                PIC X(2)  VALUE SPACE.                   
020001        05 WS-RAD                PIC X(78).                               
020101     03 WS-PRT-DUMMY             PIC X(1).                                
020201                                                                          
020301 01  FILLER           PIC X(16)   VALUE 'BLK-TAB1-OMIXAD'.                
020401*    --- BLANKETT-TABELL                                                  
020501 01  BLANKETT1-DATA-TABELL.                                               
020601    03   TABELL1-DATA OCCURS 1000.                                        
020701      05 TAB1-IDPSN                   PIC S9(3).                          
020801      05 TABELL1-IDPSN-DATA OCCURS 4.                                     
020901        07 TAB1-KDKOLLI               PIC X(8).                           
021001        07 TAB1-VLFG                  PIC S9(4)V9(3).                     
021101        07 TAB1-VKORDBTO              PIC S9(6)V9(1).                     
021201        07 TAB1-VKART-FG              PIC S9(7).                          
021301        07 TAB1-ANTAL-KOLLIN          PIC  9(3).                          
021401*                                                                         
021501 01  FILLER           PIC X(16)   VALUE 'BLK-TAB2-MIXAD '.                
021601 01  BLANKETT2-DATA-TABELL.                                               
021701    03   TABELL2-DATA OCCURS 100.                                         
021801      05 TAB2-KDKOLLI                 pic X(8).                           
021901      05 TAB2-ANTAL-PSN               PIC 9(2).                           
022001      05 TAB2-VKORDBTO                PIC S9(6)V9(1).                     
022101      05 TAB2-VKART-FG                PIC S9(7).                          
022201      05 TAB2-ANTAL-KOLLIN            PIC  9(3).                          
022301      05 TAB2-VLFG                    PIC S9(4)V9(3).                     
022401      05 TABELL2-IDPSN OCCURS 9.                                          
022501        07 TAB2-IDPSN                 PIC S9(3).                          
022601*                                                                         
022701 01  FILLER           PIC X(16)   VALUE 'BLK-TAB3-RESTEN'.                
022801 01  BLANKETT3-DATA-TABELL.                                               
022901    03   TABELL3-DATA OCCURS 100.                                         
023001      05 TAB3-KDKOLLI                 PIC X(8).                           
023101      05 TAB3-VKORDBTO                PIC S9(6)V9(1).                     
023201      05 TAB3-VKART-FG                PIC S9(7).                          
023301      05 TAB3-ANTAL-KOLLIN            PIC  9(3).                          
023401      05 TAB3-VLFG                    PIC S9(4)V9(3).                     
023501      05 TABELL3-IDPSN OCCURS 9.                                          
023601        07 TAB3-IDPSN                 PIC S9(3).                          
023701*                                                                         
023801     EJECT                                                                
023901                                                                          
024001 01  FILLER           PIC X(16)   VALUE 'RADER          '.                
024101                                                                          
024201 01  RAD0.                                                                
024301     03   FILLER                  PIC X(4)  VALUE SPACE.                  
024401     03   FILLER                  PIC X(24) VALUE                         
024501                                  '*** BOKNINGSUNDERLAG ***'.             
024601 01  RAD1.                                                                
024701     03   FILLER                  PIC X(4)  VALUE SPACE.                  
024801     03   ADR-BEGMTRAD-1          PIC X(35).                              
024901                                                                          
025001 01  RAD2.                                                                
025101     03   FILLER                  PIC X(4)  VALUE SPACE.                  
025201     03   ADR-BEGMTRAD-2          PIC X(35).                              
025301                                                                          
025401 01  RAD3.                                                                
025501     03   FILLER                  PIC X(4)  VALUE SPACE.                  
025601     03   ADR-ADGMTRAD-1          PIC X(35).                              
025701                                                                          
025801 01  RAD4.                                                                
025901     03   FILLER                  PIC X(4)  VALUE SPACE.                  
026001     03   ADR-ADGMTRAD-2          PIC X(35).                              
026101                                                                          
026201 01  RAD4A.                                                               
026301     03   FILLER                  PIC X(4)  VALUE SPACE.                  
026401     03   ADR-LAGMTRAD            PIC X(35).                              
026501                                                                          
026601 01  RAD5.                                                                
026701     03   FILLER                  PIC X(4)  VALUE SPACE.                  
026801     03   ADR-BEPSN               PIC X(75).                              
026901                                                                          
027001 01  RAD6.                                                                
027101     03   FILLER                  PIC X(4)  VALUE SPACE.                  
027201     03   ADR-ANTAL-KOLLIN        PIC Z(2)9.                              
027301     03   FILLER                  PIC X(1)  VALUE SPACE.                  
027401     03   ADR-KDKOLLI-TXT         PIC X(26).                              
027501     03   FILLER                  PIC X(9)  VALUE SPACE.                  
027601     03   ADR-NETTO-TEXT          PIC X(21) VALUE                         
027701                                  'nettovikt/net weight '.                
027801     03   ADR-VLFG                PIC Z(3)9.9.                            
027901     03   ADR-TEXT-2              PIC X(3)  VALUE SPACE.                  
028001                                                                          
028101 01  RAD7.                                                                
028201     03   FILLER                  PIC X(4)  VALUE SPACE.                  
028301     03   ADR-TEXT-3              PIC X(4)  VALUE SPACE.                  
028401     03   ADR-VKART-FG            PIC Z(6)9.                              
028501     03   ADR-TEXT-4              PIC X(6)  VALUE SPACE.                  
028601                                                                          
028701 01  RAD8.                                                                
028801     03   FILLER                  PIC X(4)  VALUE SPACE.                  
028901     03   ADR-BRUTTO-TEXT         PIC X(24) VALUE                         
029001                                  'Bruttovikt/gross weight '.             
029101     03   ADR-VKORDBTO            PIC Z(5)9.9.                            
029201     03   ADR-TEXT-5              PIC X(3)  VALUE SPACE.                  
029301                                                                          
029401     EJECT                                                                
029402***  OBS  ***  OM MAN LÄGGER TILL GODK-PSN-er (10 TOM 99)                 
029403*              MÅSTE MAN KOLLA ATT DE FINNS UPPLAGDA PÅ                   
029404*              ALLA SPRÅKEN!                                              
029405*              KOLLA PÅ BILD 1132                                         
029406*              IDPSN + SPRÅK (SE, DE, FR, ES, IT, PL, NL, FI)             
029420***  OBS  *** OBS  ********************************************           
029501 01  GODK-PSN-SW                  PIC 9(3).                               
029601     88  GODK-PSN                           VALUE 010                     
029701                                                  022 024                 
029801                                                  030                     
029901                                                  053                     
030001                                                  060                     
030101                                                  080 088 089             
030201                                                  090 091 097.            
030301*                                                                         
030401*GODK-PSN för Puls-artiklar, fom 010 tom 099.                             
030402*                                                                         
030701 01  GODK-KDKOLLI-SW              PIC X(8).                               
030801     88  FIBREBOARDBOX                       VALUE '1       '             
030901                                                   '27      '             
031001                                                   '0402    '             
031101                                                   '0403    '             
031201                                                   '0408    '             
031301                                                   '2207    '             
031401                                                   '2209    '             
031501                                                   '2231    '             
031601                                                   '2629    '             
031701                                                   '2631    '             
031801                                                   '2636    '             
031901                                                   '2639    '             
032001                                                   '3278    '             
032101                                                   '4454    '             
032201                                                   '4457    '             
032301                                                   '4460    '             
032401                                                   '4464    '             
032501                                                   '4506    '             
032601                                                   '4526    '             
032701                                                   '4548    '             
032801                                                   '5513    '             
032901                                                   '5543    '             
033001                                                   '5586    '             
033101                                                   '6111    '             
033201                                                   '6120    '             
033301                                                   '6255    '             
033401                                                   '6444    '             
033501                                                   '6451    '             
033601                                                   '7135    '             
033701                                                   '7136    '             
033801                                                   '7137    '             
033901                                                   '7138    '             
034001                                                   '7139    '             
034101                                                   '8789    '             
034201                                                   '9702    '             
034301                                                   '9703    '             
034401                                                   '9778    '             
034501                                                   'SP02    '             
034601                                                   'SP10    '.            
034701                                                                          
034801     88  PLYWOODBOX                          VALUE '1130    '             
034901                                                   '1131    '             
035001                                                   '1183    '             
035101                                                   '1635    '.            
035201                                                                          
035301     88  PLASTICDRUM                         VALUE 'PFAT    '.            
035401                                                                          
035501     88  STEELDRUM                           VALUE 'SFAT    '.            
035601                                                                          
035701     88  OVERPACK                            VALUE 'L1      '             
035801                                                   'L2      '             
035901                                                   'L3      '             
036001                                                   'L4      '             
036101                                                   'L5      '             
036201                                                   'L6      '             
036202                                                   'L34     '             
036203                                                   'L35     '             
036301                                                   'HL1     '             
036401                                                   'HL2     '             
036501                                                   'HL3     '             
036601                                                   'HL4     '             
036701                                                   'HL5     '             
036801                                                   'HL6     '             
036901                                                   'HL7     '             
037001                                                   'HL8     '             
037101                                                   '9057    '             
037201                                                   '9058    '             
037301                                                   '9066    '             
037401                                                   '9067    '             
037501                                                   '9068    '             
037601                                                   '9101    '             
037701                                                   '9102    '             
037801                                                   '9103    '             
037901                                                   '9104    '.            
038001                                                                          
038101 01  FORTSAETTNING-SW             PIC X.                                  
038201     88  FORTSAETTNING                       VALUE 'J'.                   
038301                                                                          
038401 01  RAKNA-UPP-KOLLI-SW           PIC X.                                  
038501     88  RAKNA-UPP-KOLLI                     VALUE 'J'.                   
038601                                                                          
038701 01  NY-SIDA-SW                   PIC X.                                  
038801     88  NY-SIDA-FINNS                       VALUE 'J'.                   
038901                                                                          
039001 01  TAB1-DATA-SW                 PIC X.                                  
039101     88  TAB1-DATA-FINNS                     VALUE 'J'.                   
039201                                                                          
039301 01  MIXAD-PSN-SW                 PIC X.                                  
039401     88  MIXAD-PSN                           VALUE 'J'.                   
039501     88  OMIXAD-PSN                          VALUE 'N'.                   
039601                                                                          
039701 01  PSN-MATCH-SW                 PIC X.                                  
039801     88  PSN-MATCH                           VALUE 'J'.                   
039901                                                                          
040001 01  KDKOLLI-SW                   PIC X.                                  
040101     88  KDKOLLI-OK                          VALUE 'J'.                   
040201                                                                          
040301 01  PSN-OK-SW                    PIC X.                                  
040401     88  PSN-OK                              VALUE 'J'.                   
040501                                                                          
040601 01  SKRIV-EQ-VARDE-SW            PIC X.                                  
040701     88  SKRIV-EQ-VARDE                      VALUE 'J'.                   
040801     SKIP2                                                                
040901                                                                          
041001     EJECT                                                                
041101 01  FILLER           PIC X(16)   VALUE 'IMS-WS         '.                
041201                                                                          
041301*    --- STATUS-KOD FRÅN IMS                                              
041401 01  STATUS-WS                    PIC XX.                                 
041501     88  SEGMENT-FINNS                       VALUE '  '.                  
041601     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
041701     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
041801     88  SEGMENT-SLUT                        VALUE 'GB'.                  
041901                                                                          
042001 01  GODK-STATUSKODER.                                                    
042101     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
042201                                                                          
042301                                                                          
042401 01  ALL-SSA.                                                             
042501     03 SSA1                     PIC X(96).                               
042601     03 SSA2                     PIC X(96).                               
042701                                                                          
042801     EJECT                                                                
042901*    --- IMS FUNKTIONSKODER                                               
043001*01  -COPY W0003                                                          
043101     EJECT                                                                
043201*    ---  DLI INPUT-OUTPUT AREA                                           
043301 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
043401     SKIP3                                                                
043501 01  DLI-IO-AREA.                                                         
043601     03  IO-AREA                 PIC X(600)  VALUE SPACE.                 
043701     SKIP3                                                                
043801     03  WDE611   REDEFINES IO-AREA.                                      
043901*        05  -COPY WDE611                                                 
044001     SKIP3                                                                
044101     03  WL116512 REDEFINES IO-AREA.                                      
044201*        05  -COPY WDGX1168  -PRE 1165-                                   
044301     EJECT                                                                
044701 01  DLI-IO-AREA-WDB2.                                                    
044801     03  WLGMTA01.                                                        
044901*        05  -COPY WDB201                                                 
045001     EJECT                                                                
045101 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-4514'.         
045201 01  DLI-IO-AREA-4514.                                                    
045301     03  WL451311.                                                        
045401*        05  -COPY WDGX4514                                               
045501     SKIP3                                                                
045601 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-4516'.         
045701     SKIP3                                                                
045801 01  DLI-IO-AREA-4516.                                                    
045901     03  WL451321.                                                        
046001*        05  -COPY WDGX4516                                               
046101     SKIP3                                                                
046201 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-B601'.          
046301 01  DLI-IO-AREA-B601.                                                    
046401     03  WDB601.                                                          
046501*        05  -COPY WDB601                                                 
046601     EJECT                                                                
046701 LINKAGE SECTION.                                                         
046801                                                                          
046901*                                                                         
047001*   -COPY W475ADR -PRE LINK-                                              
047101*                                                                         
047201     EJECT                                                                
047301*01  -COPY W0009   -PRE ALT-                                              
047401     EJECT                                                                
047801*01  -COPY W0008  -PRE LISB-                                              
047901     05  FILLER                  PIC X.                                   
048001                                                                          
048101*01  -COPY W0008  -PRE 4513-                                              
048201     05  FILLER                  PIC X.                                   
048301     EJECT                                                                
048401*01  -COPY W0008  -PRE 1165-                                              
048501     05  FILLER                  PIC X.                                   
048601                                                                          
048701*01  -COPY W0008  -PRE GMTA-                                              
048801     05  FILLER                  PIC X.                                   
048901     EJECT                                                                
049301*01  -COPY W0008  -PRE WDE6-                                              
049401     05  FILLER                  PIC X.                                   
049501     EJECT                                                                
049601*01  -COPY W0008  -PRE WDB6-                                              
049701     05  FILLER                  PIC X.                                   
049801     EJECT                                                                
049901 PROCEDURE DIVISION  USING LINK-W475ADR ALT-PCB                           
050001                                       LISB-PCB 4513-PCB                  
050101                                       1165-PCB GMTA-PCB                  
050301                                       WDE6-PCB                           
050401                                       WDB6-PCB.                          
050501 MAIN SECTION.                                                            
050601     PERFORM A-INIT                                                       
050701     PERFORM B-LAES-DATA                                                  
050801     IF FORTSAETTNING                                                     
050901        IF WBDC-FLWEBDC = JA                                              
051001           PERFORM C-SKRIV-BLANKETT-WEB                                   
051101        ELSE                                                              
051201           PERFORM D-SKRIV-BLANKETT                                       
051301        END-IF                                                            
051401     END-IF                                                               
051501                                                                          
051601     MOVE ZERO                     TO RETURN-CODE                         
051701     GOBACK                                                               
051801     .                                                                    
051901     EJECT                                                                
052001 A-INIT SECTION.                                                          
052101     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
052201                                                                          
052301     MOVE LINK-IDDISTR             TO W-IDDISTR-4516                      
052401                                      W-IDDISTR-WDB2                      
052501                                                                          
052601     MOVE LINK-IDKUNDNR            TO W-IDKUNDNR-4516                     
052701                                      W-IDKUNDNR-WDB2                     
052801                                                                          
052901     MOVE LINK-IDDC                TO W-IDDC-4516                         
053001                                      WS-IDDC                             
053101                                      W-IDDC-B6                           
053201                                                                          
053301     MOVE LINK-IDSKEPPN            TO W-IDSKEPPN-4516                     
053401                                                                          
053501     MOVE LINK-DASKEPPN            TO W-DASKEPPN                          
053601                                                                          
053701     MOVE NEJ                      TO TAB1-DATA-SW                        
053801     MOVE +0                       TO WS-ANTAL-KOLLIN-TOT                 
053901                                      WS-VKORDBTO-TOT                     
054001                                      WS-VLFG                             
054101     PERFORM AA-NOLLA-TAB1                                                
054201     PERFORM AB-NOLLA-TAB2                                                
054301     PERFORM AC-NOLLA-TAB3                                                
054401     PERFORM AD-KOLLA-WEB-DC                                              
054501                                                                          
054601     ACCEPT DAGENS-DATUM FROM DATE                                        
054701     MOVE DAGENS-DATUM             TO ADR-SIGN-DATUM                      
054801                                                                          
054901     IF NDC-NA                                                            
055001       MOVE DAGENS-DATUM           TO WS-DATUM-AAMMDD                     
055101       MOVE WS-AA                  TO WS-AA-NA                            
055201       MOVE WS-MM                  TO WS-MM-NA                            
055301       MOVE WS-DD                  TO WS-DD-NA                            
055401       MOVE WS-DATUM-DDMMAA        TO ADR-SIGN-DATUM                      
055501     END-IF                                                               
055601     .                                                                    
055701     EJECT                                                                
055801 AA-NOLLA-TAB1 SECTION.                                                   
055901                                                                          
056001     MOVE +1                       TO IX                                  
056101     PERFORM UNTIL IX > MAX-IX                                            
056201        MOVE ZERO                  TO TAB1-IDPSN (IX)                     
056301        MOVE +1                    TO IX2                                 
056401        PERFORM UNTIL IX2 > 4                                             
056501          MOVE SPACE               TO TAB1-KDKOLLI (IX, IX2)              
056601          MOVE ZERO                TO TAB1-VLFG (IX, IX2)                 
056701          MOVE ZERO                TO TAB1-VKORDBTO (IX, IX2)             
056801          MOVE ZERO                TO TAB1-VKART-FG (IX, IX2)             
056901          MOVE ZERO                TO TAB1-ANTAL-KOLLIN (IX, IX2)         
057001          ADD +1                   TO IX2                                 
057101        END-PERFORM                                                       
057201                                                                          
057301        ADD +1                     TO IX                                  
057401     END-PERFORM                                                          
057501     .                                                                    
057601     EJECT                                                                
057701 AB-NOLLA-TAB2 SECTION.                                                   
057801                                                                          
057901     MOVE +1                       TO IX                                  
058001     PERFORM UNTIL IX > 100                                               
058101        MOVE SPACE                 TO TAB2-KDKOLLI (IX)                   
058201        MOVE ZERO                  TO TAB2-VLFG (IX)                      
058301        MOVE ZERO                  TO TAB2-VKORDBTO (IX)                  
058401        MOVE ZERO                  TO TAB2-VKART-FG (IX)                  
058501        MOVE ZERO                  TO TAB2-ANTAL-KOLLIN (IX)              
058601        MOVE ZERO                  TO TAB2-ANTAL-PSN (IX)                 
058701                                                                          
058801        MOVE +1                    TO IX2                                 
058901        PERFORM UNTIL IX2 > 9                                             
059001          MOVE ZERO                TO TAB2-IDPSN (IX, IX2)                
059101          ADD +1                   TO IX2                                 
059201        END-PERFORM                                                       
059301                                                                          
059401        ADD +1                     TO IX                                  
059501     END-PERFORM                                                          
059601     .                                                                    
059701     EJECT                                                                
059801 AC-NOLLA-TAB3 SECTION.                                                   
059901                                                                          
060001     MOVE +1                       TO IX                                  
060101     PERFORM UNTIL IX > 100                                               
060201        MOVE SPACE                 TO TAB3-KDKOLLI (IX)                   
060301        MOVE ZERO                  TO TAB3-VLFG (IX)                      
060401        MOVE ZERO                  TO TAB3-VKORDBTO (IX)                  
060501        MOVE ZERO                  TO TAB3-VKART-FG (IX)                  
060601        MOVE ZERO                  TO TAB3-ANTAL-KOLLIN (IX)              
060701                                                                          
060801        MOVE +1                    TO IX2                                 
060901        PERFORM UNTIL IX2 > 9                                             
061001          MOVE ZERO                TO TAB3-IDPSN (IX, IX2)                
061101          ADD +1                   TO IX2                                 
061201        END-PERFORM                                                       
061301                                                                          
061401        ADD +1                     TO IX                                  
061501     END-PERFORM                                                          
061601     .                                                                    
061701                                                                          
061801     EJECT                                                                
061901 AD-KOLLA-WEB-DC SECTION.                                                 
062001                                                                          
062101     MOVE LINK-IDDC       TO WBDC-IDDC                                    
062201     CALL WL10WBDC USING WBDC-AREA                                        
062301     .                                                                    
062401                                                                          
062501                                                                          
062601 B-LAES-DATA SECTION.                                                     
062701     MOVE 'B-LAES-DATA     ' TO CURRENT-SECTION                           
062801                                                                          
062901     PERFORM BA-LAES-SKEPP-KOLLI                                          
063001                                                                          
063101     IF FORTSAETTNING                                                     
063201        PERFORM BB-LAES-KUNDREG                                           
063301     END-IF                                                               
063401     .                                                                    
063501     EJECT                                                                
063601 BA-LAES-SKEPP-KOLLI SECTION.                                             
063701     MOVE 'BA-LAES-SKEPP   ' TO CURRENT-SECTION                           
063801                                                                          
063901     MOVE +0                  TO WS-ANTAL-KOLLIN-TOT                      
064001     MOVE NEJ                 TO FORTSAETTNING-SW                         
064101                                                                          
064201     PERFORM IMS-GU-WL451311                                              
064301                                                                          
064401     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
064501       PERFORM IMS-GNP-WL451321                                           
064601                                                                          
064701       IF SEGMENT-FINNS                                                   
064801         MOVE 4516-IDPRODNR TO W-IDPRODNR                                 
064901         MOVE 4516-IDKOLLI TO W-IDKOLLI                                   
065001         PERFORM IMS-GET-WDE6                                             
065101                                                                          
065201         PERFORM BAA-KOLLA-KDKOLLI-GODK                                   
065301                                                                          
065401         IF FIBREBOARDBOX OR PLYWOODBOX OR                                
065501            PLASTICDRUM   OR STEELDRUM                                    
065601           PERFORM BAB-KOLLA-OM-MIXADE-PSN                                
065701                                                                          
065801           IF OMIXAD-PSN                                                  
065901             PERFORM S20-FLYTTA-TILL-OMIXAD-TAB                           
066001           ELSE                                                           
066101             PERFORM S21-FLYTTA-TILL-MIXAD-TAB                            
066201           END-IF                                                         
066301                                                                          
066401         ELSE                                                             
066501           PERFORM S22-FLYTTA-TILL-EJ-GODK-TAB                            
066601         END-IF                                                           
066701                                                                          
066801       END-IF                                                             
066901     END-PERFORM                                                          
067001     .                                                                    
067101     EJECT                                                                
067201 BAA-KOLLA-KDKOLLI-GODK SECTION.                                          
067301     MOVE 'BAA-KOLLA-      ' TO CURRENT-SECTION                           
067401                                                                          
067501     MOVE KOLLI-KDKOLLI TO GODK-KDKOLLI-SW                                
067601                                                                          
067701     IF FIBREBOARDBOX OR PLYWOODBOX OR                                    
067801        PLASTICDRUM   OR STEELDRUM  OR                                    
067901        OVERPACK                                                          
068001       CONTINUE                                                           
068101                                                                          
068201     ELSE                                                                 
068301       MOVE '1       ' TO KOLLI-KDKOLLI                                   
068401                                                                          
068501     END-IF                                                               
068601                                                                          
068701     IF KOLLI-IDPSN(1) = 53                                               
068801       MOVE 'SFAT    ' TO KOLLI-KDKOLLI                                   
068901                                                                          
069001     ELSE                                                                 
069101       IF KOLLI-IDPSN(1) = 60                                             
069201         MOVE 'PFAT    ' TO KOLLI-KDKOLLI                                 
069301       END-IF                                                             
069401                                                                          
069501     END-IF                                                               
069601                                                                          
069701     MOVE KOLLI-KDKOLLI TO GODK-KDKOLLI-SW                                
069801     .                                                                    
069901     EJECT                                                                
070001 BAB-KOLLA-OM-MIXADE-PSN SECTION.                                         
070101     MOVE 'BAB-KOLLA-      ' TO CURRENT-SECTION                           
070201                                                                          
070301     MOVE +1 TO IX                                                        
070401     MOVE +0 TO WS-ANTAL-PSN                                              
070501     MOVE NEJ TO MIXAD-PSN-SW                                             
070601                                                                          
070701     PERFORM UNTIL IX > 9                                                 
070801                                                                          
070901       IF KOLLI-IDPSN (IX) > ZERO                                         
071001         MOVE KOLLI-IDPSN (IX) TO GODK-PSN-SW                             
071101                                                                          
071201         IF GODK-PSN                                                      
071301           ADD +1 TO WS-ANTAL-PSN                                         
071401         END-IF                                                           
071501       END-IF                                                             
071601                                                                          
071701       ADD +1 TO IX                                                       
071801     END-PERFORM                                                          
071901                                                                          
072001     IF WS-ANTAL-PSN > 1                                                  
072101       MOVE JA TO MIXAD-PSN-SW                                            
072201     END-IF                                                               
072301     .                                                                    
072401     EJECT                                                                
072501 BB-LAES-KUNDREG SECTION.                                                 
072601     MOVE 'BB-LAES-KUNDREG ' TO CURRENT-SECTION                           
072701                                                                          
072801     PERFORM IMS-GET-GMTA                                                 
072901                                                                          
073001     MOVE GMT-BEGMT-RAD1           TO ADR-BEGMTRAD-1                      
073101     MOVE GMT-BEGMT-RAD2           TO ADR-BEGMTRAD-2                      
073201     MOVE GMT-ADGMT-GATA           TO ADR-ADGMTRAD-1                      
073301     MOVE GMT-ADGMT-PADR           TO ADR-ADGMTRAD-2                      
073401     MOVE GMT-ADGMT-LAND           TO ADR-LAGMTRAD                        
073501     .                                                                    
073601                                                                          
073701                                                                          
073801 C-SKRIV-BLANKETT-WEB SECTION.                                            
073901     MOVE 'C-SKRIV-BL-WEB  ' TO CURRENT-SECTION                           
074001                                                                          
074101     PERFORM CF-DECIDE-WEB-FORMS                                          
074201                                                                          
074301     IF TAB1-DATA-FINNS                                                   
074401        MOVE +0                    TO WS-VKORDBTO-TOT                     
074501        PERFORM CA-SKRIV-HUVUD-WEB                                        
074601        PERFORM CB-REDIGERA-BLK-OMIXAD                                    
074701        PERFORM CE-SKRIV-AVSLUTNING                                       
074801                                                                          
074901        MOVE WZ04-001-IDCOM        TO SEND-IDCOM                          
075001        PERFORM S95-SEND-CLOSE                                            
075101     END-IF                                                               
075201                                                                          
075301     IF TAB2-KDKOLLI(1) > SPACE                                           
075401        MOVE +0                    TO WS-VKORDBTO-TOT                     
075501        PERFORM CA-SKRIV-HUVUD-WEB                                        
075601        PERFORM CC-REDIGERA-BLK-MIXAD                                     
075701        PERFORM CE-SKRIV-AVSLUTNING                                       
075801                                                                          
075901        MOVE WZ04-001-IDCOM        TO SEND-IDCOM                          
076001        PERFORM S95-SEND-CLOSE                                            
076101     END-IF                                                               
076201                                                                          
076301     IF  TAB3-ANTAL-KOLLIN(1) > ZERO                                      
076401     AND TAB3-IDPSN(1, 1) > ZERO                                          
076501        MOVE +1                    TO IX                                  
076601        MOVE +0                    TO WS-VKORDBTO-TOT                     
076701        PERFORM CA-SKRIV-HUVUD-WEB                                        
076801        PERFORM CD-REDIGERA-BLK-EJ-GODK                                   
076901        PERFORM CE-SKRIV-AVSLUTNING                                       
077001                                                                          
077101        MOVE WZ04-001-IDCOM        TO SEND-IDCOM                          
077201        PERFORM S95-SEND-CLOSE                                            
077301     END-IF                                                               
077401                                                                          
077501     .                                                                    
077601                                                                          
077701 CA-SKRIV-HUVUD-WEB     SECTION.                                          
077801     MOVE 'CA-SKRIV-HUVUD  ' TO CURRENT-SECTION                           
077901                                                                          
078001     MOVE 1                 TO HDR-REQU-IDMSGVER                          
078101     MOVE 'R'               TO HDR-REQU-KDPGMACT                          
078201     MOVE SPACE             TO HDR-REQU-IDUSER                            
078301                                                                          
078401     MOVE SPACE             TO HDR-IDOUTREC                               
078501                               HDR-IDLIST                                 
078601     MOVE 'DANGEROUS-GOODS' TO HDR-IDOUTTYPE                              
078701     MOVE LINK-IDDC         TO HDR-IDOUTREC(1:2)                          
078801     MOVE SPACE             TO HDR-IDOUTREC(3:8)                          
078901     MOVE 'ADR'             TO HDR-IDLIST(1:3)                            
079001     MOVE LINK-IDDISTR      TO WS-WEB-IDDISTR                             
079101     MOVE WS-WEB-IDDISTR    TO HDR-IDLIST(4:4)                            
079201     MOVE LINK-IDTRPTNR     TO WS-WEB-IDTRPTNR                            
079301     MOVE WS-WEB-IDTRPTNR   TO HDR-IDLIST(8:3)                            
079401     PERFORM S90-SEND-OPEN                                                
079501                                                                          
079601     MOVE SEND-IDCOM       TO WZ04-001-IDCOM                              
079701     PERFORM S91-PUT-DOC-HDR                                              
079801                                                                          
079901                                                                          
080001     MOVE '1'              TO HUVUD-IDAFPRCD                              
080101     MOVE SPACE            TO HUVUD-IDFRASED                              
080201     MOVE ZERO             TO HUVUD-KVPAGE                                
080301     MOVE ZERO             TO HUVUD-IDORDNR7                              
080401     MOVE ADR-BEGMTRAD-1   TO HUVUD-BEGMT-RAD1                            
080501     MOVE ADR-BEGMTRAD-2   TO HUVUD-BEGMT-RAD2                            
080601     MOVE ADR-ADGMTRAD-1   TO HUVUD-ADGMT-GATA                            
080701     MOVE ADR-ADGMTRAD-2   TO HUVUD-ADGMT-PADR                            
080801     MOVE ADR-LAGMTRAD     TO HUVUD-ADGMT-LAND                            
080901     MOVE SPACE            TO HUVUD-AIRPORT                               
081001                                                                          
081101     PERFORM IMS-GU-WDB601                                                
081201                                                                          
081301     IF SEGMENT-FINNS                                                     
081401       MOVE DCS-ADGMT-PADR(11:20)    TO ADR-SIGN-ORT                      
081502       PERFORM CAA-HITTA-SPRAAK                                           
081503       MOVE DCS-BEGMT-RAD1        TO HUVUD-DCS-BEGMT-RAD1                 
081504       MOVE DCS-BEGMT-RAD2        TO HUVUD-DCS-BEGMT-RAD2                 
081505     ELSE                                                                 
081506       MOVE SPACE                 TO HUVUD-DCS-BEGMT-RAD1                 
081507                                     HUVUD-DCS-BEGMT-RAD2                 
081601     END-IF                                                               
081701                                                                          
081801     IF NDC-NA                                                            
081901       IF NDC-US-RU                                                       
082001         MOVE 'Rutherford, NJ      ' TO ADR-SIGN-ORT                      
082101       END-IF                                                             
082201                                                                          
082701       IF NDC-US-LA                                                       
082801         MOVE 'Carson, CA          ' TO ADR-SIGN-ORT                      
082901       END-IF                                                             
083001                                                                          
083101       IF NDC-CA                                                          
083201         MOVE 'Missisauga, Ontario ' TO ADR-SIGN-ORT                      
083301       END-IF                                                             
083401                                                                          
083501       IF NDC-JP                                                          
083601         MOVE 'Nagoya              ' TO ADR-SIGN-ORT                      
083701       END-IF                                                             
083801                                                                          
083901       IF NDC-AU                                                          
084001         MOVE 'Minto               ' TO ADR-SIGN-ORT                      
084101       END-IF                                                             
084201                                                                          
084301     END-IF                                                               
084401                                                                          
084501     MOVE ADR-SIGN-ORT               TO HUVUD-SIGN-ORT                    
084601     MOVE ADR-SIGN-DATUM             TO HUVUD-SIGN-DATUM                  
084701                                                                          
084801     MOVE W-KDFORMS                  TO HUVUD-KDFORMS                     
084901                                                                          
085001                                                                          
085101     PERFORM S92-PUT-DOC-HEAD                                             
085201     .                                                                    
085301     EJECT                                                                
085402                                                                          
085502 CAA-HITTA-SPRAAK SECTION.                                                
085602                                                                          
085702     SEARCH ALL WWLNDSPR-RAD                                              
085802        AT END                                                            
085902           MOVE 'SE'        TO WS-AKT-IDSPRAK                             
086002        WHEN WWLNDSPR-IDLANDX2(SPR-IX) = DCS-IDLANDX2                     
086102           MOVE WWLNDSPR-IDSPRAK(SPR-IX)                                  
086202                             TO WS-AKT-IDSPRAK                            
086302     END-SEARCH                                                           
086403     .                                                                    
086503     EJECT                                                                
086603                                                                          
086703 CB-REDIGERA-BLK-OMIXAD SECTION.                                          
086802                                                                          
086902     MOVE 'CB-RED-OMIXAD   ' TO CURRENT-SECTION                           
087002                                                                          
087102     MOVE '2'   TO RAD-IDAFPRCD                                           
087202                                                                          
087302     MOVE +1    TO IX                                                     
087402     MOVE +1    TO IX2                                                    
087502                                                                          
087602     PERFORM UNTIL IX > 99                                                
087702        IF TAB1-IDPSN (IX) > 0                                            
087802           MOVE TAB1-IDPSN (IX)    TO GODK-PSN-SW                         
087902           IF GODK-PSN                                                    
088002                                                                          
088102             PERFORM UNTIL IX2 > 4                                        
088202               IF TAB1-KDKOLLI (IX, IX2) > SPACE                          
088302                 PERFORM CBA-SKRIV-KOLLI-RADER                            
088402               END-IF                                                     
088502               ADD +1 TO IX2                                              
088602             END-PERFORM                                                  
088702           END-IF                                                         
088802        END-IF                                                            
088902        ADD  +1                    TO IX                                  
089002        MOVE +1                    TO IX2                                 
089102     END-PERFORM                                                          
089202                                                                          
089302     MOVE SPACE            TO RAD-FG-RAD                                  
089402     PERFORM S93-PUT-DOC-LINE                                             
089502     .                                                                    
089602                                                                          
089702                                                                          
089802 CBA-SKRIV-KOLLI-RADER SECTION.                                           
089902     MOVE 'CBA-SKRIV-KOLLI ' TO CURRENT-SECTION                           
090002                                                                          
090102     MOVE SPACE                    TO RAD5                                
090202     MOVE TAB1-IDPSN (IX)          TO W-IDPSN                             
090304     MOVE WS-AKT-IDSPRAK           TO W-IDSPRAK                           
090404                                                                          
090504     PERFORM IMS-GET-1165-WDR2                                            
090604     MOVE 1165-1168-BEPSN (1)      TO ADR-BEPSN                           
090704     MOVE RAD5                     TO RAD-FG-RAD                          
091000     PERFORM S93-PUT-DOC-LINE                                             
092000                                                                          
093000     MOVE 1165-1168-BEPSN (2)      TO ADR-BEPSN                           
094000     MOVE RAD5                     TO RAD-FG-RAD                          
094100     PERFORM S93-PUT-DOC-LINE                                             
094200                                                                          
094300     IF TAB1-IDPSN (IX) = 10 OR 11                                        
094400        MOVE SPACE                 TO RAD7                                
094500        MOVE 'NET '                TO ADR-TEXT-3                          
094600        MOVE TAB1-VKART-FG (IX, IX2) TO ADR-VKART-FG                      
094700        MOVE ' GRAMS'              TO ADR-TEXT-4                          
094800                                                                          
094900        MOVE RAD7                  TO RAD-FG-RAD                          
095000        PERFORM S93-PUT-DOC-LINE                                          
095100                                                                          
095200     END-IF                                                               
095300                                                                          
095400     MOVE TAB1-ANTAL-KOLLIN (IX, IX2) TO ADR-ANTAL-KOLLIN                 
095500                                                                          
095600     MOVE TAB1-KDKOLLI (IX, IX2)     TO GODK-KDKOLLI-SW                   
095705                                                                          
095805     IF TAB1-ANTAL-KOLLIN (IX, IX2) > 1                                   
095905*** Flera kollin. Hämta benämningar från pluralis tabellen                
096005       IF FIBREBOARDBOX                                                   
096105         SEARCH ALL FEMB-FRAD2                                            
096202            AT END                                                        
096305               MOVE 'Papplådor/Fibreboard Boxes'                          
096405                                TO ADR-KDKOLLI-TXT                        
096505            WHEN IDSPRAK-F2(F2-IX) = WS-AKT-IDSPRAK                       
096605               MOVE TEEMB-F2(F2-IX)                                       
096702                                TO ADR-KDKOLLI-TXT                        
096802         END-SEARCH                                                       
097800       END-IF                                                             
098000                                                                          
098305       IF PLYWOODBOX                                                      
098505         MOVE 'Plywoodlådor/Plywood Boxes' TO ADR-KDKOLLI-TXT             
098805       END-IF                                                             
099005                                                                          
099105       IF PLASTICDRUM                                                     
099205         SEARCH ALL FEMB-PRAD2                                            
099305            AT END                                                        
099405               MOVE 'Plastfat/Plastic Drums    '                          
099505                                TO ADR-KDKOLLI-TXT                        
099605            WHEN IDSPRAK-P2(P2-IX) = WS-AKT-IDSPRAK                       
099705               MOVE TEEMB-P2(P2-IX)                                       
099805                                TO ADR-KDKOLLI-TXT                        
099905         END-SEARCH                                                       
100105       END-IF                                                             
100205                                                                          
100305       IF STEELDRUM                                                       
100405         SEARCH ALL FEMB-SRAD2                                            
100505            AT END                                                        
100605               MOVE 'Metallfat/Steel Drums     '                          
100705                                TO ADR-KDKOLLI-TXT                        
100805            WHEN IDSPRAK-S2(S2-IX) = WS-AKT-IDSPRAK                       
100905               MOVE TEEMB-S2(S2-IX)                                       
101005                                TO ADR-KDKOLLI-TXT                        
101105         END-SEARCH                                                       
101305       END-IF                                                             
101405     ELSE                                                                 
101506*** Ett kolli. Hämta benämningar från singularis tabellen                 
101605       IF FIBREBOARDBOX                                                   
101705         SEARCH ALL FEMB-FRAD1                                            
101805            AT END                                                        
101905               MOVE 'Papplåda/Fibreboard Box   '                          
102005                                TO ADR-KDKOLLI-TXT                        
102105            WHEN IDSPRAK-F1(F1-IX) = WS-AKT-IDSPRAK                       
102205               MOVE TEEMB-F1(F1-IX)                                       
102305                                TO ADR-KDKOLLI-TXT                        
102405         END-SEARCH                                                       
102505       END-IF                                                             
102605                                                                          
102705       IF PLYWOODBOX                                                      
102805         MOVE 'Plywoodlåda/Plywood Box   ' TO ADR-KDKOLLI-TXT             
102905       END-IF                                                             
103005                                                                          
103105       IF PLASTICDRUM                                                     
103205         SEARCH ALL FEMB-PRAD1                                            
103305            AT END                                                        
103405               MOVE 'Plastfat/Plastic Drum     '                          
103505                                TO ADR-KDKOLLI-TXT                        
103605            WHEN IDSPRAK-P1(P1-IX) = WS-AKT-IDSPRAK                       
103705               MOVE TEEMB-P1(P1-IX)                                       
103805                                TO ADR-KDKOLLI-TXT                        
103905         END-SEARCH                                                       
104005       END-IF                                                             
104105                                                                          
104205       IF STEELDRUM                                                       
104305         SEARCH ALL FEMB-SRAD1                                            
104405            AT END                                                        
104505               MOVE 'Metallfat/Steel Drum      '                          
104605                                TO ADR-KDKOLLI-TXT                        
104705            WHEN IDSPRAK-S1(S1-IX) = WS-AKT-IDSPRAK                       
104805               MOVE TEEMB-S1(S1-IX)                                       
104905                                TO ADR-KDKOLLI-TXT                        
105005         END-SEARCH                                                       
105205       END-IF                                                             
105305     END-IF                                                               
105405                                                                          
105505     MOVE TAB1-VLFG (IX, IX2)      TO WS-VLFG                             
105605     IF WS-VLFG-6-7 = 0                                                   
105705        CONTINUE                                                          
105805     ELSE                                                                 
105905        COMPUTE WS-VLFG = WS-VLFG + 0.1                                   
106005     END-IF                                                               
106105                                                                          
106205     MOVE WS-VLFG                  TO ADR-VLFG                            
106305                                                                          
106405     COMPUTE WS-VKORDBTO-TOT = WS-VKORDBTO-TOT +                          
106505                                      TAB1-VKORDBTO (IX, IX2)             
106605     MOVE ' KG'                    TO ADR-TEXT-2                          
106705                                      ADR-TEXT-5                          
106805     IF SDC-NL                                                            
106905       MOVE 'net gew/net weight   ' TO ADR-NETTO-TEXT                     
107005     END-IF                                                               
107105                                                                          
107205     IF TAB1-IDPSN (IX) = 30 OR 53 OR 60                                  
107305       MOVE 'nettovol/net volume  ' TO ADR-NETTO-TEXT                     
107405       MOVE '  L'                  TO ADR-TEXT-2                          
107505*                                                                         
107605*    fix CO/april 2014                                                    
107705*    IDPSN 53 och 60 är beteckning för fg. vätska som transp.             
107805*    i fat av plast eller metall. I dagens läge kan man inte              
107905*    beräkna nettovolymen på annat sätt än att man drar av                
108005*    10 från bruttovikten...                                              
108105*    Då är nettovikten (volym i detta fall) alltid < bruttovikten         
108205*                                                                         
108305       IF TAB1-IDPSN (IX) = 53 OR 60                                      
108405         IF (TAB1-VLFG (IX, IX2) = ZERO) OR                               
108505            (TAB1-VLFG (IX, IX2) > TAB1-VKORDBTO (IX, IX2))               
108605           COMPUTE WS-VLFG = TAB1-VKORDBTO (IX, IX2) - 10                 
108705           MOVE WS-VLFG            TO ADR-VLFG                            
108805         END-IF                                                           
108905       END-IF                                                             
109005     END-IF                                                               
109105                                                                          
109205     MOVE RAD6                     TO RAD-FG-RAD                          
109305     PERFORM S93-PUT-DOC-LINE                                             
109405                                                                          
109505     MOVE SPACE                    TO RAD-FG-RAD                          
109605     PERFORM S93-PUT-DOC-LINE                                             
109705     .                                                                    
109805                                                                          
109905                                                                          
110005 CC-REDIGERA-BLK-MIXAD SECTION.                                           
110105     MOVE 'CC-RED-MIXAD    ' TO CURRENT-SECTION                           
110205                                                                          
110305     MOVE '2'    TO RAD-IDAFPRCD                                          
110405                                                                          
110505     MOVE +1     TO IX                                                    
110605     MOVE +1     TO IX2                                                   
110705                                                                          
110805     PERFORM UNTIL IX > 99                                                
110905        IF TAB2-KDKOLLI (IX) > SPACE                                      
111005           PERFORM UNTIL IX2 > 9                                          
111105             IF TAB2-IDPSN (IX, IX2 ) > 0                                 
111205               MOVE TAB2-IDPSN (IX, IX2) TO GODK-PSN-SW                   
111305               IF GODK-PSN                                                
111405                                                                          
111505                  PERFORM CCA-SKRIV-PSN-RADER                             
111605               END-IF                                                     
111705             END-IF                                                       
111805             ADD +1 TO IX2                                                
111905           END-PERFORM                                                    
112005           PERFORM CCB-SKRIV-KOLLI-RADER                                  
112105        END-IF                                                            
112205        ADD  +1                    TO IX                                  
112305        MOVE +1                    TO IX2                                 
112405     END-PERFORM                                                          
112505     .                                                                    
112605                                                                          
112705                                                                          
112805                                                                          
112905 CCA-SKRIV-PSN-RADER SECTION.                                             
113005     MOVE 'CCA-SKRIV-      ' TO CURRENT-SECTION                           
113105                                                                          
113205                                                                          
113305     MOVE SPACE                    TO RAD5                                
113405     MOVE TAB2-IDPSN (IX, IX2)     TO W-IDPSN                             
113505     MOVE WS-AKT-IDSPRAK           TO W-IDSPRAK                           
113605                                                                          
113705     PERFORM IMS-GET-1165-WDR2                                            
113805                                                                          
113905     MOVE 1165-1168-BEPSN (1)      TO ADR-BEPSN                           
114005     MOVE RAD5                     TO RAD-FG-RAD                          
114105     PERFORM S93-PUT-DOC-LINE                                             
114205                                                                          
114305     MOVE 1165-1168-BEPSN (2)      TO ADR-BEPSN                           
114405     MOVE RAD5                     TO RAD-FG-RAD                          
114505     PERFORM S93-PUT-DOC-LINE                                             
114605     .                                                                    
114705                                                                          
114805                                                                          
114905                                                                          
115005 CCB-SKRIV-KOLLI-RADER SECTION.                                           
115105     MOVE 'CCB-SKRIV-      ' TO CURRENT-SECTION                           
115205                                                                          
115305     MOVE TAB2-ANTAL-KOLLIN (IX) TO ADR-ANTAL-KOLLIN                      
115405                                                                          
115505     MOVE TAB2-KDKOLLI (IX)     TO GODK-KDKOLLI-SW                        
115506     IF TAB2-ANTAL-KOLLIN (IX) > 1                                        
115507*** Flera kollin. Hämta benämningar från pluralis tabellen                
116306       IF FIBREBOARDBOX                                                   
116307         SEARCH ALL FEMB-FRAD2                                            
116308            AT END                                                        
116309               MOVE 'Papplådor/Fibreboard Boxes'                          
116310                                TO ADR-KDKOLLI-TXT                        
116320            WHEN IDSPRAK-F2(F2-IX) = WS-AKT-IDSPRAK                       
116330               MOVE TEEMB-F2(F2-IX)                                       
116340                                TO ADR-KDKOLLI-TXT                        
116350         END-SEARCH                                                       
116360       END-IF                                                             
116361                                                                          
116370       IF PLYWOODBOX                                                      
116380         MOVE 'Plywoodlådor/Plywood Boxes' TO ADR-KDKOLLI-TXT             
116390       END-IF                                                             
116400                                                                          
116500       IF PLASTICDRUM                                                     
116600         SEARCH ALL FEMB-PRAD2                                            
116700            AT END                                                        
116800               MOVE 'Plastfat/Plastic Drums    '                          
116900                                TO ADR-KDKOLLI-TXT                        
117000            WHEN IDSPRAK-P2(P2-IX) = WS-AKT-IDSPRAK                       
117100               MOVE TEEMB-P2(P2-IX)                                       
117200                                TO ADR-KDKOLLI-TXT                        
117201         END-SEARCH                                                       
117202       END-IF                                                             
117203                                                                          
117204       IF STEELDRUM                                                       
117205         SEARCH ALL FEMB-SRAD2                                            
117206            AT END                                                        
117207               MOVE 'Metallfat/Steel Drums     '                          
117208                                TO ADR-KDKOLLI-TXT                        
117209            WHEN IDSPRAK-S2(S2-IX) = WS-AKT-IDSPRAK                       
117210               MOVE TEEMB-S2(S2-IX)                                       
117220                                TO ADR-KDKOLLI-TXT                        
117230         END-SEARCH                                                       
117240       END-IF                                                             
117250     ELSE                                                                 
117260*** Ett kolli. Hämta benämningar från singularis tabellen                 
117270       IF FIBREBOARDBOX                                                   
117280         SEARCH ALL FEMB-FRAD1                                            
117290            AT END                                                        
117300               MOVE 'Papplåda/Fibreboard Box   '                          
117400                                TO ADR-KDKOLLI-TXT                        
117500            WHEN IDSPRAK-F1(F1-IX) = WS-AKT-IDSPRAK                       
117600               MOVE TEEMB-F1(F1-IX)                                       
117700                                TO ADR-KDKOLLI-TXT                        
117800         END-SEARCH                                                       
117900       END-IF                                                             
118000                                                                          
118100       IF PLYWOODBOX                                                      
118200         MOVE 'Plywoodlåda/Plywood Box   ' TO ADR-KDKOLLI-TXT             
118300       END-IF                                                             
118400                                                                          
118500       IF PLASTICDRUM                                                     
118600         SEARCH ALL FEMB-PRAD1                                            
118700            AT END                                                        
118701               MOVE 'Plastfat/Plastic Drum     '                          
118702                                TO ADR-KDKOLLI-TXT                        
118703            WHEN IDSPRAK-P1(P1-IX) = WS-AKT-IDSPRAK                       
118704               MOVE TEEMB-P1(P1-IX)                                       
118705                                TO ADR-KDKOLLI-TXT                        
118706         END-SEARCH                                                       
118707       END-IF                                                             
118708                                                                          
118709       IF STEELDRUM                                                       
118710         SEARCH ALL FEMB-SRAD1                                            
118711            AT END                                                        
118712               MOVE 'Metallfat/Steel Drum      '                          
118713                                TO ADR-KDKOLLI-TXT                        
118714            WHEN IDSPRAK-S1(S1-IX) = WS-AKT-IDSPRAK                       
118715               MOVE TEEMB-S1(S1-IX)                                       
118716                                TO ADR-KDKOLLI-TXT                        
118717         END-SEARCH                                                       
118718       END-IF                                                             
118719     END-IF                                                               
118720                                                                          
118805     MOVE TAB2-VLFG (IX)           TO WS-VLFG                             
118905     IF WS-VLFG-6-7 = 0                                                   
119005        CONTINUE                                                          
119105     ELSE                                                                 
119205        COMPUTE WS-VLFG = WS-VLFG + 0.1                                   
119305     END-IF                                                               
119405                                                                          
119505     MOVE WS-VLFG                  TO ADR-VLFG                            
119605                                                                          
119705     COMPUTE WS-VKORDBTO-TOT = WS-VKORDBTO-TOT +                          
119805                                      TAB2-VKORDBTO (IX)                  
119905     MOVE ' KG'                    TO ADR-TEXT-2                          
120005                                      ADR-TEXT-5                          
120105     IF SDC-NL                                                            
120205       MOVE 'net gew/net weight   ' TO ADR-NETTO-TEXT                     
120305     END-IF                                                               
120405                                                                          
120505     MOVE RAD6                     TO RAD-FG-RAD                          
120605     PERFORM S93-PUT-DOC-LINE                                             
120705                                                                          
120805     MOVE SPACE                    TO RAD-FG-RAD                          
120905     PERFORM S93-PUT-DOC-LINE                                             
121005     .                                                                    
121105                                                                          
121205                                                                          
121305                                                                          
121405 CD-REDIGERA-BLK-EJ-GODK SECTION.                                         
121505     MOVE 'CD-RED-EJ-GODK  ' TO CURRENT-SECTION                           
121605                                                                          
121705     MOVE '2'     TO RAD-IDAFPRCD                                         
121805                                                                          
121905     MOVE +1      TO IX                                                   
122005     MOVE +1      TO IX2                                                  
122105                                                                          
122205     PERFORM UNTIL IX > 99                                                
122305                                                                          
122405       IF TAB3-ANTAL-KOLLIN (IX) > ZERO AND                               
122505          TAB3-IDPSN (IX, IX2)   > ZERO                                   
122605                                                                          
122705          PERFORM UNTIL IX2 > 9                                           
122805            IF TAB3-IDPSN (IX, IX2) > 0                                   
122905              MOVE TAB3-IDPSN (IX, IX2) TO GODK-PSN-SW                    
123005              IF GODK-PSN                                                 
123105                                                                          
123205                 PERFORM CDA-SKRIV-PSN-RADER                              
123305              END-IF                                                      
123405            END-IF                                                        
123505            ADD +1 TO IX2                                                 
123605          END-PERFORM                                                     
123705          PERFORM CDB-SKRIV-KOLLI-RADER                                   
123805       END-IF                                                             
123905       ADD  +1                    TO IX                                   
124005       MOVE +1                    TO IX2                                  
124105     END-PERFORM                                                          
124205                                                                          
124305     MOVE SPACE            TO RAD-FG-RAD                                  
124405     PERFORM S93-PUT-DOC-LINE                                             
124505     .                                                                    
124605                                                                          
124705                                                                          
124805 CDA-SKRIV-PSN-RADER SECTION.                                             
124905     MOVE 'CDA-SKRIV-      ' TO CURRENT-SECTION                           
125005                                                                          
125105                                                                          
125205     MOVE SPACE                    TO RAD5                                
125305     MOVE TAB3-IDPSN (IX, IX2)     TO W-IDPSN                             
125405     MOVE WS-AKT-IDSPRAK           TO W-IDSPRAK                           
125505                                                                          
125605     PERFORM IMS-GET-1165-WDR2                                            
125705                                                                          
125805     MOVE 1165-1168-BEPSN (1)      TO ADR-BEPSN                           
125905     MOVE RAD5                     TO RAD-FG-RAD                          
126005     PERFORM S93-PUT-DOC-LINE                                             
126105                                                                          
126205     MOVE 1165-1168-BEPSN (2)      TO ADR-BEPSN                           
126305     MOVE RAD5                     TO RAD-FG-RAD                          
126405     PERFORM S93-PUT-DOC-LINE                                             
126505     .                                                                    
126605                                                                          
126705                                                                          
126805                                                                          
126905 CDB-SKRIV-KOLLI-RADER SECTION.                                           
127005     MOVE 'CDB-SKRIV-      ' TO CURRENT-SECTION                           
127105                                                                          
127205     MOVE TAB3-ANTAL-KOLLIN (IX)   TO ADR-ANTAL-KOLLIN                    
127305                                                                          
127405     MOVE SPACE                    TO ADR-KDKOLLI-TXT                     
127505                                                                          
127605     MOVE TAB3-VLFG (IX)           TO WS-VLFG                             
127705     IF WS-VLFG-6-7 = 0                                                   
127805        CONTINUE                                                          
127905     ELSE                                                                 
128005        COMPUTE WS-VLFG = WS-VLFG + 0.1                                   
128105     END-IF                                                               
128205                                                                          
128305     MOVE WS-VLFG                  TO ADR-VLFG                            
128405                                                                          
128505     COMPUTE WS-VKORDBTO-TOT = WS-VKORDBTO-TOT +                          
128605                                      TAB3-VKORDBTO (IX)                  
128705     MOVE ' KG'                    TO ADR-TEXT-2                          
128805                                      ADR-TEXT-5                          
128905     IF SDC-NL                                                            
129005       MOVE 'net gew/net weight   ' TO ADR-NETTO-TEXT                     
129105     END-IF                                                               
129205                                                                          
129305     MOVE RAD6                     TO RAD-FG-RAD                          
129405     PERFORM S93-PUT-DOC-LINE                                             
129505                                                                          
129605     MOVE SPACE                    TO RAD-FG-RAD                          
129705     PERFORM S93-PUT-DOC-LINE                                             
129805     .                                                                    
129905                                                                          
130005                                                                          
130105                                                                          
130205 CE-SKRIV-AVSLUTNING SECTION.                                             
130305     MOVE 'CE-AVSLUTNING   ' TO CURRENT-SECTION                           
130405                                                                          
130505     MOVE '3'              TO RAD-IDAFPRCD                                
130605                                                                          
130705     IF SDC-NL                                                            
130805       MOVE 'bruto  gew/gr  weight   ' TO ADR-BRUTTO-TEXT                 
130905     ELSE                                                                 
131005       MOVE 'Gross weight            ' TO ADR-BRUTTO-TEXT                 
131105     END-IF                                                               
131205     MOVE WS-VKORDBTO-TOT              TO ADR-VKORDBTO                    
131305     MOVE RAD8                         TO RAD-FG-RAD                      
131405     PERFORM S93-PUT-DOC-LINE                                             
131505     .                                                                    
131605                                                                          
131705     EJECT                                                                
131805 CF-DECIDE-WEB-FORMS   SECTION.                                           
131905                                                                          
132005     MOVE '1'        TO W-KDFORMS                                         
132105     MOVE 4          TO W-KDFGTRP                                         
132205                                                                          
132305     IF CDC-SE OR DDC-SE                                                  
132405       MOVE LINK-IDDISTR TO TEST-IDDISTR                                  
132505       IF DIST66-STYRN-REP AND LINK-IDPRT = 'REP'                         
132605         MOVE '6'    TO W-KDFORMS                                         
132705         MOVE 3      TO W-KDFGTRP                                         
132805       END-IF                                                             
132905*    ELSE                                                                 
133005*      IF SDC-NL                                                          
133105*        MOVE '?' TO W-KDFORMS                                            
133205*      END-IF                                                             
133305     END-IF                                                               
133405     .                                                                    
133505                                                                          
133605     EJECT                                                                
133705 D-SKRIV-BLANKETT SECTION.                                                
133805                                                                          
133905     PERFORM S01-OPPNA-PRINTER                                            
134005     PERFORM DA-SKAPA-PRINTER-ID                                          
134105                                                                          
134205     IF TAB1-DATA-FINNS                                                   
134305       MOVE NEJ                    TO NY-SIDA-SW                          
134405       MOVE +20                    TO RAD-IX                              
134505       MOVE +0                     TO WS-VKORDBTO-TOT                     
134605       MOVE +1                     TO IX                                  
134705                                                                          
134805       PERFORM UNTIL IX > 99                                              
134905          IF RAD-IX > MAX-RAD-IX                                          
135005             PERFORM DB-SKRIV-GMT-ADRESS                                  
135105          END-IF                                                          
135205          PERFORM DC-REDIGERA-BLK-OMIXAD                                  
135305       END-PERFORM                                                        
135405     END-IF                                                               
135505                                                                          
135605     IF TAB2-KDKOLLI (1) > SPACE                                          
135705       MOVE NEJ                    TO NY-SIDA-SW                          
135805       MOVE +20                    TO RAD-IX                              
135905       MOVE +0                     TO WS-VKORDBTO-TOT                     
136005       MOVE +1                     TO IX                                  
136105                                                                          
136205       PERFORM UNTIL IX > 99                                              
136305          IF RAD-IX > MAX-RAD-IX                                          
136405             PERFORM DB-SKRIV-GMT-ADRESS                                  
136505          END-IF                                                          
136605          PERFORM DD-REDIGERA-BLK-MIXAD                                   
136705       END-PERFORM                                                        
136805     END-IF                                                               
136905                                                                          
137005     IF TAB3-ANTAL-KOLLIN (1) > ZERO AND                                  
137105        TAB3-IDPSN (1, 1)     > ZERO                                      
137205       MOVE NEJ                    TO NY-SIDA-SW                          
137305       MOVE +20                    TO RAD-IX                              
137405       MOVE +0                     TO WS-VKORDBTO-TOT                     
137505       MOVE +1                     TO IX                                  
137605                                                                          
137705       PERFORM UNTIL IX > 99                                              
137805          IF RAD-IX > MAX-RAD-IX                                          
137905             PERFORM DB-SKRIV-GMT-ADRESS                                  
138005          END-IF                                                          
138105          PERFORM DE-REDIGERA-BLK-EJ-GODK                                 
138205       END-PERFORM                                                        
138305     END-IF                                                               
138405                                                                          
138505     PERFORM S10-STAENG-PRINTER                                           
138605     .                                                                    
138705     EJECT                                                                
138805 DA-SKAPA-PRINTER-ID SECTION.                                             
138905                                                                          
139005     MOVE 4                        TO W-KDFGTRP                           
139105                                                                          
139205     IF CDC-SE OR DDC-SE                                                  
139305       MOVE LINK-IDDISTR TO TEST-IDDISTR                                  
139405       IF DIST66-STYRN-REP AND LINK-IDPRT = 'REP'                         
139505         IF DIST66-PRINTER-EUROPA2                                        
139605           MOVE 'W40506E2'        TO WS-PRT-IDPRTLST                      
139705         ELSE                                                             
139805           IF DIST66-PRINTER-NORDEN                                       
139905             MOVE 'W40506N '      TO WS-PRT-IDPRTLST                      
140005           ELSE                                                           
140105             IF DIST66-PRINTER-OVERSEAS                                   
140205               MOVE 'W40506O '    TO WS-PRT-IDPRTLST                      
140305             ELSE                                                         
140405               IF DIST66-PRINTER-EUROPA3                                  
140505                 MOVE 'W40506E3'  TO WS-PRT-IDPRTLST                      
140605               ELSE                                                       
140705                 MOVE 'W40506E2'  TO WS-PRT-IDPRTLST                      
140805               END-IF                                                     
140905             END-IF                                                       
141005           END-IF                                                         
141105         END-IF                                                           
141205         MOVE 3                   TO W-KDFGTRP                            
141305       ELSE                                                               
141405         IF DIST66-PRINTER-EUROPA2                                        
141505           MOVE 'W40501E2'        TO WS-PRT-IDPRTLST                      
141605         ELSE                                                             
141705           IF DIST66-PRINTER-NORDEN                                       
141805             MOVE 'W40501N '      TO WS-PRT-IDPRTLST                      
141905           ELSE                                                           
142005             IF DIST66-PRINTER-OVERSEAS                                   
142105               MOVE 'W40501O '    TO WS-PRT-IDPRTLST                      
142205             ELSE                                                         
142305               IF DIST66-PRINTER-EUROPA3                                  
142405                 MOVE 'W40501E3'  TO WS-PRT-IDPRTLST                      
142505               ELSE                                                       
142605                 MOVE 'W40501E2'  TO WS-PRT-IDPRTLST                      
142705               END-IF                                                     
142805             END-IF                                                       
142905           END-IF                                                         
143005         END-IF                                                           
143105       END-IF                                                             
143205*    ELSE                                                                 
143305*      IF SDC-NL                                                          
143405*        MOVE 'W4051121'          TO WS-PRT-IDPRTLST                      
143505*      END-IF                                                             
143605     END-IF                                                               
143705                                                                          
143805     MOVE LINK-IDDISTR            TO WS-PRT-IDDISTR                       
143905     MOVE LINK-KDFRAKT            TO WS-PRT-KDFRAKT                       
144005                                                                          
144105     .                                                                    
144205     EJECT                                                                
144305 DB-SKRIV-GMT-ADRESS SECTION.                                             
144405                                                                          
144505     MOVE +0                       TO WS-VKORDBTO-TOT                     
144605                                                                          
144705*--- SKRIVER GMT NAMN OCH ADRESS PÅ RAD 12 - 16                           
144805                                                                          
144905     MOVE PRT-NYSIDA-RAD6          TO PRT-RADSKIP                         
145005     MOVE SPACE                    TO WS-RAD                              
145105     PERFORM S02-SKRIV                                                    
145205                                                                          
145305     IF LINK-IDSYSTEM = '4535' OR '4665' OR '4675'                        
145405        MOVE PRT-AFTER-6           TO PRT-RADSKIP                         
145505        MOVE RAD1                  TO WS-RAD                              
145605        PERFORM S02-SKRIV                                                 
145705     ELSE                                                                 
145805        MOVE PRT-AFTER-3           TO PRT-RADSKIP                         
145905        MOVE RAD0                  TO WS-RAD                              
146005        PERFORM S02-SKRIV                                                 
146105                                                                          
146205        MOVE PRT-AFTER-3           TO PRT-RADSKIP                         
146305        MOVE RAD1                  TO WS-RAD                              
146405        PERFORM S02-SKRIV                                                 
146505     END-IF                                                               
146605                                                                          
146705     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
146805     MOVE RAD2                     TO WS-RAD                              
146905     PERFORM S02-SKRIV                                                    
147005                                                                          
147105     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
147205     MOVE RAD3                     TO WS-RAD                              
147305     PERFORM S02-SKRIV                                                    
147405                                                                          
147505     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
147605     MOVE RAD4                     TO WS-RAD                              
147705     PERFORM S02-SKRIV                                                    
147805                                                                          
147905     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
148005     MOVE RAD4A                    TO WS-RAD                              
148105     PERFORM S02-SKRIV                                                    
148205                                                                          
148305     .                                                                    
148405     EJECT                                                                
148505 DC-REDIGERA-BLK-OMIXAD SECTION.                                          
148605                                                                          
148705*--- POSITIONERA FRAM TILL FÖRSTA KOLLIRADEN                              
148805                                                                          
148905     MOVE PRT-AFTER-4              TO PRT-RADSKIP                         
149005     MOVE SPACE                    TO WS-RAD                              
149105     PERFORM S02-SKRIV                                                    
149205                                                                          
149305     MOVE +1                       TO RAD-IX                              
149405     IF NY-SIDA-FINNS                                                     
149505       CONTINUE                                                           
149605     ELSE                                                                 
149705       MOVE +1                     TO IX                                  
149805       MOVE +1                     TO IX2                                 
149905     END-IF                                                               
150005                                                                          
150105     PERFORM UNTIL IX > 99 OR RAD-IX > MAX-RAD-IX                         
150205        IF TAB1-IDPSN (IX) > 0                                            
150305           MOVE TAB1-IDPSN (IX)    TO GODK-PSN-SW                         
150405           IF GODK-PSN                                                    
150505                                                                          
150605             PERFORM UNTIL IX2 > 4                                        
150705               IF TAB1-KDKOLLI (IX, IX2) > SPACE                          
150805                 PERFORM DCA-SKRIV-KOLLI-RADER                            
150905               END-IF                                                     
151005               ADD +1 TO IX2                                              
151105             END-PERFORM                                                  
151205           END-IF                                                         
151305        END-IF                                                            
151405        ADD  +1                    TO IX                                  
151505        MOVE +1                    TO IX2                                 
151605     END-PERFORM                                                          
151705                                                                          
151805     IF RAD-IX > MAX-RAD-IX                                               
151905       PERFORM DCB-KOLLA-OM-FLER-PSN-FINNS                                
152005     END-IF                                                               
152105                                                                          
152205     IF SDC-NL                                                            
152305       MOVE 'bruto  gew/gr  weight   ' TO ADR-BRUTTO-TEXT                 
152405     END-IF                                                               
152505                                                                          
152605     MOVE WS-VKORDBTO-TOT              TO ADR-VKORDBTO                    
152705                                                                          
152805     MOVE PRT-EQUAL-40                 TO PRT-RADSKIP                     
152905     MOVE RAD8                         TO WS-RAD                          
153005     PERFORM S02-SKRIV                                                    
153105     .                                                                    
153205     EJECT                                                                
153305 DCA-SKRIV-KOLLI-RADER SECTION.                                           
153405                                                                          
153505*--- KOLLI RADER                                                          
153605                                                                          
153705     MOVE SPACE                    TO WS-RAD                              
153805                                      RAD5                                
153905     MOVE TAB1-IDPSN (IX)          TO W-IDPSN                             
154005     MOVE 'SE'                     TO W-IDSPRAK                           
154105                                                                          
154205     PERFORM IMS-GET-1165-WDR2                                            
154305                                                                          
154405     MOVE 1165-1168-BEPSN (1)      TO ADR-BEPSN                           
154505                                                                          
154605     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
154705     MOVE RAD5                     TO WS-RAD                              
154805     PERFORM S02-SKRIV                                                    
154905     ADD +1                        TO RAD-IX                              
155005                                                                          
155105     MOVE 1165-1168-BEPSN (2)      TO ADR-BEPSN                           
155205                                                                          
155305     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
155405     MOVE RAD5                     TO WS-RAD                              
155505     PERFORM S02-SKRIV                                                    
155605     ADD +1                        TO RAD-IX                              
155705                                                                          
155805     IF TAB1-IDPSN (IX) = 10 OR 11                                        
155905        MOVE SPACE                 TO RAD7                                
156005        MOVE 'NET '                TO ADR-TEXT-3                          
156105        MOVE TAB1-VKART-FG (IX, IX2) TO ADR-VKART-FG                      
156205        MOVE ' GRAMS'              TO ADR-TEXT-4                          
156305                                                                          
156405        MOVE PRT-AFTER-1           TO PRT-RADSKIP                         
156505        MOVE RAD7                  TO WS-RAD                              
156605        PERFORM S02-SKRIV                                                 
156705                                                                          
156805        ADD +1                     TO RAD-IX                              
156905                                                                          
157005     END-IF                                                               
157105                                                                          
157205     MOVE TAB1-ANTAL-KOLLIN (IX, IX2) TO ADR-ANTAL-KOLLIN                 
157305                                                                          
157405     MOVE TAB1-KDKOLLI (IX, IX2)     TO GODK-KDKOLLI-SW                   
157505     IF FIBREBOARDBOX                                                     
157605       IF TAB1-ANTAL-KOLLIN (IX, IX2) > 1                                 
157705         MOVE 'Papplådor/Fibreboard Boxes' TO ADR-KDKOLLI-TXT             
157805       ELSE                                                               
157905         MOVE 'Papplåda/Fibreboard Box   ' TO ADR-KDKOLLI-TXT             
158005       END-IF                                                             
158105     END-IF                                                               
158205                                                                          
158305     IF PLYWOODBOX                                                        
158405       IF TAB1-ANTAL-KOLLIN (IX, IX2) > 1                                 
158505         MOVE 'Plywoodlådor/Plywood Boxes' TO ADR-KDKOLLI-TXT             
158605       ELSE                                                               
158705         MOVE 'Plywoodlåda/Plywood Box   ' TO ADR-KDKOLLI-TXT             
158805       END-IF                                                             
158905     END-IF                                                               
159005                                                                          
159105     IF PLASTICDRUM                                                       
159205       IF TAB1-ANTAL-KOLLIN (IX, IX2) > 1                                 
159305         MOVE 'Plastfat/Plastic Drums    ' TO ADR-KDKOLLI-TXT             
159405       ELSE                                                               
159505         MOVE 'Plastfat/Plastic Drum     ' TO ADR-KDKOLLI-TXT             
159605       END-IF                                                             
159705     END-IF                                                               
159805                                                                          
159905     IF STEELDRUM                                                         
160005       IF TAB1-ANTAL-KOLLIN (IX, IX2) > 1                                 
160105         MOVE 'Metallfat/Steel Drums     ' TO ADR-KDKOLLI-TXT             
160205       ELSE                                                               
160305         MOVE 'Metallfat/Steel Drum      ' TO ADR-KDKOLLI-TXT             
160405       END-IF                                                             
160505     END-IF                                                               
160605                                                                          
160705     MOVE TAB1-VLFG (IX, IX2)      TO WS-VLFG                             
160805     IF WS-VLFG-6-7 = 0                                                   
160905        CONTINUE                                                          
161005     ELSE                                                                 
161105        COMPUTE WS-VLFG = WS-VLFG + 0.1                                   
161205     END-IF                                                               
161305                                                                          
161405     MOVE WS-VLFG                  TO ADR-VLFG                            
161505                                                                          
161605     COMPUTE WS-VKORDBTO-TOT = WS-VKORDBTO-TOT +                          
161705                                      TAB1-VKORDBTO (IX, IX2)             
161805     MOVE ' KG'                    TO ADR-TEXT-2                          
161905                                      ADR-TEXT-5                          
162005     IF SDC-NL                                                            
162105       MOVE 'net gew/net weight   ' TO ADR-NETTO-TEXT                     
162205     END-IF                                                               
162305                                                                          
162405     IF TAB1-IDPSN (IX) = 30 OR 53 OR 60                                  
162505       MOVE 'nettovol/net volume  ' TO ADR-NETTO-TEXT                     
162605       MOVE '  L'                  TO ADR-TEXT-2                          
162705*                                                                         
162805*    fix CO/april 2014                                                    
162905*    IDPSN 53 och 60 är beteckning för fg. vätska som transp.             
163005*    i fat av plast eller metall. I dagens läge kan man inte              
163105*    beräkna nettovolymen på annat sätt än att man drar av                
163205*    10 från bruttovikten...                                              
163305*    Då är nettovikten (volym i detta fall) alltid < bruttovikten         
163405*                                                                         
163505       IF TAB1-IDPSN (IX) = 53 OR 60                                      
163605         IF (TAB1-VLFG (IX, IX2) = ZERO) OR                               
163705            (TAB1-VLFG (IX, IX2) > TAB1-VKORDBTO (IX, IX2))               
163805           COMPUTE WS-VLFG = TAB1-VKORDBTO (IX, IX2) - 10                 
163905           MOVE WS-VLFG            TO ADR-VLFG                            
164005         END-IF                                                           
164105       END-IF                                                             
164205     END-IF                                                               
164305                                                                          
164405     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
164505     MOVE RAD6                     TO WS-RAD                              
164605     PERFORM S02-SKRIV                                                    
164705     ADD +1                        TO RAD-IX                              
164805                                                                          
164905     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
165005     MOVE SPACE                    TO WS-RAD                              
165105     PERFORM S02-SKRIV                                                    
165205     ADD +1                        TO RAD-IX                              
165305     .                                                                    
165405     EJECT                                                                
165505 DCB-KOLLA-OM-FLER-PSN-FINNS SECTION.                                     
165605                                                                          
165705     MOVE IX                       TO SPAR-IX                             
165805                                                                          
165905     PERFORM UNTIL IX > 99                                                
166005                                                                          
166105       IF TAB1-IDPSN (IX) > ZERO                                          
166205         MOVE TAB1-IDPSN (IX)      TO GODK-PSN-SW                         
166305         IF GODK-PSN                                                      
166405           MOVE JA                 TO NY-SIDA-SW                          
166505           MOVE 100                TO IX                                  
166605         END-IF                                                           
166705       END-IF                                                             
166805                                                                          
166905       ADD +1                      TO IX                                  
167005     END-PERFORM                                                          
167105                                                                          
167205     IF NY-SIDA-FINNS                                                     
167305       MOVE SPAR-IX                TO IX                                  
167405     END-IF                                                               
167505     .                                                                    
167605     EJECT                                                                
167705 DD-REDIGERA-BLK-MIXAD SECTION.                                           
167805                                                                          
167905                                                                          
168005*--- POSITIONERA FRAM TILL FÖRSTA KOLLIRADEN                              
168105                                                                          
168205     MOVE PRT-AFTER-4              TO PRT-RADSKIP                         
168305     MOVE SPACE                    TO WS-RAD                              
168405     PERFORM S02-SKRIV                                                    
168505                                                                          
168605     MOVE +1                       TO RAD-IX                              
168705     IF NY-SIDA-FINNS                                                     
168805       CONTINUE                                                           
168905     ELSE                                                                 
169005       MOVE +1                     TO IX                                  
169105       MOVE +1                     TO IX2                                 
169205     END-IF                                                               
169305                                                                          
169405     PERFORM UNTIL IX > 99 OR RAD-IX > MAX-RAD-IX                         
169505        IF TAB2-KDKOLLI (IX) > SPACE                                      
169605           PERFORM UNTIL IX2 > 9                                          
169705             IF TAB2-IDPSN (IX, IX2 ) > 0                                 
169805               MOVE TAB2-IDPSN (IX, IX2) TO GODK-PSN-SW                   
169905               IF GODK-PSN                                                
170005                                                                          
170105                  PERFORM DDA-SKRIV-PSN-RADER                             
170205               END-IF                                                     
170305             END-IF                                                       
170405             ADD +1 TO IX2                                                
170505           END-PERFORM                                                    
170605           PERFORM DDB-SKRIV-KOLLI-RADER                                  
170705        END-IF                                                            
170805        ADD  +1                    TO IX                                  
170905        MOVE +1                    TO IX2                                 
171005     END-PERFORM                                                          
171105                                                                          
171205     IF RAD-IX > MAX-RAD-IX                                               
171305       PERFORM DDC-KOLLA-OM-FLER-KDKOLLI                                  
171405     END-IF                                                               
171505                                                                          
171605     IF SDC-NL                                                            
171705       MOVE 'bruto  gew/gr  weight   ' TO ADR-BRUTTO-TEXT                 
171805     END-IF                                                               
171905                                                                          
172005     MOVE WS-VKORDBTO-TOT              TO ADR-VKORDBTO                    
172105                                                                          
172205     MOVE PRT-EQUAL-40                 TO PRT-RADSKIP                     
172305     MOVE RAD8                         TO WS-RAD                          
172405     PERFORM S02-SKRIV                                                    
172505     .                                                                    
172605     EJECT                                                                
172705 DDA-SKRIV-PSN-RADER SECTION.                                             
172805                                                                          
172905                                                                          
173005     MOVE SPACE                    TO WS-RAD                              
173105                                      RAD5                                
173205     MOVE TAB2-IDPSN (IX, IX2)     TO W-IDPSN                             
173305     MOVE 'SE'                     TO W-IDSPRAK                           
173405                                                                          
173505     PERFORM IMS-GET-1165-WDR2                                            
173605                                                                          
173705     MOVE 1165-1168-BEPSN (1)      TO ADR-BEPSN                           
173805                                                                          
173905     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
174005     MOVE RAD5                     TO WS-RAD                              
174105     PERFORM S02-SKRIV                                                    
174205     ADD +1                        TO RAD-IX                              
174305                                                                          
174405     MOVE 1165-1168-BEPSN (2)      TO ADR-BEPSN                           
174505                                                                          
174605     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
174705     MOVE RAD5                     TO WS-RAD                              
174805     PERFORM S02-SKRIV                                                    
174905     ADD +1                        TO RAD-IX                              
175005     .                                                                    
175105     EJECT                                                                
175205 DDB-SKRIV-KOLLI-RADER SECTION.                                           
175305                                                                          
175405     MOVE TAB2-ANTAL-KOLLIN (IX) TO ADR-ANTAL-KOLLIN                      
175505                                                                          
175605     MOVE TAB2-KDKOLLI (IX)     TO GODK-KDKOLLI-SW                        
175705     IF FIBREBOARDBOX                                                     
175805       IF TAB2-ANTAL-KOLLIN (IX) > 1                                      
175905         MOVE 'Papplådor/Fibreboard Boxes' TO ADR-KDKOLLI-TXT             
176005       ELSE                                                               
176105         MOVE 'Papplåda/Fibreboard Box   ' TO ADR-KDKOLLI-TXT             
176205       END-IF                                                             
176305     END-IF                                                               
176405                                                                          
176505     IF PLYWOODBOX                                                        
176605       IF TAB2-ANTAL-KOLLIN (IX) > 1                                      
176705         MOVE 'Plywoodlådor/Plywood Boxes' TO ADR-KDKOLLI-TXT             
176805       ELSE                                                               
176905         MOVE 'Plywoodlåda/Plywood Box   ' TO ADR-KDKOLLI-TXT             
177005       END-IF                                                             
177105     END-IF                                                               
177205                                                                          
177305     IF PLASTICDRUM                                                       
177405       IF TAB2-ANTAL-KOLLIN (IX) > 1                                      
177505         MOVE 'Plastfat/Plastic Drums    ' TO ADR-KDKOLLI-TXT             
177605       ELSE                                                               
177705         MOVE 'Plastfat/Plastic Drum     ' TO ADR-KDKOLLI-TXT             
177805       END-IF                                                             
177905     END-IF                                                               
178005                                                                          
178105     IF STEELDRUM                                                         
178205       IF TAB2-ANTAL-KOLLIN (IX) > 1                                      
178305         MOVE 'Metallfat/Steel Drums     ' TO ADR-KDKOLLI-TXT             
178405       ELSE                                                               
178505         MOVE 'Metallfat/Steel Drum      ' TO ADR-KDKOLLI-TXT             
178605       END-IF                                                             
178705     END-IF                                                               
178805                                                                          
178905     MOVE TAB2-VLFG (IX)           TO WS-VLFG                             
179005     IF WS-VLFG-6-7 = 0                                                   
179105        CONTINUE                                                          
179205     ELSE                                                                 
179305        COMPUTE WS-VLFG = WS-VLFG + 0.1                                   
179405     END-IF                                                               
179505                                                                          
179605     MOVE WS-VLFG                  TO ADR-VLFG                            
179705                                                                          
179805     COMPUTE WS-VKORDBTO-TOT = WS-VKORDBTO-TOT +                          
179905                                      TAB2-VKORDBTO (IX)                  
180005     MOVE ' KG'                    TO ADR-TEXT-2                          
180105                                      ADR-TEXT-5                          
180205     IF SDC-NL                                                            
180305       MOVE 'net gew/net weight   ' TO ADR-NETTO-TEXT                     
180405     END-IF                                                               
180505                                                                          
180605     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
180705     MOVE RAD6                     TO WS-RAD                              
180805     PERFORM S02-SKRIV                                                    
180905     ADD +1                        TO RAD-IX                              
181005                                                                          
181105     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
181205     MOVE SPACE                    TO WS-RAD                              
181305     PERFORM S02-SKRIV                                                    
181405     ADD +1                        TO RAD-IX                              
181505     .                                                                    
181605     EJECT                                                                
181705 DDC-KOLLA-OM-FLER-KDKOLLI SECTION.                                       
181805                                                                          
181905     MOVE IX                       TO SPAR-IX                             
182005                                                                          
182105     PERFORM UNTIL IX > 99                                                
182205                                                                          
182305       IF TAB2-KDKOLLI(IX) > SPACE                                        
182405         MOVE TAB2-KDKOLLI (IX)    TO GODK-KDKOLLI-SW                     
182505         IF FIBREBOARDBOX OR PLYWOODBOX OR                                
182605            PLASTICDRUM   OR STEELDRUM                                    
182705           MOVE JA                 TO NY-SIDA-SW                          
182805           MOVE 100                TO IX                                  
182905         END-IF                                                           
183005       END-IF                                                             
183105                                                                          
183205       ADD +1                      TO IX                                  
183305     END-PERFORM                                                          
183405                                                                          
183505     IF NY-SIDA-FINNS                                                     
183605       MOVE SPAR-IX                TO IX                                  
183705     END-IF                                                               
183805     .                                                                    
183905     EJECT                                                                
184005 DE-REDIGERA-BLK-EJ-GODK SECTION.                                         
184105                                                                          
184205                                                                          
184305     MOVE PRT-AFTER-4              TO PRT-RADSKIP                         
184405     MOVE SPACE                    TO WS-RAD                              
184505     PERFORM S02-SKRIV                                                    
184605                                                                          
184705     MOVE +1                       TO RAD-IX                              
184805     IF NY-SIDA-FINNS                                                     
184905       CONTINUE                                                           
185005     ELSE                                                                 
185105       MOVE +1                     TO IX                                  
185205       MOVE +1                     TO IX2                                 
185305     END-IF                                                               
185405                                                                          
185505     PERFORM UNTIL IX > 99 OR RAD-IX > MAX-RAD-IX                         
185605                                                                          
185705       IF TAB3-ANTAL-KOLLIN (IX) > ZERO AND                               
185805          TAB3-IDPSN (IX, IX2)   > ZERO                                   
185905                                                                          
186005          PERFORM UNTIL IX2 > 9                                           
186105            IF TAB3-IDPSN (IX, IX2) > 0                                   
186205              MOVE TAB3-IDPSN (IX, IX2) TO GODK-PSN-SW                    
186305              IF GODK-PSN                                                 
186405                                                                          
186505                 PERFORM DEA-SKRIV-PSN-RADER                              
186605              END-IF                                                      
186705            END-IF                                                        
186805            ADD +1 TO IX2                                                 
186905          END-PERFORM                                                     
187005          PERFORM DEB-SKRIV-KOLLI-RADER                                   
187105       END-IF                                                             
187205       ADD  +1                    TO IX                                   
187305       MOVE +1                    TO IX2                                  
187405     END-PERFORM                                                          
187505                                                                          
187605     IF RAD-IX > MAX-RAD-IX                                               
187705       PERFORM DEC-KOLLA-OM-FLER-PSN-FINNS                                
187805     END-IF                                                               
187905                                                                          
188005     IF SDC-NL                                                            
188105       MOVE 'bruto  gew/gr  weight   ' TO ADR-BRUTTO-TEXT                 
188205     END-IF                                                               
188305                                                                          
188405     MOVE WS-VKORDBTO-TOT              TO ADR-VKORDBTO                    
188505                                                                          
188605     MOVE PRT-EQUAL-40                 TO PRT-RADSKIP                     
188705     MOVE RAD8                         TO WS-RAD                          
188805     PERFORM S02-SKRIV                                                    
188905     .                                                                    
189005     EJECT                                                                
189105 DEA-SKRIV-PSN-RADER SECTION.                                             
189205                                                                          
189305                                                                          
189405     MOVE SPACE                    TO WS-RAD                              
189505                                      RAD5                                
189605     MOVE TAB3-IDPSN (IX, IX2)     TO W-IDPSN                             
189705     MOVE 'SE'                     TO W-IDSPRAK                           
189805                                                                          
189905     PERFORM IMS-GET-1165-WDR2                                            
190005                                                                          
190105     MOVE 1165-1168-BEPSN (1)      TO ADR-BEPSN                           
190205                                                                          
190305     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
190405     MOVE RAD5                     TO WS-RAD                              
190505     PERFORM S02-SKRIV                                                    
190605     ADD +1                        TO RAD-IX                              
190705                                                                          
190805     MOVE 1165-1168-BEPSN (2)      TO ADR-BEPSN                           
190905                                                                          
191005     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
191105     MOVE RAD5                     TO WS-RAD                              
191205     PERFORM S02-SKRIV                                                    
191305     ADD +1                        TO RAD-IX                              
191405     .                                                                    
191505     EJECT                                                                
191605 DEB-SKRIV-KOLLI-RADER SECTION.                                           
191705                                                                          
191805     MOVE TAB3-ANTAL-KOLLIN (IX)   TO ADR-ANTAL-KOLLIN                    
191905                                                                          
192005     MOVE SPACE                    TO ADR-KDKOLLI-TXT                     
192105                                                                          
192205     MOVE TAB3-VLFG (IX)           TO WS-VLFG                             
192305     IF WS-VLFG-6-7 = 0                                                   
192405        CONTINUE                                                          
192505     ELSE                                                                 
192605        COMPUTE WS-VLFG = WS-VLFG + 0.1                                   
192705     END-IF                                                               
192805                                                                          
192905     MOVE WS-VLFG                  TO ADR-VLFG                            
193005                                                                          
193105     COMPUTE WS-VKORDBTO-TOT = WS-VKORDBTO-TOT +                          
193205                                      TAB3-VKORDBTO (IX)                  
193305     MOVE ' KG'                    TO ADR-TEXT-2                          
193405                                      ADR-TEXT-5                          
193505     IF SDC-NL                                                            
193605       MOVE 'net gew/net weight   ' TO ADR-NETTO-TEXT                     
193705     END-IF                                                               
193805                                                                          
193905     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
194005     MOVE RAD6                     TO WS-RAD                              
194105     PERFORM S02-SKRIV                                                    
194205     ADD +1                        TO RAD-IX                              
194305                                                                          
194405     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
194505     MOVE SPACE                    TO WS-RAD                              
194605     PERFORM S02-SKRIV                                                    
194705     ADD +1                        TO RAD-IX                              
194805     .                                                                    
194905     EJECT                                                                
195005 DEC-KOLLA-OM-FLER-PSN-FINNS SECTION.                                     
195105                                                                          
195205     MOVE IX                       TO SPAR-IX                             
195305                                                                          
195405     PERFORM UNTIL IX > 99                                                
195505                                                                          
195605       IF TAB3-VKORDBTO (IX) > ZERO                                       
195705         MOVE JA                   TO NY-SIDA-SW                          
195805         MOVE 100                  TO IX                                  
195905       END-IF                                                             
196005                                                                          
196105       ADD +1                      TO IX                                  
196205     END-PERFORM                                                          
196305                                                                          
196405     IF NY-SIDA-FINNS                                                     
196505       MOVE SPAR-IX                TO IX                                  
196605     END-IF                                                               
196705     .                                                                    
196805     EJECT                                                                
196905 S01-OPPNA-PRINTER SECTION.                                               
197005                                                                          
197105     CALL W006PRR1 USING PRT-SPOOL-A4S                                    
197205                         PRT-OPEN                                         
197305                         WS-PRT-IDPRTLST                                  
197405                         ALT-PCB                                          
197505                         LISB-PCB                                         
197605                         WS-PRT-IDLIST                                    
197705                         WS-PRT-DUMMY                                     
197805                         WS-PRT-DUMMY                                     
197905                                                                          
198005     .                                                                    
198105     EJECT                                                                
198205 S02-SKRIV SECTION.                                                       
198305                                                                          
198405     CALL W006PRR1 USING PRT-SPOOL-A4S                                    
198505                         PRT-WRITE                                        
198605                         WS-PRT-IDPRTLST                                  
198705                         ALT-PCB                                          
198805                         LISB-PCB                                         
198905                         WS-PRT-IDLIST                                    
199005                         PRT-RADSKIP                                      
199105                         WS-PRT-LISTRAD                                   
199205     .                                                                    
199305     EJECT                                                                
199405 S10-STAENG-PRINTER SECTION.                                              
199505                                                                          
199605     CALL W006PRR1 USING PRT-SPOOL-A4S                                    
199705                         PRT-CLOSE                                        
199805                         WS-PRT-IDPRTLST                                  
199905                         ALT-PCB                                          
200005                         LISB-PCB                                         
200105                         WS-PRT-IDLIST                                    
200205                         WS-PRT-DUMMY                                     
200305                         WS-PRT-DUMMY                                     
200405                                                                          
200505     .                                                                    
200605     EJECT                                                                
200705 S20-FLYTTA-TILL-OMIXAD-TAB SECTION.                                      
200805                                                                          
200905     MOVE +1                 TO IX                                        
201005     MOVE NEJ                TO KDKOLLI-SW                                
201105                                                                          
201205     PERFORM UNTIL IX > 9                                                 
201305       IF KOLLI-IDPSN (IX)    > ZERO                                      
201405         MOVE KOLLI-IDPSN (IX) TO GODK-PSN-SW                             
201505                                                                          
201605         IF GODK-PSN                                                      
201705           MOVE JA      TO FORTSAETTNING-SW                               
201805           MOVE KOLLI-IDPSN (IX)   TO PSN-IX                              
201905                                                                          
202005           IF TAB1-IDPSN (PSN-IX) = ZERO                                  
202105             MOVE KOLLI-IDPSN (IX) TO TAB1-IDPSN (PSN-IX)                 
202205             MOVE JA TO TAB1-DATA-SW                                      
202305           END-IF                                                         
202405                                                                          
202505           MOVE +1 TO KDKOLLI-IX                                          
202605           PERFORM UNTIL KDKOLLI-IX > 4 OR KDKOLLI-OK                     
202705                                                                          
202805             IF TAB1-KDKOLLI (PSN-IX, KDKOLLI-IX) = SPACE                 
202905               MOVE KOLLI-KDKOLLI                                         
203005                        TO TAB1-KDKOLLI (PSN-IX, KDKOLLI-IX)              
203105               MOVE KOLLI-VKART-FG (IX)                                   
203205                        TO TAB1-VKART-FG (PSN-IX, KDKOLLI-IX)             
203305               MOVE KOLLI-VLFG (IX)                                       
203405                        TO TAB1-VLFG (PSN-IX, KDKOLLI-IX)                 
203505               MOVE KOLLI-VKORDBTO-KOLLI                                  
203605                        TO TAB1-VKORDBTO (PSN-IX, KDKOLLI-IX)             
203705                                                                          
203805               ADD +1   TO TAB1-ANTAL-KOLLIN (PSN-IX, KDKOLLI-IX)         
203905               MOVE JA TO KDKOLLI-SW                                      
204005             ELSE                                                         
204105               PERFORM S20A-KOLLA-KDKOLLI                                 
204205                                                                          
204305               IF WS-KDKOLLI-KOLLI = WS-KDKOLLI-TAB                       
204405                                                                          
204505                 ADD KOLLI-VKART-FG (IX)                                  
204605                        TO TAB1-VKART-FG(PSN-IX, KDKOLLI-IX)              
204705                 ADD KOLLI-VLFG (IX)                                      
204805                        TO TAB1-VLFG(PSN-IX, KDKOLLI-IX)                  
204905                 ADD KOLLI-VKORDBTO-KOLLI                                 
205005                        TO TAB1-VKORDBTO(PSN-IX, KDKOLLI-IX)              
205105                 ADD +1 TO TAB1-ANTAL-KOLLIN (PSN-IX, KDKOLLI-IX)         
205205                                                                          
205305                 MOVE JA TO KDKOLLI-SW                                    
205405               END-IF                                                     
205505             END-IF                                                       
205605                                                                          
205705             ADD +1   TO KDKOLLI-IX                                       
205805           END-PERFORM                                                    
205905         END-IF                                                           
206005       END-IF                                                             
206105       ADD +1               TO IX                                         
206205     END-PERFORM                                                          
206305     .                                                                    
206405     EJECT                                                                
206505 S20A-KOLLA-KDKOLLI SECTION.                                              
206605                                                                          
206705     MOVE KOLLI-KDKOLLI                     TO GODK-KDKOLLI-SW            
206805     IF FIBREBOARDBOX                                                     
206905       MOVE 'FIBRE  '                       TO WS-KDKOLLI-KOLLI           
207005     END-IF                                                               
207105     IF PLYWOODBOX                                                        
207205       MOVE 'PLYWOOD'                       TO WS-KDKOLLI-KOLLI           
207305     END-IF                                                               
207405     IF PLASTICDRUM                                                       
207505       MOVE 'PLASTIC'                       TO WS-KDKOLLI-KOLLI           
207605     END-IF                                                               
207705     IF STEELDRUM                                                         
207805       MOVE 'STEEL  '                       TO WS-KDKOLLI-KOLLI           
207905     END-IF                                                               
208005                                                                          
208105     MOVE TAB1-KDKOLLI (PSN-IX, KDKOLLI-IX) TO GODK-KDKOLLI-SW            
208205     IF FIBREBOARDBOX                                                     
208305       MOVE 'FIBRE  '                       TO WS-KDKOLLI-TAB             
208405     END-IF                                                               
208505     IF PLYWOODBOX                                                        
208605       MOVE 'PLYWOOD'                       TO WS-KDKOLLI-TAB             
208705     END-IF                                                               
208805     IF PLASTICDRUM                                                       
208905       MOVE 'PLASTIC'                       TO WS-KDKOLLI-TAB             
209005     END-IF                                                               
209105     IF STEELDRUM                                                         
209205       MOVE 'STEEL  '                       TO WS-KDKOLLI-TAB             
209305     END-IF                                                               
209405     .                                                                    
209505     EJECT                                                                
209605 S21-FLYTTA-TILL-MIXAD-TAB SECTION.                                       
209705                                                                          
209805     MOVE +1                 TO IX                                        
209905     MOVE +1                 TO TAB2-IX                                   
210005     MOVE +1                 TO TAB2-PSN-IX                               
210105     MOVE JA                 TO PSN-MATCH-SW                              
210205                                                                          
210305     PERFORM UNTIL TAB2-IX > 100                                          
210405       PERFORM S21A-KOLLA-OM-SAMMA-PSN                                    
210505                                                                          
210605       IF PSN-MATCH                                                       
210705                                                                          
210805         PERFORM S21B-FLYTTA-DATA1-TAB2                                   
210905         MOVE +100 TO TAB2-IX                                             
211005                                                                          
211105       ELSE                                                               
211205         IF TAB2-KDKOLLI (TAB2-IX)   = SPACE AND                          
211305            TAB2-ANTAL-PSN (TAB2-IX) = ZERO                               
211405                                                                          
211505           PERFORM S21C-FLYTTA-DATA2-TAB2                                 
211605           MOVE +100 TO TAB2-IX                                           
211705         END-IF                                                           
211805       END-IF                                                             
211905                                                                          
212005       ADD +1 TO TAB2-IX                                                  
212105     END-PERFORM                                                          
212205     .                                                                    
212305     EJECT                                                                
212405 S21A-KOLLA-OM-SAMMA-PSN SECTION.                                         
212505                                                                          
212605     PERFORM S21AA-KOLLA-KDKOLLI                                          
212705                                                                          
212805     IF WS-KDKOLLI-KOLLI = WS-KDKOLLI-TAB      AND                        
212905        WS-ANTAL-PSN     = TAB2-ANTAL-PSN (TAB2-IX)                       
213005                                                                          
213105       PERFORM UNTIL IX > 9                                               
213205         IF KOLLI-IDPSN (IX)  > ZERO                                      
213305           MOVE KOLLI-IDPSN (IX) TO GODK-PSN-SW                           
213405                                                                          
213505           IF GODK-PSN                                                    
213605             MOVE JA    TO FORTSAETTNING-SW                               
213705                                                                          
213805             IF TAB2-IDPSN (TAB2-IX, TAB2-PSN-IX) =                       
213905                KOLLI-IDPSN (IX)                                          
214005                                                                          
214105               ADD  +1 TO IX                                              
214205               MOVE +1 TO TAB2-PSN-IX                                     
214305             ELSE                                                         
214405               ADD +1 TO TAB2-PSN-IX                                      
214505               IF TAB2-PSN-IX > 9                                         
214605                 MOVE NEJ TO PSN-MATCH-SW                                 
214705                 ADD +1   TO IX                                           
214805                 MOVE +1 TO TAB2-PSN-IX                                   
214905               END-IF                                                     
215005                                                                          
215105             END-IF                                                       
215205           ELSE                                                           
215305             ADD +1   TO IX                                               
215405           END-IF                                                         
215505         ELSE                                                             
215605           ADD +1 TO IX                                                   
215705         END-IF                                                           
215805                                                                          
215905       END-PERFORM                                                        
216005                                                                          
216105     ELSE                                                                 
216205       MOVE NEJ TO PSN-MATCH-SW                                           
216305     END-IF                                                               
216405     .                                                                    
216505     EJECT                                                                
216605 S21AA-KOLLA-KDKOLLI SECTION.                                             
216705                                                                          
216805     MOVE SPACE                   TO WS-KDKOLLI-KOLLI                     
216905                                     WS-KDKOLLI-TAB                       
217005     MOVE KOLLI-KDKOLLI           TO GODK-KDKOLLI-SW                      
217105                                                                          
217205     IF FIBREBOARDBOX                                                     
217305       MOVE 'FIBRE  '             TO WS-KDKOLLI-KOLLI                     
217405     END-IF                                                               
217505     IF PLYWOODBOX                                                        
217605       MOVE 'PLYWOOD'             TO WS-KDKOLLI-KOLLI                     
217705     END-IF                                                               
217805     IF PLASTICDRUM                                                       
217905       MOVE 'PLASTIC'             TO WS-KDKOLLI-KOLLI                     
218005     END-IF                                                               
218105     IF STEELDRUM                                                         
218205       MOVE 'STEEL  '             TO WS-KDKOLLI-KOLLI                     
218305     END-IF                                                               
218405                                                                          
218505     MOVE TAB2-KDKOLLI (TAB2-IX)  TO GODK-KDKOLLI-SW                      
218605     IF FIBREBOARDBOX                                                     
218705       MOVE 'FIBRE  '             TO WS-KDKOLLI-TAB                       
218805     END-IF                                                               
218905     IF PLYWOODBOX                                                        
219005       MOVE 'PLYWOOD'             TO WS-KDKOLLI-TAB                       
219105     END-IF                                                               
219205     IF PLASTICDRUM                                                       
219305       MOVE 'PLASTIC'             TO WS-KDKOLLI-TAB                       
219405     END-IF                                                               
219505     IF STEELDRUM                                                         
219605       MOVE 'STEEL  '             TO WS-KDKOLLI-TAB                       
219705     END-IF                                                               
219805     .                                                                    
219905     EJECT                                                                
220005 S21B-FLYTTA-DATA1-TAB2 SECTION.                                          
220105                                                                          
220205     MOVE +1 TO IX                                                        
220305     MOVE +1 TO TAB2-PSN-IX                                               
220405                                                                          
220505     ADD +1               TO TAB2-ANTAL-KOLLIN (TAB2-IX)                  
220605     ADD KOLLI-VKORDBTO-KOLLI TO TAB2-VKORDBTO (TAB2-IX)                  
220705                                                                          
220805     PERFORM UNTIL IX > 9                                                 
220905       IF KOLLI-IDPSN (IX)    > ZERO                                      
221005         MOVE KOLLI-IDPSN (IX) TO GODK-PSN-SW                             
221105                                                                          
221205         IF GODK-PSN                                                      
221305           MOVE JA      TO FORTSAETTNING-SW                               
221405                                                                          
221505           ADD KOLLI-VLFG (IX)     TO TAB2-VLFG (TAB2-IX)                 
221605           ADD KOLLI-VKART-FG (IX) TO TAB2-VKART-FG (TAB2-IX)             
221705                                                                          
221805           ADD    +1 TO IX                                                
221905           ADD    +1 TO TAB2-PSN-IX                                       
222005         ELSE                                                             
222105           ADD    +1 TO IX                                                
222205         END-IF                                                           
222305       ELSE                                                               
222405         ADD +1 TO IX                                                     
222505       END-IF                                                             
222605                                                                          
222705     END-PERFORM                                                          
222805     .                                                                    
222905     EJECT                                                                
223005 S21C-FLYTTA-DATA2-TAB2 SECTION.                                          
223105                                                                          
223205     MOVE +1 TO IX                                                        
223305     MOVE +1 TO TAB2-PSN-IX                                               
223405                                                                          
223505     MOVE KOLLI-KDKOLLI       TO TAB2-KDKOLLI (TAB2-IX)                   
223605     ADD +1                   TO TAB2-ANTAL-KOLLIN (TAB2-IX)              
223705     ADD KOLLI-VKORDBTO-KOLLI TO TAB2-VKORDBTO (TAB2-IX)                  
223805     MOVE WS-ANTAL-PSN        TO TAB2-ANTAL-PSN (TAB2-IX)                 
223905                                                                          
224005     PERFORM UNTIL IX > 9                                                 
224105       IF KOLLI-IDPSN (IX)    > ZERO                                      
224205         MOVE KOLLI-IDPSN (IX) TO GODK-PSN-SW                             
224305                                                                          
224405         IF GODK-PSN                                                      
224505           MOVE JA      TO FORTSAETTNING-SW                               
224605                                                                          
224705           MOVE KOLLI-IDPSN (IX)   TO                                     
224805                TAB2-IDPSN (TAB2-IX, TAB2-PSN-IX)                         
224905           ADD KOLLI-VLFG (IX)     TO TAB2-VLFG (TAB2-IX)                 
225005           ADD KOLLI-VKART-FG (IX) TO TAB2-VKART-FG (TAB2-IX)             
225105                                                                          
225205                                                                          
225305           ADD    +1 TO IX                                                
225405           ADD    +1 TO TAB2-PSN-IX                                       
225505         ELSE                                                             
225605           ADD    +1 TO IX                                                
225705         END-IF                                                           
225805       ELSE                                                               
225905         ADD +1 TO IX                                                     
226005       END-IF                                                             
226105                                                                          
226205     END-PERFORM                                                          
226305     .                                                                    
226405     EJECT                                                                
226505 S22-FLYTTA-TILL-EJ-GODK-TAB SECTION.                                     
226605                                                                          
226705     MOVE +1  TO IX                                                       
226805     MOVE +1  TO TAB3-IX                                                  
226905     MOVE +1  TO TAB3-PSN-IX                                              
227005     MOVE NEJ TO PSN-OK-SW                                                
227105                                                                          
227205     PERFORM UNTIL TAB3-IX > +100                                         
227305       IF TAB3-KDKOLLI (TAB3-IX)      = SPACE AND                         
227405          TAB3-ANTAL-KOLLIN (TAB3-IX) = ZERO                              
227505                                                                          
227605         PERFORM UNTIL IX > 9                                             
227705           IF KOLLI-IDPSN (IX) > ZERO                                     
227805             MOVE KOLLI-IDPSN (IX) TO GODK-PSN-SW                         
227905                                                                          
228005             IF GODK-PSN                                                  
228105               MOVE JA  TO FORTSAETTNING-SW                               
228205               MOVE JA  TO PSN-OK-SW                                      
228305                                                                          
228405               MOVE KOLLI-IDPSN (IX)   TO                                 
228505                    TAB3-IDPSN (TAB3-IX, TAB3-PSN-IX)                     
228605               ADD KOLLI-VLFG (IX)     TO TAB3-VLFG (TAB3-IX)             
228705               ADD KOLLI-VKART-FG (IX) TO TAB3-VKART-FG (TAB3-IX)         
228805                                                                          
228905                                                                          
229005               ADD +1 TO IX                                               
229105               ADD +1 TO TAB3-PSN-IX                                      
229205             ELSE                                                         
229305               ADD +1 TO IX                                               
229405             END-IF                                                       
229505           ELSE                                                           
229605             ADD +1 TO IX                                                 
229705           END-IF                                                         
229805                                                                          
229905         END-PERFORM                                                      
230005                                                                          
230105         IF PSN-OK                                                        
230205           MOVE KOLLI-KDKOLLI      TO TAB3-KDKOLLI (TAB3-IX)              
230305           MOVE KOLLI-VKORDBTO-KOLLI                                      
230405                                   TO TAB3-VKORDBTO (TAB3-IX)             
230505           ADD +1                  TO TAB3-ANTAL-KOLLIN (TAB3-IX)         
230605         END-IF                                                           
230705                                                                          
230805         MOVE +100 TO TAB3-IX                                             
230905       END-IF                                                             
231005                                                                          
231105       ADD +1 TO TAB3-IX                                                  
231205     END-PERFORM                                                          
231305     .                                                                    
231405     EJECT                                                                
231505 S90-SEND-OPEN SECTION.                                                   
231605                                                                          
231705     MOVE WS-ADRESS-DP                    TO SEND-ADDISPABS               
231805     MOVE 'OPEN'                          TO SEND-KDFUNC                  
231905     CALL WZ01SEND USING SEND-CONTROL-AREA                                
232005                         SEND-OPEN-AREA                                   
232105     IF SEND-KDRC > ZERO                                                  
232205       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
232305       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
232405       DELIMITED BY SIZE INTO FELTEXT                                     
232505       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
232605     END-IF                                                               
232705     .                                                                    
232805                                                                          
232905 S91-PUT-DOC-HDR SECTION.                                                 
233005                                                                          
233105     MOVE 'PUT'                           TO SEND-KDFUNC                  
233205     MOVE WZ04-001-IDCOM                  TO SEND-IDCOM                   
233305     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
233405     CALL WZ01SEND USING SEND-CONTROL-AREA                                
233505                         SEND-KVDLEN                                      
233605                         HDR-AREA                                         
233705     IF SEND-KDRC > ZERO                                                  
233805       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
233905       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
234005       DELIMITED BY SIZE INTO FELTEXT                                     
234105       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
234205     END-IF                                                               
234305     .                                                                    
234405     SKIP3                                                                
234505 S92-PUT-DOC-HEAD SECTION.                                                
234605                                                                          
234705     MOVE 'PUT'                           TO SEND-KDFUNC                  
234805     MOVE WZ04-001-IDCOM                  TO SEND-IDCOM                   
234905     MOVE LENGTH OF DOC-HEAD-AREA         TO SEND-KVDLEN                  
235005     CALL WZ01SEND USING SEND-CONTROL-AREA                                
235105                         SEND-KVDLEN                                      
235205                         DOC-HEAD-AREA                                    
235305     IF SEND-KDRC > ZERO                                                  
235405       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
235505       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
235605       DELIMITED BY SIZE INTO FELTEXT                                     
235705       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
235805     END-IF                                                               
235905     .                                                                    
236005     SKIP3                                                                
236105 S93-PUT-DOC-LINE SECTION.                                                
236205                                                                          
236305     MOVE 'PUT'                           TO SEND-KDFUNC                  
236405     MOVE WZ04-001-IDCOM                  TO SEND-IDCOM                   
236505     MOVE LENGTH OF DOC-LINE-AREA         TO SEND-KVDLEN                  
236605     CALL WZ01SEND USING SEND-CONTROL-AREA                                
236705                         SEND-KVDLEN                                      
236805                         DOC-LINE-AREA                                    
236905     IF SEND-KDRC > ZERO                                                  
237005       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
237105       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
237205       DELIMITED BY SIZE INTO FELTEXT                                     
237305       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
237405     END-IF                                                               
237505     .                                                                    
237605     EJECT                                                                
237705 S94-PUT-DOC-FOOT SECTION.                                                
237805                                                                          
237905     MOVE 'PUT'                           TO SEND-KDFUNC                  
238005     MOVE WZ04-001-IDCOM                  TO SEND-IDCOM                   
238105     MOVE LENGTH OF DOC-LINE-AREA         TO SEND-KVDLEN                  
238205     CALL WZ01SEND USING SEND-CONTROL-AREA                                
238305                         SEND-KVDLEN                                      
238405                         DOC-FOOT-AREA                                    
238505     IF SEND-KDRC > ZERO                                                  
238605       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
238705       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
238805       DELIMITED BY SIZE INTO FELTEXT                                     
238905       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
239005     END-IF                                                               
239105     .                                                                    
239205     EJECT                                                                
239305 S95-SEND-CLOSE SECTION.                                                  
239405                                                                          
239505     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
239605     CALL WZ01SEND USING SEND-CONTROL-AREA                                
239705     .                                                                    
239805                                                                          
239905                                                                          
240005                                                                          
240105* --- IMS SEKTIONER ---                                                   
240205                                                                          
240305 IMS-GET-1165-WDR2 SECTION.                                               
240405     MOVE 'GET-1165-WDR2   ' TO CURRENT-IMS-SECTION                       
240505                                                                          
240605     MOVE SPACE TO ALL-SSA                                                
240705                                                                          
240805     STRING 'WL116501(WDGXKEY  =' W-WDGXKEY-X ')'                         
240905          DELIMITED BY SIZE INTO SSA1                                     
241005     STRING 'WL116512(KDFGTRP  =' W-KDFGTRP-X ')'                         
241105          DELIMITED BY SIZE INTO SSA2                                     
241205     MOVE '    ' TO GODK-STATUSKODER                                      
241305     CALL CBLTDLI USING GU 1165-PCB DLI-IO-AREA SSA1 SSA2                 
241405     MOVE 1165-STATUS-CODE TO STATUS-WS                                   
241505     PERFORM IMS-STATUSKONTROLL                                           
241605     .                                                                    
241705     EJECT                                                                
241805 IMS-GET-GMTA SECTION.                                                    
241905     MOVE 'GET-GMTA        ' TO CURRENT-IMS-SECTION                       
242005                                                                          
242105     MOVE SPACE TO ALL-SSA                                                
242205                                                                          
242305     STRING 'WLGMTA01(IDGMT   >=' W-IDGMT-X ')'                           
242405          DELIMITED BY SIZE INTO SSA1                                     
242505     MOVE '  '     TO GODK-STATUSKODER                                    
242605     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-AREA-wdb2 SSA1                 
242705     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
242805     PERFORM IMS-STATUSKONTROLL                                           
242905     .                                                                    
243005     SKIP3                                                                
244405 IMS-GET-WDE6 SECTION.                                                    
244505     MOVE 'GET-WDE6        ' TO CURRENT-IMS-SECTION                       
244605                                                                          
244705     MOVE SPACE TO ALL-SSA                                                
244805                                                                          
244905     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
245005            DELIMITED BY SIZE INTO SSA1                                   
245105     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
245205            DELIMITED BY SIZE INTO SSA2                                   
245305     MOVE '    ' TO GODK-STATUSKODER                                      
245405     CALL CBLTDLI USING GU    WDE6-PCB DLI-IO-AREA SSA1 SSA2              
245505     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
245605     PERFORM IMS-STATUSKONTROLL                                           
245705     .                                                                    
245805     EJECT                                                                
245905 IMS-GU-WL451311  SECTION.                                                
246005     MOVE 'GU-WL451311     ' TO CURRENT-IMS-SECTION                       
246105                                                                          
246205     MOVE SPACE TO ALL-SSA                                                
246305                                                                          
246405     STRING 'WL451301(WDGXKEY  =' W-4513-X ')'                            
246505          DELIMITED BY SIZE INTO SSA1                                     
246605     STRING 'WL451311(DASKEPPN =' W-DASKEPPN-X ')'                        
246705          DELIMITED BY SIZE INTO SSA2                                     
246805     MOVE '  ' TO GODK-STATUSKODER                                        
246905     CALL CBLTDLI USING GU 4513-PCB DLI-IO-AREA-4514 SSA1                 
247005     MOVE 4513-STATUS-CODE TO STATUS-WS                                   
247105     PERFORM IMS-STATUSKONTROLL                                           
247205     .                                                                    
247305     SKIP2                                                                
247405 IMS-GNP-WL451321 SECTION.                                                
247505     MOVE 'GNP-WL451321    ' TO CURRENT-IMS-SECTION                       
247605                                                                          
247705     MOVE SPACE TO ALL-SSA                                                
247805                                                                          
247905     STRING 'WL451321(WDGXKEY  =' W-4516-X ')'                            
248005          DELIMITED BY SIZE INTO SSA1                                     
248105     MOVE '  GE' TO GODK-STATUSKODER                                      
248205     CALL CBLTDLI USING GNP 4513-PCB DLI-IO-AREA-4516 SSA1                
248305     MOVE 4513-STATUS-CODE TO STATUS-WS                                   
248405     PERFORM IMS-STATUSKONTROLL                                           
248505     .                                                                    
248605     SKIP3                                                                
248705 IMS-GU-WDB601    SECTION.                                                
248805     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
248905          DELIMITED BY SIZE INTO SSA1                                     
249005     MOVE '  GE' TO GODK-STATUSKODER                                      
249105     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
249205     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
249305     PERFORM IMS-STATUSKONTROLL                                           
249405     .                                                                    
249505 IMS-STATUSKONTROLL SECTION.                                              
249605                                                                          
249705     SET STATUS-IX TO 1                                                   
249805     SEARCH GODK-STATUS                                                   
249905       AT END                                                             
250005       STRING 'STATUSKOD FRÅN IMS ' STATUS-WS                             
250105       DELIMITED BY SIZE INTO FELTEXT                                     
250205       CALL FELLOG                                                        
250305       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
251005     END-SEARCH                                                           
260005     .                                                                    
