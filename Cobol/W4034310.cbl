000101 ID DIVISION.                                                             
000201     SKIP2                                                                
000301 PROGRAM-ID.     W4034310.                                                
000401 AUTHOR.         GUNNAR L, IDK.                                           
000501     DATE-WRITTEN.   MARS 1981.                                           
000601                                                                          
000701     REMARKS.                                                             
000801*                                                                         
000901*    FUNKTION.                                                            
001001*        FLYTTA RAD I PACKAT KOLLI                                        
001101*                                                                         
001201*    INDATA.                                                              
001301*        REQU:        W40343I2                                            
001401*                                                                         
001501*    UTDATA.                                                              
001601*        RESP:        W40343O1                                            
001701*    SKIP3                                                                
001801*2667214 - ADJUST CASE INFO                                               
001901*2523176 - PRINT PICKING ROUND                                            
002001 ENVIRONMENT DIVISION.                                                    
002101     SKIP3                                                                
002201 DATA DIVISION.                                                           
002301     EJECT                                                                
002401 WORKING-STORAGE SECTION.                                                 
002501*    -- CHECKED BY WY2000                                                 
002601     SKIP3                                                                
002701 77   PROGRAM-NAMN           VALUE 'W4034310'                             
002801                                 PIC X(8).                                
002901                                                                          
003001*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003101 77  FILLER                      PIC X(08) VALUE 'ERRORTEX'.              
003201 77  FELTEXT                     PIC X(64) VALUE SPACE.                   
003301 77  FILLER                      PIC X(08) VALUE 'CURRIMS '.              
003401 77  CURRENT-IMS-SECTION         PIC X(32) VALUE SPACE.                   
003501                                                                          
003601 77  JA                          PIC X       VALUE 'J'.                   
003701 77  NEJ                         PIC X       VALUE 'N'.                   
003801 77  IDPRODNR-WS                 PIC X(7)    VALUE SPACE.                 
003901 77  IDKOLLI-WS                  PIC X(5)    VALUE SPACE.                 
004001 77  WS-IDPRODNR                 PIC 9(7)    VALUE ZERO.                  
004101 77  WS-IDKOLLI                  PIC 9(5)    VALUE ZERO.                  
004201 77  WS-MID-IDKOLLI              PIC 9(5).                                
004301 77  WS-KKOLLI-IDKOLLI           PIC 9(5).                                
004401       EJECT                                                              
004501 77  IDARTNR-WS                  PIC S9(9)   VALUE +0   COMP-3.           
004601 77  KVLEVART-WS                 PIC S9(7)   VALUE +0   COMP-3.           
004701 77  KVORDRAD-WS                 PIC S9(5)   VALUE +0   COMP-3.           
004801 77  SUORDV-WS                   PIC S9(9)V9(2) VALUE +0 COMP-3.          
004901 77  SUORDV-EXP-WS               PIC S9(9)V9(2) VALUE +0 COMP-3.          
005001 77  SUORDV-LOC-WS               PIC S9(9)V9(2) VALUE +0 COMP-3.          
005101 77  SUORDV-LOCPREL-WS           PIC S9(9)V9(2) VALUE +0 COMP-3.          
005201 77  KDVALISO-WS                 PIC X(3)    VALUE SPACE.                 
005301 77  KDVALISO-EXP-WS             PIC X(3)    VALUE SPACE.                 
005401 77  VKORDNTO-WS                 PIC S9(6)V9(1) VALUE +0 COMP-3.          
005501 77  VKORDNTO-WS-NYTT            PIC S9(6)V9(1) VALUE +0 COMP-3.          
005601 77  VKORDBTO-WS                 PIC S9(6)V9(1) VALUE +0 COMP-3.          
005701 77  VKTARA-WS                   PIC S9(6)V9(1) VALUE +0 COMP-3.          
005801 77  ANTAL-ISRT-WS               PIC S9(5)   VALUE +0   COMP-3.           
005901 77  ANTAL-DLET-WS               PIC S9(5)   VALUE +0   COMP-3.           
006001 77  WS-PLATS-IDTRPTNR           PIC  9(3).                               
006101 77  WS-KDFRAKT                  PIC  9(2)   VALUE 0.                     
006201*                                                                         
006301 77  MIX                         PIC S9(9)   VALUE +0   COMP SYNC.        
006401 77  FG-INDX                     PIC S9(9)   VALUE +0   COMP SYNC.        
006501 77  TAB-INDX                    PIC S9(9)   VALUE +0   COMP SYNC.        
006601 77  MAX-FG-INDX                 PIC S9(9)   VALUE +10  COMP SYNC.        
006701 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
006801 77  TMS-IX                      PIC S9(9)   VALUE +0   COMP SYNC.        
006901 77  TIAAMMDD-WS                 PIC 9(6)    VALUE ZERO.                  
007001*                                                                         
007101 77  WS-DIKOLLIL                 PIC 9(5).                                
007201 77  WS-DIKOLLIB                 PIC 9(3).                                
007301 77  WS-DIKOLLIH                 PIC 9(3).                                
007401 77  WS-KDEMBTYP                 PIC 9(1).                                
007501 77  WS-KDKOLLI                  PIC X(8).                                
007601 77  WS-IDPLKLST-IP              PIC S9(3) VALUE 0   COMP-3.              
007701 77  WS-IDPLKLST-LINE            PIC S9(3) VALUE 0   COMP-3.              
007801 77  WS-CHK-IDKOLLI              PIC X(5)  VALUE SPACES.                  
007901*                                                                         
008001 01  ALL-SPACE.                                                           
008101     03  FILLER                  PIC X(80)  VALUE SPACE.                  
008201 01  ALL-PLUS.                                                            
008301     03  FILLER                  PIC X(80)  VALUE ALL '+'.                
008401*                                                                         
008501 01  WS-KDMATT                   PIC X.                                   
008601     88 US-MEASUREMENT           VALUE 'U'.                               
008701     88 SIS-MEASUREMENT          VALUE 'S'.                               
008801                                                                          
008901 01  DYNAMISKA-SUBPROGRAM.                                                
009001     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010001     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010101     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
010201     03  W403PLAT                PIC X(8)    VALUE 'W403PLAT'.            
010301     03  W411DNOT                PIC X(8)    VALUE 'W411DNOT'.            
010401     03  WWOMVAND                PIC X(8)    VALUE 'WWOMVAND'.            
010501     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
010601     03  W403TMS1                PIC X(8)    VALUE 'W403TMS1'.            
010701     03  WL01TIDZ                PIC X(8)    VALUE 'WL01TIDZ'.            
010801     SKIP2                                                                
010901 01  FILLER                      PIC X(16)  VALUE 'W005INIT '.            
011001*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
011101*01 -COPY WMSGINIT                                                        
011201     SKIP2                                                                
011301 01  FILLER                      PIC X(16)  VALUE 'WL01TIDZ '.            
011401*01 -COPY WL01TIDZ -PRE TIDZ-                                             
011501                                                                          
011601 01  FILLER                      PIC X(16)  VALUE 'W403PLAT '.            
011701*01 -COPY W403PLAT                                                        
011801     SKIP2                                                                
011901 01  FILLER                      PIC X(16)  VALUE 'W411DNOT '.            
012001*01 -COPY W411DNOT                                                        
012101     SKIP2                                                                
012201 01  FILLER                      PIC X(16)  VALUE 'WWOMVAND '.            
012301*01 -COPY WWOMVAND                                                        
012401     SKIP2                                                                
012501 01  FILLER                      PIC X(16)  VALUE 'W403TMS1 '.            
012601*01 -COPY W403TMS1                                                        
012701     SKIP2                                                                
012801 01  TIHHMMSS-WS-8.                                                       
012901     03 TIHHMMSS-WS-6            PIC 9(6).                                
013001     03 FILLER                   PIC 9(2).                                
013101     SKIP3                                                                
013201                                                                          
013301 01  FILLER                      PIC X(08)  VALUE 'ARBFAELT'.             
013401 01  ARBETSFAELT.                                                         
013501   03  NUM-VKORDBTO              PIC S9(6)V9(1) VALUE ZERO.               
013601     SKIP2                                                                
013701 01  ARBETSAREA.                                                          
013801   03  ARB-ADRESS.                                                        
013901     05  ARB-ADFLGEO             PIC X(3) VALUE SPACE.                    
014001     05  FILLER                  PIC X(1) VALUE SPACE.                    
014101     05  ARB-ADFLOMR             PIC 9(3) VALUE ZERO.                     
014201     05  FILLER                  PIC X(1) VALUE SPACE.                    
014301     05  ARB-ADRUTNIV            PIC 9(3) VALUE ZERO.                     
014401     SKIP2                                                                
014501 01  WS-SPAR-FOER-DELETE-FAELT.                                           
014601     03  WS-SPAR-VKORDBTO-DLET   PIC S9(6)V9(1) COMP-3.                   
014701     03  WS-SPAR-VLORDBTO-DLET   PIC S9(4)V9(3) COMP-3.                   
014801     03  WS-SPAR-IDTRPTNR-DLET   PIC S9(3) COMP-3.                        
014901     03  WS-SPAR-DARFS-DLET      PIC 9(12).                               
015001     03  WS-SPAR-ADCLGEO-DLET.                                            
015101         05  WS-SPAR-IDDC-DLET     PIC X(02).                             
015201         05  WS-SPAR-ADFLGEO-DLET  PIC X(3).                              
015301     03  WS-SPAR-ADFLOMR-DLET      PIC S9(3) COMP-3.                      
015401     03  WS-SPAR-ADRUTNIV-DLET     PIC S9(3) COMP-3.                      
015501     03  WS-SPAR-DIHMODUL-DLET     PIC S9(3) COMP-3.                      
015601     03  WS-SPAR-DIDMODUL-DLET     PIC S9(3) COMP-3.                      
015701     03  WS-SPAR-ADVMODUL-DLET     PIC S9(3) COMP-3.                      
015801     03  WS-SPAR-ADHMODUL-DLET     PIC S9(3) COMP-3.                      
015901     EJECT                                                                
016001 01  FILLER                      PIC X(08)  VALUE 'DLINYCKL'.             
016101 01  NYCKLAR-TILL-DLI.                                                    
016201                                                                          
016301   03  W-WDE4F1KY-MIN-X.                                                  
016401     05 W-IDPRODNR-E4F-MIN        PIC S9(7)   VALUE ZERO  COMP-3.         
016501     05 W-IDKOLLI-E4F-MIN         PIC S9(5)   VALUE ZERO  COMP-3.         
016601     05 FILLER                    PIC X(22)   VALUE LOW-VALUE.            
016701                                                                          
016801   03  W-WDE4F1KY-MAX-X.                                                  
016901     05 W-IDPRODNR-E4F-MAX        PIC S9(7)   VALUE ZERO  COMP-3.         
017001     05 W-IDKOLLI-E4F-MAX         PIC S9(5)   VALUE ZERO  COMP-3.         
017101     05 FILLER                    PIC X(22)   VALUE HIGH-VALUE.           
017201                                                                          
017301   03  W-WDE421KY-X.                                                      
017401     05 W-IDPRODNR-X.                                                     
017501       07 W-IDPRODNR              PIC S9(7)   VALUE ZERO  COMP-3.         
017601     05 W-IDKOLLI-X.                                                      
017701       07 W-IDKOLLI               PIC S9(5)   VALUE ZERO  COMP-3.         
017801                                                                          
017901   03  W-IDPURAD-X.                                                       
018001       07 W-IDPURAD               PIC S9(5)   VALUE ZERO  COMP-3.         
018101                                                                          
018201   03  W-WDE401KY-X.                                                      
018301     05  W-IDDISTR-E401           PIC S9(5)   VALUE ZERO  COMP-3.         
018401     05  W-IDKUNDNR-E401          PIC S9(7)   VALUE ZERO  COMP-3.         
018501     05  W-IDKUNDRF-E401          PIC X(10)   VALUE SPACE.                
018601     05  W-IDPRODNR-E401          PIC S9(7)   VALUE ZERO  COMP-3.         
018701     05  W-IDPLKLST-E401          PIC S9(3)   VALUE ZERO  COMP-3.         
018801                                                                          
018901     03 W-WDQ5A1KY-MIN-X.                                                 
019001       05  W-WDQ5A1-IDGMTREF-MIN.                                         
019101         07 W-WDQ5A1-IDDISTR-MIN    PIC S9(05)  VALUE ZERO COMP-3.        
019201         07 W-WDQ5A1-IDKUNDNR-MIN   PIC S9(07)  VALUE ZERO COMP-3.        
019301         07 W-WDQ5A1-IDKUNDRF-GRP-MIN.                                    
019401           09 W-WDQ5A1-IDKUNDRF-MIN PIC  X(10).                           
019501           09 W-WDQ5A1-IDORDNR5 REDEFINES W-WDQ5A1-IDKUNDRF-MIN.          
019601             11 W-WDQ5A1-IDORDNR5-MIN PIC 9(05).                          
019701             11 FILLER             PIC  X(05).                            
019801           09 W-WDQ5A1-IDORDNR7 REDEFINES W-WDQ5A1-IDKUNDRF-MIN.          
019901             11 W-WDQ5A1-IDORDNR7-MIN PIC 9(07).                          
020001             11 FILLER             PIC  X(03).                            
020101       05 FILLER                   PIC X(17)   VALUE LOW-VALUE.           
020201                                                                          
020301     03 W-WDQ5A1KY-MAX-X.                                                 
020401       05  W-WDQ5A1-IDGMTREF-MAX.                                         
020501         07 W-WDQ5A1-IDDISTR-MAX    PIC S9(05)   VALUE ZERO               
020601                                                COMP-3.                   
020701         07 W-WDQ5A1-IDKUNDNR-MAX   PIC S9(07)   VALUE ZERO               
020801                                                COMP-3.                   
020901         07 W-WDQ5A1-IDKUNDRF-GRP-MAX.                                    
021001           09 W-WDQ5A1-IDKUNDRF-MAX PIC  X(10).                           
021101           09 W-WDQ5A1-IDORDNR5 REDEFINES W-WDQ5A1-IDKUNDRF-MAX.          
021201             11 W-WDQ5A1-IDORDNR5-MAX PIC 9(05).                          
021301             11 FILLER             PIC  X(05).                            
021401           09 W-WDQ5A1-IDORDNR7 REDEFINES W-WDQ5A1-IDKUNDRF-MAX.          
021501             11 W-WDQ5A1-IDORDNR7-MAX PIC 9(07).                          
021601             11 FILLER             PIC  X(03).                            
021701       05 FILLER                   PIC X(17)   VALUE HIGH-VALUE.          
021801                                                                          
021901     03  W-IDDC-B6-X.                                                     
022001         05 W-IDDC-B6                  PIC X(2).                          
023001                                                                          
023101*                                                                         
023201   03    W-4321-IDHTYP-X.                                                 
023301         05  W-4321-IDHTYP         PIC X(4)  VALUE '4321'.                
023401         05  W-4321-NYCKEL-VALFRI  PIC X(26) VALUE LOW-VALUE.             
023501                                                                          
023601                                                                          
023701     03  W-KDKOLLI-K5-X.                                                  
023801         05  W-KDKOLLI-K5        PIC X(8)    VALUE SPACE.                 
023901*                                                                         
024001   03    W-KDSEGKEY-X.                                                    
024101     05    W-KDSEGKEY                PIC X(1)  VALUE '1'.                 
024201                                                                          
024301     EJECT                                                                
024401*                                                                         
024501 01  FILLER                      PIC X(16)   VALUE 'RAD-TAB'.             
024601     SKIP2                                                                
024701 01  RAD-TAB.                                                             
024801*                                                                         
024901   03  FILLER                    OCCURS 500.                              
025001     05  TAB-IDRADNR             PIC S9(5)           COMP-3.              
025101     05  TAB-KVLEVART-MID        PIC S9(7)           COMP-3.              
025201     05  TAB-KVLEVART-REG        PIC S9(7)           COMP-3.              
025301     EJECT                                                                
025401 01  FILLER                      PIC X(16)   VALUE 'FG-TABELL'.           
025501     SKIP2                                                                
025601 01  FARLIGT-GODS-TABELL.                                                 
025701*                                                                         
025801   03  TABELL-POST OCCURS 10.                                             
025901     05  TAB-IDPSN               PIC  9(3)             VALUE 0.           
026001     05  TAB-VKART-FG            PIC S9(7)      COMP-3 VALUE 0.           
026101     05  TAB-VLFG                PIC S9(4)V9(3) COMP-3 VALUE 0.           
026201     EJECT                                                                
026301 01  FILLER                      PIC X(16)   VALUE 'WDECAREA'.            
026401*01  -COPY WDECAREA                                                       
026501     EJECT                                                                
026601 01  TEST-IDDISTR        PIC S9(5)  COMP-3.                               
026701                                                                          
026801*01  FILLER    -COPY WWDIST03   -RED TEST-IDDISTR.                        
026901                                                                          
027001*01  FILLER    -COPY WWDIST07   -RED TEST-IDDISTR.                        
027101                                                                          
027201*01  FILLER    -COPY WWDIST21   -RED TEST-IDDISTR.                        
027301                                                                          
027401*01  FILLER    -COPY WWDIST85   -RED TEST-IDDISTR.                        
027501     EJECT                                                                
027601* ÄT-FRAKTSEDEL SVERIGE KLASS4                                            
027701*01    FILLER  -COPY WWDIS128   -RED TEST-IDDISTR.                        
027801     EJECT                                                                
027901 01  FILLER                      PIC X(08)  VALUE 'FRAKT1  '.             
028001*   -COPY WWFRAKT1                                                        
028101     EJECT                                                                
028201*ÄT-SLUT-FRAKTSEDEL SVERIGE KLASS4                                        
028301     EJECT                                                                
028401 01  TILL-KOLLI-MEDDELANDE.                                               
028501*                                                                         
028601   03  TILL-KOLLI-1.                                                      
028701     05  FILLER                  PIC X(1)    VALUE                        
028801             'S'.                                                         
028901     05  FILLER                  PIC X(1)    VALUE                        
029001             'M'.                                                         
029101   03  FILLER                    REDEFINES TILL-KOLLI-1.                  
029201     05  TILL-KOLLI-SAKNAS       PIC X(1)    OCCURS 2.                    
029301     SKIP3                                                                
029401   03  TILL-KOLLI-2.                                                      
029501     05  FILLER                  PIC X(1)    VALUE                        
029601             'F'.                                                         
029701     05  FILLER                  PIC X(1)    VALUE                        
029801             'I'.                                                         
029901   03  FILLER                    REDEFINES TILL-KOLLI-2.                  
030001     05  TILL-KOLLI-FAKTURERAT   PIC X(1)    OCCURS 2.                    
030101     SKIP3                                                                
030201 01  MESSAGE-CODES.                                                       
030301     03  INF-MORE-PRESS-ENTER    PIC X(3)    VALUE '011'.                 
030401     03  INF-WRONG-DC            PIC X(3)    VALUE '023'.                 
030501     03  INF-CASE-UPDATED        PIC X(3)    VALUE '403'.                 
030601     03  ERR-WRONG-KEY           PIC X(3)    VALUE '043'.                 
030701     03  ERR-HILITE-FIELDS-WRONG     PIC X(3)    VALUE '020'.             
030801     03  ERR-HILITE-FIELDS-WRONG-WEB PIC X(3)    VALUE '920'.             
030901     03  ERR-ORDER-MISSING       PIC X(3)    VALUE '041'.                 
031001     03  ERR-CASE-MISSING        PIC X(3)    VALUE '041'.                 
031101     03  ERR-CASE-INVOICED-OR-RELEASED                                    
031201                                 PIC X(3)    VALUE '400'.                 
031301     03  ERR-CASE-REPORTED       PIC X(3)    VALUE '149'.                 
031401     03  ERR-CASE-LINES-MISSING  PIC X(3)    VALUE '399'.                 
031501     03  ERR-WRONG-ADDRESS       PIC X(3)    VALUE '023'.                 
031601     03  ERR-ADDRESS-HANDLING    PIC X(3)    VALUE '385'.                 
031701     03  ERR-INVALID-COMBINATION PIC X(3)    VALUE '401'.                 
031801     03  ERR-ORDER-CHANGED       PIC X(3)    VALUE '402'.                 
031901     03  ERR-CONFLICT-CHOICES    PIC X(3)    VALUE '046'.                 
032001     03  ERR-ZERO-NOT-ALLOWED    PIC X(3)    VALUE '724'.                 
032101     03  ERR-MIXED-CASE          PIC X(3)    VALUE '831'.                 
032201     03  ERR-UNAUTHORIZED        PIC X(3)    VALUE '00A'.                 
032301     03  ERR-GROSS-WEIGHT        PIC X(3)    VALUE '149'.                 
032401     EJECT                                                                
032501*                                                                         
032601 01  FILLER                      PIC X(16)   VALUE 'SPAR-KOLLI'.          
032701     SKIP2                                                                
032801 01  SPAR-KOLLI.                                                          
032901*                                                                         
033001     03  SPAR-KOLLI-KVORDRAD     PIC S9(5)      VALUE ZERO COMP-3.        
033101     03  SPAR-KOLLI-TIPACKN      PIC S9(7)      VALUE ZERO COMP-3.        
033201     03  SPAR-KOLLI-KDFARLIG     PIC S9(1)      VALUE ZERO COMP-3.        
033301     03  SPAR-KOLLI-KVFLAMP      PIC S9(2)V9(1) VALUE ZERO COMP-3.        
033401     03  SPAR-KOLLI-VKORDBTO     PIC S9(6)V9(1) VALUE ZERO COMP-3.        
033501     03  SPAR-KOLLI-VKORDNTO     PIC S9(6)V9(1) VALUE ZERO COMP-3.        
033601     03  SPAR-KOLLI-VLORDBTO     PIC S9(4)V9(3) VALUE ZERO COMP-3.        
033701     03  SPAR-KOLLI-IDTRPLOS     PIC  9(3)      VALUE ZERO.               
033801     03  SPAR-KOLLI-IDTRPVAR     PIC  9(2)      VALUE ZERO.               
033901     03  SPAR-KOLLI-KDKOLSTA     PIC S9         VALUE ZERO COMP-3.        
034001     03  SPAR-KOLLI-IDPLOCK      PIC S9(7)      VALUE ZERO COMP-3.        
034101     03  WS-KOLLI-KDKOLSTA       PIC  9.                                  
034201     SKIP3                                                                
034301 01  FILLER                      PIC X(16)   VALUE 'SPAR-ORAD '.          
034401     SKIP2                                                                
034501 01  SPAR-ORAD.                                                           
034601*                                                                         
034701     03  SPAR-ORAD-PRARTNTO      PIC S9(7)V9(2) VALUE ZERO COMP-3.        
034801     03  SPAR-ORAD-PRAVCOST      PIC S9(7)V9(2) VALUE ZERO COMP-3.        
034901     03  SPAR-ORAD-PRARTNTO-LOC  PIC S9(7)V9(2) VALUE ZERO COMP-3.        
035001     03  SPAR-ORAD-PRARTNTO-LOCPREL                                       
035101                                 PIC S9(7)V9(2) VALUE ZERO COMP-3.        
035201     03  SPAR-ORAD-VKARTNTO      PIC S9(4)V9(3) VALUE ZERO COMP-3.        
035301     03  SPAR-ORAD-KDFARLIG      PIC S9(1)      VALUE ZERO COMP-3.        
035401     03  SPAR-ORAD-KVFLAMP       PIC S9(2)V9(1) VALUE ZERO COMP-3.        
035501     03  SPAR-ORAD-KDVALISO      PIC X(3)       VALUE SPACE.              
035601     03  SPAR-ORAD-KDVALISO-EXP  PIC X(3)       VALUE SPACE.              
035701     EJECT                                                                
035801 01  FILLER                      PIC X(16)   VALUE 'SPAR-ORAD-FG'.        
035901     SKIP2                                                                
036001 01  SPAR-ORAD-FG-UPPG.                                                   
036101     03  SPAR-ORAD-IDPSN         PIC  9(3)      VALUE ZERO.               
036201     03  SPAR-ORAD-VKART-FG      PIC S9(7)      VALUE ZERO COMP-3.        
036301     03  SPAR-ORAD-VLFG          PIC S9(4)V9(3) VALUE ZERO COMP-3.        
036401     03  SPAR-ORAD-SUEQFG        PIC S9(3)V9(4) VALUE ZERO COMP-3.        
036501*                                                                         
036601 01  TOTAL-SUEQFG                PIC S9(3)V9(4) VALUE ZERO COMP-3.        
036701     EJECT                                                                
036801 01  FILLER                      PIC X(16)   VALUE 'SWITCHAR'.            
036901     SKIP3                                                                
037001 01  SWITCHAR.                                                            
037101*                                                                         
037201     03  DATA-INMATAD-SW         PIC X(1)    VALUE 'N'.                   
037301       88  DATA-INMATAD                      VALUE 'J'.                   
037401*                                                                         
037501     03  DATA-RAETT-SW           PIC X(1)    VALUE 'J'.                   
037601       88  DATA-RAETT                        VALUE 'J'.                   
037701*                                                                         
037801     03  DATA-GODKAEND-SW        PIC X(1)    VALUE 'J'.                   
037901       88  DATA-GODKAEND                     VALUE 'J'.                   
038001       88  DATA-EJ-GODKAEND                  VALUE 'N'.                   
038101*                                                                         
038201     03  NYCKLAR-RAETT-SW        PIC X(1)    VALUE 'N'.                   
038301       88  NYCKLAR-RAETT                     VALUE 'J'.                   
038401*                                                                         
038501     03  SAMMA-NYCKLAR-SW        PIC X(1)    VALUE 'N'.                   
038601       88  SAMMA-NYCKLAR                     VALUE 'J'.                   
038701*                                                                         
038801     03  UPPDAT-TILLAATEN-SW     PIC X(1)    VALUE 'N'.                   
038901       88  UPPDAT-TILLAATEN                  VALUE 'J'.                   
039001*                                                                         
039101     03  REGISTER-OK-SW          PIC X(1)    VALUE 'J'.                   
039201       88  REGISTER-OK                       VALUE 'J'.                   
039301*                                                                         
039401     03  FLYTTA-ALLA-SW          PIC X(1)    VALUE 'N'.                   
039501       88  FLYTTA-ALLA                       VALUE 'J'.                   
039601*                                                                         
039701     03  FLYTTA-RAD-SW           PIC X(1)    VALUE 'N'.                   
039801       88  FLYTTA-RAD                        VALUE 'J'.                   
039901*                                                                         
040001     03  NYTT-KOLLI-SW           PIC X(1)    VALUE 'N'.                   
040101       88  NYTT-KOLLI                        VALUE 'J'.                   
040201*                                                                         
040301     03  TILL-KOLLI-OK-SW        PIC X(1)    VALUE 'J'.                   
040401       88  TILL-KOLLI-OK                     VALUE 'J'.                   
040501*                                                                         
040601     03  FOERSTA-RAD-SW          PIC X(1)    VALUE 'J'.                   
040701       88  FOERSTA-RAD                       VALUE 'J'.                   
040801*                                                                         
040901     03  FELTEXT-UTLAGD-SW       PIC X(1)    VALUE 'N'.                   
041001       88  FELTEXT-UTLAGD                    VALUE 'J'.                   
041101     EJECT                                                                
041201******************************************************************        
041301*                                                                         
041401*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
041501*                                                                         
041601 01  FILLER                      PIC X(16)   VALUE 'MFS-WS'.              
041701     SKIP3                                                                
041801*01  -COPY WMFSAREA                                                       
041901     EJECT                                                                
042001                                                                          
042101*01  XXJK  -COPY WDGX4322    -PRE XXJK-                                   
042201     EJECT                                                                
042301******************************************************************        
042401*                                                                         
042501*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
042601*                                                                         
042701 01  IMS-WS.                                                              
042801   03  FILLER                    PIC X(16)   VALUE 'IMS-WS     '.         
042901     SKIP3                                                                
043001*                        **** STATUS-KOD FRÅN IMS                         
043101   03  STATUS-WS                 PIC X(2).                                
043201     88  SEGMENT-FINNS                       VALUE '  '.                  
043301     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
043401     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
043501     88  BASEN-SLUT                          VALUE 'GB'.                  
043601     SKIP3                                                                
043701   03  GODK-STATUSKODER.                                                  
043801     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
043901     SKIP3                                                                
044001 01    SSA1                      PIC X(192).                              
044101 01    SSA2                      PIC X(64).                               
044201 01    SSA3                      PIC X(64).                               
044301     EJECT                                                                
044401*                            IMS FUNKTIONSKODER                           
044501*01    -COPY W0003                                                        
044601     SKIP3                                                                
044701                                                                          
044801*                            DLI INPUT-OUTPUT AREA                        
044901 01  FILLER                      PIC X(16) VALUE 'DLI-IO-E4F1'.           
045001 01  DLI-IO-E4F1.                                                         
045101*  03  -COPY WDE4F1                                                       
045201     EJECT                                                                
045301 01  FILLER                      PIC X(16) VALUE 'DLI-IO-E411-21'.        
045401 01  DLI-IO-E411-21.                                                      
045501   03  DLI-IO-E411.                                                       
045601*    05  -COPY WDE411                                                     
045701   03  DLI-IO-E421.                                                       
045801*    05  -COPY WDE421                                                     
045901     EJECT                                                                
046001 01  FILLER                      PIC X(16) VALUE 'DLI-IO-E601'.           
046101 01  DLI-IO-E601.                                                         
046201*  03  -COPY WDE601                                                       
046301     EJECT                                                                
046401 01  FILLER                      PIC X(16) VALUE 'DLI-IO-E611'.           
046501 01  DLI-IO-E611.                                                         
046601*  03  -COPY WDE611                                                       
046701     EJECT                                                                
046801 01  FILLER                      PIC X(16) VALUE 'DLI-IO-E621'.           
046901 01  DLI-IO-WDE621.                                                       
047001*  03  -COPY WDE621                                                       
047101     EJECT                                                                
047201 01  FILLER                      PIC X(16) VALUE 'DLI-IO-Q5A1'.           
047301 01  DLI-IO-Q5A1.                                                         
047401*  03  -COPY WDQ5A1                                                       
047501                                                                          
047601 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
047701 01   DLI-IO-AREA-B601.                                                   
047801*     03  -COPY WDB601                                                    
047901     EJECT                                                                
048001                                                                          
048101 01  DLI-IO-AREA4.                                                        
048201     03  IO-AREA4                PIC X(100)  VALUE SPACE.                 
048301*                                                                         
048401*    03  WLXXJK01  -COPY WDGX01      -PRE 4321-  -RED IO-AREA4            
048501*    03  WLXXJK11  -COPY WDGX4322    -RED IO-AREA4                        
048601     EJECT                                                                
048701 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK501'.                      
048801 01  DLI-IO-WDK501.                                                       
048901*    03  -COPY WDK501                                                     
049001 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDR501'.              
049101 01  DLI-IO-WDR501.                                                       
049201*    03   -COPY WDGX01                                                    
049301 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDGX0102'.            
049401 01  DLI-IO-WDGX0102.                                                     
049501*    03   -COPY WDGX0102                                                  
049601                                                                          
049701 01  FILLER               PIC X(08)   VALUE 'REQUAREA'.                   
049801 LINKAGE SECTION.                                                         
049901     SKIP3                                                                
050001 01  REQU-AREA.                                                           
050101*    03 -COPY WZ01REQ2                                                    
050201*    03 -COPY W40343I2                                                    
050301     EJECT                                                                
050401 01  RESP-AREA.                                                           
050501*    03 -COPY WZ01RES2                                                    
050601*    03 -COPY W40343O1                                                    
050701     EJECT                                                                
050801 01  MAX-KVRADER                 PIC S9(4) COMP.                          
050901 01  WS-KDTRANS                  PIC X(6).                                
051000*01  -COPY W0009     -PRE MSG-                                            
051100     EJECT                                                                
051200 01  TMS-CRE-PCB                 PIC X.                                   
051300 01  TMS-DEL-PCB                 PIC X.                                   
051400 01  ATAB-PCB                    PIC X.                                   
051500*01  -COPY W0008     -PRE WDP7-                                           
051600     05  FILLER                  PIC X.                                   
051700     EJECT                                                                
051800*01  -COPY W0008     -PRE WDE6-                                           
051900     05  FILLER                  PIC X(1).                                
052000     SKIP3                                                                
052100*01  -COPY W0008     -PRE WDE4FSEQ-                                       
052200     05  FILLER                  PIC X(1).                                
052300     SKIP3                                                                
052400*01  -COPY W0008     -PRE WDE4-                                           
052500     05  FILLER                  PIC X(1).                                
052600     EJECT                                                                
052700*01  -COPY W0008     -PRE WDE4F-                                          
052800     05  FILLER                  PIC X(1).                                
052900     EJECT                                                                
053000*01  -COPY W0008     -PRE WDQ5A-                                          
053100     05  FILLER                  PIC X(1).                                
053200     EJECT                                                                
053300*01  -COPY W0008     -PRE WDB6-                                           
053400     05  FILLER                  PIC X.                                   
053500     EJECT                                                                
053600*01  -COPY W0008     -PRE PLATS-DM-                                       
053700     05  FILLER                  PIC X.                                   
053800     EJECT                                                                
053900*01  -COPY W0008     -PRE PLATS-DN-                                       
054000     05  FILLER                  PIC X.                                   
054100     EJECT                                                                
054200*01  -COPY W0008     -PRE PLATS-DP-                                       
054300     05  FILLER                  PIC X.                                   
054400     EJECT                                                                
054500*01  -COPY W0008     -PRE PLATS-DO-                                       
054600     05  FILLER                  PIC X.                                   
054700     EJECT                                                                
054800*01  -COPY W0008     -PRE PLATS-WDE6C-                                    
054900     05  FILLER                  PIC X.                                   
055000     EJECT                                                                
055100*01  -COPY W0008     -PRE PLATS-GMTC-                                     
055200     05  FILLER                  PIC X.                                   
055300     EJECT                                                                
055400*01  -COPY W0008     -PRE PLATS-WDB6-                                     
055500     05  FILLER                  PIC X.                                   
055600     EJECT                                                                
055700 01  DNOT-ORQP-PCB               PIC X.                                   
055800 01  DNOT-ORQP2-PCB              PIC X.                                   
055900 01  DNOT-ORQP3-PCB              PIC X.                                   
056000 01  DNOT-4013-PCB               PIC X.                                   
056100 01  DNOT-BENA-PCB               PIC X.                                   
056200     EJECT                                                                
056300 01  TMS-1165-PCB                PIC X.                                   
056400 01  TMS-4141-PCB                PIC X.                                   
056500 01  TMS-WDB2-PCB                PIC X.                                   
056600 01  TMS-WDB6-PCB                PIC X.                                   
056700 01  TMS-WDD3-PCB                PIC X.                                   
056800 01  TMS-WDB1-PCB                PIC X.                                   
056900 01  TMS-WDE4A-PCB               PIC X.                                   
057000 01  TMS-WDE4F-PCB               PIC X.                                   
057100 01  TMS-WDQ2-PCB                PIC X.                                   
057200 01  TMS-WDQ3-PCB                PIC X.                                   
057300 01  TMS-WDK6-PCB                PIC X.                                   
057400 01  TMS-WDE6-PCB                PIC X.                                   
057500 01  TMS-WDK5-PCB                PIC X.                                   
057600 01  TMS-WDQ2C-PCB               PIC X.                                   
057700     EJECT                                                                
057800*01  -COPY W0008     -PRE XXJK-                                           
057900     05  FILLER                  PIC X.                                   
058000                                                                          
058100*01  -COPY W0008  -PRE WDK5-                                              
058200     05  FILLER                  PIC X.                                   
058300     EJECT                                                                
058400                                                                          
058500 PROCEDURE DIVISION USING REQU-AREA RESP-AREA                             
058601                 MAX-KVRADER  WS-KDTRANS                                  
058700                 MSG-PCB    TMS-CRE-PCB TMS-DEL-PCB                       
058800                 ATAB-PCB WDP7-PCB WDE6-PCB  WDE4FSEQ-PCB                 
058900                 WDE4-PCB WDE4F-PCB WDQ5A-PCB WDB6-PCB                    
059000                 PLATS-DM-PCB PLATS-DN-PCB PLATS-DP-PCB                   
059100                 PLATS-DO-PCB PLATS-WDE6C-PCB PLATS-GMTC-PCB              
059200                 PLATS-WDB6-PCB                                           
059300                 DNOT-ORQP-PCB DNOT-ORQP2-PCB                             
059400                 DNOT-ORQP3-PCB DNOT-4013-PCB                             
059500                 DNOT-BENA-PCB                                            
059600                 TMS-1165-PCB TMS-4141-PCB TMS-WDB2-PCB                   
059700                 TMS-WDB6-PCB TMS-WDD3-PCB TMS-WDB1-PCB                   
059800                 TMS-WDE4A-PCB TMS-WDE4F-PCB TMS-WDQ2-PCB                 
059900                 TMS-WDQ3-PCB TMS-WDK6-PCB TMS-WDE6-PCB                   
060000                 TMS-WDK5-PCB TMS-WDQ2C-PCB                               
060100                 XXJK-PCB WDK5-PCB.                                       
060200                                                                          
060300 MAIN SECTION.                                                            
060400     ENTRY 'DLITCBL' USING REQU-AREA RESP-AREA                            
060501                 MAX-KVRADER  WS-KDTRANS                                  
060601                 MSG-PCB    TMS-CRE-PCB TMS-DEL-PCB                       
060700                 ATAB-PCB WDP7-PCB WDE6-PCB  WDE4FSEQ-PCB                 
060800                 WDE4-PCB WDE4F-PCB WDQ5A-PCB WDB6-PCB                    
060900                 PLATS-DM-PCB PLATS-DN-PCB PLATS-DP-PCB                   
061000                 PLATS-DO-PCB PLATS-WDE6C-PCB PLATS-GMTC-PCB              
061100                 PLATS-WDB6-PCB                                           
061200                 DNOT-ORQP-PCB DNOT-ORQP2-PCB                             
061300                 DNOT-ORQP3-PCB DNOT-4013-PCB                             
061400                 DNOT-BENA-PCB                                            
061500                 TMS-1165-PCB TMS-4141-PCB TMS-WDB2-PCB                   
061600                 TMS-WDB6-PCB TMS-WDD3-PCB TMS-WDB1-PCB                   
061700                 TMS-WDE4A-PCB TMS-WDE4F-PCB TMS-WDQ2-PCB                 
061800                 TMS-WDQ3-PCB TMS-WDK6-PCB TMS-WDE6-PCB                   
061900                 TMS-WDK5-PCB TMS-WDQ2C-PCB                               
062000                 XXJK-PCB WDK5-PCB.                                       
062100                                                                          
062200******************************************************************        
062300*                                                                *        
062400*WDE4FSEQ-PCB                                                    *        
062500*  & WDE6-PCB  : ANVÄNDS VID LÄSNINGAR OCH                       *        
062600*                UPPDATERAR KOLLIT SOM MAN FLYTTAR FRÅN          *        
062700*                                                                *        
062800*    WDE4-PCB  : UPPDATERAR KOLLIT SOM MAN FLYTTAR TILL          *        
062900*  & WDE6                                                        *        
063000******************************************************************        
063100                                                                          
063200     PERFORM A-INIT                                                       
063300     PERFORM B-CHECK-KEYS                                                 
063400     IF REQU-UPDATE                                                       
063500       PERFORM G-KONTROLL-ALL-DATA                                        
063600       IF REGISTER-OK                                                     
063700         IF DATA-GODKAEND                                                 
063800           IF DATA-INMATAD                                                
063900             IF DATA-RAETT                                                
064000               PERFORM H-UPPDATERING                                      
064100             ELSE                                                         
064200               PERFORM S14-FEL-INMATAT                                    
064300*      CALL FELLOG                                                        
064400             END-IF                                                       
064500           ELSE                                                           
064600             PERFORM D-BLAEDDRA                                           
064700           END-IF                                                         
064800         ELSE                                                             
064900           PERFORM S15-FEL-SKYDDADE                                       
065000         END-IF                                                           
065100       END-IF                                                             
065200     ELSE                                                                 
065300       MOVE ZERO                 TO RESP-KVRADER                          
065400       IF NYCKLAR-RAETT                                                   
065500         MOVE IDPRODNR-WS        TO W-IDPRODNR                            
065600         PERFORM IMS-GU-WDE601                                            
065700         IF SEGMENT-FINNS                                                 
065800           MOVE IDKOLLI-WS       TO W-IDKOLLI                             
065900           PERFORM IMS-GNP-WDE611                                         
066000           IF SEGMENT-FINNS AND                                           
066100*LK CHECK FOR NON ZERO KOLLI-DIKOLLI IS ADDED AS REPLACEMENT OF           
066200*LK OF KOLLI-KDKOLSTA WHICH IS NOT 1 EVEN AFTER PACKING IN API            
066300              KOLLI-DIKOLLIL NOT = ZERO                                   
066400             IF KOLLI-KDKOLSTA < 6                                        
066500               IF KOLLI-IDKOLLI-SAMP > ZERO                               
066600               AND KOLLI-KDSTASKLI   > SPACE                              
066700                  PERFORM S12-MIXED-CASE                                  
066800               ELSE                                                       
066900                  IF KOLLI-KDKOLSTA < 3                                   
067000                    PERFORM E-VISA-ORDER-KOLLI                            
067100                  ELSE                                                    
067200                    PERFORM S13-KOLLI-FAKTURERAT                          
067300                  END-IF                                                  
067400               END-IF                                                     
067500             ELSE                                                         
067600               PERFORM S13-KOLLI-FAKTURERAT                               
067700             END-IF                                                       
067800           ELSE                                                           
067900             PERFORM S11-KOLLI-SAKNAS                                     
068000           END-IF                                                         
068100         ELSE                                                             
068200           PERFORM S11-ORDER-SAKNAS                                       
068300         END-IF                                                           
068400       ELSE                                                               
068500         PERFORM S17-NYCKLAR-FEL                                          
068600       END-IF                                                             
068700     END-IF                                                               
068800                                                                          
068900     GOBACK                                                               
069000     .                                                                    
069100     EJECT                                                                
069200 A-INIT SECTION.                                                          
069300                                                                          
069400     IF REQU-KVRADER NOT NUMERIC                                          
069500       MOVE ZERO      TO REQU-KVRADER                                     
069600     END-IF                                                               
069700                                                                          
069800     MOVE ALL '+'                TO RESP-W40343O1                         
069900     MOVE MFS-FORMATETS-ATTR     TO RESP-IDKOLLI-ALLA-ATTR                
070000                                                                          
070100*    MOVE 001                    TO RESP-IDMSGVER                         
070200     MOVE SPACE                  TO RESP-IDMSG-ERROR                      
070300                                    RESP-IDMSG-INFO                       
070400                                    RESP-IDELMT-ERROR                     
070500                                                                          
070600     IF REQU-KDMATT = '+'                                                 
070700       MOVE ALL '+'              TO MSGI-WMSGINIT                         
070800       MOVE '001'                TO MSGI-KDCALL                           
070900       MOVE REQU-IDUSER          TO MSGI-IDUSER                           
071000       MOVE '4343'               TO MSGI-IDTRANS                          
071100       MOVE MSG-LTERM-NAME       TO MSGI-IDLTERM-USER                     
071200       CALL W005INIT          USING MSGI-WMSGINIT WDP7-PCB                
071300       MOVE MSGI-KDMATT          TO WS-KDMATT                             
071400     ELSE                                                                 
071500       MOVE REQU-KDMATT          TO WS-KDMATT                             
071600     END-IF                                                               
071700                                                                          
071800     INITIALIZE TMS-W403TMS1                                              
071900     MOVE +0    TO TMS-IX                                                 
072000                                                                          
072100     MOVE REQU-KVRADER           TO RESP-KVRADER                          
072200     IF REQU-IDSPRAK = 'SV'                                               
072300       MOVE 1 TO SPRAK-IX                                                 
072400     ELSE                                                                 
072500       MOVE 2 TO SPRAK-IX                                                 
072600     END-IF                                                               
072700                                                                          
072800     .                                                                    
072900     EJECT                                                                
073000 B-CHECK-KEYS SECTION.                                                    
073100                                                                          
073200     MOVE REQU-IDPRODNR-KEY      TO IDPRODNR-WS                           
073300     MOVE REQU-IDKOLLI-KEY       TO IDKOLLI-WS                            
073400     MOVE IDKOLLI-WS             TO WS-MID-IDKOLLI                        
073500                                    TMS-IDKOLLI(1)                        
073600                                                                          
073700     IF IDPRODNR-WS  NUMERIC AND                                          
073800        IDKOLLI-WS  NUMERIC AND                                           
073900        REQU-IDDC-KEY >= SPACE                                            
074000       MOVE JA                   TO NYCKLAR-RAETT-SW                      
074100     END-IF                                                               
074200                                                                          
074300     IF NYCKLAR-RAETT                                                     
074400       MOVE REQU-IDDC-KEY        TO W-IDDC-B6                             
074500                                    TMS-IDDC                              
074600       PERFORM IMS-GU-WDB601                                              
074700     END-IF                                                               
074800                                                                          
074900     .                                                                    
075000     EJECT                                                                
075100 G-KONTROLL-ALL-DATA  SECTION.                                            
075200                                                                          
075300     PERFORM GA-KONTROLL-SKYDDADE                                         
075400                                                                          
075500     IF DATA-GODKAEND                                                     
075600       MOVE IDPRODNR-WS TO W-IDPRODNR                                     
075700       PERFORM IMS-GU-WDE601                                              
075800       IF SEGMENT-FINNS                                                   
075901         MOVE IDKOLLI-WS       TO W-IDKOLLI                               
076001         PERFORM IMS-GNP-WDE611                                           
076101         IF SEGMENT-FINNS                                                 
076200           IF VORD-IDDC = REQU-IDDC-KEY                                   
076300             PERFORM S03-GET-INPUT-IDPLKLST                               
076400             PERFORM GB-KONTROLL-FLYTTA-ALLA                              
076500             PERFORM GC-KONTROLL-FLYTTA-RAD                               
076600             IF DATA-RAETT                                                
076700               IF FLYTTA-ALLA AND                                         
076800                 FLYTTA-RAD                                               
076900                 MOVE NEJ   TO DATA-RAETT-SW                              
077000                 MOVE ERR-CONFLICT-CHOICES TO RESP-IDMSG-ERROR            
077100                 MOVE 'CHOICES'          TO RESP-IDELMT-ERROR             
077200                 MOVE JA                 TO FELTEXT-UTLAGD-SW             
077300               ELSE                                                       
077400                 IF FLYTTA-RAD                                            
077500                   PERFORM GD-KONTROLL-ANTAL-MOT-REG                      
077600                 END-IF                                                   
077700               END-IF                                                     
077800             END-IF                                                       
077900                                                                          
078000             IF DATA-GODKAEND AND                                         
078100               REGISTER-OK AND                                            
078200               DATA-INMATAD                                               
078300               PERFORM GE-KONTROLL-NYTT-KOLLI                             
078400             END-IF                                                       
078500           ELSE                                                           
078600             MOVE NEJ TO DATA-RAETT-SW                                    
078700             MOVE INF-WRONG-DC       TO RESP-IDMSG-INFO                   
078800             MOVE 'IDDC'             TO RESP-IDELMT-ERROR                 
078900           END-IF                                                         
079000         ELSE                                                             
079100           PERFORM S11-KOLLI-SAKNAS                                       
079201         END-IF                                                           
079301       ELSE                                                               
079401         PERFORM S11-ORDER-SAKNAS                                         
079500       END-IF                                                             
079600     END-IF                                                               
079700                                                                          
079800     .                                                                    
079900     EJECT                                                                
080000 GA-KONTROLL-SKYDDADE  SECTION.                                           
080100     SKIP3                                                                
080200     IF REQU-IDPRODNR-KEY NOT NUMERIC                                     
080300       INSPECT REQU-IDPRODNR-KEY                                          
080400               REPLACING LEADING SPACE BY ZERO                            
080500       IF REQU-IDPRODNR-KEY NOT NUMERIC                                   
080600         MOVE NEJ  TO DATA-GODKAEND-SW                                    
080700       MOVE 'REQU-IDPRODNR-KEY '     TO FELTEXT                           
080800       END-IF                                                             
080900     END-IF                                                               
081000                                                                          
081100     IF REQU-IDKOLLI-KEY  NOT NUMERIC                                     
081200       INSPECT REQU-IDKOLLI-KEY                                           
081300               REPLACING LEADING SPACE BY ZERO                            
081400       IF REQU-IDKOLLI-KEY NOT NUMERIC                                    
081500         MOVE NEJ  TO DATA-GODKAEND-SW                                    
081600         MOVE 'REQU-IDKOLLI-KEY '   TO FELTEXT                            
081700       END-IF                                                             
081800     END-IF                                                               
081900                                                                          
082000     IF REQU-IDRADNR-FOM  NOT NUMERIC                                     
082100       INSPECT REQU-IDRADNR-FOM                                           
082200               REPLACING LEADING SPACE BY ZERO                            
082300       IF REQU-IDRADNR-FOM NOT NUMERIC                                    
082400         MOVE NEJ  TO DATA-GODKAEND-SW                                    
082500         MOVE 'REQU-IDRADNR-FOM '   TO FELTEXT                            
082600       END-IF                                                             
082700     END-IF                                                               
082800                                                                          
082900     MOVE 1   TO MIX                                                      
083000     PERFORM UNTIL MIX > REQU-KVRADER                                     
083100     OR DATA-EJ-GODKAEND                                                  
083200       IF REQU-FLNOLLAD-LINE (MIX) = JA OR NEJ                            
083300         CONTINUE                                                         
083400       ELSE                                                               
083500         MOVE NEJ  TO DATA-GODKAEND-SW                                    
083600       MOVE 'REQU-FLNOLLAD-LINE '   TO FELTEXT                            
083700       END-IF                                                             
083800                                                                          
083900       IF REQU-IDARTNR-LINE (MIX) NOT NUMERIC                             
084000         IF REQU-IDARTNR-LINE (MIX) = '00000000 '                         
084100           MOVE ZERO     TO REQU-IDARTNR-LINE (MIX)                       
084200         ELSE                                                             
084300           INSPECT REQU-IDARTNR-LINE (MIX)                                
084400                   REPLACING LEADING SPACE BY ZERO                        
084500           IF REQU-IDARTNR-LINE (MIX) NOT NUMERIC                         
084600             MOVE NEJ  TO DATA-GODKAEND-SW                                
084700           MOVE 'REQU-IDARTNR-LINE ' TO FELTEXT                           
084800           END-IF                                                         
084900         END-IF                                                           
085000       END-IF                                                             
085100       ADD 1  TO MIX                                                      
085200     END-PERFORM                                                          
085300                                                                          
085400     .                                                                    
085500     SKIP2                                                                
085600 GB-KONTROLL-FLYTTA-ALLA  SECTION.                                        
085700                                                                          
085800     IF REQU-FLJANEJ-ALLA = SPACE                                         
085900        MOVE ALL '+'   TO REQU-FLJANEJ-ALLA                               
086000     END-IF                                                               
086100                                                                          
086200     IF REQU-IDKOLLI-ALLA = '0000 '                                       
086300        MOVE ALL '+'     TO REQU-IDKOLLI-ALLA                             
086400     END-IF                                                               
086500                                                                          
086600     IF REQU-FLJANEJ-ALLA = ALL '+'                                       
086700       MOVE MFS-FORMATETS-ATTR TO                                         
086800            RESP-FLJANEJ-ALLA-ATTR                                        
086900       IF REQU-IDKOLLI-ALLA = ALL '+'                                     
087000         MOVE MFS-FORMATETS-ATTR    TO                                    
087100              RESP-IDKOLLI-ALLA-ATTR                                      
087200       ELSE                                                               
087300         MOVE MFS-NUM-FAELT-FEL  TO                                       
087400              RESP-IDKOLLI-ALLA-ATTR                                      
087500         MOVE JA   TO DATA-INMATAD-SW                                     
087600         MOVE NEJ  TO DATA-RAETT-SW                                       
087700         MOVE JA   TO FLYTTA-ALLA-SW                                      
087800         MOVE 'FLJANEJ'            TO RESP-IDELMT-ERROR                   
087900       END-IF                                                             
088000     ELSE                                                                 
088100       MOVE JA TO DATA-INMATAD-SW                                         
088200       MOVE JA TO FLYTTA-ALLA-SW                                          
088300       IF REQU-FLJANEJ-ALLA = 'JA ' OR 'YES'                              
088400         MOVE MFS-ALFA-FAELT-RAETT                                        
088500             TO RESP-FLJANEJ-ALLA-ATTR                                    
088600         MOVE 'FLJANEJ'            TO RESP-IDELMT-ERROR                   
088700       ELSE                                                               
088800         MOVE MFS-ALFA-FAELT-FEL                                          
088900             TO RESP-FLJANEJ-ALLA-ATTR                                    
089000         MOVE NEJ TO DATA-RAETT-SW                                        
089100         MOVE 'FLJANEJ'            TO RESP-IDELMT-ERROR                   
089200       END-IF                                                             
089300                                                                          
089400       IF REQU-IDKOLLI-ALLA = ALL '+'                                     
089500         MOVE NEJ TO DATA-RAETT-SW                                        
089600         MOVE MFS-NUM-FAELT-FEL TO                                        
089700              RESP-IDKOLLI-ALLA-ATTR                                      
089800         MOVE 'IDKOLLI'            TO RESP-IDELMT-ERROR                   
089900       ELSE                                                               
090000         IF REQU-IDKOLLI-ALLA NUMERIC AND                                 
090100           REQU-IDKOLLI-ALLA > ZERO AND                                   
090200           REQU-IDKOLLI-ALLA NOT = IDKOLLI-WS                             
090300*LK                                                                       
090400           MOVE KOLLI-VKORDNTO-KOLLI TO VKORDNTO-WS                       
090500***                                                                       
090600           MOVE REQU-IDKOLLI-ALLA TO TMS-IDKOLLI(2)                       
090700           MOVE MFS-NUM-FAELT-RAETT  TO                                   
090800                RESP-IDKOLLI-ALLA-ATTR                                    
090900** CONTROL THE CASE MOVE BETWEEN PRC                                      
091000           MOVE REQU-IDKOLLI-ALLA      TO WS-CHK-IDKOLLI                  
091100           PERFORM S05-CHECK-PRC-OF-CASES                                 
091200         ELSE                                                             
091300           MOVE NEJ   TO DATA-RAETT-SW                                    
091400           MOVE MFS-NUM-FAELT-FEL  TO                                     
091500                RESP-IDKOLLI-ALLA-ATTR                                    
091600         MOVE 'IDKOLLI'            TO RESP-IDELMT-ERROR                   
091700         END-IF                                                           
091800       END-IF                                                             
091900     END-IF                                                               
092000                                                                          
092100     .                                                                    
092200     SKIP2                                                                
092300 GC-KONTROLL-FLYTTA-RAD  SECTION.                                         
092400                                                                          
092500     MOVE 1   TO MIX                                                      
092600     PERFORM UNTIL MIX > REQU-KVRADER                                     
092700                                                                          
092801      IF REQU-IDKOLLI-LINE (MIX) = '0000 '                                
092901         MOVE ALL '+'    TO REQU-IDKOLLI-LINE (MIX)                       
093001      END-IF                                                              
093101      IF REQU-KVLEVART-LINE-IN (MIX) = '000000 '                          
093201         MOVE ALL '+'      TO REQU-KVLEVART-LINE-IN (MIX)                 
093301      END-IF                                                              
093400                                                                          
093500      IF REQU-IDARTNR-LINE (MIX) > ZERO AND                               
093600        REQU-FLNOLLAD-LINE (MIX) = NEJ                                    
093700       IF REQU-IDKOLLI-LINE (MIX) = ALL '+'                               
093800         MOVE MFS-FORMATETS-ATTR TO                                       
093900              RESP-IDKOLLI-LINE-ATTR (MIX)                                
094000         IF REQU-KVLEVART-LINE-IN (MIX) = ALL '+'                         
094100           MOVE MFS-FORMATETS-ATTR    TO                                  
094200                RESP-KVLEVART-LINE-IN-ATTR (MIX)                          
094300         ELSE                                                             
094400           MOVE MFS-NUM-FAELT-FEL  TO                                     
094500                RESP-KVLEVART-LINE-IN-ATTR (MIX)                          
094600           MOVE JA   TO DATA-INMATAD-SW                                   
094700           MOVE NEJ  TO DATA-RAETT-SW                                     
094800           MOVE JA   TO FLYTTA-RAD-SW                                     
094900         MOVE 'KVLEVART'           TO RESP-IDELMT-ERROR                   
095000         END-IF                                                           
095100       ELSE                                                               
095200         MOVE JA TO DATA-INMATAD-SW                                       
095300         MOVE JA TO FLYTTA-RAD-SW                                         
095400         IF REQU-IDKOLLI-LINE (MIX) NUMERIC AND                           
095500           REQU-IDKOLLI-LINE (MIX) > ZERO AND                             
095600           REQU-IDKOLLI-LINE (MIX) NOT = IDKOLLI-WS                       
095700           MOVE MFS-NUM-FAELT-RAETT                                       
095800               TO RESP-IDKOLLI-LINE-ATTR (MIX)                            
095900** CONTROL THE CASE MOVE BETWEEN PRC                                      
096000           MOVE REQU-IDKOLLI-LINE (MIX) TO WS-CHK-IDKOLLI                 
096100                                           TMS-IDKOLLI(2)                 
096200           PERFORM S05-CHECK-PRC-OF-CASES                                 
096300           IF REQU-KVLEVART-LINE-IN (MIX) = ALL '+'                       
096400             MOVE MFS-NUM-FAELT-FEL TO                                    
096500                  RESP-KVLEVART-LINE-IN-ATTR (MIX)                        
096600             MOVE NEJ  TO DATA-RAETT-SW                                   
096701             MOVE 'KVLEVART'       TO RESP-IDELMT-ERROR                   
096800           ELSE                                                           
096900             IF REQU-KVLEVART-LINE-IN (MIX) NUMERIC AND                   
097000               REQU-KVLEVART-LINE-IN (MIX) > ZERO                         
097100               MOVE MFS-NUM-FAELT-RAETT                                   
097200                   TO RESP-KVLEVART-LINE-IN-ATTR (MIX)                    
097300             ELSE                                                         
097400               MOVE MFS-NUM-FAELT-FEL TO                                  
097500                    RESP-KVLEVART-LINE-IN-ATTR (MIX)                      
097600               MOVE NEJ  TO DATA-RAETT-SW                                 
097701               MOVE 'KVLEVART'     TO RESP-IDELMT-ERROR                   
097800             END-IF                                                       
097900           END-IF                                                         
098000         ELSE                                                             
098100           MOVE NEJ TO DATA-RAETT-SW                                      
098200           MOVE MFS-NUM-FAELT-FEL TO                                      
098300                RESP-IDKOLLI-LINE-ATTR (MIX)                              
098401           MOVE 'IDKOLLI'         TO RESP-IDELMT-ERROR                    
098500           IF REQU-KVLEVART-LINE-IN (MIX) NUMERIC AND                     
098600             REQU-KVLEVART-LINE-IN (MIX) > ZERO                           
098700             MOVE MFS-NUM-FAELT-RAETT TO                                  
098800                  RESP-KVLEVART-LINE-IN-ATTR (MIX)                        
098900           ELSE                                                           
099000             MOVE MFS-NUM-FAELT-FEL TO                                    
099100                  RESP-KVLEVART-LINE-IN-ATTR (MIX)                        
099200           END-IF                                                         
099300         END-IF                                                           
099400       END-IF                                                             
099500      ELSE                                                                
099600         MOVE MFS-STAENG-FAELT    TO                                      
099700             RESP-IDKOLLI-LINE-ATTR (MIX)                                 
099800             RESP-KVLEVART-LINE-IN-ATTR (MIX)                             
099900      END-IF                                                              
100001      ADD 1    TO MIX                                                     
100100     END-PERFORM                                                          
100200     .                                                                    
100300     EJECT                                                                
100400 GD-KONTROLL-ANTAL-MOT-REG  SECTION.                                      
100500                                                                          
100600     MOVE IDKOLLI-WS   TO W-IDKOLLI                                       
100700     PERFORM IMS-GNP-WDE611                                               
100800****** ÄNTRAT KOLL PÅ STATUS FRÅN 1 TILL 0                                
100900     IF SEGMENT-FINNS                                                     
101000*LK CHECK FOR NON ZERO KOLLI-DIKOLLI IS ADDED AS REPLACEMENT OF           
101100*LK OF KOLLI-KDKOLSTA WHICH IS NOT 1 EVEN AFTER PACKING IN API            
101200     AND KOLLI-DIKOLLIL NOT = ZERO                                        
101300                                                                          
101400       PERFORM S06-SPARA-KOLLI                                            
101500       MOVE REQU-IDRADNR-FOM TO W-IDPURAD                                 
101600       PERFORM IMS-GU-WDE411-21-PU                                        
101700       IF SEGMENT-FINNS                                                   
101800       PERFORM GDA-INIT-RAD-TAB                                           
101900       MOVE 1 TO MIX                                                      
102000         PERFORM UNTIL MIX > REQU-KVRADER                                 
102100         OR ORAD-IDPURAD > REQU-IDRADNR-TOM                               
102200         OR DATA-EJ-GODKAEND                                              
102300         OR NOT REGISTER-OK                                               
102400                                                                          
102500           IF REQU-FLNOLLAD-LINE (MIX) = NEJ AND                          
102600             REQU-IDARTNR-LINE (MIX) > ZERO                               
102700            IF REQU-IDKOLLI-LINE (MIX) NUMERIC                            
102800             MOVE REQU-IDARTNR-LINE (MIX) TO                              
102900                  IDARTNR-WS                                              
103000             MOVE REQU-KVLEVART-LINE-IN (MIX) TO                          
103100                  TAB-KVLEVART-MID (MIX)                                  
103200             IF FOERSTA-RAD                                               
103300               MOVE NEJ TO FOERSTA-RAD-SW                                 
103400             ELSE                                                         
103500               PERFORM IMS-GN-WDE411-21                                   
103600               IF SEGMENT-SAKNAS OR BASEN-SLUT                            
103700                 PERFORM S16-REGISTER-AENDRAT                             
103800               END-IF                                                     
103900             END-IF                                                       
104000                                                                          
104100             IF REGISTER-OK                                               
104200               IF ORAD-IDARTNR = IDARTNR-WS                               
104300                 IF TAB-KVLEVART-MID (MIX)                                
104400                    > KKOLLI-KVLEVART                                     
104500                   MOVE MFS-NUM-FAELT-FEL TO                              
104600                    RESP-KVLEVART-LINE-IN-ATTR (MIX)                      
104700                   MOVE NEJ TO DATA-RAETT-SW                              
104800                   MOVE 'KVLEVART'  TO RESP-IDELMT-ERROR                  
104900                 ELSE                                                     
105000                   MOVE ORAD-IDPURAD TO                                   
105100                        TAB-IDRADNR (MIX)                                 
105200                   MOVE KKOLLI-KVLEVART TO                                
105300                        TAB-KVLEVART-REG (MIX)                            
105400*LK                                                                       
105500                   COMPUTE VKORDNTO-WS = VKORDNTO-WS +                    
105600                        (ORAD-VKARTNTO * TAB-KVLEVART-REG (MIX))          
105700***                                                                       
105800                 END-IF                                                   
105900               ELSE                                                       
106000                 PERFORM S16-REGISTER-AENDRAT                             
106100               END-IF                                                     
106200             END-IF                                                       
106300            ELSE                                                          
106400             IF FOERSTA-RAD                                               
106500               MOVE NEJ TO FOERSTA-RAD-SW                                 
106600             ELSE                                                         
106700               PERFORM IMS-GN-WDE411-21                                   
106800               IF SEGMENT-SAKNAS OR BASEN-SLUT                            
106900                 PERFORM S16-REGISTER-AENDRAT                             
107000               END-IF                                                     
107100             END-IF                                                       
107200            END-IF                                                        
107300           ELSE                                                           
107400             MOVE MFS-STAENG-FAELT TO                                     
107500                  RESP-IDKOLLI-LINE-ATTR (MIX)                            
107600                  RESP-KVLEVART-LINE-IN-ATTR (MIX)                        
107700           END-IF                                                         
107800           ADD 1 TO MIX                                                   
107900         END-PERFORM                                                      
108000       ELSE                                                               
108100         PERFORM S16-REGISTER-AENDRAT                                     
108200       END-IF                                                             
108300     ELSE                                                                 
108400       PERFORM S16-REGISTER-AENDRAT                                       
108500     END-IF                                                               
108600                                                                          
108700     .                                                                    
108800     EJECT                                                                
108900 GDA-INIT-RAD-TAB  SECTION.                                               
109000     SKIP2                                                                
109100     MOVE 1    TO MIX                                                     
109200     PERFORM UNTIL MIX > REQU-KVRADER                                     
109300       MOVE ZERO TO TAB-IDRADNR (MIX)                                     
109400                    TAB-KVLEVART-MID (MIX)                                
109500                    TAB-KVLEVART-REG (MIX)                                
109600       ADD 1   TO MIX                                                     
109700     END-PERFORM                                                          
109800                                                                          
109900     .                                                                    
110000     EJECT                                                                
110100 GE-KONTROLL-NYTT-KOLLI  SECTION.                                         
110200                                                                          
110300     PERFORM GEA-RED-MID-NYTT-KOLLI                                       
110400                                                                          
110500     IF REQU-IDKOLLI-NY = ALL '+'                                         
110600       PERFORM GEB-EJ-NYTT-KOLLI                                          
110700     ELSE                                                                 
110800       PERFORM GEC-NYTT-KOLLI-INMATAT                                     
110900     END-IF                                                               
111000                                                                          
111100     .                                                                    
111200     EJECT                                                                
111300 GEA-RED-MID-NYTT-KOLLI  SECTION.                                         
111400     SKIP3                                                                
111500     IF REQU-IDKOLLI-NY = '0000 '                                         
111600        MOVE ALL '+'   TO REQU-IDKOLLI-NY                                 
111700     END-IF                                                               
111800     IF REQU-KDKOLLI   = '0000000 '                                       
111900        MOVE ALL '+'   TO REQU-KDKOLLI                                    
112000     END-IF                                                               
112100     IF REQU-KDEMBTYP  = '00 '                                            
112200        MOVE ALL '+'   TO REQU-KDEMBTYP                                   
112300     END-IF                                                               
112400     IF REQU-VKORDBTO  = '0000000 '                                       
112500        MOVE ALL '+'   TO REQU-VKORDBTO                                   
112600     END-IF                                                               
112700     IF REQU-DIKOLLIL  = '0000 '                                          
112800        MOVE ALL '+'   TO REQU-DIKOLLIL                                   
112900     END-IF                                                               
113000     IF REQU-DIKOLLIB  = '00 '                                            
113100        MOVE ALL '+'   TO REQU-DIKOLLIB                                   
113200     END-IF                                                               
113300     IF REQU-DIKOLLIH  = '00 '                                            
113400        MOVE ALL '+'   TO REQU-DIKOLLIH                                   
113500     END-IF                                                               
113600                                                                          
113700     .                                                                    
113800     EJECT                                                                
113900 GEB-EJ-NYTT-KOLLI  SECTION.                                              
114000     SKIP3                                                                
114100     MOVE MFS-FORMATETS-ATTR TO                                           
114200          RESP-IDKOLLI-NY-ATTR                                            
114300          RESP-KDKOLLI-ATTR                                               
114400                                                                          
114500                                                                          
114600     IF REQU-KDKOLLI  = ALL '+'                                           
114700       MOVE MFS-FORMATETS-ATTR    TO                                      
114800            RESP-KDKOLLI-ATTR                                             
114900     ELSE                                                                 
115000       MOVE MFS-ALFA-FAELT-FEL  TO                                        
115100            RESP-KDKOLLI-ATTR                                             
115200       MOVE JA   TO DATA-INMATAD-SW                                       
115300       MOVE NEJ  TO DATA-RAETT-SW                                         
115400         MOVE 'KDKOLLI'          TO RESP-IDELMT-ERROR                     
115500     END-IF                                                               
115600*                                                                         
115700     IF REQU-KDEMBTYP = ALL '+'                                           
115800       MOVE MFS-FORMATETS-ATTR    TO                                      
115900            RESP-KDEMBTYP-ATTR                                            
116000     ELSE                                                                 
116100       MOVE MFS-NUM-FAELT-FEL  TO                                         
116200            RESP-KDEMBTYP-ATTR                                            
116300       MOVE JA   TO DATA-INMATAD-SW                                       
116400       MOVE NEJ  TO DATA-RAETT-SW                                         
116500         MOVE 'KDEMBTYP'          TO RESP-IDELMT-ERROR                    
116600     END-IF                                                               
116700     IF REQU-VKORDBTO = ALL '+'                                           
116800       MOVE MFS-FORMATETS-ATTR    TO                                      
116900            RESP-VKORDBTO-ATTR                                            
117000     ELSE                                                                 
117100       MOVE MFS-NUM-FAELT-FEL  TO                                         
117200            RESP-VKORDBTO-ATTR                                            
117300       MOVE JA   TO DATA-INMATAD-SW                                       
117400       MOVE NEJ  TO DATA-RAETT-SW                                         
117500         MOVE 'VKORDBTO'          TO RESP-IDELMT-ERROR                    
117600     END-IF                                                               
117700     IF REQU-DIKOLLIL = ALL '+'                                           
117800       MOVE MFS-FORMATETS-ATTR    TO                                      
117900            RESP-DIKOLLIL-ATTR                                            
118000     ELSE                                                                 
118100       MOVE MFS-NUM-FAELT-FEL  TO                                         
118200            RESP-DIKOLLIL-ATTR                                            
118300       MOVE JA   TO DATA-INMATAD-SW                                       
118400       MOVE NEJ  TO DATA-RAETT-SW                                         
118500         MOVE 'DIKOLLIL'          TO RESP-IDELMT-ERROR                    
118600     END-IF                                                               
118700     IF REQU-DIKOLLIB = ALL '+'                                           
118800       MOVE MFS-FORMATETS-ATTR    TO                                      
118900            RESP-DIKOLLIB-ATTR                                            
119000     ELSE                                                                 
119100       MOVE MFS-NUM-FAELT-FEL  TO                                         
119200            RESP-DIKOLLIB-ATTR                                            
119300       MOVE JA   TO DATA-INMATAD-SW                                       
119400       MOVE NEJ  TO DATA-RAETT-SW                                         
119500         MOVE 'DIKOLLIB'          TO RESP-IDELMT-ERROR                    
119600     END-IF                                                               
119700     IF REQU-DIKOLLIH = ALL '+'                                           
119800       MOVE MFS-FORMATETS-ATTR    TO                                      
119900            RESP-DIKOLLIH-ATTR                                            
120000     ELSE                                                                 
120100       MOVE MFS-NUM-FAELT-FEL  TO                                         
120200            RESP-DIKOLLIH-ATTR                                            
120300       MOVE JA   TO DATA-INMATAD-SW                                       
120400       MOVE NEJ  TO DATA-RAETT-SW                                         
120500         MOVE 'DIKOLLIH'          TO RESP-IDELMT-ERROR                    
120600     END-IF                                                               
120700     MOVE VORD-IDDISTR TO TEST-IDDISTR                                    
120800     .                                                                    
120900     EJECT                                                                
121000 GEC-NYTT-KOLLI-INMATAT  SECTION.                                         
121101                                                                          
121201     MOVE JA TO DATA-INMATAD-SW                                           
121300     MOVE JA TO NYTT-KOLLI-SW                                             
121401                                                                          
121501                                                                          
121601     IF DATA-RAETT                                                        
121701       MOVE MFS-NUM-FAELT-RAETT    TO RESP-IDKOLLI-NY-ATTR                
121801                                    RESP-KDEMBTYP-ATTR                    
121901                                    RESP-VKORDBTO-ATTR                    
122001                                    RESP-DIKOLLIL-ATTR                    
123001                                    RESP-DIKOLLIB-ATTR                    
124001                                    RESP-DIKOLLIH-ATTR                    
124101       MOVE MFS-ALFA-FAELT-RAETT TO RESP-KDKOLLI-ATTR                     
124201     END-IF                                                               
124301                                                                          
124401     IF REQU-IDKOLLI-NY NUMERIC AND                                       
124501       REQU-IDKOLLI-NY > ZERO                                             
124601       MOVE MFS-NUM-FAELT-RAETT                                           
124701           TO RESP-IDKOLLI-NY-ATTR                                        
124801     ELSE                                                                 
124901       MOVE MFS-NUM-FAELT-FEL                                             
125001           TO RESP-IDKOLLI-NY-ATTR                                        
125101       MOVE NEJ TO DATA-RAETT-SW                                          
125201       MOVE 'IDKOLLI-NY'          TO RESP-IDELMT-ERROR                    
125301     END-IF                                                               
125401                                                                          
125501     IF REQU-KDKOLLI NOT = ALL '+'                                        
125601       MOVE REQU-KDKOLLI TO W-KDKOLLI-K5                                  
125701       PERFORM IMS-GU-WDK501                                              
125801       IF SEGMENT-SAKNAS                                                  
125901         MOVE MFS-ALFA-FAELT-FEL                                          
126001             TO RESP-KDKOLLI-ATTR                                         
127001         MOVE NEJ TO DATA-RAETT-SW                                        
127101         MOVE 'KDKOLLI'          TO RESP-IDELMT-ERROR                     
127201       ELSE                                                               
127301         MOVE EMB-VKTARA   TO  VKTARA-WS                                  
127401         MOVE MFS-ALFA-FAELT-RAETT                                        
127501           TO RESP-KDKOLLI-ATTR                                           
127601       END-IF                                                             
127701     END-IF                                                               
127801                                                                          
127901     IF REQU-KDEMBTYP = ALL '+'                                           
128001       CONTINUE                                                           
128101     ELSE                                                                 
128201       IF REQU-KDEMBTYP NUMERIC                                           
128301         MOVE MFS-NUM-FAELT-RAETT    TO                                   
128401              RESP-KDEMBTYP-ATTR                                          
128501       ELSE                                                               
128601         MOVE NEJ     TO DATA-RAETT-SW                                    
128701         MOVE MFS-NUM-FAELT-FEL    TO                                     
128801              RESP-KDEMBTYP-ATTR                                          
128901       MOVE 'KDEMBTYP'            TO RESP-IDELMT-ERROR                    
129001       END-IF                                                             
129101     END-IF                                                               
129201                                                                          
129301     COMPUTE VKORDBTO-WS = VKORDNTO-WS + VKTARA-WS                        
129401     END-COMPUTE                                                          
129501     IF REQU-VKORDBTO NOT = ALL '+'                                       
129601       MOVE REQU-VKORDBTO TO DEC-IDFRIDATA                                
129701       MOVE 6              TO DEC-KVHELTAL                                
129801       MOVE 1              TO DEC-KVDECIMAL                               
129901       CALL WDECEDIT USING DEC-WDECAREA                                   
130001       IF DEC-KDSVAR-OK AND DEC-IDEDITDATA > 0                            
130101         MOVE MFS-NUM-FAELT-RAETT TO RESP-VKORDBTO-ATTR                   
130201         MOVE DEC-IDEDITDATA TO NUM-VKORDBTO                              
130301         IF US-MEASUREMENT                                                
130401           COMPUTE NUM-VKORDBTO ROUNDED =                                 
130501                               NUM-VKORDBTO * CONV-LB-TO-KG               
130601           END-COMPUTE                                                    
130701         END-IF                                                           
130801                                                                          
130901*LK COMPARE INPUT GROSS WEIGHT WITH ACTUAL LINE WEIGHT                    
131001         IF NUM-VKORDBTO < VKORDBTO-WS                                    
131101            PERFORM S21-WEIGHT-ERR                                        
131201         ELSE                                                             
131301            MOVE NUM-VKORDBTO  TO VKORDBTO-WS                             
131401            COMPUTE VKTARA-WS = NUM-VKORDBTO - VKORDNTO-WS                
131501            END-COMPUTE                                                   
131601         END-IF                                                           
131701***                                                                       
131801       ELSE                                                               
131901         MOVE NEJ               TO DATA-RAETT-SW                          
132001         MOVE MFS-NUM-FAELT-FEL TO RESP-VKORDBTO-ATTR                     
132101         MOVE 'VKORDBTO'        TO RESP-IDELMT-ERROR                      
132201       END-IF                                                             
132301     END-IF                                                               
132401                                                                          
132501       IF REQU-DIKOLLIL = ALL '+'                                         
132601         CONTINUE                                                         
132701       ELSE                                                               
132801         IF REQU-DIKOLLIL NUMERIC                                         
132901           MOVE MFS-NUM-FAELT-RAETT  TO                                   
133001                RESP-DIKOLLIL-ATTR                                        
133101         ELSE                                                             
133201           MOVE NEJ   TO DATA-RAETT-SW                                    
133301           MOVE MFS-NUM-FAELT-FEL  TO                                     
133401                RESP-DIKOLLIL-ATTR                                        
133501         MOVE 'DIKOLLIL'          TO RESP-IDELMT-ERROR                    
133601         END-IF                                                           
133701       END-IF                                                             
133801                                                                          
133901       IF REQU-DIKOLLIB = ALL '+'                                         
134001         CONTINUE                                                         
134101       ELSE                                                               
134201         IF REQU-DIKOLLIB NUMERIC                                         
134301           MOVE MFS-NUM-FAELT-RAETT  TO                                   
134401                RESP-DIKOLLIB-ATTR                                        
134501         ELSE                                                             
134601           MOVE NEJ   TO DATA-RAETT-SW                                    
134701           MOVE MFS-NUM-FAELT-FEL  TO                                     
134801                RESP-DIKOLLIB-ATTR                                        
134901         MOVE 'DIKOLLIB'          TO RESP-IDELMT-ERROR                    
135001         END-IF                                                           
135101       END-IF                                                             
135201                                                                          
135301       IF REQU-DIKOLLIH = ALL '+'                                         
135401         CONTINUE                                                         
135501       ELSE                                                               
135601         IF REQU-DIKOLLIH NUMERIC AND                                     
135701            REQU-DIKOLLIH > ZERO                                          
135801           MOVE MFS-NUM-FAELT-RAETT  TO                                   
135901                RESP-DIKOLLIH-ATTR                                        
136001         ELSE                                                             
136101           MOVE NEJ   TO DATA-RAETT-SW                                    
136201           MOVE MFS-NUM-FAELT-FEL  TO                                     
136301                RESP-DIKOLLIH-ATTR                                        
136401         MOVE 'DIKOLLIH'          TO RESP-IDELMT-ERROR                    
136501         END-IF                                                           
136601       END-IF                                                             
136701                                                                          
136801       MOVE VORD-IDDISTR TO TEST-IDDISTR                                  
136901*      IF DIST03-SVERIGE AND                                              
137001*         DCS-IDLANDX2 = 'SE'                                             
137101*         MOVE MFS-ALFA-FAELT-RAETT TO RESP-ADFLGEO-ATTR                  
137201*         MOVE MFS-NUM-FAELT-RAETT TO RESP-ADFLOMR-ATTR                   
137301*         MOVE MFS-NUM-FAELT-RAETT TO RESP-ADRUTNIV-ATTR                  
137401*      ELSE                                                               
137501                                                                          
137601          PERFORM S01A-KOLLA-PLATS                                        
137701          CALL W403PLAT USING PLATS-W403PLAT                              
137801                              PLATS-DM-PCB                                
137901                              PLATS-DN-PCB                                
138001                              PLATS-DP-PCB                                
138101                              PLATS-DO-PCB                                
138201                              PLATS-WDE6C-PCB                             
138301                              PLATS-GMTC-PCB                              
138401                              PLATS-WDB6-PCB                              
138501                                                                          
138601          IF PLATS-KDSVAR = SPACE                                         
138701            CONTINUE                                                      
138801          ELSE                                                            
138901            IF DATA-RAETT                                                 
139001              MOVE NEJ TO DATA-RAETT-SW                                   
139101              MOVE JA                    TO FELTEXT-UTLAGD-SW             
139201            END-IF                                                        
139301          END-IF                                                          
139401*      END-IF                                                             
139501                                                                          
139601       IF DATA-RAETT                                                      
139701         PERFORM IMS-GU-WDE601                                            
139801         MOVE REQU-IDKOLLI-NY TO W-IDKOLLI                                
139901         PERFORM IMS-GNP-WDE611                                           
140001         IF SEGMENT-FINNS                                                 
140101           MOVE NEJ TO DATA-RAETT-SW                                      
140201           MOVE MFS-NUM-FAELT-FEL TO                                      
140301                RESP-IDKOLLI-NY-ATTR                                      
140401           MOVE 'IDKOLLI-NY'      TO RESP-IDELMT-ERROR                    
140501           MOVE ERR-CASE-REPORTED                                         
140601                                 TO RESP-IDMSG-ERROR                      
140701           MOVE JA                   TO FELTEXT-UTLAGD-SW                 
140801         END-IF                                                           
140901       END-IF                                                             
141001                                                                          
141101       IF DATA-RAETT                                                      
141201         IF REQU-KDKOLLI   = ALL '+' AND                                  
141301            REQU-KDEMBTYP  = ALL '+'                                      
141401           MOVE NEJ TO DATA-RAETT-SW                                      
141501           MOVE MFS-ALFA-FAELT-FEL TO                                     
141601                RESP-KDKOLLI-ATTR                                         
141701           MOVE 'KDKOLLI'      TO RESP-IDELMT-ERROR                       
141801         END-IF                                                           
141901       END-IF                                                             
142001                                                                          
142101       IF DATA-RAETT                                                      
142201         IF REQU-KDKOLLI   NOT = ALL '+' AND                              
142301            REQU-KDEMBTYP  NOT = ALL '+'                                  
142401           MOVE NEJ TO DATA-RAETT-SW                                      
142501           MOVE MFS-ALFA-FAELT-FEL TO                                     
142601                RESP-KDKOLLI-ATTR                                         
142701           MOVE 'KDKOLLI'      TO RESP-IDELMT-ERROR                       
142801           MOVE ERR-INVALID-COMBINATION                                   
142901                                 TO RESP-IDMSG-ERROR                      
143001           MOVE JA                   TO FELTEXT-UTLAGD-SW                 
143101         END-IF                                                           
143201       END-IF                                                             
143301                                                                          
143401       IF DATA-RAETT                                                      
143501         IF REQU-KDEMBTYP  NOT = ALL '+' AND                              
143601            REQU-DIKOLLIL  = ALL '+'                                      
143701           MOVE NEJ TO DATA-RAETT-SW                                      
143801           MOVE MFS-NUM-FAELT-FEL TO                                      
143901                RESP-DIKOLLIL-ATTR                                        
144001           MOVE 'DIKOLLIL'      TO RESP-IDELMT-ERROR                      
144101         END-IF                                                           
144201       END-IF                                                             
144301                                                                          
144401       IF DATA-RAETT                                                      
144501         IF REQU-KDEMBTYP  NOT = ALL '+' AND                              
144601            REQU-DIKOLLIB  = ALL '+'                                      
144701           MOVE NEJ TO DATA-RAETT-SW                                      
144801           MOVE MFS-NUM-FAELT-FEL TO                                      
144901                RESP-DIKOLLIB-ATTR                                        
145001           MOVE 'DIKOLLIB'      TO RESP-IDELMT-ERROR                      
145101         END-IF                                                           
145201       END-IF                                                             
145301                                                                          
145401       IF DATA-RAETT                                                      
145501         IF REQU-KDEMBTYP  NOT = ALL '+' AND                              
145601            REQU-DIKOLLIH  = ALL '+'                                      
145701           MOVE NEJ TO DATA-RAETT-SW                                      
145801           MOVE MFS-NUM-FAELT-FEL TO                                      
145901                RESP-DIKOLLIH-ATTR                                        
146001           MOVE 'DIKOLLIH'      TO RESP-IDELMT-ERROR                      
146101         END-IF                                                           
146201       END-IF                                                             
146301                                                                          
146401**     KOLLA LÄNGD                                                        
146501                                                                          
146601       IF DATA-RAETT                                                      
146701         IF REQU-KDKOLLI   NOT = ALL '+'                                  
146801           IF EMB-DIKOLLIL = ZERO                                         
146901             IF REQU-DIKOLLIL = ALL '+'                                   
147001**             LÄNGD VARKEN I WDK5 ELLER INMATAT                          
147101               MOVE NEJ TO DATA-RAETT-SW                                  
147201               MOVE MFS-NUM-FAELT-FEL TO                                  
147301                    RESP-DIKOLLIL-ATTR                                    
147401               MOVE 'DIKOLLIL'    TO RESP-IDELMT-ERROR                    
147501               MOVE ERR-ZERO-NOT-ALLOWED                                  
147601                                     TO RESP-IDMSG-ERROR                  
147701               MOVE JA               TO FELTEXT-UTLAGD-SW                 
147801             END-IF                                                       
147901           ELSE                                                           
148001             IF REQU-DIKOLLIL NOT = ALL '+'                               
148101**             LÄNGD BÅDE I WDK5 OCH INMATAT                              
148201               MOVE NEJ TO DATA-RAETT-SW                                  
148301               MOVE MFS-NUM-FAELT-FEL TO                                  
148401                    RESP-DIKOLLIL-ATTR                                    
148501               MOVE 'DIKOLLIL'    TO RESP-IDELMT-ERROR                    
148601               MOVE ERR-INVALID-COMBINATION                               
148701                                     TO RESP-IDMSG-ERROR                  
148801               MOVE JA               TO FELTEXT-UTLAGD-SW                 
148901             END-IF                                                       
149001           END-IF                                                         
149101         END-IF                                                           
149201       END-IF                                                             
149301                                                                          
149401**     KOLLA BREDD  > NOLL                                                
149501                                                                          
149601       IF DATA-RAETT                                                      
149701         IF REQU-KDKOLLI   NOT = ALL '+'                                  
149801           IF EMB-DIKOLLIB = ZERO                                         
149901             IF REQU-DIKOLLIB = ALL '+'                                   
150001**             BREDD VARKEN I WDK5 ELLER INMATAT                          
150101               MOVE NEJ TO DATA-RAETT-SW                                  
150201               MOVE MFS-NUM-FAELT-FEL TO                                  
150301                    RESP-DIKOLLIB-ATTR                                    
150401               MOVE 'DIKOLLIB'    TO RESP-IDELMT-ERROR                    
150501               MOVE ERR-ZERO-NOT-ALLOWED                                  
150601                                     TO RESP-IDMSG-ERROR                  
150701               MOVE JA               TO FELTEXT-UTLAGD-SW                 
150801             END-IF                                                       
150901           ELSE                                                           
151001             IF REQU-DIKOLLIB NOT = ALL '+'                               
151101**             BREDD BÅDE I WDK5 OCH INMATAT                              
151201               MOVE NEJ TO DATA-RAETT-SW                                  
151301               MOVE MFS-NUM-FAELT-FEL TO                                  
151401                    RESP-DIKOLLIB-ATTR                                    
151501               MOVE 'DIKOLLIB'    TO RESP-IDELMT-ERROR                    
151601               MOVE ERR-INVALID-COMBINATION                               
151701                                     TO RESP-IDMSG-ERROR                  
151801               MOVE JA               TO FELTEXT-UTLAGD-SW                 
151901             END-IF                                                       
152001           END-IF                                                         
152101         END-IF                                                           
152201       END-IF                                                             
152301                                                                          
152401**     KOLLA HÖJD > NOLL                                                  
152501                                                                          
152601       IF DATA-RAETT                                                      
152701         IF REQU-KDKOLLI   NOT = ALL '+'                                  
152801           IF EMB-DIKOLLIH = ZERO                                         
152901             IF REQU-DIKOLLIH = ALL '+'                                   
153001**             HÖJD  VARKEN I WDK5 ELLER INMATAT                          
153101               MOVE NEJ TO DATA-RAETT-SW                                  
153201               MOVE MFS-NUM-FAELT-FEL TO                                  
153301                    RESP-DIKOLLIH-ATTR                                    
153401               MOVE 'DIKOLLIH'    TO RESP-IDELMT-ERROR                    
153501               MOVE ERR-ZERO-NOT-ALLOWED                                  
153601                                     TO RESP-IDMSG-ERROR                  
153701               MOVE JA               TO FELTEXT-UTLAGD-SW                 
153801             END-IF                                                       
153901           ELSE                                                           
154001             IF REQU-DIKOLLIH NOT = ALL '+'                               
154101**             HÖJD  BÅDE I WDK5 OCH INMATAT                              
154201               MOVE NEJ TO DATA-RAETT-SW                                  
154301               MOVE MFS-NUM-FAELT-FEL TO                                  
154401                    RESP-DIKOLLIH-ATTR                                    
154501               MOVE 'DIKOLLIH'    TO RESP-IDELMT-ERROR                    
154601               MOVE ERR-INVALID-COMBINATION                               
154701                                     TO RESP-IDMSG-ERROR                  
154801               MOVE JA               TO FELTEXT-UTLAGD-SW                 
154901             END-IF                                                       
155001           END-IF                                                         
155101         END-IF                                                           
155201       END-IF                                                             
155301                                                                          
155401     .                                                                    
155501     EJECT                                                                
155601 H-UPPDATERING  SECTION.                                                  
155701                                                                          
155801     MOVE   '**POS-C-1'       TO FELTEXT                                  
155901     MOVE ZERO    TO VKORDNTO-WS                                          
156001     IF SPAR-KOLLI-KVORDRAD = +00000                                      
156101       MOVE IDPRODNR-WS TO W-IDPRODNR                                     
156201       PERFORM IMS-GU-WDE601                                              
156301       MOVE WS-MID-IDKOLLI    TO W-IDKOLLI                                
156401       PERFORM IMS-GNP-WDE611                                             
156501       IF SEGMENT-FINNS                                                   
156601         PERFORM S06-SPARA-KOLLI                                          
156701       ELSE                                                               
156801         MOVE ZERO             TO SPAR-KOLLI-KDKOLSTA                     
156901       END-IF                                                             
157001     END-IF                                                               
157101                                                                          
157201*LK CHECK FOR NON ZERO KOLLI-DIKOLLI IS ADDED AS REPLACEMENT OF           
157301*LK OF KOLLI-KDKOLSTA WHICH IS NOT 1 EVEN AFTER PACKING IN API            
157401     IF KOLLI-DIKOLLIL NOT = ZERO                                         
157501        IF SPAR-KOLLI-KDKOLSTA   < 2                                      
157601           MOVE SPAR-KOLLI-KVORDRAD    TO KVORDRAD-WS                     
157701           IF FLYTTA-ALLA                                                 
157801             PERFORM  HA-FLYTTA-ALLA                                      
157901           ELSE                                                           
158001             MOVE JA   TO FOERSTA-RAD-SW                                  
158101             MOVE 1 TO MIX                                                
158201             PERFORM UNTIL MIX >  REQU-KVRADER                            
158301             OR NOT REGISTER-OK                                           
158401                 IF REQU-IDKOLLI-LINE (MIX) NUMERIC                       
158501                   PERFORM HB-FLYTTA-EN-RAD                               
158601                 ELSE                                                     
158701                   PERFORM MFS-ROER-EJ-RAD                                
158801                 END-IF                                                   
158901                 ADD 1   TO MIX                                           
159001             END-PERFORM                                                  
159101             IF NYTT-KOLLI                                                
159201               PERFORM S30-UPPDAT-VORD                                    
159301               MOVE NEJ TO NYTT-KOLLI-SW                                  
159401             END-IF                                                       
159501             IF ANTAL-DLET-WS  > ZERO  OR                                 
159601               SUORDV-WS > ZERO    OR                                     
159701               SUORDV-EXP-WS > ZERO    OR                                 
159801               SUORDV-LOC-WS > ZERO    OR                                 
159901               SUORDV-LOCPREL-WS > ZERO    OR                             
160001               VKORDNTO-WS > ZERO                                         
160101               PERFORM S08-UPPDAT-FRAN-KOLLI                              
160201             END-IF                                                       
160301           END-IF                                                         
160401                                                                          
160501           IF REGISTER-OK                                                 
160601             IF TILL-KOLLI-OK                                             
160701               PERFORM MFS-STAENG-NYTT-KOLLI                              
160801             ELSE                                                         
160901               PERFORM MFS-OEPPNA-NYTT-KOLLI                              
161001             END-IF                                                       
161101             PERFORM MFS-ROER-EJ-HUVUD                                    
161201             MOVE KVORDRAD-WS   TO RESP-KVORDRAD                          
161301           END-IF                                                         
161401**PACK REPORT TO TMS                                                      
161501           IF REGISTER-OK AND TILL-KOLLI-OK                               
161601              MOVE W-IDDISTR-E401       TO TMS-IDDISTR                    
161701              MOVE W-IDKUNDNR-E401      TO TMS-IDKUNDNR                   
161801              MOVE W-IDKUNDRF-E401(1:5) TO TMS-IDORDNR7                   
161901** Inactive until integration tests are completed!                        
162001              CALL W403TMS1 USING TMS-W403TMS1                            
162101                          TMS-CRE-PCB TMS-DEL-PCB ATAB-PCB                
162201                          TMS-1165-PCB TMS-4141-PCB TMS-WDB2-PCB          
162301                          TMS-WDB6-PCB TMS-WDD3-PCB TMS-WDB1-PCB          
162401                          TMS-WDE4A-PCB TMS-WDE4F-PCB TMS-WDQ2-PCB        
162501                          TMS-WDQ3-PCB TMS-WDK6-PCB TMS-WDE6-PCB          
162601                          TMS-WDK5-PCB TMS-WDQ2C-PCB                      
162701           END-IF                                                         
162801        ELSE                                                              
162901          PERFORM S13-KOLLI-FAKTURERAT                                    
163001        END-IF                                                            
163101     ELSE                                                                 
163201       MOVE SPAR-KOLLI-KDKOLSTA   TO WS-KOLLI-KDKOLSTA                    
163301       PERFORM S11-KOLLI-SAKNAS                                           
163401     END-IF                                                               
163501     .                                                                    
163601     EJECT                                                                
163701 HA-FLYTTA-ALLA  SECTION.                                                 
163801     SKIP3                                                                
163901                                                                          
164001     MOVE REQU-IDKOLLI-ALLA TO W-IDKOLLI                                  
164101     PERFORM IMS-GHU-WDE611-GE                                            
164201     IF SEGMENT-FINNS                                                     
164301*LK CHECK FOR NON ZERO KOLLI-DIKOLLI IS ADDED AS REPLACEMENT OF           
164401*LK OF KOLLI-KDKOLSTA WHICH IS NOT 1 EVEN AFTER PACKING IN API            
164501     AND KOLLI-DIKOLLIL NOT = ZERO                                        
164601       IF KOLLI-KDKOLSTA < 3                                              
164701         PERFORM HAA-BYT-KOLLI-ALLA                                       
164801       ELSE                                                               
164901         MOVE TILL-KOLLI-FAKTURERAT (SPRAK-IX) TO                         
165001              RESP-KDSVAR-ALLA                                            
165101         PERFORM MFS-TILL-KOLLI-SF-ALLA                                   
165201         MOVE NEJ   TO TILL-KOLLI-OK-SW                                   
165301       END-IF                                                             
165401     ELSE                                                                 
165501       IF NYTT-KOLLI                                                      
165601         IF REQU-IDKOLLI-NY = REQU-IDKOLLI-ALLA                           
165701           PERFORM S01-NYUPPL-KOLLI                                       
165801           PERFORM HAA-BYT-KOLLI-ALLA                                     
165901         ELSE                                                             
166001           MOVE TILL-KOLLI-SAKNAS (SPRAK-IX) TO                           
166101                RESP-KDSVAR-ALLA                                          
166201           PERFORM MFS-TILL-KOLLI-SF-ALLA                                 
166301           MOVE NEJ   TO TILL-KOLLI-OK-SW                                 
166401         END-IF                                                           
166501       ELSE                                                               
166601         MOVE TILL-KOLLI-SAKNAS (SPRAK-IX) TO                             
166701              RESP-KDSVAR-ALLA                                            
166801         PERFORM MFS-TILL-KOLLI-SF-ALLA                                   
166901         MOVE NEJ   TO TILL-KOLLI-OK-SW                                   
167001       END-IF                                                             
167101     END-IF                                                               
167201                                                                          
167301     .                                                                    
167401     EJECT                                                                
167501 HAA-BYT-KOLLI-ALLA SECTION.                                              
167601                                                                          
167701     SKIP3                                                                
167801     MOVE IDPRODNR-WS       TO W-IDPRODNR-E4F-MIN                         
167901                               W-IDPRODNR-E4F-MAX                         
168001     MOVE IDKOLLI-WS        TO W-IDKOLLI-E4F-MIN                          
168101                               W-IDKOLLI-E4F-MAX                          
168201     MOVE REQU-IDRADNR-FOM  TO W-IDPURAD                                  
168301     PERFORM IMS-GU-WDE4F1-PU                                             
168401                                                                          
168501     IF SEGMENT-FINNS                                                     
168601      MOVE  1     TO    MIX                                               
168701      PERFORM UNTIL MIX > REQU-KVRADER                                    
168801       IF SEGMENT-FINNS AND                                               
168901         SEQF-IDPURAD NOT > REQU-IDRADNR-TOM                              
169001         MOVE SEQF-IDDISTR   TO W-IDDISTR-E401                            
169101         MOVE SEQF-IDKUNDNR  TO W-IDKUNDNR-E401                           
169201         MOVE SEQF-IDKUNDRF  TO W-IDKUNDRF-E401                           
169301         MOVE SEQF-IDPRODNR  TO W-IDPRODNR-E401                           
169401         MOVE SEQF-IDPLKLST  TO W-IDPLKLST-E401                           
169501         MOVE SEQF-IDPURAD   TO W-IDPURAD                                 
169601         PERFORM IMS-GU-WDE411                                            
169701         PERFORM S04-SPARA-ORAD                                           
169801                                                                          
169901         IF DCS-NDC-NA                                                    
170001            PERFORM S32-DATA-TILL-DEL-NOTE-KOLLI                          
170101         END-IF                                                           
170201                                                                          
170301         MOVE SEQF-KVLEVART TO KVLEVART-WS                                
170401         PERFORM S07-UPPDAT-ARB-KOLLI                                     
170501****** BORTTAG AV DET GAMLA KOLLIT                                        
170601         MOVE IDKOLLI-WS TO W-IDKOLLI                                     
170701         PERFORM IMS-GHNP-WDE421                                          
170801         PERFORM IMS-DLET-WDE421                                          
170901         ADD 1   TO ANTAL-DLET-WS                                         
171001                                                                          
171101****** FLYTTA RAD TILL NYTT KOLLI                                         
171201****** OBS! EFTER DLET AV GAMLA KOLLIT BYTER MAN IDKOLLI I                
171301****** .... BÅDE IO-AREA OCH IMS-NYCKEL FÖR UPPDAT AV NYA KOLLIT          
171401         MOVE REQU-IDKOLLI-ALLA TO KKOLLI-IDKOLLI                         
171501                                  W-IDKOLLI                               
171601         PERFORM IMS-ISRT-WDE421                                          
171701         IF SEGMENT-FINNS-REDAN                                           
171801           PERFORM IMS-GHNP-WDE421                                        
171901           ADD SEQF-KVLEVART TO KKOLLI-KVLEVART                           
172001           PERFORM IMS-REPL-WDE421                                        
172101         ELSE                                                             
172201           ADD 1  TO ANTAL-ISRT-WS                                        
172301         END-IF                                                           
172401                                                                          
172501         MOVE ZERO    TO RESP-KVLEVART-LINE (MIX)                         
172601         PERFORM MFS-HEL-RAD-FLYTTAD                                      
172701                                                                          
172801         PERFORM IMS-GN-WDE4F1                                            
172901       ELSE                                                               
173001         PERFORM MFS-TOM-RAD                                              
173101       END-IF                                                             
173201       ADD 1   TO MIX                                                     
173301      END-PERFORM                                                         
173401                                                                          
173501      PERFORM S08-UPPDAT-FRAN-KOLLI                                       
173601      PERFORM S09-UPPDAT-TILL-KOLLI-ALLA                                  
173701      PERFORM MFS-EFTER-FLYTTA-ALLA                                       
173801     ELSE                                                                 
173901       PERFORM S16-REGISTER-AENDRAT                                       
174001     END-IF                                                               
174101                                                                          
174201     .                                                                    
174301     EJECT                                                                
174401 HB-FLYTTA-EN-RAD  SECTION.                                               
174501     SKIP3                                                                
174601     MOVE REQU-IDKOLLI-LINE (MIX) TO W-IDKOLLI                            
174701     PERFORM IMS-GHU-WDE611-GE                                            
174801     IF SEGMENT-FINNS                                                     
174901*LK CHECK FOR NON ZERO KOLLI-DIKOLLI IS ADDED AS REPLACEMENT OF           
175001*LK OF KOLLI-KDKOLSTA WHICH IS NOT 1 EVEN AFTER PACKING IN API            
175101     AND KOLLI-DIKOLLIL NOT = ZERO                                        
175201         IF KOLLI-KDKOLSTA < 3                                            
175301           PERFORM HBA-FLYTTA-RAD-UPPD-KOLLI                              
175401         ELSE                                                             
175501           MOVE TILL-KOLLI-FAKTURERAT (SPRAK-IX) TO                       
175601                RESP-KDSVAR-LINE (MIX)                                    
175701           PERFORM MFS-TILL-KOLLI-SF                                      
175801           MOVE NEJ TO TILL-KOLLI-OK-SW                                   
175901         END-IF                                                           
176001     ELSE                                                                 
176101       IF NYTT-KOLLI                                                      
176201         IF REQU-IDKOLLI-NY = REQU-IDKOLLI-LINE (MIX)                     
176301           PERFORM S01-NYUPPL-KOLLI                                       
176401           PERFORM HBA-FLYTTA-RAD-UPPD-KOLLI                              
176501         ELSE                                                             
176601           MOVE TILL-KOLLI-SAKNAS (SPRAK-IX) TO                           
176701                RESP-KDSVAR-LINE (MIX)                                    
176801           PERFORM MFS-TILL-KOLLI-SF                                      
176901           MOVE NEJ   TO TILL-KOLLI-OK-SW                                 
177001         END-IF                                                           
177101       ELSE                                                               
177201         MOVE TILL-KOLLI-SAKNAS (SPRAK-IX) TO                             
177301              RESP-KDSVAR-LINE (MIX)                                      
177401         PERFORM MFS-TILL-KOLLI-SF                                        
177501         MOVE NEJ   TO TILL-KOLLI-OK-SW                                   
177601       END-IF                                                             
177701     END-IF                                                               
177801                                                                          
177901     .                                                                    
178001     EJECT                                                                
178101 HBA-FLYTTA-RAD-UPPD-KOLLI  SECTION.                                      
178201                                                                          
178301     SKIP3                                                                
178401     MOVE IDPRODNR-WS       TO W-IDPRODNR-E4F-MIN                         
178501                               W-IDPRODNR-E4F-MAX                         
178601     MOVE IDKOLLI-WS        TO W-IDKOLLI-E4F-MIN                          
178701                               W-IDKOLLI-E4F-MAX                          
178801                               W-IDKOLLI                                  
178901     IF FOERSTA-RAD                                                       
179001       MOVE NEJ               TO FOERSTA-RAD-SW                           
179101       MOVE REQU-IDRADNR-FOM  TO W-IDPURAD                                
179201       PERFORM IMS-GU-WDE4F1-PU                                           
179301     ELSE                                                                 
179401       PERFORM IMS-GU-WDE4F1                                              
179501     END-IF                                                               
179601                                                                          
179701     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
179801                OR SEQF-IDPURAD     > REQU-IDRADNR-TOM                    
179901                OR SEQF-IDPURAD NOT < TAB-IDRADNR (MIX)                   
180001                                                                          
180101        PERFORM IMS-GN-WDE4F1                                             
180201     END-PERFORM                                                          
180301                                                                          
180401     IF SEGMENT-FINNS AND SEQF-IDPURAD = TAB-IDRADNR (MIX)                
180501       MOVE SEQF-IDDISTR   TO W-IDDISTR-E401                              
180601       MOVE SEQF-IDKUNDNR  TO W-IDKUNDNR-E401                             
180701       MOVE SEQF-IDKUNDRF  TO W-IDKUNDRF-E401                             
180801       MOVE SEQF-IDPRODNR  TO W-IDPRODNR-E401                             
180901       MOVE SEQF-IDPLKLST  TO W-IDPLKLST-E401                             
181001       MOVE SEQF-IDPURAD   TO W-IDPURAD                                   
181101       PERFORM IMS-GU-WDE411                                              
181201       PERFORM S04-SPARA-ORAD                                             
181301                                                                          
181401       IF TAB-KVLEVART-MID (MIX) = TAB-KVLEVART-REG (MIX)                 
181501         PERFORM HBAA-FLYTTA-HEL-RAD                                      
181601       ELSE                                                               
181701         PERFORM HBAB-FLYTTA-DEL-AV-RAD                                   
181801       END-IF                                                             
181901       PERFORM S07-UPPDAT-ARB-KOLLI                                       
182001       PERFORM S10-UPPDAT-TILL-KOLLI-RAD                                  
182101     ELSE                                                                 
182201       PERFORM S16-REGISTER-AENDRAT                                       
182301     END-IF                                                               
182401     .                                                                    
182501     EJECT                                                                
182601 HBAA-FLYTTA-HEL-RAD  SECTION.                                            
182701     SKIP3                                                                
182801     IF DCS-NDC-NA                                                        
182901        PERFORM S33-DATA-TILL-DEL-NOTE-ARTIKEL                            
183001     END-IF                                                               
183101                                                                          
183201     PERFORM IMS-GHNP-WDE421                                              
183301     PERFORM IMS-DLET-WDE421                                              
183401     ADD 1   TO ANTAL-DLET-WS                                             
183501                                                                          
183601****** FLYTTA RAD TILL NYTT KOLLI                                         
183701****** OBS! EFTER DLET AV GAMLA KOLLIT BYTER MAN IDKOLLI I                
183801****** .... BÅDE IO-AREA OCH IMS-NYCKEL FÖR UPPDAT AV NYA KOLLIT          
183901                                                                          
184001     MOVE REQU-IDKOLLI-LINE (MIX) TO KKOLLI-IDKOLLI                       
184101                                   W-IDKOLLI                              
184201     PERFORM IMS-ISRT-WDE421                                              
184301     IF SEGMENT-FINNS-REDAN                                               
184401       PERFORM IMS-GHNP-WDE421                                            
184501       ADD TAB-KVLEVART-MID (MIX)  TO KKOLLI-KVLEVART                     
184601       PERFORM IMS-REPL-WDE421                                            
184701       MOVE ZERO    TO ANTAL-ISRT-WS                                      
184801     ELSE                                                                 
184901       MOVE 1       TO ANTAL-ISRT-WS                                      
185001     END-IF                                                               
185101     MOVE TAB-KVLEVART-MID (MIX)   TO KVLEVART-WS                         
185201     MOVE ZERO            TO RESP-KVLEVART-LINE (MIX)                     
185301     PERFORM MFS-HEL-RAD-FLYTTAD                                          
185401     .                                                                    
185501     EJECT                                                                
185601 HBAB-FLYTTA-DEL-AV-RAD  SECTION.                                         
185701     SKIP3                                                                
185801     IF DCS-NDC-NA                                                        
185901        PERFORM S33-DATA-TILL-DEL-NOTE-ARTIKEL                            
186001     END-IF                                                               
186101                                                                          
186201     PERFORM IMS-GHNP-WDE421                                              
186301     SUBTRACT TAB-KVLEVART-MID (MIX) FROM KKOLLI-KVLEVART                 
186401                                                                          
186501     IF KKOLLI-KVLEVART > ZERO                                            
186601       PERFORM IMS-REPL-WDE421                                            
186701       MOVE KKOLLI-KVLEVART TO RESP-KVLEVART-LINE (MIX)                   
186801       MOVE TAB-KVLEVART-MID (MIX)  TO KVLEVART-WS                        
186901       PERFORM MFS-DEL-AV-RAD-FLYTTAD                                     
187001     ELSE                                                                 
187101       PERFORM IMS-DLET-WDE421                                            
187201       ADD 1 TO ANTAL-DLET-WS                                             
187301       MOVE ZERO TO RESP-KVLEVART-LINE (MIX)                              
187401       MOVE TAB-KVLEVART-MID (MIX)  TO KVLEVART-WS                        
187501       PERFORM MFS-HEL-RAD-FLYTTAD                                        
187601     END-IF                                                               
187701                                                                          
187801****** FLYTTA RAD TILL NYTT KOLLI                                         
187901****** OBS! EFTER DLET AV GAMLA KOLLIT BYTER MAN IDKOLLI I                
188001****** .... BÅDE IO-AREA OCH IMS-NYCKEL FÖR UPPDAT AV NYA KOLLIT          
188101                                                                          
188201     MOVE REQU-IDKOLLI-LINE (MIX) TO KKOLLI-IDKOLLI                       
188301                                    W-IDKOLLI                             
188401     MOVE TAB-KVLEVART-MID (MIX) TO KKOLLI-KVLEVART                       
188501     PERFORM IMS-ISRT-WDE421                                              
188601     IF SEGMENT-FINNS-REDAN                                               
188701       PERFORM IMS-GHNP-WDE421                                            
188801       ADD TAB-KVLEVART-MID (MIX) TO KKOLLI-KVLEVART                      
188901       PERFORM IMS-REPL-WDE421                                            
189001       MOVE ZERO   TO ANTAL-ISRT-WS                                       
189101     ELSE                                                                 
189201       MOVE 1      TO ANTAL-ISRT-WS                                       
189301     END-IF                                                               
189401     .                                                                    
189501     EJECT                                                                
189601 D-BLAEDDRA SECTION.                                                      
189701     SKIP3                                                                
189801     MOVE ALL-PLUS          TO RESP-ORDER-INFO                            
189901                               RESP-KVORDRAD                              
190001                               RESP-TIPACKN                               
190101     MOVE ALL-SPACE         TO RESP-IDKOLLI-ALLA                          
190201                               RESP-KDSVAR-ALLA                           
190301                               RESP-FLJANEJ-ALLA                          
190401                                                                          
190501     IF REQU-IDRADNR-TOM = ALL '+'                                        
190601       MOVE ZERO            TO REQU-IDRADNR-TOM                           
190701       MOVE ZERO            TO W-IDPURAD                                  
190801     ELSE                                                                 
190901       IF REQU-IDRADNR-TOM NUMERIC                                        
191001         MOVE REQU-IDRADNR-TOM TO W-IDPURAD                               
191101       ELSE                                                               
191201         MOVE ZERO             TO W-IDPURAD                               
191301       END-IF                                                             
191401     END-IF                                                               
191501                                                                          
191601     MOVE IDKOLLI-WS        TO W-IDKOLLI                                  
191701     PERFORM IMS-GU-WDE411-21-NEXT                                        
191801     IF SEGMENT-FINNS                                                     
191901       PERFORM S02-REDIGERA-BILD                                          
192001     ELSE                                                                 
192101       PERFORM IMS-GU-WDE411-21                                           
192201       IF SEGMENT-FINNS                                                   
192301         PERFORM S02-REDIGERA-BILD                                        
192401       ELSE                                                               
192501         PERFORM S18-KOLLI-TOMT                                           
192601       END-IF                                                             
192701     END-IF                                                               
192801     .                                                                    
192901     EJECT                                                                
193001 E-VISA-ORDER-KOLLI  SECTION.                                             
193101     SKIP3                                                                
193201     IF   VORD-IDDC  = REQU-IDDC-KEY                                      
193301        MOVE VORD-IDDISTR TO RESP-IDDISTR                                 
193401        MOVE VORD-IDKUNDNR TO RESP-IDKUNDNR                               
193501        MOVE VORD-IDDC    TO RESP-IDDC                                    
193601        MOVE VORD-KDORDKL TO RESP-KDORDKL                                 
193701        MOVE KOLLI-KVORDRAD TO RESP-KVORDRAD                              
193801        MOVE KOLLI-TIPACKN TO RESP-TIPACKN                                
193901        INSPECT RESP-TIPACKN REPLACING LEADING SPACE BY ZERO              
194001        MOVE ALL-SPACE TO RESP-FLJANEJ-ALLA                               
194101                                RESP-KDSVAR-ALLA                          
194201                                RESP-IDKOLLI-ALLA                         
194301                                                                          
194401        PERFORM IMS-GU-WDE411-21                                          
194501        IF SEGMENT-FINNS                                                  
194601          PERFORM S02-REDIGERA-BILD                                       
194701        ELSE                                                              
194801          PERFORM S19-KOLLI-TOMT                                          
194901        END-IF                                                            
195001     ELSE                                                                 
195101        MOVE INF-WRONG-DC            TO RESP-IDMSG-INFO                   
195201        MOVE 'IDDC'                  TO RESP-IDELMT-ERROR                 
195301     END-IF                                                               
195401     .                                                                    
195501     EJECT                                                                
195601 S01-NYUPPL-KOLLI  SECTION.                                               
195701                                                                          
195801     MOVE VORD-IDDISTR  TO TEST-IDDISTR                                   
195901*    IF DIST03-SVERIGE AND                                                
196001*       DCS-IDLANDX2 = 'SE'                                               
196101*      MOVE VORD-IDDC   TO KOLLI-IDDC                                     
196201*      MOVE SPACE       TO KOLLI-ADFLGEO                                  
196301*      MOVE ZERO        TO KOLLI-ADFLOMR                                  
196401*                          KOLLI-IDTRPTNR                                 
196501*                          KOLLI-ADRUTNIV                                 
196601*                          KOLLI-ADVMODUL                                 
196701*                          KOLLI-DIDMODUL                                 
196801*                          KOLLI-DIHMODUL                                 
196901*                          KOLLI-ADHMODUL                                 
197001*    ELSE                                                                 
197101       PERFORM S01A-KOLLA-PLATS                                           
197201       CALL W403PLAT USING PLATS-W403PLAT                                 
197301                           PLATS-DM-PCB                                   
197401                           PLATS-DN-PCB                                   
197501                           PLATS-DP-PCB                                   
197601                           PLATS-DO-PCB                                   
197701                           PLATS-WDE6C-PCB                                
197801                           PLATS-GMTC-PCB                                 
197901                           PLATS-WDB6-PCB                                 
198001                                                                          
198101       IF PLATS-KDSVAR = SPACE                                            
198201         MOVE PLATS-IDTRPTNR   TO KOLLI-IDTRPTNR                          
198301         MOVE PLATS-IDDC       TO KOLLI-IDDC                              
198401         MOVE PLATS-ADFLGEO    TO KOLLI-ADFLGEO                           
198501                                                                          
198601         IF SPAR-KOLLI-KDFARLIG = +4                                      
198701         OR SPAR-KOLLI-KDFARLIG = +7                                      
198801           MOVE +950          TO KOLLI-ADFLOMR                            
198901         ELSE                                                             
199001           MOVE PLATS-ADFLOMR TO KOLLI-ADFLOMR                            
199101         END-IF                                                           
199201                                                                          
199301         MOVE PLATS-ADRUTNIV TO KOLLI-ADRUTNIV                            
199401         MOVE PLATS-ADVMODUL TO KOLLI-ADVMODUL                            
199501         MOVE PLATS-DIDMODUL TO KOLLI-DIDMODUL                            
199601         MOVE PLATS-DIHMODUL TO KOLLI-DIHMODUL                            
199701         MOVE PLATS-ADHMODUL TO KOLLI-ADHMODUL                            
199801                                                                          
199901         MOVE PLATS-ADFLGEO  TO ARB-ADFLGEO                               
200001         MOVE PLATS-ADFLOMR  TO ARB-ADFLOMR                               
200101         MOVE PLATS-ADRUTNIV TO ARB-ADRUTNIV                              
200201                                                                          
200301         MOVE INF-CASE-UPDATED   TO RESP-IDMSG-INFO                       
200401         MOVE ARB-ADRESS         TO RESP-ARB-ADDRESS                      
200501         MOVE ARB-ADRESS         TO RESP-IDELMT-ERROR                     
200601       END-IF                                                             
200701*    END-IF                                                               
200801*                                                                         
200901     MOVE SPAR-KOLLI-IDTRPLOS  TO KOLLI-IDTRPLOS                          
201001     MOVE SPAR-KOLLI-IDTRPVAR  TO KOLLI-IDTRPVAR                          
201101                                                                          
201201     MOVE W-IDKOLLI     TO KOLLI-IDKOLLI                                  
201301     MOVE JA            TO KOLLI-FLUTLAST                                 
201401     MOVE NEJ           TO KOLLI-FLBANDST                                 
201501                           KOLLI-FLFRSUTS                                 
201601                                                                          
201701*    LÄNGD,HÖJD,BREDD ANTINGEN INATAD PÅ BILD ELLER                       
201801*    HAR KDKOLLI ANGETTS HAR VÄRDEN HÄMTATS FRÅN WDK5                     
201901*    ÄR WDK5-S VÄRDEN NOLL, KRÄVS INMATNING FRÅN BILD                     
202001                                                                          
202101     IF REQU-DIKOLLIL NOT = ALL '+'                                       
202201      MOVE REQU-DIKOLLIL TO WS-DIKOLLIL                                   
202301     END-IF                                                               
202401                                                                          
202501     IF REQU-DIKOLLIB NOT = ALL '+'                                       
202601      MOVE REQU-DIKOLLIB TO WS-DIKOLLIB                                   
202701     END-IF                                                               
202801                                                                          
202901     IF REQU-DIKOLLIH NOT = ALL '+'                                       
203001      MOVE REQU-DIKOLLIH TO WS-DIKOLLIH                                   
203101     END-IF                                                               
203201                                                                          
203301     IF US-MEASUREMENT                                                    
203401       COMPUTE WS-DIKOLLIL = WS-DIKOLLIL * CONV-IN-TO-CM                  
203501       END-COMPUTE                                                        
203601       COMPUTE WS-DIKOLLIB = WS-DIKOLLIB * CONV-IN-TO-CM                  
203701       END-COMPUTE                                                        
203801       COMPUTE WS-DIKOLLIH = WS-DIKOLLIH * CONV-IN-TO-CM                  
203900       END-COMPUTE                                                        
204001     END-IF                                                               
204101                                                                          
204201     IF REQU-KDKOLLI NOT = ALL '+'                                        
204301       MOVE EMB-KDEMBTYP  TO WS-KDEMBTYP                                  
204401       MOVE REQU-KDKOLLI  TO WS-KDKOLLI                                   
204501                                                                          
204601       IF EMB-DIKOLLIL > ZERO                                             
204701         MOVE EMB-DIKOLLIL  TO WS-DIKOLLIL                                
204801       END-IF                                                             
204901                                                                          
205001       IF EMB-DIKOLLIB > ZERO                                             
205101         MOVE EMB-DIKOLLIB  TO WS-DIKOLLIB                                
205201       END-IF                                                             
205301                                                                          
205401       IF EMB-DIKOLLIH > ZERO                                             
205501         MOVE EMB-DIKOLLIH  TO WS-DIKOLLIH                                
205601       END-IF                                                             
205701     ELSE                                                                 
205801       MOVE REQU-KDEMBTYP TO WS-KDEMBTYP                                  
205901       MOVE SPACE         TO WS-KDKOLLI                                   
206001     END-IF                                                               
206101                                                                          
206201     MOVE WS-DIKOLLIL   TO KOLLI-DIKOLLIL                                 
206301     MOVE WS-DIKOLLIB   TO KOLLI-DIKOLLIB                                 
206401     MOVE WS-DIKOLLIH   TO KOLLI-DIKOLLIH                                 
206501     MOVE VKTARA-WS     TO KOLLI-VKORDBTO-KOLLI                           
206601     MOVE NEJ           TO KOLLI-FLTULLG                                  
206701     MOVE ZERO          TO KOLLI-IDKOLLI-FLER                             
206801                           KOLLI-IDFAKLOP                                 
206901                           KOLLI-IDFAKT                                   
207001                           KOLLI-IDFAKT-EXP                               
207101                           KOLLI-KVFALRAD                                 
207201                           KOLLI-KVFLAMP-KOLLI                            
207301                           KOLLI-KVORDRAD                                 
207401                           KOLLI-TIAAVVD-PATR                             
207501                           KOLLI-SUORDV-KOLLI                             
207601                           KOLLI-SUORDV-KLI-EXP                           
207701                           KOLLI-SUORDV-LOC                               
207801                           KOLLI-SUORDV-LOCPREL                           
207901                           KOLLI-TIFAKT                                   
208001                           KOLLI-TIFAKT-EXP                               
208101                           KOLLI-TIFAKTID                                 
208201                           KOLLI-TIFAKTID-EXP                             
208301                           KOLLI-TILASTN                                  
208401                           KOLLI-TILASTID                                 
208501                           KOLLI-IDLASTN                                  
208601                           KOLLI-KDFARLIG-KOLLI                           
208701                           KOLLI-VKORDNTO-KOLLI                           
208801                           KOLLI-SUEQFG                                   
208901                           KOLLI-IDKOLLI-SAMP                             
209001* OBS - HÄR INITIERAS NYA DDGS-FÄLT RENT GENERELLT                        
209101* OBS - KONTROLLERA OM DETTA ÄR KORREKT!!!                                
209201                           KOLLI-DASUPREF                                 
209301                           KOLLI-TISUPTID                                 
209401                           KOLLI-IDLEVNR                                  
209501                           KOLLI-KDVIA                                    
209601                           KOLLI-IDTULLNR                                 
209701                           KOLLI-RETULKS                                  
209801                           KOLLI-IDSHIPM                                  
209901     MOVE VORD-KDVALISO     TO KOLLI-KDVALISO                             
210001     MOVE VORD-KDVALISO-EXP TO KOLLI-KDVALISO-EXP                         
210101     MOVE SPAR-KOLLI-IDPLOCK                                              
210201                            TO KOLLI-IDPLOCK                              
210301     MOVE WS-KDEMBTYP       TO KOLLI-KDEMBTYP                             
210401                                                                          
210501     IF WS-KDTRANS = 'W4A343'                                             
210601       MOVE 0               TO KOLLI-KDKOLSTA                             
210701     ELSE                                                                 
210801       MOVE 1               TO KOLLI-KDKOLSTA                             
210901     END-IF                                                               
211001                                                                          
211100     MOVE WS-KDKOLLI        TO KOLLI-KDKOLLI                              
211200     MOVE SPACE             TO KOLLI-IDLBBET                              
211300                               KOLLI-IDSUPREF                             
211400                               KOLLI-KDARTURS-KOLLI                       
211500                               KOLLI-IDTULFTG                             
211600                               KOLLI-KDSTASKLI                            
211610                               KOLLI-FILLERX2                             
211700                                                                          
211800     ACCEPT TIAAMMDD-WS FROM DATE                                         
211900     ACCEPT TIHHMMSS-WS-8  FROM TIME                                      
212000                                                                          
212100     IF DCS-NDC OR                                                        
212200       (DCS-SDC AND DCS-IDLANDX2 = 'GB')                                  
212300       MOVE '011'                TO TIDZ-MSGI-KDCALL                      
212400       MOVE DCS-IDTIDZON         TO TIDZ-MSGI-IDTIDZON                    
212501       MOVE DCS-IDDC             TO TIDZ-MSGI-IDDC                        
212600       MOVE TIAAMMDD-WS          TO TIDZ-MSGI-TILOKDAT                    
212700       MOVE TIHHMMSS-WS-6        TO TIDZ-MSGI-TILOKTID                    
212800       CALL WL01TIDZ          USING TIDZ-MSGI-WL01TIDZ                    
212900       MOVE TIDZ-MSGI-TILOKDAT   TO TIAAMMDD-WS                           
213000       MOVE TIDZ-MSGI-TILOKTID   TO TIHHMMSS-WS-6 (1:4)                   
213100     END-IF                                                               
213200     MOVE TIAAMMDD-WS      TO KOLLI-TIPACKN                               
213300     MOVE TIHHMMSS-WS-6    TO KOLLI-TIPACTID                              
213400                                                                          
213500     MOVE VORD-IDDISTR     TO KOLLI-IDDISTR                               
213600     MOVE VORD-IDKUNDNR    TO KOLLI-IDKUNDNR                              
213700     MOVE VORD-DARFS       TO KOLLI-DARFS                                 
213800*                                                                         
213901     COMPUTE KOLLI-VLORDBTO-KOLLI                                         
214001           = KOLLI-DIKOLLIL                                               
214101           * KOLLI-DIKOLLIB                                               
214201           * KOLLI-DIKOLLIH                                               
214301           / 1000000                                                      
214401     MOVE VORD-KDORDKL    TO KOLLI-KDORDKL                                
214500     MOVE VORD-FLAUTFAK   TO KOLLI-FLAUTFAK                               
214600*                                                                         
214700     MOVE +1 TO FG-INDX                                                   
214800     PERFORM UNTIL FG-INDX > MAX-FG-INDX                                  
214900       MOVE ZERO          TO KOLLI-IDPSN(FG-INDX)                         
215000                             KOLLI-VKART-FG(FG-INDX)                      
215100                             KOLLI-VLFG(FG-INDX)                          
215200       ADD +1 TO FG-INDX                                                  
215300     END-PERFORM                                                          
215400*                                                                         
215500     PERFORM IMS-ISRT-WDE611                                              
215601     IF PLATS-IDDC-CROSS > SPACES                                         
215701      MOVE W-KDSEGKEY-X         TO  CROSS-KDSEGKEY                        
215801      MOVE PLATS-IDDC           TO  CROSS-IDDC-SEND                       
215901      MOVE PLATS-IDDC-CROSS     TO  CROSS-IDDC-CROSS                      
216001      MOVE KOLLI-IDDISTR        TO  CROSS-IDDISTR                         
216101      MOVE KOLLI-IDKUNDNR       TO  CROSS-IDKUNDNR                        
216201      MOVE VORD-IDPRODNR        TO  CROSS-IDPRODNR                        
216301      MOVE KOLLI-IDKOLLI        TO  CROSS-IDKOLLI                         
216401      MOVE KOLLI-IDLEVNR        TO  CROSS-IDLEVNR                         
216501      MOVE KOLLI-IDSUPREF       TO  CROSS-IDSUPREF                        
216601      MOVE KOLLI-DARFS(3:6)     TO  CROSS-TIRFSDAT                        
216701      MOVE ZERO                 TO  CROSS-IDTRPTNR-CROSS                  
216801      MOVE ZERO                 TO  CROSS-TIRECXDAT                       
216901      MOVE ZERO                 TO  CROSS-TIRECXTID                       
217001      MOVE ZERO                 TO  CROSS-TISKEPPN                        
217101      MOVE ZERO                 TO  CROSS-IDSHIPM-CROSS                   
217201      MOVE SPACE                TO  CROSS-IDLBBET-CROSS                   
217301      MOVE 1                    TO  CROSS-KDKOLSTA-CROSS                  
217401      PERFORM IMS-ISRT-WDE621                                             
217501     END-IF                                                               
217600                                                                          
217701     IF WS-KDTRANS = 'W4A343'                                             
217801       CONTINUE                                                           
217901     ELSE                                                                 
218001       PERFORM S99A-PACKTRANS-VR-ISRT                                     
218101     END-IF                                                               
218200     .                                                                    
218300     EJECT                                                                
218400 S01A-KOLLA-PLATS  SECTION.                                               
218500                                                                          
218600     IF VORD-KDORDKL = 4                                                  
218700       IF  VORD-IDDISTR  = +00878                                         
218800       AND VORD-IDKUNDNR > +006000                                        
218900         MOVE +2                TO PLATS-KDCALL                           
219000       ELSE                                                               
219100         MOVE +1                TO PLATS-KDCALL                           
219200       END-IF                                                             
219300     ELSE                                                                 
219400       MOVE +2                  TO PLATS-KDCALL                           
219500     END-IF                                                               
219600     IF SPAR-KOLLI-KDFARLIG = +4                                          
219700     OR SPAR-KOLLI-KDFARLIG = +7                                          
219800       MOVE +6                  TO PLATS-KDCALL                           
219900     ELSE                                                                 
220000       MOVE ZERO                TO PLATS-KDCALL                           
220100     END-IF                                                               
220200     MOVE VORD-IDDC             TO PLATS-IDDC                             
220300     MOVE VORD-IDDISTR          TO PLATS-IDDISTR                          
220400     MOVE VORD-IDKUNDNR         TO PLATS-IDKUNDNR                         
220500     MOVE VORD-KDFRAKT          TO PLATS-KDFRAKT                          
220600     MOVE VORD-KDORDKL          TO PLATS-KDORDKLX                         
220700     MOVE ZERO                  TO PLATS-IDORDNR                          
220800     MOVE ZERO                  TO PLATS-DIKOLLIH                         
220900                                   PLATS-DIKOLLIL                         
221000                                   PLATS-DIKOLLIB                         
221100                                   PLATS-VKORDNTO-KOLLI                   
221200     MOVE SPACE                 TO PLATS-KDKOLLID                         
221300     MOVE SPACE                 TO PLATS-ADFLGEO                          
221400     MOVE ZERO                  TO PLATS-ADFLOMR                          
221500     MOVE ZERO                  TO PLATS-ADRUTNIV                         
221600     MOVE ZERO                  TO PLATS-IDTRPTNR                         
221700                                   PLATS-DIHMODUL                         
221800                                   PLATS-DIDMODUL                         
221900                                   PLATS-ADVMODUL                         
222000                                   PLATS-ADHMODUL                         
222100     MOVE SPACE                 TO PLATS-FLUTLAST                         
222200                                   PLATS-IDDC-CROSS                       
222300                                                                          
222400                                                                          
222500     .                                                                    
222600 S02-REDIGERA-BILD  SECTION.                                              
222700                                                                          
222800     MOVE ORAD-IDPURAD           TO RESP-IDRADNR-FOM                      
222900     MOVE ZERO TO MIX                                                     
223000     MOVE ZERO TO RESP-KVRADER                                            
223100     PERFORM UNTIL NOT SEGMENT-FINNS                                      
223200     OR MIX = MAX-KVRADER                                                 
223300                                                                          
223400       MOVE KKOLLI-IDKOLLI   TO WS-KKOLLI-IDKOLLI                         
223500                                                                          
223600       IF KKOLLI-IDKOLLI = WS-MID-IDKOLLI                                 
223700         ADD 1 TO MIX                                                     
223800         MOVE NEJ              TO RESP-FLNOLLAD-LINE (MIX)                
223900         MOVE ORAD-IDPURAD     TO RESP-IDPURAD-LINE (MIX)                 
224000         MOVE ORAD-IDARTNR     TO RESP-IDARTNR-LINE (MIX)                 
224100         MOVE KKOLLI-KVLEVART  TO RESP-KVLEVART-LINE (MIX)                
224200         MOVE ALL-SPACE        TO RESP-KDSVAR-LINE (MIX)                  
224300                                  RESP-IDKOLLI-LINE (MIX)                 
224400                                  RESP-KVLEVART-LINE-IN (MIX)             
224500         MOVE MFS-FORMATETS-ATTR TO RESP-IDKOLLI-LINE-ATTR (MIX)          
224600                                RESP-KVLEVART-LINE-IN-ATTR (MIX)          
224700         MOVE ORAD-IDPURAD     TO RESP-IDRADNR-TOM                        
224800       END-IF                                                             
224900                                                                          
225000       PERFORM IMS-GN-WDE411-21                                           
225100     END-PERFORM                                                          
225200     MOVE MIX TO RESP-KVRADER                                             
225300     IF SEGMENT-FINNS                                                     
225400       MOVE INF-MORE-PRESS-ENTER TO RESP-IDMSG-INFO                       
225500       MOVE 'ENTER'              TO RESP-IDELMT-ERROR                     
225600     END-IF                                                               
225700                                                                          
225800     PERFORM MFS-STAENG-NYTT-KOLLI                                        
225900                                                                          
226000     .                                                                    
226100     EJECT                                                                
226200 S03-GET-INPUT-IDPLKLST SECTION.                                          
226300                                                                          
226400     MOVE IDPRODNR-WS       TO W-IDPRODNR-E4F-MIN                         
226500                               W-IDPRODNR-E4F-MAX                         
226600     MOVE IDKOLLI-WS        TO W-IDKOLLI-E4F-MIN                          
226700                               W-IDKOLLI-E4F-MAX                          
226800     PERFORM IMS-GU-WDE4F1                                                
226900     IF SEGMENT-FINNS                                                     
227000        MOVE SEQF-IDPLKLST  TO WS-IDPLKLST-IP                             
227100     END-IF                                                               
227200     .                                                                    
227300     EJECT                                                                
227400 S04-SPARA-ORAD  SECTION.                                                 
227500     SKIP2                                                                
227600     MOVE ORAD-KDFARLIG      TO SPAR-ORAD-KDFARLIG                        
227700     MOVE ORAD-KVFLAMP       TO SPAR-ORAD-KVFLAMP                         
227800     MOVE ORAD-PRARTNTO      TO SPAR-ORAD-PRARTNTO                        
227900     MOVE ORAD-PRAVCOST      TO SPAR-ORAD-PRAVCOST                        
228000     MOVE ORAD-PRARTNTO-LOC  TO SPAR-ORAD-PRARTNTO-LOC                    
228100     MOVE ORAD-PRARTNTO-LOCPREL                                           
228200                             TO SPAR-ORAD-PRARTNTO-LOCPREL                
228300     MOVE ORAD-KDVALISO      TO SPAR-ORAD-KDVALISO                        
228400     MOVE ORAD-KDVALISO-EXP  TO SPAR-ORAD-KDVALISO-EXP                    
228500     MOVE ORAD-VKARTNTO      TO SPAR-ORAD-VKARTNTO                        
228600*                                                                         
228700     MOVE ORAD-IDPSN         TO SPAR-ORAD-IDPSN                           
228800     MOVE ORAD-VKART-FG      TO SPAR-ORAD-VKART-FG                        
228900     MOVE ORAD-VLFG          TO SPAR-ORAD-VLFG                            
229000     MOVE ORAD-SUEQFG        TO SPAR-ORAD-SUEQFG                          
229100     SKIP3                                                                
229200     .                                                                    
229300 S05-CHECK-PRC-OF-CASES  SECTION.                                         
229400                                                                          
229500     MOVE WS-CHK-IDKOLLI                TO W-IDKOLLI-E4F-MIN              
229600                                           W-IDKOLLI-E4F-MAX              
229700     PERFORM IMS-GU-WDE4F1                                                
229800     IF SEGMENT-FINNS                                                     
229900        MOVE SEQF-IDPLKLST              TO WS-IDPLKLST-LINE               
230000        IF WS-IDPLKLST-LINE NOT = WS-IDPLKLST-IP                          
230100           MOVE NEJ                     TO DATA-RAETT-SW                  
230200           IF REQU-FLJANEJ-ALLA = 'JA ' OR 'YES'                          
230300              MOVE MFS-NUM-FIELD-WRONG  TO REQU-IDKOLLI-ALLA              
230400           ELSE                                                           
230500              MOVE MFS-NUM-FIELD-WRONG  TO REQU-IDKOLLI-LINE (MIX)        
230600           END-IF                                                         
230700           MOVE ERR-INVALID-COMBINATION TO RESP-IDMSG-ERROR               
230800           MOVE 'DIFFERENT PRC'         TO RESP-IDELMT-ERROR              
230900           MOVE JA                      TO FELTEXT-UTLAGD-SW              
231000        END-IF                                                            
231100     END-IF                                                               
231200     .                                                                    
231300     SKIP2                                                                
231400 S06-SPARA-KOLLI  SECTION.                                                
231500                                                                          
231600     MOVE KOLLI-KDKOLSTA       TO SPAR-KOLLI-KDKOLSTA                     
231700     MOVE KOLLI-KVORDRAD       TO SPAR-KOLLI-KVORDRAD                     
231800     MOVE KOLLI-TIPACKN        TO SPAR-KOLLI-TIPACKN                      
231900     MOVE KOLLI-KDFARLIG-KOLLI TO SPAR-KOLLI-KDFARLIG                     
232000     MOVE KOLLI-KVFLAMP-KOLLI  TO SPAR-KOLLI-KVFLAMP                      
232100     MOVE KOLLI-VKORDBTO-KOLLI TO SPAR-KOLLI-VKORDBTO                     
232200     MOVE KOLLI-VKORDNTO-KOLLI TO SPAR-KOLLI-VKORDNTO                     
232300     MOVE KOLLI-VLORDBTO-KOLLI TO SPAR-KOLLI-VLORDBTO                     
232400     MOVE KOLLI-IDTRPLOS       TO SPAR-KOLLI-IDTRPLOS                     
232500     MOVE KOLLI-IDTRPVAR       TO SPAR-KOLLI-IDTRPVAR                     
232600     MOVE KOLLI-IDPLOCK        TO SPAR-KOLLI-IDPLOCK                      
232700     .                                                                    
232800     EJECT                                                                
232900 S07-UPPDAT-ARB-KOLLI  SECTION.                                           
233000     SKIP2                                                                
233100     IF SPAR-ORAD-KDFARLIG    > SPAR-KOLLI-KDFARLIG                       
233200        MOVE SPAR-ORAD-KDFARLIG   TO SPAR-KOLLI-KDFARLIG                  
233300     END-IF                                                               
233400     IF (SPAR-ORAD-KVFLAMP    < SPAR-KOLLI-KVFLAMP AND                    
233500       SPAR-ORAD-KVFLAMP    > ZERO) OR                                    
233600       SPAR-KOLLI-KVFLAMP    = ZERO                                       
233700       MOVE SPAR-ORAD-KVFLAMP    TO SPAR-KOLLI-KVFLAMP                    
233800     END-IF                                                               
233900                                                                          
234000     COMPUTE SUORDV-WS = SUORDV-WS                                        
234100                         + (SPAR-ORAD-PRARTNTO                            
234200                         * KVLEVART-WS)                                   
234300     COMPUTE SUORDV-EXP-WS = SUORDV-EXP-WS                                
234400                         + (SPAR-ORAD-PRAVCOST                            
234500                         * KVLEVART-WS)                                   
234600     COMPUTE SUORDV-LOC-WS       = SUORDV-LOC-WS                          
234700                         + (SPAR-ORAD-PRARTNTO-LOC                        
234800                         * KVLEVART-WS)                                   
234900     COMPUTE SUORDV-LOCPREL-WS   = SUORDV-LOCPREL-WS                      
235000                         + (SPAR-ORAD-PRARTNTO-LOCPREL                    
235100                         * KVLEVART-WS)                                   
235200     COMPUTE VKORDNTO-WS = VKORDNTO-WS                                    
235300                         + (SPAR-ORAD-VKARTNTO                            
235400                         * KVLEVART-WS)                                   
235501                                                                          
235601     IF REQU-IDKOLLI-NY = REQU-IDKOLLI-LINE (MIX)                         
235701       COMPUTE VKORDNTO-WS-NYTT = VKORDNTO-WS-NYTT                        
235801                           + (SPAR-ORAD-VKARTNTO                          
235901                           * KVLEVART-WS)                                 
236001     END-IF                                                               
236101                                                                          
236200     MOVE SPAR-ORAD-KDVALISO     TO KDVALISO-WS                           
236300     MOVE SPAR-ORAD-KDVALISO-EXP TO KDVALISO-EXP-WS                       
236400*                                                                         
236500     PERFORM S07A-SPARA-FG-DATA                                           
236600     .                                                                    
236700     EJECT                                                                
236800 S07A-SPARA-FG-DATA SECTION.                                              
236900                                                                          
237000     MOVE +1 TO TAB-INDX                                                  
237100     PERFORM UNTIL TAB-INDX > MAX-FG-INDX                                 
237200                                                                          
237300       IF TAB-IDPSN(TAB-INDX) = ZERO                                      
237400         MOVE SPAR-ORAD-IDPSN TO TAB-IDPSN(TAB-INDX)                      
237500         PERFORM S07AA-BERAEKNA-FG-FAELT                                  
237600                                                                          
237700       ELSE                                                               
237800         IF SPAR-ORAD-IDPSN = TAB-IDPSN(TAB-INDX)                         
237900           PERFORM S07AA-BERAEKNA-FG-FAELT                                
238000         END-IF                                                           
238100       END-IF                                                             
238200                                                                          
238300       ADD +1 TO TAB-INDX                                                 
238400     END-PERFORM                                                          
238500                                                                          
238600     COMPUTE TOTAL-SUEQFG = TOTAL-SUEQFG      +                           
238700                            (SPAR-ORAD-SUEQFG *                           
238800                             KVLEVART-WS)                                 
238900     .                                                                    
239000     EJECT                                                                
239100 S07AA-BERAEKNA-FG-FAELT SECTION.                                         
239200                                                                          
239300     COMPUTE TAB-VLFG(TAB-INDX) = TAB-VLFG(TAB-INDX) +                    
239400                                 (SPAR-ORAD-VLFG     *                    
239500                                  KVLEVART-WS)                            
239600     IF SPAR-ORAD-IDPSN = 10 OR 11                                        
239700       COMPUTE TAB-VKART-FG(TAB-INDX) = TAB-VKART-FG(TAB-INDX) +          
239800                                       (SPAR-ORAD-VKART-FG     *          
239900                                        KVLEVART-WS)                      
240000     ELSE                                                                 
240100       MOVE ZERO TO TAB-VKART-FG(TAB-INDX)                                
240200     END-IF                                                               
240300                                                                          
240400     MOVE 10 TO TAB-INDX                                                  
240500     .                                                                    
240600     EJECT                                                                
240700 S08-UPPDAT-FRAN-KOLLI  SECTION.                                          
240800                                                                          
240900     MOVE IDKOLLI-WS          TO W-IDKOLLI                                
241000     PERFORM IMS-GHU-WDE611                                               
241101     SUBTRACT SUORDV-WS         FROM KOLLI-SUORDV-KOLLI                   
241201     SUBTRACT SUORDV-EXP-WS     FROM KOLLI-SUORDV-KLI-EXP                 
241301     SUBTRACT SUORDV-LOC-WS     FROM KOLLI-SUORDV-LOC                     
241401     SUBTRACT SUORDV-LOCPREL-WS FROM KOLLI-SUORDV-LOCPREL                 
241501     SUBTRACT VKORDNTO-WS       FROM KOLLI-VKORDNTO-KOLLI                 
241601     SUBTRACT VKORDNTO-WS       FROM KOLLI-VKORDBTO-KOLLI                 
241700                                                                          
241800     IF KOLLI-VKORDNTO-KOLLI < ZERO                                       
241900         MOVE ZERO        TO   KOLLI-VKORDNTO-KOLLI                       
242000     END-IF                                                               
242100     IF KOLLI-VKORDBTO-KOLLI < ZERO                                       
242200         MOVE ZERO        TO   KOLLI-VKORDBTO-KOLLI                       
242300     END-IF                                                               
242401     IF KOLLI-VKORDNTO-KOLLI > KOLLI-VKORDBTO-KOLLI                       
242501         MOVE KOLLI-VKORDBTO-KOLLI                                        
242601                          TO   KOLLI-VKORDNTO-KOLLI                       
242701     END-IF                                                               
242801                                                                          
242900     SUBTRACT ANTAL-DLET-WS FROM KOLLI-KVORDRAD                           
243000     MOVE KOLLI-KVORDRAD     TO KVORDRAD-WS                               
243100                                                                          
243200     PERFORM S08B-UPPDAT-FRAN-FG-DATA                                     
243300                                                                          
243400     IF KOLLI-KVORDRAD = ZERO                                             
243500        MOVE KOLLI-VKORDBTO-KOLLI TO WS-SPAR-VKORDBTO-DLET                
243600        MOVE KOLLI-VLORDBTO-KOLLI TO WS-SPAR-VLORDBTO-DLET                
243700        MOVE KOLLI-IDTRPTNR TO WS-SPAR-IDTRPTNR-DLET                      
243800        MOVE KOLLI-DARFS    TO WS-SPAR-DARFS-DLET                         
243900        MOVE KOLLI-ADCLGEO  TO WS-SPAR-ADCLGEO-DLET                       
244000        MOVE KOLLI-ADFLOMR  TO WS-SPAR-ADFLOMR-DLET                       
244100        MOVE KOLLI-ADRUTNIV TO WS-SPAR-ADRUTNIV-DLET                      
244200        MOVE KOLLI-DIHMODUL TO WS-SPAR-DIHMODUL-DLET                      
244300        MOVE KOLLI-DIDMODUL TO WS-SPAR-DIDMODUL-DLET                      
244400        MOVE KOLLI-ADVMODUL TO WS-SPAR-ADVMODUL-DLET                      
244500        MOVE KOLLI-ADHMODUL TO WS-SPAR-ADHMODUL-DLET                      
244600        PERFORM IMS-DLET-WDE611                                           
244700        PERFORM S99B-PACKTRANS-VR-DLET                                    
244800        PERFORM S08A-BOKA-AV-PLATS                                        
244900                                                                          
245000        PERFORM IMS-GHU-WDE601                                            
245100        COMPUTE VORD-KVKOLLI  = VORD-KVKOLLI - 1                          
245200        COMPUTE VORD-KVKOLPAC = VORD-KVKOLPAC - 1                         
245300                                                                          
245400        COMPUTE VORD-VKORDBTO =                                           
245500        VORD-VKORDBTO - WS-SPAR-VKORDBTO-DLET                             
245600        COMPUTE VORD-VLORDBTO =                                           
245700        VORD-VLORDBTO - WS-SPAR-VLORDBTO-DLET                             
245800        MOVE ZERO TO KVORDRAD-WS                                          
245900        PERFORM IMS-REPL-WDE601                                           
246000     ELSE                                                                 
246101        PERFORM IMS-REPL-WDE611                                           
246200     END-IF                                                               
246300     .                                                                    
246400     EJECT                                                                
246500 S08A-BOKA-AV-PLATS  SECTION.                                             
246600     SKIP2                                                                
246700     MOVE +3                    TO PLATS-KDCALL                           
246800     MOVE WS-SPAR-IDTRPTNR-DLET TO PLATS-IDTRPTNR                         
246900     MOVE WS-SPAR-DARFS-DLET (3:10) TO PLATS-TIRFS                        
247000     MOVE WS-SPAR-ADCLGEO-DLET  TO PLATS-ADCLGEO                          
247100     MOVE WS-SPAR-ADFLOMR-DLET  TO PLATS-ADFLOMR                          
247200     MOVE WS-SPAR-ADRUTNIV-DLET TO PLATS-ADRUTNIV                         
247300     MOVE WS-SPAR-DIHMODUL-DLET TO PLATS-DIHMODUL                         
247400     MOVE WS-SPAR-DIDMODUL-DLET TO PLATS-DIDMODUL                         
247500     MOVE WS-SPAR-ADVMODUL-DLET TO PLATS-ADVMODUL                         
247600     MOVE WS-SPAR-ADHMODUL-DLET TO PLATS-ADHMODUL                         
247700                                                                          
247800     CALL W403PLAT USING PLATS-W403PLAT                                   
247900                         PLATS-DM-PCB                                     
248000                         PLATS-DN-PCB                                     
248100                         PLATS-DP-PCB                                     
248200                         PLATS-DO-PCB                                     
248300                         PLATS-WDE6C-PCB                                  
248400                         PLATS-GMTC-PCB                                   
248500                         PLATS-WDB6-PCB                                   
248600     .                                                                    
248700     EJECT                                                                
248800 S08B-UPPDAT-FRAN-FG-DATA SECTION.                                        
248900                                                                          
249000     MOVE +1 TO TAB-INDX                                                  
249100     PERFORM UNTIL TAB-INDX > MAX-FG-INDX                                 
249200       IF TAB-IDPSN(TAB-INDX) > ZERO                                      
249300         MOVE +1 TO FG-INDX                                               
249400                                                                          
249500         PERFORM UNTIL FG-INDX > MAX-FG-INDX                              
249600           IF TAB-IDPSN(TAB-INDX) = KOLLI-IDPSN(FG-INDX)                  
249700             SUBTRACT TAB-VLFG(TAB-INDX) FROM                             
249800                      KOLLI-VLFG(FG-INDX)                                 
249900                                                                          
250000             IF KOLLI-IDPSN(FG-INDX) = 10 OR 11                           
250100               SUBTRACT TAB-VKART-FG(TAB-INDX) FROM                       
250200                        KOLLI-VKART-FG(FG-INDX)                           
250300             END-IF                                                       
250400             IF KOLLI-VLFG(FG-INDX) = ZERO                                
250500               MOVE ZERO TO KOLLI-IDPSN(FG-INDX)                          
250600             END-IF                                                       
250700                                                                          
250800             MOVE +10 TO FG-INDX                                          
250900           END-IF                                                         
251000           ADD +1 TO FG-INDX                                              
251100         END-PERFORM                                                      
251200       END-IF                                                             
251300                                                                          
251400       ADD +1 TO TAB-INDX                                                 
251500     END-PERFORM                                                          
251600                                                                          
251700     IF TOTAL-SUEQFG > ZERO                                               
251800       SUBTRACT TOTAL-SUEQFG FROM KOLLI-SUEQFG                            
251900     END-IF                                                               
252000     .                                                                    
252100     EJECT                                                                
252200 S09-UPPDAT-TILL-KOLLI-ALLA  SECTION.                                     
252300     SKIP2                                                                
252400     MOVE REQU-IDKOLLI-ALLA       TO W-IDKOLLI                            
252500     PERFORM IMS-GHU-WDE611                                               
252600     IF SPAR-KOLLI-KDFARLIG    > KOLLI-KDFARLIG-KOLLI                     
252700        MOVE SPAR-KOLLI-KDFARLIG   TO KOLLI-KDFARLIG-KOLLI                
252800     END-IF                                                               
252900     IF (SPAR-KOLLI-KVFLAMP    < KOLLI-KVFLAMP-KOLLI AND                  
253000       SPAR-KOLLI-KVFLAMP    > ZERO) OR                                   
253100       KOLLI-KVFLAMP-KOLLI    = ZERO                                      
253200       MOVE SPAR-KOLLI-KVFLAMP    TO KOLLI-KVFLAMP-KOLLI                  
253300     END-IF                                                               
253400     ADD SUORDV-WS     TO KOLLI-SUORDV-KOLLI                              
253500     ADD SUORDV-EXP-WS TO KOLLI-SUORDV-KLI-EXP                            
253600     ADD SUORDV-LOC-WS   TO KOLLI-SUORDV-LOC                              
253700     ADD SUORDV-LOCPREL-WS   TO KOLLI-SUORDV-LOCPREL                      
253800     MOVE KDVALISO-WS        TO KOLLI-KDVALISO                            
253900     MOVE KDVALISO-EXP-WS    TO KOLLI-KDVALISO-EXP                        
254000     ADD VKORDNTO-WS         TO KOLLI-VKORDNTO-KOLLI                      
254100                                KOLLI-VKORDBTO-KOLLI                      
254200                                                                          
254301     IF  KOLLI-VKORDNTO-KOLLI > KOLLI-VKORDBTO-KOLLI                      
254401         MOVE KOLLI-VKORDBTO-KOLLI                                        
254501                          TO KOLLI-VKORDNTO-KOLLI                         
254601     END-IF                                                               
254701     ADD ANTAL-ISRT-WS       TO KOLLI-KVORDRAD                            
254800*                                                                         
254900     PERFORM S20-UPPDAT-TILL-FG-DATA                                      
255000*                                                                         
255100     PERFORM IMS-REPL-WDE611                                              
255200     IF NYTT-KOLLI                                                        
255300       PERFORM S30-UPPDAT-VORD                                            
255400       MOVE NEJ TO NYTT-KOLLI-SW                                          
255500     END-IF                                                               
255600     .                                                                    
255700     EJECT                                                                
255800 S10-UPPDAT-TILL-KOLLI-RAD  SECTION.                                      
255900     SKIP2                                                                
256000     MOVE REQU-IDKOLLI-LINE (MIX) TO W-IDKOLLI                            
256100     PERFORM IMS-GHU-WDE611                                               
256200                                                                          
256300     IF SPAR-ORAD-KDFARLIG    > KOLLI-KDFARLIG-KOLLI                      
256400        MOVE SPAR-ORAD-KDFARLIG   TO KOLLI-KDFARLIG-KOLLI                 
256500     END-IF                                                               
256600     IF (SPAR-ORAD-KVFLAMP    < KOLLI-KVFLAMP-KOLLI AND                   
256700       SPAR-ORAD-KVFLAMP    > ZERO) OR                                    
256800       KOLLI-KVFLAMP-KOLLI    = ZERO                                      
256900       MOVE SPAR-ORAD-KVFLAMP    TO KOLLI-KVFLAMP-KOLLI                   
257000     END-IF                                                               
257100      COMPUTE KOLLI-SUORDV-KOLLI                                          
257200                          = KOLLI-SUORDV-KOLLI                            
257300                          + (SPAR-ORAD-PRARTNTO                           
257400                          * KVLEVART-WS)                                  
257500      COMPUTE KOLLI-SUORDV-KLI-EXP                                        
257600                          = KOLLI-SUORDV-KLI-EXP                          
257700                          + (SPAR-ORAD-PRAVCOST                           
257800                          * KVLEVART-WS)                                  
257900      COMPUTE KOLLI-SUORDV-LOC                                            
258000                          = KOLLI-SUORDV-LOC                              
258100                          + (SPAR-ORAD-PRARTNTO-LOC                       
258200                          * KVLEVART-WS)                                  
258300      COMPUTE KOLLI-SUORDV-LOCPREL                                        
258400                          = KOLLI-SUORDV-LOCPREL                          
258500                          + (SPAR-ORAD-PRARTNTO-LOCPREL                   
258600                          * KVLEVART-WS)                                  
258700      COMPUTE KOLLI-VKORDNTO-KOLLI                                        
258800                          = KOLLI-VKORDNTO-KOLLI                          
258900                          + (SPAR-ORAD-VKARTNTO                           
259000                          * KVLEVART-WS)                                  
259100                                                                          
259200      COMPUTE KOLLI-VKORDBTO-KOLLI                                        
259301                          = KOLLI-VKORDBTO-KOLLI                          
259401                          + (SPAR-ORAD-VKARTNTO                           
259500                          * KVLEVART-WS)                                  
259600                                                                          
259700     MOVE SPAR-ORAD-KDVALISO     TO KOLLI-KDVALISO                        
259800     MOVE SPAR-ORAD-KDVALISO-EXP TO KOLLI-KDVALISO-EXP                    
259900     ADD ANTAL-ISRT-WS   TO KOLLI-KVORDRAD                                
260000*                                                                         
260100     PERFORM S20-UPPDAT-TILL-FG-DATA                                      
260200*                                                                         
260300     PERFORM IMS-REPL-WDE611                                              
260400     .                                                                    
260500     EJECT                                                                
260600 S11-KOLLI-SAKNAS  SECTION.                                               
260700                                                                          
260800     MOVE ERR-CASE-MISSING       TO RESP-IDMSG-ERROR                      
260900     MOVE 'IDKOLLI'              TO RESP-IDELMT-ERROR                     
261000     PERFORM MFS-SAMMA-BILD                                               
261100     PERFORM MFS-STAENG-ALLT                                              
261200     SKIP3                                                                
261300                                                                          
261400     .                                                                    
261500 S11-ORDER-SAKNAS  SECTION.                                               
261600                                                                          
261700     MOVE ERR-ORDER-MISSING      TO RESP-IDMSG-ERROR                      
261800     MOVE 'IDPRODNR'             TO RESP-IDELMT-ERROR                     
261900     PERFORM MFS-SAMMA-BILD                                               
262000     PERFORM MFS-STAENG-ALLT                                              
262100     SKIP3                                                                
262200                                                                          
262300     .                                                                    
262400 S12-MIXED-CASE    SECTION.                                               
262500                                                                          
262600     MOVE ERR-MIXED-CASE         TO RESP-IDMSG-ERROR                      
262700     MOVE 'MIXEDCASE'            TO RESP-IDELMT-ERROR                     
262800     PERFORM MFS-SAMMA-BILD                                               
262900     PERFORM MFS-STAENG-ALLT                                              
263000     SKIP3                                                                
263100                                                                          
263200     .                                                                    
263300 S13-KOLLI-FAKTURERAT  SECTION.                                           
263400                                                                          
263500     MOVE ERR-CASE-INVOICED-OR-RELEASED                                   
263600                                 TO RESP-IDMSG-ERROR                      
263700     PERFORM MFS-SAMMA-BILD                                               
263800     PERFORM MFS-STAENG-ALLT                                              
263900     SKIP3                                                                
264000                                                                          
264100     .                                                                    
264200 S14-FEL-INMATAT  SECTION.                                                
264300                                                                          
264400     IF FELTEXT-UTLAGD                                                    
264500       CONTINUE                                                           
264600     ELSE                                                                 
264700       IF REQU-IDREQVER = '001' OR '002'                                  
264800         MOVE ERR-HILITE-FIELDS-WRONG-WEB TO RESP-IDMSG-ERROR             
264900       ELSE                                                               
265000         MOVE ERR-HILITE-FIELDS-WRONG     TO RESP-IDMSG-ERROR             
265100       END-IF                                                             
265200     END-IF                                                               
265300     PERFORM MFS-SAMMA-BILD                                               
265400     SKIP3                                                                
265500                                                                          
265600     .                                                                    
265700 S15-FEL-SKYDDADE  SECTION.                                               
265800                                                                          
265900     MOVE ERR-INVALID-COMBINATION                                         
266000                              TO RESP-IDMSG-ERROR                         
266100     PERFORM MFS-SAMMA-BILD                                               
266200     SKIP3                                                                
266300                                                                          
266400     .                                                                    
266500 S16-REGISTER-AENDRAT  SECTION.                                           
266600                                                                          
266700     MOVE ERR-ORDER-CHANGED   TO RESP-IDMSG-ERROR                         
266800     PERFORM MFS-SAMMA-BILD                                               
266900     MOVE NEJ                 TO REGISTER-OK-SW                           
267000     SKIP3                                                                
267100                                                                          
267200     .                                                                    
267300 S17-NYCKLAR-FEL  SECTION.                                                
267400                                                                          
267500     MOVE ERR-WRONG-KEY          TO RESP-IDMSG-ERROR                      
267600     PERFORM MFS-SAMMA-BILD                                               
267700     PERFORM MFS-STAENG-ALLT                                              
267800     SKIP3                                                                
267900                                                                          
268000     .                                                                    
268100 S18-KOLLI-TOMT  SECTION.                                                 
268200                                                                          
268300     MOVE ERR-CASE-LINES-MISSING TO RESP-IDMSG-ERROR                      
268400     PERFORM MFS-SAMMA-BILD                                               
268500     MOVE NEJ                 TO REGISTER-OK-SW                           
268600                                                                          
268700     .                                                                    
268800     EJECT                                                                
268900 S19-KOLLI-TOMT  SECTION.                                                 
269000                                                                          
269100     MOVE ERR-CASE-LINES-MISSING TO RESP-IDMSG-ERROR                      
269200     PERFORM MFS-SAMMA-BILD                                               
269300     MOVE NEJ                 TO REGISTER-OK-SW                           
269400                                                                          
269500     .                                                                    
269600     EJECT                                                                
269700 S21-WEIGHT-ERR        SECTION.                                           
269800                                                                          
269900     MOVE NEJ                 TO DATA-RAETT-SW                            
270000     MOVE MFS-NUM-FAELT-FEL   TO RESP-VKORDBTO-ATTR                       
270101     MOVE 'VKORDBTO'          TO RESP-IDELMT-ERROR                        
270200     PERFORM MFS-SAMMA-BILD                                               
270300     SKIP3                                                                
270400     .                                                                    
270500 S20-UPPDAT-TILL-FG-DATA SECTION.                                         
270600                                                                          
270700     MOVE +1 TO TAB-INDX                                                  
270800     PERFORM UNTIL TAB-INDX > MAX-FG-INDX                                 
270900       IF TAB-IDPSN(TAB-INDX) > ZERO                                      
271000         MOVE +1 TO FG-INDX                                               
271100                                                                          
271200         PERFORM UNTIL FG-INDX > MAX-FG-INDX                              
271300           IF TAB-IDPSN(TAB-INDX) = KOLLI-IDPSN(FG-INDX)                  
271400             ADD TAB-VLFG(TAB-INDX) TO                                    
271500                 KOLLI-VLFG(FG-INDX)                                      
271600             IF KOLLI-IDPSN(FG-INDX) = 10 OR 11                           
271700               ADD TAB-VKART-FG(TAB-INDX) TO                              
271800                   KOLLI-VKART-FG(FG-INDX)                                
271900             END-IF                                                       
272000             MOVE +10 TO FG-INDX                                          
272100           ELSE                                                           
272200             IF KOLLI-IDPSN(FG-INDX) = ZERO                               
272300               MOVE TAB-IDPSN(TAB-INDX) TO KOLLI-IDPSN(FG-INDX)           
272400               MOVE TAB-VLFG(TAB-INDX)  TO KOLLI-VLFG(FG-INDX)            
272500               IF TAB-IDPSN(TAB-INDX) = 10 OR 11                          
272600                 MOVE TAB-VKART-FG(TAB-INDX) TO                           
272700                      KOLLI-VKART-FG(FG-INDX)                             
272800               END-IF                                                     
272900               MOVE +10 TO FG-INDX                                        
273000             END-IF                                                       
273100           END-IF                                                         
273200           ADD +1 TO FG-INDX                                              
273300         END-PERFORM                                                      
273400       ELSE                                                               
273500         MOVE +10 TO TAB-INDX                                             
273600       END-IF                                                             
273700                                                                          
273800       ADD +1 TO TAB-INDX                                                 
273900     END-PERFORM                                                          
274000                                                                          
274100     IF TOTAL-SUEQFG > ZERO                                               
274200       ADD TOTAL-SUEQFG TO KOLLI-SUEQFG                                   
274300     END-IF                                                               
274400     .                                                                    
274500     EJECT                                                                
274600 S30-UPPDAT-VORD  SECTION.                                                
274700                                                                          
274800     PERFORM IMS-GHU-WDE601                                               
274900     ADD  1                    TO VORD-KVKOLLI                            
275000     ADD  1                    TO VORD-KVKOLPAC                           
275100     MOVE KOLLI-TIPACKN        TO VORD-TIPACKN-SK                         
275200     ADD  KOLLI-VLORDBTO-KOLLI TO VORD-VLORDBTO                           
275301     ADD  VKTARA-WS            TO VORD-VKORDBTO                           
275400                                                                          
275500     MOVE VORD-IDDISTR         TO TEST-IDDISTR                            
275600                                                                          
275700     IF  VORD-KDORDSTA < +3                                               
275800     AND VORD-KVKOLLI  > +0                                               
275900     AND DIST03-SVERIGE                                                   
276000                                                                          
276100       MOVE VORD-KDFRAKT   TO WS-KDFRAKT                                  
276200       MOVE WS-KDFRAKT     TO FRAK01-KDFRAKT                              
276300       IF  FRAK01-SVERIGE2                                                
276400       OR  FRAK01-NORDEN                                                  
276500       OR  FRAK01-KDFRAKT21                                               
276600       OR  FRAK01-KDFRAKT62                                               
276700         IF  VORD-FLDIRLEV = NEJ                                          
276800         AND VORD-KDFRAKT  NOT = +17                                      
276900            IF NOT DIS128-FRAKTS                                          
277000              MOVE JA TO VORD-FLFRAKTS                                    
277100            END-IF                                                        
277200         END-IF                                                           
277300       END-IF                                                             
277400     END-IF                                                               
277500                                                                          
277600     PERFORM IMS-REPL-WDE601                                              
277700     .                                                                    
277800     EJECT                                                                
277900                                                                          
278000                                                                          
278100 S32-DATA-TILL-DEL-NOTE-KOLLI SECTION.                                    
278200                                                                          
278300     MOVE VORD-IDDISTR          TO TEST-IDDISTR                           
278400     IF DIST07-USA-RETAILER-DNOTE                                         
278500     OR DIST07-CAN-RETAILER                                               
278600                                                                          
278700        MOVE LOW-VALUE     TO   W-WDQ5A1KY-MIN-X                          
278800        MOVE HIGH-VALUE    TO   W-WDQ5A1KY-MAX-X                          
278900                                                                          
279000        MOVE SEQF-IDDISTR       TO  W-WDQ5A1-IDDISTR-MIN                  
279100                                    W-WDQ5A1-IDDISTR-MAX                  
279200        MOVE SEQF-IDKUNDNR      TO  W-WDQ5A1-IDKUNDNR-MIN                 
279300                                    W-WDQ5A1-IDKUNDNR-MAX                 
279400        MOVE ZERO               TO  W-WDQ5A1-IDORDNR7-MIN                 
279500                                    W-WDQ5A1-IDORDNR7-MAX                 
279600        MOVE SEQF-IDORDNR5      TO  W-WDQ5A1-IDORDNR7-MIN(3:5)            
279700                                    W-WDQ5A1-IDORDNR7-MAX(3:5)            
279800                                                                          
279900        PERFORM IMS-GU-WDQ5A1                                             
280000        IF SEGMENT-FINNS                                                  
280100           INITIALIZE DNOT-ORDER-INFO                                     
280200           MOVE PROGRAM-NAMN             TO DNOT-IDPGM                    
280300           MOVE SEQA-IDORDER             TO DNOT-IDORDER                  
280400           MOVE SEQA-IDDC                TO DNOT-IDDC                     
280500           MOVE ZERO                     TO DNOT-IDARTNR                  
280600           MOVE VORD-IDDC                TO DNOT-IDDC                     
280700           MOVE IDKOLLI-WS               TO DNOT-IDKOLLI-BORT             
280800           MOVE REQU-IDKOLLI-ALLA        TO DNOT-IDKOLLI                  
280900           MOVE SEQF-IDPURAD             TO DNOT-IDPURAD                  
281000                                                                          
281100           CALL W411DNOT USING DNOT-W411DNOT                              
281200                               DNOT-ORQP-PCB                              
281300                               DNOT-ORQP2-PCB                             
281400                               DNOT-ORQP3-PCB                             
281500                               DNOT-4013-PCB                              
281600                               DNOT-BENA-PCB                              
281700        END-IF                                                            
281800     END-IF                                                               
281900     .                                                                    
282000     EJECT                                                                
282100                                                                          
282200                                                                          
282300 S33-DATA-TILL-DEL-NOTE-ARTIKEL SECTION.                                  
282400                                                                          
282500     MOVE VORD-IDDISTR          TO TEST-IDDISTR                           
282600     IF DIST07-USA-RETAILER-DNOTE                                         
282700     OR DIST07-CAN-RETAILER                                               
282800        MOVE LOW-VALUE     TO   W-WDQ5A1KY-MIN-X                          
282900        MOVE HIGH-VALUE    TO   W-WDQ5A1KY-MAX-X                          
283000                                                                          
283100        MOVE SEQF-IDDISTR       TO  W-WDQ5A1-IDDISTR-MIN                  
283200                                    W-WDQ5A1-IDDISTR-MAX                  
283300        MOVE SEQF-IDKUNDNR      TO  W-WDQ5A1-IDKUNDNR-MIN                 
283400                                    W-WDQ5A1-IDKUNDNR-MAX                 
283500        MOVE ZERO               TO  W-WDQ5A1-IDORDNR7-MIN                 
283600                                    W-WDQ5A1-IDORDNR7-MAX                 
283700        MOVE SEQF-IDORDNR5      TO  W-WDQ5A1-IDORDNR7-MIN(3:5)            
283800                                    W-WDQ5A1-IDORDNR7-MAX(3:5)            
283900                                                                          
284000        PERFORM IMS-GU-WDQ5A1                                             
284100        IF SEGMENT-FINNS                                                  
284200           INITIALIZE DNOT-ORDER-INFO                                     
284300           MOVE PROGRAM-NAMN             TO DNOT-IDPGM                    
284400           MOVE SEQA-IDORDER             TO DNOT-IDORDER                  
284500           MOVE SEQA-IDDC                TO DNOT-IDDC                     
284600           MOVE REQU-IDARTNR-LINE(MIX)   TO DNOT-IDARTNR                  
284700           MOVE VORD-IDDC                TO DNOT-IDDC                     
284800           MOVE IDKOLLI-WS               TO DNOT-IDKOLLI-BORT             
284900           MOVE REQU-IDKOLLI-LINE(MIX)   TO DNOT-IDKOLLI                  
285000           MOVE TAB-KVLEVART-MID(MIX)    TO DNOT-KVLEVART                 
285100           MOVE SEQF-IDPURAD             TO DNOT-IDPURAD                  
285200*          OBS DNOT-KVLEVART INNEHÅLLER ANTAL FLYTTADE                    
285300                                                                          
285400                                                                          
285500           CALL W411DNOT USING DNOT-W411DNOT                              
285600                               DNOT-ORQP-PCB                              
285700                               DNOT-ORQP2-PCB                             
285800                               DNOT-ORQP3-PCB                             
285900                               DNOT-4013-PCB                              
286000                               DNOT-BENA-PCB                              
286100        END-IF                                                            
286200     END-IF                                                               
286300     .                                                                    
286400     EJECT                                                                
286500                                                                          
286600 S99A-PACKTRANS-VR-ISRT SECTION.                                          
286700                                                                          
286800     IF DIST03-SVERIGE-100-799                                            
286900     OR DIST03-NORGE                                                      
287000     OR DIST03-DANMARK-900                                                
287100     OR DIST85-PU-VIA-VR                                                  
287200     OR DIST21-TYRE                                                       
287300        MOVE IDPRODNR-WS TO XXJK-4322-IDPRODNR                            
287400        MOVE W-IDKOLLI TO XXJK-4322-IDKOLLI                               
287500        MOVE XXJK-4322-WDGX4322 TO 4322-WDGX4322                          
287600        PERFORM IMS-ISRT-4322-SEGM                                        
287700     END-IF                                                               
287800     .                                                                    
287900     EJECT                                                                
288000 S99B-PACKTRANS-VR-DLET SECTION.                                          
288100                                                                          
288200     IF DIST03-SVERIGE-100-799                                            
288300     OR DIST03-NORGE                                                      
288400     OR DIST03-DANMARK-900                                                
288500     OR DIST85-PU-VIA-VR                                                  
288600     OR DIST21-TYRE                                                       
288700       PERFORM IMS-GHU-WLXXJK01                                           
288800       PERFORM IMS-GHNP-WLXXJK11                                          
288900       PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                         
289000         MOVE 4322-IDPRODNR TO WS-IDPRODNR                                
289100         MOVE 4322-IDKOLLI  TO WS-IDKOLLI                                 
289200         IF WS-IDPRODNR = VORD-IDPRODNR                                   
289300         AND WS-IDKOLLI = KOLLI-IDKOLLI                                   
289400           PERFORM IMS-DLET-WLXXJK11                                      
289500         END-IF                                                           
289600         PERFORM IMS-GHNP-WLXXJK11                                        
289700       END-PERFORM                                                        
289800     END-IF                                                               
289900     .                                                                    
290000     EJECT                                                                
290100                                                                          
290200* MFS SEKTIONER                                                           
290300     SKIP3                                                                
290400                                                                          
290500 MFS-TOM-RAD  SECTION.                                                    
290600     SKIP2                                                                
290700     MOVE JA      TO RESP-FLNOLLAD-LINE (MIX)                             
290800     MOVE SPACE   TO RESP-KDSVAR-LINE (MIX)                               
290900     MOVE ALL-SPACE          TO                                           
291000          RESP-IDPURAD-LINE (MIX)                                         
291100          RESP-IDARTNR-LINE (MIX)                                         
291200          RESP-KVLEVART-LINE (MIX)                                        
291300          RESP-IDKOLLI-LINE (MIX)                                         
291400          RESP-KVLEVART-LINE-IN (MIX)                                     
291500     MOVE MFS-STAENG-FAELT   TO                                           
291600          RESP-IDKOLLI-LINE-ATTR (MIX)                                    
291700          RESP-KVLEVART-LINE-IN-ATTR (MIX)                                
291800     SKIP3                                                                
291900                                                                          
292000     .                                                                    
292100 MFS-ROER-EJ-RAD  SECTION.                                                
292200     SKIP2                                                                
292300     MOVE REQU-FLNOLLAD-LINE (MIX) TO RESP-FLNOLLAD-LINE (MIX)            
292400     MOVE ALL-PLUS             TO                                         
292500          RESP-KDSVAR-LINE (MIX)                                          
292600          RESP-IDPURAD-LINE (MIX)                                         
292700          RESP-IDARTNR-LINE (MIX)                                         
292800          RESP-KVLEVART-LINE (MIX)                                        
292900          RESP-IDKOLLI-LINE (MIX)                                         
293000          RESP-KVLEVART-LINE-IN (MIX)                                     
293100     IF RESP-FLNOLLAD-LINE (MIX) = JA                                     
293200       MOVE MFS-STAENG-FAELT   TO                                         
293300            RESP-IDKOLLI-LINE-ATTR (MIX)                                  
293400            RESP-KVLEVART-LINE-IN-ATTR (MIX)                              
293500     ELSE                                                                 
293600       MOVE MFS-FORMATETS-ATTR TO                                         
293700            RESP-IDKOLLI-LINE-ATTR (MIX)                                  
293800            RESP-KVLEVART-LINE-IN-ATTR (MIX)                              
293900     END-IF                                                               
294000                                                                          
294100     .                                                                    
294200     EJECT                                                                
294300 MFS-TILL-KOLLI-SF-ALLA  SECTION.                                         
294400     SKIP2                                                                
294500     MOVE ALL-PLUS             TO                                         
294600          RESP-FLJANEJ-ALLA                                               
294700          RESP-IDKOLLI-ALLA                                               
294800     MOVE 1   TO MIX                                                      
294900     PERFORM UNTIL MIX > MAX-KVRADER                                      
295000       PERFORM MFS-TILL-KOLLI-SF                                          
295100       ADD 1    TO MIX                                                    
295200     END-PERFORM                                                          
295300     SKIP3                                                                
295400                                                                          
295500     .                                                                    
295600 MFS-TILL-KOLLI-SF  SECTION.                                              
295700     SKIP2                                                                
295800     MOVE REQU-FLNOLLAD-LINE (MIX) TO                                     
295900          RESP-FLNOLLAD-LINE (MIX)                                        
296000                                                                          
296100     MOVE ALL-PLUS             TO                                         
296200          RESP-IDPURAD-LINE (MIX)                                         
296300          RESP-IDARTNR-LINE (MIX)                                         
296400          RESP-KVLEVART-LINE (MIX)                                        
296500          RESP-IDKOLLI-LINE (MIX)                                         
296600          RESP-KVLEVART-LINE-IN (MIX)                                     
296700     IF REQU-IDARTNR-LINE (MIX) > ZERO AND                                
296800        REQU-FLNOLLAD-LINE (MIX) = NEJ                                    
296900         MOVE MFS-NUM-FAELT-RAETT TO                                      
297000              RESP-IDKOLLI-LINE-ATTR (MIX)                                
297100              RESP-KVLEVART-LINE-IN-ATTR (MIX)                            
297200     ELSE                                                                 
297300         MOVE MFS-STAENG-FAELT TO                                         
297400              RESP-IDKOLLI-LINE-ATTR (MIX)                                
297500              RESP-KVLEVART-LINE-IN-ATTR (MIX)                            
297600     END-IF                                                               
297700                                                                          
297800     .                                                                    
297900     EJECT                                                                
298000 MFS-HEL-RAD-FLYTTAD  SECTION.                                            
298100     SKIP2                                                                
298200     MOVE JA      TO RESP-FLNOLLAD-LINE (MIX)                             
298300     MOVE SPACE   TO RESP-KDSVAR-LINE (MIX)                               
298400     MOVE ALL-PLUS             TO                                         
298500          RESP-IDPURAD-LINE (MIX)                                         
298600          RESP-IDARTNR-LINE (MIX)                                         
298700     MOVE ALL-SPACE          TO                                           
298800          RESP-IDKOLLI-LINE (MIX)                                         
298900          RESP-KVLEVART-LINE-IN (MIX)                                     
299000     MOVE MFS-STAENG-FAELT   TO                                           
299100          RESP-IDKOLLI-LINE-ATTR (MIX)                                    
299200          RESP-KVLEVART-LINE-IN-ATTR (MIX)                                
299300     SKIP3                                                                
299400                                                                          
299500     .                                                                    
299600 MFS-DEL-AV-RAD-FLYTTAD  SECTION.                                         
299700     SKIP2                                                                
299800     MOVE NEJ     TO RESP-FLNOLLAD-LINE (MIX)                             
299900     MOVE SPACE   TO RESP-KDSVAR-LINE (MIX)                               
300000     MOVE ALL-PLUS             TO                                         
300100          RESP-IDPURAD-LINE (MIX)                                         
300200          RESP-IDARTNR-LINE (MIX)                                         
300300     MOVE ALL-SPACE          TO                                           
300400          RESP-IDKOLLI-LINE (MIX)                                         
300500          RESP-KVLEVART-LINE-IN (MIX)                                     
300600     MOVE MFS-FORMATETS-ATTR TO                                           
300700          RESP-IDKOLLI-LINE-ATTR (MIX)                                    
300800          RESP-KVLEVART-LINE-IN-ATTR (MIX)                                
300900                                                                          
301000     .                                                                    
301100     EJECT                                                                
301200 MFS-OEPPNA-NYTT-KOLLI  SECTION.                                          
301300     SKIP2                                                                
301400     MOVE MFS-OEPPNA-NUM-FAELT  TO                                        
301500          RESP-IDKOLLI-NY-ATTR                                            
301600          RESP-KDKOLLI-ATTR                                               
301700          RESP-KDEMBTYP-ATTR                                              
301800          RESP-VKORDBTO-ATTR                                              
301900          RESP-DIKOLLIL-ATTR                                              
302000          RESP-DIKOLLIB-ATTR                                              
302100          RESP-DIKOLLIH-ATTR                                              
302200                                                                          
302300     PERFORM MFS-RENSA-NYTT-KOLLI                                         
302400     SKIP3                                                                
302500     .                                                                    
302600 MFS-STAENG-NYTT-KOLLI  SECTION.                                          
302700                                                                          
302800     MOVE MFS-FORMATETS-ATTR    TO                                        
302900          RESP-IDKOLLI-NY-ATTR                                            
303000          RESP-KDKOLLI-ATTR                                               
303100          RESP-KDEMBTYP-ATTR                                              
303200          RESP-VKORDBTO-ATTR                                              
303300          RESP-DIKOLLIL-ATTR                                              
303400          RESP-DIKOLLIB-ATTR                                              
303500          RESP-DIKOLLIH-ATTR                                              
303600     PERFORM MFS-RENSA-NYTT-KOLLI                                         
303700                                                                          
303800     .                                                                    
303900     EJECT                                                                
304000 MFS-RENSA-NYTT-KOLLI  SECTION.                                           
304100     SKIP2                                                                
304200     MOVE ALL-SPACE             TO                                        
304300          RESP-IDKOLLI-NY                                                 
304400          RESP-KDKOLLI                                                    
304500          RESP-KDEMBTYP                                                   
304600          RESP-VKORDBTO                                                   
304700          RESP-DIKOLLIL                                                   
304800          RESP-DIKOLLIB                                                   
304900          RESP-DIKOLLIH                                                   
305000     SKIP3                                                                
305100                                                                          
305200     .                                                                    
305300 MFS-ROER-EJ-HUVUD  SECTION.                                              
305400     SKIP2                                                                
305500     MOVE ALL-PLUS              TO                                        
305600          RESP-IDRADNR-FOM                                                
305700          RESP-IDRADNR-TOM                                                
305800          RESP-ORDER-INFO                                                 
305900          RESP-TIPACKN                                                    
306000                                                                          
306100     .                                                                    
306200     EJECT                                                                
306300 MFS-EFTER-FLYTTA-ALLA SECTION.                                           
306400     SKIP2                                                                
306500     MOVE MFS-STAENG-FAELT  TO                                            
306600          RESP-FLJANEJ-ALLA-ATTR                                          
306700          RESP-IDKOLLI-ALLA-ATTR                                          
306800     MOVE ALL-SPACE         TO                                            
306900          RESP-FLJANEJ-ALLA                                               
307000          RESP-KDSVAR-ALLA                                                
307100          RESP-IDKOLLI-ALLA                                               
307200     MOVE 1    TO MIX                                                     
307300     PERFORM UNTIL MIX > MAX-KVRADER                                      
307400         PERFORM MFS-HEL-RAD-FLYTTAD                                      
307500         ADD 1   TO MIX                                                   
307600     END-PERFORM                                                          
307700     SKIP3                                                                
307800                                                                          
307900     .                                                                    
308000 MFS-STAENG-ALLT SECTION.                                                 
308100     SKIP2                                                                
308200     MOVE MFS-STAENG-FAELT   TO                                           
308300          RESP-FLJANEJ-ALLA-ATTR                                          
308400          RESP-IDKOLLI-ALLA-ATTR                                          
308500     MOVE ALL-SPACE         TO                                            
308600          RESP-FLJANEJ-ALLA                                               
308700          RESP-IDKOLLI-ALLA                                               
308800                                                                          
308900     MOVE 1    TO MIX                                                     
309000     PERFORM UNTIL MIX > MAX-KVRADER                                      
309100         MOVE MFS-STAENG-FAELT TO                                         
309200              RESP-IDKOLLI-LINE-ATTR (MIX)                                
309300              RESP-KVLEVART-LINE-IN-ATTR (MIX)                            
309400         ADD 1   TO MIX                                                   
309500     END-PERFORM                                                          
309600                                                                          
309700     PERFORM MFS-STAENG-NYTT-KOLLI                                        
309800     SKIP3                                                                
309900                                                                          
310000     .                                                                    
310100 MFS-SAMMA-BILD  SECTION.                                                 
310200     SKIP2                                                                
310300      MOVE ALL-PLUS              TO RESP-IDRADNR-FOM                      
310400                                    RESP-IDRADNR-TOM                      
310500                                    RESP-ORDER-INFO                       
310600                                    RESP-KVORDRAD                         
310700                                    RESP-TIPACKN                          
310800                                    RESP-FLJANEJ-ALLA                     
310900                                    RESP-KDSVAR-ALLA                      
311000                                    RESP-IDKOLLI-ALLA                     
311100                                    RESP-IDKOLLI-NY                       
311200                                    RESP-KDKOLLI                          
311300                                    RESP-KDEMBTYP                         
311400                                    RESP-VKORDBTO                         
311500                                    RESP-DIKOLLIL                         
311600                                    RESP-DIKOLLIB                         
311700                                    RESP-DIKOLLIH                         
311800                                                                          
311900     MOVE 1   TO MIX                                                      
312000     PERFORM UNTIL MIX > MAX-KVRADER                                      
312100          MOVE ALL-PLUS TO                                                
312200                                    RESP-FLNOLLAD-LINE (MIX)              
312300                                    RESP-IDPURAD-LINE (MIX)               
312400                                    RESP-IDARTNR-LINE (MIX)               
312500                                    RESP-KVLEVART-LINE (MIX)              
312600                                    RESP-KDSVAR-LINE (MIX)                
312700                                    RESP-IDKOLLI-LINE (MIX)               
312800                                    RESP-KVLEVART-LINE-IN (MIX)           
312900         ADD 1  TO MIX                                                    
313000     END-PERFORM                                                          
313100                                                                          
313200     MOVE ALL-PLUS TO                                                     
313300                                    RESP-IDKOLLI-NY                       
313400                                    RESP-KDEMBTYP                         
313500                                    RESP-KDKOLLI                          
313600                                    RESP-VKORDBTO                         
313700                                    RESP-DIKOLLIL                         
313800                                    RESP-DIKOLLIB                         
313900                                    RESP-DIKOLLIH                         
314000     .                                                                    
314100     EJECT                                                                
314200* IMS SEKTIONER                                                           
314300     SKIP3                                                                
314400                                                                          
314500 IMS-GU-WDE601      SECTION.                                              
314600     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
314700            DELIMITED BY SIZE INTO SSA1                                   
314800     MOVE '  GE' TO GODK-STATUSKODER                                      
314900     CALL CBLTDLI USING GU   WDE6-PCB DLI-IO-E601 SSA1                    
315000     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
315100     PERFORM IMS-STATUSKONTROLL                                           
315200     .                                                                    
315300 IMS-GNP-WDE611 SECTION.                                                  
315400     STRING 'WDE611  *F(IDKOLLI  =' W-IDKOLLI-X ')'                       
315500            DELIMITED BY SIZE INTO SSA1                                   
315600     MOVE '  GE' TO GODK-STATUSKODER                                      
315700     CALL CBLTDLI USING GNP   WDE6-PCB DLI-IO-E611 SSA1                   
315800     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
315900     PERFORM IMS-STATUSKONTROLL                                           
316000     .                                                                    
316100 IMS-GHU-WDE601  SECTION.                                                 
316200     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
316300            DELIMITED BY SIZE INTO SSA1                                   
316400     MOVE '  ' TO GODK-STATUSKODER                                        
316500     CALL CBLTDLI USING GHU   WDE6-PCB DLI-IO-E601 SSA1                   
316600     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
316700     PERFORM IMS-STATUSKONTROLL                                           
316800     .                                                                    
316900 IMS-REPL-WDE601 SECTION.                                                 
317000     MOVE '  ' TO GODK-STATUSKODER                                        
317100     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-E601                         
317200     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
317300     PERFORM IMS-STATUSKONTROLL                                           
317400     .                                                                    
317500     EJECT                                                                
317600 IMS-GHU-WDE611 SECTION.                                                  
317700     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
317800            DELIMITED BY SIZE INTO SSA1                                   
317900     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
318000            DELIMITED BY SIZE INTO SSA2                                   
318100     MOVE '  ' TO GODK-STATUSKODER                                        
318200     CALL CBLTDLI USING GHU   WDE6-PCB DLI-IO-E611 SSA1 SSA2              
318300     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
318400     PERFORM IMS-STATUSKONTROLL                                           
318500     .                                                                    
318600 IMS-GHU-WDE611-GE SECTION.                                               
318700     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
318800            DELIMITED BY SIZE INTO SSA1                                   
318900     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
319000            DELIMITED BY SIZE INTO SSA2                                   
319100     MOVE '  GE' TO GODK-STATUSKODER                                      
319200     CALL CBLTDLI USING GHU   WDE6-PCB DLI-IO-E611 SSA1 SSA2              
319300     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
319400     PERFORM IMS-STATUSKONTROLL                                           
319500     .                                                                    
319600 IMS-ISRT-WDE611 SECTION.                                                 
319700     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
319800            DELIMITED BY SIZE INTO SSA1                                   
319900     MOVE 'WDE611'    TO SSA2                                             
320000     MOVE '  ' TO GODK-STATUSKODER                                        
320100     CALL CBLTDLI USING ISRT  WDE6-PCB DLI-IO-E611 SSA1 SSA2              
320200     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
320300     PERFORM IMS-STATUSKONTROLL                                           
320400     .                                                                    
320501 IMS-ISRT-WDE621 SECTION.                                                 
320601                                                                          
320701     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
320801          DELIMITED BY SIZE INTO SSA1                                     
320901     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
321001          DELIMITED BY SIZE INTO SSA2                                     
321101     MOVE 'WDE621 ' TO SSA3                                               
321201     MOVE '  II' TO GODK-STATUSKODER                                      
321301     CALL CBLTDLI USING ISRT WDE6-PCB DLI-IO-WDE621 SSA1 SSA2 SSA3        
321401     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
321501     PERFORM IMS-STATUSKONTROLL                                           
321601     .                                                                    
321701     EJECT                                                                
321800 IMS-DLET-WDE611 SECTION.                                                 
321900     MOVE '  ' TO GODK-STATUSKODER                                        
322000     CALL CBLTDLI USING DLET WDE6-PCB DLI-IO-E611                         
322100     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
322200     PERFORM IMS-STATUSKONTROLL                                           
322300     .                                                                    
322400 IMS-REPL-WDE611 SECTION.                                                 
322500     MOVE '  ' TO GODK-STATUSKODER                                        
322600     CALL CBLTDLI USING REPL WDE6-PCB DLI-IO-E611                         
322700     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
322800     PERFORM IMS-STATUSKONTROLL                                           
322900     .                                                                    
323000     EJECT                                                                
323100 IMS-GU-WDE411-21-NEXT SECTION.                                           
323200     MOVE 'IMS-GU-WDE411-21-NEXT'     TO  CURRENT-IMS-SECTION             
323300                                                                          
323400     STRING 'WDE411  *D(WDE4FSEQ =' W-WDE421KY-X                          
323500                      '&IDPURAD  >' W-IDPURAD-X ')'                       
323600            DELIMITED BY SIZE INTO SSA1                                   
323700     STRING 'WDE421  (WDE421KY =' W-WDE421KY-X ')'                        
323800            DELIMITED BY SIZE INTO SSA2                                   
323900     MOVE '  GE' TO GODK-STATUSKODER                                      
324000     CALL CBLTDLI USING GU WDE4FSEQ-PCB DLI-IO-E411-21 SSA1 SSA2          
324100     MOVE WDE4FSEQ-STATUS-CODE TO STATUS-WS                               
324200     PERFORM IMS-STATUSKONTROLL                                           
324300     SKIP3                                                                
324400     .                                                                    
324500 IMS-GU-WDE411-21-PU SECTION.                                             
324600     MOVE 'IMS-GU-WDE411-21-PU  '     TO  CURRENT-IMS-SECTION             
324700                                                                          
324800     STRING 'WDE411  *D(WDE4FSEQ =' W-WDE421KY-X                          
324900                    '&IDPURAD  =' W-IDPURAD-X ')'                         
325000            DELIMITED BY SIZE INTO SSA1                                   
325100     STRING 'WDE421  (WDE421KY =' W-WDE421KY-X ')'                        
325200            DELIMITED BY SIZE INTO SSA2                                   
325300     MOVE '  GE' TO GODK-STATUSKODER                                      
325400     CALL CBLTDLI USING GU WDE4FSEQ-PCB DLI-IO-E411-21 SSA1 SSA2          
325500     MOVE WDE4FSEQ-STATUS-CODE TO STATUS-WS                               
325600     PERFORM IMS-STATUSKONTROLL                                           
325700     SKIP3                                                                
325800     .                                                                    
325900 IMS-GU-WDE411-21 SECTION.                                                
326000     MOVE 'IMS-GU-WDE411-21     '     TO  CURRENT-IMS-SECTION             
326100                                                                          
326200     STRING 'WDE411  *D(WDE4FSEQ =' W-WDE421KY-X ')'                      
326300            DELIMITED BY SIZE INTO SSA1                                   
326400     STRING 'WDE421  (WDE421KY =' W-WDE421KY-X ')'                        
326500            DELIMITED BY SIZE INTO SSA2                                   
326600     MOVE '  GE' TO GODK-STATUSKODER                                      
326700     CALL CBLTDLI USING GU WDE4FSEQ-PCB DLI-IO-E411-21 SSA1 SSA2          
326800     MOVE WDE4FSEQ-STATUS-CODE TO STATUS-WS                               
326900     PERFORM IMS-STATUSKONTROLL                                           
327000     SKIP3                                                                
327100     .                                                                    
327200 IMS-GN-WDE411-21 SECTION.                                                
327300     MOVE 'IMS-GN-WDE411-21     '     TO  CURRENT-IMS-SECTION             
327400                                                                          
327500     STRING 'WDE411  *D(WDE4FSEQ =' W-WDE421KY-X ')'                      
327600            DELIMITED BY SIZE INTO SSA1                                   
327700     STRING 'WDE421  (WDE421KY =' W-WDE421KY-X ')'                        
327800            DELIMITED BY SIZE INTO SSA2                                   
327900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
328000     CALL CBLTDLI USING GN WDE4FSEQ-PCB DLI-IO-E411-21 SSA1 SSA2          
328100     MOVE WDE4FSEQ-STATUS-CODE TO STATUS-WS                               
328200     PERFORM IMS-STATUSKONTROLL                                           
328300     SKIP3                                                                
328400     .                                                                    
328500     EJECT                                                                
328600 IMS-GU-WDE4F1-PU SECTION.                                                
328700     STRING 'WDE4F1  (WDE4F1KY>=' W-WDE4F1KY-MIN-X                        
328800                    '&WDE4F1KY<=' W-WDE4F1KY-MAX-X                        
328900                    '&IDPURAD >=' W-IDPURAD-X ')'                         
329000            DELIMITED BY SIZE INTO SSA1                                   
329100     MOVE '  GE' TO GODK-STATUSKODER                                      
329200     CALL CBLTDLI USING GU WDE4F-PCB DLI-IO-E4F1 SSA1                     
329300     MOVE WDE4F-STATUS-CODE TO STATUS-WS                                  
329400     PERFORM IMS-STATUSKONTROLL                                           
329500     SKIP3                                                                
329600     .                                                                    
329700 IMS-GU-WDE4F1 SECTION.                                                   
329800     STRING 'WDE4F1  (WDE4F1KY>=' W-WDE4F1KY-MIN-X                        
329900                    '&WDE4F1KY<=' W-WDE4F1KY-MAX-X ')'                    
330000            DELIMITED BY SIZE INTO SSA1                                   
330100     MOVE '  GE' TO GODK-STATUSKODER                                      
330200     CALL CBLTDLI USING GU WDE4F-PCB DLI-IO-E4F1 SSA1                     
330300     MOVE WDE4F-STATUS-CODE TO STATUS-WS                                  
330400     PERFORM IMS-STATUSKONTROLL                                           
330500     SKIP3                                                                
330600     .                                                                    
330700 IMS-GN-WDE4F1 SECTION.                                                   
330800     STRING 'WDE4F1  (WDE4F1KY>=' W-WDE4F1KY-MIN-X                        
330900                    '&WDE4F1KY<=' W-WDE4F1KY-MAX-X ')'                    
331000            DELIMITED BY SIZE INTO SSA1                                   
331100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
331200     CALL CBLTDLI USING GN WDE4F-PCB DLI-IO-E4F1 SSA1                     
331300     MOVE WDE4F-STATUS-CODE TO STATUS-WS                                  
331400     PERFORM IMS-STATUSKONTROLL                                           
331500     .                                                                    
331600     EJECT                                                                
331700 IMS-GU-WDE411 SECTION.                                                   
331800     STRING 'WDE401  (WDE401KY =' W-WDE401KY-X ')'                        
331900            DELIMITED BY SIZE INTO SSA1                                   
332000     STRING 'WDE411  (IDPURAD  =' W-IDPURAD-X ')'                         
332100            DELIMITED BY SIZE INTO SSA2                                   
332200     MOVE '  ' TO GODK-STATUSKODER                                        
332300     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-E411 SSA1 SSA2                 
332400     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
332500     PERFORM IMS-STATUSKONTROLL                                           
332600     SKIP3                                                                
332700     .                                                                    
332800 IMS-GHNP-WDE421 SECTION.                                                 
332900     STRING 'WDE421  *F(WDE421KY =' W-WDE421KY-X ')'                      
333000            DELIMITED BY SIZE INTO SSA1                                   
333100     MOVE '  ' TO GODK-STATUSKODER                                        
333200     CALL CBLTDLI USING GHNP WDE4-PCB DLI-IO-E421 SSA1                    
333300     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
333400     PERFORM IMS-STATUSKONTROLL                                           
333500     SKIP3                                                                
333600     .                                                                    
333700 IMS-ISRT-WDE421 SECTION.                                                 
333800     STRING 'WDE401  (WDE401KY =' W-WDE401KY-X ')'                        
333900            DELIMITED BY SIZE INTO SSA1                                   
334000     STRING 'WDE411  (IDPURAD  =' W-IDPURAD-X ')'                         
334100            DELIMITED BY SIZE INTO SSA2                                   
334200     MOVE 'WDE421'    TO SSA3                                             
334300     MOVE '  II' TO GODK-STATUSKODER                                      
334400     CALL CBLTDLI USING ISRT WDE4-PCB DLI-IO-E421 SSA1 SSA2 SSA3          
334500     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
334600     PERFORM IMS-STATUSKONTROLL                                           
334700     SKIP3                                                                
334800     .                                                                    
334900 IMS-DLET-WDE421 SECTION.                                                 
335000     MOVE '  ' TO GODK-STATUSKODER                                        
335100     CALL CBLTDLI USING DLET WDE4-PCB DLI-IO-E421                         
335200     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
335300     PERFORM IMS-STATUSKONTROLL                                           
335400     .                                                                    
335500 IMS-REPL-WDE421 SECTION.                                                 
335600     MOVE '  ' TO GODK-STATUSKODER                                        
335700     CALL CBLTDLI USING REPL WDE4-PCB DLI-IO-E421                         
335800     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
335900     PERFORM IMS-STATUSKONTROLL                                           
336000     .                                                                    
336100     EJECT                                                                
336200 IMS-GU-WDQ5A1 SECTION.                                                   
336300                                                                          
336400     STRING 'WDQ5A1  (WDQ5A1KY=>' W-WDQ5A1KY-MIN-X                        
336500                    '&WDQ5A1KY<=' W-WDQ5A1KY-MAX-X ')'                    
336600          DELIMITED BY SIZE INTO SSA1                                     
336700     MOVE '  GE'               TO GODK-STATUSKODER                        
336800     CALL CBLTDLI USING GU WDQ5A-PCB DLI-IO-Q5A1 SSA1                     
336900     MOVE WDQ5A-STATUS-CODE    TO STATUS-WS                               
337000     PERFORM IMS-STATUSKONTROLL                                           
337100     .                                                                    
337200                                                                          
337300 IMS-GU-WDB601    SECTION.                                                
337400     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
337500          DELIMITED BY SIZE INTO SSA1                                     
337600     MOVE '  GE' TO GODK-STATUSKODER                                      
337700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
337800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
337900     PERFORM IMS-STATUSKONTROLL                                           
338000     IF SEGMENT-SAKNAS                                                    
338100        MOVE SPACE TO DCS-KDDC                                            
338200     END-IF                                                               
338300     .                                                                    
338400     SKIP2                                                                
338500 IMS-ISRT-4322-SEGM SECTION.                                              
338600     STRING 'WLXXJK01(WDGXKEY  =' W-4321-IDHTYP-X ')'                     
338700            DELIMITED BY SIZE INTO SSA1                                   
338800     MOVE 'WLXXJK11*L' TO SSA2                                            
338900     MOVE '  ' TO GODK-STATUSKODER                                        
339000     CALL CBLTDLI USING ISRT XXJK-PCB DLI-IO-AREA4 SSA1 SSA2              
339100     MOVE XXJK-STATUS-CODE TO STATUS-WS                                   
339200     PERFORM IMS-STATUSKONTROLL                                           
339300     .                                                                    
339400     SKIP2                                                                
339500                                                                          
339600 IMS-GHU-WLXXJK01 SECTION.                                                
339700     SKIP2                                                                
339800     STRING 'WLXXJK01(WDGXKEY  =' W-4321-IDHTYP-X ')'                     
339900            DELIMITED BY SIZE INTO SSA1                                   
340000     MOVE '  GE' TO GODK-STATUSKODER                                      
340100     CALL CBLTDLI USING GHU XXJK-PCB DLI-IO-AREA4 SSA1                    
340200     MOVE XXJK-STATUS-CODE TO STATUS-WS                                   
340300     PERFORM IMS-STATUSKONTROLL                                           
340400     .                                                                    
340500     SKIP3                                                                
340600 IMS-GHNP-WLXXJK11 SECTION.                                               
340700     SKIP2                                                                
340800     MOVE 'WLXXJK11 ' TO SSA1                                             
340900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
341000     CALL CBLTDLI USING GHNP XXJK-PCB DLI-IO-AREA4 SSA1                   
341100     MOVE XXJK-STATUS-CODE TO STATUS-WS                                   
341200     PERFORM IMS-STATUSKONTROLL                                           
341300     .                                                                    
341400     SKIP3                                                                
341500 IMS-DLET-WLXXJK11 SECTION.                                               
341600     SKIP2                                                                
341700     MOVE 'WLXXJK11 ' TO SSA1                                             
341800     MOVE '  ' TO GODK-STATUSKODER                                        
341900     CALL CBLTDLI USING DLET XXJK-PCB DLI-IO-AREA4 SSA1                   
342000     MOVE XXJK-STATUS-CODE TO STATUS-WS                                   
342100     PERFORM IMS-STATUSKONTROLL                                           
342200     .                                                                    
342300     EJECT                                                                
342400 IMS-GU-WDK501 SECTION.                                                   
342500                                                                          
342600     STRING 'WDK501  (KDKOLLI  =' W-KDKOLLI-K5-X ')'                      
342700          DELIMITED BY SIZE INTO SSA1                                     
342800     MOVE '  GE'             TO GODK-STATUSKODER                          
342900     CALL CBLTDLI USING GU WDK5-PCB DLI-IO-WDK501 SSA1                    
343000     MOVE WDK5-STATUS-CODE   TO STATUS-WS                                 
343100     PERFORM IMS-STATUSKONTROLL                                           
343200     .                                                                    
343300     EJECT                                                                
343400 IMS-STATUSKONTROLL SECTION.                                              
343500     SET STATUS-IX TO 1                                                   
343600     SEARCH GODK-STATUS AT END CALL FELLOG                                
343700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
343800     END-SEARCH                                                           
343900     .                                                                    
344000     EJECT                                                                
