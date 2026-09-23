000001 PROCESS DYNAM                                                            
000002*                                                                         
000003******************************************************************        
000004*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0104      *        
000005******************************************************************        
000006*                                                                         
000007 ID DIVISION.                                                             
000008                                                                          
000009 PROGRAM-ID.     W6030400.                                                
000010 AUTHOR.         LARS THELL / KENT JEBSEN / TOMMIE JIVARP                 
000011 DATE-WRITTEN.   94/12/08. /  DEC -96    /  98/03/18.                     
000012 DATE-COMPILED.                                                           
000013                                                                          
000014*    FUNKTION:                                                            
000015*        LAGERDATA REGISTRERING  SDC/NDC                                  
000016*      (PLATSSÄTTNING, VIKT- & VOLYM ANGIVELSER FÖR USA:S                 
000017*                                               LOKALA ARTIKLAR.)         
000018*                                                                         
000019*                                                                         
000020*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
000021*                                      WDK7                               
000022*                              WLLOCB (WDJ9)                              
000023*                                                                         
000024*     +  STARTAR EV DISPATCH FÖR UPPDATERING AV ADRESS PÅ                 
000025*        INLEVERANSREGISTRET.                                             
000026*                                                                         
000027*                                                                         
000028*    INDATA.                                                              
000029*        TRANSAKTION: W6T304                                              
000030*        MID:         W6I30401                                            
000031*                                                                         
000032*    UTDATA.                                                              
000033*        MOD:         W6O30401                                            
000034*    SUBPROGRAM:                                                          
000035*        W006KOM  (DISPATCH)                                              
000036*                                                                         
000037*    E-TRACKER: 7450328  2008-HÖST  VOHF                                  
000038*    E-TRACKER:10254592  2015       DECOMISSION VOHF                      
000039*    E-TRACKER:10251642  2016-01-05 UPDATE ADDRESS ON BYART FOR           
000040*                                   CORE PARTS.                           
000041     SKIP3                                                                
000042 ENVIRONMENT DIVISION.                                                    
000043 DATA DIVISION.                                                           
000044     EJECT                                                                
000045 WORKING-STORAGE SECTION.                                                 
000046                                                                          
000047*    -- CHECKED BY WY2000                                                 
000048 77  IDPGM                       PIC X(08)   VALUE 'W6030400'.            
000049                                                                          
000050*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
000051 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
000052                                                                          
000053 77  JA                          PIC X       VALUE 'J'.                   
000054 77  NEJ                         PIC X       VALUE 'N'.                   
000055 77  TAB-IX                      PIC S9(3)   VALUE ZERO COMP-3.           
000056 77  TAB-IX-MAX                  PIC S9(3)   VALUE +5   COMP-3.           
000057 01  WS-IDARTNR                  PIC X(9)    VALUE ZERO.                  
000058 01  FILLER REDEFINES WS-IDARTNR.                                         
000059   03  FILLER                    PIC 9(5).                                
000060   03  WS-ARTSIFFRA              PIC 9(1).                                
000061     88  ART-0                   VALUE 6.                                 
000062     88  ART-1                   VALUE 4  7.                              
000063     88  ART-2                   VALUE 5  8.                              
000064     88  ART-3                   VALUE 9.                                 
000065   03  FILLER                    PIC 9(3).                                
000066 77  W-IDARTNR-BYT               PIC S9(9) COMP-3 VALUE ZERO.             
000067 77  WS-VKART                    PIC 9(7)        VALUE ZERO.              
000068 77  WS-VKART-DEC                PIC 9(7)V9(2)   VALUE ZERO.              
000069 77  WS-VLARTNTO                 PIC S9(8)V9(1)  VALUE ZERO.              
000070 77  LOGG-DATUM                  PIC S9(8)       VALUE ZERO.              
000071 77  LOGG-TID                    PIC S9(7)       VALUE ZERO.              
000072 77  PRIME-LOCATION              PIC X           VALUE 'P'.               
000073                                                                          
000074 01  SPARA-VLARTNTO             PIC S9(8)V9(1)   COMP-3.                  
000075 01  SPARA-VKART                PIC S9(7)        COMP-3.                  
000076 01  WS-VLARTNTO-DISP-NUM        PIC 9(8)V9(1).                           
000077 01  WS-VLARTNTO-DISP-XX  REDEFINES WS-VLARTNTO-DISP-NUM.                 
000078     05 WS-VLARTNTO-DISP-ALFA    PIC X(9).                                
000079                                                                          
000080 01  ADLAGOMR-WS                 PIC 9(3).                                
000081 01  FILLER REDEFINES ADLAGOMR-WS.                                        
000082     03  FILLER                  PIC 9(1).                                
000083     03  WS-ADLAGOMR             PIC 9(2).                                
000084                                                                          
000085 01  ADGANG-WS                   PIC 9(3).                                
000086 01  FILLER REDEFINES ADGANG-WS.                                          
000087     03  FILLER                  PIC 9(1).                                
000088     03  WS-ADGANG               PIC 9(2).                                
000089                                                                          
000090 77  WS-IDDC-SPAR                PIC X(2)          VALUE SPACES.          
000091 77  WS-UPD-VOLUME               PIC X       VALUE 'N'.                   
000092 77  WS-UPD-LOCATION             PIC X       VALUE 'N'.                   
000093 77  WS-ADPLATS                  PIC 9(5).                                
000094 77  DAGENS-DATUM                PIC 9(6)          VALUE ZERO.            
000095 77  DAGENS-TID                  PIC 9(8)          VALUE ZERO.            
000096 77  LNG-P-TO-P-PREFIX           PIC S9(4)   VALUE +17  COMP SYNC.        
000097 77  WS-ADLAGOMR-WDJ9            PIC 9(2)          VALUE ZERO.            
000098 77  WS-ADGANG-WDJ9              PIC 9(2)          VALUE ZERO.            
000099 77  WS-ADPLATS-WDJ9             PIC 9(5)          VALUE ZERO.            
000100 77  WS-VKART-UPD                PIC X             VALUE 'N'.             
000101 77  WS-VLARTNTO-UPD             PIC X             VALUE 'N'.             
000102 01  WS-KDPRODSL-X               PIC 9(2).                                
000103 01  WS-KDPRODSL REDEFINES WS-KDPRODSL-X.                                 
000104     03 WS-KDPRODSL-POS1                PIC 9.                            
000105     03 FILLER                          PIC X.                            
000106                                                                          
000107*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
000108                                                                          
000109 77  INDATA-SW                   PIC X       VALUE 'J'.                   
000110     88  INDATA-OK                           VALUE 'J'.                   
000111     88  INDATA-FEL                          VALUE 'N'.                   
000112                                                                          
000113 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
000114     88  NYCKLAR-OK                          VALUE 'J'.                   
000115     88  NYCKLAR-FEL                         VALUE 'N'.                   
000116                                                                          
000117 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
000118     88  EGEN-MID                            VALUE '6304'.                
000119     88  GODK-MID                            VALUE '6301' '6302'          
000120                                            '6304' '6305' '6306'          
000121                                            '6307' '6308' '6309'.         
000122     88  HELP-MID                            VALUE '0551'.                
000123                                                                          
000124 01  FILLER                  PIC X(16)  VALUE 'SWITCHAR        '.         
000125 01  SWITCHAR.                                                            
000126     03  CORE-SW             PIC X     VALUE 'N'.                         
000127       88  CORE-OK                     VALUE 'J'.                         
000128                                                                          
000129     03  TESTA-SW             PIC X     VALUE 'N'.                        
000130       88  TESTA-OK                     VALUE 'J'.                        
000131                                                                          
000132     03  WDK712-FINNS-SW      PIC X     VALUE 'N'.                        
000133       88  WDK712-FINNS                 VALUE 'J'.                        
000134     EJECT                                                                
000135*    --- VALID DC CODES                                                   
000136*01 -COPY WWDC99                                                          
000137     EJECT                                                                
000138*01 -COPY WWDC99               -PRE REF-                                  
000139     EJECT                                                                
000140*    --- FÖR TEST AV BYTESNR                                              
000141*                                                                         
000142 01  FILLER                      PIC  X(16)  VALUE 'BYTES-TEST'.          
000143 01  TEST-IDARTNR                PIC  9(9)   COMP-3.                      
000144*01  FILLER  -COPY WWBYT01     -RED TEST-IDARTNR.                         
000145                                                                          
000146*01  FILLER  -COPY WWBYT02     -RED TEST-IDARTNR.                         
000147                                                                          
000148*01  FILLER  -COPY WWBYT03     -RED TEST-IDARTNR.                         
000149                                                                          
000150*01  FILLER  -COPY WWBYT16     -RED TEST-IDARTNR.                         
000151                                                                          
000152     EJECT                                                                
000153*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
000154 01  GENERELLA-SUBPROGRAM.                                                
000155     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
000156     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
000157     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000158     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000159     03  WWOMVAND                PIC X(8)    VALUE 'WWOMVAND'.            
000160     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
000161     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
000162     03  W488PRMA                PIC X(8)    VALUE 'W488PRMA'.            
000163     EJECT                                                                
000164*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
000165*01 -COPY WMSGINIT                                                        
000166     EJECT                                                                
000167*   -COPY WWLNDKON                                                        
000168     EJECT                                                                
000169*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
000170*01 -COPY WMEDAREA                                                        
000171     EJECT                                                                
000172 01  FILLER                      PIC X(16)  VALUE 'WWOMVAND '.            
000173*   -COPY WWOMVAND                                                        
000174*                                                                         
000175     SKIP3                                                                
000176 01  MESSAGE-CODES.                                                       
000177     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
000178     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
000179     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
000180     03  ARTIKEL-SAKNAS          PIC X(3)    VALUE '017'.                 
000181     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
000182     03  GOODS-ADDRESS-MISSING   PIC X(3)    VALUE '166'.                 
000183     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
000184     03  ZERO-NOT-ALLOWED        PIC X(3)    VALUE '724'.                 
000185     03  UPD-NOT-ALLOWED         PIC X(3)    VALUE '777'.                 
000186 01  MESSAGE-TEXTS.                                                       
000187     03  INF-SEE-SCREEN-6306     PIC X(30) VALUE                          
000188         'FOR MORE INFO. SEE SCREEN 6306'.                                
000189     EJECT                                                                
000190                                                                          
000191*01  -COPY WDECAREA                                                       
000192     EJECT                                                                
000193 01  FILLER                      PIC X(16)   VALUE 'W488PRMA'.            
000194     SKIP3                                                                
000195*01  -COPY W488PRMA                                                       
000196     EJECT                                                                
000197*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
000198*                                                                         
000199 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
000200     SKIP3                                                                
000201*01  MID -COPY W6I30401                                                   
000202     EJECT                                                                
000203 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
000204     SKIP3                                                                
000205*01  -COPY WMSGAREA                                                       
000206     EJECT                                                                
000207     03  MOD REDEFINES MSG-AREA.                                          
000208*      05  -COPY W6O30401                                                 
000209     EJECT                                                                
000210 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
000211*01  -COPY WMFSAREA                                                       
000212     EJECT                                                                
000213                                                                          
000214 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
000215     SKIP3                                                                
000216 01  FILLER                  PIC X(16)  VALUE 'KOM-MSG-IO-AREA '.         
000217 01  KOM-MSG-IO-AREA.                                                     
000218*03  -COPY WMSGKOM                                                        
000219     EJECT                                                                
000220 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-SW'.           
000221 01      P-TO-P-SW.                                                       
000222                                                                          
000223  02     P-TO-P-KVLL             PIC S9(4)           COMP SYNC.           
000224  02     P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.               
000225  02     P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.               
000226  02     P-TO-P-KDTRANS          PIC X(8).                                
000227  02     P-TO-P-IDTRANS          PIC X(4).                                
000228  02     P-TO-P-KDMFSFOR         PIC X(1).                                
000229  02     P-TO-P-DATA             PIC X(1000).                             
000230     EJECT                                                                
000231 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-SW2'.          
000232 01  P-TO-P-SW2.                                                          
000233     02     P-TO-P2-KVLL             PIC S9(4) COMP SYNC.                 
000234     02     P-TO-P2-KDZ1             PIC X(1)  VALUE LOW-VALUE.           
000235     02     P-TO-P2-KDZ2             PIC X(1)  VALUE LOW-VALUE.           
000236     02     P-TO-P2-KDTRANS          PIC X(8).                            
000237     02     P-TO-P2-IDTRANS          PIC X(4).                            
000238     02     P-TO-P2-KDMFSFOR         PIC X(1).                            
000239     02     MID -COPY W4I28901   -PRE P-TO-P2-                            
000240*****************************************************************         
000241     EJECT                                                                
000242                                                                          
000243 01      FILLER                  PIC X(32)   VALUE SPACE.                 
000244*****************************************************************         
000245 01      FILLER                  PIC X(24)   VALUE                        
000246                                 'MOD619A-MID-W6I19A01'.                  
000247     -COPY W6I19A01 -PRE MOD619A-                                         
000248     EJECT                                                                
000249 01      FILLER                  PIC X(24)   VALUE                        
000250                                 'MOD619B-MID-W6I19B01'.                  
000251     SKIP2                                                                
000252     -COPY W6I19B01 -PRE MOD619B-                                         
000253     EJECT                                                                
000254******************************************************************        
000255*    --- ARBETS-AREOR TILL DB2 OCH IMS-SEKTIONERNA                        
000256*                                                                         
000257 01  FILLER                      PIC X(16)   VALUE 'DB2-WS     '.         
000258*01  -COPY BYART -PRE BYART-                                              
000259     EJECT                                                                
000260 01  FILLER                      PIC X(16)   VALUE 'BYART-AREA'.          
000261       EXEC SQL INCLUDE BYART END-EXEC.                                   
000262     SKIP3                                                                
000263 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
000264       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
000265*                        **** STATUS-KOD FRÅN DB2                         
000266 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
000267 01  DB2-WS.                                                              
000268     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
000269         88  CURSOR-OK                       VALUE 000.                   
000270         88  RADER-FINNS                     VALUE 000.                   
000271         88  RADER-SAKNAS                    VALUE 100.                   
000272         88  904-KOD                         VALUE 904.                   
000273     SKIP1                                                                
000274   03  GODK-SQLCODESKODER.                                                
000275       05  GODK-SQLCODE OCCURS 5                                          
000276           INDEXED BY SQLCODE-IX PIC 999.                                 
000277     EJECT                                                                
000278 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000279     SKIP3                                                                
000280 01  NYCKLAR-TILL-DLI.                                                    
000281     03  W-IDARTNR-X.                                                     
000282         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
000283     03  W-IDARTNR2-X.                                                    
000284         05  W-IDARTNR2          PIC S9(9)   VALUE ZERO COMP-3.           
000285     03  W-IDDC-X.                                                        
000286         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
000287     03  W-IDDC-B6-X.                                                     
000288         05  W-IDDC-B6       PIC X(2)   VALUE SPACE.                      
000289     03  W-IDLAND-X.                                                      
000290         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
000291     03  W-KDSEGKEY-X.                                                    
000292         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
000293     03  W-WDJ911KY-X.                                                    
000294         05  W-IDDC-WDJ9         PIC X(2)    VALUE ZERO.                  
000295         05  W-DASTADAT          PIC S9(9)   VALUE ZERO.                  
000296         05  W-TISTATID          PIC S9(7)   VALUE ZERO.                  
000297         05  W-ADLAGOMR          PIC 9(2)    VALUE ZERO.                  
000298         05  W-ADGANG            PIC 9(2)    VALUE ZERO.                  
000299         05  W-ADPLATS           PIC 9(5)    VALUE ZERO.                  
000300     03  WDK7A1KY-MIN-X.                                                  
000301         05 W-IDDC-MIN          PIC X(2)           VALUE SPACE.           
000302         05 W-ADART-MIN.                                                  
000303            07  W-ADLAGOMR-MIN    PIC S9(3)  COMP-3  VALUE ZERO.          
000304            07  W-ADGANG-MIN      PIC S9(3)  COMP-3  VALUE ZERO.          
000305            07  W-ADPLATS-MIN     PIC S9(5)  COMP-3  VALUE ZERO.          
000306         05 W-IDARTNR-MIN         PIC S9(9)  COMP-3  VALUE ZERO.          
000307     03  WDK7A1KY-MAX-X.                                                  
000308         05  W-IDDC-MAX           PIC X(2)           VALUE SPACE.         
000309         05  W-ADART-MAX.                                                 
000310            07  W-ADLAGOMR-MAX    PIC S9(3)  COMP-3  VALUE ZERO.          
000311            07  W-ADGANG-MAX      PIC S9(3)  COMP-3  VALUE ZERO.          
000312            07  W-ADPLATS-MAX     PIC S9(5)  COMP-3  VALUE ZERO.          
000313         05 W-IDARTNR-MAX         PIC S9(9)  COMP-3  VALUE ZERO.          
000314     SKIP2                                                                
000315     03  W-IDDC-MIN-X.                                                    
000316         05  W-IDDC-MIN-1        PIC X(2)    VALUE LOW-VALUE.             
000317                                                                          
000318     03  W-IDDC-MAX-X.                                                    
000319         05  W-IDDC-MAX-1        PIC X(2)    VALUE HIGH-VALUE.            
000320                                                                          
000321     03  W-IDDC-REF-MIN-X.                                                
000322         05  W-IDDC-REF-MIN      PIC X(2)    VALUE LOW-VALUE.             
000323                                                                          
000330     03  W-IDDC-REF-MAX-X.                                                
000340         05  W-IDDC-REF-MAX      PIC X(2)    VALUE HIGH-VALUE.            
000350                                                                          
000360*    --- STATUS-KOD FRÅN IMS                                              
000370 01  STATUS-WS                   PIC XX.                                  
000371     88  SEGMENT-FINNS                       VALUE '  '.                  
000372     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
000373     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000374     SKIP2                                                                
000375 01  GODK-STATUSKODER.                                                    
000376     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000377     SKIP3                                                                
000378 01  FILLER                  PIC X(16)  VALUE 'SSA1            '.         
000379 01  SSA1                    PIC X(64).                                   
000380 01  FILLER                  PIC X(16)  VALUE 'SSA2            '.         
000381 01  SSA2                    PIC X(64).                                   
000382 01  FILLER                  PIC X(16)  VALUE 'SSA3            '.         
000383 01  SSA3                    PIC X(32).                                   
000384     EJECT                                                                
000385*    --- IMS FUNKTIONSKODER                                               
000386*01  -COPY W0003                                                          
000387     EJECT                                                                
000388*    ---  DLI INPUT-OUTPUT AREOR                                          
000389 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-AREA-WDK6'.         
000390     SKIP3                                                                
000391 01  DLI-IO-AREA-WDK6.                                                    
000392     03  IO-AREA-WDK6            PIC X(900)  VALUE SPACE.                 
000393     SKIP3                                                                
000394     03  WLARTC01 REDEFINES IO-AREA-WDK6.                                 
000395*        05  -COPY WDK601                                                 
000396     SKIP3                                                                
000397     03  WLARTC11 REDEFINES IO-AREA-WDK6.                                 
000398*        05  -COPY WDK611                                                 
000399     SKIP3                                                                
000400 01  FILLER                      PIC X(16)   VALUE                        
000401     'DLI-IO-AREA-K711'.                                                  
000402     SKIP3                                                                
000403 01  DLI-IO-AREA-WDK711.                                                  
000404*    03  -COPY WDK711                                                     
000405     SKIP3                                                                
000406 01  FILLER                      PIC X(16)   VALUE                        
000407     'DLI-IO-AREA-K712'.                                                  
000408 01  DLI-IO-AREA-WDK712.                                                  
000409*    03  -COPY WDK712                                                     
000410     EJECT                                                                
000411 01  DLI-IO-AREA-WDK7A.                                                   
000412     03  IO-AREA-WDK7A           PIC X(300)  VALUE SPACE.                 
000413     03  WDK7A1 REDEFINES IO-AREA-WDK7A.                                  
000414*        05  -COPY WDK7A1                                                 
000415     EJECT                                                                
000416 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WLLOCB01'.        
000417 01  DLI-IO-WLLOCB01.                                                     
000418*    03  -COPY WDJ901    -PRE LOCB-                                       
000419     EJECT                                                                
000420 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WLLOCB11'.        
000421 01  DLI-IO-WLLOCB11.                                                     
000422*    03  -COPY WDJ911    -PRE LOCB-                                       
000423     EJECT                                                                
000424 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
000425 01   DLI-IO-AREA-B601.                                                   
000426*     03  -COPY WDB601                                                    
000427                                                                          
000428     SKIP3                                                                
000429 LINKAGE SECTION.                                                         
000430                                                                          
000431*01  -COPY W0009   -PRE MSG-                                              
000432     EJECT                                                                
000433*01  -COPY W0009   -PRE DISP-                                             
000434     EJECT                                                                
000435*01  -COPY W0009   -PRE 4289-                                             
000436     EJECT                                                                
000437*01  -COPY W0009   -PRE SYNQ-                                             
000438     EJECT                                                                
000439*01  -COPY W0008   -PRE USEA-                                             
000440     05  FILLER                  PIC X.                                   
000441                                                                          
000442*01  -COPY W0008   -PRE ARTC-                                             
000443     05  FILLER                  PIC X.                                   
000444*01  -COPY W0008   -PRE WDK6-                                             
000445     05  FILLER                  PIC X.                                   
000446*01  -COPY W0008   -PRE WDK7-                                             
000447     05  FILLER                  PIC X.                                   
000448*01  -COPY W0008   -PRE LOCB-                                             
000449     05  FILLER                  PIC X.                                   
000450*    PCB'ER FÖR SUBPGM                                                    
000451                                                                          
000452 01 KOM-KOMA-PCB                 PIC X.                                   
000453*01  -COPY W0008   -PRE WDK7A-                                            
000454     05  FILLER                  PIC X.                                   
000455     EJECT                                                                
000456*01  -COPY W0008      -PRE WDB6-                                          
000457     05  FILLER                  PIC X.                                   
000458     EJECT                                                                
000459*    PCB'ER FÖR SUBPGM                                                    
000460 01  SYNQ-ATAB-PCB               PIC X.                                   
000461 01  SYNQ-WDK6-PCB               PIC X.                                   
000462 01  SYNQ-WDD3-PCB               PIC X.                                   
000463     EJECT                                                                
000464 PROCEDURE DIVISION USING MSG-PCB DISP-PCB 4289-PCB SYNQ-PCB              
000465                          USEA-PCB                                        
000466                          ARTC-PCB WDK6-PCB WDK7-PCB LOCB-PCB             
000467                          KOM-KOMA-PCB WDK7A-PCB WDB6-PCB                 
000468                          SYNQ-ATAB-PCB SYNQ-WDK6-PCB                     
000469                          SYNQ-WDD3-PCB.                                  
000470 MAIN SECTION.                                                            
000471     ENTRY 'DLITCBL' USING MSG-PCB DISP-PCB 4289-PCB SYNQ-PCB             
000472                           USEA-PCB                                       
000473                           ARTC-PCB WDK6-PCB WDK7-PCB LOCB-PCB            
000474                           KOM-KOMA-PCB WDK7A-PCB WDB6-PCB                
000475                           SYNQ-ATAB-PCB SYNQ-WDK6-PCB                    
000476                           SYNQ-WDD3-PCB.                                 
000477                                                                          
000478     PERFORM IMS-GET-MSG                                                  
000479                                                                          
000480     IF SEGMENT-FINNS                                                     
000481       PERFORM A-INIT                                                     
000482       PERFORM B-KOLLA-NYCKLAR                                            
000483       IF NYCKLAR-OK                                                      
000484         IF MFS-UPDATE                                                    
000485           PERFORM G-KOLLA-INPUT                                          
000486*            CALL FELLOG                                                  
000487           IF INDATA-OK                                                   
000488             PERFORM H-UPPDATERA                                          
000489           END-IF                                                         
000490         ELSE                                                             
000491           IF MFS-FIRST                                                   
000492             PERFORM C-FOERSTA-SIDA                                       
000493           ELSE                                                           
000494             PERFORM E-SAMMA-SIDA                                         
000495           END-IF                                                         
000496         END-IF                                                           
000497         PERFORM F-LAES-VISA-INFO                                         
000498       END-IF                                                             
000499       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O30401 + 4                      
000500       PERFORM IMS-INSERT-MSG                                             
000501     END-IF                                                               
000502                                                                          
000503     MOVE ZERO TO RETURN-CODE                                             
000504     GOBACK                                                               
000505     .                                                                    
000506     EJECT                                                                
000507 A-INIT SECTION.                                                          
000508                                                                          
000509     IF MSG-DUBBLA-TRANSKODER                                             
000510       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I30401                 
000511       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
000512       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
000513     ELSE                                                                 
000514       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W6I30401                 
000515       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
000516       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
000517     END-IF                                                               
000518                                                                          
000519     MOVE MSG-KDTRTYP                     TO MFS-KDTRTYP                  
000520     MOVE MSG-IDPFK                       TO MFS-IDPFK                    
000521     MOVE MFS-IDTRANS                     TO W-IDTRANS                    
000522                                                                          
000523     ACCEPT DAGENS-DATUM                  FROM DATE                       
000524     ACCEPT DAGENS-TID                    FROM TIME                       
000525     MOVE LOW-VALUE                       TO MSG-AREA                     
000526     MOVE 'W6O304N1'                      TO MFS-IDMOD                    
000527     MOVE '6304'                          TO MOD-IDTRANS                  
000528     MOVE MFS-RENSA-FAELT                 TO MOD-TEMFSFEL                 
000529                                             MOD-TEMFSINF                 
000530                                                                          
000531     IF EGEN-MID OR HELP-MID                                              
000532       CONTINUE                                                           
000533     ELSE                                                                 
000534       MOVE SPACE                         TO MFS-KDTRTYP                  
000535       MOVE '7'                           TO MFS-IDPFK                    
000536     END-IF                                                               
000537                                                                          
000538     MOVE LOW-VALUE  TO W-ADART-MIN                                       
000539     MOVE HIGH-VALUE TO W-ADART-MAX                                       
000540     MOVE ZERO       TO W-IDARTNR-MIN                                     
000541     MOVE +999999999 TO W-IDARTNR-MAX                                     
000542     .                                                                    
000543     EJECT                                                                
000544 B-KOLLA-NYCKLAR SECTION.                                                 
000545                                                                          
000546     MOVE JA                        TO NYCKLAR-SW                         
000547                                                                          
000548     MOVE ALL '+'                   TO MSGI-WMSGINIT                      
000549     MOVE '001'                     TO MSGI-KDCALL                        
000550     MOVE MSG-SIGNON-USERID         TO MSGI-IDUSER                        
000551     MOVE MSG-LTERM-NAME            TO MSGI-IDLTERM-USER                  
000552     MOVE '6304'                    TO MSGI-IDTRANS                       
000553     IF EGEN-MID OR HELP-MID                                              
000554        MOVE MID-IDARTNR-IN         TO MSGI-IDARTNR                       
000555     END-IF                                                               
000556     CALL W005INIT                  USING MSGI-WMSGINIT USEA-PCB          
000557                                                                          
000558     MOVE MSGI-IDLAND-SPR           TO MED-IDSKYLT                        
000559                                                                          
000560*    -- KONTROLL AV IDARTNR                                               
000561     MOVE MFS-RENSA-FAELT           TO MOD-IDARTNR-IN                     
000562                                                                          
000563     IF MID-IDARTNR-IN              NOT = ALL '+'                         
000564       MOVE '7'                     TO MFS-IDPFK                          
000565       MOVE SPACE                   TO MFS-KDTRTYP                        
000566     END-IF                                                               
000567                                                                          
000568     IF MSGI-IDARTNR NUMERIC                                              
000569       MOVE MSGI-IDARTNR            TO W-IDARTNR                          
000570                                       WS-IDARTNR                         
000571     ELSE                                                                 
000572       MOVE NEJ                     TO NYCKLAR-SW                         
000573     END-IF                                                               
000574                                                                          
000575*    -- KONTROLL AV IDDC                                                  
000576     MOVE MFS-RENSA-FAELT           TO MOD-IDDC-IN                        
000577                                                                          
000578     IF EGEN-MID                                                          
000579       IF MID-IDDC-IN                 NOT = ALL '+'                       
000580         MOVE MID-IDDC-IN             TO W-IDDC-B6                        
000581         MOVE '7'                     TO MFS-IDPFK                        
000582         MOVE SPACE                   TO MFS-KDTRTYP                      
000583       ELSE                                                               
000584         MOVE MID-IDDC-UT             TO W-IDDC-B6                        
000585       END-IF                                                             
000586     ELSE                                                                 
000587       MOVE MSGI-IDDC                 TO W-IDDC-B6                        
000588     END-IF                                                               
000589     MOVE W-IDDC-B6                   TO WS-IDDC-SPAR                     
000590     PERFORM IMS-GU-WDB601                                                
000591                                                                          
000592     IF DCS-KDDC = SPACE OR DCS-DDC                                       
000593        MOVE NEJ                    TO NYCKLAR-SW                         
000594     ELSE                                                                 
000595        MOVE DCS-IDDC               TO W-IDDC                             
000596                                       W-IDDC-MIN                         
000597                                       W-IDDC-MAX                         
000598     END-IF                                                               
000599                                                                          
000600                                                                          
000601     IF GODK-MID OR NYCKLAR-OK                                            
000602       MOVE MSGI-IDARTNR            TO MOD-IDARTNR-UT                     
000603       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
000604       MOVE DCS-IDDC                 TO MOD-IDDC-UT                       
000605       INSPECT MOD-IDDC-UT REPLACING LEADING ZERO BY SPACE                
000606     ELSE                                                                 
000607       MOVE MFS-RENSA-FAELT         TO MOD-IDARTNR-UT                     
000608                                       MOD-IDDC-UT                        
000609     END-IF                                                               
000610                                                                          
000611     IF NYCKLAR-FEL                                                       
000612       MOVE ERR-WRONG-KEY           TO MED-IDMFSFEL                       
000613       CALL WMEDKONV                USING MED-WMEDAREA                    
000614       MOVE MED-MFSFEL              TO MOD-TEMFSFEL                       
000615       PERFORM MFS-RENSA-FAELT-IN                                         
000616       PERFORM MFS-RENSA-FAELT-UT                                         
000617     END-IF                                                               
000618     .                                                                    
000619     EJECT                                                                
000620 C-FOERSTA-SIDA SECTION.                                                  
000621                                                                          
000622     PERFORM MFS-RENSA-FAELT-IN                                           
000623     .                                                                    
000624     EJECT                                                                
000625 E-SAMMA-SIDA SECTION.                                                    
000626                                                                          
000627     IF EGEN-MID OR HELP-MID                                              
000628       IF MID-INPUT                     = ALL '+'                         
000629        PERFORM MFS-RENSA-FAELT-IN                                        
000630       ELSE                                                               
000631        MOVE INF-PRESS-PF11        TO MED-IDMFSINF                        
000632        CALL WMEDKONV              USING MED-WMEDAREA                     
000633        MOVE MED-MFSINF            TO MOD-TEMFSINF                        
000634        PERFORM EA-MID-INDATA-TILL-MOD                                    
000635       END-IF                                                             
000636     ELSE                                                                 
000637        PERFORM MFS-RENSA-FAELT-IN                                        
000638     END-IF                                                               
000639     .                                                                    
000640     EJECT                                                                
000641 EA-MID-INDATA-TILL-MOD SECTION.                                          
000642                                                                          
000643     IF MID-ADLAGOMR-IN             NOT = ALL '+'                         
000644        MOVE MID-ADLAGOMR-IN        TO MOD-ADLAGOMR-IN                    
000645        MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-ADLAGOMR-IN-ATTR               
000646     ELSE                                                                 
000647        MOVE MFS-RENSA-FAELT        TO MOD-ADLAGOMR-IN                    
000648     END-IF                                                               
000649                                                                          
000650     IF MID-ADGANG-IN               NOT = ALL '+'                         
000651        MOVE MID-ADGANG-IN          TO MOD-ADGANG-IN                      
000652        MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-ADGANG-IN-ATTR                 
000653     ELSE                                                                 
000654        MOVE MFS-RENSA-FAELT        TO MOD-ADGANG-IN                      
000655     END-IF                                                               
000656                                                                          
000657     IF MID-ADPLATS-IN              NOT = ALL '+'                         
000658        MOVE MID-ADPLATS-IN         TO MOD-ADPLATS-IN                     
000659        MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-ADPLATS-IN-ATTR                
000660     ELSE                                                                 
000661        MOVE MFS-RENSA-FAELT        TO MOD-ADPLATS-IN                     
000662     END-IF                                                               
000663                                                                          
000664     IF MID-VKART-IN                NOT = ALL '+'                         
000665        MOVE MID-VKART-IN           TO MOD-VKART-IN                       
000666        MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-VKART-IN-ATTR                  
000667     ELSE                                                                 
000668        MOVE MFS-RENSA-FAELT        TO MOD-VKART-IN                       
000669     END-IF                                                               
000670                                                                          
000671     IF MID-VLARTNTO-IN             NOT = ALL '+'                         
000672        MOVE MID-VLARTNTO-IN        TO MOD-VLARTNTO-IN                    
000673        MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-VLARTNTO-IN-ATTR               
000674     ELSE                                                                 
000675        MOVE MFS-RENSA-FAELT        TO MOD-VLARTNTO-IN                    
000676     END-IF                                                               
000677                                                                          
000678     IF MID-KDVSOP-IN               NOT = ALL '+'                         
000679        MOVE MID-KDVSOP-IN          TO MOD-KDVSOP-IN                      
000680        MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KDVSOP-IN-ATTR                 
000681     ELSE                                                                 
000682        MOVE MFS-RENSA-FAELT        TO MOD-KDVSOP-IN                      
000683     END-IF                                                               
000684     .                                                                    
000685     EJECT                                                                
000686 F-LAES-VISA-INFO SECTION.                                                
000687                                                                          
000688     PERFORM IMS-GHU-ARTC11                                               
000689                                                                          
000690     IF SEGMENT-SAKNAS                                                    
000691        MOVE ARTIKEL-SAKNAS         TO MED-IDMFSFEL                       
000692        CALL WMEDKONV               USING MED-WMEDAREA                    
000693        MOVE MED-MFSFEL             TO MOD-TEMFSFEL                       
000694        PERFORM MFS-RENSA-FAELT-UT                                        
000695        PERFORM MFS-CLOSE-VSOP-IN                                         
000696     ELSE                                                                 
000697       MOVE W-IDDC               TO WS-IDDC                               
000698       IF NDC-US OR NDC-CN                                                
000699         IF NDC-US                                                        
000700            MOVE 'US'         TO W-IDLAND                                 
000710         ELSE                                                             
000720            MOVE 'CN'         TO W-IDLAND                                 
000730         END-IF                                                           
000740**       MOVE DCS-IDLANDX2 TO W-IDLAND                                    
000750         MOVE NEJ          TO WDK712-FINNS-SW                             
000760         PERFORM IMS-GU-WDK712                                            
000770         IF SEGMENT-FINNS                                                 
000780            MOVE JA        TO WDK712-FINNS-SW                             
000790         END-IF                                                           
000800       END-IF                                                             
000801       IF MSGI-KDMATT = 'U'                                               
000802         COMPUTE MOD-VKART-UT ROUNDED =                                   
000803                                   CLAG-VKART * CONV-GR-TO-OZ             
000804         END-COMPUTE                                                      
000805         COMPUTE MOD-VLARTNTO-UT ROUNDED =                                
000806                                   CLAG-VLARTNTO * CONV-CM3-TO-IN3        
000807         END-COMPUTE                                                      
000808         MOVE '    OZ'               TO MOD-BESORT-VKART-UT               
000809         MOVE 'CU.IN.'               TO MOD-BESORT-VLARTNTO-UT            
000810         MOVE CLAG-KDVSOP            TO MOD-KDVSOP                        
000811       ELSE                                                               
000812         MOVE CLAG-VKART             TO MOD-VKART-UT                      
000813         MOVE CLAG-VLARTNTO          TO MOD-VLARTNTO-UT                   
000814         MOVE '     G'               TO MOD-BESORT-VKART-UT               
000815         MOVE '   CM3'               TO MOD-BESORT-VLARTNTO-UT            
000816         MOVE CLAG-KDVSOP            TO MOD-KDVSOP                        
000817       END-IF                                                             
000818                                                                          
000819       IF WDK712-FINNS                                                    
000820         IF LART-VKART > 0                                                
000821           IF MSGI-KDMATT = 'U'                                           
000822             COMPUTE MOD-VKART-UT ROUNDED =                               
000823                                   LART-VKART * CONV-GR-TO-OZ             
000824           ELSE                                                           
000825             MOVE LART-VKART         TO MOD-VKART-UT                      
000826           END-IF                                                         
000827         END-IF                                                           
000828         IF LART-VLARTNTO > 0                                             
000829           IF MSGI-KDMATT = 'U'                                           
000830             COMPUTE MOD-VLARTNTO-UT ROUNDED =                            
000831                                   LART-VLARTNTO * CONV-CM3-TO-IN3        
000832           ELSE                                                           
000833             MOVE LART-VLARTNTO      TO MOD-VLARTNTO-UT                   
000834           END-IF                                                         
000835         END-IF                                                           
000836       END-IF                                                             
000837                                                                          
000838        PERFORM IMS-GHU-WDK711                                            
000839                                                                          
000840        IF SEGMENT-SAKNAS                                                 
000841           MOVE GOODS-ADDRESS-MISSING  TO MED-IDMFSFEL                    
000842           CALL WMEDKONV               USING MED-WMEDAREA                 
000843           MOVE MED-MFSFEL             TO MOD-TEMFSFEL                    
000844           PERFORM MFS-RENSA-WDK711-FAELT-UT                              
000845           PERFORM MFS-CLOSE-VSOP-IN                                      
000846        ELSE                                                              
000847           MOVE SLAG-ADLAGOMR          TO MOD-ADLAGOMR-UT                 
000848                                          W-ADLAGOMR-MIN                  
000849                                          W-ADLAGOMR-MAX                  
000850           MOVE SLAG-ADGANG            TO MOD-ADGANG-UT                   
000851                                          W-ADGANG-MIN                    
000852                                          W-ADGANG-MAX                    
000853           MOVE SLAG-ADPLATS           TO MOD-ADPLATS-UT                  
000854                                          W-ADPLATS-MIN                   
000855                                          W-ADPLATS-MAX                   
000856           PERFORM FA-LAS-ADRESS                                          
000857           PERFORM FB-CNTRL-VSOP                                          
000858        END-IF                                                            
000859     END-IF                                                               
000860     .                                                                    
000861 FA-LAS-ADRESS SECTION.                                                   
000862                                                                          
000863     MOVE +1 TO TAB-IX                                                    
000864     PERFORM IMS-GU-WDK7A                                                 
000865     PERFORM UNTIL TAB-IX > TAB-IX-MAX                                    
000866        IF SEGMENT-FINNS                                                  
000867           IF SEQA-IDARTNR = W-IDARTNR                                    
000868              CONTINUE                                                    
000869           ELSE                                                           
000870              MOVE SEQA-IDARTNR TO MOD-IDARTNR(TAB-IX)                    
000871              ADD +1 TO TAB-IX                                            
000872           END-IF                                                         
000873        ELSE                                                              
000874           MOVE MFS-RENSA-FAELT TO MOD-IDARTNR(TAB-IX)                    
000875           ADD +1 TO TAB-IX                                               
000876        END-IF                                                            
000877        PERFORM IMS-GN-WDK7A                                              
000878     END-PERFORM                                                          
000879                                                                          
000880     IF SEGMENT-FINNS                                                     
000881        MOVE INF-SEE-SCREEN-6306 TO MOD-INFO                              
000882     ELSE                                                                 
000883        MOVE MFS-RENSA-FAELT TO MOD-INFO                                  
000884     END-IF                                                               
000885     .                                                                    
000886     EJECT                                                                
000887 FB-CNTRL-VSOP SECTION.                                                   
000888      IF WS-IDDC-SPAR     =  MSGI-IDDC                                    
000889      AND SLAG-IDDC-REF   =  SPACE                                        
000890       CONTINUE                                                           
000891      ELSE                                                                
000892       MOVE MFS-CLOSE-FIELD            TO MOD-KDVSOP-IN-ATTR              
000893      END-IF                                                              
000894     .                                                                    
000895     EJECT                                                                
000896 G-KOLLA-INPUT SECTION.                                                   
000897     MOVE JA                        TO INDATA-SW                          
000898     IF MID-INPUT = ALL '+'                                               
000899        MOVE ERR-PF11-AND-NO-DATA   TO MED-IDMFSFEL                       
000900        CALL WMEDKONV               USING MED-WMEDAREA                    
000901        MOVE MED-MFSFEL             TO MOD-TEMFSFEL                       
000902        PERFORM MFS-ROER-EJ-FAELT-IN                                      
000903        PERFORM MFS-ROER-EJ-FAELT-UT                                      
000904     ELSE                                                                 
000905                                                                          
000906       IF MID-ADLAGOMR-IN           NOT = ALL '+'                         
000907         INSPECT MID-ADLAGOMR-IN REPLACING LEADING SPACE BY ZERO          
000908         IF MID-ADLAGOMR-IN         NOT NUMERIC                           
000909           MOVE MFS-NUM-FAELT-FEL   TO MOD-ADLAGOMR-IN-ATTR               
000910           MOVE NEJ                 TO INDATA-SW                          
000911         ELSE                                                             
000912           MOVE MFS-NUM-FAELT-RAETT TO MOD-ADLAGOMR-IN-ATTR               
000913         END-IF                                                           
000914       ELSE                                                               
000915         MOVE MFS-NUM-FAELT-RAETT   TO MOD-ADLAGOMR-IN-ATTR               
000916       END-IF                                                             
000917                                                                          
000918       IF MID-ADGANG-IN             NOT = ALL '+'                         
000919         INSPECT MID-ADGANG-IN REPLACING LEADING SPACE BY ZERO            
000920         IF MID-ADGANG-IN           NOT NUMERIC                           
000921           MOVE MFS-NUM-FAELT-FEL   TO MOD-ADGANG-IN-ATTR                 
000922           MOVE NEJ                 TO INDATA-SW                          
000923         ELSE                                                             
000924           MOVE MFS-NUM-FAELT-RAETT TO MOD-ADGANG-IN-ATTR                 
000925         END-IF                                                           
000926       ELSE                                                               
000927         MOVE MFS-NUM-FAELT-RAETT   TO MOD-ADGANG-IN-ATTR                 
000928       END-IF                                                             
000929                                                                          
000930       IF MID-ADPLATS-IN            NOT = ALL '+'                         
000931         INSPECT MID-ADPLATS-IN REPLACING LEADING SPACE BY ZERO           
000932         IF MID-ADPLATS-IN          NOT NUMERIC                           
000933           MOVE MFS-NUM-FAELT-FEL   TO MOD-ADPLATS-IN-ATTR                
000934           MOVE NEJ                 TO INDATA-SW                          
000935         ELSE                                                             
000936           MOVE MFS-NUM-FAELT-RAETT TO MOD-ADPLATS-IN-ATTR                
000937         END-IF                                                           
000938       ELSE                                                               
000939         MOVE MFS-NUM-FAELT-RAETT   TO MOD-ADPLATS-IN-ATTR                
000940       END-IF                                                             
000941                                                                          
000942       IF MID-KDVSOP-IN             NOT = ALL '+'                         
000943         INSPECT MID-KDVSOP-IN   REPLACING LEADING SPACE BY ZERO          
000944         IF MID-KDVSOP-IN        NOT NUMERIC                              
000945           MOVE MFS-NUM-FAELT-FEL   TO MOD-KDVSOP-IN-ATTR                 
000946           MOVE NEJ                 TO INDATA-SW                          
000947         ELSE                                                             
000948           MOVE MFS-NUM-FAELT-RAETT TO MOD-KDVSOP-IN-ATTR                 
000949         END-IF                                                           
000950       ELSE                                                               
000951         MOVE MFS-NUM-FAELT-RAETT TO MOD-KDVSOP-IN-ATTR                   
000952       END-IF                                                             
000953                                                                          
000954       IF MID-VKART-IN            NOT = ALL '+'                           
000955         INSPECT MID-VKART-IN REPLACING LEADING SPACE BY ZERO             
000956         MOVE MID-VKART-IN          TO DEC-IDFRIDATA                      
000957         MOVE  7                    TO DEC-KVHELTAL                       
000958         MOVE  2                    TO DEC-KVDECIMAL                      
000959         CALL WDECEDIT              USING DEC-WDECAREA                    
000960         IF DEC-KDSVAR-OK                                                 
000961           MOVE DEC-IDEDITDATA      TO WS-VKART-DEC                       
000962           MOVE MFS-NUM-FAELT-RAETT TO MOD-VKART-IN-ATTR                  
000963         ELSE                                                             
000964           MOVE MFS-NUM-FAELT-FEL   TO MOD-VKART-IN-ATTR                  
000965           MOVE NEJ                 TO INDATA-SW                          
000966         END-IF                                                           
000967                                                                          
000968         IF INDATA-OK                                                     
000969           IF WS-VKART-DEC NOT NUMERIC OR                                 
000970              WS-VKART-DEC = ZERO                                         
000971             MOVE MFS-NUM-FAELT-FEL     TO MOD-VKART-IN-ATTR              
000972             MOVE NEJ                   TO INDATA-SW                      
000973           ELSE                                                           
000974             IF MSGI-KDMATT           = 'U'                               
000975               COMPUTE WS-VKART ROUNDED =                                 
000976                                   WS-VKART-DEC * CONV-OZ-TO-GR           
000977               END-COMPUTE                                                
000978             ELSE                                                         
000979               MOVE WS-VKART-DEC TO WS-VKART                              
000980             END-IF                                                       
000981                                                                          
000982******* 9999999 = MAX-VÄRDE VIKT (GRAM)                                   
000983             IF WS-VKART > 9999999                                        
000984               MOVE MFS-NUM-FAELT-FEL     TO MOD-VKART-IN-ATTR            
000985               MOVE NEJ                   TO INDATA-SW                    
000986             END-IF                                                       
000987                                                                          
000988           END-IF                                                         
000989         END-IF                                                           
000990       ELSE                                                               
000991         MOVE MFS-NUM-FAELT-RAETT     TO MOD-VKART-IN-ATTR                
000992       END-IF                                                             
000993                                                                          
000994       IF MID-VLARTNTO-IN           NOT = ALL '+'                         
000995         INSPECT MID-VLARTNTO-IN REPLACING LEADING SPACE BY ZERO          
000996         MOVE MID-VLARTNTO-IN       TO DEC-IDFRIDATA                      
000997         MOVE  8                    TO DEC-KVHELTAL                       
000998         MOVE  1                    TO DEC-KVDECIMAL                      
000999         CALL WDECEDIT              USING DEC-WDECAREA                    
001000         IF DEC-KDSVAR-OK                                                 
001001           MOVE DEC-IDEDITDATA      TO WS-VLARTNTO                        
001002           MOVE MFS-NUM-FAELT-RAETT TO MOD-VLARTNTO-IN-ATTR               
001003         ELSE                                                             
001004           MOVE MFS-NUM-FAELT-FEL   TO MOD-VLARTNTO-IN-ATTR               
001005           MOVE NEJ                 TO INDATA-SW                          
001006         END-IF                                                           
001007                                                                          
001008         IF INDATA-OK                                                     
001009           IF WS-VLARTNTO NOT NUMERIC OR                                  
001010              WS-VLARTNTO = ZERO                                          
001011             MOVE MFS-NUM-FAELT-FEL     TO MOD-VLARTNTO-IN-ATTR           
001012             MOVE NEJ                   TO INDATA-SW                      
001013           ELSE                                                           
001014             IF MSGI-KDMATT           = 'U'                               
001015               COMPUTE WS-VLARTNTO ROUNDED =                              
001016                                     WS-VLARTNTO * CONV-IN3-TO-CM3        
001017               END-COMPUTE                                                
001018             END-IF                                                       
001019             MOVE WS-VLARTNTO           TO WS-VLARTNTO-DISP-NUM           
001020                                                                          
001021******* 99999999.9 = MAX-VÄRDE VOLYM (CM3)                                
001022             IF WS-VLARTNTO > 99999999.9                                  
001023               MOVE MFS-NUM-FAELT-FEL TO MOD-VLARTNTO-IN-ATTR             
001024               MOVE NEJ               TO INDATA-SW                        
001025             END-IF                                                       
001026                                                                          
001027           END-IF                                                         
001028         END-IF                                                           
001029       ELSE                                                               
001030         MOVE MFS-NUM-FAELT-RAETT     TO MOD-VLARTNTO-IN-ATTR             
001031       END-IF                                                             
001032     END-IF                                                               
001033                                                                          
001034     IF INDATA-FEL                                                        
001035       MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                          
001036       CALL WMEDKONV             USING MED-WMEDAREA                       
001037       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
001038       PERFORM MFS-ROER-EJ-FAELT-UT                                       
001039       PERFORM MFS-ROER-EJ-FAELT-IN                                       
001040     ELSE                                                                 
001041       PERFORM IMS-GU-ARTC01                                              
001042       IF SEGMENT-FINNS                                                   
001043         IF MID-VKART-IN    NOT = ALL '+'                    OR           
001044            MID-VLARTNTO-IN NOT = ALL '+'                                 
001045           MOVE ART-KDPRODSL     TO WS-KDPRODSL-X                         
001046           IF DCS-IDDC NOT = MSGI-IDDC                                    
001047              MOVE MSGI-IDDC     TO W-IDDC-B6                             
001048              PERFORM IMS-GU-WDB601                                       
001049           END-IF                                                         
001050           PERFORM IMS-GHU-WDK711                                         
001060           IF SLAG-IDDC-REF = '  '                                        
001070              CONTINUE                                                    
001080           ELSE                                                           
001090              MOVE NEJ  TO INDATA-SW                                      
001100           END-IF                                                         
001110         END-IF                                                           
001120                                                                          
001121         IF MID-ADLAGOMR-IN            NOT = ALL '+'         OR           
001122            MID-ADGANG-IN              NOT = ALL '+'         OR           
001123            MID-ADPLATS-IN             NOT = ALL '+'                      
001124            IF DCS-IDDC                NOT = MSGI-IDDC                    
001125              MOVE NEJ                 TO INDATA-SW                       
001126            END-IF                                                        
001127                                                                          
001128         END-IF                                                           
001129         IF INDATA-FEL                                                    
001130           MOVE UPD-NOT-ALLOWED       TO MED-IDMFSFEL                     
001131           CALL WMEDKONV              USING MED-WMEDAREA                  
001132           MOVE MED-MFSFEL            TO MOD-TEMFSFEL                     
001133           PERFORM MFS-RENSA-FAELT-IN                                     
001134         ELSE                                                             
001135           IF MID-ADLAGOMR-IN            NOT = ALL '+'       OR           
001136              MID-ADPLATS-IN             NOT = ALL '+'                    
001137                                                                          
001138             PERFORM IMS-GHU-WDK711                                       
001139             IF SEGMENT-FINNS                                             
001140               IF SLAG-KVLS > 0                                           
001141                 MOVE MID-ADLAGOMR-IN TO WS-ADLAGOMR                      
001142                 MOVE MID-ADPLATS-IN  TO WS-ADPLATS                       
001143                 IF WS-ADLAGOMR    = 0                      OR            
001144                    WS-ADPLATS      = 0                                   
001145                   MOVE NEJ         TO INDATA-SW                          
001146                   IF WS-ADLAGOMR    = 0                                  
001147                     MOVE MFS-NUM-FAELT-FEL                               
001148                                   TO MOD-ADLAGOMR-IN-ATTR                
001149                   END-IF                                                 
001150                   IF WS-ADPLATS     = 0                                  
001151                     MOVE MFS-NUM-FAELT-FEL                               
001152                                   TO MOD-ADPLATS-IN-ATTR                 
001153                   END-IF                                                 
001154                 END-IF                                                   
001155               END-IF                                                     
001156             END-IF                                                       
001157           END-IF                                                         
001158**   FÅR EJ NOLLA VIKT ELLER VOLYM OM SALDO FINNS PÅ CDC                  
001159           IF INDATA-FEL                                                  
001160             CONTINUE                                                     
001161           ELSE                                                           
001162             IF (WS-VKART-DEC = ZERO AND                                  
001163                 MID-VKART-IN NOT = ALL '+') OR                           
001164                (WS-VLARTNTO = ZERO AND                                   
001165                 MID-VLARTNTO-IN NOT = ALL '+')                           
001166                PERFORM IMS-GU-ARTC11                                     
001167                IF CLAG-KVLS       = 0                                    
001168                  AND CLAG-KVEFRS  = 0                                    
001169                  AND CLAG-KVAKS-CDC = 0                                  
001170                  AND CLAG-KVAKS-PAV = 0                                  
001171                  AND CLAG-KVAKS-T = 0                                    
001172                  CONTINUE                                                
001173                ELSE                                                      
001174                  IF WS-VKART-DEC = ZERO AND                              
001175                                      MID-VKART-IN NOT = ALL '+'          
001176                    MOVE MFS-NUM-FAELT-FEL TO MOD-VKART-IN-ATTR           
001177                  ELSE                                                    
001178                    MOVE MFS-NUM-FAELT-FEL TO MOD-VLARTNTO-IN-ATTR        
001179                  END-IF                                                  
001180                  MOVE NEJ                 TO INDATA-SW                   
001181                END-IF                                                    
001182             END-IF                                                       
001183           END-IF                                                         
001184                                                                          
001185           IF INDATA-FEL                                                  
001186             MOVE ZERO-NOT-ALLOWED      TO MED-IDMFSFEL                   
001187             CALL WMEDKONV              USING MED-WMEDAREA                
001188             MOVE MED-MFSFEL            TO MOD-TEMFSFEL                   
001189             PERFORM MFS-ROER-EJ-FAELT-UT                                 
001190             PERFORM MFS-ROER-EJ-FAELT-IN                                 
001191           END-IF                                                         
001192         END-IF                                                           
001193       ELSE                                                               
001194         MOVE ARTIKEL-SAKNAS         TO MED-IDMFSFEL                      
001195         CALL WMEDKONV               USING MED-WMEDAREA                   
001196         MOVE MED-MFSFEL             TO MOD-TEMFSFEL                      
001197         PERFORM MFS-RENSA-FAELT-IN                                       
001198         PERFORM MFS-RENSA-FAELT-UT                                       
001199       END-IF                                                             
001200     END-IF                                                               
001201     .                                                                    
001202     EJECT                                                                
001203 H-UPPDATERA SECTION.                                                     
001204                                                                          
001205     MOVE 'STA H-SEC '          TO FELTEXT                                
001206                                                                          
001207     PERFORM IMS-GHU-ARTC11                                               
001208     IF SEGMENT-FINNS                                                     
001209       MOVE NEJ TO WS-UPD-VOLUME                                          
001210       MOVE NEJ TO WS-UPD-LOCATION                                        
001211       IF MID-KDVSOP-IN    NOT = ALL '+'                                  
001212         MOVE MID-KDVSOP-IN    TO CLAG-KDVSOP                             
001213       END-IF                                                             
001214       MOVE W-IDDC TO WS-IDDC                                             
001215       IF NDC-US OR NDC-CN                                                
001216         IF NDC-US                                                        
001217            MOVE 'US'         TO W-IDLAND                                 
001218         ELSE                                                             
001219            MOVE 'CN'         TO W-IDLAND                                 
001220         END-IF                                                           
001221*        MOVE DCS-IDLANDX2 TO W-IDLAND                                    
001222         MOVE NEJ          TO WDK712-FINNS-SW                             
001223         PERFORM IMS-GHU-WDK712                                           
001224         IF SEGMENT-FINNS                                                 
001225           MOVE JA       TO WDK712-FINNS-SW                               
001226         END-IF                                                           
001227       END-IF                                                             
001228       IF WDK712-FINNS                                                    
001229         IF MID-VKART-IN     NOT = ALL '+'                                
001230           MOVE WS-VKART         TO LART-VKART                            
001231                                    SPARA-VKART                           
001232           MOVE NEJ              TO LART-FLMSKUPD                         
001233         ELSE                                                             
001234           MOVE LART-VKART       TO WS-VKART                              
001235         END-IF                                                           
001236                                                                          
001237         IF MID-VLARTNTO-IN  NOT = ALL '+'                                
001238           MOVE WS-VLARTNTO      TO LART-VLARTNTO                         
001239                                    SPARA-VLARTNTO                        
001240           MOVE NEJ              TO LART-FLMSKUPD                         
001241         ELSE                                                             
001242           MOVE LART-VLARTNTO    TO WS-VLARTNTO-DISP-NUM                  
001243         END-IF                                                           
001244       ELSE                                                               
001245         IF MID-VKART-IN     NOT = ALL '+'                                
001246           MOVE WS-VKART         TO CLAG-VKART                            
001247                                      SPARA-VKART                         
001248         ELSE                                                             
001249           MOVE CLAG-VKART       TO WS-VKART                              
001250         END-IF                                                           
001251                                                                          
001252         IF MID-VLARTNTO-IN  NOT = ALL '+'                                
001253           MOVE WS-VLARTNTO      TO CLAG-VLARTNTO                         
001254                                      SPARA-VLARTNTO                      
001255         ELSE                                                             
001256           MOVE CLAG-VLARTNTO    TO WS-VLARTNTO-DISP-NUM                  
001257         END-IF                                                           
001258       END-IF                                                             
001259                                                                          
001260       IF MID-VKART-IN    NOT = ALL '+' OR                                
001261          MID-VLARTNTO-IN NOT = ALL '+' OR                                
001262          MID-KDVSOP-IN   NOT = ALL '+'                                   
001263         IF WDK712-FINNS                                                  
001264            PERFORM IMS-REPL-WDK712                                       
001265** UPDATE COUNTRY SEGMENT - START                                         
001266                   IF NDC-CN                                              
001267                      MOVE '40'        TO W-IDDC-MIN-1                    
001268                                          W-IDDC-MAX-1                    
001269                      MOVE 'A'         TO W-IDDC-MIN-1(2:1)               
001270                      MOVE '9'         TO W-IDDC-MAX-1(2:1)               
001271                      MOVE '70'        TO W-IDDC-REF-MIN                  
001272                                          W-IDDC-REF-MAX                  
001273                      MOVE 'A'         TO W-IDDC-REF-MIN(2:1)             
001274                      MOVE '9'         TO W-IDDC-REF-MAX(2:1)             
001275                   END-IF                                                 
001276                                                                          
001277                   IF NDC-US                                              
001278                         MOVE '70'     TO W-IDDC-MIN-1                    
001279                                          W-IDDC-MAX-1                    
001280                         MOVE 'A'      TO W-IDDC-MIN-1(2:1)               
001281                         MOVE '9'      TO W-IDDC-MAX-1(2:1)               
001290                                                                          
001300                         MOVE '40'     TO W-IDDC-REF-MIN                  
001400                                          W-IDDC-REF-MAX                  
001500                         MOVE 'A'      TO W-IDDC-REF-MIN(2:1)             
001600                         MOVE '9'      TO W-IDDC-REF-MAX(2:1)             
001700                   END-IF                                                 
001800                                                                          
001900                   PERFORM IMS-GU-WDK711-REF                              
002000                   IF SEGMENT-FINNS                                       
002100                      IF NDC-US                                           
002200                         MOVE WC-LAND-CN     TO LART-IDLANDX2             
002300                                                W-IDLAND                  
002400                      ELSE                                                
002500                         MOVE WC-LAND-US     TO LART-IDLANDX2             
002600                                                W-IDLAND                  
002700                      END-IF                                              
002800                      PERFORM IMS-GHU-WDK712                              
002900                      IF MID-VKART-IN NOT = ALL '+'                       
003000                         MOVE WS-VKART    TO LART-VKART                   
003010                         MOVE NEJ         TO LART-FLMSKUPD                
003020                      END-IF                                              
003030                      IF MID-VLARTNTO-IN NOT = ALL '+'                    
003040                        MOVE WS-VLARTNTO-DISP-NUM TO LART-VLARTNTO        
003050                        MOVE NEJ                  TO LART-FLMSKUPD        
003060                      END-IF                                              
003070                      PERFORM IMS-REPL-WDK712                             
003080                   END-IF                                                 
003090** UPDATE COUNTRY SEGMENT - END                                           
003100             IF MID-KDVSOP-IN    NOT = ALL '+'                            
003200                 MOVE MSGI-IDUSER  TO CLAG-IDUSER-VUPD                    
003300                 MOVE DAGENS-DATUM TO CLAG-TIUPPDAT-VUPD                  
003400                 PERFORM IMS-REPL-ARTC11                                  
003500             END-IF                                                       
003600**                                                                        
003700             MOVE NEJ TO WS-VKART-UPD                                     
003701                         WS-VLARTNTO-UPD                                  
003702             MOVE CLAG-IDDC-REF   TO REF-WS-IDDC                          
003703             IF  (NDC-US AND REF-NDC-US) OR                               
003704                 (NDC-CN AND REF-NDC-CN)                                  
003705               IF MID-VKART-IN NOT = ALL '+'                              
003706                 MOVE JA TO WS-VKART-UPD                                  
003707               END-IF                                                     
003708               IF MID-VLARTNTO-IN NOT = ALL '+'                           
003709                 MOVE JA TO WS-VLARTNTO-UPD                               
003710               END-IF                                                     
003711             END-IF                                                       
003712             IF  NDC-US OR NDC-CN                                         
003714               IF MID-VKART-IN NOT = ALL '+' AND CLAG-VKART = 0           
003715                 MOVE JA TO WS-VKART-UPD                                  
003716               END-IF                                                     
003717               IF MID-VLARTNTO-IN NOT = ALL '+' AND                       
003718                  CLAG-VLARTNTO = 0                                       
003719                 MOVE JA TO WS-VLARTNTO-UPD                               
003720               END-IF                                                     
003721             END-IF                                                       
003722             IF  WS-VKART-UPD = JA OR WS-VLARTNTO-UPD = JA                
003724                 IF WS-VKART-UPD = JA                                     
003725                    MOVE WS-VKART    TO CLAG-VKART                        
003726                 END-IF                                                   
003727                 IF WS-VLARTNTO-UPD = JA                                  
003728                    IF CLAG-VLARTNTO = ZERO                               
003729                      MOVE 002        TO SYNQ-KDCALL                      
003730                    ELSE                                                  
003731                      MOVE 003        TO SYNQ-KDCALL                      
003732                    END-IF                                                
003733                    MOVE WS-VLARTNTO-DISP-NUM TO CLAG-VLARTNTO            
003734                    MOVE JA TO WS-UPD-VOLUME                              
003735                 END-IF                                                   
003736                                                                          
003737                 IF WS-VKART-UPD = JA OR                                  
003738                    WS-VLARTNTO-UPD = JA                                  
003739                    MOVE MSGI-IDUSER  TO CLAG-IDUSER-VUPD                 
003740                    MOVE DAGENS-DATUM TO CLAG-TIUPPDAT-VUPD               
003741                    IF MID-VKART-IN    NOT = ALL '+' AND                  
003742                      (CLAG-VKART-NTO = ZERO OR                           
003743                       CLAG-VKART-NTO > CLAG-VKART)                       
003744                       MOVE CLAG-VKART TO CLAG-VKART-NTO                  
003745                       MOVE '4'        TO CLAG-KDUVKNTO                   
003746                    END-IF                                                
003747                 END-IF                                                   
003748                 PERFORM IMS-REPL-ARTC11                                  
003749                 IF WS-UPD-VOLUME = JA                                    
003750                   MOVE W-IDARTNR         TO SYNQ-IDARTNR                 
003751                   CALL W488PRMA USING  SYNQ-W488PRMA                     
003752                   SYNQ-ATAB-PCB SYNQ-WDK6-PCB SYNQ-WDD3-PCB              
003753                 END-IF                                                   
003754             END-IF                                                       
003755**                                                                        
003756         ELSE                                                             
003757             MOVE MSGI-IDUSER  TO CLAG-IDUSER-VUPD                        
003758             MOVE DAGENS-DATUM TO CLAG-TIUPPDAT-VUPD                      
003759             IF MID-VKART-IN    NOT = ALL '+' AND                         
003760               (CLAG-VKART-NTO = ZERO OR                                  
003761                CLAG-VKART-NTO > CLAG-VKART)                              
003762                MOVE CLAG-VKART TO CLAG-VKART-NTO                         
003763                MOVE '4'        TO CLAG-KDUVKNTO                          
003764             END-IF                                                       
003765             PERFORM IMS-REPL-ARTC11                                      
003766         END-IF                                                           
003767**************SCR2045678****************************************          
003768         PERFORM HB-KONTROLL-BYTES                                        
003769         IF CORE-OK                                                       
003770           PERFORM IMS-GHU-WDK611                                         
003771           IF NDC-US OR NDC-CN                                            
003772             MOVE NEJ  TO WDK712-FINNS-SW                                 
003773             PERFORM IMS-GHU-WDK712-BYTES                                 
003774             IF SEGMENT-FINNS                                             
003775               MOVE JA TO WDK712-FINNS-SW                                 
003776             END-IF                                                       
003777           END-IF                                                         
003778           IF MID-VKART-IN NOT = ALL '+'                                  
003779             IF WDK712-FINNS                                              
003780               MOVE SPARA-VKART      TO LART-VKART                        
003781               MOVE NEJ              TO LART-FLMSKUPD                     
003782             ELSE                                                         
003783               MOVE SPARA-VKART      TO CLAG-VKART                        
003784             END-IF                                                       
003785           END-IF                                                         
003786           IF MID-VLARTNTO-IN      NOT = ALL '+'                          
003787             IF WDK712-FINNS                                              
003788               MOVE SPARA-VLARTNTO   TO LART-VLARTNTO                     
003789               MOVE NEJ              TO LART-FLMSKUPD                     
003790             ELSE                                                         
003791               MOVE SPARA-VLARTNTO   TO CLAG-VLARTNTO                     
003792             END-IF                                                       
003793           END-IF                                                         
003794           IF SEGMENT-FINNS                                               
003795             IF WDK712-FINNS                                              
003796               PERFORM IMS-REPL-WDK712                                    
003797             ELSE                                                         
003798               IF MID-VKART-IN    NOT = ALL '+' OR                        
003799                  MID-VLARTNTO-IN NOT = ALL '+'                           
003800                  MOVE MSGI-IDUSER  TO CLAG-IDUSER-VUPD                   
003801                  MOVE DAGENS-DATUM TO CLAG-TIUPPDAT-VUPD                 
003802                  IF MID-VKART-IN NOT = ALL '+' AND                       
003803                    (CLAG-VKART-NTO = ZERO OR                             
003804                     CLAG-VKART-NTO > CLAG-VKART)                         
003805                     MOVE CLAG-VKART TO CLAG-VKART-NTO                    
003806                     MOVE '4'        TO CLAG-KDUVKNTO                     
003807                  END-IF                                                  
003808               END-IF                                                     
003809               PERFORM IMS-REPL-WDK611                                    
003810             END-IF                                                       
003811           END-IF                                                         
003812         END-IF                                                           
003813**************SCR2045678****************************************          
003814       END-IF                                                             
003815     END-IF                                                               
003816                                                                          
003817     IF MID-ADLAGOMR-IN NOT = ALL '+'                   OR                
003818        MID-ADGANG-IN   NOT = ALL '+'                   OR                
003819        MID-ADPLATS-IN  NOT = ALL '+'                                     
003820                                                                          
003821        PERFORM HC-UPDATE-BYART                                           
003822        PERFORM IMS-GHU-WDK711                                            
003823        MOVE JA TO WS-UPD-LOCATION                                        
003824        IF SEGMENT-FINNS                                                  
003825          IF MID-ADLAGOMR-IN NOT = ALL '+'                                
003826             MOVE MID-ADLAGOMR-IN     TO SLAG-ADLAGOMR                    
003827                                         WS-ADLAGOMR-WDJ9                 
003828          ELSE                                                            
003829             MOVE SLAG-ADLAGOMR       TO WS-ADLAGOMR-WDJ9                 
003830          END-IF                                                          
003831                                                                          
003832          IF MID-ADGANG-IN   NOT = ALL '+'                                
003833             MOVE MID-ADGANG-IN       TO SLAG-ADGANG                      
003834                                         WS-ADGANG-WDJ9                   
003835          ELSE                                                            
003836             MOVE SLAG-ADGANG         TO WS-ADGANG-WDJ9                   
003837          END-IF                                                          
003838                                                                          
003839          IF MID-ADPLATS-IN  NOT = ALL '+'                                
003840             MOVE MID-ADPLATS-IN      TO SLAG-ADPLATS                     
003841                                         WS-ADPLATS-WDJ9                  
003842          ELSE                                                            
003843             MOVE SLAG-ADPLATS        TO WS-ADPLATS-WDJ9                  
003844          END-IF                                                          
003845          PERFORM IMS-REPL-WDK711                                         
003846                                                                          
003854                                                                          
003855***       UPPDATERING AV PLATSHISTORIK (WDJ9)                             
003856          PERFORM IMS-GU-LOCB01                                           
003857          IF SEGMENT-SAKNAS                                               
003858            MOVE W-IDARTNR TO LOCB-ART-IDARTNR                            
003859            PERFORM IMS-ISRT-LOCB01                                       
003860            PERFORM IMS-GU-LOCB01                                         
003861          END-IF                                                          
003862          IF SEGMENT-FINNS                                                
003863            PERFORM UNTIL SEGMENT-SAKNAS OR LOCB-HIST-KDLOC = 'P'         
003864            PERFORM IMS-GHNP-LOCB11                                       
003865              IF SEGMENT-FINNS AND LOCB-HIST-KDLOC = 'P'                  
003866                MOVE FUNCTION CURRENT-DATE(1:8) TO                        
003867                                              LOCB-HIST-DASTODAT          
003868                MOVE MSGI-IDUSER TO LOCB-HIST-IDUSER-STO                  
003869                PERFORM IMS-REPL-LOCB11                                   
003870              END-IF                                                      
003871            END-PERFORM                                                   
003872            MOVE FUNCTION CURRENT-DATE(1:8)  TO LOGG-DATUM                
003873            MOVE FUNCTION CURRENT-DATE(9:6)  TO LOGG-TID                  
003874            COMPUTE LOCB-HIST-DASTADAT-9KOMPL = 99999999 -                
003875                                                      LOGG-DATUM          
003876            COMPUTE LOCB-HIST-TISTATID-9KOMPL = 999999 - LOGG-TID         
003877            MOVE DCS-IDDC           TO LOCB-HIST-IDDC                     
003878            MOVE PRIME-LOCATION     TO LOCB-HIST-KDLOC                    
003879            MOVE WS-ADLAGOMR-WDJ9   TO LOCB-HIST-ADLAGOMR                 
003880            MOVE WS-ADGANG-WDJ9     TO LOCB-HIST-ADGANG                   
003881            MOVE WS-ADPLATS-WDJ9    TO LOCB-HIST-ADPLATS                  
003882            MOVE MSGI-IDUSER        TO LOCB-HIST-IDUSER                   
003883            MOVE SPACE              TO LOCB-HIST-IDUSER-STO               
003884            MOVE ZERO               TO LOCB-HIST-DASTODAT                 
003885                                                                          
003886            PERFORM IMS-ISRT-LOCB11                                       
003887          END-IF                                                          
003888        END-IF                                                            
003889     END-IF                                                               
003890                                                                          
003891*UPPDATERING AV ORDERKÖN MED LAGEROMRÅDE.                                 
003892*ÖPPNAS NÄR SDC/NDC SKALL BÖRJA ANVÄNDA DENNA FUNKTION.                   
003893     IF MID-ADLAGOMR-IN NOT = ALL '+'                                     
003894     OR MID-ADGANG-IN   NOT = ALL '+'                                     
003895     OR MID-ADPLATS-IN  NOT = ALL '+'                                     
003896     OR MID-VKART-IN    NOT = ALL '+'                                     
003897     OR MID-VLARTNTO-IN NOT = ALL '+'                                     
003898        PERFORM HA-STARTA-W40289                                          
003899        PERFORM I-STARTA-DISPATCHEN                                       
003900     END-IF                                                               
003901                                                                          
003910     MOVE INF-UPDATE-DONE            TO MED-IDMFSINF                      
003911     CALL WMEDKONV                   USING MED-WMEDAREA                   
003912     MOVE MED-MFSINF                 TO MOD-TEMFSINF                      
003913     PERFORM MFS-FORM-ATTR                                                
003914     PERFORM MFS-RENSA-FAELT-IN                                           
003915     .                                                                    
003916     EJECT                                                                
003917 HA-STARTA-W40289 SECTION.                                                
003918     MOVE 'STA HA-STARTA SEC'  TO FELTEXT                                 
003919                                                                          
003920     MOVE W-IDARTNR            TO P-TO-P2-MID-IDARTNR-IN                  
003921     MOVE DCS-IDDC             TO P-TO-P2-MID-IDDC-IN                     
003922     IF WS-UPD-LOCATION = JA                                              
003923      MOVE WS-ADLAGOMR-WDJ9     TO P-TO-P2-MID-ADLAGOMR-IN                
003924      MOVE WS-ADGANG-WDJ9       TO P-TO-P2-MID-ADGANG-IN                  
003925      MOVE WS-ADPLATS-WDJ9      TO P-TO-P2-MID-ADPLATS-IN                 
003926     ELSE                                                                 
003927      MOVE ZERO              TO P-TO-P2-MID-ADLAGOMR-IN                   
003928      MOVE ZERO              TO P-TO-P2-MID-ADGANG-IN                     
003929      MOVE ZERO              TO P-TO-P2-MID-ADPLATS-IN                    
003930      MOVE ZERO              TO P-TO-P2-MID-IDDC-IN                       
003931     END-IF                                                               
003933     IF MID-VKART-IN NOT = ALL '+'                                        
003934      MOVE WS-VKART TO P-TO-P2-MID-VKART-IN                               
003935     ELSE                                                                 
003936      MOVE ZERO TO P-TO-P2-MID-VKART-IN                                   
003937     END-IF                                                               
003938                                                                          
003939     IF MID-VLARTNTO-IN NOT = ALL '+'                                     
003940      MOVE WS-VLARTNTO TO P-TO-P2-MID-VLARTNTO-IN                         
003941     ELSE                                                                 
003942      MOVE ZERO TO P-TO-P2-MID-VLARTNTO-IN                                
003943     END-IF                                                               
003944                                                                          
003945     MOVE ZERO                 TO P-TO-P2-MID-IDDISTR-IN                  
003946                                  P-TO-P2-MID-IDKUNDNR-IN                 
003947                                  P-TO-P2-MID-IDORDNR5-IN                 
003948                                  P-TO-P2-MID-IDORDER-IN                  
003949                                  P-TO-P2-MID-IDLOPNR-IN                  
003950                                                                          
003951     COMPUTE P-TO-P2-KVLL   =  LENGTH OF P-TO-P2-MID-W4I28901 + 25        
003952     END-COMPUTE                                                          
003953                                                                          
003954     MOVE 'W4T289X '           TO P-TO-P2-KDTRANS                         
003955     MOVE '6304'               TO P-TO-P2-IDTRANS                         
003956     MOVE MFS-KDMFSFOR         TO P-TO-P2-KDMFSFOR                        
003957                                                                          
003958     PERFORM IMS-PURG-4289                                                
003959     .                                                                    
003960     EJECT                                                                
003961 HB-KONTROLL-BYTES SECTION.                                               
003962                                                                          
003963     MOVE W-IDARTNR        TO TEST-IDARTNR                                
003964     IF BYT02-RENOV                                                       
003965       IF BYT16-BYTES                                                     
003966          COMPUTE TEST-IDARTNR = TEST-IDARTNR +                           
003967                                 6000                                     
003968          END-COMPUTE                                                     
003969       ELSE                                                               
003970          COMPUTE TEST-IDARTNR = TEST-IDARTNR +                           
003971                                 1000                                     
003972          END-COMPUTE                                                     
003973       END-IF                                                             
003974       MOVE JA TO CORE-SW                                                 
003975       MOVE TEST-IDARTNR      TO W-IDARTNR2                               
003976     END-IF                                                               
003977     .                                                                    
003978     EJECT                                                                
003979 HC-UPDATE-BYART       SECTION.                                           
003980                                                                          
003981     MOVE W-IDDC                 TO WS-IDDC                               
003982     MOVE WS-IDARTNR             TO TEST-IDARTNR                          
003983     IF BYT03-OBJEKT AND                                                  
003984        SDC-NL-ET                                                         
003985       IF BYT16-RADIO                                                     
003986         MOVE 3                  TO WS-ARTSIFFRA                          
003987       ELSE                                                               
003988         IF ART-0                                                         
003989           MOVE 0                TO WS-ARTSIFFRA                          
003990         ELSE                                                             
003991           IF ART-1                                                       
003992             MOVE 1              TO WS-ARTSIFFRA                          
003993           ELSE                                                           
003994             IF ART-2                                                     
003995               MOVE 2            TO WS-ARTSIFFRA                          
003996             ELSE                                                         
003997               IF ART-3                                                   
003998                 MOVE 3          TO WS-ARTSIFFRA                          
003999               END-IF                                                     
004000             END-IF                                                       
004001           END-IF                                                         
004002         END-IF                                                           
004003       END-IF                                                             
004004                                                                          
004005       MOVE WS-IDARTNR           TO W-IDARTNR-BYT                         
004006                                                                          
004007       PERFORM DB2-SELECT-BYART                                           
004008       IF RADER-FINNS                                                     
004009         IF MID-ADLAGOMR-IN NOT = ALL '+'                                 
004010           MOVE MID-ADLAGOMR-IN  TO WS-ADLAGOMR                           
004011           MOVE ADLAGOMR-WS      TO BYART-ADLAGOMR                        
004012         END-IF                                                           
004013                                                                          
004014         IF MID-ADGANG-IN NOT = ALL '+'                                   
004015           MOVE MID-ADGANG-IN    TO WS-ADGANG                             
004016           MOVE ADGANG-WS        TO BYART-ADGANG                          
004017         END-IF                                                           
004018                                                                          
004019         IF MID-ADPLATS-IN NOT = ALL '+'                                  
004020           MOVE MID-ADPLATS-IN   TO BYART-ADPLATS                         
004021         END-IF                                                           
004022                                                                          
004023         PERFORM DB2-UPDATE-BYART                                         
004024       END-IF                                                             
004025     END-IF                                                               
004026     .                                                                    
004027     EJECT                                                                
004028 I-STARTA-DISPATCHEN   SECTION.                                           
004029                                                                          
004030     MOVE SPACE                 TO MSG-KOM-WMSGKOM                        
004031     MOVE +54                   TO MSG-KOM-KVLL                           
004032     MOVE LOW-VALUE             TO MSG-KOM-KDZ1                           
004033     MOVE LOW-VALUE             TO MSG-KOM-KDZ2                           
004034     MOVE SPACE                 TO MSG-KOM-KDTRANS                        
004035     MOVE 'W6I19B01'            TO MSG-KOM-IDCPYTXT                       
004036     MOVE 'INLEV   '            TO MSG-KOM-IDSNDNOD                       
004037     MOVE 'W6030400'            TO MSG-KOM-IDSNDJOB                       
004038     MOVE DAGENS-DATUM          TO MSG-KOM-TIREGDAT                       
004039     MOVE DAGENS-TID            TO MSG-KOM-TIKLOCK                        
004040     MOVE SPACE                 TO MSG-KOM-IDMFSMED                       
004041                                                                          
004042     MOVE ALL '+'               TO MOD619B-MID-W6I19B01                   
004043     MOVE W-IDARTNR             TO MOD619B-MID-IDARTNR                    
004044     MOVE DCS-IDDC              TO MOD619B-MID-IDDC                       
004045     MOVE MID-ADLAGOMR-IN       TO MOD619B-MID-ADLAGOMR                   
004046     MOVE MID-ADGANG-IN         TO MOD619B-MID-ADGANG                     
004047     MOVE MID-ADPLATS-IN        TO MOD619B-MID-ADPLATS                    
004048     MOVE WS-VKART              TO MOD619B-MID-VKART                      
004049     MOVE WS-VLARTNTO-DISP-ALFA TO MOD619B-MID-VLARTNTO                   
004050     COMPUTE P-TO-P-KVLL        =  LNG-P-TO-P-PREFIX + 87                 
004051     MOVE 'W6T19BX '            TO P-TO-P-KDTRANS                         
004052     MOVE '6304'                TO P-TO-P-IDTRANS                         
004053     MOVE MFS-KDMFSFOR          TO P-TO-P-KDMFSFOR                        
004054     MOVE MOD619B-MID-W6I19B01  TO P-TO-P-DATA                            
004055                                                                          
004056     CALL W006KOM USING MSG-PCB                                           
004057                        DISP-PCB                                          
004058                        KOM-KOMA-PCB                                      
004059                        MSG-KOM-WMSGKOM                                   
004060                        P-TO-P-SW                                         
004061     .                                                                    
004062     EJECT                                                                
004063 MFS-RENSA-FAELT-UT SECTION.                                              
004064                                                                          
004065*    --- ALLA UTDATA-FÄLT                                                 
004066     MOVE MFS-RENSA-FAELT TO MOD-ADLAGOMR-UT                              
004067                             MOD-ADGANG-UT                                
004068                             MOD-ADPLATS-UT                               
004069                             MOD-VKART-UT                                 
004070                             MOD-VLARTNTO-UT                              
004071                             MOD-KDVSOP                                   
004072     .                                                                    
004073     SKIP2                                                                
004074 MFS-RENSA-WDK711-FAELT-UT SECTION.                                       
004075                                                                          
004076*    --- ALLA UTDATA-FÄLT                                                 
004077     MOVE MFS-RENSA-FAELT TO MOD-ADLAGOMR-UT                              
004078                             MOD-ADGANG-UT                                
004079                             MOD-ADPLATS-UT                               
004080                             MOD-INFO                                     
004081     MOVE +1 TO TAB-IX                                                    
004082     PERFORM UNTIL TAB-IX > TAB-IX-MAX                                    
004083        MOVE MFS-RENSA-FAELT TO MOD-IDARTNR(TAB-IX)                       
004084        ADD +1 TO TAB-IX                                                  
004085     END-PERFORM                                                          
004086     .                                                                    
004087     SKIP2                                                                
004088 MFS-RENSA-FAELT-IN SECTION.                                              
004089                                                                          
004090*    --- ALLA INDATA-FÄLT                                                 
004091     MOVE MFS-RENSA-FAELT TO MOD-ADLAGOMR-IN                              
004092                             MOD-ADGANG-IN                                
004093                             MOD-ADPLATS-IN                               
004094                             MOD-VKART-IN                                 
004095                             MOD-VLARTNTO-IN                              
004096                             MOD-KDVSOP-IN                                
004097     .                                                                    
004098     SKIP2                                                                
004099 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
004100                                                                          
004101*    --- ALLA UTDATA-FÄLT                                                 
004102     MOVE MFS-ROER-EJ-FAELT TO   MOD-ADLAGOMR-UT                          
004103                                 MOD-ADGANG-UT                            
004104                                 MOD-ADPLATS-UT                           
004105                                 MOD-VKART-UT                             
004106                                 MOD-VLARTNTO-UT                          
004107                                 MOD-INFO                                 
004108     MOVE +1 TO TAB-IX                                                    
004109     PERFORM UNTIL TAB-IX > TAB-IX-MAX                                    
004110        MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR(TAB-IX)                     
004111        ADD +1 TO TAB-IX                                                  
004112     END-PERFORM                                                          
004113     .                                                                    
004114     SKIP2                                                                
004115 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
004116                                                                          
004117*    --- ALLA INDATA-FÄLT                                                 
004118     MOVE MFS-ROER-EJ-FAELT    TO MOD-ADLAGOMR-IN                         
004119                                  MOD-ADGANG-IN                           
004120                                  MOD-ADPLATS-IN                          
004121                                  MOD-VKART-IN                            
004122                                  MOD-VLARTNTO-IN                         
004123                                  MOD-KDVSOP-IN                           
004124     .                                                                    
004125     SKIP2                                                                
004126 MFS-FORM-ATTR SECTION.                                                   
004127                                                                          
004128*    --- ALLA INDATA-FÄLT                                                 
004129     MOVE MFS-FORMATETS-ATTR TO MOD-ADLAGOMR-IN-ATTR                      
004130                                MOD-ADGANG-IN-ATTR                        
004131                                MOD-ADPLATS-IN-ATTR                       
004132                                MOD-VKART-IN-ATTR                         
004133                                MOD-VLARTNTO-IN-ATTR                      
004134     .                                                                    
004135     EJECT                                                                
004136 MFS-CLOSE-VSOP-IN  SECTION.                                              
004137                                                                          
004138*    NO UPDATE OF VSOP - CLOSE FIELD                                      
004139     MOVE MFS-CLOSE-FIELD            TO MOD-KDVSOP-IN-ATTR                
004140     .                                                                    
004141     EJECT                                                                
004142* --- IMS SEKTIONER ---                                                   
004143     SKIP3                                                                
004144 IMS-GET-MSG SECTION.                                                     
004145                                                                          
004146     MOVE '  QC' TO GODK-STATUSKODER                                      
004147     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
004148     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
004149     PERFORM IMS-STATUSKONTROLL                                           
004150     .                                                                    
004151     SKIP3                                                                
004152 IMS-INSERT-MSG SECTION.                                                  
004153                                                                          
004154     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
004155     MOVE SPACE TO GODK-STATUSKODER                                       
004156     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
004157     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
004158     PERFORM IMS-STATUSKONTROLL                                           
004159     .                                                                    
004160     EJECT                                                                
004161 IMS-PURG-4289    SECTION.                                                
004162                                                                          
004163     MOVE LOW-VALUE TO P-TO-P2-KDZ1 P-TO-P2-KDZ2                          
004164     MOVE SPACE TO GODK-STATUSKODER                                       
004165     CALL  CBLTDLI  USING PURG 4289-PCB P-TO-P-SW2                        
004166     MOVE 4289-STATUS-CODE TO STATUS-WS                                   
004167     PERFORM IMS-STATUSKONTROLL                                           
004168     .                                                                    
004169                                                                          
004170 IMS-GU-ARTC01 SECTION.                                                   
004171                                                                          
004172     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
004173          DELIMITED BY SIZE INTO SSA1                                     
004174     MOVE '  GE' TO GODK-STATUSKODER                                      
004175     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-WDK6 SSA1                 
004176     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
004177     PERFORM IMS-STATUSKONTROLL                                           
004178     .                                                                    
004179     SKIP3                                                                
004180 IMS-GHU-ARTC11 SECTION.                                                  
004181                                                                          
004182     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
004183          DELIMITED BY SIZE INTO SSA1                                     
004184     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
004185          DELIMITED BY SIZE INTO SSA2                                     
004186     MOVE '  GE' TO GODK-STATUSKODER                                      
004187     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA-WDK6 SSA1 SSA2           
004188     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
004189     PERFORM IMS-STATUSKONTROLL                                           
004190     .                                                                    
004191     SKIP3                                                                
004192 IMS-GU-ARTC11 SECTION.                                                   
004193                                                                          
004194     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
004195          DELIMITED BY SIZE INTO SSA1                                     
004196     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
004197          DELIMITED BY SIZE INTO SSA2                                     
004198     MOVE '  GE' TO GODK-STATUSKODER                                      
004199     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-WDK6 SSA1 SSA2            
004200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
004201     PERFORM IMS-STATUSKONTROLL                                           
004202     .                                                                    
004203     SKIP3                                                                
004204 IMS-GHU-WDK711 SECTION.                                                  
004205                                                                          
004206     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
004207          DELIMITED BY SIZE INTO SSA1                                     
004208     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
004209          DELIMITED BY SIZE INTO SSA2                                     
004210     MOVE '  GE' TO GODK-STATUSKODER                                      
004211     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-AREA-WDK711 SSA1 SSA2         
004212     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
004213     PERFORM IMS-STATUSKONTROLL                                           
004214     .                                                                    
004215     SKIP3                                                                
004216 IMS-GU-WDK711 SECTION.                                                   
004217                                                                          
004218     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
004219          DELIMITED BY SIZE INTO SSA1                                     
004220     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
004221          DELIMITED BY SIZE INTO SSA2                                     
004222     MOVE '  GE' TO GODK-STATUSKODER                                      
004223     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK711 SSA1 SSA2          
004224     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
004225     PERFORM IMS-STATUSKONTROLL                                           
004226     .                                                                    
004227     SKIP3                                                                
004228 IMS-REPL-ARTC11 SECTION.                                                 
004229                                                                          
004230     MOVE '  ' TO GODK-STATUSKODER                                        
004231     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA-WDK6                    
004232     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
004233     PERFORM IMS-STATUSKONTROLL                                           
004234     .                                                                    
004235     EJECT                                                                
004236 IMS-REPL-WDK711 SECTION.                                                 
004237                                                                          
004238     MOVE '  ' TO GODK-STATUSKODER                                        
004239     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-AREA-WDK711                  
004240     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
004241     PERFORM IMS-STATUSKONTROLL                                           
004242     .                                                                    
004243     EJECT                                                                
004244                                                                          
004245 IMS-GU-WDK711-REF SECTION.                                               
004246     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
004247          DELIMITED BY SIZE INTO SSA1                                     
004248     STRING 'WDK711  (IDDC    >=' W-IDDC-MIN-X                            
004249                    '&IDDC    <=' W-IDDC-MAX-X                            
004250                    '&IDDCREF >=' W-IDDC-REF-MIN-X                        
004251                    '&IDDCREF <=' W-IDDC-REF-MAX-X ')'                    
004252          DELIMITED BY SIZE INTO SSA2                                     
004253     MOVE '  GE' TO GODK-STATUSKODER                                      
004254     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK711 SSA1 SSA2          
004255     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
004256     PERFORM IMS-STATUSKONTROLL                                           
004257     .                                                                    
004258     EJECT                                                                
004259 IMS-GU-WDK712   SECTION.                                                 
004260                                                                          
004261     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
004262          DELIMITED BY SIZE INTO SSA1                                     
004263     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
004264          DELIMITED BY SIZE INTO SSA2                                     
004265     MOVE '  GE' TO GODK-STATUSKODER                                      
004266     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK712 SSA1 SSA2          
004267     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
004268     PERFORM IMS-STATUSKONTROLL                                           
004269     .                                                                    
004270     EJECT                                                                
004271 IMS-GHU-WDK712   SECTION.                                                
004272                                                                          
004273     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
004274          DELIMITED BY SIZE INTO SSA1                                     
004275     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
004276          DELIMITED BY SIZE INTO SSA2                                     
004277     MOVE '  GE' TO GODK-STATUSKODER                                      
004278     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-AREA-WDK712 SSA1 SSA2         
004279     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
004280     PERFORM IMS-STATUSKONTROLL                                           
004281     .                                                                    
004282     EJECT                                                                
004283 IMS-GHU-WDK712-BYTES  SECTION.                                           
004284                                                                          
004285     STRING 'WDK701  (IDARTNR  =' W-IDARTNR2-X ')'                        
004286          DELIMITED BY SIZE INTO SSA1                                     
004287     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
004288          DELIMITED BY SIZE INTO SSA2                                     
004289     MOVE '  GE' TO GODK-STATUSKODER                                      
004290     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-AREA-WDK712 SSA1 SSA2         
004291     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
004292     PERFORM IMS-STATUSKONTROLL                                           
004293     .                                                                    
004294     EJECT                                                                
004295 IMS-REPL-WDK712 SECTION.                                                 
004296                                                                          
004297     MOVE '  ' TO GODK-STATUSKODER                                        
004298     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-AREA-WDK712                  
004299     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
004300     PERFORM IMS-STATUSKONTROLL                                           
004301     .                                                                    
004302     EJECT                                                                
004303                                                                          
004304 IMS-GU-LOCB01 SECTION.                                                   
004305     SKIP3                                                                
004306     STRING 'WLLOCB01(IDARTNR  =' W-IDARTNR-X ')'                         
004307          DELIMITED BY SIZE INTO SSA1                                     
004308     MOVE '  GE' TO GODK-STATUSKODER                                      
004309     CALL CBLTDLI USING GU LOCB-PCB DLI-IO-WLLOCB01 SSA1                  
004310     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
004311     PERFORM IMS-STATUSKONTROLL                                           
004312     .                                                                    
004313     EJECT                                                                
004314                                                                          
004315 IMS-ISRT-LOCB01 SECTION.                                                 
004316                                                                          
004317     MOVE 'WLLOCB01 ' TO SSA1                                             
004318     MOVE '  ' TO GODK-STATUSKODER                                        
004319     CALL CBLTDLI USING ISRT LOCB-PCB DLI-IO-WLLOCB01 SSA1                
004320     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
004321     PERFORM IMS-STATUSKONTROLL                                           
004322     .                                                                    
004323     SKIP3                                                                
004324                                                                          
004325 IMS-GHNP-LOCB11 SECTION.                                                 
004326                                                                          
004327     STRING 'WLLOCB11(IDDC     =' W-IDDC-X ')'                            
004328             DELIMITED BY SIZE INTO SSA1                                  
004329     MOVE '  GE' TO GODK-STATUSKODER                                      
004330     CALL CBLTDLI USING GHNP LOCB-PCB DLI-IO-WLLOCB11 SSA1                
004331     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
004332     PERFORM IMS-STATUSKONTROLL                                           
004333                                                                          
004334     EJECT                                                                
004335     .                                                                    
004336                                                                          
004337 IMS-REPL-LOCB11 SECTION.                                                 
004338                                                                          
004339     MOVE '  ' TO GODK-STATUSKODER                                        
004340     CALL CBLTDLI USING REPL LOCB-PCB DLI-IO-WLLOCB11                     
004341     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
004342     PERFORM IMS-STATUSKONTROLL                                           
004343     .                                                                    
004344     SKIP3                                                                
004345 IMS-ISRT-LOCB11 SECTION.                                                 
004346                                                                          
004347     STRING 'WLLOCB01(IDARTNR  =' W-IDARTNR-X ')'                         
004348          DELIMITED BY SIZE INTO SSA1                                     
004349     MOVE 'WLLOCB11 ' TO SSA2                                             
004350     MOVE '  II' TO GODK-STATUSKODER                                      
004351     CALL CBLTDLI USING ISRT LOCB-PCB DLI-IO-WLLOCB11 SSA1 SSA2           
004352     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
004353     PERFORM IMS-STATUSKONTROLL                                           
004354     .                                                                    
004355     EJECT                                                                
004356 IMS-GU-WDK7A SECTION.                                                    
004357     STRING 'WDK7A1  (WDK7A1KY=>' WDK7A1KY-MIN-X                          
004358                    '&WDK7A1KY=<' WDK7A1KY-MAX-X ')'                      
004359          DELIMITED BY SIZE INTO SSA1                                     
004360     MOVE '  GE' TO GODK-STATUSKODER                                      
004361     CALL CBLTDLI USING GU WDK7A-PCB DLI-IO-AREA-WDK7A SSA1               
004362     MOVE WDK7A-STATUS-CODE TO STATUS-WS                                  
004363     PERFORM IMS-STATUSKONTROLL                                           
004364     .                                                                    
004365     SKIP3                                                                
004366 IMS-GN-WDK7A SECTION.                                                    
004367     STRING 'WDK7A1  (WDK7A1KY=>' WDK7A1KY-MIN-X                          
004368                    '&WDK7A1KY=<' WDK7A1KY-MAX-X ')'                      
004369          DELIMITED BY SIZE INTO SSA1                                     
004370     MOVE '  GE' TO GODK-STATUSKODER                                      
004371     CALL CBLTDLI USING GN WDK7A-PCB DLI-IO-AREA-WDK7A SSA1               
004372     MOVE WDK7A-STATUS-CODE TO STATUS-WS                                  
004373     PERFORM IMS-STATUSKONTROLL                                           
004374     .                                                                    
004375     EJECT                                                                
004376                                                                          
004377 IMS-GU-WDB601    SECTION.                                                
004378     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
004379          DELIMITED BY SIZE INTO SSA1                                     
004380     MOVE '  GE' TO GODK-STATUSKODER                                      
004381     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
004382     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
004383     PERFORM IMS-STATUSKONTROLL                                           
004384     IF SEGMENT-SAKNAS                                                    
004385        MOVE SPACE TO DCS-KDDC                                            
004386     END-IF                                                               
004387     .                                                                    
004388     EJECT                                                                
004389 IMS-GHU-WDK611 SECTION.                                                  
004390                                                                          
004391     STRING 'WDK601  (IDARTNR  =' W-IDARTNR2-X ')'                        
004392             DELIMITED BY SIZE INTO SSA1                                  
004393     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
004394             DELIMITED BY SIZE INTO SSA2                                  
004395     MOVE '  GE' TO GODK-STATUSKODER                                      
004396     CALL CBLTDLI  USING GHU WDK6-PCB DLI-IO-AREA-WDK6  SSA1 SSA2         
004397     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
004398     PERFORM IMS-STATUSKONTROLL                                           
004399     SKIP3                                                                
004400     .                                                                    
004401                                                                          
004402 IMS-REPL-WDK611 SECTION.                                                 
004403                                                                          
004404     MOVE '    ' TO GODK-STATUSKODER                                      
004405     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-AREA-WDK6                    
004406     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
004407     PERFORM IMS-STATUSKONTROLL                                           
004408     .                                                                    
004409     EJECT                                                                
004410                                                                          
004411 IMS-STATUSKONTROLL SECTION.                                              
004412                                                                          
004413     SET STATUS-IX TO 1                                                   
004414     SEARCH GODK-STATUS                                                   
004415       AT END                                                             
004416         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
004417         DELIMITED BY SIZE INTO FELTEXT                                   
004418         CALL FELLOG                                                      
004419       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
004420         CONTINUE                                                         
004421     END-SEARCH                                                           
004422     .                                                                    
004423 DB2-SELECT-BYART SECTION.                                                
004424     MOVE 000100904              TO GODK-SQLCODESKODER                    
004425     EXEC SQL SELECT                                                      
004426                  IDARTNR_BYT,                                            
004427                  ADLAGOMR,                                               
004428                  ADGANG,                                                 
004429                  ADPLATS                                                 
004430              INTO                                                        
004431                  :BYART-IDARTNR-BYT,                                     
004432                  :BYART-ADLAGOMR,                                        
004433                  :BYART-ADGANG,                                          
004434                  :BYART-ADPLATS                                          
004435            FROM BYART                                                    
004436            WHERE IDARTNR_BYT = :W-IDARTNR-BYT                            
004437     END-EXEC                                                             
004438     MOVE SQLCODE                TO SQLCODE-WS                            
004439     PERFORM DB2-STATUSKONTROLL                                           
004440     .                                                                    
004441 DB2-UPDATE-BYART    SECTION.                                             
004442     MOVE 000                    TO GODK-SQLCODESKODER                    
004443     EXEC SQL UPDATE BYART                                                
004444        SET ADLAGOMR          = :BYART-ADLAGOMR,                          
004445            ADGANG            = :BYART-ADGANG,                            
004446            ADPLATS           = :BYART-ADPLATS                            
004447        WHERE IDARTNR_BYT     = :W-IDARTNR-BYT                            
004448     END-EXEC                                                             
004449     MOVE SQLCODE                TO SQLCODE-WS                            
004450     PERFORM DB2-STATUSKONTROLL                                           
004451     .                                                                    
004452     EJECT                                                                
004453 DB2-STATUSKONTROLL SECTION.                                              
004454     SKIP2                                                                
004455     SET SQLCODE-IX              TO 1                                     
004456     SEARCH GODK-SQLCODE                                                  
004457       AT END                                                             
004458         CALL FELLOG                                                      
004459        WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS                       
004460           CONTINUE                                                       
004461     END-SEARCH                                                           
004470     .                                                                    
