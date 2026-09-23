000001 ID DIVISION.                                                             
000002 PROGRAM-ID.     W4122200.                                                
000003 AUTHOR.         DADHICH PRERNA/ELAINE SJÖBLOM                            
000004 DATE-WRITTEN.   SEPTEMBER 2021.                                          
000005 DATE-COMPILED.                                                           
000006                                                                          
000007* FUNCTION:                                                               
000008*     PROGRAM CHECKS WORKSHOP ORDERS FROM BACKORDER (WDA5)                
000010*     ALL CLASS 3 WITH KDSTARAD AS 2 (BO)                                 
000011*                                                                         
000012*     THE PROGRAM READS     WDA5                                          
000014*                                                                         
000020     SKIP3                                                                
000021 ENVIRONMENT DIVISION.                                                    
000022 CONFIGURATION SECTION.                                                   
000023 SPECIAL-NAMES.                                                           
000024     CLASS IDSYS4578 IS 'LDC' 'LYN' 'ECO' 'VOU' 'POL' 'ACC'               
000025                        'APA' 'APB' 'APC' 'APD' 'APE' 'APF' 'APG'         
000026                        'APH' 'API' 'APJ'.                                
000030     SKIP2                                                                
000031 INPUT-OUTPUT SECTION.                                                    
000032                                                                          
000033 FILE-CONTROL.                                                            
000034     SELECT W41224           ASSIGN TO      W41222D1.                     
000035     EJECT                                                                
000036 DATA DIVISION.                                                           
000037     SKIP2                                                                
000038 FILE SECTION.                                                            
000039 FD  W41224                                                               
000040     RECORDING F                                                          
000041     BLOCK CONTAINS 0.                                                    
000042 01  W41224-POST   PIC X(26).                                             
000043     EJECT                                                                
000044 WORKING-STORAGE SECTION.                                                 
000045                                                                          
000046 77  IDPGM                       PIC X(8)    VALUE 'W4122200'.            
000047 77  CURR-SECTION                PIC X(16)   VALUE 'MAIN'.                
000048 77  CURR-IMS-SECTION            PIC X(16)   VALUE SPACE.                 
000049 77  JA                          PIC X       VALUE 'J'.                   
000050 77  NEJ                         PIC X       VALUE 'N'.                   
000051 77  IX                          PIC 99      VALUE ZERO.                  
000052 77  TAB-IX                      PIC 9(5)    VALUE ZERO.                  
000053 77  PART-IX                     PIC 9(5)    VALUE ZERO.                  
000054 77  MAX-TVS-IX                  PIC 9(2)    VALUE 16.                    
000055 77  MAX-SALDO-IX                PIC 9(5)    VALUE 5000.                  
000056 77  MAX-IX                      PIC 9(2)    VALUE 08.                    
000057 77  WS-ALLOC-DC                 PIC X(2)    VALUE SPACE.                 
000061 77  WS-TIRFS-CDC                PIC 9(6)    VALUE ZERO.                  
000062 77  WS-TIRFS-LDC                PIC 9(6)    VALUE ZERO.                  
000064 77  WS-RFS-DATE-SW              PIC X(1)  VALUE 'N'.                     
000065 77  RFS-IX                      PIC S9(4)   VALUE +1  COMP SYNC.         
000066 77  MAX-RFS-IX                  PIC S9(4)   VALUE +4  COMP SYNC.         
000067 77  W-KVOKS-DAG                 PIC 9(7)    VALUE ZERO.                  
000068 77  W-KVOKS-BULK                PIC 9(7)    VALUE ZERO.                  
000069 77  W-DISP                      PIC S9(8)   VALUE ZERO.                  
000070 77  WS-KVOKS-TOT                PIC S9(8)   VALUE ZERO.                  
000071 77  WS-KVLS-KVAKS-TOT           PIC S9(8)   VALUE ZERO.                  
000072 77  W-DISP-XDC                  PIC S9(8)   VALUE ZERO.                  
000073 77  W-IDTRP                     PIC X(5)    VALUE SPACE.                 
000074 01  W-KVLEDTIM                  PIC 9(5)    VALUE ZERO.                  
000075 01  FILLER REDEFINES W-KVLEDTIM.                                         
000076     03  FILLER                  PIC 9(3).                                
000077     03  W-LEDTIM-MM             PIC 9(2).                                
000078 01  W-TITRPAVG                  PIC 9(7)  VALUE ZERO.                    
000079 01  FILLER REDEFINES W-TITRPAVG.                                         
000080     03  FILLER                  PIC 9(5).                                
000081     03  W-TITRP-MM              PIC 9(2).                                
000082 77  W-FIRST-CUT-OFF             PIC 9(5)    VALUE ZERO.                  
000083 77  SPARA-IDORDNR               PIC X(7)    VALUE SPACE.                 
000084 77  SPAR-KVART                  PIC 9(7)    VALUE ZERO.                  
000085 77  VERKSTADSORDER              PIC X(2)    VALUE 'PW'.                  
000086 77  BUTIKSORDER                 PIC X(2)    VALUE 'PC'.                  
000087                                                                          
000088 01  W-CDC-CUT-OFF               PIC 9(4)    VALUE ZERO.                  
000089 01  FILLER REDEFINES W-CDC-CUT-OFF.                                      
000090     03  W-CDC-CUT-OFF-TT        PIC 9(2).                                
000091     03  W-CDC-CUT-OFF-MM        PIC 9(2).                                
000092                                                                          
000093 01  W-LDC-CUT-OFF               PIC 9(4)    VALUE ZERO.                  
000094 01  FILLER REDEFINES W-LDC-CUT-OFF.                                      
000095     03  W-LDC-CUT-OFF-TT        PIC 9(2).                                
000096     03  W-LDC-CUT-OFF-MM        PIC 9(2).                                
000097                                                                          
000098 01  W-CUT-OFF-RED.                                                       
000099     03  W-RED-TT                   PIC 9(2)  VALUE ZERO.                 
000100     03  FILLER                     PIC X(1)  VALUE '.'.                  
000101     03  W-RED-MM                   PIC 9(2)  VALUE ZERO.                 
000102     03  FILLER                     PIC X(1)  VALUE SPACE.                
000103                                                                          
000104 01  WS-DAT                      PIC 9(6).                                
000105 01  FILLER REDEFINES WS-DAT.                                             
000106     03 WS-YEAR                  PIC 9(2).                                
000107     03 WS-MONTH                 PIC 9(2).                                
000108     03 WS-DAYS                  PIC 9(2).                                
000109                                                                          
000115 01  WS-TIREPDAT-X.                                                       
000116     03 WS-TIREPDAT              PIC S9(7) COMP-3.                        
000117 77  WS-CURR-AAVVD               PIC 9(5)  VALUE ZERO.                    
000118 77  WS-CURR-AAMMDD              PIC 9(6)  VALUE ZERO.                    
000119                                                                          
000120                                                                          
000121                                                                          
000122 01  TODAYS-DATE                 PIC 9(6)   VALUE ZERO.                   
000123 01  CURRENT-TIME.                                                        
000124     03  CURR-DATE               PIC 9(8).                                
000125     03  CURR-TIME.                                                       
000126         05  CURR-NUM-TIME       PIC 9(8).                                
000127         05  FILLER REDEFINES CURR-NUM-TIME.                              
000128             07  CURR-TTMM       PIC 9(4).                                
000129             07  FILLER          PIC X(4).                                
000130                                                                          
000135 01  SALDO-TABLE.                                                         
000136    03  SALDO-PART-DC-TAB   OCCURS 5000 TIMES.                            
000137     05  TAB-IDARTNR          PIC S9(9)   COMP-3 VALUE ZERO.              
000138     05  TAB-IDDC             PIC X(2)    VALUE SPACE.                    
000139     05  TAB-SALDO            PIC S9(8)   VALUE ZERO.                     
000140                                                                          
000141 01  W41224UT-AREA-START         PIC X(24)   VALUE                        
000141                                 'W41224UT-AREA-START'.                   
000142*01  AREA -COPY W41224     -PRE UT-                                       
000157                                                                          
000158 01  GENERAL-SUBPROGRAMS.                                                 
000159     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
000160     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000161     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000164     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
000165     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
000167     SKIP2                                                                
000174 01  FILLER                      PIC X(16)   VALUE 'WDATKONV'.            
000175*    --- PARAMETRAR FOR SUBPROGRAM WDATKONV                               
000176*01 -COPY WDATAREA                                                        
000177 01  FILLER                      PIC X(16)   VALUE 'WORKDAY '.            
000178*    --- PARAMETRAR FOR SUBPROGRAM WORKDAY                                
000179*01 -COPY WORKAREA                                                        
000180*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
000181                                                                          
000182 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
000183 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
000184 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
000185     SKIP2                                                                
000186 01  ERROR-TEXT.                                                          
000187     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
000188     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
000189                                                                          
000190     EJECT                                                                
000191 01  FILLER                      PIC X(16)   VALUE 'WWDCKONS'.            
000193*01 -COPY WWDCKONS                                                        
000194                                                                          
000195 01  FILLER                      PIC X(16)   VALUE 'WWDC99'.              
000196*01  -COPY WWDC99                                                         
000197                                                                          
000201*    --- AREAS FOR IMS-SECTIONS                                           
000202*                                                                         
000203 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000204                                                                          
000205 01  W-KDORDKL-X.                                                         
000206     03  W-KDORDKL               PIC S9      COMP-3 VALUE 3.              
000207                                                                          
000208 01  W-IDSYSTX3-X.                                                        
000209     03  W-IDSYSTX3              PIC X(3)    VALUE 'LDC'.                 
000210                                                                          
000211 01  W-IDSYSTX3-LYNK-X.                                                   
000212     03  W-IDSYSTX3-LYNK         PIC X(3)    VALUE 'LYN'.                 
000213                                                                          
000214 01  W-KDSTARAD-X.                                                        
000215     03  W-KDSTARAD              PIC X(1)    VALUE '2'.                   
000216                                                                          
000217 01  PART-DC-SW                  PIC X.                                   
000218     88  PART-DC-FOUND                       VALUE 'J'.                   
000219     88  PART-DC-MISSING                     VALUE 'N'.                   
000220                                                                          
000221 01  SALDO-SW                    PIC X.                                   
000222     88  SALDO-FOUND                         VALUE 'J'.                   
000223     88  NO-SALDO                            VALUE 'N'.                   
000224                                                                          
000225 01  CDC-INVBAL-SW               PIC X.                                   
000226     88  CDC-INVBAL-FOUND                    VALUE 'J'.                   
000227     88  NO-CDC-INVBAL                       VALUE 'N'.                   
000228                                                                          
000229 01  NOTE-SW                     PIC X.                                   
000230     88  NOTE-EXISTS                         VALUE 'J'.                   
000231     88  NOTE-MISSING                        VALUE 'N'.                   
000232                                                                          
000233 01  CDC-CUT-OFF-TIME-SW         PIC X.                                   
000234     88  CDC-CUT-OFF-TIME-NOT-ACHIEVED       VALUE 'J'.                   
000235     88  CDC-CUT-OFF-TIME-ACHIEVED           VALUE 'N'.                   
000236                                                                          
000237 01  LDC-CUT-OFF-TIME-SW         PIC X.                                   
000238     88  LDC-CUT-OFF-TIME-NOT-ACHIEVED       VALUE 'J'.                   
000239     88  LDC-CUT-OFF-TIME-ACHIEVED           VALUE 'N'.                   
000240                                                                          
000241 01  KEYS-FOR-DLI.                                                        
000242     03  W-IDARTNR-X.                                                     
000243         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
000244                                                                          
000245     03  W-IDDC-X.                                                        
000246         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
000247                                                                          
000253     03  W-WDA5KEY-MIN-X.                                                 
000254         05  W-A5-IDDISTR-MIN    PIC S9(5)   VALUE ZERO COMP-3.           
000255         05  W-A5-IDKUNDNR-MIN   PIC S9(7)   VALUE ZERO COMP-3.           
000256         05  W-A5-IDKUNDRF-MIN   PIC X(10)   VALUE SPACE.                 
000257         05  W-A5-IDARTNR-MIN    PIC S9(9)   VALUE ZERO COMP-3.           
000258         05  W-A5-IDLOPNR-MIN    PIC S9(3)   VALUE ZERO COMP-3.           
000259                                                                          
000260     03  W-WDA5KEY-MAX-X.                                                 
000261         05  W-A5-IDDISTR-MAX    PIC S9(5)   VALUE ZERO COMP-3.           
000262         05  W-A5-IDKUNDNR-MAX   PIC S9(7)   VALUE ZERO COMP-3.           
000263         05  W-A5-IDKUNDRF-MAX   PIC X(10)   VALUE SPACE.                 
000264         05  W-A5-IDARTNR-MAX    PIC S9(9)   VALUE ZERO COMP-3.           
000265         05  W-A5-IDLOPNR-MAX    PIC S9(3)   VALUE ZERO COMP-3.           
000266                                                                          
000267     03  W-WDA5KEY-X.                                                     
000268         05  W-A5-IDDISTR-KEY    PIC S9(5)   VALUE ZERO COMP-3.           
000269         05  W-A5-IDKUNDNR-KEY   PIC S9(7)   VALUE ZERO COMP-3.           
000270         05  W-A5-IDKUNDRF-KEY   PIC X(10)   VALUE SPACE.                 
000271         05  FILLER REDEFINES W-A5-IDKUNDRF-KEY.                          
000272             07  W-A5-IDORDNR-KEY    PIC 9(5).                            
000273             07  FILLER              PIC X(5).                            
000274         05  W-A5-IDARTNR-KEY    PIC S9(9)   VALUE ZERO COMP-3.           
000275         05  W-A5-IDLOPNR-KEY    PIC S9(3)   VALUE ZERO COMP-3.           
000276                                                                          
000277     03  W-IDGMT-X.                                                       
000278         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
000279         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
000280                                                                          
000281     03  W-4563KEY-X.                                                     
000282         05  W-4563-IDHTYP      PIC X(4)     VALUE '4563'.                
000283         05  FILLER             PIC X(26)    VALUE LOW-VALUE.             
000284     03  W-4564KEY-X.                                                     
000285         05  W-IDGMTREF-4564    PIC X(17)    VALUE SPACE.                 
000286         05  W-IDARTNR-4564     PIC S9(9)    VALUE ZERO COMP-3.           
000287         05  W-IDLOPNR-4564     PIC S9(3)    VALUE ZERO COMP-3.           
000288                                                                          
000289     03  W-WDGXKEY-4433-X.                                                
000290         05  W-4433-IDHTYP      PIC  X(4)    VALUE '4433'.                
000291         05  W-4433-IDDC        PIC  X(2)    VALUE SPACE.                 
000292         05  FILLER             PIC  X(24)   VALUE LOW-VALUE.             
000293*                                                                         
000294     03  W-WDGXKEY-4434-MIN-X.                                            
000295         05  W-4434-IDTRP-MIN    PIC  X(5)   VALUE SPACE.                 
000296         05  W-4434-TITRPAVG-MIN PIC S9(7)   COMP-3 VALUE ZERO.           
000297         05  W-4434-LOW-VALUE    PIC X       VALUE LOW-VALUE.             
000298                                                                          
000299     03  W-WDGXKEY-4434-MAX-X.                                            
000300         05  W-4434-IDTRP-MAX    PIC  X(5)   VALUE SPACE.                 
000301         05  W-4434-TITRPAVG-MAX PIC S9(7)   COMP-3 VALUE 9999.           
000302         05  W-4434-LOW-VALUE    PIC X       VALUE LOW-VALUE.             
000303                                                                          
000307     03  W-WDB501KY-X.                                                    
000308         05  W-IDDC-WDB5         PIC X(2)    VALUE SPACE.                 
000309         05  W-KDFRAKT-WDB5      PIC S9(3)   VALUE ZERO COMP-3.           
000310         05  W-IDDISTR-WDB5      PIC S9(5)   VALUE ZERO COMP-3.           
000311         05  W-IDKUNDNR-WDB5     PIC S9(7) VALUE +9999999 COMP-3.         
000312                                                                          
000313     03  W-WDB301KY-X.                                                    
000314         05  W-IDDC-DC           PIC  X(2)   VALUE '11'.                  
000315         05  W-IDDISTR-DC        PIC S9(5)   VALUE ZERO COMP-3.           
000316         05  W-IDKUNDNR-DC       PIC S9(7)   VALUE ZERO COMP-3.           
000317     03  W-WDB301KY-DEF-X.                                                
000318         05  W-IDDC-DEF          PIC  X(2)   VALUE '11'.                  
000319         05  W-IDDISTR-DC-DEF    PIC S9(5)   VALUE ZERO COMP-3.           
000320         05  W-IDKUNDNR-DC-DEF   PIC S9(7)  VALUE +9999999 COMP-3.        
000352                                                                          
000353*    --- STATUS-KOD FRÅN IMS                                              
000354 01  STATUS-WS                   PIC XX.                                  
000355     88  SEGMENT-FOUND                       VALUE '  '.                  
000356     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
000357     88  SEGMENT-MISSING                     VALUE 'GE'.                  
000358     88  SEGMENT-SLUT                        VALUE 'GB'.                  
000359     SKIP2                                                                
000360 01  GOOD-STATUSCODES.                                                    
000361     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000362     SKIP3                                                                
000363 01  SSA1                        PIC X(800).                              
000364 01  SSA2                        PIC X(160).                              
000365     EJECT                                                                
000366*    --- IMS FUNCTION CODES                                               
000367*01  -COPY W0003                                                          
000368     EJECT                                                                
000369*    ---  DLI INPUT-OUTPUT AREA                                           
000370 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA501'.                      
000371 01  DLI-IO-WDA501.                                                       
000372*    03  -COPY WDA501                                                     
000373                                                                          
000374 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
000375 01  DLI-IO-WDB201.                                                       
000376*    03  -COPY WDB201                                                     
000377                                                                          
000386 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
000387 01  DLI-IO-WDK601.                                                       
000388*    03  -COPY WDK601                                                     
000389                                                                          
000390 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
000391 01  DLI-IO-WDK611.                                                       
000392*    03  -COPY WDK611                                                     
000393                                                                          
000394 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
000395 01  DLI-IO-WDK711.                                                       
000396*    03  -COPY WDK711                                                     
000397                                                                          
000398 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR5  '.                      
000399 01  DLI-IO-WDR5.                                                         
000400*    03   -COPY WDGX4564                                                  
000401 01  FILLER         PIC X(16) VALUE 'WDR101 AREA'.                        
000402 01   DLI-IO-R101.                                                        
000403*     03  -COPY WDGX4433                                                  
000404                                                                          
000405 01  FILLER         PIC X(16) VALUE 'WDR130 AREA'.                        
000406 01   DLI-IO-R130.                                                        
000407*     03  -COPY WDGX4434                                                  
000408                                                                          
000409 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB501'.                      
000410 01  DLI-IO-WDB501.                                                       
000411*    03   -COPY WDB501                                                    
000412 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB301'.                      
000413 01  DLI-IO-WDB301.                                                       
000414*    03   -COPY WDB301                                                    
000415                                                                          
000416 LINKAGE SECTION.                                                         
000417                                                                          
000422*01  -COPY W0008  -PRE WDA5-                                              
000423     05  FILLER                  PIC X.                                   
000424*01  -COPY W0008  -PRE WDB2-                                              
000425     05  FILLER                  PIC X.                                   
000426*01  -COPY W0008  -PRE WDR5-                                              
000427     05  FILLER                  PIC X.                                   
000428*01  -COPY W0008  -PRE WDK6-                                              
000429     05  FILLER                  PIC X.                                   
000432*01  -COPY W0008  -PRE WDK9-                                              
000433     05  FILLER                  PIC X.                                   
000436*01  -COPY W0008  -PRE WDK7-                                              
000437     05  FILLER                  PIC X.                                   
000438*01  -COPY W0008  -PRE WDB5-                                              
000439     05  FILLER                  PIC X.                                   
000440*01  -COPY W0008  -PRE WDR1-                                              
000441     05  FILLER                  PIC X.                                   
000442*01  -COPY W0008  -PRE WDB3-                                              
000443     05  FILLER                  PIC X.                                   
000444     EJECT                                                                
000449                                                                          
000453 PROCEDURE DIVISION  USING WDA5-PCB                                       
000454                           WDB2-PCB WDR5-PCB                              
000455                           WDK6-PCB WDK9-PCB                              
000457                           WDK7-PCB                                       
000458                           WDB5-PCB WDR1-PCB WDB3-PCB.                    
000461                                                                          
000462 MAIN SECTION.                                                            
000473                                                                          
000474     PERFORM A-INIT                                                       
000475                                                                          
000476     MOVE LOW-VALUE  TO W-WDA5KEY-MIN-X                                   
000477     MOVE HIGH-VALUE TO W-WDA5KEY-MAX-X                                   
000478** ONLY THE LDC DISTRICTS IN THE FOLLOWING RANGE TO BE CONSIDERED         
000479** FOR AUTO RELEASE                                                       
000480     MOVE 700        TO W-A5-IDDISTR-MIN                                  
000481     MOVE 6000       TO W-A5-IDDISTR-MAX                                  
000482                                                                          
000483     PERFORM IMS-GN-WDA501                                                
000484                                                                          
000485     PERFORM UNTIL SEGMENT-SLUT OR SEGMENT-MISSING                        
000486       IF RAD-IDSYSTEM (1:3) IS IDSYS4578                                 
000487        IF RAD-KDTPOTYP = +0                                              
000488         PERFORM B-CHECK-NOTE                                             
000489         IF NOTE-MISSING                                                  
000490            MOVE NEJ   TO SALDO-SW                                        
000491            MOVE SPACE TO WS-ALLOC-DC                                     
000492                                                                          
000493            MOVE RAD-IDARTNR    TO W-IDARTNR                              
000494            PERFORM IMS-GU-WDK611                                         
000495                                                                          
000496            IF CLAG-KDFARLIG = 4 OR                                       
000497              (CLAG-KDUART = 'P' OR 'M' OR 'S' OR 'L') OR                 
000498              (CLAG-KDLEVSP = 20 OR 21 OR 22) OR                          
000499              (CLAG-FLAUTREL = 'J' OR 'Y')                                
000500               CONTINUE                                                   
000501            ELSE                                                          
000502                                                                          
000503              MOVE RAD-IDDISTR   TO W-IDDISTR                             
000504              MOVE RAD-IDKUNDNR  TO W-IDKUNDNR                            
000505                                                                          
000506              PERFORM IMS-GU-WDB201                                       
000507                                                                          
000508              PERFORM C-GET-CDC-CUT-OFF-TIME                              
000509                                                                          
000510              IF CDC-CUT-OFF-TIME-NOT-ACHIEVED                            
000511*** CHECK FOR QTY ON CLEARING DC'S ON 4412 SCREEN                         
000512                MOVE +1 TO IX                                             
000513                PERFORM UNTIL IX > MAX-IX OR SALDO-FOUND OR               
000514                              GMT-IDDC-PREPLAN (IX) = SPACE               
000515                  MOVE GMT-IDDC-PREPLAN (IX) TO W-IDDC                    
000516                                               WS-IDDC                    
000517                  PERFORM D-CHECK-SALDO                                   
000518                  IF SALDO-FOUND                                          
000519                    MOVE GMT-IDDC-PREPLAN (IX) TO WS-ALLOC-DC             
000522                    PERFORM S01-WRITE-FILE                                
000523                  END-IF                                                  
000524                  ADD +1 TO IX                                            
000525                END-PERFORM                                               
000526              ELSE                                                        
000527                IF CDC-CUT-OFF-TIME-ACHIEVED                              
000528*** CHECK FROM PREPLANNED QUEUE OF 4412 SCREEN FOR QTY ON CLEARING        
000529*** DCs EXCEPT DC 11                                                      
000530                  PERFORM F-GET-LDC-CUT-OFF-TIME                          
000531                                                                          
000532                  IF LDC-CUT-OFF-TIME-NOT-ACHIEVED                        
000533                    MOVE +1 TO IX                                         
000534                    PERFORM UNTIL IX > MAX-IX OR SALDO-FOUND OR           
000535                                 GMT-IDDC-PREPLAN (IX) = SPACE OR         
000536                                 GMT-IDDC-PREPLAN (IX) = WC-CDC-SE        
000537                      MOVE GMT-IDDC-PREPLAN (IX) TO W-IDDC                
000538                                                   WS-IDDC                
000539                      PERFORM D-CHECK-SALDO                               
000540                      IF SALDO-FOUND                                      
000541                        MOVE GMT-IDDC-PREPLAN (IX) TO WS-ALLOC-DC         
000544                        PERFORM S01-WRITE-FILE                            
000545                      END-IF                                              
000546                      ADD +1 TO IX                                        
000547                    END-PERFORM                                           
000548                  END-IF                                                  
000549                END-IF                                                    
000550              END-IF                                                      
000551            END-IF                                                        
000552         END-IF                                                           
000553        END-IF                                                            
000554       END-IF                                                             
000555       PERFORM IMS-GN-WDA501                                              
000556                                                                          
000557     END-PERFORM                                                          
000558                                                                          
000559     PERFORM Z-FINIT                                                      
000560                                                                          
000561     MOVE ZERO TO RETURN-CODE                                             
000562     GOBACK                                                               
000563     .                                                                    
000564     EJECT                                                                
000565 A-INIT SECTION.                                                          
000566     OPEN OUTPUT W41224                                                   
000567     INITIALIZE SALDO-TABLE                                               
000568     DISPLAY  '                                                  '        
000569              'CDC     LDC    CDC     LDC              '                  
000570     DISPLAY  'DIST   IDKUNDNR IDORDNR  ORDNR-NY IDARTRNR KVAR DC'        
000571              'RFS DT  RFS DT CUT-OFF CUT-OFF CURR-TTMM'                  
000572     ACCEPT TODAYS-DATE FROM DATE                                         
000573                                                                          
000574     ACCEPT CURR-TIME    FROM TIME                                        
000575     ACCEPT WS-DAT       FROM DATE                                        
000576                                                                          
000577     MOVE "AAMMDD"       TO DAT-KDDATFORM                                 
000578     MOVE WS-DAT         TO DAT-I-TIDATUM                                 
000579     CALL WDATKONV USING    DAT-KDDATFORM,                                
000580                            DAT-I-TIDATUM,                                
000581                            DAT-O-TIDATUM,                                
000582                            DAT-KDSVAR                                    
000583                                                                          
000584     IF DAT-KDSVAR-OK                                                     
000585        MOVE DAT-TIAAVVD    TO WS-CURR-AAVVD                              
000586        MOVE DAT-TIAAMMDD   TO WS-CURR-AAMMDD                             
000587        MOVE WS-CURR-AAMMDD TO WS-TIREPDAT                                
000588     END-IF                                                               
000589     .                                                                    
000590                                                                          
000591 B-CHECK-NOTE     SECTION.                                                
000592                                                                          
000593     MOVE NEJ   TO NOTE-SW                                                
000594                                                                          
000595     MOVE RAD-IDGMTREF    TO W-IDGMTREF-4564                              
000596     MOVE RAD-IDARTNR     TO W-IDARTNR-4564                               
000597     MOVE RAD-IDLOPNR     TO W-IDLOPNR-4564                               
000598                                                                          
000599     PERFORM IMS-GU-WDR5                                                  
000600     IF SEGMENT-FOUND AND 4564-BETEXT-010 NOT = SPACE                     
000601        MOVE JA    TO NOTE-SW                                             
000602     END-IF                                                               
000603     .                                                                    
000604     EJECT                                                                
000605                                                                          
000606 D-CHECK-SALDO SECTION.                                                   
000607* CHECK SALDO FIRST IN THE INTERNAL TABLE(UPDATED) AND IF PART/DC         
000608* COMBINATION NOT FOUND THE CHECK SALDO DIRECTLY IN DB                    
000609     PERFORM DA-CHECK-SALDO-TABLE                                         
000610     IF PART-DC-MISSING                                                   
000611       PERFORM DB-CHECK-SALDO-K6-K7                                       
000612     END-IF                                                               
000613     .                                                                    
000614                                                                          
000615 DA-CHECK-SALDO-TABLE SECTION.                                            
000616** IF PART/DC ALREADY EXIST IN TABLE, CHECK SALDO AND UPDATE IT           
000617** ACCORDINGLY                                                            
000618     MOVE NEJ  TO PART-DC-SW                                              
000619     MOVE NEJ  TO CDC-INVBAL-SW                                           
000620     MOVE +1   TO TAB-IX                                                  
000621*                                                                         
000622     PERFORM UNTIL TAB-IX > MAX-SALDO-IX OR SALDO-FOUND OR                
000623                   PART-DC-FOUND OR TAB-IDARTNR (TAB-IX) = ZERO           
000624       IF RAD-IDARTNR = TAB-IDARTNR(TAB-IX) AND                           
000625          WS-IDDC     = TAB-IDDC(TAB-IX)                                  
000626          MOVE JA TO PART-DC-SW                                           
000627          IF TAB-SALDO(TAB-IX) >= RAD-KVART                               
000628            MOVE JA   TO SALDO-SW                                         
000629            COMPUTE TAB-SALDO(TAB-IX) = TAB-SALDO(TAB-IX) -               
000630                                        RAD-KVART                         
000631          END-IF                                                          
000632       END-IF                                                             
000633       ADD +1 TO TAB-IX                                                   
000634     END-PERFORM                                                          
000635     .                                                                    
000636                                                                          
000637 DB-CHECK-SALDO-K6-K7 SECTION.                                            
000638** IF PART/DC DOESN'T EXIST IN INTERNAL TABLE, READ THE CORR. DB          
000639** AND CHECK IF SALDO IS AVAILABLE. ALSO INSERT IN INTERNAL TABLE.        
000640     MOVE RAD-IDARTNR    TO W-IDARTNR                                     
000641*                                                                         
000642     IF CDC                                                               
000643       PERFORM IMS-GU-WDK611                                              
000644                                                                          
000645       IF CLAG-KVUTRS > ZERO OR                                           
000646          CLAG-KVSPARR-KVAL > ZERO                                        
000647*         *Do not release the order                                       
000648          MOVE JA TO CDC-INVBAL-SW                                        
000649          MOVE NEJ TO SALDO-SW                                            
000650       ELSE                                                               
000651         IF CLAG-KVLS >= RAD-KVART                                        
000652            MOVE JA TO SALDO-SW                                           
000653         END-IF                                                           
000654       END-IF                                                             
000655     ELSE                                                                 
000656       PERFORM IMS-GU-WDK711                                              
000657       IF SEGMENT-FOUND                                                   
000658         IF SLAG-KDLEVSP = 20 OR 21 OR 22                                 
000659           CONTINUE                                                       
000660         ELSE                                                             
000661           IF SLAG-KVOKS-DAG < +0                                         
000662             MOVE +0             TO W-KVOKS-DAG                           
000663           ELSE                                                           
000664             MOVE SLAG-KVOKS-DAG TO W-KVOKS-DAG                           
000665           END-IF                                                         
000666           IF SLAG-KVOKS-BULK < +0                                        
000667             MOVE +0              TO W-KVOKS-BULK                         
000668           ELSE                                                           
000669             MOVE SLAG-KVOKS-BULK TO W-KVOKS-BULK                         
000670           END-IF                                                         
000671           COMPUTE WS-KVLS-KVAKS-TOT = SLAG-KVLS +                        
000672                                       SLAG-KVAKS-SDC                     
000673           COMPUTE WS-KVOKS-TOT = W-KVOKS-DAG +                           
000674                                  W-KVOKS-BULK                            
000675           COMPUTE W-DISP-XDC = WS-KVLS-KVAKS-TOT -                       
000676                                WS-KVOKS-TOT                              
000677                                                                          
000678           IF W-DISP-XDC > ZERO                                           
000679             COMPUTE W-DISP-XDC = W-DISP-XDC       -                      
000680                                  SLAG-KVUTRS -                           
000681                                  SLAG-KVSPARR-KVAL                       
000682           END-IF                                                         
000683           IF W-DISP-XDC >= RAD-KVART                                     
000684             MOVE JA TO SALDO-SW                                          
000685           END-IF                                                         
000686         END-IF                                                           
000687       END-IF                                                             
000688     END-IF                                                               
000689                                                                          
000690*IF THE PART/DC MISSING IN INTERNAL TABLE, INSERT IT.                     
000691     IF PART-DC-MISSING AND NO-CDC-INVBAL                                 
000692       PERFORM DBA-INSERT-IDARTNR-TABLE                                   
000693     END-IF                                                               
000694     .                                                                    
000695                                                                          
000696 DBA-INSERT-IDARTNR-TABLE SECTION.                                        
000697* FOR EVERY NEW COMBINATION OF PART/DC INSERT INTO TABLE                  
000698     IF CDC                                                               
000699       COMPUTE W-DISP = CLAG-KVLS                                         
000700     ELSE                                                                 
000701       COMPUTE W-DISP = W-DISP-XDC                                        
000702     END-IF                                                               
000703*                                                                         
000704     ADD +1 TO PART-IX                                                    
000705     MOVE RAD-IDARTNR TO TAB-IDARTNR(PART-IX)                             
000706     MOVE WS-IDDC     TO TAB-IDDC(PART-IX)                                
000707     IF SALDO-FOUND                                                       
000708       COMPUTE TAB-SALDO(TAB-IX) = W-DISP - RAD-KVART                     
000709     ELSE                                                                 
000710       IF W-DISP > ZERO                                                   
000711         MOVE W-DISP    TO TAB-SALDO(TAB-IX)                              
000712       ELSE                                                               
000713         MOVE ZERO      TO TAB-SALDO(TAB-IX)                              
000714       END-IF                                                             
000715     END-IF                                                               
000716     .                                                                    
000717                                                                          
000718 C-GET-CDC-CUT-OFF-TIME SECTION.                                          
000719                                                                          
000720     MOVE NEJ  TO CDC-CUT-OFF-TIME-SW                                     
000721     MOVE ZERO TO W-CDC-CUT-OFF                                           
000722                                                                          
000723     PERFORM CA-RFSDATE-CDC                                               
000724                                                                          
000725     IF WS-TIRFS-CDC = TODAYS-DATE                                        
000726*      *SHALL ONLY RELEASE ORDERS WITH RFS TODAYS DATE                    
000727       PERFORM CB-GET-TRP-AND-TIME                                        
000728       IF SEGMENT-FOUND                                                   
000729          PERFORM CC-GET-DEPARTURE-TIME                                   
000730       END-IF                                                             
000731       IF W-CDC-CUT-OFF = ZERO                                            
000732*      *CUT OFF TIME IS MISSING                                           
000733          MOVE NEJ TO CDC-CUT-OFF-TIME-SW                                 
000734       ELSE                                                               
000735          IF W-CDC-CUT-OFF > CURR-TTMM                                    
000736*      *RELEASE ORDERS UNTIL CDC CUT-OFF TIME ACHIEVED                    
000737             MOVE JA  TO CDC-CUT-OFF-TIME-SW                              
000738          ELSE                                                            
000739             MOVE NEJ TO CDC-CUT-OFF-TIME-SW                              
000740          END-IF                                                          
000741       END-IF                                                             
000742     END-IF                                                               
000743     .                                                                    
000744                                                                          
000745 CA-RFSDATE-CDC SECTION.                                                  
000746                                                                          
000747     MOVE 'N'                      TO WS-RFS-DATE-SW                      
000748     MOVE +003                     TO WORK-KDCALL                         
000749     MOVE 11                       TO WORK-IDDC                           
000750       PERFORM                                                            
000751       VARYING RFS-IX FROM 1 BY 1                                         
000752         UNTIL RFS-IX > MAX-RFS-IX                                        
000753         IF GMT-IDDC-RFS (RFS-IX) = WORK-IDDC                             
000754           MOVE GMT-KVDAGAR-RFS (RFS-IX)                                  
000755                                   TO WORK-KVWORKD                        
000756           MOVE 'Y'                TO WS-RFS-DATE-SW                      
000757         END-IF                                                           
000758       END-PERFORM                                                        
000759       IF WS-RFS-DATE-SW = 'N'                                            
000760          MOVE GMT-KVDAGAR-RFS-DEF  TO WORK-KVWORKD                       
000761       END-IF                                                             
000762       ADD +1  TO WORK-KVWORKD                                            
000763       MOVE RAD-TIREPDAT       TO WORK-TIAAMMDD-TOM                       
000764                                                                          
000765       CALL WORKDAY                USING WORK-KDCALL                      
000766                                         WORK-DATE-AREA                   
000767                                         WORK-KDSVAR                      
000768       MOVE WORK-TIAAMMDD-FOM  TO WS-TIRFS-CDC                            
000769     .                                                                    
000770     EJECT                                                                
000771 CB-GET-TRP-AND-TIME SECTION.                                             
000772                                                                          
000773     MOVE RAD-IDDC             TO W-IDDC-WDB5                             
000774     MOVE RAD-KDFRAKT          TO W-KDFRAKT-WDB5                          
000775     MOVE RAD-IDDISTR          TO W-IDDISTR-WDB5                          
000776     MOVE RAD-IDKUNDNR         TO W-IDKUNDNR-WDB5                         
000777     PERFORM IMS-GU-WDB501                                                
000778     IF SEGMENT-MISSING                                                   
000779        MOVE 9999999           TO W-IDKUNDNR-WDB5                         
000780        PERFORM IMS-GU-WDB501                                             
000781     END-IF                                                               
000782     IF SEGMENT-FOUND                                                     
000783        MOVE FK-IDTRP-3        TO W-IDTRP                                 
000784        MOVE RAD-IDDISTR       TO W-IDDISTR-DC                            
000785                                  W-IDDISTR-DC-DEF                        
000786        MOVE RAD-IDKUNDNR      TO W-IDKUNDNR-DC                           
000787        PERFORM IMS-GU-WDB301                                             
000788        COMPUTE W-KVLEDTIM = DC-KVLEDTIM-3 * 100                          
000789     END-IF                                                               
000790     .                                                                    
000791     EJECT                                                                
000792                                                                          
000793 CC-GET-DEPARTURE-TIME SECTION.                                           
000794                                                                          
000795     MOVE    WC-CDC-SE        TO W-4433-IDDC                              
000796     PERFORM IMS-GU-WDR101                                                
000797     MOVE    W-IDTRP          TO W-4434-IDTRP-MIN                         
000798                                 W-4434-IDTRP-MAX                         
000799     MOVE ZERO                TO W-4434-TITRPAVG-MIN                      
000800                                 W-FIRST-CUT-OFF                          
000801                                 W-CDC-CUT-OFF                            
000802*    In W-WDGXKEY-4434-MAX we have W-4434-TITRPAVG-MAX = 9999             
000803*    as we are only looking for daily transports.                         
000804                                                                          
000805     PERFORM IMS-GNP-WDR130                                               
000806                                                                          
000807     PERFORM UNTIL SEGMENT-MISSING OR                                     
000808              W-CDC-CUT-OFF > 0                                           
000809        MOVE 4434-TITRPAVG  TO W-FIRST-CUT-OFF                            
000810                               W-TITRPAVG                                 
000811                                                                          
000812        IF W-TITRP-MM < W-LEDTIM-MM                                       
000813           ADD 40 TO W-KVLEDTIM                                           
000814        END-IF                                                            
000815                                                                          
000816        COMPUTE W-CDC-CUT-OFF = 4434-TITRPAVG - W-KVLEDTIM                
000817        IF TODAYS-DATE < WS-TIRFS-CDC                                     
000818          MOVE W-CDC-CUT-OFF-TT TO W-RED-TT                               
000819          MOVE W-CDC-CUT-OFF-MM TO W-RED-MM                               
000820        ELSE                                                              
000821          IF CURR-TTMM < W-CDC-CUT-OFF                                    
000822            MOVE W-CDC-CUT-OFF-TT TO W-RED-TT                             
000823            MOVE W-CDC-CUT-OFF-MM TO W-RED-MM                             
000824          ELSE                                                            
000825             MOVE ZERO TO W-CDC-CUT-OFF                                   
000826             PERFORM IMS-GNP-WDR130                                       
000827          END-IF                                                          
000828        END-IF                                                            
000829     END-PERFORM                                                          
000830     .                                                                    
000831     EJECT                                                                
000832                                                                          
001098 F-GET-LDC-CUT-OFF-TIME SECTION.                                          
001099     MOVE 'F-GET-LDC-CUT-  '  TO CURR-SECTION                             
001100                                                                          
001101     MOVE NEJ  TO LDC-CUT-OFF-TIME-SW                                     
001102     MOVE ZERO TO W-LDC-CUT-OFF                                           
001103                                                                          
001104     PERFORM FA-RFSDATE-LDC                                               
001105                                                                          
001106     IF WS-TIRFS-LDC = TODAYS-DATE                                        
001107*      *SHALL ONLY RELEASE ORDERS WITH RFS TODAYS DATE                    
001108       PERFORM FB-GET-TRPID-N-LEAD-DEP-TIME                               
001109       IF SEGMENT-FOUND                                                   
001110          PERFORM FC-GET-DEPARTURE-TIME                                   
001111       END-IF                                                             
001112       IF W-LDC-CUT-OFF = ZERO                                            
001113*      *CUT OFF TIME IS MISSING                                           
001114          MOVE NEJ TO LDC-CUT-OFF-TIME-SW                                 
001115       ELSE                                                               
001116          IF W-LDC-CUT-OFF > CURR-TTMM                                    
001117*      *RELEASE ORDERS UNTIL LDC CUT-OFF TIME ACHIEVED                    
001118             MOVE JA  TO LDC-CUT-OFF-TIME-SW                              
001119          ELSE                                                            
001120             MOVE NEJ TO LDC-CUT-OFF-TIME-SW                              
001121          END-IF                                                          
001122       END-IF                                                             
001123     END-IF                                                               
001124     .                                                                    
001125 FA-RFSDATE-LDC SECTION.                                                  
001126     MOVE 'FA-RFSDATE-LDC  '  TO CURR-SECTION                             
001127                                                                          
001128     MOVE GMT-IDDC-BULK(1)         TO WORK-IDDC                           
001129     MOVE +002                     TO WORK-KDCALL                         
001130     MOVE +001                     TO WORK-KVWORKD                        
001131     MOVE RAD-TIREPDAT             TO WORK-TIAAMMDD-FOM                   
001132     CALL WORKDAY                  USING WORK-KDCALL                      
001133                                         WORK-DATE-AREA                   
001134                                         WORK-KDSVAR                      
001135     IF WORK-KDSVAR-FEL                                                   
001136        MOVE 'SECT F-1, DATUM SAKNAS I WORKDAY'                           
001137                                   TO ERROR-TEXT                          
001138        CALL ABEND                 USING RKOD-ABEND-NO-DUMP               
001139     ELSE                                                                 
001140       MOVE +003                   TO WORK-KDCALL                         
001141       MOVE GMT-KVDAGAR-RFS-DEF    TO WORK-KVWORKD                        
001142       PERFORM                                                            
001143       VARYING RFS-IX FROM 1 BY 1                                         
001144         UNTIL RFS-IX > MAX-RFS-IX                                        
001145         IF GMT-IDDC-RFS (RFS-IX) = WORK-IDDC                             
001146           MOVE GMT-KVDAGAR-RFS (RFS-IX)                                  
001147                                   TO WORK-KVWORKD                        
001148         END-IF                                                           
001149       END-PERFORM                                                        
001150       ADD +1  TO WORK-KVWORKD                                            
001151*      +1 FÖR ATT VARIABELN SKALL KUNNA INNEHÅLLA                         
001152*      ANTAL DAGAR FÖRE RFS.                                              
001153*      0 GER DÅ SAMMA DAG, 1 GER FÖRSTA ARBETSDAG FÖRE OSV...             
001154*      OM VI INTE ADDERAR +1 SKULLE VARIABELN SÄTTAS SÅ                   
001155*      1 GER SAMMA DAG, 2 FÖRSTA ARBETSDAG FÖRE OSV...                    
001156*                                                                         
001157                                                                          
001158       CALL WORKDAY                USING WORK-KDCALL                      
001159                                         WORK-DATE-AREA                   
001160                                         WORK-KDSVAR                      
001161       IF WORK-KDSVAR-FEL                                                 
001162          MOVE 'SECT F-2, DATUM SAKNAS I WORKDAY'                         
001163                                   TO ERROR-TEXT                          
001164          CALL ABEND               USING RKOD-ABEND-NO-DUMP               
001165       ELSE                                                               
001166         IF WORK-TIAAMMDD-FOM < WS-DAT                                    
001167           MOVE GMT-IDDC-BULK(1)   TO WORK-IDDC                           
001168           MOVE +002               TO WORK-KDCALL                         
001169           MOVE +001               TO WORK-KVWORKD                        
001170           MOVE WS-DAT             TO WORK-TIAAMMDD-FOM                   
001171           CALL WORKDAY            USING WORK-KDCALL                      
001172                                         WORK-DATE-AREA                   
001173                                         WORK-KDSVAR                      
001174           IF WORK-KDSVAR-FEL                                             
001175              MOVE 'SECT F-3, DATUM SAKNAS I WORKDAY'                     
001176                                   TO ERROR-TEXT                          
001177              CALL ABEND           USING RKOD-ABEND-NO-DUMP               
001178           ELSE                                                           
001179              MOVE WORK-TIAAMMDD-TOM TO WS-TIRFS-LDC                      
001180           END-IF                                                         
001181         ELSE                                                             
001182           MOVE WORK-TIAAMMDD-FOM  TO WS-TIRFS-LDC                        
001183         END-IF                                                           
001184       END-IF                                                             
001185     END-IF                                                               
001186     .                                                                    
001187     EJECT                                                                
001188                                                                          
001189 FB-GET-TRPID-N-LEAD-DEP-TIME SECTION.                                    
001190     MOVE 'FB-GET-TRPID-  '  TO CURR-SECTION                              
001191                                                                          
001192     MOVE GMT-IDDC-BULK (1)    TO W-IDDC-WDB5                             
001193     MOVE RAD-KDFRAKT          TO W-KDFRAKT-WDB5                          
001194     MOVE RAD-IDDISTR          TO W-IDDISTR-WDB5                          
001195     MOVE RAD-IDKUNDNR         TO W-IDKUNDNR-WDB5                         
001196     PERFORM IMS-GU-WDB501                                                
001197     IF SEGMENT-MISSING                                                   
001198        MOVE 9999999           TO W-IDKUNDNR-WDB5                         
001199        PERFORM IMS-GU-WDB501                                             
001200     END-IF                                                               
001201     IF SEGMENT-FOUND                                                     
001202        IF RAD-KDORDKL = 0                                                
001203           MOVE FK-IDTRP-0             TO W-IDTRP                         
001204           COMPUTE W-KVLEDTIM = DC-KVLEDTIM-0 * 100                       
001205        ELSE                                                              
001206           IF RAD-KDORDKL = 1                                             
001207              MOVE FK-IDTRP-1          TO W-IDTRP                         
001208              COMPUTE W-KVLEDTIM = DC-KVLEDTIM-1 * 100                    
001209           ELSE                                                           
001210              IF RAD-KDORDKL = 2                                          
001211                 MOVE FK-IDTRP-2       TO W-IDTRP                         
001212                 COMPUTE W-KVLEDTIM = DC-KVLEDTIM-2 * 100                 
001213              ELSE                                                        
001214                 IF RAD-KDORDKL = 3                                       
001215                    MOVE FK-IDTRP-3    TO W-IDTRP                         
001216                    COMPUTE W-KVLEDTIM = DC-KVLEDTIM-3 * 100              
001217                 ELSE                                                     
001218                    IF RAD-KDORDKL = 4                                    
001219                       MOVE FK-IDTRP-4 TO W-IDTRP                         
001220                       COMPUTE W-KVLEDTIM = DC-KVLEDTIM-4 * 100           
001221                    END-IF                                                
001222                 END-IF                                                   
001223              END-IF                                                      
001224           END-IF                                                         
001225        END-IF                                                            
001226     END-IF                                                               
001227     .                                                                    
001228     EJECT                                                                
001229                                                                          
001230 FC-GET-DEPARTURE-TIME SECTION.                                           
001231     MOVE 'FC-GET-DEP     ' TO CURR-SECTION                               
001232                                                                          
001233     MOVE    GMT-IDDC-BULK(1) TO W-4433-IDDC                              
001234     PERFORM IMS-GU-WDR101                                                
001235     MOVE    W-IDTRP          TO W-4434-IDTRP-MIN                         
001236                                 W-4434-IDTRP-MAX                         
001237     MOVE ZERO                TO W-4434-TITRPAVG-MIN                      
001238                                 W-FIRST-CUT-OFF                          
001239                                 W-LDC-CUT-OFF                            
001240*    IN W-WDGXKEY-4434-MAX WE HAVE W-4434-TITRPAVG-MAX = 9999             
001241*    AS WE ARE ONLY LOOKING FOR DAILY TRANSPORTS.                         
001242                                                                          
001243     PERFORM IMS-GNP-WDR130                                               
001244                                                                          
001245     PERFORM UNTIL SEGMENT-MISSING OR                                     
001246              W-LDC-CUT-OFF > 0                                           
001247        MOVE 4434-TITRPAVG  TO W-FIRST-CUT-OFF                            
001248                               W-TITRPAVG                                 
001249        IF W-TITRP-MM < W-LEDTIM-MM                                       
001250           ADD 40 TO W-KVLEDTIM                                           
001251        END-IF                                                            
001252                                                                          
001253        COMPUTE W-LDC-CUT-OFF = 4434-TITRPAVG - W-KVLEDTIM                
001254        IF TODAYS-DATE < WS-TIRFS-LDC                                     
001255           MOVE W-LDC-CUT-OFF-TT TO W-RED-TT                              
001256           MOVE W-LDC-CUT-OFF-MM TO W-RED-MM                              
001257        ELSE                                                              
001258           IF CURR-TTMM < W-LDC-CUT-OFF                                   
001259              MOVE W-LDC-CUT-OFF-TT TO W-RED-TT                           
001260              MOVE W-LDC-CUT-OFF-MM TO W-RED-MM                           
001261           ELSE                                                           
001262              MOVE ZERO TO W-LDC-CUT-OFF                                  
001263              PERFORM IMS-GNP-WDR130                                      
001264           END-IF                                                         
001265        END-IF                                                            
001266     END-PERFORM                                                          
001267     .                                                                    
001268     EJECT                                                                
001269                                                                          
001270  S01-WRITE-FILE SECTION.                                                 
001271     DISPLAY   RAD-IDDISTR '/' RAD-IDKUNDNR                               
001272                           '/' RAD-IDORDNR7                               
001273                           '/' SPARA-IDORDNR                              
001274                           '/' RAD-IDARTNR                                
001275                           '/' RAD-kvart                                  
001276                           '/' WS-ALLOC-DC                                
001277                           '/' WS-TIRFS-CDC                               
001278                           '/' WS-TIRFS-LDC                               
001279                           '/' W-CDC-CUT-OFF                              
001280                           '/' W-LDC-CUT-OFF                              
001281                           '/' CURR-TTMM                                  
001282                                                                          
001283     MOVE RAD-IDDISTR  TO UT-IDDISTR                                      
001284     MOVE RAD-IDKUNDNR TO UT-IDKUNDNR                                     
001285     MOVE RAD-IDORDNR5 TO UT-IDKUNDRF                                     
001287     MOVE RAD-IDARTNR  TO UT-IDARTNR                                      
001288     MOVE RAD-IDLOPNR  TO UT-IDLOPNR                                      
001289     MOVE WS-ALLOC-DC  TO UT-IDDC                                         
001290     WRITE W41224-POST FROM UT-W41224                                     
001292     .                                                                    
001293                                                                          
001294 Z-FINIT SECTION.                                                         
001295     CLOSE W41224                                                         
001296     .                                                                    
001297     EJECT                                                                
001298 S99-ABEND SECTION.                                                       
001299                                                                          
001300     CALL ABEND USING RKOD-ABEND                                          
001301     .                                                                    
001302     EJECT                                                                
001303* --- IMS SECTIONS  ---                                                   
001304                                                                          
001305     EJECT                                                                
001306 IMS-GU-WDB201 SECTION.                                                   
001307                                                                          
001308     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
001309          DELIMITED BY SIZE INTO SSA1                                     
001310     MOVE '  ' TO GOOD-STATUSCODES                                        
001311     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
001312     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
001313     PERFORM IMS-STATUSCHECK                                              
001314     .                                                                    
001315 IMS-GN-WDA501 SECTION.                                                   
001316                                                                          
001317     STRING 'WDA501  (WDA501KY=>' W-WDA5KEY-MIN-X                         
001318                    '&WDA501KY=<' W-WDA5KEY-MAX-X                         
001319                    '&KDORDKL = ' W-KDORDKL-X                             
001320                    '&TIREPDAT >' WS-TIREPDAT-X                           
001321                    '&KDSTARAD =' W-KDSTARAD                              
001322                    '&KDORDTYP= ' VERKSTADSORDER                          
001323                    '!WDA501KY=>' W-WDA5KEY-MIN-X                         
001324                    '&WDA501KY=<' W-WDA5KEY-MAX-X                         
001325                    '&KDORDKL = ' W-KDORDKL-X                             
001326                    '&TIREPDAT >' WS-TIREPDAT-X                           
001327                    '&KDSTARAD =' W-KDSTARAD                              
001328                    '&KDORDTYP= ' BUTIKSORDER ')'                         
001329          DELIMITED BY SIZE  INTO SSA1                                    
001330     MOVE '  GEGB'             TO GOOD-STATUSCODES                        
001331     CALL CBLTDLI USING GN WDA5-PCB DLI-IO-WDA501 SSA1                    
001332     MOVE WDA5-STATUS-CODE     TO STATUS-WS                               
001333     PERFORM IMS-STATUSCHECK                                              
001334     .                                                                    
001343 IMS-GU-WDK611 SECTION.                                                   
001344                                                                          
001345     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
001346          DELIMITED BY SIZE INTO SSA1                                     
001347     MOVE 'WDK611' TO SSA2                                                
001348     MOVE '  ' TO GOOD-STATUSCODES                                        
001349     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
001350     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
001351     PERFORM IMS-STATUSCHECK                                              
001352     .                                                                    
001372 IMS-GU-WDK711 SECTION.                                                   
001373     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
001374            DELIMITED BY SIZE INTO SSA1                                   
001375     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
001376            DELIMITED BY SIZE INTO SSA2                                   
001377     MOVE '  GE' TO GOOD-STATUSCODES                                      
001378     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
001379     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
001380     PERFORM IMS-STATUSCHECK                                              
001381     .                                                                    
001382 IMS-GU-WDR5  SECTION.                                                    
001383     STRING 'WDR501  (WDGXKEY  =' W-4563KEY-X ')'                         
001384            DELIMITED BY SIZE INTO SSA1                                   
001385     STRING 'WDGX4564(KY4564   =' W-4564KEY-X ')'                         
001386            DELIMITED BY SIZE INTO SSA2                                   
001387                                                                          
001388     MOVE '  GE' TO GOOD-STATUSCODES                                      
001389     CALL CBLTDLI USING                                                   
001390           GU WDR5-PCB DLI-IO-WDR5 SSA1 SSA2                              
001391     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
001392     PERFORM IMS-STATUSCHECK                                              
001393     .                                                                    
001394                                                                          
001418 IMS-GU-WDR101 SECTION.                                                   
001419                                                                          
001420     STRING  'WDR101  (WDGXKEY  =' W-WDGXKEY-4433-X ')'                   
001421             DELIMITED BY SIZE INTO SSA1                                  
001422     MOVE    '  '               TO GOOD-STATUSCODES                       
001423     CALL    CBLTDLI USING GU WDR1-PCB DLI-IO-R101 SSA1                   
001424     MOVE    WDR1-STATUS-CODE   TO STATUS-WS                              
001425     PERFORM IMS-STATUSCHECK                                              
001426     .                                                                    
001427                                                                          
001428                                                                          
001429 IMS-GNP-WDR130 SECTION.                                                  
001430                                                                          
001431     STRING 'WDR130  (WDGXKEY >=' W-WDGXKEY-4434-MIN-X                    
001432                    '&WDGXKEY <=' W-WDGXKEY-4434-MAX-X ')'                
001433             DELIMITED BY SIZE INTO SSA1                                  
001434     MOVE    '  GE'             TO GOOD-STATUSCODES                       
001435     CALL    CBLTDLI USING GNP WDR1-PCB DLI-IO-R130 SSA1                  
001436     MOVE    WDR1-STATUS-CODE   TO STATUS-WS                              
001437     PERFORM IMS-STATUSCHECK                                              
001438     .                                                                    
001439                                                                          
001440 IMS-GU-WDB501 SECTION.                                                   
001441                                                                          
001442     STRING 'WDB501  (WDB501KY =' W-WDB501KY-X ')'                        
001443          DELIMITED BY SIZE INTO SSA1                                     
001444     MOVE '  GE' TO GOOD-STATUSCODES                                      
001445     CALL CBLTDLI USING GU WDB5-PCB DLI-IO-WDB501 SSA1                    
001446     MOVE WDB5-STATUS-CODE TO STATUS-WS                                   
001447     PERFORM IMS-STATUSCHECK                                              
001448     .                                                                    
001449                                                                          
001450 IMS-GU-WDB301 SECTION.                                                   
001451                                                                          
001452     STRING 'WDB301  (WDB301KY =' W-WDB301KY-X                            
001453                    '!WDB301KY =' W-WDB301KY-DEF-X ')'                    
001454          DELIMITED BY SIZE INTO SSA1                                     
001455     MOVE '    '              TO GOOD-STATUSCODES                         
001456     CALL CBLTDLI USING GU WDB3-PCB DLI-IO-WDB301 SSA1                    
001457     MOVE WDB3-STATUS-CODE    TO STATUS-WS                                
001458     PERFORM IMS-STATUSCHECK                                              
001459     .                                                                    
001460 IMS-STATUSCHECK SECTION.                                                 
001461                                                                          
001462     SET STATUS-IX TO 1                                                   
001463     SEARCH GOOD-STATUS                                                   
001464       AT END                                                             
001465         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
001466           DELIMITED BY SIZE INTO ERROR-TEXT                              
001467         DISPLAY ERROR-TEXT                                               
001468         CALL FELLOG                                                      
001469       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
001470         CONTINUE                                                         
001471     END-SEARCH                                                           
001472     .                                                                    
