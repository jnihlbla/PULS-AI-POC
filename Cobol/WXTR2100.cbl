000001 ID DIVISION.                                                             
000002 PROGRAM-ID.     WXTR2100.                                                
000003 AUTHOR.         ARUP DATTA.                                              
000004 DATE-WRITTEN.   MARCH 2020.                                              
000005 DATE-COMPILED.                                                           
000006                                                                          
000007                                                                          
000008*    FUNCTION:                                                            
000009*        READ WDR5 (WDGX2224) AND CREATE EXTRACT FILE                     
000010*        FOR ALL ALARMS CREATED DURING THE WEEK                           
000011*                                                                         
000012*        READ WDD4 AND CREATE EXTRACT FILE FOR ALL ALARMS                 
000013*        CREATED DURING THE WEEK                                          
000014*                                                                         
000015*        READ WDR3 AND CREATE EXTRACT FILE FOR ALL ALARMS                 
000016*        THAT WERE CREATED AND DELETED DURING THE WEEK                    
000017*                                                                         
000018*        READ INPUT LOG FILE FOR ALARMS CREATED AND DELETED               
000019*        DURING THE CURRENT WEEK AND WRITE TO EXTRACT FILE                
000020*                                                                         
000021*        CREATE LOG FILE TO CLEANUP WDR3 FOR THE EXTRACTED                
000022*        INFORMATION                                                      
000023*                                                                         
000024                                                                          
000025 ENVIRONMENT DIVISION.                                                    
000026                                                                          
000027 INPUT-OUTPUT SECTION.                                                    
000028                                                                          
000029 FILE-CONTROL.                                                            
000030                                                                          
000031*          --- INPUT LOG FILE WITH DELETED ALARMS                         
000032     SELECT WXTR21                     ASSIGN TO WXTR21D1.                
000033*          --- FILE EXTRACT ALARMS WDR5                                   
000034     SELECT WXTR21A                    ASSIGN TO WXTR21D2.                
000035*          --- FILE EXTRACT ALARMS WDD4                                   
000036     SELECT WXTR21B                    ASSIGN TO WXTR21D3.                
000037*          --- LOG FILE FOR WDR3 CLEAN-UP                                 
000038     SELECT WXTR22                     ASSIGN TO WXTR21D4.                
000039     EJECT                                                                
000040 DATA DIVISION.                                                           
000041     SKIP3                                                                
000042 FILE SECTION.                                                            
000043                                                                          
000044 FD  WXTR21                                                               
000045     RECORDING       F                                                    
000046     BLOCK CONTAINS  0.                                                   
000047*01  POST -COPY W214ALOG -PRE  IN1-     -L.                               
000048     EJECT                                                                
000049 FD  WXTR21A                                                              
000050     RECORDING       F                                                    
000051     BLOCK CONTAINS  0.                                                   
000052*01  POST -COPY WXTR21A  -PRE  UT1-     -L.                               
000053     EJECT                                                                
000054 FD  WXTR21B                                                              
000055     RECORDING       F                                                    
000056     BLOCK CONTAINS  0.                                                   
000057*01  POST -COPY WXTR21B  -PRE  UT2-     -L.                               
000058     EJECT                                                                
000059 FD  WXTR22                                                               
000060     RECORDING       F                                                    
000061     BLOCK CONTAINS  0.                                                   
000062*01  POST -COPY WDR301   -PRE  BORT-    -L.                               
000063     EJECT                                                                
000064 WORKING-STORAGE SECTION.                                                 
000065*    -- CHECKED BY WY2000                                                 
000066                                                                          
000067 77  IDPGM                       PIC X(8)    VALUE 'WXTR2100'.            
000068 77  JA                          PIC X       VALUE 'J'.                   
000069 77  NEJ                         PIC X       VALUE 'N'.                   
000070 77  WS-IDARTNR                  PIC 9(9)    VALUE ZERO.                  
000071 77  WS-2171-TEORSLRM            PIC X(25)   VALUE SPACE.                 
000072 77  WS-2172-TEORSLRM            PIC X(25)   VALUE SPACE.                 
000073 77  WS-TEST-KDLARM-2224         PIC S9(3)   VALUE ZERO COMP-3.           
000074 77  WS-TEST-KDLARM-D401         PIC S9(3)   VALUE ZERO COMP-3.           
000075     EJECT                                                                
000076 77  WXTR21-EOF-SW               PIC X       VALUE 'N'.                   
000077     88  END-OF-WXTR21                       VALUE 'J'.                   
000078     EJECT                                                                
000079 01  FELTEXT.                                                             
000080     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
000081     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
000082                                                                          
000083 01  WS-ALARM-AAVV               PIC 9(4)    VALUE ZERO.                  
000084 01  DAGENS-AAVV                 PIC 9(4)    VALUE ZERO.                  
000085 01  FILLER REDEFINES DAGENS-AAVV.                                        
000086     03  DAGENS-AA               PIC 9(2).                                
000087     03  DAGENS-VV               PIC 9(2).                                
000088     EJECT                                                                
000089 01  WS-DAREGDAT                 PIC 9(8).                                
000090 01  FILLER REDEFINES WS-DAREGDAT.                                        
000091     03 FILLER                   PIC 9(2).                                
000092     03 WS-TIREGDAT              PIC 9(6).                                
000093     EJECT                                                                
000094 01  WS-ALARM-DESC-2224.                                                  
000095     03  TEORSLRM-1            PIC X(25)                                  
000096                               VALUE 'TPO-ALERT               '.          
000097     03  TEORSLRM-2            PIC X(25)                                  
000098                               VALUE 'PRO FORMA               '.          
000099     03  TEORSLRM-3            PIC X(25)                                  
000100                               VALUE 'IR ENTERED              '.          
000101     03  TEORSLRM-4            PIC X(25)                                  
000102                               VALUE 'IR CHANGED              '.          
000103     03  TEORSLRM-5            PIC X(25)                                  
000104                               VALUE 'IR DELETED              '.          
000105     03  TEORSLRM-6            PIC X(25)                                  
000106                               VALUE 'INVESTIGATION BALANCE   '.          
000107     03  TEORSLRM-7            PIC X(25)                                  
000108                               VALUE 'BACKORDER               '.          
000109     03  TEORSLRM-8            PIC X(25)                                  
000110                               VALUE 'CORE BALANCE LEVEL      '.          
000111     03  TEORSLRM-9            PIC X(25)                                  
000112                               VALUE 'DELIVERY INFO EXPIRED   '.          
000113     03  TEORSLRM-10           PIC X(25)                                  
000114                               VALUE 'BELOW SAFETY STOCK      '.          
000115     03  TEORSLRM-11           PIC X(25)                                  
000116                               VALUE 'VOR-QUEUE               '.          
000117     03  TEORSLRM-12           PIC X(25)                                  
000118                               VALUE 'PUBLICATION WEEK CHANGED'.          
000119     03  TEORSLRM-223          PIC X(25)                                  
000120                               VALUE 'NOT SUFFICIENT CALL OFFS'.          
000121     03  TEORSLRM-601          PIC X(25)                                  
000122                               VALUE 'EOP UPD FRM KDP - PG 15 '.          
000123     03  TEORSLRM-610          PIC X(25)                                  
000124                               VALUE 'SS CODE REMOVED         '.          
000125     03  TEORSLRM-708          PIC X(25)                                  
000126                               VALUE 'SI+:PURCH.REQ.REJECTED  '.          
000127     03  TEORSLRM-710          PIC X(25)                                  
000128                               VALUE 'SI+:CANCELLATION REJECTED'.         
000129     03  TEORSLRM-712          PIC X(25)                                  
000130                               VALUE 'SI+:ALREADY ON ORDER     '.         
000131     03  TEORSLRM-720          PIC X(25)                                  
000132                               VALUE 'SI+:PURCH REJECTED REQ.  '.         
000133     03  TEORSLRM-761          PIC X(25)                                  
000134                               VALUE 'SUPPL IS MISSING ON 2114 '.         
000135     03  TEORSLRM-777          PIC X(25)                                  
000136                               VALUE 'NAP:CANCELLATION 6 WEEKS '.         
000137     03  TEORSLRM-240          PIC X(25)                                  
000138                               VALUE 'DDGS 1.0 COULD NOT BE UPD'.         
000139     03  TEORSLRM-999          PIC X(25)                                  
000140                               VALUE 'BLOCKED DEL. MOVE NOT OK '.         
000141     EJECT                                                                
000142 01  WS-ALARM-DESC-D401.                                                  
000143     03  TEORSLRM-220          PIC X(25)                                  
000144                               VALUE 'PREADVICE DEVIATION      '.         
000145     03  TEORSLRM-225          PIC X(25)                                  
000146                               VALUE 'NOT ARRIVED              '.         
000147     03  TEORSLRM-226          PIC X(25)                                  
000148                               VALUE 'QUANTITY DEVIATION       '.         
000149     03  TEORSLRM-230          PIC X(25)                                  
000150                               VALUE 'TOO EARLY                '.         
000151     03  TEORSLRM-235          PIC X(25)                                  
000152                               VALUE 'NOT SCHEDULED            '.         
000153     EJECT                                                                
000154 01  DYNAMISKA-SUBPROGRAM.                                                
000155     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000156     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000157     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
000158     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
000159     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
000160     EJECT                                                                
000161*    --- PARAMETRAR TILL DATKORT                                          
000162 01  PROGRAM-NAMN                PIC X(6)    VALUE 'WXTR21'.              
000163     SKIP2                                                                
000164 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
000165     SKIP2                                                                
000166*01  -COPY WDATKORT                                                       
000167     EJECT                                                                
000168*    --- PARAMETRAR TILL POSTSUM                                          
000169*01  -COPY W0005   -PRE  POSTSUM-                                         
000170*                                                                         
000171     EJECT                                                                
000172*    --- PARAMETRAR TILL WDATKONV                                         
000173*01 -COPY WDATAREA                                                        
000174     EJECT                                                                
000175*    --- COPYBOOK WITH VALID DC CODES                                     
000176*01 -COPY WWDC99                                                          
000177     EJECT                                                                
000178*    --- AREA FOR LOG DATA                                                
000179*01  AREA -COPY W214ALOG    -PRE LOG-                                     
000180     EJECT                                                                
000181 01  IN-AREA-START               PIC X(16)   VALUE                        
000182                                             'IN-AREA-START'.             
000183*01  AREA -COPY W214ALOG    -PRE IN1-                                     
000184     EJECT                                                                
000185 01  UT-AREA-START               PIC X(16)   VALUE                        
000186                                             'UT-AREA-START'.             
000187*01  AREA -COPY WXTR21A     -PRE UT1-                                     
000188     EJECT                                                                
000189*01  AREA -COPY WXTR21B     -PRE UT2-                                     
000190     EJECT                                                                
000191 01  BORT-AREA-START             PIC X(16)   VALUE                        
000192                                             'BORT-AREA-START'.           
000193*01  AREA -COPY WDR301      -PRE BORT-                                    
000194     EJECT                                                                
000195 01  NYCKLAR-TILL-DLI.                                                    
000196     03  W-WDR301KY-X.                                                    
000197         05  W-WDR301KY          PIC X(27)    VALUE SPACE.                
000198     03  W-IDPGM-2171-X.                                                  
000199         05  W-IDPGM-2171        PIC X(08)    VALUE 'W2017100'.           
000200     03  W-IDPGM-2172-X.                                                  
000201         05  W-IDPGM-2172        PIC X(08)    VALUE 'W2017200'.           
000202     03  W-IDHTYP-X.                                                      
000203         05  W-IDHTYP            PIC X(4)     VALUE '2223'.               
000206     03  W-IDARTNR-X.                                                     
000207         05  W-IDARTNR           PIC S9(9)    VALUE ZERO  COMP-3.         
000208                                                                          
000209*    --- STATUS-KOD FRÅN IMS                                              
000210 01  STATUS-WS                   PIC XX.                                  
000211     88  SEGMENT-FINNS                       VALUE '  '.                  
000212     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000213     88  SEGMENT-SLUT                        VALUE 'GB'.                  
000214     88  IMS-EJ-OK                           VALUE 'XD'.                  
000215                                                                          
000216 01  GODK-STATUSKODER.                                                    
000217     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000218 01  SSA1                        PIC X(96).                               
000219 01  SSA2                        PIC X(64).                               
000220     EJECT                                                                
000221*    --- IMS FUNKTIONSKODER                                               
000222*01  -COPY W0003                                                          
000223     EJECT                                                                
000224*    ---  DLI INPUT-OUTPUT AREA                                           
000225 01  FILLER           PIC X(24) VALUE 'DLI-IO-WDR501-2223'.               
000226 01  DLI-IO-WDR501-2223.                                                  
000227*  03  -COPY WDGX2223  -PRE XXBU-                                         
000228     EJECT                                                                
000229*                                                                         
000230 01  FILLER           PIC X(24) VALUE 'DLI-IO-WDGX2224'.                  
000231 01  DLI-IO-WDGX2224.                                                     
000232*  03  -COPY WDGX2224  -PRE XXBU-                                         
000233     EJECT                                                                
000234*                                                                         
000235 01  FILLER           PIC X(16) VALUE 'DLI-IO-WDD401'.                    
000236 01  DLI-IO-WDD401.                                                       
000237*    03  -COPY WDD401                                                     
000238     EJECT                                                                
000239*                                                                         
000240 01  FILLER           PIC X(16) VALUE 'DLI-IO-WDR301'.                    
000241 01  DLI-IO-WDR301.                                                       
000242*  03  -COPY WDR301                                                       
000243     EJECT                                                                
000244*                                                                         
000245 01  FILLER           PIC X(16) VALUE 'DLI-IO-WDK601'.                    
000246 01  DLI-IO-WDK601.                                                       
000247*  03  -COPY WDK601                                                       
000248     EJECT                                                                
000249 LINKAGE SECTION.                                                         
000250*01  -COPY W0008  -PRE WDR5-                                              
000251     05  FILLER                  PIC X.                                   
000252*01  -COPY W0008  -PRE WDD4-                                              
000253     05  FILLER                  PIC X.                                   
000254*01  -COPY W0008  -PRE WDR3-                                              
000255     05  FILLER                  PIC X.                                   
000256*01  -COPY W0008  -PRE WDK6-                                              
000257     05  FILLER                  PIC X.                                   
000258     EJECT                                                                
000259 PROCEDURE DIVISION  USING WDR5-PCB WDD4-PCB WDR3-PCB WDK6-PCB.           
000260 MAIN SECTION.                                                            
000261     ENTRY 'DLITCBL' USING WDR5-PCB WDD4-PCB WDR3-PCB WDK6-PCB.           
000262                                                                          
000263     PERFORM A-INIT                                                       
000264                                                                          
000265     PERFORM B-PROCESS-ALARM-WDR5                                         
000266     PERFORM C-PROCESS-ALARM-WDD4                                         
000267     PERFORM D-PROCESS-ALARM-WDR3                                         
000268     PERFORM E-PROCESS-INP-ALARM-LOG                                      
000269                                                                          
000270                                                                          
000271     PERFORM Z-FINIT                                                      
000272     MOVE ZERO TO RETURN-CODE                                             
000273     GOBACK                                                               
000274     .                                                                    
000275     EJECT                                                                
000276 A-INIT SECTION.                                                          
000277                                                                          
000278     OPEN INPUT  WXTR21                                                   
000279     OPEN OUTPUT WXTR21A                                                  
000280                 WXTR21B                                                  
000281                 WXTR22                                                   
000282     MOVE IDPGM              TO POSTSUM-PROGNAMN                          
000283                                                                          
000284     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
000285     MOVE D-AAR    TO DAGENS-AA                                           
000286     MOVE D-VECKA  TO DAGENS-VV                                           
000287     .                                                                    
000288     EJECT                                                                
000289                                                                          
000290 B-PROCESS-ALARM-WDR5 SECTION.                                            
000291                                                                          
000292     PERFORM IMS-GET-XXBU-ROT                                             
000293     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
000294        PERFORM IMS-GNP-WDGX2224                                          
000295        PERFORM UNTIL SEGMENT-SAKNAS                                      
000296          MOVE XXBU-2224-TIREGDAT         TO DAT-I-TIDATUM                
000297                                                                          
000298          PERFORM S51-CONVERT-DATE-FMT                                    
000299          IF  DAT-KDSVAR = ' '                                            
000300             MOVE DAT-TIAAVV-GRP          TO WS-ALARM-AAVV                
000301             IF WS-ALARM-AAVV  = DAGENS-AAVV                              
000302                MOVE XXBU-2224-KDLARM     TO UT1-EXTA-KDLARM              
000303                MOVE XXBU-2224-IDARTNR    TO UT1-EXTA-IDARTNR             
000304                MOVE XXBU-2224-IDDC       TO UT1-EXTA-IDDC                
000305                MOVE XXBU-2224-IDDISTR    TO UT1-EXTA-IDDISTR             
000306                MOVE XXBU-2224-TIREGDAT   TO UT1-EXTA-TIREGDAT            
000307                MOVE XXBU-2223-IDANSK     TO UT1-EXTA-IDANSK              
000308                MOVE XXBU-2224-IDLEVNR    TO UT1-EXTA-IDLEVNR             
000309                PERFORM BA-CUSTOMIZE-LARM-INFO                            
000310                PERFORM S11-SKRIV-WXTR21A                                 
000311             END-IF                                                       
000312          END-IF                                                          
000313          PERFORM IMS-GNP-WDGX2224                                        
000314        END-PERFORM                                                       
000315        PERFORM IMS-GET-XXBU-ROT                                          
000316     END-PERFORM                                                          
000317     .                                                                    
000318     EJECT                                                                
000319                                                                          
000320 BA-CUSTOMIZE-LARM-INFO SECTION.                                          
000321                                                                          
000322     MOVE XXBU-2224-TISENBEK-DAG    TO DAT-I-TIDATUM                      
000323     PERFORM S51-CONVERT-DATE-FMT                                         
000324     IF DAT-KDSVAR = ' '                                                  
000325        MOVE DAT-TIAAVVD            TO UT1-EXTA-TIAAVVD                   
000326     ELSE                                                                 
000327        MOVE ZERO                   TO UT1-EXTA-TIAAVVD                   
000328     END-IF                                                               
000329*                                                                         
000330***  GET ALARM DESCRIPTION                                                
000331     MOVE XXBU-2224-KDLARM          TO WS-TEST-KDLARM-2224                
000332     PERFORM S21-GET-ALARM-DESC                                           
000333                                                                          
000334     MOVE WS-2171-TEORSLRM          TO UT1-EXTA-TEORSLRM                  
000335*                                                                         
000336***  GET SUPPLIER FROM WDK6                                               
000337     MOVE XXBU-2224-IDARTNR         TO W-IDARTNR                          
000338     PERFORM S52-GET-SUPPLIER-K6                                          
000339     .                                                                    
000340     EJECT                                                                
000341                                                                          
000342 C-PROCESS-ALARM-WDD4 SECTION.                                            
000343     PERFORM IMS-GET-WDD401                                               
000344     PERFORM UNTIL SEGMENT-SAKNAS    OR SEGMENT-SLUT                      
000345       COMPUTE WS-DAREGDAT = 99999999 - LAK-DAREGDAT-9KOMPL               
000346       MOVE WS-TIREGDAT              TO DAT-I-TIDATUM                     
000347                                                                          
000348       PERFORM S51-CONVERT-DATE-FMT                                       
000349       IF DAT-KDSVAR = ' '                                                
000350          MOVE DAT-TIAAVV-GRP        TO WS-ALARM-AAVV                     
000351          IF WS-ALARM-AAVV = DAGENS-AAVV                                  
000352             MOVE LAK-KDLARM         TO UT2-EXTB-KDLARM                   
000353             MOVE LAK-IDARTNR        TO UT2-EXTB-IDARTNR                  
000354             MOVE LAK-IDDC           TO UT2-EXTB-IDDC                     
000355             MOVE LAK-IDANSK         TO UT2-EXTB-IDANSK                   
000356             MOVE LAK-IDLEVNR        TO UT2-EXTB-IDLEVNR                  
000357             MOVE WS-TIREGDAT        TO UT2-EXTB-TIREGDAT                 
000358             MOVE LAK-TIAAMMDD       TO UT2-EXTB-TIPLANDAT                
000359             MOVE LAK-KVAVIS         TO UT2-EXTB-KVAVIS                   
000360             MOVE LAK-KVAVROP        TO UT2-EXTB-KVAVROP                  
000361*                                                                         
000362             MOVE LAK-KDLARM         TO WS-TEST-KDLARM-D401               
000363             PERFORM S21-GET-ALARM-DESC                                   
000364             MOVE WS-2172-TEORSLRM   TO UT2-EXTB-TEORSLRM                 
000365*                                                                         
000366             PERFORM S12-SKRIV-WXTR21B                                    
000367          END-IF                                                          
000368       END-IF                                                             
000369       PERFORM IMS-GET-WDD401                                             
000370     END-PERFORM                                                          
000371     .                                                                    
000372     EJECT                                                                
000373                                                                          
000374 D-PROCESS-ALARM-WDR3  SECTION.                                           
000375                                                                          
000376     PERFORM IMS-GET-FILC-ROT                                             
000377     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
000378                                                                          
000379        MOVE FIL-WDR301-DATA         TO LOG-AREA                          
000380        EVALUATE FIL-IDPGM                                                
000381          WHEN 'W2017100'                                                 
000382             MOVE LOG-ALOG-KDLARM      TO UT1-EXTA-KDLARM                 
000383             MOVE LOG-ALOG-IDARTNR     TO UT1-EXTA-IDARTNR                
000384             MOVE LOG-ALOG-IDANSK      TO UT1-EXTA-IDANSK                 
000385             MOVE LOG-ALOG-IDLEVNR     TO UT1-EXTA-IDLEVNR                
000386             MOVE LOG-ALOG-TIAAVVD     TO UT1-EXTA-TIAAVVD                
000387             MOVE LOG-ALOG-IDDISTR     TO UT1-EXTA-IDDISTR                
000388             MOVE LOG-ALOG-TIREGDAT    TO UT1-EXTA-TIREGDAT               
000389*                                                                         
000390             MOVE LOG-ALOG-KDLARM      TO WS-TEST-KDLARM-2224             
000391             PERFORM S21-GET-ALARM-DESC                                   
000392             MOVE WS-2171-TEORSLRM     TO UT1-EXTA-TEORSLRM               
000393*                                                                         
000394             MOVE LOG-ALOG-IDARTNR     TO W-IDARTNR                       
000395             PERFORM S52-GET-SUPPLIER-K6                                  
000396*                                                                         
000397             PERFORM S11-SKRIV-WXTR21A                                    
000398          WHEN 'W2017200'                                                 
000399             MOVE LOG-ALOG-KDLARM      TO UT2-EXTB-KDLARM                 
000400             MOVE LOG-ALOG-IDARTNR     TO UT2-EXTB-IDARTNR                
000401             MOVE LOG-ALOG-IDANSK      TO UT2-EXTB-IDANSK                 
000402             MOVE LOG-ALOG-IDLEVNR     TO UT2-EXTB-IDLEVNR                
000403             MOVE LOG-ALOG-TIREGDAT    TO UT2-EXTB-TIREGDAT               
000404             MOVE LOG-ALOG-TIPLANDAT   TO UT2-EXTB-TIPLANDAT              
000405             MOVE LOG-ALOG-KVAVIS      TO UT2-EXTB-KVAVIS                 
000406             MOVE LOG-ALOG-KVAVROP     TO UT2-EXTB-KVAVROP                
000407*                                                                         
000408             MOVE LOG-ALOG-KDLARM      TO WS-TEST-KDLARM-D401             
000409             PERFORM S21-GET-ALARM-DESC                                   
000410             MOVE WS-2172-TEORSLRM     TO UT2-EXTB-TEORSLRM               
000411             PERFORM S12-SKRIV-WXTR21B                                    
000412        END-EVALUATE                                                      
000413*                                                                         
000414        MOVE FIL-WDR301              TO BORT-FIL-WDR301                   
000415        PERFORM S13-SKRIV-WXTR22                                          
000416                                                                          
000417        PERFORM IMS-GET-FILC-ROT                                          
000418     END-PERFORM                                                          
000419     .                                                                    
000420     EJECT                                                                
000421 E-PROCESS-INP-ALARM-LOG SECTION.                                         
000422                                                                          
000423     PERFORM S01-LAES-WXTR21                                              
000424     PERFORM UNTIL END-OF-WXTR21                                          
000425        EVALUATE IN1-ALOG-IDSYSTEM                                        
000426          WHEN 'WDR5'                                                     
000427             MOVE IN1-ALOG-KDLARM      TO UT1-EXTA-KDLARM                 
000428             MOVE IN1-ALOG-IDARTNR     TO UT1-EXTA-IDARTNR                
000429             MOVE IN1-ALOG-IDANSK      TO UT1-EXTA-IDANSK                 
000430             MOVE IN1-ALOG-IDLEVNR     TO UT1-EXTA-IDLEVNR                
000431             MOVE IN1-ALOG-TIAAVVD     TO UT1-EXTA-TIAAVVD                
000432             MOVE IN1-ALOG-IDDISTR     TO UT1-EXTA-IDDISTR                
000433             MOVE IN1-ALOG-TIREGDAT    TO UT1-EXTA-TIREGDAT               
000434*                                                                         
000435             MOVE IN1-ALOG-KDLARM      TO WS-TEST-KDLARM-2224             
000436             PERFORM S21-GET-ALARM-DESC                                   
000437             MOVE WS-2171-TEORSLRM     TO UT1-EXTA-TEORSLRM               
000438*                                                                         
000439             MOVE IN1-ALOG-IDARTNR     TO W-IDARTNR                       
000440             PERFORM S52-GET-SUPPLIER-K6                                  
000441*                                                                         
000442             PERFORM S11-SKRIV-WXTR21A                                    
000443          WHEN 'WDD4'                                                     
000444             MOVE IN1-ALOG-KDLARM      TO UT2-EXTB-KDLARM                 
000445             MOVE IN1-ALOG-IDARTNR     TO UT2-EXTB-IDARTNR                
000446             MOVE IN1-ALOG-IDANSK      TO UT2-EXTB-IDANSK                 
000447             MOVE IN1-ALOG-IDLEVNR     TO UT2-EXTB-IDLEVNR                
000448             MOVE IN1-ALOG-TIREGDAT    TO UT2-EXTB-TIREGDAT               
000449             MOVE IN1-ALOG-TIPLANDAT   TO UT2-EXTB-TIPLANDAT              
000450             MOVE IN1-ALOG-KVAVIS      TO UT2-EXTB-KVAVIS                 
000451             MOVE IN1-ALOG-KVAVROP     TO UT2-EXTB-KVAVROP                
000452*                                                                         
000453             MOVE IN1-ALOG-KDLARM      TO WS-TEST-KDLARM-D401             
000454             PERFORM S21-GET-ALARM-DESC                                   
000455             MOVE WS-2172-TEORSLRM     TO UT2-EXTB-TEORSLRM               
000456             PERFORM S12-SKRIV-WXTR21B                                    
000457        END-EVALUATE                                                      
000458        PERFORM S01-LAES-WXTR21                                           
000459     END-PERFORM                                                          
000460     .                                                                    
000461     EJECT                                                                
000462 Z-FINIT SECTION.                                                         
000463                                                                          
000464     CLOSE WXTR21                                                         
000465           WXTR21A                                                        
000466           WXTR21B                                                        
000467           WXTR22                                                         
000468                                                                          
000469     MOVE 'S' TO POSTSUM-OPKOD                                            
000470     CALL POSTSUM USING POSTSUM-PARM                                      
000471     .                                                                    
000472     EJECT                                                                
000473 S01-LAES-WXTR21  SECTION.                                                
000474     SKIP2                                                                
000475     READ WXTR21          INTO IN1-AREA                                   
000476     AT END                                                               
000477        MOVE HIGH-VALUE     TO IN1-AREA                                   
000478        SET END-OF-WXTR21   TO TRUE                                       
000479     NOT AT END                                                           
000480        MOVE 'WXTR21'       TO POSTSUM-FDNAMN                             
000481        MOVE 'WXTR21D1'     TO POSTSUM-DDNAMN2                            
000482        CALL POSTSUM USING POSTSUM-PARM                                   
000483     END-READ                                                             
000484     .                                                                    
000485     EJECT                                                                
000486 S11-SKRIV-WXTR21A SECTION.                                               
000487                                                                          
000488     WRITE UT1-POST FROM UT1-AREA                                         
000489                                                                          
000490     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
000491     MOVE 'WXTR21A'  TO POSTSUM-FDNAMN                                    
000492     MOVE 'WXTR21D2' TO POSTSUM-DDNAMN2                                   
000493     CALL POSTSUM USING POSTSUM-PARM                                      
000494     .                                                                    
000495     SKIP3                                                                
000496 S12-SKRIV-WXTR21B SECTION.                                               
000497                                                                          
000498     WRITE UT2-POST FROM UT2-AREA                                         
000499                                                                          
000500     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
000501     MOVE 'WXTR21B'  TO POSTSUM-FDNAMN                                    
000502     MOVE 'WXTR21D3' TO POSTSUM-DDNAMN2                                   
000503     CALL POSTSUM USING POSTSUM-PARM                                      
000504     .                                                                    
000505     SKIP3                                                                
000506 S13-SKRIV-WXTR22 SECTION.                                                
000507                                                                          
000508     WRITE BORT-POST FROM BORT-AREA                                       
000509                                                                          
000510     MOVE SPACE       TO POSTSUM-TRANSTYP                                 
000511     MOVE 'WXTR22 '   TO POSTSUM-FDNAMN                                   
000512     MOVE 'WXTR21D4'  TO POSTSUM-DDNAMN2                                  
000513     CALL POSTSUM USING POSTSUM-PARM                                      
000514     .                                                                    
000515     EJECT                                                                
000516 S21-GET-ALARM-DESC   SECTION.                                            
000517     IF WS-TEST-KDLARM-2224 > ZERO                                        
000518        EVALUATE WS-TEST-KDLARM-2224                                      
000519           WHEN 100                                                       
000520              MOVE TEORSLRM-1          TO WS-2171-TEORSLRM                
000521           WHEN 110                                                       
000522              MOVE TEORSLRM-1          TO WS-2171-TEORSLRM                
000523           WHEN 150                                                       
000524              MOVE TEORSLRM-2          TO WS-2171-TEORSLRM                
000525           WHEN 200                                                       
000526              MOVE TEORSLRM-6          TO WS-2171-TEORSLRM                
000527           WHEN 210                                                       
000528              MOVE TEORSLRM-7          TO WS-2171-TEORSLRM                
000529           WHEN 221                                                       
000530              MOVE TEORSLRM-9          TO WS-2171-TEORSLRM                
000531           WHEN 222                                                       
000532              MOVE TEORSLRM-10         TO WS-2171-TEORSLRM                
000533           WHEN 223                                                       
000534              MOVE TEORSLRM-223        TO WS-2171-TEORSLRM                
000535           WHEN 240                                                       
000536              MOVE TEORSLRM-240        TO WS-2171-TEORSLRM                
000537           WHEN 300                                                       
000538              MOVE TEORSLRM-3          TO WS-2171-TEORSLRM                
000539           WHEN 301                                                       
000540              MOVE TEORSLRM-4          TO WS-2171-TEORSLRM                
000541           WHEN 302                                                       
000542              MOVE TEORSLRM-5          TO WS-2171-TEORSLRM                
000543           WHEN 400                                                       
000544              MOVE TEORSLRM-8          TO WS-2171-TEORSLRM                
000545           WHEN 500                                                       
000546              MOVE TEORSLRM-11         TO WS-2171-TEORSLRM                
000547           WHEN 600                                                       
000548              MOVE TEORSLRM-12         TO WS-2171-TEORSLRM                
000549           WHEN 601                                                       
000550              MOVE TEORSLRM-601        TO WS-2171-TEORSLRM                
000551           WHEN 610                                                       
000552              MOVE TEORSLRM-610        TO WS-2171-TEORSLRM                
000553           WHEN 708                                                       
000554              MOVE TEORSLRM-708        TO WS-2171-TEORSLRM                
000555           WHEN 710                                                       
000556              MOVE TEORSLRM-710        TO WS-2171-TEORSLRM                
000557           WHEN 712                                                       
000558              MOVE TEORSLRM-712        TO WS-2171-TEORSLRM                
000559           WHEN 720                                                       
000560              MOVE TEORSLRM-720        TO WS-2171-TEORSLRM                
000561           WHEN 761                                                       
000562              MOVE TEORSLRM-761        TO WS-2171-TEORSLRM                
000563           WHEN 777                                                       
000564              MOVE TEORSLRM-777        TO WS-2171-TEORSLRM                
000565           WHEN 999                                                       
000566              MOVE TEORSLRM-999        TO WS-2171-TEORSLRM                
000567           WHEN OTHER                                                     
000568               MOVE SPACES             TO WS-2171-TEORSLRM                
000569        END-EVALUATE                                                      
000570     END-IF                                                               
000571*                                                                         
000572     IF WS-TEST-KDLARM-D401 > ZERO                                        
000573        EVALUATE WS-TEST-KDLARM-D401                                      
000574           WHEN 220                                                       
000575              MOVE TEORSLRM-220        TO WS-2172-TEORSLRM                
000576           WHEN 225                                                       
000577              MOVE TEORSLRM-225        TO WS-2172-TEORSLRM                
000578           WHEN 230                                                       
000579              MOVE TEORSLRM-230        TO WS-2172-TEORSLRM                
000580           WHEN OTHER                                                     
000581              MOVE TEORSLRM-235        TO WS-2172-TEORSLRM                
000582        END-EVALUATE                                                      
000583     END-IF                                                               
000584     .                                                                    
000585     EJECT                                                                
000586 S51-CONVERT-DATE-FMT SECTION.                                            
000587                                                                          
000588     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
000589     CALL WDATKONV       USING DAT-KDDATFORM                              
000590                               DAT-I-TIDATUM                              
000591                               DAT-O-TIDATUM                              
000592                               DAT-KDSVAR                                 
000593     .                                                                    
000594     EJECT                                                                
000595 S52-GET-SUPPLIER-K6  SECTION.                                            
000596                                                                          
000597     PERFORM IMS-GU-K601                                                  
000598     IF SEGMENT-FINNS                                                     
000599        MOVE ART-IDLEVNR            TO UT1-EXTA-IDLEVNR                   
000600     END-IF                                                               
000601     .                                                                    
000602     EJECT                                                                
000603* --- IMS SEKTIONER ---                                                   
000604                                                                          
000605 IMS-GET-XXBU-ROT SECTION.                                                
000606                                                                          
000607     STRING 'WDR501  (IDHTYP   =' W-IDHTYP-X ')  '                        
000608          DELIMITED BY SIZE INTO SSA1                                     
000609     MOVE '  GEGB'         TO GODK-STATUSKODER                            
000610     CALL CBLTDLI USING GN WDR5-PCB DLI-IO-WDR501-2223 SSA1               
000611     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
000612     PERFORM IMS-STATUSKONTROLL                                           
000613     .                                                                    
000614     SKIP3                                                                
000615 IMS-GNP-WDGX2224 SECTION.                                                
000616                                                                          
000617     MOVE 'WDR550 '        TO SSA1                                        
000618     MOVE '  GEGB'         TO GODK-STATUSKODER                            
000619     CALL CBLTDLI USING GNP WDR5-PCB DLI-IO-WDGX2224 SSA1                 
000620     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
000621     PERFORM IMS-STATUSKONTROLL                                           
000622     .                                                                    
000623     SKIP3                                                                
000624 IMS-GET-WDD401   SECTION.                                                
000625                                                                          
000626     MOVE 'WDD401 '        TO SSA1                                        
000627     MOVE '  GEGB'         TO GODK-STATUSKODER                            
000628     CALL CBLTDLI USING GN  WDD4-PCB DLI-IO-WDD401 SSA1                   
000629     MOVE WDD4-STATUS-CODE TO STATUS-WS                                   
000630     PERFORM IMS-STATUSKONTROLL                                           
000631     .                                                                    
000632     SKIP3                                                                
000633 IMS-GET-FILC-ROT SECTION.                                                
000634                                                                          
000635     STRING 'WDR301  (IDPGM    =' W-IDPGM-2171-X                          
000636                    '!IDPGM    =' W-IDPGM-2172-X ')'                      
000637          DELIMITED BY SIZE INTO SSA1                                     
000638     MOVE '  GEGB' TO GODK-STATUSKODER                                    
000639     CALL CBLTDLI USING GN WDR3-PCB DLI-IO-WDR301 SSA1                    
000640     MOVE WDR3-STATUS-CODE TO STATUS-WS                                   
000641     PERFORM IMS-STATUSKONTROLL                                           
000642     .                                                                    
000643     SKIP3                                                                
000644 IMS-GU-K601 SECTION.                                                     
000645     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
000646          DELIMITED BY SIZE INTO SSA1                                     
000647     MOVE '  GE' TO GODK-STATUSKODER                                      
000648     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
000649     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
000650     PERFORM IMS-STATUSKONTROLL                                           
000651     .                                                                    
000652     SKIP3                                                                
000653 IMS-STATUSKONTROLL SECTION.                                              
000654     SET STATUS-IX TO 1                                                   
000655     SEARCH GODK-STATUS                                                   
000656       AT END                                                             
000657         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
000658           DELIMITED BY SIZE INTO FELTEXT                                 
000659         DISPLAY FELTEXT                                                  
000660         CALL FELLOG                                                      
000661       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
000662         CONTINUE                                                         
000663     END-SEARCH                                                           
000664     .                                                                    
