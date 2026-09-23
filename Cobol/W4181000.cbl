000001 ID DIVISION.                                                             
000002                                                                          
000003 PROGRAM-ID.     W4181000.                                                
000004 AUTHOR.         JAN-ERIK FRANTZEN.                                       
000005 DATE-WRITTEN.   95/06/14.                                                
000006 DATE-COMPILED.                                                           
000007                                                                          
000009*    FUNKTION:                                                            
000010*        KONTROLL AV LEVANM.POSTER FRÅN VIPS OCH VCOM MED HJÄLP           
000011*        AV SUBPROGRAM W418KTL1 OCH W418KTL2.                             
000012*        KONTROLL AV RETURMATRIS VIA SUBPROGRAM W418KTL3.                 
000013*                                                                         
000014*        PROGRAMMET LÄSER      WLKREE (WDA2)                              
000015*        PROGRAMMET LÄSER      WDL5                                       
000016*        PROGRAMMET LÄSER      WDB2                                       
000017*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
000018*        PROGRAMMET LÄSER      WLXXMI (WDR1 HTYP 4102)                    
000019*        PROGRAMMET LÄSER      WLGMTB (WDB3)                              
000020*                                                                         
000021*    ABENDKODER:                                                          
000022*        U0016 -  . . . .                                                 
000023*        U1000 -  . . . .                                                 
000024*                                                                         
000025* CHANGE LOG:                                                             
000026*        E-TRACKER 8635407  20091021 RETURN CODES MATRIX                  
000027*        E-TRACKER 9822116  20101014 DISCRAPANCY/RETURNS HAZ.MAT.         
000028*        E-TRACKER 10143271 2011-11-22 CHINA WAREHOUSE PROJECT-1          
000029*                                                                         
000030*                                                                         
000031                                                                          
000032                                                                          
000033 ENVIRONMENT DIVISION.                                                    
000034     SKIP2                                                                
000035 INPUT-OUTPUT SECTION.                                                    
000036                                                                          
000037 FILE-CONTROL.                                                            
000038     SKIP2                                                                
000039*          --- INFIL                                                      
000040     SELECT W41808                     ASSIGN TO W41810D1.                
000041     SKIP2                                                                
000042*          --- UTFILER                                                    
000043*          --- GODKÄNDA POSTER FÖR UPPLÄGG PÅ KRED.BASEN                  
000044     SELECT W41810                     ASSIGN TO W41810D2.                
000045     SKIP2                                                                
000046*          --- POSTTYP 002A BEKRÄFTELSEPOSTER                             
000047     SELECT W41811                     ASSIGN TO W41810D3.                
000048     SKIP2                                                                
000049*          --- GODKÄNDA BUYBACK POSTER FÖR UPPLÄGG PÅ KRED.BASEN          
000050     SELECT W41813                     ASSIGN TO W41810D4.                
000051     SKIP2                                                                
000052*          --- FELPOSTER TILL VIPS                                        
000053     SELECT W41814                     ASSIGN TO W41810D5.                
000054     EJECT                                                                
000055 DATA DIVISION.                                                           
000056     SKIP2                                                                
000057 FILE SECTION.                                                            
000058     SKIP3                                                                
000059 FD  W41808                                                               
000060     RECORDING       F                                                    
000061     BLOCK CONTAINS  0.                                                   
000062                                                                          
000063*01  -COPY W41808        -L.                                              
000064     SKIP3                                                                
000065 FD  W41810                                                               
000066     RECORDING       F                                                    
000067     BLOCK CONTAINS  0.                                                   
000068                                                                          
000069*01  POST -COPY W41810   -PRE  UT1-  -L.                                  
000070     SKIP3                                                                
000071 FD  W41811                                                               
000072     RECORDING       F                                                    
000073     BLOCK CONTAINS  0.                                                   
000074                                                                          
000075*01  POST -COPY W41811 -PRE  UT2-  -L.                                    
000076     SKIP3                                                                
000077 FD  W41813                                                               
000078     RECORDING       F                                                    
000079     BLOCK CONTAINS  0.                                                   
000080                                                                          
000081*01  POST -COPY W41810   -PRE  UT5-  -L.                                  
000082     SKIP3                                                                
000083 FD  W41814                                                               
000084     RECORDING       V                                                    
000085     BLOCK CONTAINS  0.                                                   
000086                                                                          
000087*01  UT3-POST   -COPY W461RKDN     -L.                                    
000088     EJECT                                                                
000089 WORKING-STORAGE SECTION.                                                 
000090                                                                          
000091*    -- CHECKED BY WY2000                                                 
000092     SKIP3                                                                
000093 77  IDPGM                       PIC X(8)    VALUE 'W4181000'.            
000094 77  JA                          PIC X       VALUE 'J'.                   
000095 77  NEJ                         PIC X       VALUE 'N'.                   
000096 77  YES                         PIC X       VALUE 'Y'.                   
000097 77  FEL                         PIC X       VALUE 'N'.                   
000098 77  LDC-FEL                     PIC X       VALUE 'N'.                   
000099 77  MATRIX-FEL                  PIC X       VALUE 'N'.                   
000100 77  IX                          PIC S9(9)   VALUE +0  COMP SYNC.         
000101 77  INDX                        PIC S9(9)   VALUE +0  COMP SYNC.         
000102 77  MAX-IX                      PIC S9(9)   VALUE +50 COMP SYNC.         
000103 77  TAB-IX                      PIC S9(9)   VALUE +0  COMP SYNC.         
000104 77  DUMMY-IDARTNR               PIC S9(9)   VALUE +100 COMP-3.           
000105                                                                          
000106 77  W41808-EOF-SW               PIC X       VALUE 'N'.                   
000107     88  END-OF-W41808                       VALUE 'J'.                   
000108                                                                          
000109 77  LEVANM-SW                   PIC X       VALUE 'N'.                   
000110     88  NY-LEVANM                           VALUE 'J'.                   
000111                                                                          
000112 77  BUY-BACK-SW                 PIC X       VALUE 'N'.                   
000113     88  BARA-BUY-BACK-POSTER                VALUE 'J'.                   
000114                                                                          
000115*-KODERNA 52, 53 OCH 72 AUTOMATGODKÄNNS.                                  
000116*-ALSO FOR DDGS ORDES AND FOR CODE 70,92 BUT SHOULD BE FOR                
000117*-IDFTG = 57 AND DC SHOULD NOT BE 61 AND 62                               
000118 77  AUTO-KOD-SW                 PIC X       VALUE 'N'.                   
000119     88  BARA-AUTO-KOD-POSTER                VALUE 'J'.                   
000120                                                                          
000121 77  LDC-SW                      PIC X       VALUE 'N'.                   
000122     88  EJ-KOD-72-LDC                       VALUE 'J'.                   
000123                                                                          
000124 77  KOD-72-LDC-SW               PIC X       VALUE 'N'.                   
000125     88  KOD-72-LDC                          VALUE 'J'.                   
000126     EJECT                                                                
000127                                                                          
000128 01  TEST-IDDISTR-LDC            PIC S9(5) COMP-3.                        
000129                                                                          
000130 01  TEST-IDKUNDNR-LDC           PIC S9(7) COMP-3.                        
000131                                                                          
000132 01  TEST-IDDISTR                PIC S9(5) COMP-3.                        
000133*01  FILLER -COPY WWDIST79 -RED TEST-IDDISTR.                             
000134     EJECT                                                                
000135                                                                          
000136*   VALID IDDC CODES                                                      
000137*01    -COPY WWDC99                                                       
000138     EJECT                                                                
000139                                                                          
000140 01  DYNAMISKA-SUBPROGRAM.                                                
000141*                                                                         
000142     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
000143     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
000144     03  W418KTL1                PIC X(8)    VALUE 'W418KTL1'.            
000145     03  W418KTL2                PIC X(8)    VALUE 'W418KTL2'.            
000146     03  W418KTL3                PIC X(8)    VALUE 'W418KTL3'.            
000147     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000148     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000149     03  W418OKOD                PIC X(8)    VALUE 'W418OKOD'.            
000150     SKIP2                                                                
000151*    --- PARAMETRAR TILL ABEND                                            
000152                                                                          
000153 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
000154 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
000155     EJECT                                                                
000156                                                                          
000157*    ---  LÄNKAREA TILL W418OKOD                                          
000158 01  FILLER                      PIC X(16)   VALUE 'W418OKOD'.            
000159                                                                          
000160*01 -COPY W418OKOD           -PRE OKOD-.                                  
000161                                                                          
000162     SKIP2                                                                
000163 01  FELTEXT.                                                             
000164     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
000165     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
000166     EJECT                                                                
000167*    --- LEVANM-TABELL                                                    
000168 01  LEVANM-POST-TABELL.                                                  
000169    03   LEVANM-POST OCCURS 10000.                                        
000170*     05   -COPY W41808    -PRE TAB-                                      
000171      05  TAB-IDANALYS          PIC X(12).                                
000172      05  TAB-IDKONTO           PIC S9(11) COMP-3.                        
000173      05  TAB-IDKST             PIC X(10).                                
000174      05  TAB-KDKREBEH          PIC X(3).                                 
000175      05  TAB-IDFELKOD          PIC X(3) OCCURS 50.                       
000176      05  TAB-IDUSER-PACK       PIC X(8).                                 
000177      05  TAB-KDORDKL           PIC S9(1)  COMP-3.                        
000178      05  TAB-FLANLYSF          PIC X(1).                                 
000179      05  TAB-FLPRQUES          PIC X(1).                                 
000180      05  TAB-KDKREBEH-MX       PIC X(3).                                 
000181     EJECT                                                                
000182                                                                          
000183*    --- PARAMETRAR TILL POSTSUM                                          
000184*                                                                         
000185*01  -COPY W0005   -PRE  POSTSUM-                                         
000186     EJECT                                                                
000187 01  IN-AREA-START              PIC X(24)   VALUE                         
000188                                 'IN-AREA-START  '.                       
000189     SKIP2                                                                
000190 01  IN-AREA.                                                             
000191     03  IN-AREA-0.                                                       
000192       05  IN-IDPTYP             PIC X(3).                                
000193       05  FILLER                PIC X(400).                              
000194*   03  FILLER -COPY W41808    -PRE IN1-  -RED  IN-AREA-0                 
000195     EJECT                                                                
000196 01  UT1-AREA-START              PIC X(24)   VALUE                        
000197                                 'UT1-AREA-START  '.                      
000198     SKIP2                                                                
000199                                                                          
000200*01  AREA -COPY W41810       -PRE UT1-                                    
000201     EJECT                                                                
000202 01  UT2-AREA-START              PIC X(24)   VALUE                        
000203                                 'UT2-AREA-START  '.                      
000204     SKIP2                                                                
000205                                                                          
000206*01  AREA -COPY W41811       -PRE UT2-                                    
000207     EJECT                                                                
000208 01  UT5-AREA-START              PIC X(24)   VALUE                        
000209                                 'UT5-AREA-START  '.                      
000210     SKIP2                                                                
000211                                                                          
000212*01  AREA -COPY W41810       -PRE UT5-                                    
000213     EJECT                                                                
000214 01  UT3-AREA-START              PIC X(24)   VALUE                        
000215                                 'UT3-AREA-START  '.                      
000216     SKIP2                                                                
000217                                                                          
000218*01  UT3-AREA -COPY W461RKDN                                              
000219     EJECT                                                                
000220 01  UT4-AREA-START              PIC X(24)   VALUE                        
000221                                 'UT4-AREA-START  '.                      
000222     SKIP2                                                                
000223                                                                          
000224*01  UT4-AREA -COPY W461RKI0                                              
000225     EJECT                                                                
000226*01  -COPY W418KTL1  -PRE LINK1-                                          
000227     EJECT                                                                
000228*01  -COPY W418KTL2  -PRE LINK2-                                          
000229     EJECT                                                                
000230*01  -COPY W418KTL3                                                       
000231     EJECT                                                                
000232*01  -COPY WWIDFTG                                                        
000233     EJECT                                                                
000234*01  -COPY WWDCKONS                                                       
000235     EJECT                                                                
000236 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000237     SKIP3                                                                
000238 01  NYCKLAR-TILL-DLI.                                                    
000239                                                                          
000240    03 W-IDGMT-X.                                                         
000241       05 W-WDB2-IDDISTR         PIC S9(5)    VALUE ZERO COMP-3.          
000242       05 W-WDB2-IDKUNDNR        PIC S9(7)    VALUE ZERO COMP-3.          
000243                                                                          
000244     SKIP2                                                                
000245*    --- STATUS-KOD FRÅN IMS                                              
000246 01  STATUS-WS                   PIC XX.                                  
000247     88  SEGMENT-FINNS                       VALUE '  '.                  
000248     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000249     SKIP2                                                                
000250 01  GODK-STATUSKODER.                                                    
000251     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000252     SKIP3                                                                
000253 01  SSA1                        PIC X(64).                               
000254 01  SSA2                        PIC X(64).                               
000255     EJECT                                                                
000256*    --- IMS FUNKTIONSKODER                                               
000257*01  -COPY W0003                                                          
000258     EJECT                                                                
000259*    ---  DLI INPUT-OUTPUT AREA                                           
000260 01  FILLER                      PIC X(16)   VALUE                        
000261                                               'AREA FOR WDB2  '.         
000262 01  DLI-IO-AREA-WDB2.                                                    
000263     03  DLI-IO-WDB201.                                                   
000264*        05  -COPY WDB201                                                 
000265     EJECT                                                                
000266 LINKAGE SECTION.                                                         
000267                                                                          
000268*01  -COPY W0008  -PRE KREE-                                              
000269     05  FILLER                  PIC X.                                   
000270     EJECT                                                                
000271*01  -COPY W0008  -PRE WDL5-                                              
000272     05  FILLER                  PIC X.                                   
000273     EJECT                                                                
000274*01  -COPY W0008  -PRE WDB2-                                              
000275     05  FILLER                  PIC X.                                   
000276     EJECT                                                                
000277*01  -COPY W0008  -PRE ARTC-                                              
000278     05  FILLER                  PIC X.                                   
000279     EJECT                                                                
000280*01  -COPY W0008  -PRE XXMI-                                              
000281     05  FILLER                  PIC X.                                   
000282     EJECT                                                                
000283*01  -COPY W0008                 -PRE PARTC-.                             
000284         05  WLPARTC-KONKAT-NKL  PIC X.                                   
000285     EJECT                                                                
000286                                                                          
000287*01  -COPY W0008                 -PRE PWDK7-.                             
000288         05  WDK7-KONKAT-NKL     PIC X.                                   
000289     EJECT                                                                
000290                                                                          
000291*01  -COPY W0008                 -PRE PGMTA-.                             
000292         05  WLPRIA-KONKAT-NKL   PIC X.                                   
000293     EJECT                                                                
000294                                                                          
000295*01  -COPY W0008                 -PRE BETA-.                              
000296         05  WLBETA-KONKAT-NKL   PIC X.                                   
000297     EJECT                                                                
000298                                                                          
000299*01  -COPY W0008                 -PRE PRIA-.                              
000300         05  WLPRIA-KONKAT-NKL   PIC X.                                   
000301     EJECT                                                                
000302                                                                          
000303*01  -COPY W0008                 -PRE GPRIB-.                             
000304         05  WLGPRIB-KONKAT-NKL   PIC X.                                  
000305     EJECT                                                                
000306*01  -COPY W0008  -PRE GMTB-                                              
000307     05  FILLER                  PIC X.                                   
000308     EJECT                                                                
000309*01  -COPY W0008  -PRE 9305-                                              
000310     05  FILLER                  PIC X.                                   
000311     EJECT                                                                
000312*01  -COPY W0008  -PRE WDB1-                                              
000313     05  FILLER                  PIC X.                                   
000314 01  4128-PCB                    PIC X.                                   
000315     EJECT                                                                
000316 01  COST-WDK6-PCB               PIC X.                                   
000317 01  COST-WDK7-PCB               PIC X.                                   
000318 01  COST-WDF1-PCB               PIC X.                                   
000319 01  COST-9305-PCB               PIC X.                                   
000320 01  COST-WDK72-PCB              PIC X.                                   
000321 01  COST-WDB6-PCB               PIC X.                                   
000322 01  PRIS-COST-WDK6-PCB          PIC X.                                   
000323 01  PRIS-COST-WDK7-PCB          PIC X.                                   
000324 01  PRIS-COST-WDF1-PCB          PIC X.                                   
000325 01  PRIS-COST-9305-PCB          PIC X.                                   
000326 01  PRIS-COST-WDK72-PCB         PIC X.                                   
000327 01  PRIS-COST-WDB6-PCB          PIC X.                                   
000328 01  KTL3-WDA8-PCB               PIC X.                                   
000329 01  KTL3-WDB2-PCB               PIC X.                                   
000330 01  KTL3-WDK6-PCB               PIC X.                                   
000331 01  KTL3-WDK7-PCB               PIC X.                                   
000332 01  KTL3-WDB6-PCB               PIC X.                                   
000333 01  KTL3-1165-PCB               PIC X.                                   
000334     EJECT                                                                
000335 PROCEDURE DIVISION  USING KREE-PCB  WDL5-PCB                             
000336                           WDB2-PCB  ARTC-PCB XXMI-PCB                    
000337                           PARTC-PCB PWDK7-PCB PGMTA-PCB                  
000338                           BETA-PCB  PRIA-PCB GPRIB-PCB                   
000339                           GMTB-PCB  9305-PCB                             
000340                           WDB1-PCB 4128-PCB                              
000341                           COST-WDK6-PCB                                  
000342                           COST-WDK7-PCB                                  
000343                           COST-WDF1-PCB                                  
000344                           COST-9305-PCB                                  
000345                           COST-WDK72-PCB                                 
000346                           COST-WDB6-PCB                                  
000347                           PRIS-COST-WDK6-PCB                             
000348                           PRIS-COST-WDK7-PCB                             
000349                           PRIS-COST-WDF1-PCB                             
000350                           PRIS-COST-9305-PCB                             
000351                           PRIS-COST-WDK72-PCB                            
000352                           PRIS-COST-WDB6-PCB                             
000353                           KTL3-WDA8-PCB                                  
000354                           KTL3-WDB2-PCB                                  
000355                           KTL3-WDK6-PCB                                  
000356                           KTL3-WDK7-PCB                                  
000357                           KTL3-WDB6-PCB                                  
000358                           KTL3-1165-PCB.                                 
000359                                                                          
000360 MAIN SECTION.                                                            
000361                                                                          
000362     ENTRY 'DLITCBL' USING KREE-PCB  WDL5-PCB                             
000363                           WDB2-PCB  ARTC-PCB XXMI-PCB                    
000364                           PARTC-PCB PWDK7-PCB PGMTA-PCB                  
000365                           BETA-PCB  PRIA-PCB GPRIB-PCB                   
000366                           GMTB-PCB  9305-PCB                             
000367                           WDB1-PCB 4128-PCB                              
000368                           COST-WDK6-PCB                                  
000369                           COST-WDK7-PCB                                  
000370                           COST-WDF1-PCB                                  
000371                           COST-9305-PCB                                  
000372                           COST-WDK72-PCB                                 
000373                           COST-WDB6-PCB                                  
000374                           PRIS-COST-WDK6-PCB                             
000375                           PRIS-COST-WDK7-PCB                             
000376                           PRIS-COST-WDF1-PCB                             
000377                           PRIS-COST-9305-PCB                             
000378                           PRIS-COST-WDK72-PCB                            
000379                           PRIS-COST-WDB6-PCB                             
000380                           KTL3-WDA8-PCB                                  
000381                           KTL3-WDB2-PCB                                  
000382                           KTL3-WDK6-PCB                                  
000383                           KTL3-WDK7-PCB                                  
000384                           KTL3-WDB6-PCB                                  
000385                           KTL3-1165-PCB.                                 
000386                                                                          
000387     PERFORM A-INIT                                                       
000388     PERFORM S01-LAES-W41808                                              
000389     PERFORM UNTIL END-OF-W41808                                          
000390       IF IN-IDPTYP = 'BAT' OR '01B' OR 'REF' OR '001' OR 'FAK' OR        
000391                      '01C'                                               
000392          PERFORM B-LAS-IN-LEVANM-I-TABELL                                
000393          PERFORM C-BEHANDLA-LEV-POST                                     
000394          PERFORM E-NOLLA-TABELLEN                                        
000395                                                                          
000396       ELSE                                                               
000397          PERFORM D-BEHANDLA-BKR-POST                                     
000398          PERFORM S01-LAES-W41808                                         
000399                                                                          
000400       END-IF                                                             
000401     END-PERFORM                                                          
000402                                                                          
000403                                                                          
000404     PERFORM Z-FINIT                                                      
000405                                                                          
000406     MOVE ZERO TO RETURN-CODE                                             
000407     GOBACK                                                               
000408     .                                                                    
000409     EJECT                                                                
000410 A-INIT SECTION.                                                          
000411*    DISPLAY '** A-INIT '                                                 
000412     MOVE '** A-INIT '         TO FELTEXT-STR                             
000413                                                                          
000414     OPEN INPUT  W41808                                                   
000415                                                                          
000416     OPEN OUTPUT W41810                                                   
000417                 W41811                                                   
000418                 W41813                                                   
000419                 W41814                                                   
000420                                                                          
000421     MOVE IDPGM                TO POSTSUM-PROGNAMN                        
000422     MOVE SPACE                TO LEVANM-POST-TABELL                      
000423     MOVE NEJ                  TO BUY-BACK-SW                             
000424     MOVE JA                   TO AUTO-KOD-SW                             
000425     .                                                                    
000426     EJECT                                                                
000427 B-LAS-IN-LEVANM-I-TABELL SECTION.                                        
000428*    DISPLAY '** B-LAS-IN-LEVANM-I-TABELL'                                
000429     MOVE '** B-LAS-IN-LEVANM-I-TABELL'                                   
000430                               TO FELTEXT-STR                             
000431                                                                          
000432     MOVE +1                   TO TAB-IX                                  
000433     MOVE NEJ                  TO LEVANM-SW                               
000434     MOVE NEJ                  TO LDC-SW                                  
000435     MOVE NEJ                  TO KOD-72-LDC-SW                           
000436     MOVE IN1-IDDISTR          TO TEST-IDDISTR-LDC                        
000437                                  TEST-IDDISTR                            
000438     MOVE IN1-IDKUNDNR         TO TEST-IDKUNDNR-LDC                       
000439     MOVE IN1-IDFTG            TO WS-IDFTG                                
000440                                                                          
000441     PERFORM S02-DIST-KUND-LDC                                            
000442                                                                          
000443     PERFORM UNTIL NY-LEVANM OR END-OF-W41808                             
000444        MOVE IN1-W41808        TO TAB-W41808 (TAB-IX)                     
000445        MOVE IN1-IDDC          TO WS-IDDC                                 
000446                                                                          
000447        IF IN1-KDANMORS = '72'   OR '52'  OR '53' OR                      
000448           GOOD-DDC              OR                                       
000449          ((IN1-KDANMORS = '70'  OR                                       
000450                           '92'  OR '20' OR '21') AND                     
000451           (IN1-IDFTG    = '57'  AND                                      
000452            IN1-IDDC NOT = '61'  AND '6A' AND '62'))                      
000453            CONTINUE                                                      
000454        ELSE                                                              
000455          MOVE NEJ TO AUTO-KOD-SW                                         
000456        END-IF                                                            
000457                                                                          
000458        IF IDFTG-CN                                                       
000459             IF (GMT-FLLDCKND = JA AND                                    
000460             (GMT-IDDC-RET72 (1) NOT = WC-NDC-CN-71 AND                   
000461              GMT-IDDC-RET72 (1) NOT = WC-NDC-CN-72 AND                   
000462              GMT-IDDC-RET72 (1) NOT = WC-NDC-CN-73 AND                   
000463              GMT-IDDC-RET72 (1) NOT = WC-NDC-CN-74))                     
000464               IF IN1-KDANMORS = '72'                                     
000465                 MOVE JA TO KOD-72-LDC-SW                                 
000466               ELSE                                                       
000467                 MOVE JA TO LDC-SW                                        
000468               END-IF                                                     
000469             END-IF                                                       
000470        ELSE                                                              
000471          IF (GMT-FLLDCKND = JA OR GMT-FLRETUR = JA) AND                  
000472            GMT-IDDC-RET72 (1) NOT = '11'                                 
000473            IF IN1-KDANMORS = '72'                                        
000474              MOVE JA  TO KOD-72-LDC-SW                                   
000475            ELSE                                                          
000476              MOVE JA  TO LDC-SW                                          
000477            END-IF                                                        
000478          END-IF                                                          
000479        END-IF                                                            
000480                                                                          
000481        IF TAB-FLDIRLEV (TAB-IX) = ' ' OR '0'                             
000482           MOVE NEJ            TO TAB-FLDIRLEV (TAB-IX)                   
000483        ELSE                                                              
000484           IF TAB-FLDIRLEV (TAB-IX) = '1'                                 
000485              MOVE JA          TO TAB-FLDIRLEV (TAB-IX)                   
000486           END-IF                                                         
000487        END-IF                                                            
000488        PERFORM S01-LAES-W41808                                           
000489        IF NOT END-OF-W41808                                              
000490           IF IN1-IDDISTR    = TAB-IDDISTR (TAB-IX) AND                   
000491              IN1-IDKUNDNR   = TAB-IDKUNDNR(TAB-IX) AND                   
000492              IN1-IDRAPPNR   = TAB-IDRAPPNR(TAB-IX)                       
000493                                                                          
000494              IF IN1-KDANMORS = '98'                                      
000495                MOVE JA        TO BUY-BACK-SW                             
000496              END-IF                                                      
000497                                                                          
000498              ADD +1           TO TAB-IX                                  
000499           ELSE                                                           
000500              MOVE JA          TO LEVANM-SW                               
000501           END-IF                                                         
000502        END-IF                                                            
000503     END-PERFORM                                                          
000504*    DISPLAY '*** TAB-IX  = ' TAB-IX                                      
000505     .                                                                    
000506     EJECT                                                                
000507 C-BEHANDLA-LEV-POST SECTION.                                             
000508*    DISPLAY '** C-BEHANDLA'                                              
000509     MOVE '** C-BEHANDLA-LEV-POST'                                        
000510                               TO FELTEXT-STR                             
000511                                                                          
000512     MOVE NEJ                  TO FEL                                     
000513     MOVE NEJ                  TO LDC-FEL                                 
000514     MOVE NEJ                  TO MATRIX-FEL                              
000515     MOVE +1                   TO INDX                                    
000516                                  IX                                      
000517     PERFORM UNTIL INDX > TAB-IX                                          
000518        PERFORM CA-FLYTTA-LAENK-DATA                                      
000519*       DISPLAY '***---- HOPPAR TILL W418KTL1         -----******'        
000520                                                                          
000521        CALL W418KTL1 USING LINK1-W418KTL1 KREE-PCB                       
000522                                           WDL5-PCB                       
000523                                           WDB2-PCB                       
000524                                           ARTC-PCB                       
000525                                           XXMI-PCB                       
000526                                          PARTC-PCB                       
000527                                          PWDK7-PCB                       
000528                                          PGMTA-PCB                       
000529                                           BETA-PCB                       
000530                                           PRIA-PCB                       
000531                                          GPRIB-PCB                       
000532                                           GMTB-PCB                       
000533                                           9305-PCB                       
000534                                           WDB1-PCB                       
000535                                           4128-PCB                       
000536                                      COST-WDK6-PCB                       
000537                                      COST-WDK7-PCB                       
000538                                      COST-WDF1-PCB                       
000539                                      COST-9305-PCB                       
000540                                      COST-WDK72-PCB                      
000541                                      COST-WDB6-PCB                       
000542                                 PRIS-COST-WDK6-PCB                       
000543                                 PRIS-COST-WDK7-PCB                       
000544                                 PRIS-COST-WDF1-PCB                       
000545                                 PRIS-COST-9305-PCB                       
000546                                 PRIS-COST-WDK72-PCB                      
000547                                 PRIS-COST-WDB6-PCB                       
000548                                                                          
000549        MOVE LINK1-FLANLYSF    TO TAB-FLANLYSF   (INDX)                   
000550        MOVE LINK1-FLDIRLEV    TO TAB-FLDIRLEV   (INDX)                   
000551        MOVE LINK1-IDANALYS    TO TAB-IDANALYS   (INDX)                   
000552        MOVE LINK1-IDKONTO     TO TAB-IDKONTO    (INDX)                   
000553        MOVE LINK1-IDKST       TO TAB-IDKST      (INDX)                   
000554        MOVE LINK1-IDDC        TO TAB-IDDC       (INDX)                   
000555        MOVE LINK1-IDFTG       TO TAB-IDFTG      (INDX)                   
000556        MOVE LINK1-IDKOLLI     TO TAB-IDKOLLI    (INDX)                   
000557        MOVE LINK1-IDUSER-PACK TO TAB-IDUSER-PACK(INDX)                   
000558        MOVE LINK1-KDFRAKT     TO TAB-KDFRAKT    (INDX)                   
000559        MOVE LINK1-KDORDKL     TO TAB-KDORDKL    (INDX)                   
000560        MOVE LINK1-PRARTBTO    TO TAB-PRARTBTO   (INDX)                   
000561        MOVE LINK1-PRARTBTO-LOC TO TAB-PRARTBTO-LOC (INDX)                
000562        MOVE LINK1-KDVAT       TO TAB-KDVAT      (INDX)                   
000563        MOVE LINK1-KDVALISO    TO TAB-KDVALISO   (INDX)                   
000564        MOVE LINK1-BEART-VIPS  TO TAB-BEART-VIPS (INDX)                   
000565        MOVE LINK1-FLPRQUES    TO TAB-FLPRQUES   (INDX)                   
000566        MOVE LINK1-PRARTSTD    TO TAB-PRARTSTD   (INDX)                   
000567        MOVE LINK1-PRARTSJK    TO TAB-PRARTSJK   (INDX)                   
000568                                                                          
000569***-- KOLLA NYA MATRIXEN FÖR RETURER.                                     
000570        MOVE 'N00'             TO TAB-KDKREBEH-MX(INDX)                   
000571        IF LINK1-IDARTNR = DUMMY-IDARTNR                                  
000572          CONTINUE                                                        
000573        ELSE                                                              
000574          MOVE LINK1-KDANMORS    TO OKOD-KDANMORS                         
000575          CALL W418OKOD USING OKOD-W418OKOD                               
000576          IF OKOD-FL-RETILL = 'J'                                         
000577                                                                          
000578            MOVE IDPGM           TO KTL3-IDPGM                            
000579            MOVE LINK1-IDARTNR   TO KTL3-IDARTNR                          
000580            MOVE LINK1-KDANMORS  TO KTL3-KDANMORS                         
000581            MOVE LINK1-IDDC      TO KTL3-IDDC                             
000582            MOVE LINK1-IDDISTR   TO KTL3-IDDISTR                          
000583            MOVE LINK1-IDKUNDNR  TO KTL3-IDKUNDNR                         
000584            CALL W418KTL3 USING KTL3-W418KTL3 KTL3-WDA8-PCB               
000585                                              KTL3-WDB2-PCB               
000586                                              KTL3-WDK6-PCB               
000587                                              KTL3-WDK7-PCB               
000588                                              KTL3-WDB6-PCB               
000589                                              KTL3-1165-PCB               
000590                                                                          
000591                                                                          
000592             IF KTL3-KDSVAR = YES                                         
000593               IF KTL3-KDRETBEH = 'S'                                     
000594                 MOVE JA             TO MATRIX-FEL                        
000595                 MOVE 'N12'          TO TAB-KDKREBEH-MX(INDX)             
000596               END-IF                                                     
000597               IF KTL3-KDRETBEH = 'Q'                                     
000598                 MOVE 'Q  '          TO TAB-KDKREBEH (INDX)               
000599               END-IF                                                     
000600               IF KTL3-KDRETBEH = 'P'                                     
000601                 MOVE 'P  '          TO TAB-KDKREBEH (INDX)               
000602               END-IF                                                     
000603             END-IF                                                       
000604          END-IF                                                          
000605        END-IF                                                            
000606                                                                          
000607        IF LINK1-KDKREBEH = SPACE                                         
000608          IF TAB-KDKREBEH   (INDX) = 'Q  ' OR 'P  '                       
000609            CONTINUE                                                      
000610          ELSE                                                            
000611            MOVE LINK1-IDDC    TO WS-IDDC                                 
000612            IF LINK1-KDANMORS = '98' OR '72' OR '52' OR '53' OR           
000613                                '96'         OR                           
000614               GOOD-DDC              OR                                   
000615              ((LINK1-KDANMORS = '70'OR                                   
000616                                 '92'OR '21' OR '20') AND                 
000617               (LINK1-IDFTG    = '57'  AND                                
000618                LINK1-IDDC NOT = '61'  AND '6A' AND '62'))                
000619               MOVE 'Y  '      TO TAB-KDKREBEH   (INDX)                   
000620            ELSE                                                          
000621              IF LINK1-IDPTYP   = 'FAK'                                   
000622                 MOVE 'Y  '    TO TAB-KDKREBEH   (INDX)                   
000623              ELSE                                                        
000624                 MOVE 'R  '    TO TAB-KDKREBEH   (INDX)                   
000625              END-IF                                                      
000626            END-IF                                                        
000627          END-IF                                                          
000628        ELSE                                                              
000629           MOVE LINK1-KDKREBEH TO TAB-KDKREBEH   (INDX)                   
000630        END-IF                                                            
000631        IF LINK1-KDSVAR = SPACE                                           
000632           IF LINK1-KDKREBEH = 'R72' OR 'R73'                             
000633              MOVE LINK1-KDKREBEH                                         
000634                            TO TAB-IDFELKOD (INDX, 1)                     
000635           ELSE                                                           
000636              MOVE '000'    TO TAB-IDFELKOD (INDX, 1)                     
000637           END-IF                                                         
000638        ELSE                                                              
000639           MOVE JA             TO FEL                                     
000640           PERFORM CB-KOLLA-FELTABELL                                     
000641        END-IF                                                            
000642                                                                          
000643        ADD +1                 TO INDX                                    
000644     END-PERFORM                                                          
000645                                                                          
000646*-MAN FÅR ALDRIG BLANDA KOD 72 MED ANDRA KODER FÖR LDC/LDC-GB             
000647*-KOD 72 RETURNERAS TILL LDC OCH ALLA ANDRA KODER TILL CDC.               
000648*-LDC-GB ÄR EN PILOTTEST OCH GÄLLER VISSA KUNDER. LDC-GB = SDC23          
000649*-DVS ETT VISST OMRÅDE AV LAGRET SKALL GÄLLA SOM LDC.                     
000650                                                                          
000651     MOVE +1                   TO INDX                                    
000652                                  IX                                      
000653     PERFORM UNTIL INDX > TAB-IX                                          
000654       IF EJ-KOD-72-LDC AND KOD-72-LDC                                    
000655         MOVE JA      TO LDC-FEL                                          
000656         PERFORM CCA-FLYTTA-FELPOST-DATA                                  
000657         PERFORM S13-SKRIV-W41814                                         
000658       ELSE                                                               
000659         IF MATRIX-FEL = JA                                               
000660           PERFORM CCA-FLYTTA-FELPOST-DATA                                
000661           PERFORM S13-SKRIV-W41814                                       
000662         ELSE                                                             
000663           IF FEL = JA                                                    
000664              PERFORM CC-BEHANDLA-FELPOST                                 
000665           ELSE                                                           
000666              IF BARA-BUY-BACK-POSTER                                     
000667                 PERFORM CD-FLYTTA-SKRIV-BUY-BACK                         
000668              ELSE                                                        
000669                 PERFORM CE-FLYTTA-SKRIV-GODK-POST                        
000670              END-IF                                                      
000671           END-IF                                                         
000672         END-IF                                                           
000673       END-IF                                                             
000674       ADD +1                 TO INDX                                     
000675     END-PERFORM                                                          
000676     .                                                                    
000677     EJECT                                                                
000678 CA-FLYTTA-LAENK-DATA SECTION.                                            
000679*    DISPLAY '** CA-FLYTTA'                                               
000680     MOVE '** CA-FLYTTA-LAENK-DATA'                                       
000681                               TO FELTEXT-STR                             
000682                                                                          
000683     MOVE NEJ                  TO LINK1-FLANLYSF                          
000684     MOVE TAB-FLAUTKRE  (INDX) TO LINK1-FLAUTKRE                          
000685     MOVE TAB-FLDIRLEV  (INDX) TO LINK1-FLDIRLEV                          
000686     MOVE SPACE                TO LINK1-IDANALYS                          
000687     MOVE +0                   TO LINK1-IDKONTO                           
000688     MOVE SPACE                TO LINK1-IDKST                             
000689     MOVE TAB-IDARTNR   (INDX) TO LINK1-IDARTNR                           
000690     MOVE TAB-IDDC      (INDX) TO LINK1-IDDC                              
000691     MOVE TAB-IDDISTR   (INDX) TO LINK1-IDDISTR                           
000692                                                                          
000693     MOVE TAB-IDFAKT      (INDX) TO LINK1-IDFAKT                          
000694     MOVE TAB-TIFAKT      (INDX) TO LINK1-TIFAKT                          
000695                                                                          
000696     MOVE +1                   TO IX                                      
000697     PERFORM UNTIL IX > MAX-IX                                            
000698        MOVE SPACE             TO LINK1-IDFELKOD(IX)                      
000699        ADD +1                 TO IX                                      
000700     END-PERFORM                                                          
000701                                                                          
000702     MOVE TAB-IDFTG     (INDX) TO LINK1-IDFTG                             
000703     MOVE TAB-IDKOLLI   (INDX) TO LINK1-IDKOLLI                           
000704     MOVE TAB-IDKUNDNR  (INDX) TO LINK1-IDKUNDNR                          
000705     MOVE TAB-IDORDNR   (INDX) TO LINK1-IDORDNR5                          
000706     MOVE TAB-IDPTYP    (INDX) TO LINK1-IDPTYP                            
000707     MOVE TAB-IDRAPPNR  (INDX) TO LINK1-IDRAPPNR                          
000708     MOVE SPACE                TO LINK1-IDUSER-PACK                       
000709     MOVE TAB-KDANMORS  (INDX) TO LINK1-KDANMORS                          
000710     MOVE TAB-KDEMBLEV  (INDX) TO LINK1-KDEMBLEV                          
000711     MOVE TAB-KDFAKTYP  (INDX) TO LINK1-KDFAKTYP                          
000712     MOVE TAB-KDFRAKT   (INDX) TO LINK1-KDFRAKT                           
000713     MOVE SPACE                TO LINK1-KDKREBEH                          
000714     MOVE +9                   TO LINK1-KDORDKL                           
000715     MOVE SPACE                TO LINK1-KDSVAR                            
000716     MOVE TAB-KVLEVANM  (INDX) TO LINK1-KVLEVANM                          
000717     MOVE TAB-PRARTBTO  (INDX) TO LINK1-PRARTBTO                          
000718     MOVE TAB-PRARTBTO-LOC  (INDX) TO LINK1-PRARTBTO-LOC                  
000719     MOVE TAB-TILEVANM  (INDX) TO LINK1-TILEVANM                          
000720     MOVE TAB-KDVAT     (INDX) TO LINK1-KDVAT                             
000721     MOVE TAB-KDVALISO  (INDX) TO LINK1-KDVALISO                          
000722     MOVE TAB-BEART-VIPS (INDX) TO LINK1-BEART-VIPS                       
000723     MOVE 'N'                  TO LINK1-FLPRQUES                          
000724     MOVE TAB-PRARTSTD  (INDX) TO LINK1-PRARTSTD                          
000725     MOVE TAB-PRARTSJK  (INDX) TO LINK1-PRARTSJK                          
000726     .                                                                    
000727     EJECT                                                                
000728 CB-KOLLA-FELTABELL SECTION.                                              
000729*    DISPLAY '** CB-KOLLA'                                                
000730******************************************************************        
000731*    LÄSER IGENOM TABELLEN MED FELKODER FRÅN                     *        
000732*    SUBPROGRAM W418KTL1.                                        *        
000733******************************************************************        
000734     MOVE '** CB-KOLLA-FELTABELL'                                         
000735                               TO FELTEXT-STR                             
000736                                                                          
000737*    MOVE NEJ                  TO FEL                                     
000738     MOVE +1                   TO IX                                      
000739                                                                          
000740     PERFORM UNTIL IX > MAX-IX                                            
000741*       DISPLAY '*** LINK1-IDFELKOD (IX)= ' LINK1-IDFELKOD(IX)            
000742        IF LINK1-IDFELKOD(IX) = SPACE                                     
000743           CONTINUE                                                       
000744        ELSE                                                              
000745           MOVE LINK1-IDFELKOD(IX)                                        
000746                               TO TAB-IDFELKOD (INDX, IX)                 
000747        END-IF                                                            
000748        ADD +1                 TO IX                                      
000749     END-PERFORM                                                          
000750*    DISPLAY '*** SLUT CB- '                                              
000751     .                                                                    
000752     EJECT                                                                
000753 CC-BEHANDLA-FELPOST SECTION.                                             
000754*    DISPLAY '** CC-BEHANDLA-FELPOST'                                     
000755           PERFORM UNTIL TAB-IDFELKOD(INDX, IX) = SPACE                   
000756                      OR IX > 50                                          
000757              PERFORM CCA-FLYTTA-FELPOST-DATA                             
000758              PERFORM S13-SKRIV-W41814                                    
000759              ADD +1           TO IX                                      
000760           END-PERFORM                                                    
000761           MOVE +1             TO IX                                      
000762     .                                                                    
000763     EJECT                                                                
000764 CCA-FLYTTA-FELPOST-DATA SECTION.                                         
000765*    DISPLAY '** CCA-KOLLA'                                               
000766     MOVE '** CCA-FLYTTA-FELPOST-DATA'                                    
000767                                 TO FELTEXT-STR                           
000768                                                                          
000769     MOVE SPACE                  TO RKD-W461RKDN                          
000770     MOVE 'RKD'                  TO RKD-IDPTYP                            
000771     MOVE TAB-IDDISTR   (INDX)   TO RKD-IDDISTR                           
000772     MOVE TAB-IDKUNDNR  (INDX)   TO RKD-IDKUNDNR                          
000773     MOVE TAB-IDDC(INDX)         TO RKD-IDDC                              
000774     MOVE TAB-IDRAPPNR  (INDX)   TO RKD-IDRAPPNR                          
000775     MOVE TAB-IDORDNR   (INDX)   TO RKD-IDORDNR                           
000776     MOVE TAB-IDKOLLI   (INDX)   TO RKD-IDKOLLI                           
000777     MOVE TAB-IDARTNR   (INDX)   TO RKD-IDARTNR                           
000778     MOVE +0                     TO RKD-REKSIFFR                          
000779     MOVE TAB-IDRADNR   (INDX)   TO RKD-IDRADNR                           
000780                                                                          
000781     IF LDC-FEL = JA                                                      
000782       MOVE 'N76'                TO RKD-KDKREBEH                          
000783     ELSE                                                                 
000784       IF MATRIX-FEL = JA                                                 
000785         MOVE TAB-KDKREBEH-MX (INDX)  TO RKD-KDKREBEH                     
000786       ELSE                                                               
000787         IF TAB-IDFELKOD (INDX, IX) = 'R72' OR 'R73'                      
000788            MOVE 'N00'           TO RKD-KDKREBEH                          
000789         ELSE                                                             
000790            MOVE TAB-IDFELKOD(INDX, IX) TO RKD-KDKREBEH                   
000791            MOVE 'N'             TO RKD-KDKREBEH (1 : 1)                  
000792         END-IF                                                           
000793       END-IF                                                             
000794     END-IF                                                               
000795                                                                          
000796     MOVE TAB-KDANMORS  (INDX)        TO RKD-KDANMORS                     
000797     MOVE TAB-KVLEVANM  (INDX)        TO RKD-KVLEVANM                     
000798     MOVE +0                          TO RKD-FLSKROT                      
000799                                                                          
000800     IF DIST79-DEALER-PRICE OR                                            
000802        DIST79-ECOM-PRICE                                                 
000803       MOVE TAB-KDVALISO (INDX)       TO RKD-KDVALISO                     
000804       MOVE TAB-PRARTBTO  (INDX)      TO RKD-PRARTBTO                     
000805       MOVE TAB-PRARTBTO-LOC (INDX)   TO RKD-PRARTBTO-LOC                 
000806                                                                          
000807       COMPUTE RKD-SULNELOC = TAB-PRARTBTO-LOC (INDX) *                   
000808                              TAB-KVLEVANM (INDX)                         
000809       MOVE TAB-PRARTSTD (INDX)       TO RKD-PRARTSTD                     
000810       MOVE TAB-PRARTSJK (INDX)       TO RKD-PRARTSJK                     
000811     ELSE                                                                 
000812       MOVE SPACE                     TO RKD-KDVALISO                     
000813       MOVE TAB-PRARTBTO    (INDX)    TO RKD-PRARTBTO                     
000814       MOVE +0                        TO RKD-PRARTBTO-LOC                 
000815       MOVE +0                        TO RKD-SULNELOC                     
000816       MOVE +0                        TO RKD-PRARTSTD                     
000817       MOVE +0                        TO RKD-PRARTSJK                     
000818     END-IF                                                               
000819                                                                          
000820*    DISPLAY  '*** RKD-KDKREBEH= ' RKD-KDKREBEH                           
000821     .                                                                    
000822     EJECT                                                                
000823 CD-FLYTTA-SKRIV-BUY-BACK SECTION.                                        
000824*    DISPLAY '** CD-FLYTTA'                                               
000825     MOVE '** CD-FLYTTA-SKRIV-BUY-BACK'                                   
000826                               TO FELTEXT-STR                             
000827     MOVE 'BAT'                TO UT5-IDPTYP                              
000828     MOVE TAB-IDDISTR   (INDX) TO UT5-IDDISTR                             
000829     MOVE TAB-IDKUNDNR  (INDX) TO UT5-IDKUNDNR                            
000830     MOVE TAB-IDRAPPNR  (INDX) TO UT5-IDRAPPNR                            
000831     MOVE TAB-IDARTNR   (INDX) TO UT5-IDARTNR                             
000832     MOVE TAB-IDRADNR   (INDX) TO UT5-IDRADNR                             
000833     MOVE TAB-FLAUTKRE  (INDX) TO UT5-FLAUTKRE                            
000834     MOVE TAB-FLANLYSF  (INDX) TO UT5-FLANLYSF                            
000835     MOVE TAB-FLDIRLEV  (INDX) TO UT5-FLDIRLEV                            
000836     MOVE TAB-IDANALYS  (INDX) TO UT5-IDANALYS                            
000837     MOVE TAB-IDKONTO   (INDX) TO UT5-IDKONTO                             
000838     MOVE TAB-IDKST     (INDX) TO UT5-IDKST                               
000839     MOVE TAB-IDDC      (INDX) TO UT5-IDDC                                
000840     MOVE TAB-IDDC-RET  (INDX) TO UT5-IDDC-RET                            
000841     MOVE TAB-IDFAKT    (INDX) TO UT5-IDFAKT                              
000842     MOVE TAB-IDFAKT-LOC(INDX) TO UT5-IDFAKT-LOC                          
000843     MOVE TAB-IDFTG     (INDX) TO UT5-IDFTG                               
000844     MOVE TAB-IDKOLLI   (INDX) TO UT5-IDKOLLI                             
000845     MOVE TAB-IDLOPNRM  (INDX) TO UT5-IDLOPNRM                            
000846     MOVE TAB-IDORDNR   (INDX) TO UT5-IDORDNR                             
000847     MOVE TAB-IDUSER-PACK(INDX) TO UT5-IDUSER-PACK                        
000848     MOVE TAB-KDANMORS  (INDX) TO UT5-KDANMORS                            
000849     MOVE TAB-KDEMBLEV  (INDX) TO UT5-KDEMBLEV                            
000850     MOVE TAB-KDFAKTYP  (INDX) TO UT5-KDFAKTYP                            
000851     MOVE TAB-KDFRAKT   (INDX) TO UT5-KDFRAKT                             
000852     IF TAB-KDKREBEH (INDX) = '000'                                       
000853        MOVE SPACE             TO UT5-KDKREBEH                            
000854     ELSE                                                                 
000855        MOVE TAB-KDKREBEH (INDX)                                          
000856                               TO UT5-KDKREBEH                            
000857     END-IF                                                               
000858     MOVE TAB-KDORDKL   (INDX) TO UT5-KDORDKL                             
000859     MOVE TAB-KVLEVANM  (INDX) TO UT5-KVLEVANM                            
000860     MOVE TAB-PRARTBTO  (INDX) TO UT5-PRARTBTO                            
000861     MOVE TAB-PRARTBTO-LOC(INDX) TO UT5-PRARTBTO-LOC                      
000862     MOVE TAB-PRARTBTO-LOCINV(INDX) TO UT5-PRARTBTO-LOCINV                
000863     MOVE TAB-PRFRAKT   (INDX) TO UT5-PRFRAKT                             
000864     MOVE TAB-TIFAKT    (INDX) TO UT5-TIFAKT                              
000865     MOVE TAB-TIFAKT-LOC(INDX) TO UT5-TIFAKT-LOC                          
000866     MOVE TAB-TILEVANM  (INDX) TO UT5-TILEVANM                            
000867     MOVE TAB-KDVAT     (INDX) TO UT5-KDVAT                               
000868     MOVE TAB-KDVALISO  (INDX) TO UT5-KDVALISO                            
000869     MOVE TAB-BEART-VIPS(INDX) TO UT5-BEART-VIPS                          
000870     MOVE TAB-PRARTSTD  (INDX) TO UT5-PRARTSTD                            
000871     MOVE TAB-PRARTSJK  (INDX) TO UT5-PRARTSJK                            
000872     MOVE TAB-FLPRQUES  (INDX) TO UT5-FLPRQUES                            
000873     MOVE TAB-TEANMNOT-REG (INDX 1)                                       
000874                               TO UT5-TEANMNOT-REG (1)                    
000875     MOVE TAB-TEANMNOT-REG (INDX 2)                                       
000876                               TO UT5-TEANMNOT-REG (2)                    
000877     MOVE TAB-TEANMNOT-REG (INDX 3)                                       
000878                               TO UT5-TEANMNOT-REG (3)                    
000879*                                                                         
000880     PERFORM S15-SKRIV-W41813                                             
000881     .                                                                    
000882     EJECT                                                                
000883 CE-FLYTTA-SKRIV-GODK-POST SECTION.                                       
000884*    DISPLAY '** CE-FLYTTA'                                               
000885     MOVE '** CE-FLYTTA-SKRIV-GODK-POST'                                  
000886                               TO FELTEXT-STR                             
000887     IF BARA-AUTO-KOD-POSTER                                              
000888       MOVE 'B72'              TO UT1-IDPTYP                              
000889     ELSE                                                                 
000890       IF TAB-IDPTYP    (INDX) = 'FAK'                                    
000891         MOVE TAB-IDPTYP(INDX) TO UT1-IDPTYP                              
000892       ELSE                                                               
000893         MOVE 'BAT'            TO UT1-IDPTYP                              
000894       END-IF                                                             
000895     END-IF                                                               
000896                                                                          
000897     MOVE TAB-IDDISTR   (INDX) TO UT1-IDDISTR                             
000898     MOVE TAB-IDKUNDNR  (INDX) TO UT1-IDKUNDNR                            
000899     MOVE TAB-IDRAPPNR  (INDX) TO UT1-IDRAPPNR                            
000900     MOVE TAB-IDARTNR   (INDX) TO UT1-IDARTNR                             
000901     MOVE TAB-IDRADNR   (INDX) TO UT1-IDRADNR                             
000902     MOVE TAB-FLAUTKRE  (INDX) TO UT1-FLAUTKRE                            
000903     MOVE TAB-FLANLYSF  (INDX) TO UT1-FLANLYSF                            
000904     MOVE TAB-FLDIRLEV  (INDX) TO UT1-FLDIRLEV                            
000905     MOVE TAB-IDANALYS  (INDX) TO UT1-IDANALYS                            
000906     MOVE TAB-IDKONTO   (INDX) TO UT1-IDKONTO                             
000907     MOVE TAB-IDKST     (INDX) TO UT1-IDKST                               
000908     MOVE TAB-IDDC      (INDX) TO UT1-IDDC                                
000909     MOVE TAB-IDDC-RET  (INDX) TO UT1-IDDC-RET                            
000910     MOVE TAB-IDFAKT    (INDX) TO UT1-IDFAKT                              
000911     MOVE TAB-IDFAKT-LOC(INDX) TO UT1-IDFAKT-LOC                          
000912     MOVE TAB-IDFTG     (INDX) TO UT1-IDFTG                               
000913     MOVE TAB-IDKOLLI   (INDX) TO UT1-IDKOLLI                             
000914     MOVE TAB-IDLOPNRM  (INDX) TO UT1-IDLOPNRM                            
000915     MOVE TAB-IDORDNR   (INDX) TO UT1-IDORDNR                             
000916     MOVE TAB-IDUSER-PACK(INDX) TO UT1-IDUSER-PACK                        
000917     MOVE TAB-KDANMORS  (INDX) TO UT1-KDANMORS                            
000918     MOVE TAB-KDEMBLEV  (INDX) TO UT1-KDEMBLEV                            
000919     MOVE TAB-KDFAKTYP  (INDX) TO UT1-KDFAKTYP                            
000920     MOVE TAB-KDFRAKT   (INDX) TO UT1-KDFRAKT                             
000921     IF TAB-KDKREBEH (INDX) = '000'                                       
000922        MOVE SPACE             TO UT1-KDKREBEH                            
000923     ELSE                                                                 
000924        MOVE TAB-KDKREBEH (INDX)                                          
000925                               TO UT1-KDKREBEH                            
000926     END-IF                                                               
000927     MOVE TAB-KDORDKL   (INDX) TO UT1-KDORDKL                             
000928     MOVE TAB-KVLEVANM  (INDX) TO UT1-KVLEVANM                            
000929     MOVE TAB-PRARTBTO  (INDX) TO UT1-PRARTBTO                            
000930     MOVE TAB-PRARTBTO-LOC(INDX) TO UT1-PRARTBTO-LOC                      
000931     MOVE TAB-PRARTBTO-LOCINV(INDX) TO UT1-PRARTBTO-LOCINV                
000932     MOVE TAB-PRFRAKT   (INDX) TO UT1-PRFRAKT                             
000933     MOVE TAB-TIFAKT    (INDX) TO UT1-TIFAKT                              
000934     MOVE TAB-TIFAKT-LOC(INDX) TO UT1-TIFAKT-LOC                          
000935     MOVE TAB-TILEVANM  (INDX) TO UT1-TILEVANM                            
000936     MOVE TAB-KDVAT     (INDX) TO UT1-KDVAT                               
000937     MOVE TAB-KDVALISO  (INDX) TO UT1-KDVALISO                            
000938     MOVE TAB-BEART-VIPS(INDX) TO UT1-BEART-VIPS                          
000939     MOVE TAB-PRARTSTD  (INDX) TO UT1-PRARTSTD                            
000940     MOVE TAB-PRARTSJK  (INDX) TO UT1-PRARTSJK                            
000941     MOVE TAB-FLPRQUES  (INDX)   TO UT1-FLPRQUES                          
000942     MOVE TAB-TEANMNOT-REG (INDX 1)                                       
000943                               TO UT1-TEANMNOT-REG (1)                    
000944     MOVE TAB-TEANMNOT-REG (INDX 2)                                       
000945                               TO UT1-TEANMNOT-REG (2)                    
000946     MOVE TAB-TEANMNOT-REG (INDX 3)                                       
000947                               TO UT1-TEANMNOT-REG (3)                    
000948*                                                                         
000949     PERFORM S11-SKRIV-W41810                                             
000950     .                                                                    
000951     EJECT                                                                
000952 D-BEHANDLA-BKR-POST SECTION.                                             
000953*    DISPLAY '** D-BEHANDLA-BKR'                                          
000954     MOVE '** D-BEHANDLA-BKR-POST'                                        
000955                               TO FELTEXT-STR                             
000956                                                                          
000957     PERFORM DA-FLYTTA-LAENK-DATA                                         
000958                                                                          
000959     CALL W418KTL2 USING LINK2-W418KTL2 KREE-PCB                          
000960                                        WDB2-PCB                          
000961                                                                          
000962*    DISPLAY '*** LINK2-IDFELKOD (1) = ' LINK2-IDFELKOD (1)               
000963*    DISPLAY '*** LINK2-IDFELKOD (2) = ' LINK2-IDFELKOD (2)               
000964     IF LINK2-KDSVAR = SPACE                                              
000965        MOVE NEJ               TO FEL                                     
000966     ELSE                                                                 
000967        PERFORM DB-KOLLA-FELTABELL                                        
000968     END-IF                                                               
000969                                                                          
000970     MOVE +1                   TO INDX                                    
000971     IF FEL = JA                                                          
000972        PERFORM UNTIL LINK2-IDFELKOD(INDX) = SPACE                        
000973                   OR INDX > 50                                           
000974           PERFORM DC-FLYTTA-FELPOST-DATA                                 
000975*--- VIPS KAN INTE TA EMOT EN FELTRANS FÖR TILLFÄLLET 960430 JEF          
000976*          PERFORM S14-SKRIV-W41814                                       
000977           ADD +1              TO INDX                                    
000978        END-PERFORM                                                       
000979     ELSE                                                                 
000980        PERFORM DD-FLYTTA-GODK-POST-DATA                                  
000981        PERFORM S12-SKRIV-W41811                                          
000982     END-IF                                                               
000983     .                                                                    
000984     EJECT                                                                
000985 DA-FLYTTA-LAENK-DATA SECTION.                                            
000986     MOVE '** DA-FLYTTA-LAENK-DATA'                                       
000987                               TO FELTEXT-STR                             
000988*    DISPLAY '*** DA-FLYTTA-LAENK-DATA'                                   
000989                                                                          
000990     MOVE IN1-IDDISTR          TO LINK2-IDDISTR                           
000991     MOVE IN1-IDKUNDNR         TO LINK2-IDKUNDNR                          
000992     MOVE IN1-IDRAPPNR         TO LINK2-IDRAPPNR                          
000993     MOVE IN1-IDARTNR          TO LINK2-IDARTNR                           
000994     MOVE IN1-IDRADNR          TO LINK2-IDRADNR                           
000995     MOVE IN1-KVLEVANM         TO LINK2-KVLEVANM-BEKR                     
000996     MOVE SPACE                TO LINK2-KDSVAR                            
000997                                                                          
000998     MOVE +1                   TO IX                                      
000999     PERFORM UNTIL IX > MAX-IX                                            
001000        MOVE SPACE             TO LINK2-IDFELKOD(IX)                      
001001        ADD +1                 TO IX                                      
001002     END-PERFORM                                                          
001003     .                                                                    
001004     EJECT                                                                
001005 DB-KOLLA-FELTABELL SECTION.                                              
001006     MOVE '** DB-KOLLA-FELTABELL'                                         
001007                               TO FELTEXT-STR                             
001008                                                                          
001009     MOVE NEJ                  TO FEL                                     
001010     MOVE +1                   TO IX                                      
001011                                                                          
001012     PERFORM UNTIL IX > MAX-IX                                            
001013*       DISPLAY '*** LINK2-IDFELKOD (IX)= ' LINK2-IDFELKOD(IX)            
001014        IF LINK2-IDFELKOD(IX) = SPACE                                     
001015           MOVE +51            TO IX                                      
001016        ELSE                                                              
001017           MOVE JA             TO FEL                                     
001018        END-IF                                                            
001019        ADD +1                 TO IX                                      
001020     END-PERFORM                                                          
001021*    DISPLAY '*** SLUT DB- '                                              
001022     .                                                                    
001023     EJECT                                                                
001024 DC-FLYTTA-FELPOST-DATA SECTION.                                          
001025*    DISPLAY '** DC-FLYTTA'                                               
001026     MOVE '** DC-FLYTTA-FELPOST-DATA'                                     
001027                                 TO FELTEXT-STR                           
001028                                                                          
001029     MOVE SPACE                  TO RKI-W461RKI0                          
001030     MOVE 'RKI'                  TO RKI-IDPTYP                            
001031     MOVE LINK2-IDDISTR          TO RKI-IDDISTR                           
001032     MOVE LINK2-IDKUNDNR         TO RKI-IDKUNDNR                          
001033     MOVE LINK2-IDRAPPNR         TO RKI-IDRAPPNR                          
001034     MOVE LINK2-IDARTNR          TO RKI-IDARTNR                           
001035     MOVE LINK2-IDRADNR          TO RKI-IDRADNR                           
001036     MOVE LINK2-IDFELKOD(INDX)   TO RKI-KDKREBEH                          
001037     MOVE 'N'                    TO RKI-KDKREBEH (1 : 1)                  
001038     MOVE LINK2-KVLEVANM-BEKR    TO RKI-KVLEVANM-BEKR                     
001039     .                                                                    
001040     EJECT                                                                
001041 DD-FLYTTA-GODK-POST-DATA SECTION.                                        
001042*    DISPLAY '*** DD-FLYTTA-GODK-POST-DATA'                               
001043                                                                          
001044     MOVE 'BKR'                TO UT2-IDPTYP                              
001045     MOVE IN1-IDDISTR          TO UT2-IDDISTR                             
001046     MOVE IN1-IDKUNDNR         TO UT2-IDKUNDNR                            
001047     MOVE IN1-IDRAPPNR         TO UT2-IDRAPPNR                            
001048     MOVE IN1-IDARTNR          TO UT2-IDARTNR                             
001049     MOVE IN1-IDRADNR          TO UT2-IDRADNR                             
001050     MOVE IN1-KVLEVANM         TO UT2-KVLEVANM-BEKR                       
001051     .                                                                    
001052     EJECT                                                                
001053 E-NOLLA-TABELLEN SECTION.                                                
001054*    DISPLAY '** E-NOLLA'                                                 
001055     MOVE '** E-NOLLA-TABELLEN'                                           
001056                               TO FELTEXT-STR                             
001057                                                                          
001058     MOVE SPACE                TO LEVANM-POST-TABELL                      
001059     MOVE NEJ                  TO BUY-BACK-SW                             
001060     MOVE JA                   TO AUTO-KOD-SW                             
001061     .                                                                    
001062     EJECT                                                                
001063 Z-FINIT SECTION.                                                         
001064*    DISPLAY '** Z-FINIT'                                                 
001065     MOVE '** Z-FINIT'         TO FELTEXT-STR                             
001066                                                                          
001067     CLOSE W41808                                                         
001068           W41810                                                         
001069           W41811                                                         
001070           W41813                                                         
001071           W41814                                                         
001072     SKIP2                                                                
001073     MOVE 'S' TO POSTSUM-OPKOD                                            
001074     CALL POSTSUM USING POSTSUM-PARM                                      
001075     .                                                                    
001076     EJECT                                                                
001077 S01-LAES-W41808  SECTION.                                                
001078*    DISPLAY '** S01 LAES'                                                
001079     MOVE '** S01-LAES-W41808' TO FELTEXT-STR                             
001080                                                                          
001081     READ W41808 INTO IN-AREA                                             
001082     AT END                                                               
001083        SET END-OF-W41808      TO TRUE                                    
001084                                                                          
001085     NOT AT END                                                           
001086        MOVE IN-IDPTYP        TO POSTSUM-TRANSTYP                         
001087        MOVE 'W41808'          TO POSTSUM-FDNAMN                          
001088        MOVE 'W41810D1'        TO POSTSUM-DDNAMN2                         
001089        CALL POSTSUM USING POSTSUM-PARM                                   
001090     END-READ                                                             
001091     .                                                                    
001092     EJECT                                                                
001093 S02-DIST-KUND-LDC SECTION.                                               
001094                                                                          
001095     MOVE TEST-IDDISTR-LDC    TO  W-WDB2-IDDISTR                          
001096     MOVE TEST-IDKUNDNR-LDC   TO  W-WDB2-IDKUNDNR                         
001097                                                                          
001098     PERFORM IMS-GU-WDB201                                                
001099                                                                          
001100     IF SEGMENT-SAKNAS                                                    
001101        MOVE NEJ              TO  GMT-FLLDCKND                            
001102                                  GMT-FLRETUR                             
001103     END-IF                                                               
001104     .                                                                    
001105     EJECT                                                                
001106 S11-SKRIV-W41810 SECTION.                                                
001107*    DISPLAY '** S11-SKRIV'                                               
001108     MOVE '**S11-SKRIV-W41810' TO FELTEXT-STR                             
001109                                                                          
001110     WRITE UT1-POST FROM UT1-AREA                                         
001111                                                                          
001112     MOVE UT1-IDPTYP           TO POSTSUM-TRANSTYP                        
001113     MOVE 'W41810'             TO POSTSUM-FDNAMN                          
001114     MOVE 'W41810D2'           TO POSTSUM-DDNAMN2                         
001115     CALL POSTSUM USING POSTSUM-PARM                                      
001116     .                                                                    
001117     EJECT                                                                
001118 S12-SKRIV-W41811 SECTION.                                                
001119*    DISPLAY '** S12-SKRIV'                                               
001120     MOVE '**S12-SKRIV-W41811' TO FELTEXT-STR                             
001121                                                                          
001122     WRITE UT2-POST FROM UT2-AREA                                         
001123                                                                          
001124     MOVE UT2-IDPTYP           TO POSTSUM-TRANSTYP                        
001125     MOVE 'W41811'             TO POSTSUM-FDNAMN                          
001126     MOVE 'W41810D3'           TO POSTSUM-DDNAMN2                         
001127     CALL POSTSUM USING POSTSUM-PARM                                      
001128     .                                                                    
001129     EJECT                                                                
001130 S13-SKRIV-W41814 SECTION.                                                
001131*    DISPLAY '** S13-SKRIV'                                               
001132     MOVE '**S13-SKRIV-W41814' TO FELTEXT-STR                             
001133                                                                          
001134     WRITE UT3-POST FROM UT3-AREA                                         
001135                                                                          
001136     MOVE RKD-IDPTYP           TO POSTSUM-TRANSTYP                        
001137     MOVE 'W41814'             TO POSTSUM-FDNAMN                          
001138     MOVE 'W41810D5'           TO POSTSUM-DDNAMN2                         
001139     CALL POSTSUM USING POSTSUM-PARM                                      
001140     .                                                                    
001141     EJECT                                                                
001142*S14-SKRIV-W41814 SECTION.                                                
001143*    DISPLAY '** S14-SKRIV'                                               
001144*    MOVE '**S14-SKRIV-W41814' TO FELTEXT-STR                             
001145*                                                                         
001146*    WRITE UT3-POST FROM UT4-AREA                                         
001147*                                                                         
001148*    MOVE RKI-IDPTYP           TO POSTSUM-TRANSTYP                        
001149*    MOVE 'W41814'             TO POSTSUM-FDNAMN                          
001150*    MOVE 'W41810D5'           TO POSTSUM-DDNAMN2                         
001151*    CALL POSTSUM USING POSTSUM-PARM                                      
001152*    .                                                                    
001153*    EJECT                                                                
001154 S15-SKRIV-W41813 SECTION.                                                
001155*    DISPLAY '** S15-SKRIV'                                               
001156     MOVE '**S15-SKRIV-W41813' TO FELTEXT-STR                             
001157                                                                          
001158     WRITE UT5-POST FROM UT5-AREA                                         
001159                                                                          
001160     MOVE UT5-IDPTYP           TO POSTSUM-TRANSTYP                        
001161     MOVE 'W41813'             TO POSTSUM-FDNAMN                          
001162     MOVE 'W41810D4'           TO POSTSUM-DDNAMN2                         
001163     CALL POSTSUM USING POSTSUM-PARM                                      
001164     .                                                                    
001165     EJECT                                                                
001166* --- IMS SEKTIONER ---                                                   
001167 IMS-GU-WDB201      SECTION.                                              
001168                                                                          
001169     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
001170          DELIMITED BY SIZE INTO SSA1                                     
001171     MOVE '  GE' TO GODK-STATUSKODER                                      
001172     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
001173     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
001174     PERFORM IMS-STATUSKONTROLL                                           
001175     .                                                                    
001176     EJECT                                                                
001177 IMS-STATUSKONTROLL SECTION.                                              
001178                                                                          
001179     SET STATUS-IX TO 1                                                   
001180     SEARCH GODK-STATUS                                                   
001181       AT END                                                             
001182         MOVE 'FEL STATUSKOD FRÅN IMS' TO FELTEXT-STR                     
001183         DISPLAY FELTEXT                                                  
001184         CALL FELLOG                                                      
001185       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
001186         CONTINUE                                                         
001187     END-SEARCH                                                           
001188     .                                                                    
