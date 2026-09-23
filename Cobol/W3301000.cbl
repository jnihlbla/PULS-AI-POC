000001 ID DIVISION.                                                             
000002 PROGRAM-ID.             W3301000.                                        
000003 AUTHOR.                 RONNY STENHOLM                                   
000004 DATE-WRITTEN.       SEP 1989.                                            
000005                                                                          
000006*                                                                         
000007*    FUNKTION:                                                            
000008*        FYRA FILER W33003, W33005, W33007 OCH W33009 ÄR                  
000009*        SORTERADE SAMMAN I EN PROCEDUR W330P010                          
000010*        ETT ANTAL TOTALSUMMOR RÄKNAS FRAM >                              
000011*                                                                         
000012*        TOTALA ANTALET LEVERERADE ARTIKLAR                               
000013*        TOTAL FÖRSÄLJNINGSSUMMA PÅ LEVERERADE ARTIKLAR                   
000014*                                                                         
000015*        TOTALA ANTALET LEVERERADE ARTIKLAR MED MANUELLT SATT PRIS        
000016*        TOTAL FÖRSÄLJNINGSSUMMA PÅ LEVERERADE ARTIKLAR MED               
000017*        MANUELLT SATT PRIS                                               
000018*                                                                         
000019*        TOTALA ANTALET LEVERERADE ARTIKLAR MED                           
000020*        RABATT/FUNKTIONSGRUPP                                            
000021*        TOTAL FÖRSÄLJNINGSSUMMA PÅ LEVERERADE ARTIKLAR MED               
000022*        RABATT/FUNKTIONSGRUPP                                            
000023*                                                                         
000024*        TOTALA ANTALET LEVERERADE ARTIKLAR MED                           
000025*        SPECIALPRIS                                                      
000026*        TOTAL FÖRSÄLJNINGSSUMMA PÅ LEVERERADE ARTIKLAR MED               
000027*        SPECIALPRIS                                                      
000028*                                                                         
000029*        TOTALA ANTALET LEVERERADE ARTIKLAR SOM                           
000030*        KREDITERATS                                                      
000031*        TOTAL FÖRSÄLJNINGSSUMMA PÅ LEVERERADE ARTIKLAR SOM               
000032*        KREDITERATS                                                      
000033*                                                                         
000034*        DESSA TOTALER ÄR BERÄKNADE PÅ FSGVV, ARTNR SAMT DISTR.           
000035*        ÖVRIGA TYPER AV PRISKODER BEHANDLAS EJ.                          
000036*        EN AV DE FYRA INFILERNA INNEHÅLLER JUSTERINGSTRANSAR             
000037*        OCH BIDRAR INTE TILL OVANSTÅENDE TOTALER.                        
000038*                                                                         
000039*        FELAKTIGA TRANSAKTIONER SKRIVES PÅ FELFIL, VILKEN SEN            
000040*        SKRIVS UT PÅ FELLISTA AV UT-TRATTEN.                             
000041*                                                                         
000042* REMARKS: ETRACKER ÄRENDE 1534965                                        
000043* REMARKS: ETRACKER ÄRENDE 1534965                                        
000044*                                                                         
000045*                                                                         
000046*    ABENDKODER:                                                          
000047*--------------------------                                               
000048     EJECT                                                                
000049 ENVIRONMENT DIVISION.                                                    
000050                                                                          
000051 INPUT-OUTPUT SECTION.                                                    
000052                                                                          
000053 FILE-CONTROL.                                                            
000054     SKIP2                                                                
000055*    ---- INFILER:                                                        
000056*                        - FYRA SAMSORTERADE FILER = W33010               
000057     SELECT  W33010        ASSIGN  W33010D1.                              
000058     SKIP2                                                                
000059*    ---- UTFILER:                                                        
000060*                            - UTPOSTER                                   
000061     SELECT  W33011        ASSIGN  W33010D2.                              
000062*                            - FELPOSTER                                  
000063     SELECT  W33010-FEL    ASSIGN  W33010D3.                              
000064*                            - FELPOSTER                                  
000065     SELECT  W33018        ASSIGN  W33010D4.                              
000066*                            - FIL TILL SALES AND TARGET                  
000067*  TRATTEN ÄR EJ KLAR ____________________________________________        
000068     EJECT                                                                
000069 DATA DIVISION.                                                           
000070                                                                          
000071 FILE SECTION.                                                            
000072     SKIP2                                                                
000073 FD  W33010                                                               
000074     LABEL RECORD STANDARD                                                
000075     RECORDING  V                                                         
000076     BLOCK CONTAINS 0.                                                    
000077                                                                          
000078*    -COPY W330100 -PRE IN1-   -L.                                        
000079*    -COPY W330200 -PRE IN2-   -L.                                        
000080     EJECT                                                                
000081 FD  W33010-FEL                                                           
000082     LABEL RECORD STANDARD                                                
000083     RECORDING  V                                                         
000084     BLOCK CONTAINS 0.                                                    
000085 01  FEL-POST.                                                            
000086*    03  -COPY W092W001    -L.                                            
000087     03 FEL-DEL                 PIC X(76).                                
000088     EJECT                                                                
000089                                                                          
000090 FD  W33011                                                               
000091     LABEL RECORD STANDARD                                                
000092     RECORDING  V                                                         
000093     BLOCK CONTAINS 0.                                                    
000094*01  POST   -COPY W330110  -PRE SUMMA-   -L.                              
000095*01  POST   -COPY W330200  -PRE JUST-   -L.                               
000096     EJECT                                                                
000097                                                                          
000098 FD  W33018                                                               
000099     LABEL RECORD STANDARD                                                
000100     RECORDING  F                                                         
000101     BLOCK CONTAINS 0.                                                    
000102*01  POST   -COPY W33018   -PRE SALES-   -L.                              
000103     EJECT                                                                
000104 WORKING-STORAGE SECTION.                                                 
000105     SKIP2                                                                
000106*    -COPY WY2000W3                                                       
000107     SKIP3                                                                
000108 77  PROGRAM-NAMN            PIC X(8) VALUE 'W3301000'.                   
000109     SKIP2                                                                
000110*    ---- KONSTANTER                                                      
000111                                                                          
000112 77  JA                      PIC X       VALUE 'J'.                       
000113 77  NEJ                     PIC X       VALUE 'N'.                       
000114*    ---- ARBETSFÄLT                                                      
000115 77  SPAR-IDPTYP             PIC X(3)  VALUE SPACE.                       
000116 77  SPAR-IDDISTR            PIC S9(5)      COMP-3  VALUE ZERO.           
000117 77  SPAR-IDARTNR            PIC S9(9)      COMP-3  VALUE ZERO.           
000118 77  SPAR-TIFSGVV            PIC S9(5)      COMP-3  VALUE ZERO.           
000119 77  WS-PRARTSJK             PIC S9(7)V9(2) COMP-3  VALUE ZERO.           
000120 77  WS-SULEVANT             PIC S9(9)      COMP-3  VALUE ZERO.           
000121 77  WS-SUARTFSG             PIC S9(9)V9(2) COMP-3  VALUE ZERO.           
000122 77  WS-SULEVANT-SPEC        PIC S9(9)      COMP-3  VALUE ZERO.           
000123 77  WS-SULEVANT-RAB         PIC S9(9)      COMP-3  VALUE ZERO.           
000124 77  WS-SULEVANT-KRE         PIC S9(9)      COMP-3  VALUE ZERO.           
000125 77  WS-SULEVANT-MAN         PIC S9(9)      COMP-3  VALUE ZERO.           
000126 77  WS-SULEVANT-DO          PIC S9(9)      COMP-3  VALUE ZERO.           
000127 77  WS-SUARTFSG-SPEC        PIC S9(9)V9(2) COMP-3  VALUE ZERO.           
000128 77  WS-SUARTFSG-RAB         PIC S9(9)V9(2) COMP-3  VALUE ZERO.           
000129 77  WS-SUARTFSG-KRE         PIC S9(9)V9(2) COMP-3  VALUE ZERO.           
000130 77  WS-SUARTFSG-MAN         PIC S9(9)V9(2) COMP-3  VALUE ZERO.           
000131 77  WS-SUARTFSG-DO          PIC S9(9)V9(2) COMP-3  VALUE ZERO.           
000132 77  W-DATE-AAMM             PIC 9(4)    VALUE ZERO.                      
000133 77  WS-KDVALISO-HUV         PIC X(3)    VALUE 'SEK'.                     
000134     SKIP2                                                                
000135                                                                          
000136 01  PACKA-UPP-KORT.                                                      
000137   03  P-UPP-IDPTYP               PIC X(3).                               
000138   03  P-UPP-IDARTNR              PIC 9(9).                               
000139   03  P-UPP-TIFSGVV              PIC 9(5).                               
000140   03  P-UPP-IDDISTR              PIC 9(5).                               
000141   03  P-UPP-KVLEVART             PIC S9(7).                              
000142   03  P-UPP-PRARTNTO             PIC S9(7)V9(2).                         
000143   03  P-UPP-PRARTSJK             PIC S9(7)V9(2).                         
000144   03  P-UPP-IDUSER               PIC X(8).                               
000145*    ---- VALID IDDC CODES                                                
000146*01   -COPY WWDCKONS                                                      
000147*    ---- END-OF-FILE SWITCHAR                                            
000148                                                                          
000149 77  W33010-SW               PIC X       VALUE 'N'.                       
000150   88  EOF-W33010                        VALUE 'J'.                       
000151     EJECT                                                                
000152*    ----  SWITCHAR ÖVRIGA                                                
000153                                                                          
000154 77  INDATA-FEL              PIC X       VALUE 'N'.                       
000155                                                                          
000156 01  DATUM                       PIC 9(8)    VALUE ZERO.                  
000157 01  FILLER REDEFINES DATUM.                                              
000158     03  DATUM-1.                                                         
000159       05  DATUM-SEKEL           PIC 9(2).                                
000160       05  DATUM-TIFSGVV         PIC 9(4).                                
000161     03  DATUM-DAG               PIC 9(2).                                
000162                                                                          
000163 01  DAGENS-DATUM                PIC 9(8).                                
000164 01  FILLER     REDEFINES DAGENS-DATUM.                                   
000165     03  DAGENS-SEKEL            PIC 9(2).                                
000166     03  DAGENS-AAR              PIC 9(2).                                
000167     03  DAGENS-MAANAD           PIC 9(2).                                
000168     03  DAGENS-DAG              PIC 9(2).                                
000169     EJECT                                                                
000170*    ---- SUBPROGRAM OCH PARAMETER-AREOR                                  
000171     SKIP2                                                                
000172 01  DYNAMISKA-SUBPROGRAM.                                                
000173   03  ABEND                 PIC X(8)    VALUE 'ABEND   '.                
000174   03  POSTSUM               PIC X(8)    VALUE 'POSTSUM '.                
000175   03  DATKORT               PIC X(8)    VALUE 'DATKORT'.                 
000176   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
000177   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
000178   03  W335PRIS              PIC X(8)    VALUE 'W335PRIS'.                
000179   03  W335CURR              PIC X(8)    VALUE 'W335CURR'.                
000180   03  W510CURR              PIC X(8)    VALUE 'W510CURR'.                
000181     SKIP2                                                                
000182*    ---- PARAMETRAR TILL ABEND                                           
000183                                                                          
000184 01  RKOD-ABEND-UTAN-DUMP    PIC S9(4)   VALUE +16  COMP SYNC.            
000185     SKIP2                                                                
000186     EJECT                                                                
000187*    ----  PARAMETRAR TILL POSTSUM                                        
000188                                                                          
000189*01  -COPY W0005  -PRE POSTSUM-.                                          
000190     EJECT                                                                
000191*    ---   DATKORT                                                        
000192*                                                                         
000193 01  FILLER                       PIC X(16)  VALUE 'DATKORT'.             
000194 01  DATKORT-PROGNAMN             PIC X(6)   VALUE SPACE.                 
000195 01  DATKORT-ID                   PIC X(6)   VALUE 'WDATUM'.              
000196*01  -COPY WDATKORT.                                                      
000197     EJECT                                                                
000198 01  TEST-IDDISTR       PIC 9(5)   COMP-3.                                
000199*                                                                         
000200*01  FILLER -COPY WWDIST58    -RED TEST-IDDISTR.                          
000201     EJECT                                                                
000202*    ---- POST-AREOR OCH IMS KOMMUNIKATIONS-AREOR                         
000203     SKIP2                                                                
000204*    ----  AREA FÖR INPOSTER                                              
000205 01  FILLER                  PIC X(24) VALUE 'IN-AREA-START'.             
000206     SKIP2                                                                
000207 01  IN0-AREA.                                                            
000208     03  FILLER              PIC X(36) VALUE SPACE.                       
000209     SKIP2                                                                
000210*01  FILLER -COPY W330200 -PRE IN200- -RED  IN0-AREA.                     
000211     EJECT                                                                
000212*01  FILLER -COPY W330100 -PRE IN- -RED  IN0-AREA.                        
000213     EJECT                                                                
000214 01  FILLER                  PIC X(24) VALUE 'UT-AREA-START'.             
000215     SKIP2                                                                
000216 01  UT110-AREA.                                                          
000217     03  FILLER              PIC X(85) VALUE SPACE.                       
000218     SKIP2                                                                
000219*01  FILLER -COPY W330110 -PRE W330110- -RED  UT110-AREA.                 
000220     EJECT                                                                
000221 01  UT200-AREA.                                                          
000222     03  FILLER              PIC X(85) VALUE SPACE.                       
000223     SKIP2                                                                
000224*01  FILLER -COPY W330200 -PRE W330200- -RED  UT200-AREA.                 
000225     EJECT                                                                
000226*    ----  AREA FÖR FELPOSTER                                             
000227 01  FILLER                  PIC X(24) VALUE 'FEL-AREA-START'.            
000228     SKIP2                                                                
000229 01  FEL-AREA.                                                            
000230*    03  FILLER -COPY W092W001 -PRE W092-.                                
000231     03 FEL-KORT             PIC X(76).                                   
000232     EJECT                                                                
000233 01  FILLER                  PIC X(24) VALUE 'SALES-AREA'.                
000234*01  AREA   -PRE SALES- -COPY W33018                                      
000235     EJECT                                                                
000236 01  FILLER                      PIC X(16) VALUE 'PRISTILL-AREA'.         
000237*01  PRIS-AREA   -COPY W335PRIS                                           
000238                                                                          
000239 01  FILLER                      PIC X(16) VALUE 'CURR-AREA'.             
000240*01  CURR-AREA   -COPY W335CURR                                           
000241                                                                          
000242**    --- PARAMETRAR TILL SUBPROGRAM W510CURR                             
000243**   -COPY W510CURR                                                       
000244     EJECT                                                                
000245*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA                                
000246                                                                          
000247 01  FILLER                  PIC X(16) VALUE 'IMS-WS'.                    
000248     SKIP2                                                                
000249*    ---- STATUSKOD FRÅN IMS                                              
000250                                                                          
000251 01  STATUS-WS               PIC XX.                                      
000252     88  SEGMENT-FINNS                    VALUE '  '.                     
000253     88  SEGMENT-SAKNAS                   VALUE 'GE'.                     
000254     88  SEGMENT-FINNS-REDAN              VALUE 'II'.                     
000255     SKIP2                                                                
000256 01  GODK-STATUSKODER.                                                    
000257     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000258     SKIP2                                                                
000259 01  SSA1                    PIC X(64).                                   
000260 01  SSA2                    PIC X(64).                                   
000261 01  SSA3                    PIC X(64).                                   
000262     SKIP2                                                                
000263*    ----  NYCKLAR OCH SÖKFÄLT TILL DLI                                   
000264                                                                          
000265 01  NYCKLAR-TILL-DLI.                                                    
000266                                                                          
000267   03  W-IDARTNR-X.                                                       
000268     05  W-IDARTNR           PIC S9(9)    COMP-3.                         
000269                                                                          
000270                                                                          
000271** NYCKLAR TILL KUNDREGISTRET *********                                   
000272   03  W-IDGMT-MIN-X.                                                     
000273     05  W-IDDISTR-1         PIC S9(5)   VALUE ZERO COMP-3.               
000274     05  W-IDKUNDNR-1        PIC S9(7)   VALUE ZERO COMP-3.               
000275                                                                          
000276   03  W-IDGMT-MAX-X.                                                     
000277     05  W-IDDISTR-2         PIC S9(5)   VALUE ZERO COMP-3.               
000278     05  W-IDKUNDNR-2        PIC S9(7)   VALUE +9999999 COMP-3.           
000284     EJECT                                                                
000285*01  -COPY W0003                                                          
000286     EJECT                                                                
000287 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA'.               
000288     SKIP2                                                                
000289 01  DLI-IO-AREA.                                                         
000290   03  IO-AREA               PIC X(900).                                  
000291     SKIP2                                                                
000292*03  WLARTC11 -COPY WDK611  -PRE EKO-        -RED IO-AREA                 
000293     EJECT                                                                
000294                                                                          
000295 01  FILLER               PIC X(16)   VALUE 'WDB201 AREA'.                
000296 01  DLI-IO-B201.                                                         
000297*03  -COPY WDB201  -PRE GMTA-                                             
000299     EJECT                                                                
000304 LINKAGE SECTION.                                                         
000305     SKIP2                                                                
000306*01  -COPY W0008 -PRE  ARTC-                                              
000307       05  FILLER                PIC X.                                   
000308     EJECT                                                                
000309*01  -COPY W0008 -PRE  GMTA-                                              
000310       05  FILLER                PIC X.                                   
000311     EJECT                                                                
000312*01  -COPY W0008 -PRE  PRIS-ARTC-.                                        
000313     05  FILLER        PIC X.                                             
000314     EJECT                                                                
000315                                                                          
000316*01  -COPY W0008 -PRE  PRIS-WDK7-.                                        
000317     05  FILLER        PIC X.                                             
000318     EJECT                                                                
000319                                                                          
000320*01  -COPY W0008 -PRE  PRIS-GMTA-.                                        
000321     05  FILLER        PIC X.                                             
000322     EJECT                                                                
000323                                                                          
000324*01  -COPY W0008 -PRE  PRIS-BETA-.                                        
000325     05  FILLER        PIC X.                                             
000326                                                                          
000327*01  -COPY W0008 -PRE  PRIS-GPRIA-.                                       
000328     05  FILLER        PIC X.                                             
000329     EJECT                                                                
000330*01  -COPY W0008 -PRE  PRIS-GPRIB-.                                       
000331     05  FILLER        PIC X.                                             
000332     EJECT                                                                
000333 01  PRIS-COST-WDK6-PCB          PIC X.                                   
000334 01  PRIS-COST-WDK7-PCB          PIC X.                                   
000335 01  PRIS-COST-WDF1-PCB          PIC X.                                   
000336*01  -COPY W0008 -PRE  PRIS-COST-9305-.                                   
000337     05  FILLER        PIC X.                                             
000338 01  PRIS-COST-WDK72-PCB         PIC X.                                   
000339 01  PRIS-COST-WDB6-PCB         PIC X.                                    
000341*01  -COPY W0008 -PRE  9305-                                              
000342       05  FILLER                PIC X.                                   
000344     EJECT                                                                
000345                                                                          
000346 PROCEDURE DIVISION  USING  ARTC-PCB GMTA-PCB                             
000347     PRIS-ARTC-PCB PRIS-WDK7-PCB PRIS-GMTA-PCB                            
000348     PRIS-BETA-PCB PRIS-GPRIA-PCB PRIS-GPRIB-PCB                          
000349     PRIS-COST-WDK6-PCB                                                   
000350     PRIS-COST-WDK7-PCB                                                   
000351     PRIS-COST-WDF1-PCB                                                   
000352     PRIS-COST-9305-PCB                                                   
000353     PRIS-COST-WDK72-PCB                                                  
000354     PRIS-COST-WDB6-PCB                                                   
000356     9305-PCB.                                                            
000357     ENTRY 'DLITCBL' USING  ARTC-PCB GMTA-PCB                             
000358     PRIS-ARTC-PCB PRIS-WDK7-PCB PRIS-GMTA-PCB                            
000359     PRIS-BETA-PCB PRIS-GPRIA-PCB PRIS-GPRIB-PCB                          
000360     PRIS-COST-WDK6-PCB                                                   
000361     PRIS-COST-WDK7-PCB                                                   
000362     PRIS-COST-WDF1-PCB                                                   
000363     PRIS-COST-9305-PCB                                                   
000364     PRIS-COST-WDK72-PCB                                                  
000365     PRIS-COST-WDB6-PCB                                                   
000367     9305-PCB.                                                            
000368                                                                          
000369 STYR SECTION.                                                            
000370                                                                          
000371     PERFORM A-INIT                                                       
000372     PERFORM S01-LAS-IN-FIL                                               
000373     PERFORM UNTIL EOF-W33010                                             
000374       PERFORM B-NOLLA-ALLA-TOTSUM                                        
000375       IF IN-IDPTYP = 100                                                 
000376*-------------------SUMMATRANSAR---------------                           
000377         PERFORM C-BEHANDLA-FOERSTA-POST                                  
000378         PERFORM D-BEHANDLA-POST-HUNDRA                                   
000379       ELSE                                                               
000380*-------------------JUSTERINGSTRANSAR----------                           
000381         PERFORM E-BEHANDLA-POST-TVAHUNDRA                                
000382       END-IF                                                             
000383     END-PERFORM                                                          
000384     PERFORM Z-FINIT                                                      
000385     MOVE ZERO TO RETURN-CODE                                             
000386     GOBACK                                                               
000387     .                                                                    
000388     EJECT                                                                
000389 A-INIT       SECTION.                                                    
000390     SKIP2                                                                
000391     OPEN INPUT W33010                                                    
000392     SKIP2                                                                
000393     OPEN OUTPUT W33011                                                   
000394                 W33010-FEL                                               
000395                 W33018                                                   
000396                                                                          
000397     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
000398                                                                          
000399*    MOVE SPACE TO FEL-IDPTYP                                             
000400*                  FEL-FILLER2                                            
000401                                                                          
000402     MOVE PROGRAM-NAMN TO DATKORT-PROGNAMN                                
000403                                                                          
000404     CALL DATKORT USING DATKORT-PROGNAMN DATKORT-ID DATUMKORT             
000405     MOVE D-AAR    TO DAGENS-AAR                                          
000406     MOVE D-MAANAD TO DAGENS-MAANAD                                       
000407     MOVE D-DAG    TO DAGENS-DAG                                          
000408     MOVE 20       TO DAGENS-SEKEL                                        
000409     MOVE DAGENS-AAR                 TO W-DATE-AAMM(1:2)                  
000410     MOVE DAGENS-MAANAD              TO W-DATE-AAMM(3:2)                  
000411     .                                                                    
000412     EJECT                                                                
000413 B-NOLLA-ALLA-TOTSUM SECTION.                                             
000414     SKIP2                                                                
000415     MOVE ZERO TO      WS-PRARTSJK                                        
000416                       WS-SULEVANT                                        
000417                       WS-SUARTFSG                                        
000418                       WS-SULEVANT-DO                                     
000419                       WS-SULEVANT-SPEC                                   
000420                       WS-SULEVANT-RAB                                    
000421                       WS-SULEVANT-KRE                                    
000422                       WS-SULEVANT-MAN                                    
000423                       WS-SUARTFSG-DO                                     
000424                       WS-SUARTFSG-SPEC                                   
000425                       WS-SUARTFSG-RAB                                    
000426                       WS-SUARTFSG-KRE                                    
000427                       WS-SUARTFSG-MAN                                    
000428     .                                                                    
000429     EJECT                                                                
000430                                                                          
000431 C-BEHANDLA-FOERSTA-POST SECTION.                                         
000432     SKIP2                                                                
000433     MOVE IN-IDARTNR TO W-IDARTNR                                         
000434     PERFORM IMS-HAMTA-SJALVKST                                           
000435     IF SEGMENT-FINNS                                                     
000436       MOVE EKO-CLAG-PRARTSJK TO W330110-PRARTSJK                         
000437     ELSE                                                                 
000438       MOVE 9999.11 TO W330110-PRARTSJK                                   
000439     END-IF                                                               
000440     MOVE IN-TIFSGVV TO W330110-TIFSGVV                                   
000441     .                                                                    
000442     EJECT                                                                
000443                                                                          
000444 D-BEHANDLA-POST-HUNDRA SECTION.                                          
000445                                                                          
000446     MOVE IN-IDPTYP TO SPAR-IDPTYP                                        
000447     MOVE IN-IDDISTR TO SPAR-IDDISTR                                      
000448     MOVE IN-IDARTNR TO SPAR-IDARTNR                                      
000449     MOVE IN-TIFSGVV TO SPAR-TIFSGVV                                      
000450     MOVE SPAR-TIFSGVV   TO TMP1-YYWW                                     
000451     MOVE IN-TIFSGVV     TO TMP2-YYWW                                     
000452     PERFORM WY2000P3                                                     
000453     PERFORM UNTIL SPAR-IDPTYP NOT = IN-IDPTYP OR                         
000454                   SPAR-IDDISTR < IN-IDDISTR OR                           
000455                   SPAR-IDARTNR < IN-IDARTNR OR                           
000456                   TMP1-YYWW    < TMP2-YYWW  OR                           
000457                   EOF-W33010                                             
000458       PERFORM S03-SUMTOT                                                 
000459       IF IN-KDORDKL > 2                                                  
000460         CONTINUE                                                         
000461       ELSE                                                               
000462         IF IN-KDORDKL = 2 AND IN-IDDISTR > 799                           
000463           CONTINUE                                                       
000464         ELSE                                                             
000465           PERFORM S04-SUM-DAGORDER                                       
000466         END-IF                                                           
000467       END-IF                                                             
000468       IF IN-KDPRTYP = 'P'                                                
000469         PERFORM S05-SUM-MANUELLA-PRISER                                  
000470       END-IF                                                             
000471       IF IN-KDPRTYP = 'F'                                                
000472         PERFORM S06-SUM-RABATT-FUNKTIONSGRP                              
000473       END-IF                                                             
000474       IF IN-KDPRTYP = 'S'                                                
000475         PERFORM S07-SUM-SPEC-PRIS                                        
000476       END-IF                                                             
000477       IF IN-KDPRTYP = 'K'                                                
000478         PERFORM S08-SUM-KRED                                             
000479       END-IF                                                             
000480       PERFORM S01-LAS-IN-FIL                                             
000481       MOVE SPAR-TIFSGVV   TO TMP1-YYWW                                   
000482       MOVE IN-TIFSGVV     TO TMP2-YYWW                                   
000483       PERFORM WY2000P3                                                   
000484     END-PERFORM                                                          
000485     PERFORM DA-FLYTTA-TILL-UTAREA                                        
000486     PERFORM DB-SKRIV-POST-110                                            
000487     .                                                                    
000488     EJECT                                                                
000489                                                                          
000490 DA-FLYTTA-TILL-UTAREA SECTION.                                           
000491                                                                          
000492     MOVE 110              TO W330110-IDPTYP                              
000493     MOVE SPAR-IDARTNR     TO W330110-IDARTNR                             
000494     MOVE SPAR-IDDISTR     TO W330110-IDDISTR                             
000495     MOVE WS-SULEVANT      TO W330110-SULEVANT                            
000496     MOVE WS-SUARTFSG      TO W330110-SUARTFSG                            
000497     MOVE WS-SULEVANT-SPEC TO W330110-SULEVANT-SPEC                       
000498     MOVE WS-SULEVANT-RAB  TO W330110-SULEVANT-RAB                        
000499     MOVE WS-SULEVANT-KRE  TO W330110-SULEVANT-KRE                        
000500     MOVE WS-SULEVANT-MAN  TO W330110-SULEVANT-MAN                        
000501     MOVE WS-SULEVANT-DO   TO W330110-SULEVANT-DO                         
000502     MOVE WS-SUARTFSG-SPEC TO W330110-SUARTFSG-SPEC                       
000503     MOVE WS-SUARTFSG-RAB  TO W330110-SUARTFSG-RAB                        
000504     MOVE WS-SUARTFSG-KRE  TO W330110-SUARTFSG-KRE                        
000505     MOVE WS-SUARTFSG-MAN  TO W330110-SUARTFSG-MAN                        
000506     MOVE WS-SUARTFSG-DO   TO W330110-SUARTFSG-DO                         
000507     PERFORM DD-SKAPA-SALES-TARGET                                        
000508     .                                                                    
000509     EJECT                                                                
000510                                                                          
000511 DB-SKRIV-POST-110 SECTION.                                               
000512     WRITE SUMMA-POST FROM UT110-AREA                                     
000513     MOVE SPACE TO POSTSUM-TRANSTYP                                       
000514     MOVE 'W33011' TO POSTSUM-FDNAMN                                      
000515     MOVE 'W33011D2' TO POSTSUM-DDNAMN2                                   
000516     CALL POSTSUM USING POSTSUM-PARM                                      
000517     .                                                                    
000518     EJECT                                                                
000519                                                                          
000520 DD-SKAPA-SALES-TARGET SECTION.                                           
000521                                                                          
000522                                                                          
000523     MOVE SPAR-IDDISTR     TO TEST-IDDISTR                                
000524     IF NOT DIST58-FEL                                                    
000525        MOVE 1                TO PRIS-KDCALL                              
000526        MOVE PROGRAM-NAMN     TO PRIS-IDPGM                               
000527        MOVE SPAR-IDARTNR     TO PRIS-IDARTNR                             
000528        MOVE SPAR-IDDISTR     TO PRIS-IDDISTR                             
000529        MOVE ZERO             TO PRIS-IDKUNDNR                            
000530        MOVE WC-CDC-SE        TO PRIS-IDDC                                
000531        MOVE +4               TO PRIS-KDORDKL                             
000532        MOVE +1               TO PRIS-KVBEART                             
000533        MOVE SPACE            TO PRIS-FLINVEST                            
000534                                                                          
000535        CALL W335PRIS USING PRIS-AREA                                     
000536                            PRIS-ARTC-PCB                                 
000537                            PRIS-WDK7-PCB                                 
000538                            PRIS-GMTA-PCB                                 
000539                            PRIS-BETA-PCB                                 
000540                            PRIS-GPRIA-PCB                                
000541                            PRIS-GPRIB-PCB                                
000542                            PRIS-COST-WDK6-PCB                            
000543                            PRIS-COST-WDK7-PCB                            
000544                            PRIS-COST-WDF1-PCB                            
000545                            PRIS-COST-9305-PCB                            
000546                            PRIS-COST-WDK72-PCB                           
000547                            PRIS-COST-WDB6-PCB                            
000549                                                                          
000550        MOVE PRIS-PRARTNTO    TO SALES-PRARTNTO-BULK                      
000551        IF PRIS-KDVALISO = 'SEK' OR SPACE                                 
000552          CONTINUE                                                        
000553        ELSE                                                              
000554          MOVE PRIS-KDVALISO             TO CURR-KDVALISO-ROW             
000555          MOVE W-DATE-AAMM               TO CURR-TIAAMM                   
000556          MOVE WS-KDVALISO-HUV           TO CURR-KDVALISO-HUV             
000557          MOVE 'M'                       TO CURR-KDVALTYP                 
000558          CALL W510CURR USING CURR-W510CURR 9305-PCB                      
000559          IF CURR-KDSVAR = ' '                                            
000562            CONTINUE                                                      
000563          ELSE                                                            
000564            MOVE 1                       TO CURR-PRKURS-NEW               
000565          END-IF                                                          
000566          MOVE ZERO                      TO CURR-SUORDV-IN                
000567                                            CURR-PRARTVNA-IN              
000568                                            CURR-PRARTSTD-IN              
000569                                            CURR-PRKURS-02                
000570          MOVE CURR-PRKURS-NEW           TO CURR-PRKURS                   
000571          MOVE PRIS-PRARTNTO             TO CURR-PRARTSJK-IN              
000572          MOVE CURR-KDVALISO-ROW         TO CURR-KDVALISO-01              
000573          MOVE +2                        TO CURR-KDCALL                   
000574          CALL W335CURR            USING CURR-W335CURR                    
000575          MOVE CURR-PRARTSJK-UT          TO SALES-PRARTNTO-BULK           
000576        END-IF                                                            
000577                                                                          
000578        MOVE 1                TO PRIS-KDCALL                              
000579        MOVE PROGRAM-NAMN     TO PRIS-IDPGM                               
000580        MOVE SPAR-IDARTNR     TO PRIS-IDARTNR                             
000581        MOVE SPAR-IDDISTR     TO PRIS-IDDISTR                             
000582        MOVE ZERO             TO PRIS-IDKUNDNR                            
000583        MOVE WC-CDC-SE        TO PRIS-IDDC                                
000584        MOVE +1               TO PRIS-KDORDKL                             
000585        MOVE +1               TO PRIS-KVBEART                             
000586        MOVE SPACE            TO PRIS-FLINVEST                            
000587                                                                          
000588        CALL W335PRIS USING PRIS-AREA                                     
000589                            PRIS-ARTC-PCB                                 
000590                            PRIS-WDK7-PCB                                 
000591                            PRIS-GMTA-PCB                                 
000592                            PRIS-BETA-PCB                                 
000593                            PRIS-GPRIA-PCB                                
000594                            PRIS-GPRIB-PCB                                
000595                            PRIS-COST-WDK6-PCB                            
000596                            PRIS-COST-WDK7-PCB                            
000597                            PRIS-COST-WDF1-PCB                            
000598                            PRIS-COST-9305-PCB                            
000599                            PRIS-COST-WDK72-PCB                           
000600                            PRIS-COST-WDB6-PCB                            
000602                                                                          
000603        MOVE PRIS-PRARTNTO    TO SALES-PRARTNTO-DO                        
000604        IF PRIS-KDVALISO = 'SEK' OR SPACE                                 
000605          CONTINUE                                                        
000606        ELSE                                                              
000607          MOVE PRIS-KDVALISO             TO CURR-KDVALISO-ROW             
000609          MOVE W-DATE-AAMM               TO CURR-TIAAMM                   
000610          MOVE WS-KDVALISO-HUV           TO CURR-KDVALISO-HUV             
000611          MOVE 'M'                       TO CURR-KDVALTYP                 
000612          CALL W510CURR USING CURR-W510CURR 9305-PCB                      
000613          IF CURR-KDSVAR = ' '                                            
000614            CONTINUE                                                      
000615          ELSE                                                            
000616            MOVE 1                       TO CURR-PRKURS-NEW               
000617          END-IF                                                          
000618                                                                          
000619          MOVE ZERO                      TO CURR-SUORDV-IN                
000620                                            CURR-PRARTVNA-IN              
000621                                            CURR-PRARTSTD-IN              
000622                                            CURR-PRKURS-02                
000623          MOVE CURR-PRKURS-NEW           TO CURR-PRKURS                   
000624          MOVE PRIS-PRARTNTO             TO CURR-PRARTSJK-IN              
000625          MOVE CURR-KDVALISO-ROW         TO CURR-KDVALISO-01              
000626          MOVE +2                        TO CURR-KDCALL                   
000627          CALL W335CURR            USING CURR-W335CURR                    
000628          MOVE CURR-PRARTSJK-UT          TO SALES-PRARTNTO-DO             
000629        END-IF                                                            
000630                                                                          
000631        MOVE SPAR-IDDISTR     TO SALES-IDDISTR                            
000632        MOVE SPAR-IDARTNR     TO SALES-IDARTNR                            
000633        MOVE WS-SULEVANT      TO SALES-SULEVANT                           
000634        MOVE WS-SUARTFSG      TO SALES-SUARTFSG                           
000635        MOVE WS-SULEVANT-DO   TO SALES-SULEVANT-DO                        
000636        MOVE WS-SUARTFSG-DO   TO SALES-SUARTFSG-DO                        
000637        MOVE WS-SULEVANT-KRE  TO SALES-SULEVANT-KRE                       
000638        MOVE WS-SUARTFSG-KRE  TO SALES-SUARTFSG-KRE                       
000639        DISPLAY 'DAGENS DATUM ' DAGENS-DATUM                              
000640        MOVE DAGENS-DATUM     TO SALES-DADATUM                            
000641        MOVE W330110-TIFSGVV  TO DATUM-TIFSGVV                            
000642        IF  DATUM-TIFSGVV (1:2)  < 60                                     
000643            MOVE 20           TO DATUM-SEKEL                              
000644        ELSE                                                              
000645            MOVE 19           TO DATUM-SEKEL                              
000646        END-IF                                                            
000647        MOVE DATUM-1          TO SALES-TIFSGVV                            
000648        PERFORM DE-SKRIV-POST-SALES                                       
000649     END-IF                                                               
000650     .                                                                    
000651     EJECT                                                                
000652                                                                          
000653 DE-SKRIV-POST-SALES SECTION.                                             
000654                                                                          
000655     WRITE SALES-POST FROM SALES-AREA                                     
000656     MOVE SPACE TO POSTSUM-TRANSTYP                                       
000657     MOVE 'W33018' TO POSTSUM-FDNAMN                                      
000658     MOVE 'W33010D4' TO POSTSUM-DDNAMN2                                   
000659     CALL POSTSUM USING POSTSUM-PARM                                      
000660     .                                                                    
000661     EJECT                                                                
000662                                                                          
000663 E-BEHANDLA-POST-TVAHUNDRA SECTION.                                       
000664                                                                          
000665     MOVE NEJ TO INDATA-FEL                                               
000666     MOVE IN200-IDARTNR TO W-IDARTNR                                      
000667     PERFORM IMS-GU-ARTNR                                                 
000668     IF SEGMENT-FINNS                                                     
000669       IF IN200-IDDISTR NOT = ZERO                                        
000670         CONTINUE                                                         
000671         MOVE IN200-IDDISTR TO W-IDDISTR-1                                
000672                               W-IDDISTR-2                                
000673                                                                          
000674         PERFORM IMS-GET-WLGMTA01                                         
000675         IF SEGMENT-FINNS                                                 
000676           CONTINUE                                                       
000677         ELSE                                                             
000678           MOVE JA TO INDATA-FEL                                          
000679           DISPLAY W-IDDISTR-1 ' DISTRNR FINNS-EJ '                       
000680           MOVE 312 TO W092-IDFELKODX                                     
000681         END-IF                                                           
000682       END-IF                                                             
000683     ELSE                                                                 
000684       MOVE JA TO INDATA-FEL                                              
000685       DISPLAY W-IDARTNR ' ARTNR FINNS-EJ '                               
000686       MOVE 311 TO W092-IDFELKODX                                         
000687                                                                          
000688     END-IF                                                               
000689     IF INDATA-FEL = NEJ                                                  
000690       PERFORM EA-FLYTTA-INAREA-TILL-UTAREA                               
000691       PERFORM EB-SKRIV-POST-200                                          
000692     ELSE                                                                 
000693       PERFORM EC-FLYTTA-TILL-FELAREA                                     
000694       PERFORM ED-SKRIV-POST-PA-TRATT                                     
000695     END-IF                                                               
000696     PERFORM S01-LAS-IN-FIL                                               
000697     .                                                                    
000698     EJECT                                                                
000699                                                                          
000700 EA-FLYTTA-INAREA-TILL-UTAREA SECTION.                                    
000701                                                                          
000702     MOVE IN200-IDPTYP     TO W330200-IDPTYP                              
000703     MOVE IN200-IDARTNR    TO W330200-IDARTNR                             
000704     MOVE IN200-IDDISTR    TO W330200-IDDISTR                             
000705     MOVE IN200-TIFSGVV    TO W330200-TIFSGVV                             
000706     MOVE IN200-KVLEVART   TO W330200-KVLEVART                            
000707     MOVE IN200-PRARTNTO   TO W330200-PRARTNTO                            
000708     MOVE IN200-PRARTSJK   TO W330200-PRARTSJK                            
000709     MOVE IN200-IDUSER     TO W330200-IDUSER                              
000710     .                                                                    
000711     EJECT                                                                
000712                                                                          
000713 EB-SKRIV-POST-200 SECTION.                                               
000714                                                                          
000715     WRITE JUST-POST FROM UT200-AREA                                      
000716     MOVE SPACE TO POSTSUM-TRANSTYP                                       
000717     MOVE 'W33011' TO POSTSUM-FDNAMN                                      
000718     MOVE 'W33010D2' TO POSTSUM-DDNAMN2                                   
000719     CALL POSTSUM USING POSTSUM-PARM                                      
000720     .                                                                    
000721     EJECT                                                                
000722 EC-FLYTTA-TILL-FELAREA SECTION.                                          
000723                                                                          
000724     MOVE IN200-IDPTYP  TO W092-IDPTYP                                    
000725     MOVE IN200-IDARTNR TO W092-SORTBGP                                   
000726                                                                          
000727     MOVE ZERO           TO W092-IDDISTR                                  
000728                            W092-IDKUNDNR                                 
000729                            W092-KDCLAGER                                 
000730                            W092-KDFRAKT                                  
000731                            W092-IDORDNR                                  
000732                            W092-KDORDKL                                  
000733                            W092-KDFELMRK                                 
000734     MOVE SPACE          TO W092-FILLER2                                  
000735                                                                          
000736     MOVE '200'          TO P-UPP-IDPTYP                                  
000737     MOVE IN200-IDARTNR  TO P-UPP-IDARTNR                                 
000738     MOVE IN200-IDDISTR  TO P-UPP-IDDISTR                                 
000739     MOVE IN200-TIFSGVV  TO P-UPP-TIFSGVV                                 
000740     MOVE IN200-KVLEVART TO P-UPP-KVLEVART                                
000741     MOVE IN200-PRARTNTO TO P-UPP-PRARTNTO                                
000742     MOVE IN200-PRARTSJK TO P-UPP-PRARTSJK                                
000743     MOVE IN200-IDUSER   TO P-UPP-IDUSER                                  
000744                                                                          
000745     MOVE PACKA-UPP-KORT TO FEL-KORT                                      
000746     .                                                                    
000747                                                                          
000748 ED-SKRIV-POST-PA-TRATT SECTION.                                          
000749                                                                          
000750     WRITE FEL-POST FROM FEL-AREA                                         
000751     MOVE SPACE TO POSTSUM-TRANSTYP                                       
000752     MOVE 'W33010' TO POSTSUM-FDNAMN                                      
000753     MOVE 'W33010D3' TO POSTSUM-DDNAMN2                                   
000754     CALL POSTSUM USING POSTSUM-PARM                                      
000755     .                                                                    
000756     EJECT                                                                
000757                                                                          
000758 Z-FINIT   SECTION.                                                       
000759     SKIP2                                                                
000760     CLOSE  W33010                                                        
000761            W33011                                                        
000762            W33010-FEL                                                    
000763            W33018                                                        
000764*    ----  SKRIV UT ANTAL LÄSTA OCH SKRIVNA POSTER                        
000765     MOVE 'S' TO POSTSUM-OPKOD                                            
000766     CALL POSTSUM USING POSTSUM-PARM                                      
000767     .                                                                    
000768     EJECT                                                                
000769                                                                          
000770 S01-LAS-IN-FIL SECTION.                                                  
000771     READ W33010 INTO IN0-AREA                                            
000772     AT END                                                               
000773         SET EOF-W33010 TO TRUE                                           
000774     END-READ                                                             
000775     IF NOT EOF-W33010                                                    
000776       MOVE SPACE TO POSTSUM-TRANSTYP                                     
000777       MOVE 'W33010' TO POSTSUM-FDNAMN                                    
000778       MOVE 'W33010D1' TO POSTSUM-DDNAMN2                                 
000779       CALL POSTSUM USING POSTSUM-PARM                                    
000780     END-IF                                                               
000781     .                                                                    
000782     EJECT                                                                
000783                                                                          
000784 S03-SUMTOT SECTION.                                                      
000785     IF IN-KDPRTYP = 'K'                                                  
000786       SUBTRACT IN-KVLEVART FROM WS-SULEVANT                              
000787       SUBTRACT IN-PRARTNTO FROM WS-SUARTFSG                              
000788     ELSE                                                                 
000789       ADD IN-KVLEVART TO WS-SULEVANT                                     
000790       ADD IN-PRARTNTO TO WS-SUARTFSG                                     
000791     END-IF                                                               
000792     .                                                                    
000793     EJECT                                                                
000794                                                                          
000795 S04-SUM-DAGORDER SECTION.                                                
000796                                                                          
000797     ADD IN-KVLEVART TO WS-SULEVANT-DO                                    
000798     ADD IN-PRARTNTO TO WS-SUARTFSG-DO                                    
000799     .                                                                    
000800                                                                          
000801 S05-SUM-MANUELLA-PRISER SECTION.                                         
000802                                                                          
000803     ADD IN-KVLEVART TO WS-SULEVANT-MAN                                   
000804     ADD IN-PRARTNTO TO WS-SUARTFSG-MAN                                   
000805     .                                                                    
000806                                                                          
000807 S06-SUM-RABATT-FUNKTIONSGRP SECTION.                                     
000808                                                                          
000809     ADD IN-KVLEVART TO WS-SULEVANT-RAB                                   
000810     ADD IN-PRARTNTO TO WS-SUARTFSG-RAB                                   
000811     .                                                                    
000812                                                                          
000813 S07-SUM-SPEC-PRIS SECTION.                                               
000814                                                                          
000815     ADD IN-KVLEVART TO WS-SULEVANT-SPEC                                  
000816     ADD IN-PRARTNTO TO WS-SUARTFSG-SPEC                                  
000817     .                                                                    
000818     EJECT                                                                
000819                                                                          
000820 S08-SUM-KRED SECTION.                                                    
000821                                                                          
000822     ADD IN-KVLEVART TO WS-SULEVANT-KRE                                   
000823     ADD IN-PRARTNTO TO WS-SUARTFSG-KRE                                   
000824     .                                                                    
000825     EJECT                                                                
000826                                                                          
000827*    ---- IMS SEKTIONER                                                   
000828                                                                          
000829 IMS-GET-WLGMTA01  SECTION.                                               
000830                                                                          
000831     STRING 'WLGMTA01(IDGMT    >' W-IDGMT-MIN-X ')'                       
000832                   '&IDGMT   <=' W-IDGMT-MAX-X ')'                        
000833             DELIMITED BY SIZE INTO SSA1                                  
000834     MOVE '  GE' TO GODK-STATUSKODER                                      
000835     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-B201 SSA1                      
000836     MOVE GMTA-STATUS-CODE  TO STATUS-WS                                  
000837     PERFORM IMS-STATUSKONTROLL                                           
000838     .                                                                    
000839     SKIP2                                                                
000840 IMS-GU-ARTNR SECTION.                                                    
000841                                                                          
000842     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
000843            DELIMITED BY SIZE INTO SSA1                                   
000844     MOVE '  GE'                TO GODK-STATUSKODER                       
000845     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
000846     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
000847     PERFORM IMS-STATUSKONTROLL                                           
000848     .                                                                    
000849     SKIP2                                                                
000850 IMS-HAMTA-SJALVKST SECTION.                                              
000851*    MOVE 'WLARTC11 ' TO SSA2                                             
000852                                                                          
000853     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
000854            DELIMITED BY SIZE INTO SSA1                                   
000855     STRING 'WLARTC11 '                                                   
000856            DELIMITED BY SIZE INTO SSA2                                   
000857     MOVE '  GE'                TO GODK-STATUSKODER                       
000858     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1 SSA2                 
000859     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
000860     PERFORM IMS-STATUSKONTROLL                                           
000861     .                                                                    
000862     SKIP2                                                                
000863 IMS-STATUSKONTROLL SECTION.                                              
000864                                                                          
000865     SET STATUS-IX TO 1                                                   
000866     SEARCH GODK-STATUS                                                   
000867       AT END CALL FELLOG                                                 
000868       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
000869     END-SEARCH                                                           
000870     .                                                                    
000871     EJECT                                                                
000880*    -COPY WY2000P3                                                       
