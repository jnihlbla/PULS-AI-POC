000001 ID DIVISION.                                                             
000002 PROGRAM-ID.     W6125000.                                                
000003 AUTHOR.         JOHAN LINDKVIST.                                         
000004 DATE-WRITTEN.   97/04/08.                                                
000005 DATE-COMPILED.                                                           
000006                                                                          
000007**   FUNKTION:                                                            
000008*        LÄSER WD06-DATABASEN DAGLIGEN FÖR ATT SAMLA SDC-HISTORIK         
000009*        OM ARTIKLAR TILL USA/CAN MED VISS IDDISTR.                       
000010*        TRE UTFILER:                                                     
000011*        W61251 : VARJE DAG. ALLA PT=310                                  
000012*        W61253 : VARJE DAG. VISSA R30 OCH R32 SAMT 310:ER                
000013*                                                                         
000014*        PROGRAMMET LÄSER      WLINLC (WDL6)                              
000015*        PROGRAMMET LÄSER      WLINLC (WDL6)                              
000016*                                                                         
000017*    ABENDKODER:                                                          
000018*        U0016 -  . . . .                                                 
000019*        U1000 -  . . . .                                                 
000020*                                                                         
000021*                                                                         
000022                                                                          
000023     SKIP3                                                                
000024 ENVIRONMENT DIVISION.                                                    
000025     SKIP2                                                                
000026 INPUT-OUTPUT SECTION.                                                    
000027                                                                          
000028 FILE-CONTROL.                                                            
000029     SKIP2                                                                
000030*          --- PT=310                                                     
000031     SELECT W61251                    ASSIGN TO W61250D1.                 
000032     SKIP2                                                                
000033*          --- R30 OCH R32                                                
000034     SELECT W61253                    ASSIGN TO W61250D3.                 
000035*          --- INFO TO ÄT TOT LISTA                                       
000036     SELECT W61254                    ASSIGN TO W61250D4.                 
000037*          --- FILE TO DATA-LINK                                          
000038     SELECT W61257                    ASSIGN TO W61250D5.                 
000039*          --- FILE TO DATA-LINK- <>R32                                   
000040     SELECT W61257A                   ASSIGN TO W61250D6.                 
000041     EJECT                                                                
000042 DATA DIVISION.                                                           
000043     SKIP2                                                                
000044 FILE SECTION.                                                            
000045     SKIP3                                                                
000046 FD  W61251                                                               
000047     RECORDING       F                                                    
000048     BLOCK CONTAINS  0.                                                   
000049                                                                          
000050*01  POST -COPY W61251 -PRE UT1- -L.                                      
000051     SKIP3                                                                
000052 FD  W61253                                                               
000053     RECORDING       F                                                    
000054     BLOCK CONTAINS  0.                                                   
000055                                                                          
000056*01  POST -COPY W61253  -PRE  UT3-  -L.                                   
000057     EJECT                                                                
000058 FD  W61254                                                               
000059     RECORDING       F                                                    
000060     BLOCK CONTAINS  0.                                                   
000061                                                                          
000062*01  POST -COPY W61250  -PRE  UT4-  -L.                                   
000063     EJECT                                                                
000064 FD  W61257                                                               
000065     RECORDING       F                                                    
000066     BLOCK CONTAINS  0.                                                   
000067                                                                          
000068*01  POST -COPY W61257  -PRE  UT5-  -L.                                   
000069     EJECT                                                                
000070 FD  W61257A                                                              
000071     RECORDING       F                                                    
000072     BLOCK CONTAINS  0.                                                   
000073                                                                          
000074*01  POST -COPY W61257  -PRE  UT6-  -L.                                   
000075     EJECT                                                                
000076 WORKING-STORAGE SECTION.                                                 
000077                                                                          
000078                                                                          
000079*    -- CHECKED BY WY2000                                                 
000080 77  IDPGM                       PIC X(8)    VALUE 'W6125000'.            
000081 77  JA                          PIC X       VALUE 'J'.                   
000082 77  NEJ                         PIC X       VALUE 'N'.                   
000083     EJECT                                                                
000084 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
000085 01  FILLER REDEFINES DAGENS-DATUM.                                       
000086     03  DAGENS-DATUM-AAR        PIC 9(2).                                
000087     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
000088     03  DAGENS-DATUM-DAG        PIC 9(2).                                
000089                                                                          
000090 01  SPAR-IDARTNR                PIC 9(8)    VALUE ZERO.                  
000091 01  INLEVNR-BERAK-X             PIC 9(16).                               
000092 01  INLEVNR-BERAK REDEFINES INLEVNR-BERAK-X.                             
000093     03 INLEV-SEKEL              PIC 9(2).                                
000094     03 INLEV-DATUM              PIC 9(6).                                
000095     03 INLEV-KLOCKA             PIC 9(4).                                
000096     03 FILLER                   PIC 9(4).                                
000097 01  FLSAMMA-DATUM               PIC X.                                   
000098                                                                          
000099*01  TESTAREA -COPY WWDIST35                                              
000100                                                                          
000101       EJECT                                                              
000102*        --- VALID IDDC CODES                                             
000103*                                                                         
000104*01      -COPY WWDC99                                                     
000105       EJECT                                                              
000106 01  DYNAMISKA-SUBPROGRAM.                                                
000107*                                                                         
000108     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
000109     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000110     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000111     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
000112     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
000113     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
000114     SKIP2                                                                
000115*01    -COPY WWDCKONS                                                     
000116     EJECT                                                                
000117*    --- PARAMETRAR TILL ABEND                                            
000118                                                                          
000119 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
000120 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
000121 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
000122     SKIP2                                                                
000123 01  FELTEXT.                                                             
000124     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
000125     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
000126     EJECT                                                                
000127*    --- PARAMETRAR TILL DATKORT                                          
000128*                                                                         
000129 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W61250'.              
000130     SKIP2                                                                
000131 01  DATUMKORT-ID                PIC X(6)    VALUE '000000'.              
000132     SKIP2                                                                
000133*01  -COPY WDATKORT                                                       
000134     EJECT                                                                
000135*    --- PARAMETRAR TILL POSTSUM                                          
000136*                                                                         
000137*01  -COPY W0005   -PRE  POSTSUM-                                         
000138     EJECT                                                                
000139*01  -COPY WDATAREA                                                       
000140     EJECT                                                                
000141 01  W61251-AREA-START           PIC X(24)   VALUE                        
000142                                 'W61251-AREA-START  '.                   
000143     SKIP2                                                                
000144                                                                          
000145*01  AREA -COPY W61251    -PRE UT1-                                       
000146     EJECT                                                                
000147 01  W61253-AREA-START           PIC X(24)   VALUE                        
000148                                 'W61253-AREA-START  '.                   
000149     SKIP2                                                                
000150                                                                          
000151*01  AREA -COPY W61253     -PRE UT3-                                      
000152     EJECT                                                                
000153 01  W61254-AREA-START           PIC X(24)   VALUE                        
000154                                 'W61254-AREA-START  '.                   
000155*01  AREA -COPY W61250    -PRE UT4-                                       
000156     EJECT                                                                
000157                                                                          
000158 01  W61257-AREA-START           PIC X(24)   VALUE                        
000159                                 'W61257-AREA-START  '.                   
000160*01  AREA -COPY W61257    -PRE UT5-                                       
000161     EJECT                                                                
000162 01  W61257A-AREA-START          PIC X(24)   VALUE                        
000163                                 'W61257A-AREA-START  '.                  
000164*01  AREA -COPY W61257    -PRE UT6-                                       
000165     EJECT                                                                
000166                                                                          
000167*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000168*                                                                         
000169     EJECT                                                                
000170 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000171     SKIP3                                                                
000172 01  NYCKLAR-TILL-DLI.                                                    
000173     03  W-IDARTNR-X.                                                     
000174         05  W-IDARTNR           PIC S9(8)   VALUE ZERO COMP-3.           
000175     03  W-WDL612KY-X.                                                    
000176         05  W-DAREGDAT          PIC 9(8)    VALUE ZERO.                  
000177         05  W-TIREGTID          PIC S9(7)   VALUE ZERO COMP-3.           
000178     SKIP2                                                                
000179*    --- STATUS-KOD FRÅN IMS                                              
000180 01  STATUS-WS                   PIC XX.                                  
000181     88  SEGMENT-FINNS                       VALUE '  '.                  
000182     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
000183     SKIP2                                                                
000184 01  GODK-STATUSKODER.                                                    
000185     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000186     SKIP3                                                                
000187 01  SSA1                        PIC X(64).                               
000188 01  SSA2                        PIC X(64).                               
000189     EJECT                                                                
000190*    --- IMS FUNKTIONSKODER                                               
000191*01  -COPY W0003                                                          
000192     EJECT                                                                
000193*    ---  DLI INPUT-OUTPUT AREA                                           
000194 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLINLC01'.                    
000195                                                                          
000196 01 DLI-IO-AREA.                                                          
000197     03 IO-AREA     PIC X(600) VALUE SPACE.                               
000198         03 DLI-IO-WLINLC01 REDEFINES IO-AREA.                            
000199*            05 -COPY WDL601 -PRE INLC-                                   
000200     EJECT                                                                
000201         03 DLI-IO-WLINLC11 REDEFINES IO-AREA.                            
000202*            05 -COPY WDL611 -PRE INLC-                                   
000203     EJECT                                                                
000204         03 DLI-IO-WLINLC12 REDEFINES IO-AREA.                            
000205             05 -COPY WDL612 -PRE INLC-                                   
000206     EJECT                                                                
000207 LINKAGE SECTION.                                                         
000208                                                                          
000209     EJECT                                                                
000210*01  -COPY W0008  -PRE INLC-                                              
000211     05  FILLER                  PIC X.                                   
000212     EJECT                                                                
000213 PROCEDURE DIVISION  USING INLC-PCB.                                      
000214 MAIN SECTION.                                                            
000215     ENTRY 'DLITCBL' USING INLC-PCB.                                      
000216                                                                          
000217     PERFORM A-INIT                                                       
000218     PERFORM IMS-GET-INLC                                                 
000219     PERFORM UNTIL SEGMENT-SAKNAS                                         
000220       EVALUATE INLC-SEG-NAME-FB                                          
000221       WHEN 'WDL601  '                                                    
000222         MOVE INLC-ART-IDARTNR  TO  SPAR-IDARTNR                          
000223       WHEN 'WDL611  '                                                    
000224         MOVE INLC-INL-IDDISTR  TO DIST35-IDDISTR                         
000225                                                                          
000226         MOVE INLC-INL-IDDC TO WS-IDDC                                    
000227         IF (NDC-NA OR NDC-NS) AND                                        
000228            (DIST35-REFILL-NA                                             
000228            OR DIST35-REFILL-NS                                           
000229            OR DIST35-NDCCN-CDC-REFILL                                    
000230            OR DIST35-NDCUS-CDC-REFILL                                    
000231            OR DIST35-REFILL-NA-JAP                                       
000232            OR DIST35-NA-TRANSFER                                         
000233            OR DIST35-NA-NDC-RETURNS                                      
000234            OR DIST35-NONVCC-NONVCC-REFILL                                
000235            OR DIST35-REFILL-INOM-NA) AND                                 
000236            (INLC-INL-IDPTYP = 'R30' OR '310' OR 'R32')                   
000237             OR DIST35-CDC-1C-REFILL                                      
000238             OR DIST35-CDC-GB-3A-REFILL                                   
000239             OR DIST35-CDC-NL-REFILL                                      
000240             OR DIST35-CDC-AT-REFILL                                      
000241             OR DIST35-CDC-ES-REFILL                                      
000242             OR DIST35-CDC-IT-REFILL                                      
000243             OR DIST35-CDC-TR-REFILL                                      
000243             OR DIST35-CDC-ZA-REFILL OR                                   
000244             (DIST35-CDC-LDC-REFILL)                                      
000245                                                                          
000246             PERFORM B-LAGRA-TESTDATA                                     
000247             PERFORM C-TILLDELNING                                        
000248                                                                          
000249             IF INLC-INL-IDPTYP = '310'                                   
000250               PERFORM S11-SKRIV-W61251                                   
000251               PERFORM S13-SKRIV-W61253                                   
000252             END-IF                                                       
000253                                                                          
000254             IF (INLC-INL-IDPTYP    =  'R30')        AND                  
000255                ((INLC-INL-KDFRAKT  =  17)           OR                   
000256                 (INLEV-DATUM       =  DAGENS-DATUM))                     
000257               PERFORM S13-SKRIV-W61253                                   
000258             END-IF                                                       
000259                                                                          
000260             IF (INLC-INL-IDPTYP    = 'R32'         AND                   
000261                  INLC-INL-FLMAKUL  = 'N'           AND                   
000262                  FLSAMMA-DATUM     = NEJ)                                
000263                 IF INLC-INL-TIINLINL = DAGENS-DATUM                      
000264                   IF INLC-INL-KVANTMOT = 0                               
000265                     CONTINUE                                             
000266                   ELSE                                                   
000267                     PERFORM S13-SKRIV-W61253                             
000268                   END-IF                                                 
000269                 END-IF                                                   
000270             END-IF                                                       
000271                                                                          
000272         ELSE                                                             
000273           CONTINUE                                                       
000274         END-IF                                                           
000275                                                                          
000276         PERFORM D-SKRIV-W61254                                           
000277                                                                          
000278         PERFORM E-SKRIV-W61257-W61257A                                   
000279                                                                          
000280       WHEN OTHER                                                         
000281         CONTINUE                                                         
000282       END-EVALUATE                                                       
000283                                                                          
000284       PERFORM IMS-GET-INLC                                               
000285     END-PERFORM                                                          
000286                                                                          
000287     PERFORM Z-FINIT                                                      
000288                                                                          
000289     MOVE ZERO TO RETURN-CODE                                             
000290     GOBACK                                                               
000291     .                                                                    
000292     EJECT                                                                
000293 A-INIT SECTION.                                                          
000294                                                                          
000295     OPEN OUTPUT W61251                                                   
000296                 W61253                                                   
000297                 W61254                                                   
000298                 W61257                                                   
000299                 W61257A                                                  
000300                                                                          
000301     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
000302     MOVE D-AAR       TO DAGENS-DATUM-AAR                                 
000303     MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                              
000304     MOVE D-DAG       TO DAGENS-DATUM-DAG                                 
000305     MOVE IDPGM       TO POSTSUM-PROGNAMN                                 
000306                                                                          
000307     .                                                                    
000308     EJECT                                                                
000309                                                                          
000310 B-LAGRA-TESTDATA SECTION.                                                
000311     SUBTRACT INLC-INL-DAINLEV FROM 9999999999999999                      
000312                                GIVING INLEVNR-BERAK-X                    
000313                                                                          
000314     IF NDC                                                               
000315        IF    (INLEV-DATUM  =  INLC-INL-TIINLINL) AND                     
000316              (INLEV-DATUM  =  INLC-INL-TIINLMOT)                         
000317            MOVE JA TO FLSAMMA-DATUM                                      
000318        ELSE                                                              
000319            MOVE NEJ TO FLSAMMA-DATUM                                     
000320        END-IF                                                            
000321     ELSE                                                                 
000322        MOVE NEJ TO FLSAMMA-DATUM                                         
000323     END-IF                                                               
000324                                                                          
000325     .                                                                    
000326     EJECT                                                                
000327 C-TILLDELNING SECTION.                                                   
000328     MOVE SPAR-IDARTNR        TO  UT1-SHIST-IDARTNR                       
000329     MOVE INLC-INL-FLPRIO     TO  UT1-SHIST-FLPRIO                        
000330     MOVE INLC-INL-IDDC       TO  UT1-SHIST-IDDC                          
000331     MOVE INLC-INL-IDFAKT     TO  UT1-SHIST-IDFAKT                        
000332     MOVE INLC-INL-IDKOLLI    TO  UT1-SHIST-IDKOLLI                       
000333     MOVE INLC-INL-KDVALISO   TO  UT1-SHIST-KDVALISO                      
000334     MOVE INLC-INL-KVAVIS     TO  UT1-SHIST-KVAVIS                        
000335     MOVE INLC-INL-PRARTNTO   TO  UT1-SHIST-PRARTNTO                      
000336     MOVE INLC-INL-PRKURS     TO  UT1-SHIST-PRKURS                        
000337     MOVE INLC-INL-IDORDNR5   TO  UT1-SHIST-IDORDNR5                      
000338     MOVE INLC-INL-IDKUNDNR   TO  UT1-SHIST-IDKUNDNR                      
000339                                                                          
000340     MOVE INLC-INL-IDPTYP     TO  UT3-SHIST-IDPTYP                        
000341     MOVE INLC-INL-IDDC       TO  UT3-SHIST-IDDC                          
000342     MOVE INLC-INL-IDDISTR    TO  UT3-SHIST-IDDISTR                       
000343     MOVE INLC-INL-IDFAKT     TO  UT3-SHIST-IDFAKT                        
000344     MOVE INLC-INL-IDKOLLI    TO  UT3-SHIST-IDKOLLI                       
000345     MOVE INLC-INL-IDKUNDNR   TO  UT3-SHIST-IDKUNDNR                      
000346     MOVE SPAR-IDARTNR        TO  UT3-SHIST-IDARTNR                       
000347     MOVE INLC-INL-PRARTNTO   TO  UT3-SHIST-PRARTNTO                      
000348     MOVE INLC-INL-PRKURS     TO  UT3-SHIST-PRKURS                        
000349     MOVE INLC-INL-KDVALISO   TO  UT3-SHIST-KDVALISO                      
000350     MOVE INLC-INL-KVAVIS     TO  UT3-SHIST-KVAVIS                        
000351     MOVE INLC-INL-KVANTMOT   TO  UT3-SHIST-KVANTMOT                      
000352     MOVE INLC-INL-TIBERANK   TO  UT3-SHIST-TIBERANK                      
000353     MOVE INLC-INL-FLPRIO     TO  UT3-SHIST-FLPRIO                        
000354     MOVE INLC-INL-KDFRAKT    TO  UT3-SHIST-KDFRAKT                       
000355     MOVE INLC-INL-IDORDNR5   TO  UT3-SHIST-IDORDNR5                      
000356     MOVE INLC-INL-TIINLMOT   TO  UT3-SHIST-TIINLMOT                      
000357     MOVE INLC-INL-TIINLMTI   TO  UT3-SHIST-TIINLMTI                      
000358     MOVE INLC-INL-TIINLINL   TO  UT3-SHIST-TIINLINL                      
000359     MOVE INLC-INL-TIINLITI   TO  UT3-SHIST-TIINLITI                      
000360     .                                                                    
000361 D-SKRIV-W61254 SECTION.                                                  
000362                                                                          
000363     IF INLC-INL-KDRT = +0                                                
000364        IF INLC-INL-IDDC = WC-CDC-SE                                      
000365                        OR WC-NDC-US-RU                                   
000366                        OR WC-NDC-JP-61                                   
000367                        OR WC-NDC-JP-6A                                   
000368                        OR WC-NDC-AU                                      
000369                        OR WC-NDC-IN                                      
000370                        OR WC-NDC-AE                                      
000371                        OR WC-NDC-TR                                      
000372                        OR WC-NDC-CN-71                                   
000373                        OR WC-NDC-CN-72                                   
000374           MOVE SPAR-IDARTNR        TO UT4-IDARTNR                        
000375           MOVE INLC-INL-IDDC       TO UT4-IDDC                           
000376           MOVE INLC-INL-TIBERANK   TO UT4-TIBERANK                       
000377           MOVE INLC-INL-TIINLMOT   TO UT4-TIINLMOT                       
000378           MOVE INLC-INL-TIAVIDAT   TO UT4-TIAVIDAT                       
000379                                                                          
000380           PERFORM S14-SKRIV-W61254                                       
000381        END-IF                                                            
000382     END-IF                                                               
000383     .                                                                    
000384                                                                          
000385 E-SKRIV-W61257-W61257A SECTION.                                          
000386                                                                          
000387*** FÖRST, KOLLA I DC-TABELL OM DC ÄR AKTUELLT                            
000388     IF  INLC-INL-IDPTYP = 'R32' OR                                       
               (INLC-INL-IDPTYP = 'R34' AND INLC-INL-TIINLINL > 0)              
000389        IF  INLC-INL-TIINLINL = DAGENS-DATUM                              
000392            MOVE SPAR-IDARTNR      TO UT5-IDARTNR                         
000393            MOVE INLC-INL-DAINLEV  TO UT5-DAINLEV                         
000394            MOVE INLC-INL-ADLAGOMR TO UT5-ADLAGOMR                        
000395            MOVE INLC-INL-ADGANG   TO UT5-ADGANG                          
000396            MOVE INLC-INL-ADPLATS  TO UT5-ADPLATS                         
000397            MOVE INLC-INL-FLMAKUL  TO UT5-FLMAKUL                         
000398            MOVE INLC-INL-FLPRIO   TO UT5-FLPRIO                          
000399            MOVE INLC-INL-FLSKAKOL TO UT5-FLSKAKOL                        
000400            MOVE INLC-INL-IDDC     TO UT5-IDDC                            
000401            MOVE INLC-INL-IDLEVNR  TO UT5-IDLEVNR                         
000402            MOVE INLC-INL-IDLOPNRM TO UT5-IDLOPNRM                        
000403            MOVE INLC-INL-IDFAKT   TO UT5-IDFAKT                          
000404            MOVE INLC-INL-IDKUNDRF TO UT5-IDKUNDRF                        
000405            MOVE INLC-INL-IDKOLLI  TO UT5-IDKOLLI                         
000406            MOVE INLC-INL-IDPTYP   TO UT5-IDPTYP                          
000407            MOVE INLC-INL-KDFRAKT  TO UT5-KDFRAKT                         
000408            MOVE INLC-INL-KDKOLLI  TO UT5-KDKOLLI                         
000409            MOVE INLC-INL-KDRT     TO UT5-KDRT                            
000410            MOVE INLC-INL-KDVALISO TO UT5-KDVALISO                        
000411            MOVE INLC-INL-KVANTMOT TO UT5-KVANTMOT                        
000412            MOVE INLC-INL-KVART-SKROT TO UT5-KVART-SKROT                  
000413            MOVE INLC-INL-KVAVIS   TO UT5-KVAVIS                          
000414            MOVE INLC-INL-PRARTNTO TO UT5-PRARTNTO                        
000415            MOVE INLC-INL-PRKURS   TO UT5-PRKURS                          
000416            MOVE INLC-INL-TIBERANK TO UT5-TIBERANK                        
000417            MOVE INLC-INL-TIINLMOT TO UT5-TIINLMOT                        
000418            MOVE INLC-INL-TIINLMTI TO UT5-TIINLMTI                        
000419            MOVE INLC-INL-TIINLINL TO UT5-TIINLINL                        
000420            MOVE INLC-INL-TIINLITI TO UT5-TIINLITI                        
000421            MOVE INLC-INL-ADINLOMR TO UT5-ADINLOMR                        
000422            MOVE INLC-INL-IDANALYS TO UT5-IDANALYS                        
000423            MOVE INLC-INL-IDKONTO  TO UT5-IDKONTO                         
000424            MOVE INLC-INL-IDKST    TO UT5-IDKST                           
000425            MOVE INLC-INL-IDUSER-003 TO UT5-IDUSER-003                    
000429            MOVE INLC-INL-KVRETUR  TO UT5-KVRETUR                         
000430            MOVE INLC-INL-KDAVVANT TO UT5-KDAVVANT                        
000431            MOVE INLC-INL-TIAVIDAT TO UT5-TIAVIDAT                        
000432            MOVE INLC-INL-IDDC-LEV TO UT5-IDDC-LEV                        
000432            MOVE INLC-INL-IDDISTR  TO UT5-IDDISTR                         
000432            MOVE INLC-INL-IDKUNDNR TO UT5-IDKUNDNR                        
000432            MOVE INLC-INL-FLTULLST TO UT5-FLTULLST                        
000432            MOVE INLC-INL-KVTULRET TO UT5-KVTULRET                        
000433                                                                          
000434            PERFORM S15-SKRIV-W61257                                      
000435        END-IF                                                            
000436     ELSE                                                                 
000437            MOVE SPAR-IDARTNR      TO UT6-IDARTNR                         
000438            MOVE INLC-INL-DAINLEV  TO UT6-DAINLEV                         
000439            MOVE INLC-INL-ADLAGOMR TO UT6-ADLAGOMR                        
000440            MOVE INLC-INL-ADGANG   TO UT6-ADGANG                          
000441            MOVE INLC-INL-ADPLATS  TO UT6-ADPLATS                         
000442            MOVE INLC-INL-FLMAKUL  TO UT6-FLMAKUL                         
000443            MOVE INLC-INL-FLPRIO   TO UT6-FLPRIO                          
000444            MOVE INLC-INL-FLSKAKOL TO UT6-FLSKAKOL                        
000445            MOVE INLC-INL-IDDC     TO UT6-IDDC                            
000446            MOVE INLC-INL-IDLEVNR  TO UT6-IDLEVNR                         
000447            MOVE INLC-INL-IDLOPNRM TO UT6-IDLOPNRM                        
000448            MOVE INLC-INL-IDFAKT   TO UT6-IDFAKT                          
000449            MOVE INLC-INL-IDKUNDRF TO UT6-IDKUNDRF                        
000450            MOVE INLC-INL-IDKOLLI  TO UT6-IDKOLLI                         
000451            MOVE INLC-INL-IDPTYP   TO UT6-IDPTYP                          
000452            MOVE INLC-INL-KDFRAKT  TO UT6-KDFRAKT                         
000453            MOVE INLC-INL-KDKOLLI  TO UT6-KDKOLLI                         
000454            MOVE INLC-INL-KDRT     TO UT6-KDRT                            
000455            MOVE INLC-INL-KDVALISO TO UT6-KDVALISO                        
000456            MOVE INLC-INL-KVANTMOT TO UT6-KVANTMOT                        
000457            MOVE INLC-INL-KVART-SKROT TO UT6-KVART-SKROT                  
000458            MOVE INLC-INL-KVAVIS   TO UT6-KVAVIS                          
000459            MOVE INLC-INL-PRARTNTO TO UT6-PRARTNTO                        
000460            MOVE INLC-INL-PRKURS   TO UT6-PRKURS                          
000461            MOVE INLC-INL-TIBERANK TO UT6-TIBERANK                        
000462            MOVE INLC-INL-TIINLMOT TO UT6-TIINLMOT                        
000463            MOVE INLC-INL-TIINLMTI TO UT6-TIINLMTI                        
000464            MOVE INLC-INL-TIINLINL TO UT6-TIINLINL                        
000465            MOVE INLC-INL-TIINLITI TO UT6-TIINLITI                        
000466            MOVE INLC-INL-ADINLOMR TO UT6-ADINLOMR                        
000467            MOVE INLC-INL-IDANALYS TO UT6-IDANALYS                        
000468            MOVE INLC-INL-IDKONTO  TO UT6-IDKONTO                         
000469            MOVE INLC-INL-IDKST    TO UT6-IDKST                           
000470            MOVE INLC-INL-IDUSER-003 TO UT6-IDUSER-003                    
000474            MOVE INLC-INL-KVRETUR  TO UT6-KVRETUR                         
000475            MOVE INLC-INL-KDAVVANT TO UT6-KDAVVANT                        
000476            MOVE INLC-INL-TIAVIDAT TO UT6-TIAVIDAT                        
000477            MOVE INLC-INL-IDDC-LEV TO UT6-IDDC-LEV                        
000432            MOVE INLC-INL-IDDISTR  TO UT6-IDDISTR                         
000432            MOVE INLC-INL-IDKUNDNR TO UT6-IDKUNDNR                        
000432            MOVE INLC-INL-FLTULLST TO UT6-FLTULLST                        
000432            MOVE INLC-INL-KVTULRET TO UT6-KVTULRET                        
000433                                                                          
000478                                                                          
000479            PERFORM S16-SKRIV-W61257A                                     
000480                                                                          
000482     END-IF                                                               
000483     .                                                                    
000484                                                                          
000485 Z-FINIT SECTION.                                                         
000486     CLOSE W61251                                                         
000487           W61253                                                         
000488           W61254                                                         
000489           W61257                                                         
000490           W61257A                                                        
000491     SKIP2                                                                
000492     MOVE 'S' TO POSTSUM-OPKOD                                            
000493     CALL POSTSUM USING POSTSUM-PARM                                      
000494     .                                                                    
000495     EJECT                                                                
000496 S11-SKRIV-W61251 SECTION.                                                
000497                                                                          
000498     WRITE UT1-POST FROM UT1-AREA                                         
000499                                                                          
000500     MOVE INLC-INL-IDPTYP TO POSTSUM-TRANSTYP                             
000501     MOVE 'W61251' TO POSTSUM-FDNAMN                                      
000502     MOVE 'W61250D1' TO POSTSUM-DDNAMN2                                   
000503     CALL POSTSUM USING POSTSUM-PARM                                      
000504     .                                                                    
000505     EJECT                                                                
000506 S13-SKRIV-W61253 SECTION.                                                
000507                                                                          
000508     WRITE UT3-POST FROM UT3-AREA                                         
000509                                                                          
000510     MOVE INLC-INL-IDPTYP TO POSTSUM-TRANSTYP                             
000511     MOVE 'W61253' TO POSTSUM-FDNAMN                                      
000512     MOVE 'W61250D3' TO POSTSUM-DDNAMN2                                   
000513     CALL POSTSUM USING POSTSUM-PARM                                      
000514     .                                                                    
000515     EJECT                                                                
000516 S14-SKRIV-W61254 SECTION.                                                
000517                                                                          
000518     WRITE UT4-POST FROM UT4-AREA                                         
000519                                                                          
000520     MOVE INLC-INL-IDPTYP TO POSTSUM-TRANSTYP                             
000521     MOVE 'W61254' TO POSTSUM-FDNAMN                                      
000522     MOVE 'W61250D4' TO POSTSUM-DDNAMN2                                   
000523     CALL POSTSUM USING POSTSUM-PARM                                      
000524     .                                                                    
000525     EJECT                                                                
000526 S15-SKRIV-W61257 SECTION.                                                
000527                                                                          
000528     WRITE UT5-POST FROM UT5-AREA                                         
000529                                                                          
000530     MOVE INLC-INL-IDPTYP TO POSTSUM-TRANSTYP                             
000531     MOVE 'W61257' TO POSTSUM-FDNAMN                                      
000532     MOVE 'W61250D5' TO POSTSUM-DDNAMN2                                   
000533     CALL POSTSUM USING POSTSUM-PARM                                      
000534     .                                                                    
000535     EJECT                                                                
000536 S16-SKRIV-W61257A SECTION.                                               
000537                                                                          
000538     WRITE UT6-POST FROM UT6-AREA                                         
000539                                                                          
000540     MOVE INLC-INL-IDPTYP TO POSTSUM-TRANSTYP                             
000541     MOVE 'W61257A'  TO POSTSUM-FDNAMN                                    
000542     MOVE 'W61250D6' TO POSTSUM-DDNAMN2                                   
000543     CALL POSTSUM USING POSTSUM-PARM                                      
000544     .                                                                    
000545     EJECT                                                                
000546* --- IMS SEKTIONER ---                                                   
000547     SKIP3                                                                
000548     EJECT                                                                
000549 IMS-GET-INLC SECTION.                                                    
000550                                                                          
000551     CALL CBLTDLI USING GN INLC-PCB DLI-IO-AREA                           
000552     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
000553     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
000554     PERFORM IMS-STATUSKONTROLL                                           
000555     .                                                                    
000556     EJECT                                                                
000557 IMS-STATUSKONTROLL SECTION.                                              
000558                                                                          
000559     SET STATUS-IX TO 1                                                   
000560     SEARCH GODK-STATUS                                                   
000561       AT END                                                             
000562         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
000563           DELIMITED BY SIZE INTO FELTEXT                                 
000564         DISPLAY FELTEXT                                                  
000565         CALL FELLOG                                                      
000566       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
000567         CONTINUE                                                         
000568     END-SEARCH                                                           
000569     .                                                                    
