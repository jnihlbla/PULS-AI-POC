000001 ID DIVISION.                                                             
000002 PROGRAM-ID.     W3031500.                                                
000003 AUTHOR.         THOMAS LARSSON.                                          
000004 DATE-WRITTEN.   93/12/08.                                                
000005 DATE-COMPILED.                                                           
000006                                                                          
000007*    FUNKTION:                                                            
000008*        VISAR ALLA 99 RABATTERNA FÖR VALT PRISOMRÅDE.                    
000009*                                                                         
000010*        PROGRAMMET LÄSER  WDC2 WDB1 OCH WDB2                             
000011*                                                                         
000012*    INDATA.                                                              
000013*        TRANSAKTION: W3T315                                              
000014*        MID:         W3I31501                                            
000015*                                                                         
000016*    UTDATA.                                                              
000017*        MOD:         W3O31501                                            
000018                                                                          
000019     SKIP3                                                                
000020 ENVIRONMENT DIVISION.                                                    
000021     EJECT                                                                
000022 DATA DIVISION.                                                           
000023 WORKING-STORAGE SECTION.                                                 
000024                                                                          
000025*    -- CHECKED BY WY2000                                                 
000026 77  IDPGM                       PIC X(08)   VALUE 'W3031500'.            
000027                                                                          
000028*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
000029 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
000030                                                                          
000031 77  JA                          PIC X       VALUE 'J'.                   
000032 77  NEJ                         PIC X       VALUE 'N'.                   
000033                                                                          
000034*    --- INDEX FÖR BLÄDDRINGSRADER                                        
000035 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
000036 77  K-INDX                      PIC S9(4)  VALUE +0    COMP SYNC.        
000037 77  MAX-INDX                    PIC S9(4)  VALUE +26   COMP SYNC.        
000038 77  RAB-INDX                    PIC S9(4)  VALUE +0    COMP SYNC.        
000039 77  MAX-RAB-INDX                PIC S9(4)  VALUE +99   COMP SYNC.        
000040 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
000041                                                                          
000042*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
000043                                                                          
000044 01  WS-IDDISTR                  PIC X(5)    VALUE SPACE.                 
000045 01  FILLER REDEFINES WS-IDDISTR.                                         
000046     03 FILLER                   PIC X(1).                                
000047     03 WS-IDDISTR-IN            PIC X(4).                                
000048                                                                          
000049 01  WS-IDPROMR                  PIC X(3)    VALUE SPACE.                 
000050 01  FILLER REDEFINES WS-IDPROMR.                                         
000051     03  WS-MARKBOLAG            PIC X(1).                                
000052     03  FILLER                  PIC X(2).                                
000053                                                                          
000054                                                                          
000055 01  DAGENS-DATUM                PIC 9(8)    VALUE ZERO.                  
000056                                                                          
000057 01  KOLL-SW                     PIC X       VALUE 'N'.                   
000058                                                                          
000059 77  PRISOMR-SW                  PIC X       VALUE 'J'.                   
000060     88  PRISOMR-FINNS                       VALUE 'J'.                   
000061     88  PRISOMR-SAKNAS                      VALUE 'N'.                   
000062                                                                          
000063 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
000064     88  NYCKLAR-OK                          VALUE 'J'.                   
000065     88  NYCKLAR-FEL                         VALUE 'N'.                   
000066                                                                          
000067 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
000068     88  EGEN-MID                            VALUE '3315'.                
000069     88  GODK-MID                            VALUE '3315'.                
000070     88  HELP-MID                            VALUE '0551'.                
000071     EJECT                                                                
000072*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
000073 01  GENERELLA-SUBPROGRAM.                                                
000074     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
000075     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000076     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000077     EJECT                                                                
000078*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
000079*01 -COPY WMEDAREA                                                        
000080     SKIP3                                                                
000081 01  MESSAGE-CODES.                                                       
000082     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
000083     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
000084     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
000085     03  ERR-PRICEAREA-MISSING   PIC X(3)    VALUE '236'.                 
000086     03  THIS-IS-THE-LAST-PAGE   PIC X(3)    VALUE '106'.                 
000087     EJECT                                                                
000088*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
000089*                                                                         
000090 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
000091     SKIP3                                                                
000092*01  MID -COPY W3I31501                                                   
000093     EJECT                                                                
000094 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
000095     SKIP3                                                                
000096*01  -COPY WMSGAREA                                                       
000097     EJECT                                                                
000098     03  MOD REDEFINES MSG-AREA.                                          
000099*      05  -COPY W3O31501                                                 
000100     EJECT                                                                
000101 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
000102     SKIP3                                                                
000103*01  -COPY WMFSAREA                                                       
000104     EJECT                                                                
000105*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000106*                                                                         
000107     SKIP2                                                                
000108 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000109     SKIP3                                                                
000110 01  NYCKLAR-TILL-DLI.                                                    
000111     03  W-IDPROMR-X.                                                     
000112         05  W-IDPROMR           PIC X(3)    VALUE SPACE.                 
000113     03  W-DASTADAT-X.                                                    
000114         05  W-DASTADAT          PIC 9(8)   VALUE ZERO.                   
000115     03  W-KDARTRAB-X.                                                    
000116         05  W-KDARTRAB          PIC  9(2)   VALUE ZERO.                  
000117*   NYCKLAR TILL KUNDREG             ***********                          
000118     03  W-IDGMT-MIN-X.                                                   
000119         05  W-IDDISTR-B1        PIC S9(5)   VALUE ZERO COMP-3.           
000120         05  W-IDKUNDNR-B1       PIC S9(7)   VALUE ZERO COMP-3.           
000121                                                                          
000122     03  W-IDGMT-MAX-X.                                                   
000123         05  W-IDDISTR-B2        PIC S9(5)   VALUE ZERO COMP-3.           
000124         05  W-IDKUNDNR-B2       PIC S9(7)   VALUE 9999999                
000125                                                        COMP-3.           
000126                                                                          
000127     03  W-IDDISTR-X.                                                     
000128         05  WA-IDDISTR  PIC S9(5)           COMP-3.                      
000129*   NYCKLAR TILL BETALNINGSREGISTRET ***********                          
000130     03  W-WDB101KY-X.                                                    
000131         05  W-WDB1-IDPARTNR     PIC X(9)    VALUE SPACE.                 
000132         05  W-WDB1-IDFTG        PIC 9(2)    VALUE ZERO.                  
000133                                                                          
000134*    --- STATUS-KOD FRÅN IMS                                              
000135 01  STATUS-WS                   PIC XX.                                  
000136     88  SEGMENT-FINNS                       VALUE '  '.                  
000137     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
000138     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000139     SKIP2                                                                
000140 01  GODK-STATUSKODER.                                                    
000141     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000142     SKIP3                                                                
000143 01  SSA1                        PIC X(64).                               
000144 01  SSA2                        PIC X(64).                               
000145     EJECT                                                                
000146*    --- IMS FUNKTIONSKODER                                               
000147*01  -COPY W0003                                                          
000148     EJECT                                                                
000149*    ---  DLI INPUT-OUTPUT AREA                                           
000150**   KUNDREGISTER                                                         
000151 01  FILLER               PIC X(16)   VALUE 'WDB201 AREA'.                
000152 01  DLI-IO-B201.                                                         
000153*     03  -COPY WDB201 -PRE WDB2-                                         
000154     EJECT                                                                
000155 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
000156     SKIP3                                                                
000158 01  DLI-IO-AREA.                                                         
000159     03  IO-AREA                 PIC X(850)  VALUE SPACE.                 
000160     SKIP3                                                                
000161     03  WDC201 REDEFINES IO-AREA.                                        
000162*        05  -COPY WDC201  -PRE WDC2-                                     
000163     SKIP3                                                                
000164     03  WDC213 REDEFINES IO-AREA.                                        
000165*        05  -COPY WDC213  -PRE WDC2-                                     
000166     EJECT                                                                
000171**   BETALNINGSREGISTER                                                   
000172 01  DLI-IO-AREA3.                                                        
000173     03  IO-AREA3                PIC X(400)  VALUE SPACE.                 
000174     SKIP3                                                                
000175     03  WDB101   REDEFINES IO-AREA3.                                     
000176*        05  -COPY WDB101  -PRE WDB1-                                     
000177     EJECT                                                                
000178 LINKAGE SECTION.                                                         
000179                                                                          
000180*01  -COPY W0009   -PRE MSG-                                              
000181     EJECT                                                                
000182*01  -COPY W0008  -PRE WDC2-                                              
000183     05  FILLER                  PIC X.                                   
000184     EJECT                                                                
000185*01  -COPY W0008  -PRE WDB2-                                              
000186     05  FILLER                  PIC X.                                   
000187     EJECT                                                                
000188*01  -COPY W0008  -PRE WDB1-                                              
000189     05  FILLER                  PIC X.                                   
000190     EJECT                                                                
000191 PROCEDURE DIVISION  USING MSG-PCB WDC2-PCB WDB2-PCB                      
000192     WDB1-PCB.                                                            
000193 MAIN SECTION.                                                            
000194     ENTRY 'DLITCBL' USING MSG-PCB WDC2-PCB WDB2-PCB                      
000195     WDB1-PCB.                                                            
000196                                                                          
000197     PERFORM IMS-GET-MSG                                                  
000198     IF SEGMENT-FINNS                                                     
000199       PERFORM A-INIT                                                     
000200       PERFORM B-KOLLA-NYCKLAR                                            
000201       IF NYCKLAR-OK                                                      
000202           IF MFS-FIRST                                                   
000203             PERFORM C-FOERSTA-SIDA                                       
000204           ELSE                                                           
000205             IF MFS-NEXT                                                  
000206               PERFORM D-NAESTA-SIDA                                      
000207             ELSE                                                         
000208               PERFORM E-SAMMA-SIDA                                       
000209             END-IF                                                       
000210           END-IF                                                         
000211         PERFORM F-LAES-VISA-INFO                                         
000212       END-IF                                                             
000213       COMPUTE MSG-KVLL = LENGTH OF MOD-W3O31501 + 4                      
000214       PERFORM IMS-INSERT-MSG                                             
000215     END-IF                                                               
000216                                                                          
000217     MOVE ZERO TO RETURN-CODE                                             
000218     GOBACK                                                               
000219     .                                                                    
000220     EJECT                                                                
000221 A-INIT SECTION.                                                          
000222                                                                          
000223     IF MSG-DUBBLA-TRANSKODER                                             
000224       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I31501                 
000225       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
000226       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
000227     ELSE                                                                 
000228       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W3I31501                  
000229       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
000230       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
000231     END-IF                                                               
000232                                                                          
000233     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
000234     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
000235     MOVE MFS-IDTRANS TO W-IDTRANS                                        
000236                                                                          
000237     MOVE LOW-VALUE TO MSG-AREA                                           
000238     MOVE 'W3O315N1' TO MFS-IDMOD                                         
000239     MOVE '3315' TO MOD-IDTRANS                                           
000240     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
000241                                                                          
000242     IF EGEN-MID OR HELP-MID                                              
000243       CONTINUE                                                           
000244     ELSE                                                                 
000245       MOVE SPACE TO MFS-KDTRTYP                                          
000246       MOVE '7' TO MFS-IDPFK                                              
000247     END-IF                                                               
000248                                                                          
000249     IF ENGLISH-TEXT                                                      
000250       MOVE +2 TO SPRAK-IX                                                
000251       MOVE 'GB ' TO MED-IDSKYLT                                          
000252     ELSE                                                                 
000253       MOVE +1 TO SPRAK-IX                                                
000254       MOVE 'S  ' TO MED-IDSKYLT                                          
000255     END-IF                                                               
000256                                                                          
000257     MOVE FUNCTION CURRENT-DATE(1:8) TO  DAGENS-DATUM                     
000258     MOVE DAGENS-DATUM TO W-DASTADAT                                      
000259     MOVE NEJ TO KOLL-SW                                                  
000260     .                                                                    
000261     EJECT                                                                
000262 B-KOLLA-NYCKLAR SECTION.                                                 
000263                                                                          
000264     MOVE JA TO NYCKLAR-SW                                                
000265     MOVE JA TO PRISOMR-SW                                                
000266                                                                          
000267*    -- KONTROLL AV IDPROMR                                               
000268     MOVE MFS-RENSA-FAELT TO MOD-IDPROMR-IN                               
000269                                                                          
000270     IF MID-IDPROMR-IN = ALL '+'                                          
000271       MOVE MID-IDPROMR-UT TO WS-IDPROMR                                  
000272     ELSE                                                                 
000273       MOVE MID-IDPROMR-IN TO WS-IDPROMR                                  
000274       MOVE SPACE       TO MID-IDDISTR-UT                                 
000275       MOVE '7'         TO MFS-IDPFK                                      
000276       MOVE SPACE       TO MFS-KDTRTYP                                    
000277     END-IF                                                               
000278                                                                          
000279     IF WS-IDPROMR NOT = SPACE                                            
000280       MOVE WS-IDPROMR TO W-IDPROMR                                       
000281     END-IF                                                               
000282                                                                          
000283*    -- KONTROLL AV IDDISTR                                               
000284     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
000285                                                                          
000286     IF MID-IDDISTR-IN = ALL '+'                                          
000287       MOVE MID-IDDISTR-UT TO WS-IDDISTR-IN                               
000288     ELSE                                                                 
000289       MOVE MID-IDDISTR-IN TO WS-IDDISTR-IN                               
000290       MOVE '7'         TO MFS-IDPFK                                      
000291       MOVE SPACE       TO MFS-KDTRTYP                                    
000292     END-IF                                                               
000293                                                                          
000294     INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                   
000295                                                                          
000296     IF WS-IDDISTR NUMERIC                                                
000297       IF WS-IDDISTR > ZERO                                               
000298         MOVE WS-IDDISTR TO WA-IDDISTR                                    
000299                                                                          
000300                            W-IDDISTR-B1                                  
000301                            W-IDDISTR-B2                                  
000302        END-IF                                                            
000303     END-IF                                                               
000304                                                                          
000305     IF WS-IDPROMR = SPACE                                                
000306        IF WS-IDDISTR NUMERIC                                             
000307           IF WS-IDDISTR > ZERO                                           
000308              MOVE NEJ TO PRISOMR-SW                                      
000309           ELSE                                                           
000310              MOVE NEJ TO NYCKLAR-SW                                      
000311           END-IF                                                         
000312        ELSE                                                              
000313           MOVE NEJ TO NYCKLAR-SW                                         
000314        END-IF                                                            
000315     ELSE                                                                 
000316        IF WS-IDDISTR NUMERIC                                             
000317           IF WS-IDDISTR > ZERO                                           
000318              MOVE NEJ TO PRISOMR-SW                                      
000319           END-IF                                                         
000320        END-IF                                                            
000321     END-IF                                                               
000322     IF EGEN-MID OR GODK-MID                                              
000323        CONTINUE                                                          
000324     ELSE                                                                 
000325        MOVE NEJ TO NYCKLAR-SW                                            
000326     END-IF                                                               
000327                                                                          
000328     IF NYCKLAR-OK                                                        
000329       IF EGEN-MID OR GODK-MID                                            
000330          IF PRISOMR-SAKNAS                                               
000331            MOVE MFS-RENSA-FAELT TO MOD-IDPROMR-UT                        
000332            MOVE SPACE           TO W-IDPROMR                             
000333            MOVE WS-IDDISTR-IN   TO MOD-IDDISTR-UT                        
000334            INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE        
000335          ELSE                                                            
000336            MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                        
000337            MOVE WS-IDPROMR      TO MOD-IDPROMR-UT                        
000338          END-IF                                                          
000339       ELSE                                                               
000340         MOVE MFS-RENSA-FAELT TO MOD-IDPROMR-UT                           
000341         MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                           
000342       END-IF                                                             
000343     ELSE                                                                 
000344       IF EGEN-MID OR GODK-MID                                            
000345         MOVE WS-IDPROMR    TO MOD-IDPROMR-UT                             
000346         MOVE WS-IDDISTR-IN TO MOD-IDDISTR-UT                             
000347         INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE           
000348       ELSE                                                               
000349         MOVE MFS-RENSA-FAELT TO MOD-IDPROMR-UT                           
000350         MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                           
000351       END-IF                                                             
000352     END-IF                                                               
000353                                                                          
000354     IF NYCKLAR-FEL                                                       
000355       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
000356       CALL WMEDKONV USING MED-WMEDAREA                                   
000357       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
000358       PERFORM MFS-RENSA-FAELT-UT                                         
000359     END-IF                                                               
000360     .                                                                    
000361     EJECT                                                                
000362 C-FOERSTA-SIDA SECTION.                                                  
000363                                                                          
000364     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
000365     CALL WMEDKONV USING MED-WMEDAREA                                     
000366     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
000367                                                                          
000368*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
000369*    MOVE SPACE TO W-IDPROMR                                              
000370     MOVE +1    TO RAB-INDX                                               
000371     .                                                                    
000372     EJECT                                                                
000373 D-NAESTA-SIDA SECTION.                                                   
000374                                                                          
000375     MOVE MID-IDPROMR-SPAR TO W-IDPROMR                                   
000376     MOVE MID-KDARTRAB-NEXT TO RAB-INDX                                   
000377     MOVE NEJ               TO KOLL-SW                                    
000378     .                                                                    
000379     EJECT                                                                
000380 E-SAMMA-SIDA SECTION.                                                    
000381                                                                          
000382     IF EGEN-MID OR HELP-MID                                              
000383       MOVE MID-IDPROMR-SPAR TO W-IDPROMR                                 
000384       MOVE MID-KDARTRAB-ENTER TO RAB-INDX                                
000385       MOVE NEJ             TO KOLL-SW                                    
000386     END-IF                                                               
000387     .                                                                    
000388     EJECT                                                                
000389 F-LAES-VISA-INFO SECTION.                                                
000390                                                                          
000391     IF PRISOMR-SAKNAS                                                    
000392        PERFORM FA-HT-PROM-VIA-DISTRIKT                                   
000393     END-IF                                                               
000394                                                                          
000395     PERFORM IMS-GU-WDC201                                                
000396                                                                          
000397     IF SEGMENT-SAKNAS                                                    
000398       MOVE ERR-PRICEAREA-MISSING TO MED-IDMFSFEL                         
000399       CALL WMEDKONV USING MED-WMEDAREA                                   
000400       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
000401       PERFORM MFS-RENSA-FAELT-UT                                         
000402     ELSE                                                                 
000403       MOVE +1 TO INDX                                                    
000404                  K-INDX                                                  
000405       MOVE WDC2-PRO-IDPROMR TO MOD-IDPROMR-SPAR                          
000406       PERFORM IMS-GNP-WDC213                                             
000407       IF SEGMENT-FINNS                                                   
000408         MOVE WDC2-RAB-KDARTRAB(RAB-INDX) TO MOD-KDARTRAB-ENTER           
000409       ELSE                                                               
000410         MOVE SPACE            TO MOD-IDPROMR-SPAR                        
000411         MOVE ZERO             TO MOD-KDARTRAB-ENTER                      
000412       END-IF                                                             
000413                                                                          
000414       PERFORM UNTIL K-INDX > MAX-INDX OR RAB-INDX > MAX-RAB-INDX         
000415         IF SEGMENT-FINNS                                                 
000416           MOVE WDC2-RAB-DASTADAT(3:6) TO MOD-TISTADAT                    
000417           MOVE WDC2-RAB-KDARTRAB (RAB-INDX) TO MOD-NOR-RAB (INDX)        
000418           MOVE WDC2-RAB-REARTRAB-DO (RAB-INDX) TO                        
000419                MOD-DO-NOR-RAB (INDX)                                     
000420           MOVE WDC2-RAB-REARTRAB-BULK (RAB-INDX) TO                      
000421                MOD-MO-NOR-RAB (INDX)                                     
000422         ELSE                                                             
000423           MOVE MFS-RENSA-FAELT TO MOD-NOR-RAB    (INDX)                  
000424                                   MOD-DO-NOR-RAB (INDX)                  
000425                                   MOD-MO-NOR-RAB (INDX)                  
000426         END-IF                                                           
000427                                                                          
000428         ADD 2 TO INDX                                                    
000429         ADD 1 TO K-INDX                                                  
000430         ADD 1 TO RAB-INDX                                                
000431                                                                          
000432         IF K-INDX > 13 AND KOLL-SW = NEJ                                 
000433           MOVE JA TO KOLL-SW                                             
000434           MOVE +2 TO INDX                                                
000435         END-IF                                                           
000436                                                                          
000437       END-PERFORM                                                        
000438                                                                          
000439       IF SEGMENT-FINNS AND RAB-INDX < MAX-RAB-INDX                       
000440         MOVE WDC2-RAB-KDARTRAB (RAB-INDX) TO MOD-KDARTRAB-NEXT           
000441         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
000442         CALL WMEDKONV USING MED-WMEDAREA                                 
000443         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
000444       ELSE                                                               
000445         MOVE  1               TO MOD-KDARTRAB-NEXT                       
000446         MOVE THIS-IS-THE-LAST-PAGE TO MED-IDMFSINF                       
000447         CALL WMEDKONV USING MED-WMEDAREA                                 
000448         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
000449       END-IF                                                             
000450                                                                          
000451     END-IF                                                               
000452     .                                                                    
000453     EJECT                                                                
000454 FA-HT-PROM-VIA-DISTRIKT SECTION.                                         
000455                                                                          
000456     PERFORM IMS-GET-WDB201                                               
000457     IF SEGMENT-FINNS                                                     
000458       MOVE WDB2-GMT-IDPARTNR   TO W-WDB1-IDPARTNR                        
000459       MOVE WDB2-GMT-IDFTG      TO W-WDB1-IDFTG                           
000460       PERFORM IMS-GU-WDB101                                              
000461       IF SEGMENT-FINNS                                                   
000462         MOVE WDB1-BET-IDPROMR  TO WS-IDPROMR                             
000463         MOVE WS-IDPROMR        TO W-IDPROMR                              
000464       END-IF                                                             
000465     END-IF                                                               
000466     .                                                                    
000467     EJECT                                                                
000468                                                                          
000469 MFS-RENSA-FAELT-UT SECTION.                                              
000470                                                                          
000471*    --- ALLA UTDATA-FÄLT                                                 
000472*    --- INKL. BLÄDDRINGSNYCKLAR                                          
000473     MOVE MFS-RENSA-FAELT TO MOD-IDPROMR-SPAR                             
000474                             MOD-KDARTRAB-ENTER                           
000475                             MOD-KDARTRAB-NEXT                            
000476                             MOD-TISTADAT                                 
000477     MOVE +1 TO INDX                                                      
000478     PERFORM UNTIL INDX > MAX-INDX                                        
000479       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
000480       ADD +1 TO INDX                                                     
000481     END-PERFORM                                                          
000482     SKIP2                                                                
000483     EJECT                                                                
000484     .                                                                    
000485     SKIP2                                                                
000486 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
000487                                                                          
000488*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
000489     MOVE MFS-RENSA-FAELT TO MOD-NOR-RAB    (INDX)                        
000490                             MOD-DO-NOR-RAB (INDX)                        
000491                             MOD-MO-NOR-RAB (INDX)                        
000492     .                                                                    
000493     SKIP2                                                                
000494* --- IMS SEKTIONER ---                                                   
000495     SKIP3                                                                
000496 IMS-GET-MSG SECTION.                                                     
000497                                                                          
000498     MOVE '  QC' TO GODK-STATUSKODER                                      
000499     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
000500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
000501     PERFORM IMS-STATUSKONTROLL                                           
000502     .                                                                    
000503     SKIP3                                                                
000504 IMS-INSERT-MSG SECTION.                                                  
000505                                                                          
000506     IF NOT ENGLISH-TEXT                                                  
000507       MOVE '0' TO MFS-KDHUVOMR                                           
000508     END-IF                                                               
000509     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
000510     MOVE SPACE TO GODK-STATUSKODER                                       
000511     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
000512     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
000513     PERFORM IMS-STATUSKONTROLL                                           
000514     .                                                                    
000515     EJECT                                                                
000516 IMS-GU-WDC201 SECTION.                                                   
000517                                                                          
000518     STRING 'WDC201  (IDPROMR  =' W-IDPROMR-X ')'                         
000519          DELIMITED BY SIZE INTO SSA1                                     
000520     MOVE '  GE' TO GODK-STATUSKODER                                      
000521     CALL CBLTDLI USING GU WDC2-PCB DLI-IO-AREA SSA1                      
000522     MOVE WDC2-STATUS-CODE TO STATUS-WS                                   
000523     PERFORM IMS-STATUSKONTROLL                                           
000524     .                                                                    
000525     EJECT                                                                
000526 IMS-GNP-WDC213 SECTION.                                                  
000527                                                                          
000528     STRING 'WDC213  (DASTADAT<=' W-DASTADAT-X ')'                        
000529          DELIMITED BY SIZE INTO SSA1                                     
000530     MOVE '  GE' TO GODK-STATUSKODER                                      
000531     CALL CBLTDLI USING GNP WDC2-PCB DLI-IO-AREA SSA1                     
000532     MOVE WDC2-STATUS-CODE TO STATUS-WS                                   
000533     PERFORM IMS-STATUSKONTROLL                                           
000534     .                                                                    
000535     EJECT                                                                
000536 IMS-GET-WDB201    SECTION.                                               
000537                                                                          
000538     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
000539                   '&IDGMT   <=' W-IDGMT-MAX-X ')'                        
000540             DELIMITED BY SIZE INTO SSA1                                  
000541     MOVE '  GE' TO GODK-STATUSKODER                                      
000542     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-B201 SSA1                      
000543     MOVE WDB2-STATUS-CODE  TO STATUS-WS                                  
000544     PERFORM IMS-STATUSKONTROLL                                           
000545     .                                                                    
000546     SKIP2                                                                
000547 IMS-GU-WDB101 SECTION.                                                   
000548                                                                          
000549     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
000550          DELIMITED BY SIZE INTO SSA1                                     
000551     MOVE '  GE' TO GODK-STATUSKODER                                      
000552     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-AREA3 SSA1                     
000553     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
000554     PERFORM IMS-STATUSKONTROLL                                           
000555     .                                                                    
000556     SKIP3                                                                
000557 IMS-STATUSKONTROLL SECTION.                                              
000558                                                                          
000559     SET STATUS-IX TO 1                                                   
000560     SEARCH GODK-STATUS                                                   
000561       AT END                                                             
000562         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
000563         DELIMITED BY SIZE INTO FELTEXT                                   
000564         CALL FELLOG                                                      
000565       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
000566         CONTINUE                                                         
000567     END-SEARCH                                                           
000568     .                                                                    
