000001 ID DIVISION.                                                             
000002                                                                          
000003 PROGRAM-ID.     W4072500.                                                
000004 AUTHOR.         LASSE CALAIS.                                            
000005 DATE-WRITTEN.   95/05/02.                                                
000006 DATE-COMPILED.                                                           
000007                                                                          
000008*    FUNKTION:                                                            
000009*        LÄSER FAKTURAHISTORIK FRÅN WDL5                                  
000010*                                                                         
000011*        PROGRAMMET LÄSER        WDL5                                     
000012*        PROGRAMMET LÄSER        WDK6                                     
000013*        PROGRAMMET LÄSER        WDK7                                     
000014*        PROGRAMMET LÄSER        WDB6                                     
000015*                                                                         
000016*                                                                         
000017*    E-TRACKER 8687963 DATE  2010-03-18 REFERRALS PICKING AREA            
000018*    STORY             DATE  2024-08-01 ADAP.TO THE NEW WDL5              
000019*                                                                         
000020*                                                                         
000021*    INDATA.                                                              
000022*        TRANSAKTION: W4T725                                              
000023*        MID:         W4I72501                                            
000024*                                                                         
000025*    UTDATA.                                                              
000026*        MOD:         W4O72501                                            
000027                                                                          
000028                                                                          
000029 ENVIRONMENT DIVISION.                                                    
000030     EJECT                                                                
000031 DATA DIVISION.                                                           
000032 WORKING-STORAGE SECTION.                                                 
000033*    -- CHECKED BY WY2000                                                 
000034     SKIP3                                                                
000035 77  IDPGM                       PIC X(08)   VALUE 'W4072500'.            
000036 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
000037 77  JA                          PIC X       VALUE 'J'.                   
000038 77  NEJ                         PIC X       VALUE 'N'.                   
000039 77  DBS-SECTION                 PIC X(30)   VALUE SPACE.                 
000040 77  W-VKARTNTO                  PIC 9(4)V9(3) VALUE ZERO.                
000041 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
000042 77  MAX-INDX                    PIC S9(4)  VALUE +14   COMP SYNC.        
000043 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +0    COMP SYNC.        
000044                                                                          
000045*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
000046 77  WS-IDFAKT                   PIC X(7)    VALUE SPACE.                 
000047 77  WS-IDKUNDNR                 PIC X(6)    VALUE SPACE.                 
000048 77  WS-IDORDNR                  PIC X(5)    VALUE SPACE.                 
000049 77  WS-IDKOLLI                  PIC X(5)    VALUE SPACE.                 
000050 77  WS-IDPRODNR                 PIC X(7)    VALUE SPACE.                 
000051 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
000052 77  WS-VKART                    PIC S9(7)  VALUE ZERO.                   
000053                                                                          
000054 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
000055     88  NYCKLAR-OK                          VALUE 'J'.                   
000056     88  NYCKLAR-FEL                         VALUE 'N'.                   
000057                                                                          
000058 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
000059     88  EGEN-MID                            VALUE '4725'.                
000060     88  GODK-MID                            VALUE '4721' '4722'          
000061                                                   '4723' '4724'          
000062                                                   '4725' '4726'          
000063                                                   '4727' '4728'          
000064                                                   '4729'.                
000065     88  HELP-MID                            VALUE '0551'.                
000066     EJECT                                                                
000067                                                                          
000068                                                                          
000069*                                                                         
000070 01  TEST-IDDISTR                PIC 9(5)   VALUE ZERO COMP-3.            
000071*01  FILLER  -COPY WWDIST79      -RED TEST-IDDISTR.                       
000072*                                                                         
000073*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
000074 01  GENERELLA-SUBPROGRAM.                                                
000075     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
000076     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000077     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000078     EJECT                                                                
000079*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
000080*01 -COPY WMEDAREA                                                        
000081                                                                          
000082 01  MESSAGE-CODES.                                                       
000083     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
000084     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
000085     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
000086     03  ERR-KEY-MISSING         PIC X(3)    VALUE '005'.                 
000087     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
000088     EJECT                                                                
000089*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
000090*                                                                         
000091 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
000092                                                                          
000093*01  MID -COPY W4I72501                                                   
000094     EJECT                                                                
000095 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
000096                                                                          
000097*01  -COPY WMSGAREA                                                       
000098     EJECT                                                                
000099     03  MOD REDEFINES MSG-AREA.                                          
000100*      05  -COPY W4O72501                                                 
000101     EJECT                                                                
000102 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
000103                                                                          
000104*01  -COPY WMFSAREA                                                       
000105     EJECT                                                                
000106*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000107 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000108                                                                          
000109 01  NYCKLAR-TILL-DLI.                                                    
000110                                                                          
000111     03  W-IDFAKT-X.                                                      
000112         07 W-IDFAKT             PIC S9(7)   COMP-3.                      
000113                                                                          
000114     03  W-WDL511KY-X.                                                    
000115         05  W-IDPRODNR          PIC S9(7)   COMP-3.                      
000116         05  W-IDKOLLI           PIC S9(5)   COMP-3.                      
000117                                                                          
000118     03  W-IDPRODNR-MIN-X.                                                
000119         05  W-IDPRODNR-MIN      PIC S9(7)   VALUE ZERO COMP-3.           
000120                                                                          
000130     03  W-IDPRODNR-MAX-X.                                                
000131         05  W-IDPRODNR-MAX      PIC S9(7)   VALUE ZERO COMP-3.           
000132                                                                          
000133     03  W-IDKOLLI-MIN-X.                                                 
000134         05  W-IDKOLLI-MIN       PIC S9(5)   VALUE ZERO COMP-3.           
000135                                                                          
000136     03  W-IDKOLLI-MAX-X.                                                 
000137         05  W-IDKOLLI-MAX       PIC S9(5)   VALUE ZERO COMP-3.           
000138                                                                          
000139     03  W-IDKUNDNR-MIN-X.                                                
000140         05  W-IDKUNDNR-MIN      PIC S9(7)   VALUE ZERO COMP-3.           
000141                                                                          
000142     03  W-IDKUNDNR-MAX-X.                                                
000143         05  W-IDKUNDNR-MAX      PIC S9(7)   VALUE ZERO COMP-3.           
000144                                                                          
000145     03  W-IDORDNR-MIN-X.                                                 
000146         05  W-IDORDNR-MIN       PIC  9(7)   VALUE ZERO.                  
000147                                                                          
000148     03  W-IDORDNR-MAX-X.                                                 
000149         05  W-IDORDNR-MAX       PIC  9(7)   VALUE ZERO.                  
000150                                                                          
000151     03  W-WDL521KY-X.                                                    
000152         05  W-IDARTNR-L5        PIC S9(9)   COMP-3.                      
000153         05  W-IDPURAD-L5        PIC S9(5)   COMP-3.                      
000154                                                                          
000155     03  W-WDL521KY-MIN-X.                                                
000156         05  W-IDARTNR-MIN       PIC S9(9)   COMP-3.                      
000157         05  W-IDPURAD-MIN       PIC S9(5)   COMP-3 VALUE +0.             
000158                                                                          
000159     03  W-WDL521KY-MAX-X.                                                
000160         05  W-IDARTNR-MAX       PIC S9(9)   COMP-3.                      
000161         05  W-IDPURAD-MAX       PIC S9(5)   COMP-3 VALUE +99999.         
000162                                                                          
000163     03  W-KDSEGKEY-X.                                                    
000164         05  W-KDSEGKEY          PIC  X      VALUE '1'.                   
000165                                                                          
000166     03  W-IDARTNR-X.                                                     
000167         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
000168                                                                          
000169     03  W-IDDC-X.                                                        
000170         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
000180                                                                          
000190     EJECT                                                                
000191*    -NYCKLAR TIL WDB201                                                  
000192     03  W-IDGMT-X.                                                       
000193       05  W-IDDISTR-WDB2        PIC S9(5) VALUE ZERO COMP-3.             
000194       05  W-IDKUNDNR-WDB2       PIC S9(7) VALUE ZERO COMP-3.             
000195     03  W-IDGMT-MIN-X.                                                   
000196       05  W-IDDISTR-WDB2-MIN    PIC S9(5) VALUE ZERO COMP-3.             
000197       05  W-IDKUNDNR-WDB2-MIN   PIC S9(7) VALUE ZERO COMP-3.             
000198     03  W-IDGMT-MAX-X.                                                   
000199       05  W-IDDISTR-WDB2-MAX    PIC S9(5) VALUE ZERO COMP-3.             
000200       05  W-IDKUNDNR-WDB2-MAX   PIC S9(7) VALUE ZERO COMP-3.             
000201                                                                          
000202* TILL WDB101                                                             
000203     03  W-WDB101KY-X.                                                    
000204       05  W-WDB1-IDPARTNR       PIC X(9)  VALUE SPACE.                   
000205       05  W-WDB1-IDFTG          PIC 9(2)  VALUE ZERO.                    
000206*                                                                         
000207*    --- STATUS-KOD FRÅN IMS                                              
000208 01  STATUS-WS                   PIC XX.                                  
000209     88  SEGMENT-FINNS                       VALUE '  '.                  
000210     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000211     88  BASEN-SLUT                          VALUE 'GB'.                  
000212                                                                          
000213 01  GODK-STATUSKODER.                                                    
000214     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000215                                                                          
000216 01  SSA1                        PIC X(256).                              
000217 01  SSA2                        PIC X(64).                               
000218     EJECT                                                                
000219*    --- IMS FUNKTIONSKODER                                               
000220*01  -COPY W0003                                                          
000221     EJECT                                                                
000222*    ---  DLI INPUT-OUTPUT AREA                                           
000223 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
000224                                                                          
000225 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-L501'.         
000226 01  DLI-IO-AREA-WDL501.                                                  
000227*    03  -COPY WDL501                                                     
000228     EJECT                                                                
000229 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-L511'.         
000230 01  DLI-IO-AREA-WDL511.                                                  
000231*    03  -COPY WDL511                                                     
000232     EJECT                                                                
000233 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-L521'.         
000234 01  DLI-IO-AREA-WDL521.                                                  
000235*    03  -COPY WDL521                                                     
000236     EJECT                                                                
000237 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-K611'.         
000238 01  DLI-IO-AREA-WDK611.                                                  
000239*    03  -COPY WDK611                                                     
000240     EJECT                                                                
000241 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDB201'.         
000242 01  DLI-IO-WDB201.                                                       
000243*     03  -COPY WDB201.                                                   
000244     EJECT                                                                
000245 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDB101'.         
000246 01  DLI-IO-WDB101.                                                       
000247*     03  -COPY WDB101.                                                   
000248     EJECT                                                                
000249 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDB601'.         
000250 01  DLI-IO-WDB601.                                                       
000251*     03  -COPY WDB601.                                                   
000252     EJECT                                                                
000253 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK711'.         
000254 01  DLI-IO-WDK711.                                                       
000255*    03  -COPY WDK711                                                     
000256     EJECT                                                                
000257 LINKAGE SECTION.                                                         
000258*01  -COPY W0009  -PRE MSG-                                               
000259*01  -COPY W0008  -PRE WDL5-                                              
000260     05  FILLER                  PIC X.                                   
000261     EJECT                                                                
000262*01  -COPY W0008  -PRE WDK6-                                              
000263     05  FILLER                  PIC X.                                   
000264     EJECT                                                                
000265*01  -COPY W0008      -PRE WDB1-                                          
000266     05  FILLER                  PIC X.                                   
000267     EJECT                                                                
000268*01  -COPY W0008      -PRE WDB2-                                          
000269     05  FILLER                  PIC X.                                   
000270     EJECT                                                                
000271*01  -COPY W0008      -PRE WDB6-                                          
000272     05  FILLER                  PIC X.                                   
000273     EJECT                                                                
000274*01  -COPY W0008      -PRE WDK7-                                          
000275     05  FILLER                  PIC X.                                   
000276     EJECT                                                                
000277 PROCEDURE DIVISION  USING MSG-PCB WDL5-PCB                               
000278                          WDK6-PCB WDB1-PCB WDB2-PCB                      
000279                          WDB6-PCB WDK7-PCB.                              
000280     ENTRY 'DLITCBL' USING MSG-PCB WDL5-PCB                               
000281                          WDK6-PCB WDB1-PCB WDB2-PCB                      
000282                          WDB6-PCB WDK7-PCB.                              
000283                                                                          
000284     PERFORM IMS-GET-MSG                                                  
000285     IF SEGMENT-FINNS                                                     
000286       PERFORM A-INIT                                                     
000287       PERFORM B-KOLLA-NYCKLAR                                            
000288       IF NYCKLAR-OK                                                      
000289         IF MFS-FIRST                                                     
000290           PERFORM C-FOERSTA-SIDA                                         
000291         ELSE                                                             
000292           IF MFS-NEXT                                                    
000293             PERFORM D-NAESTA-SIDA                                        
000294           ELSE                                                           
000295             PERFORM E-SAMMA-SIDA                                         
000296           END-IF                                                         
000297         END-IF                                                           
000298         PERFORM F-LAES-VISA-INFO                                         
000299       END-IF                                                             
000300       PERFORM Z-FINIT                                                    
000301     END-IF                                                               
000302                                                                          
000303     MOVE ZERO                  TO RETURN-CODE                            
000304     GOBACK                                                               
000305     .                                                                    
000306     EJECT                                                                
000307 A-INIT                         SECTION.                                  
000308                                                                          
000309     IF MSG-DUBBLA-TRANSKODER                                             
000310       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I72501                 
000311       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
000312       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
000313     ELSE                                                                 
000314       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I72501                  
000315       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
000316       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
000317     END-IF                                                               
000318                                                                          
000319     MOVE MSG-KDTRTYP           TO MFS-KDTRTYP                            
000320     MOVE MSG-IDPFK             TO MFS-IDPFK                              
000321     MOVE MFS-IDTRANS           TO W-IDTRANS                              
000322                                                                          
000323     MOVE LOW-VALUE             TO MSG-AREA                               
000324     MOVE 'W4O72501'            TO MFS-IDMOD                              
000325     MOVE '4725'                TO MOD-IDTRANS                            
000326     MOVE MFS-RENSA-FAELT       TO MOD-TEMFSFEL MOD-TEMFSINF              
000327     MOVE SPACE                 TO MED-IDMFSFEL MED-IDMFSINF              
000328                                                                          
000329     COMPUTE MAX-MOD-LAENGD = LENGTH OF MOD-W4O72501 + 4                  
000330                                                                          
000331     IF EGEN-MID                                                          
000332       CONTINUE                                                           
000333     ELSE                                                                 
000334       MOVE SPACE               TO MFS-KDTRTYP                            
000335       MOVE '7'                 TO MFS-IDPFK                              
000336     END-IF                                                               
000337                                                                          
000338     IF ENGLISH-TEXT                                                      
000339       MOVE 'GB '               TO MED-IDSKYLT                            
000340     ELSE                                                                 
000341       MOVE 'S  '               TO MED-IDSKYLT                            
000342     END-IF                                                               
000343                                                                          
000344     MOVE  LOW-VALUE         TO W-IDGMT-MIN-X                             
000345                                                                          
000346     MOVE HIGH-VALUE         TO W-IDGMT-MAX-X                             
000347     .                                                                    
000348     EJECT                                                                
000349 B-KOLLA-NYCKLAR                SECTION.                                  
000350                                                                          
000351     MOVE JA                    TO NYCKLAR-SW                             
000352                                                                          
000353*    -- KONTROLL AV IDFAKT                                                
000354     MOVE MFS-RENSA-FAELT       TO MOD-IDFAKT-IN                          
000355                                                                          
000356     IF MID-IDFAKT-IN = ALL '+'                                           
000357       MOVE MID-IDFAKT-UT       TO WS-IDFAKT                              
000358       INSPECT WS-IDFAKT REPLACING LEADING SPACE BY ZERO                  
000359     ELSE                                                                 
000360       MOVE MID-IDFAKT-IN       TO WS-IDFAKT                              
000361       MOVE '7'                 TO MFS-IDPFK                              
000362       MOVE SPACE               TO MFS-KDTRTYP                            
000363     END-IF                                                               
000364     IF WS-IDFAKT NUMERIC AND WS-IDFAKT > ZERO                            
000365       MOVE WS-IDFAKT           TO W-IDFAKT                               
000366     ELSE                                                                 
000367       MOVE NEJ                 TO NYCKLAR-SW                             
000368     END-IF                                                               
000369                                                                          
000370*    -- KONTROLL AV IDKUNDNR                                              
000371     MOVE MFS-RENSA-FAELT       TO MOD-IDKUNDNR-IN                        
000372                                                                          
000373     IF MID-IDKUNDNR-IN = ALL '+'                                         
000374       MOVE MID-IDKUNDNR-UT     TO WS-IDKUNDNR                            
000375       INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO                
000376     ELSE                                                                 
000377       MOVE MID-IDKUNDNR-IN     TO WS-IDKUNDNR                            
000378       MOVE '7'                 TO MFS-IDPFK                              
000379       MOVE SPACE               TO MFS-KDTRTYP                            
000380     END-IF                                                               
000381     IF WS-IDKUNDNR NUMERIC                                               
000382       CONTINUE                                                           
000383     ELSE                                                                 
000384       MOVE NEJ                 TO NYCKLAR-SW                             
000385     END-IF                                                               
000386                                                                          
000387*    -- KONTROLL AV IDORDNR                                               
000388     MOVE MFS-RENSA-FAELT       TO MOD-IDORDNR-IN                         
000389                                                                          
000390     IF MID-IDORDNR-IN = ALL '+'                                          
000391       MOVE MID-IDORDNR-UT      TO WS-IDORDNR                             
000392       INSPECT WS-IDORDNR REPLACING LEADING SPACE BY ZERO                 
000393     ELSE                                                                 
000394       MOVE MID-IDORDNR-IN      TO WS-IDORDNR                             
000395       MOVE '7'                 TO MFS-IDPFK                              
000396       MOVE SPACE               TO MFS-KDTRTYP                            
000397     END-IF                                                               
000398     IF WS-IDORDNR NUMERIC                                                
000399       CONTINUE                                                           
000400     ELSE                                                                 
000401       MOVE NEJ                 TO NYCKLAR-SW                             
000402     END-IF                                                               
000403                                                                          
000404*    -- KONTROLL AV IDKOLLI                                               
000405     MOVE MFS-RENSA-FAELT       TO MOD-IDKOLLI-IN                         
000406                                                                          
000407     IF MID-IDKOLLI-IN = ALL '+'                                          
000408       MOVE MID-IDKOLLI-UT      TO WS-IDKOLLI                             
000409       INSPECT WS-IDKOLLI REPLACING LEADING SPACE BY ZERO                 
000410     ELSE                                                                 
000411       MOVE MID-IDKOLLI-IN      TO WS-IDKOLLI                             
000412       MOVE '7'                 TO MFS-IDPFK                              
000413       MOVE SPACE               TO MFS-KDTRTYP                            
000414     END-IF                                                               
000415     IF WS-IDKOLLI NUMERIC                                                
000416       CONTINUE                                                           
000417     ELSE                                                                 
000418       MOVE NEJ                 TO NYCKLAR-SW                             
000419     END-IF                                                               
000420                                                                          
000421*    -- KONTROLL AV IDPRODNR                                              
000422     MOVE MFS-RENSA-FAELT       TO MOD-IDPRODNR-IN                        
000423                                                                          
000424     IF MID-IDPRODNR-IN = ALL '+'                                         
000425       MOVE MID-IDPRODNR-UT     TO WS-IDPRODNR                            
000426       INSPECT WS-IDPRODNR REPLACING LEADING SPACE BY ZERO                
000427     ELSE                                                                 
000428       MOVE MID-IDPRODNR-IN     TO WS-IDPRODNR                            
000429       MOVE '7'                 TO MFS-IDPFK                              
000430       MOVE SPACE               TO MFS-KDTRTYP                            
000431     END-IF                                                               
000432     IF WS-IDPRODNR NOT NUMERIC                                           
000433       MOVE NEJ                 TO NYCKLAR-SW                             
000434     END-IF                                                               
000435                                                                          
000436*    -- KONTROLL AV IDARTNR                                               
000437     MOVE MFS-RENSA-FAELT       TO MOD-IDARTNR-IN                         
000438                                                                          
000439     IF MID-IDARTNR-IN = ALL '+'                                          
000440       MOVE MID-IDARTNR-UT      TO WS-IDARTNR                             
000441       INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO                 
000442     ELSE                                                                 
000443       MOVE MID-IDARTNR-IN      TO WS-IDARTNR                             
000444       MOVE '7'                 TO MFS-IDPFK                              
000445       MOVE SPACE               TO MFS-KDTRTYP                            
000446     END-IF                                                               
000447                                                                          
000448     IF NYCKLAR-OK                                                        
000449       PERFORM BA-KOLLA-NYCKEL-SAMBAND                                    
000450     END-IF                                                               
000451                                                                          
000452     IF EGEN-MID AND NYCKLAR-OK                                           
000453       MOVE WS-IDFAKT           TO MOD-IDFAKT-UT                          
000454       INSPECT MOD-IDFAKT-UT REPLACING LEADING ZERO BY SPACE              
000455       MOVE WS-IDKUNDNR         TO MOD-IDKUNDNR-UT                        
000456       INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE            
000457       MOVE WS-IDORDNR          TO MOD-IDORDNR-UT                         
000458       INSPECT MOD-IDORDNR-UT REPLACING LEADING ZERO BY SPACE             
000459       MOVE WS-IDKOLLI          TO MOD-IDKOLLI-UT                         
000460       INSPECT MOD-IDKOLLI-UT REPLACING LEADING ZERO BY SPACE             
000461       MOVE WS-IDPRODNR         TO MOD-IDPRODNR-UT                        
000462       INSPECT MOD-IDPRODNR-UT REPLACING LEADING ZERO BY SPACE            
000463       MOVE WS-IDARTNR          TO MOD-IDARTNR-UT                         
000464       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
000465     ELSE                                                                 
000466       MOVE MFS-RENSA-FAELT     TO MOD-IDFAKT-UT                          
000467                               MOD-IDKUNDNR-UT                            
000468                               MOD-IDORDNR-UT                             
000469                               MOD-IDKOLLI-UT                             
000470                               MOD-IDPRODNR-UT                            
000471                               MOD-IDARTNR-UT                             
000472     END-IF                                                               
000473                                                                          
000474     IF NYCKLAR-FEL                                                       
000475       MOVE ERR-WRONG-KEY       TO MED-IDMFSFEL                           
000476       PERFORM MFS-RENSA-FAELT-UT                                         
000477     END-IF                                                               
000478     .                                                                    
000479     EJECT                                                                
000480 BA-KOLLA-NYCKEL-SAMBAND        SECTION.                                  
000481                                                                          
000482     IF WS-IDPRODNR              > ZERO                                   
000483       MOVE WS-IDPRODNR         TO W-IDPRODNR-MIN                         
000484                                   W-IDPRODNR-MAX                         
000485     ELSE                                                                 
000486       MOVE ZERO                TO W-IDPRODNR-MIN                         
000487       MOVE +9999999            TO W-IDPRODNR-MAX                         
000488     END-IF                                                               
000489                                                                          
000490     IF WS-IDKOLLI               > ZERO                                   
000491       MOVE WS-IDKOLLI          TO W-IDKOLLI-MIN                          
000492                                   W-IDKOLLI-MAX                          
000493     ELSE                                                                 
000494       MOVE ZERO                TO W-IDKOLLI-MIN                          
000495       MOVE +99999              TO W-IDKOLLI-MAX                          
000496     END-IF                                                               
000497                                                                          
000498     IF WS-IDARTNR               > ZERO                                   
000499       MOVE WS-IDARTNR          TO W-IDARTNR-MIN                          
000500                                   W-IDARTNR-MAX                          
000501     ELSE                                                                 
000502       MOVE ZERO                TO W-IDARTNR-MIN                          
000503       MOVE +999999999          TO W-IDARTNR-MAX                          
000504     END-IF                                                               
000505                                                                          
000506     IF WS-IDORDNR               > ZERO                                   
000507       MOVE WS-IDORDNR          TO W-IDORDNR-MIN                          
000508                                   W-IDORDNR-MAX                          
000509     ELSE                                                                 
000510       MOVE ZERO                TO W-IDORDNR-MIN                          
000511       MOVE 9999999             TO W-IDORDNR-MAX                          
000512     END-IF                                                               
000513                                                                          
000514     IF WS-IDKUNDNR              > ZERO                                   
000515       MOVE WS-IDKUNDNR         TO W-IDKUNDNR-MIN                         
000516                                   W-IDKUNDNR-MAX                         
000517     ELSE                                                                 
000518       MOVE ZERO                TO W-IDKUNDNR-MIN                         
000519       MOVE +9999999            TO W-IDKUNDNR-MAX                         
000520     END-IF                                                               
000521     .                                                                    
000522     EJECT                                                                
000523 C-FOERSTA-SIDA                 SECTION.                                  
000524                                                                          
000525     MOVE INF-FIRST-PAGE        TO MED-IDMFSINF                           
000526     .                                                                    
000527     EJECT                                                                
000528 D-NAESTA-SIDA                  SECTION.                                  
000529                                                                          
000530     IF MID-IDPRODNR-NEXT > ZERO                                          
000531       MOVE MID-IDPRODNR-NEXT   TO W-IDPRODNR                             
000532       MOVE MID-IDKOLLI-NEXT    TO W-IDKOLLI                              
000533       MOVE MID-IDARTNR-NEXT    TO W-IDARTNR-L5                           
000534       MOVE MID-IDLOPNR-NEXT    TO W-IDPURAD-L5                           
000535     END-IF                                                               
000536     .                                                                    
000537     EJECT                                                                
000538 E-SAMMA-SIDA                   SECTION.                                  
000539                                                                          
000540     IF EGEN-MID OR HELP-MID                                              
000541       IF MID-IDPRODNR-ENTER > ZERO                                       
000542         MOVE MID-IDPRODNR-ENTER TO W-IDPRODNR                            
000543         MOVE MID-IDKOLLI-ENTER TO W-IDKOLLI                              
000544         MOVE MID-IDARTNR-ENTER TO W-IDARTNR-L5                           
000545         MOVE MID-IDLOPNR-ENTER TO W-IDPURAD-L5                           
000546       END-IF                                                             
000547     END-IF                                                               
000548     .                                                                    
000549     EJECT                                                                
000550 F-LAES-VISA-INFO               SECTION.                                  
000551                                                                          
000552     PERFORM IMS-GU-WDL501                                                
000553                                                                          
000554     IF SEGMENT-SAKNAS                                                    
000555        MOVE ERR-KEY-MISSING    TO MED-IDMFSFEL                           
000556        PERFORM MFS-RENSA-FAELT-UT                                        
000557     ELSE                                                                 
000558       IF NOT MFS-FIRST                                                   
000559         PERFORM IMS-GNP-WDL511-UNIK                                      
000560       ELSE                                                               
000561         PERFORM IMS-GNP-WDL511                                           
000562       END-IF                                                             
000563       IF SEGMENT-SAKNAS                                                  
000564          MOVE ERR-KEY-MISSING    TO MED-IDMFSFEL                         
000565          PERFORM MFS-RENSA-FAELT-UT                                      
000566       ELSE                                                               
000567*--------NÄR DET ÄR EN DEALER-NET/DDI- MARKNAD VISAS LOKAL VALUTA.        
000568         MOVE FAKC-IDDISTR       TO TEST-IDDISTR                          
000569         IF DIST79-DEALER-PRICE                                           
000571           MOVE FAKC-IDDISTR    TO W-IDDISTR-WDB2                         
000572                                     W-IDDISTR-WDB2-MIN                   
000573                                     W-IDDISTR-WDB2-MAX                   
000574                                     TEST-IDDISTR                         
000575           MOVE FAKC-IDKUNDNR   TO W-IDKUNDNR-WDB2                        
000576                                     W-IDKUNDNR-WDB2-MIN                  
000577                                     W-IDKUNDNR-WDB2-MAX                  
000578           PERFORM S10-HAMTA-KDVALISO                                     
000579         ELSE                                                             
000580           IF DIST79-ECOM-PRICE                                           
000581             MOVE FAK-KDVALISO  TO MOD-KDVALISO                           
000582           ELSE                                                           
000583             MOVE 'SEK'         TO MOD-KDVALISO                           
000584           END-IF                                                         
000585         END-IF                                                           
000586                                                                          
000587         PERFORM FA-INIT-MOD-LINES                                        
000588       END-IF                                                             
000589     END-IF                                                               
000590     .                                                                    
000600     EJECT                                                                
000610 FA-INIT-MOD-LINES SECTION.                                               
000620                                                                          
000650     MOVE FAKC-IDPRODNR TO W-IDPRODNR                                     
000660     MOVE FAKC-IDKOLLI  TO W-IDKOLLI                                      
000680     IF NOT MFS-FIRST                                                     
000690       PERFORM IMS-GNP-WDL521-UNIK                                        
000691     ELSE                                                                 
000692       PERFORM IMS-GNP-WDL521                                             
000693     END-IF                                                               
000695     MOVE +0                  TO INDX                                     
000696     PERFORM UNTIL INDX > MAX-INDX                                        
000697       PERFORM UNTIL SEGMENT-SAKNAS                                       
000698                  OR INDX = MAX-INDX                                      
000699         ADD  +1              TO INDX                                     
000700         IF INDX = 1                                                      
000701           MOVE FAKC-IDPRODNR TO MOD-IDPRODNR-ENTER                       
000702           MOVE FAKC-IDKOLLI  TO MOD-IDKOLLI-ENTER                        
000703           MOVE FAKL-IDARTNR  TO MOD-IDARTNR-ENTER                        
000704           MOVE FAKL-IDPURAD  TO MOD-IDLOPNR-ENTER                        
000705           MOVE FAK-IDDC      TO W-IDDC                                   
000706           PERFORM IMS-GU-WDB601                                          
000707         END-IF                                                           
000708         MOVE FAKL-IDARTNR    TO MOD-IDARTNR (INDX)                       
000709                                 W-IDARTNR                                
000710         PERFORM FAA-LAES-LO                                              
000711                                                                          
000712         MOVE FAKL-BEART      TO MOD-BEART (INDX)                         
000713                                                                          
000714         COMPUTE W-VKARTNTO = FAKL-KVLEVART * FAKL-VKARTNTO               
000715         MOVE W-VKARTNTO      TO MOD-VKARTNTO (INDX)                      
000716                                                                          
000717*-- NÄR DET ÄR EN DEALER-NET/DDI- MARKNAD SKALL LOKALT PRIS VISAS.        
000718         MOVE FAKC-IDDISTR         TO TEST-IDDISTR                        
000719         IF DIST79-DEALER-PRICE OR                                        
000721            DIST79-ECOM-PRICE                                             
000722           MOVE FAKL-PRARTNTO-LOC  TO MOD-PRARTNTO (INDX)                 
000723         ELSE                                                             
000724           MOVE FAKL-PRARTNTO      TO MOD-PRARTNTO (INDX)                 
000725         END-IF                                                           
000726                                                                          
000727         MOVE FAKL-KVLEVART    TO MOD-KVLEVART (INDX)                     
000728         PERFORM IMS-GNP-WDL521                                           
000729       END-PERFORM                                                        
000730       IF SEGMENT-SAKNAS                                                  
000731         PERFORM IMS-GNP-WDL511                                           
000732         IF SEGMENT-SAKNAS                                                
000740           PERFORM UNTIL INDX = MAX-INDX                                  
000750             ADD 1          TO INDX                                       
000751             PERFORM MFS-RENSA-RAD-FAELT-UT                               
000752           END-PERFORM                                                    
000753         ELSE                                                             
000754           MOVE FAKC-IDPRODNR TO W-IDPRODNR                               
000755           MOVE FAKC-IDKOLLI  TO W-IDKOLLI                                
000756           PERFORM IMS-GNP-WDL521                                         
000757         END-IF                                                           
000758         IF INDX = MAX-INDX                                               
000759           ADD  +1 TO INDX                                                
000760         END-IF                                                           
000761       ELSE                                                               
000762         ADD  +1 TO INDX                                                  
000763       END-IF                                                             
000764     END-PERFORM                                                          
000765                                                                          
000766     IF SEGMENT-FINNS                                                     
000767       MOVE FAKC-IDPRODNR     TO MOD-IDPRODNR-NEXT                        
000768       MOVE FAKC-IDKOLLI      TO MOD-IDKOLLI-NEXT                         
000769       MOVE FAKL-IDARTNR      TO MOD-IDARTNR-NEXT                         
000770       MOVE FAKL-IDPURAD      TO MOD-IDLOPNR-NEXT                         
000771                                                                          
000772       MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                          
000773     ELSE                                                                 
000774       MOVE MOD-IDPRODNR-ENTER TO MOD-IDPRODNR-NEXT                       
000775       MOVE MOD-IDKOLLI-ENTER  TO MOD-IDKOLLI-NEXT                        
000776       MOVE MOD-IDARTNR-ENTER  TO MOD-IDARTNR-NEXT                        
000777       MOVE MOD-IDLOPNR-ENTER  TO MOD-IDLOPNR-NEXT                        
000778       MOVE INF-LAST-PAGE      TO MED-IDMFSINF                            
000779     END-IF                                                               
000780     .                                                                    
000781     EJECT                                                                
000782 FAA-LAES-LO SECTION.                                                     
000783                                                                          
000784     IF DCS-CDC                                                           
000785       PERFORM IMS-GET-WDK611                                             
000786       MOVE CLAG-ADLAGOMR   TO MOD-ADLAGOMR (INDX)                        
000787       MOVE CLAG-ADGANG     TO MOD-ADGANG (INDX)                          
000788       MOVE CLAG-ADPLATS    TO MOD-ADPLATS (INDX)                         
000789     ELSE                                                                 
000790       PERFORM IMS-GU-WDK711                                              
000791       IF SEGMENT-FINNS                                                   
000792         MOVE SLAG-ADLAGOMR   TO MOD-ADLAGOMR (INDX)                      
000793         MOVE SLAG-ADGANG     TO MOD-ADGANG (INDX)                        
000794         MOVE SLAG-ADPLATS    TO MOD-ADPLATS (INDX)                       
000795       ELSE                                                               
000796         MOVE ZERO            TO MOD-ADLAGOMR (INDX)                      
000797         MOVE ZERO            TO MOD-ADGANG (INDX)                        
000798         MOVE ZERO            TO MOD-ADPLATS (INDX)                       
000799       END-IF                                                             
000800     END-IF                                                               
000801     .                                                                    
000802     EJECT                                                                
000803 Z-FINIT                        SECTION.                                  
000804                                                                          
000805     IF MED-IDMFSFEL NOT = SPACE OR MED-IDMFSINF NOT = SPACE              
000806       CALL WMEDKONV USING MED-WMEDAREA                                   
000807       MOVE MED-MFSFEL          TO MOD-TEMFSFEL                           
000808       MOVE MED-MFSINF          TO MOD-TEMFSINF                           
000809     END-IF                                                               
000810                                                                          
000811     MOVE MAX-MOD-LAENGD        TO MSG-KVLL                               
000812     PERFORM IMS-INSERT-MSG                                               
000813     .                                                                    
000814 S10-HAMTA-KDVALISO             SECTION.                                  
000815                                                                          
000816     PERFORM IMS-GU-GMTA-WDB201                                           
000817     IF SEGMENT-FINNS                                                     
000818       CONTINUE                                                           
000819     ELSE                                                                 
000820       PERFORM IMS-GET-WDB201                                             
000821     END-IF                                                               
000822     MOVE GMT-IDPARTNR       TO W-WDB1-IDPARTNR                           
000823     MOVE GMT-IDFTG          TO W-WDB1-IDFTG                              
000824     PERFORM IMS-GU-WDB1-WDB101                                           
000825     IF SEGMENT-FINNS                                                     
000826       IF DIST79-DEALER-PRICE                                             
000828         MOVE BET-KDVALISO     TO MOD-KDVALISO                            
000829       ELSE                                                               
000830         MOVE 'SEK'            TO MOD-KDVALISO                            
000831       END-IF                                                             
000832     ELSE                                                                 
000833       MOVE SPACE              TO MOD-KDVALISO                            
000834     END-IF                                                               
000835     .                                                                    
000836     EJECT                                                                
000837 MFS-RENSA-FAELT-UT             SECTION.                                  
000838                                                                          
000839*    --- ALLA UTDATA-FÄLT                                                 
000840*    --- INKL. BLÄDDRINGSNYCKLAR                                          
000841     MOVE ZERO                  TO MOD-IDPRODNR-ENTER                     
000842                                   MOD-IDKOLLI-ENTER                      
000843                                   MOD-IDARTNR-ENTER                      
000844                                   MOD-IDLOPNR-ENTER                      
000845                                   MOD-IDPRODNR-NEXT                      
000846                                   MOD-IDKOLLI-NEXT                       
000847                                   MOD-IDARTNR-NEXT                       
000848                                   MOD-IDLOPNR-NEXT                       
000849     MOVE +1                    TO INDX                                   
000850     PERFORM UNTIL INDX > MAX-INDX                                        
000851       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
000852       ADD +1                   TO INDX                                   
000853     END-PERFORM                                                          
000854     .                                                                    
000855                                                                          
000856 MFS-RENSA-RAD-FAELT-UT         SECTION.                                  
000857                                                                          
000858*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
000859     MOVE MFS-RENSA-FAELT       TO MOD-IDARTNR (INDX)                     
000860                                   MOD-ADLAGOMR (INDX)                    
000861                                   MOD-ADGANG (INDX)                      
000862                                   MOD-ADPLATS (INDX)                     
000863                                   MOD-BEART (INDX)                       
000864                                   MOD-PRARTNTO (INDX)                    
000865                                   MOD-KVLEVART (INDX)                    
000866                                   MOD-VKARTNTO (INDX)                    
000867          .                                                               
000868     EJECT                                                                
000869* --- IMS SEKTIONER ---                                                   
000870                                                                          
000871 IMS-GET-MSG                    SECTION.                                  
000872                                                                          
000873     MOVE '  QC' TO GODK-STATUSKODER                                      
000874     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
000875     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
000876     PERFORM IMS-STATUSKONTROLL                                           
000877     .                                                                    
000878                                                                          
000879 IMS-INSERT-MSG                 SECTION.                                  
000880                                                                          
000881     IF ENGLISH-TEXT                                                      
000882       MOVE 'N' TO MFS-KDHUVOMR                                           
000883     END-IF                                                               
000884     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
000885     MOVE SPACE TO GODK-STATUSKODER                                       
000886     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
000887     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
000888     PERFORM IMS-STATUSKONTROLL                                           
000889     .                                                                    
000890     EJECT                                                                
000891 IMS-GU-WDL501                 SECTION.                                   
000892                                                                          
000893     STRING 'WDL501  (IDFAKT   =' W-IDFAKT-X ')'                          
000894          DELIMITED BY SIZE INTO SSA1                                     
000895     MOVE '  GE' TO GODK-STATUSKODER                                      
000896     CALL CBLTDLI USING GU WDL5-PCB DLI-IO-AREA-WDL501 SSA1               
000897     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
000898     PERFORM IMS-STATUSKONTROLL                                           
000899     .                                                                    
000900     EJECT                                                                
000901 IMS-GNP-WDL511                 SECTION.                                  
000902                                                                          
000903     STRING 'WDL511  (IDPRODNR>=' W-IDPRODNR-MIN-X                        
000904                    '&IDPRODNR<=' W-IDPRODNR-MAX-X                        
000905                    '&IDKOLLI >=' W-IDKOLLI-MIN-X                         
000906                    '&IDKOLLI <=' W-IDKOLLI-MAX-X                         
000907                    '&IDKUNDNR>=' W-IDKUNDNR-MIN-X                        
000908                    '&IDKUNDNR<=' W-IDKUNDNR-MAX-X                        
000909                    '&IDORDNR7>=' W-IDORDNR-MIN-X                         
000910                    '&IDORDNR7<=' W-IDORDNR-MAX-X ')'                     
000920          DELIMITED BY SIZE INTO SSA1                                     
000921     MOVE '  GE' TO GODK-STATUSKODER                                      
000922     CALL CBLTDLI USING GNP WDL5-PCB DLI-IO-AREA-WDL511 SSA1              
000923     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
000924     PERFORM IMS-STATUSKONTROLL                                           
000925     .                                                                    
000926     EJECT                                                                
000927 IMS-GNP-WDL511-UNIK            SECTION.                                  
000928                                                                          
000929     STRING 'WDL511  (WDL511KY =' W-WDL511KY-X ')'                        
000930          DELIMITED BY SIZE INTO SSA1                                     
000931     MOVE '  GE' TO GODK-STATUSKODER                                      
000932     CALL CBLTDLI USING GNP WDL5-PCB DLI-IO-AREA-WDL511 SSA1              
000933     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
000934     PERFORM IMS-STATUSKONTROLL                                           
000935     .                                                                    
000936     EJECT                                                                
000937 IMS-GNP-WDL521                 SECTION.                                  
000938                                                                          
000939     STRING 'WDL511  (WDL511KY =' W-WDL511KY-X ')'                        
000940          DELIMITED BY SIZE INTO SSA1                                     
000941     STRING 'WDL521  (WDL521KY>=' W-WDL521KY-MIN-X                        
000942                    '&WDL521KY<=' W-WDL521KY-MAX-X ')'                    
000943          DELIMITED BY SIZE INTO SSA2                                     
000944     MOVE '  GE' TO GODK-STATUSKODER                                      
000945     CALL CBLTDLI USING GNP WDL5-PCB DLI-IO-AREA-WDL521 SSA1 SSA2         
000946     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
000947     PERFORM IMS-STATUSKONTROLL                                           
000948     .                                                                    
000949     EJECT                                                                
000950 IMS-GNP-WDL521-UNIK            SECTION.                                  
000951                                                                          
000952     STRING 'WDL511  (WDL511KY =' W-WDL511KY-X ')'                        
000953          DELIMITED BY SIZE INTO SSA1                                     
000954     STRING 'WDL521  (WDL521KY =' W-WDL521KY-X ')'                        
000955          DELIMITED BY SIZE INTO SSA2                                     
000956     MOVE '  GE' TO GODK-STATUSKODER                                      
000957     CALL CBLTDLI USING GNP WDL5-PCB DLI-IO-AREA-WDL521 SSA1 SSA2         
000958     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
000959     PERFORM IMS-STATUSKONTROLL                                           
000960     .                                                                    
000961     EJECT                                                                
000962 IMS-GET-WDK611                 SECTION.                                  
000963     MOVE 'IMS-GET-WDK611'  TO DBS-SECTION                                
000964                                                                          
000965     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
000966          DELIMITED BY SIZE INTO SSA1                                     
000967     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
000968          DELIMITED BY SIZE INTO SSA2                                     
000969     MOVE '    ' TO GODK-STATUSKODER                                      
000970     CALL CBLTDLI USING GU  WDK6-PCB DLI-IO-AREA-WDK611 SSA1 SSA2         
000971     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
000972     PERFORM IMS-STATUSKONTROLL                                           
000973     .                                                                    
000974     EJECT                                                                
000975 IMS-GU-WDK711 SECTION.                                                   
000976     MOVE 'IMS-GU-WDK711'    TO DBS-SECTION                               
000977                                                                          
000978     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
000979          DELIMITED BY SIZE INTO SSA1                                     
000980     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
000981          DELIMITED BY SIZE INTO SSA2                                     
000982     MOVE '  GE' TO GODK-STATUSKODER                                      
000983     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
000984     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
000985     PERFORM IMS-STATUSKONTROLL                                           
000986     .                                                                    
000987     EJECT                                                                
000988 IMS-GU-GMTA-WDB201               SECTION.                                
000989     MOVE 'IMS-GU-GMTA-WDB201'    TO DBS-SECTION                          
000990                                                                          
000991     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
000992          DELIMITED BY SIZE INTO SSA1                                     
000993     MOVE '  GE'              TO GODK-STATUSKODER                         
000994                                                                          
000995     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
000996     MOVE WDB2-STATUS-CODE    TO STATUS-WS                                
000997     PERFORM IMS-STATUSKONTROLL                                           
000998     .                                                                    
000999     EJECT                                                                
001000 IMS-GET-WDB201 SECTION.                                                  
001001     MOVE 'IMS-GET-WDB201'    TO DBS-SECTION                              
001002                                                                          
001003     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
001004                    '&IDGMT   <=' W-IDGMT-MAX-X ')'                       
001005          DELIMITED BY SIZE INTO SSA1                                     
001006     MOVE '    '              TO GODK-STATUSKODER                         
001007                                                                          
001008     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
001009     MOVE WDB2-STATUS-CODE    TO STATUS-WS                                
001010     PERFORM IMS-STATUSKONTROLL                                           
001011     .                                                                    
001012     EJECT                                                                
001013 IMS-GU-WDB1-WDB101              SECTION.                                 
001014     MOVE 'IMS-GU-WDB1-WDB101'    TO DBS-SECTION                          
001015                                                                          
001016     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
001017          DELIMITED BY SIZE INTO SSA1                                     
001018     MOVE '  GE'              TO GODK-STATUSKODER                         
001019                                                                          
001020     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
001021     MOVE WDB1-STATUS-CODE    TO STATUS-WS                                
001022     PERFORM IMS-STATUSKONTROLL                                           
001023     .                                                                    
001024     EJECT                                                                
001025 IMS-GU-WDB601 SECTION.                                                   
001026     MOVE 'IMS-GU-WDB601'    TO DBS-SECTION                               
001027                                                                          
001028     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
001029          DELIMITED BY SIZE INTO SSA1                                     
001030     MOVE '  GE' TO GODK-STATUSKODER                                      
001031     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
001032     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
001033     PERFORM IMS-STATUSKONTROLL                                           
001034     .                                                                    
001035     EJECT                                                                
001036 IMS-STATUSKONTROLL             SECTION.                                  
001037                                                                          
001038     SET STATUS-IX TO 1                                                   
001039     SEARCH GODK-STATUS                                                   
001040       AT END                                                             
001041         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
001042         DELIMITED BY SIZE INTO FELTEXT                                   
001043         CALL FELLOG                                                      
001044       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
001045         CONTINUE                                                         
001046     END-SEARCH                                                           
001047     .                                                                    
