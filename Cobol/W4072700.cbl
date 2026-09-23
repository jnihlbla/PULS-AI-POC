000001*                                                                         
000002******************************************************************        
000003*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0145      *        
000004******************************************************************        
000005*                                                                         
000006 ID DIVISION.                                                             
000007                                                                          
000008 PROGRAM-ID.     W4072700.                                                
000009 AUTHOR.         JEF                                                      
000010 DATE-WRITTEN.   97/04/16.                                                
000011 DATE-COMPILED.                                                           
000012                                                                          
000013*    FUNKTION:                                                            
000014*        LÄSER FAKTURAHISTORIK FRÅN WDL5                                  
000015*                                                                         
000016*    INDATA.                                                              
000017*        TRANSAKTION: W4T727                                              
000018*        MID:         W4I72701                                            
000019*                                                                         
000020*    UTDATA.                                                              
000021*        MOD:         W4O72701                                            
000022                                                                          
000023                                                                          
000024 ENVIRONMENT DIVISION.                                                    
000025     EJECT                                                                
000026 DATA DIVISION.                                                           
000027 WORKING-STORAGE SECTION.                                                 
000028*    -- CHECKED BY WY2000                                                 
000029     SKIP3                                                                
000030 77  IDPGM                       PIC X(08)   VALUE 'W4072700'.            
000031 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
000032 77  JA                          PIC X       VALUE 'J'.                   
000033 77  NEJ                         PIC X       VALUE 'N'.                   
000034 77  W-VKARTNTO                  PIC 9(4)V9(3) VALUE ZERO.                
000035 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
000036 77  MAX-INDX                    PIC S9(4)  VALUE +14   COMP SYNC.        
000037 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +0    COMP SYNC.        
000038                                                                          
000039*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
000040 77  WS-IDFAKT                   PIC X(7)    VALUE SPACE.                 
000041 77  WS-IDKUNDNR                 PIC X(6)    VALUE SPACE.                 
000042 77  WS-IDORDNR                  PIC X(5)    VALUE SPACE.                 
000043 77  WS-IDKOLLI                  PIC X(5)    VALUE SPACE.                 
000044 77  WS-IDPRODNR                 PIC X(7)    VALUE SPACE.                 
000045 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
000046                                                                          
000047 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
000048     88  NYCKLAR-OK                          VALUE 'J'.                   
000049     88  NYCKLAR-FEL                         VALUE 'N'.                   
000050                                                                          
000051 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
000052     88  EGEN-MID                            VALUE '4727'.                
000053     88  GODK-MID                            VALUE '4721' '4722'          
000054                                                   '4723' '4724'          
000055                                                   '4725' '4726'          
000056                                                   '4727' '4728'          
000057                                                   '4729'.                
000058     88  HELP-MID                            VALUE '0551'.                
000059     EJECT                                                                
000060*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
000061 01  GENERELLA-SUBPROGRAM.                                                
000062     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
000063     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000064     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000065     EJECT                                                                
000066*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
000067*01 -COPY WMEDAREA                                                        
000068                                                                          
000069 01  MESSAGE-CODES.                                                       
000070     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
000071     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
000072     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
000073     03  ERR-KEY-MISSING         PIC X(3)    VALUE '005'.                 
000074     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
000075     EJECT                                                                
000076*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
000077*                                                                         
000078 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
000079                                                                          
000080*01  MID -COPY W4I72701                                                   
000081     EJECT                                                                
000082 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
000083                                                                          
000084*01  -COPY WMSGAREA                                                       
000085     EJECT                                                                
000086     03  MOD REDEFINES MSG-AREA.                                          
000087*      05  -COPY W4O72701                                                 
000088     EJECT                                                                
000089 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
000090                                                                          
000091*01  -COPY WMFSAREA                                                       
000092     EJECT                                                                
000093*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000094 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000095                                                                          
000096 01  NYCKLAR-TILL-DLI.                                                    
000097     03  W-IDFAKT-X.                                                      
000098         05 W-IDFAKT             PIC S9(7)   COMP-3.                      
000099                                                                          
000100     03  W-WDL511KY-X.                                                    
000101         05  W-IDPRODNR          PIC S9(7)   COMP-3.                      
000102         05  W-IDKOLLI           PIC S9(5)   COMP-3.                      
000103                                                                          
000104     03  W-IDPRODNR-MIN-X.                                                
000105         05  W-IDPRODNR-MIN      PIC S9(7)   VALUE ZERO COMP-3.           
000106                                                                          
000107     03  W-IDPRODNR-MAX-X.                                                
000108         05  W-IDPRODNR-MAX      PIC S9(7)   VALUE ZERO COMP-3.           
000109                                                                          
000110     03  W-IDKOLLI-MIN-X.                                                 
000111         05  W-IDKOLLI-MIN       PIC S9(5)   VALUE ZERO COMP-3.           
000112                                                                          
000113     03  W-IDKOLLI-MAX-X.                                                 
000114         05  W-IDKOLLI-MAX       PIC S9(5)   VALUE ZERO COMP-3.           
000115                                                                          
000116     03  W-IDKUNDNR-MIN-X.                                                
000117         05  W-IDKUNDNR-MIN      PIC S9(7)   VALUE ZERO COMP-3.           
000118                                                                          
000119     03  W-IDKUNDNR-MAX-X.                                                
000120         05  W-IDKUNDNR-MAX      PIC S9(7)   VALUE ZERO COMP-3.           
000121                                                                          
000122     03  W-IDORDNR-MIN-X.                                                 
000123         05  W-IDORDNR-MIN       PIC  9(7)   VALUE ZERO.                  
000124                                                                          
000125     03  W-IDORDNR-MAX-X.                                                 
000126         05  W-IDORDNR-MAX       PIC  9(7)   VALUE ZERO.                  
000127                                                                          
000128     03  W-WDL521KY-X.                                                    
000129         05  W-IDARTNR           PIC S9(9)   COMP-3.                      
000130         05  W-IDPURAD           PIC S9(5)   COMP-3.                      
000131                                                                          
000132     03  W-WDL521KY-MIN-X.                                                
000133         05  W-IDARTNR-MIN       PIC S9(9)   COMP-3.                      
000134         05  W-IDPURAD-MIN       PIC S9(5)   COMP-3 VALUE +0.             
000135                                                                          
000136     03  W-WDL521KY-MAX-X.                                                
000137         05  W-IDARTNR-MAX       PIC S9(9)   COMP-3.                      
000138         05  W-IDPURAD-MAX       PIC S9(5)   COMP-3 VALUE +99999.         
000139     EJECT                                                                
000140*    --- STATUS-KOD FRÅN IMS                                              
000141 01  STATUS-WS                   PIC XX.                                  
000142     88  SEGMENT-FINNS                       VALUE '  '.                  
000143     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000144     88  BASEN-SLUT                          VALUE 'GB'.                  
000145                                                                          
000146 01  GODK-STATUSKODER.                                                    
000147     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000148                                                                          
000149 01  SSA1                        PIC X(256).                              
000150 01  SSA2                        PIC X(96).                               
000151     EJECT                                                                
000152*    --- IMS FUNKTIONSKODER                                               
000153*01  -COPY W0003                                                          
000154     EJECT                                                                
000155*    ---  DLI INPUT-OUTPUT AREA                                           
000156 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
000157                                                                          
000158 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-L501'.         
000159 01  DLI-IO-AREA-WDL501.                                                  
000160*    03  -COPY WDL501                                                     
000161     EJECT                                                                
000162 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-L511'.         
000163 01  DLI-IO-AREA-WDL511.                                                  
000164*    03  -COPY WDL511                                                     
000165     EJECT                                                                
000166 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-L521'.         
000167 01  DLI-IO-AREA-WDL521.                                                  
000168*    03  -COPY WDL521                                                     
000169     EJECT                                                                
000170 LINKAGE SECTION.                                                         
000171*01  -COPY W0009  -PRE MSG-                                               
000172*01  -COPY W0008  -PRE WDL5-                                              
000173     05  FILLER                  PIC X.                                   
000174     EJECT                                                                
000175 PROCEDURE DIVISION  USING MSG-PCB WDL5-PCB.                              
000176     ENTRY 'DLITCBL' USING MSG-PCB WDL5-PCB.                              
000177                                                                          
000178     PERFORM IMS-GET-MSG                                                  
000179     IF SEGMENT-FINNS                                                     
000180       PERFORM A-INIT                                                     
000181       PERFORM B-KOLLA-NYCKLAR                                            
000182       IF NYCKLAR-OK                                                      
000183         IF MFS-FIRST                                                     
000184           PERFORM C-FOERSTA-SIDA                                         
000185         ELSE                                                             
000186           IF MFS-NEXT                                                    
000187             PERFORM D-NAESTA-SIDA                                        
000188           ELSE                                                           
000189             PERFORM E-SAMMA-SIDA                                         
000190           END-IF                                                         
000191         END-IF                                                           
000192         PERFORM F-LAES-VISA-INFO                                         
000193       END-IF                                                             
000194       PERFORM Z-FINIT                                                    
000195     END-IF                                                               
000196                                                                          
000197     MOVE ZERO                  TO RETURN-CODE                            
000198     GOBACK                                                               
000199     .                                                                    
000200     EJECT                                                                
000201 A-INIT                         SECTION.                                  
000202                                                                          
000203     IF MSG-DUBBLA-TRANSKODER                                             
000204       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I72701                 
000205       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
000206       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
000207     ELSE                                                                 
000208       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I72701                  
000209       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
000210       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
000211     END-IF                                                               
000212                                                                          
000213     MOVE MSG-KDTRTYP           TO MFS-KDTRTYP                            
000214     MOVE MSG-IDPFK             TO MFS-IDPFK                              
000215     MOVE MFS-IDTRANS           TO W-IDTRANS                              
000216                                                                          
000217     MOVE LOW-VALUE             TO MSG-AREA                               
000218     MOVE 'W4O72701'            TO MFS-IDMOD                              
000219     MOVE '4727'                TO MOD-IDTRANS                            
000220     MOVE MFS-RENSA-FAELT       TO MOD-TEMFSFEL MOD-TEMFSINF              
000221     MOVE SPACE                 TO MED-IDMFSFEL MED-IDMFSINF              
000222                                                                          
000223     COMPUTE MAX-MOD-LAENGD = LENGTH OF MOD-W4O72701 + 4                  
000224                                                                          
000225     IF EGEN-MID                                                          
000226       CONTINUE                                                           
000227     ELSE                                                                 
000228       MOVE SPACE               TO MFS-KDTRTYP                            
000229       MOVE '7'                 TO MFS-IDPFK                              
000230     END-IF                                                               
000231                                                                          
000232     IF ENGLISH-TEXT                                                      
000233       MOVE 'GB '               TO MED-IDSKYLT                            
000234     ELSE                                                                 
000235       MOVE 'S  '               TO MED-IDSKYLT                            
000236     END-IF                                                               
000237     .                                                                    
000238     EJECT                                                                
000239 B-KOLLA-NYCKLAR                SECTION.                                  
000240                                                                          
000241     MOVE JA                    TO NYCKLAR-SW                             
000242                                                                          
000243*    -- KONTROLL AV IDFAKT                                                
000244     MOVE MFS-RENSA-FAELT       TO MOD-IDFAKT-IN                          
000245                                                                          
000246     IF MID-IDFAKT-IN = ALL '+'                                           
000247       MOVE MID-IDFAKT-UT       TO WS-IDFAKT                              
000248       INSPECT WS-IDFAKT REPLACING LEADING SPACE BY ZERO                  
000249     ELSE                                                                 
000250       MOVE MID-IDFAKT-IN       TO WS-IDFAKT                              
000251       MOVE '7'                 TO MFS-IDPFK                              
000252       MOVE SPACE               TO MFS-KDTRTYP                            
000253     END-IF                                                               
000254     IF WS-IDFAKT NUMERIC AND WS-IDFAKT > ZERO                            
000255       MOVE WS-IDFAKT           TO W-IDFAKT                               
000256     ELSE                                                                 
000257       MOVE NEJ                 TO NYCKLAR-SW                             
000258     END-IF                                                               
000259                                                                          
000260*    -- KONTROLL AV IDKUNDNR                                              
000261     MOVE MFS-RENSA-FAELT       TO MOD-IDKUNDNR-IN                        
000262                                                                          
000263     IF MID-IDKUNDNR-IN = ALL '+'                                         
000264       MOVE MID-IDKUNDNR-UT     TO WS-IDKUNDNR                            
000265       INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO                
000266     ELSE                                                                 
000267       MOVE MID-IDKUNDNR-IN     TO WS-IDKUNDNR                            
000268       MOVE '7'                 TO MFS-IDPFK                              
000269       MOVE SPACE               TO MFS-KDTRTYP                            
000270     END-IF                                                               
000271     IF WS-IDKUNDNR NUMERIC                                               
000272       CONTINUE                                                           
000273     ELSE                                                                 
000274       MOVE NEJ                 TO NYCKLAR-SW                             
000275     END-IF                                                               
000276                                                                          
000277*    -- KONTROLL AV IDORDNR                                               
000278     MOVE MFS-RENSA-FAELT       TO MOD-IDORDNR-IN                         
000279                                                                          
000280     IF MID-IDORDNR-IN = ALL '+'                                          
000281       MOVE MID-IDORDNR-UT      TO WS-IDORDNR                             
000282       INSPECT WS-IDORDNR REPLACING LEADING SPACE BY ZERO                 
000283     ELSE                                                                 
000284       MOVE MID-IDORDNR-IN      TO WS-IDORDNR                             
000285       MOVE '7'                 TO MFS-IDPFK                              
000286       MOVE SPACE               TO MFS-KDTRTYP                            
000287     END-IF                                                               
000288     IF WS-IDORDNR NUMERIC                                                
000289       CONTINUE                                                           
000290     ELSE                                                                 
000291       MOVE NEJ                 TO NYCKLAR-SW                             
000292     END-IF                                                               
000293                                                                          
000294*    -- KONTROLL AV IDKOLLI                                               
000295     MOVE MFS-RENSA-FAELT       TO MOD-IDKOLLI-IN                         
000296                                                                          
000297     IF MID-IDKOLLI-IN = ALL '+'                                          
000298       MOVE MID-IDKOLLI-UT      TO WS-IDKOLLI                             
000299       INSPECT WS-IDKOLLI REPLACING LEADING SPACE BY ZERO                 
000300     ELSE                                                                 
000301       MOVE MID-IDKOLLI-IN      TO WS-IDKOLLI                             
000302       MOVE '7'                 TO MFS-IDPFK                              
000303       MOVE SPACE               TO MFS-KDTRTYP                            
000304     END-IF                                                               
000305     IF WS-IDKOLLI NUMERIC                                                
000306       CONTINUE                                                           
000307     ELSE                                                                 
000308       MOVE NEJ                 TO NYCKLAR-SW                             
000309     END-IF                                                               
000310                                                                          
000311*    -- KONTROLL AV IDPRODNR                                              
000312     MOVE MFS-RENSA-FAELT       TO MOD-IDPRODNR-IN                        
000313                                                                          
000314     IF MID-IDPRODNR-IN = ALL '+'                                         
000315       MOVE MID-IDPRODNR-UT     TO WS-IDPRODNR                            
000316       INSPECT WS-IDPRODNR REPLACING LEADING SPACE BY ZERO                
000317     ELSE                                                                 
000318       MOVE MID-IDPRODNR-IN     TO WS-IDPRODNR                            
000319       MOVE '7'                 TO MFS-IDPFK                              
000320       MOVE SPACE               TO MFS-KDTRTYP                            
000321     END-IF                                                               
000322     IF WS-IDPRODNR NOT NUMERIC                                           
000323       MOVE NEJ                 TO NYCKLAR-SW                             
000324     END-IF                                                               
000325                                                                          
000326*    -- KONTROLL AV IDARTNR                                               
000327     MOVE MFS-RENSA-FAELT       TO MOD-IDARTNR-IN                         
000328                                                                          
000329     IF MID-IDARTNR-IN = ALL '+'                                          
000330       MOVE MID-IDARTNR-UT      TO WS-IDARTNR                             
000331       INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO                 
000332     ELSE                                                                 
000333       MOVE MID-IDARTNR-IN      TO WS-IDARTNR                             
000334       MOVE '7'                 TO MFS-IDPFK                              
000335       MOVE SPACE               TO MFS-KDTRTYP                            
000336     END-IF                                                               
000337                                                                          
000338     IF NYCKLAR-OK                                                        
000339       PERFORM BA-KOLLA-NYCKEL-SAMBAND                                    
000340     END-IF                                                               
000341                                                                          
000342     IF EGEN-MID AND NYCKLAR-OK                                           
000343       MOVE WS-IDFAKT           TO MOD-IDFAKT-UT                          
000344       INSPECT MOD-IDFAKT-UT REPLACING LEADING ZERO BY SPACE              
000345       MOVE WS-IDKUNDNR         TO MOD-IDKUNDNR-UT                        
000346       INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE            
000347       MOVE WS-IDORDNR          TO MOD-IDORDNR-UT                         
000348       INSPECT MOD-IDORDNR-UT REPLACING LEADING ZERO BY SPACE             
000349       MOVE WS-IDKOLLI          TO MOD-IDKOLLI-UT                         
000350       INSPECT MOD-IDKOLLI-UT REPLACING LEADING ZERO BY SPACE             
000351       MOVE WS-IDPRODNR         TO MOD-IDPRODNR-UT                        
000352       INSPECT MOD-IDPRODNR-UT REPLACING LEADING ZERO BY SPACE            
000353       MOVE WS-IDARTNR          TO MOD-IDARTNR-UT                         
000354       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
000355     ELSE                                                                 
000356       MOVE MFS-RENSA-FAELT     TO MOD-IDFAKT-UT                          
000357                               MOD-IDKUNDNR-UT                            
000358                               MOD-IDORDNR-UT                             
000359                               MOD-IDKOLLI-UT                             
000360                               MOD-IDPRODNR-UT                            
000361                               MOD-IDARTNR-UT                             
000362     END-IF                                                               
000363                                                                          
000364     IF NYCKLAR-FEL                                                       
000365       MOVE ERR-WRONG-KEY       TO MED-IDMFSFEL                           
000366       PERFORM MFS-RENSA-FAELT-UT                                         
000367     END-IF                                                               
000368     .                                                                    
000369     EJECT                                                                
000370 BA-KOLLA-NYCKEL-SAMBAND        SECTION.                                  
000371                                                                          
000372     IF WS-IDPRODNR              > ZERO                                   
000373       MOVE WS-IDPRODNR         TO W-IDPRODNR-MIN                         
000374                                   W-IDPRODNR-MAX                         
000375     ELSE                                                                 
000376       MOVE ZERO                TO W-IDPRODNR-MIN                         
000377       MOVE +9999999            TO W-IDPRODNR-MAX                         
000378     END-IF                                                               
000379                                                                          
000380     IF WS-IDKOLLI               > ZERO                                   
000381       MOVE WS-IDKOLLI          TO W-IDKOLLI-MIN                          
000382                                   W-IDKOLLI-MAX                          
000383     ELSE                                                                 
000384       MOVE ZERO                TO W-IDKOLLI-MIN                          
000385       MOVE +99999              TO W-IDKOLLI-MAX                          
000386     END-IF                                                               
000387                                                                          
000388     IF WS-IDARTNR               > ZERO                                   
000389       MOVE WS-IDARTNR          TO W-IDARTNR-MIN                          
000390                                   W-IDARTNR-MAX                          
000391     ELSE                                                                 
000392       MOVE ZERO                TO W-IDARTNR-MIN                          
000393       MOVE +999999999          TO W-IDARTNR-MAX                          
000394     END-IF                                                               
000395                                                                          
000396     IF WS-IDORDNR               > ZERO                                   
000397       MOVE WS-IDORDNR          TO W-IDORDNR-MIN                          
000398                                   W-IDORDNR-MAX                          
000399     ELSE                                                                 
000400       MOVE ZERO                TO W-IDORDNR-MIN                          
000401       MOVE 9999999             TO W-IDORDNR-MAX                          
000402     END-IF                                                               
000403                                                                          
000404     IF WS-IDKUNDNR              > ZERO                                   
000405       MOVE WS-IDKUNDNR         TO W-IDKUNDNR-MIN                         
000406                                   W-IDKUNDNR-MAX                         
000407     ELSE                                                                 
000408       MOVE ZERO                TO W-IDKUNDNR-MIN                         
000409       MOVE +9999999            TO W-IDKUNDNR-MAX                         
000410     END-IF                                                               
000411     .                                                                    
000412     EJECT                                                                
000413 C-FOERSTA-SIDA                 SECTION.                                  
000414                                                                          
000415     MOVE INF-FIRST-PAGE        TO MED-IDMFSINF                           
000416     .                                                                    
000417     EJECT                                                                
000418 D-NAESTA-SIDA                  SECTION.                                  
000419                                                                          
000420     IF MID-IDPRODNR-NEXT > ZERO                                          
000421       MOVE MID-IDPRODNR-NEXT   TO W-IDPRODNR                             
000422       MOVE MID-IDKOLLI-NEXT    TO W-IDKOLLI                              
000423       MOVE MID-IDARTNR-NEXT    TO W-IDARTNR                              
000424       MOVE MID-IDLOPNR-NEXT    TO W-IDPURAD                              
000425     END-IF                                                               
000426     .                                                                    
000427     EJECT                                                                
000428 E-SAMMA-SIDA                   SECTION.                                  
000429                                                                          
000430     IF EGEN-MID OR HELP-MID                                              
000431       IF MID-IDPRODNR-ENTER > ZERO                                       
000432         MOVE MID-IDPRODNR-ENTER TO W-IDPRODNR                            
000433         MOVE MID-IDKOLLI-ENTER TO W-IDKOLLI                              
000434         MOVE MID-IDARTNR-ENTER TO W-IDARTNR                              
000435         MOVE MID-IDLOPNR-ENTER TO W-IDPURAD                              
000436       END-IF                                                             
000437     END-IF                                                               
000438     .                                                                    
000439     EJECT                                                                
000440 F-LAES-VISA-INFO               SECTION.                                  
000441                                                                          
000442     PERFORM IMS-GU-WDL501                                                
000443                                                                          
000444     IF SEGMENT-SAKNAS OR BASEN-SLUT                                      
000445        MOVE ERR-KEY-MISSING    TO MED-IDMFSFEL                           
000446        PERFORM MFS-RENSA-FAELT-UT                                        
000447     ELSE                                                                 
000448       IF NOT MFS-FIRST                                                   
000449         PERFORM IMS-GNP-WDL511-UNIK                                      
000450       ELSE                                                               
000451         PERFORM IMS-GNP-WDL511                                           
000452       END-IF                                                             
000453       IF SEGMENT-SAKNAS                                                  
000454          MOVE ERR-KEY-MISSING    TO MED-IDMFSFEL                         
000455          PERFORM MFS-RENSA-FAELT-UT                                      
000456       ELSE                                                               
000457                                                                          
000458         PERFORM FA-INIT-MOD-LINES                                        
000459       END-IF                                                             
000460     END-IF                                                               
000461     .                                                                    
000462     EJECT                                                                
000463 FA-INIT-MOD-LINES SECTION.                                               
000464                                                                          
000465     MOVE FAKC-IDPRODNR TO W-IDPRODNR                                     
000466     MOVE FAKC-IDKOLLI  TO W-IDKOLLI                                      
000467     IF NOT MFS-FIRST                                                     
000468       PERFORM IMS-GNP-WDL521-UNIK                                        
000469     ELSE                                                                 
000470       PERFORM IMS-GNP-WDL521                                             
000471     END-IF                                                               
000472     MOVE +0                  TO INDX                                     
000473     PERFORM UNTIL INDX > MAX-INDX                                        
000474       PERFORM UNTIL SEGMENT-SAKNAS                                       
000475                  OR INDX = MAX-INDX                                      
000476         ADD  +1              TO INDX                                     
000477         IF INDX = 1                                                      
000478           MOVE FAKC-IDPRODNR TO MOD-IDPRODNR-ENTER                       
000479           MOVE FAKC-IDKOLLI  TO MOD-IDKOLLI-ENTER                        
000480           MOVE FAKL-IDARTNR  TO MOD-IDARTNR-ENTER                        
000481           MOVE FAKL-IDPURAD  TO MOD-IDLOPNR-ENTER                        
000482         END-IF                                                           
000483         MOVE FAKC-IDDISTR    TO MOD-IDDISTR (INDX)                       
000484         MOVE FAKC-IDKUNDNR   TO MOD-IDKUNDNR(INDX)                       
000485         MOVE FAKC-IDORDNR7   TO MOD-IDORDNR7(INDX)                       
000486         MOVE FAKC-IDPRODNR   TO MOD-IDPRODNR(INDX)                       
000487         MOVE FAK-IDDC        TO MOD-IDDC    (INDX)                       
000488         MOVE FAKC-IDKOLLI    TO MOD-IDKOLLI (INDX)                       
000489         MOVE FAKL-IDARTNR    TO MOD-IDARTNR (INDX)                       
000490         MOVE FAKL-KVBEART-Q  TO MOD-KVBEART (INDX)                       
000491         MOVE FAKL-KVAVBART   TO MOD-KVAVBART(INDX)                       
000492         MOVE FAKL-KVLEVART   TO MOD-KVLEVART(INDX)                       
000493         MOVE FAKL-IDLEVNR    TO MOD-IDLEVNR (INDX)                       
000494                                                                          
000495         PERFORM IMS-GNP-WDL521                                           
000496       END-PERFORM                                                        
000497       IF SEGMENT-SAKNAS                                                  
000498         PERFORM IMS-GNP-WDL511                                           
000499         IF SEGMENT-SAKNAS                                                
000500           PERFORM UNTIL INDX = MAX-INDX                                  
000510             ADD 1          TO INDX                                       
000511             PERFORM MFS-RENSA-RAD-FAELT-UT                               
000512           END-PERFORM                                                    
000513         ELSE                                                             
000514           MOVE FAKC-IDPRODNR TO W-IDPRODNR                               
000515           MOVE FAKC-IDKOLLI  TO W-IDKOLLI                                
000516           PERFORM IMS-GNP-WDL521                                         
000517         END-IF                                                           
000518         IF INDX = MAX-INDX                                               
000519           ADD  +1 TO INDX                                                
000520         END-IF                                                           
000521       ELSE                                                               
000522         ADD  +1 TO INDX                                                  
000523       END-IF                                                             
000524     END-PERFORM                                                          
000525                                                                          
000526     IF SEGMENT-FINNS                                                     
000527       MOVE FAKC-IDPRODNR     TO MOD-IDPRODNR-NEXT                        
000528       MOVE FAKC-IDKOLLI      TO MOD-IDKOLLI-NEXT                         
000529       MOVE FAKL-IDARTNR      TO MOD-IDARTNR-NEXT                         
000530       MOVE FAKL-IDPURAD      TO MOD-IDLOPNR-NEXT                         
000531                                                                          
000532       MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                          
000533     ELSE                                                                 
000534       MOVE MOD-IDPRODNR-ENTER TO MOD-IDPRODNR-NEXT                       
000535       MOVE MOD-IDKOLLI-ENTER  TO MOD-IDKOLLI-NEXT                        
000536       MOVE MOD-IDARTNR-ENTER  TO MOD-IDARTNR-NEXT                        
000537       MOVE MOD-IDLOPNR-ENTER  TO MOD-IDLOPNR-NEXT                        
000538       MOVE INF-LAST-PAGE      TO MED-IDMFSINF                            
000539     END-IF                                                               
000540     .                                                                    
000541     EJECT                                                                
000542 Z-FINIT                        SECTION.                                  
000543                                                                          
000544     IF MED-IDMFSFEL NOT = SPACE OR MED-IDMFSINF NOT = SPACE              
000545       CALL WMEDKONV USING MED-WMEDAREA                                   
000546       MOVE MED-MFSFEL          TO MOD-TEMFSFEL                           
000547       MOVE MED-MFSINF          TO MOD-TEMFSINF                           
000548     END-IF                                                               
000549                                                                          
000550     MOVE MAX-MOD-LAENGD        TO MSG-KVLL                               
000551     PERFORM IMS-INSERT-MSG                                               
000552     .                                                                    
000553 MFS-RENSA-FAELT-UT             SECTION.                                  
000554                                                                          
000555*    --- ALLA UTDATA-FÄLT                                                 
000556*    --- INKL. BLÄDDRINGSNYCKLAR                                          
000557     MOVE ZERO                  TO MOD-IDPRODNR-ENTER                     
000558                                   MOD-IDKOLLI-ENTER                      
000559                                   MOD-IDARTNR-ENTER                      
000560                                   MOD-IDLOPNR-ENTER                      
000561                                   MOD-IDPRODNR-NEXT                      
000562                                   MOD-IDKOLLI-NEXT                       
000563                                   MOD-IDARTNR-NEXT                       
000564                                   MOD-IDLOPNR-NEXT                       
000565     MOVE +1                    TO INDX                                   
000566     PERFORM UNTIL INDX > MAX-INDX                                        
000567       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
000568       ADD +1                   TO INDX                                   
000569     END-PERFORM                                                          
000570     .                                                                    
000571                                                                          
000572 MFS-RENSA-RAD-FAELT-UT         SECTION.                                  
000573                                                                          
000574*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
000575     MOVE MFS-RENSA-FAELT       TO MOD-IDDISTR (INDX)                     
000576                                   MOD-IDKUNDNR(INDX)                     
000577                                   MOD-IDORDNR7(INDX)                     
000578                                   MOD-IDPRODNR(INDX)                     
000579                                   MOD-IDDC    (INDX)                     
000580                                   MOD-IDKOLLI (INDX)                     
000581                                   MOD-IDARTNR (INDX)                     
000582                                   MOD-KVBEART (INDX)                     
000583                                   MOD-KVAVBART (INDX)                    
000584                                   MOD-KVLEVART (INDX)                    
000585                                   MOD-IDLEVNR  (INDX)                    
000586          .                                                               
000587     EJECT                                                                
000588* --- IMS SEKTIONER ---                                                   
000589                                                                          
000590 IMS-GET-MSG                    SECTION.                                  
000591                                                                          
000592     MOVE '  QC' TO GODK-STATUSKODER                                      
000593     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
000594     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
000595     PERFORM IMS-STATUSKONTROLL                                           
000596     .                                                                    
000597                                                                          
000598 IMS-INSERT-MSG                 SECTION.                                  
000599                                                                          
000600     IF ENGLISH-TEXT                                                      
000601       MOVE 'N' TO MFS-KDHUVOMR                                           
000602     END-IF                                                               
000603     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
000604     MOVE SPACE TO GODK-STATUSKODER                                       
000605     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
000606     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
000607     PERFORM IMS-STATUSKONTROLL                                           
000608     .                                                                    
000609     EJECT                                                                
000610 IMS-STATUSKONTROLL             SECTION.                                  
000611                                                                          
000612 IMS-GU-WDL501                 SECTION.                                   
000613                                                                          
000614     STRING 'WDL501  (IDFAKT   =' W-IDFAKT-X ')'                          
000615          DELIMITED BY SIZE INTO SSA1                                     
000616     MOVE '  GE' TO GODK-STATUSKODER                                      
000617     CALL CBLTDLI USING GU WDL5-PCB DLI-IO-AREA-WDL501 SSA1               
000618     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
000619     PERFORM IMS-STATUSKONTROLL                                           
000620     .                                                                    
000621     EJECT                                                                
000622 IMS-GNP-WDL511                 SECTION.                                  
000623                                                                          
000624     STRING 'WDL511  (IDPRODNR>=' W-IDPRODNR-MIN-X                        
000625                    '&IDPRODNR<=' W-IDPRODNR-MAX-X                        
000626                    '&IDKOLLI >=' W-IDKOLLI-MIN-X                         
000627                    '&IDKOLLI <=' W-IDKOLLI-MAX-X                         
000628                    '&IDKUNDNR>=' W-IDKUNDNR-MIN-X                        
000629                    '&IDKUNDNR<=' W-IDKUNDNR-MAX-X                        
000630                    '&IDORDNR7>=' W-IDORDNR-MIN-X                         
000631                    '&IDORDNR7<=' W-IDORDNR-MAX-X ')'                     
000632          DELIMITED BY SIZE INTO SSA1                                     
000633     MOVE '  GE' TO GODK-STATUSKODER                                      
000634     CALL CBLTDLI USING GNP WDL5-PCB DLI-IO-AREA-WDL511 SSA1              
000635     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
000636     PERFORM IMS-STATUSKONTROLL                                           
000637     .                                                                    
000638     EJECT                                                                
000639 IMS-GNP-WDL511-UNIK            SECTION.                                  
000640                                                                          
000641     STRING 'WDL511  (WDL511KY =' W-WDL511KY-X ')'                        
000642          DELIMITED BY SIZE INTO SSA1                                     
000643     MOVE '  GE' TO GODK-STATUSKODER                                      
000644     CALL CBLTDLI USING GNP WDL5-PCB DLI-IO-AREA-WDL511 SSA1              
000645     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
000646     PERFORM IMS-STATUSKONTROLL                                           
000647     .                                                                    
000648     EJECT                                                                
000649 IMS-GNP-WDL521                 SECTION.                                  
000650                                                                          
000651     STRING 'WDL511  (WDL511KY =' W-WDL511KY-X ')'                        
000652          DELIMITED BY SIZE INTO SSA1                                     
000653     STRING 'WDL521  (WDL521KY>=' W-WDL521KY-MIN-X                        
000654                    '&WDL521KY<=' W-WDL521KY-MAX-X ')'                    
000655          DELIMITED BY SIZE INTO SSA2                                     
000656     MOVE '  GE' TO GODK-STATUSKODER                                      
000657     CALL CBLTDLI USING GNP WDL5-PCB DLI-IO-AREA-WDL521 SSA1 SSA2         
000658     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
000659     PERFORM IMS-STATUSKONTROLL                                           
000660     .                                                                    
000670     EJECT                                                                
000671 IMS-GNP-WDL521-UNIK            SECTION.                                  
000672                                                                          
000673     STRING 'WDL511  (WDL511KY =' W-WDL511KY-X ')'                        
000674          DELIMITED BY SIZE INTO SSA1                                     
000675     STRING 'WDL521  (WDL521KY =' W-WDL521KY-X ')'                        
000676          DELIMITED BY SIZE INTO SSA2                                     
000677     MOVE '  GE' TO GODK-STATUSKODER                                      
000678     CALL CBLTDLI USING GNP WDL5-PCB DLI-IO-AREA-WDL521 SSA1 SSA2         
000679     MOVE WDL5-STATUS-CODE TO STATUS-WS                                   
000680     PERFORM IMS-STATUSKONTROLL                                           
000681     .                                                                    
000682     EJECT                                                                
000683     SET STATUS-IX TO 1                                                   
000684     SEARCH GODK-STATUS                                                   
000685       AT END                                                             
000686         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
000687         DELIMITED BY SIZE INTO FELTEXT                                   
000688         CALL FELLOG                                                      
000689       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
000690         CONTINUE                                                         
000691     END-SEARCH                                                           
000692     .                                                                    
