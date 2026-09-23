000001 ID DIVISION.                                                             
000002 PROGRAM-ID.     W5013100.                                                
000003 AUTHOR.         MARKUS ASPFJÄLL.                                         
000004 DATE-WRITTEN.   02/02/22.                                                
000005 DATE-COMPILED.                                                           
000006                                                                          
000007*    FUNKTION:                                                            
000008*        LÄSER WDB1 - KUNDREGISTRET                                       
000009*                                                                         
000010*        PROGRAMMET LÄSER      WDB1                                       
000011*                   UPDATES    WDB1                                       
000012*                                                                         
000013*    INDATA.                                                              
000014*        TRANSAKTION: W5T131                                              
000015*        MID:         W5I13101                                            
000016*                                                                         
000017*    UTDATA.                                                              
000018*        MOD:         W5O13101                                            
000019                                                                          
000020     SKIP3                                                                
000021 ENVIRONMENT DIVISION.                                                    
000022                                                                          
000023 DATA DIVISION.                                                           
000024     EJECT                                                                
000025 WORKING-STORAGE SECTION.                                                 
000026 77  IDPGM                       PIC X(08)   VALUE 'W5013100'.            
000027                                                                          
000028*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
000029 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
000030                                                                          
000031 77  JA                          PIC X       VALUE 'J'.                   
000032 77  NEJ                         PIC X       VALUE 'N'.                   
000033                                                                          
000034*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
000035                                                                          
000036 77  INDATA-SW                   PIC X       VALUE 'J'.                   
000037     88  INDATA-OK                           VALUE 'J'.                   
000038     88  INDATA-FEL                          VALUE 'N'.                   
000039                                                                          
000040 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
000041     88  NYCKLAR-OK                          VALUE 'J'.                   
000042     88  NYCKLAR-FEL                         VALUE 'N'.                   
000043                                                                          
000044 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
000045     88  EGEN-MID                            VALUE '5131'.                
000046     88  GODK-MID                            VALUE '5131' '5132'          
000047                                                   '5133' '5134'          
000048                                                   '5135' '5136'          
000049                                                   '5137' '5138'          
000050                                                   '5139'.                
000051     88  HELP-MID                            VALUE '0551'.                
000052     EJECT                                                                
000053                                                                          
000054 77  WS-IDFTG                    PIC X(2)   VALUE SPACE.                  
000055 77  W-RESERAVG                  PIC 9(2).9(1)   VALUE ZERO.              
000056 77  W-FLDIRVAT                  PIC X(1)   VALUE 'N'.                    
000057 77  W-FLINVGRP                  PIC X(1)   VALUE 'N'.                    
000058 77  W-IDLEVNR-FIN               PIC X(5)   VALUE SPACE.                  
000059                                                                          
000060*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
000061 01  GENERELLA-SUBPROGRAM.                                                
000062     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
000063     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
000064     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000065     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000066     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
000067     EJECT                                                                
000068*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
000069*01 -COPY WMEDAREA                                                        
000070     SKIP3                                                                
000071 01  MESSAGE-CODES.                                                       
000072     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
000073     03  ERR-NOT-NUMERIC         PIC X(3)    VALUE '020'.                 
000074     03  INF-CUSTOMER-MISSING    PIC X(3)    VALUE '063'.                 
000075     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
000076     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
000077     03  URVAL-SAKNAS            PIC X(3)    VALUE '005'.                 
000078     EJECT                                                                
000079*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
000080*                                                                         
000081 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
000082     SKIP3                                                                
000083*01 -COPY WMSGINIT                                                        
000084     EJECT                                                                
000085                                                                          
000086*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
000087*                                                                         
000088 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
000089     SKIP3                                                                
000090*01  MID -COPY W5I13101                                                   
000091     EJECT                                                                
000092 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
000093     SKIP3                                                                
000094*01  -COPY WMSGAREA                                                       
000095     EJECT                                                                
000096     03  MOD REDEFINES MSG-AREA.                                          
000097*      05  -COPY W5O13101                                                 
000098     EJECT                                                                
000099 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
000100     SKIP3                                                                
000101*01  -COPY WMFSAREA                                                       
000102     EJECT                                                                
000103 01  FILLER                      PIC X(16)   VALUE 'WDECAREA'.            
000104*    --- PARAMETRAR TILL SUBPROGRAM WDECEDIT                              
000105*   -COPY WDECAREA                                                        
000106*                                                                         
000107*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000108*                                                                         
000109 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000110     SKIP3                                                                
000111 01  NYCKLAR-TILL-DLI.                                                    
000112     03  W-WDB101KY-X.                                                    
000113         05  W-IDPARTNR          PIC X(9)    VALUE SPACE.                 
000114         05  W-IDFTG             PIC 9(2)    VALUE ZERO.                  
000115     SKIP2                                                                
000116*    --- STATUS-KOD FRÅN IMS                                              
000117 01  STATUS-WS                   PIC XX.                                  
000118     88  SEGMENT-FINNS                       VALUE '  '.                  
000119     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
000120     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000121     SKIP2                                                                
000122 01  GODK-STATUSKODER.                                                    
000123     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000124     SKIP3                                                                
000125 01  SSA1                        PIC X(64).                               
000126 01  SSA2                        PIC X(64).                               
000127     EJECT                                                                
000128*    --- IMS FUNKTIONSKODER                                               
000129*01  -COPY W0003                                                          
000130     EJECT                                                                
000131*    ---  DLI INPUT-OUTPUT AREA                                           
000132                                                                          
000133 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB101'.                      
000134 01  DLI-IO-WDB101.                                                       
000135*    03  -COPY WDB101                                                     
000136     EJECT                                                                
000137 LINKAGE SECTION.                                                         
000138*01  -COPY W0009   -PRE MSG-                                              
000139*01  -COPY W0008   -PRE WDP7-                                             
000140     05  FILLER                  PIC X.                                   
000141                                                                          
000142*01  -COPY W0008  -PRE WDB1-                                              
000143     05  FILLER                  PIC X.                                   
000144     EJECT                                                                
000145 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDB1-PCB.                     
000146 MAIN SECTION.                                                            
000147     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDB1-PCB.                     
000148                                                                          
000149     PERFORM IMS-GET-MSG                                                  
000150     IF SEGMENT-FINNS                                                     
000151       PERFORM A-INIT                                                     
000152       PERFORM B-KOLLA-NYCKLAR                                            
000153       IF NYCKLAR-OK                                                      
000154         IF MFS-UPDATE                                                    
000155           PERFORM G-KOLLA-INPUT                                          
000156           IF INDATA-OK                                                   
000157             PERFORM H-UPPDATERA-KUNDREG                                  
000158           END-IF                                                         
000159         ELSE                                                             
000160           PERFORM F-LAES-VISA-INFO                                       
000161         END-IF                                                           
000162       END-IF                                                             
000163       COMPUTE MSG-KVLL = LENGTH OF MOD-W5O13101 + 4                      
000164       PERFORM IMS-INSERT-MSG                                             
000165     END-IF                                                               
000166                                                                          
000167     MOVE ZERO TO RETURN-CODE                                             
000168     GOBACK                                                               
000169     .                                                                    
000170     EJECT                                                                
000171 A-INIT SECTION.                                                          
000172                                                                          
000173     IF MSG-DUBBLA-TRANSKODER                                             
000174       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I13101                 
000175       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
000176       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
000177     ELSE                                                                 
000178       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I13101                  
000179       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
000180       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
000181     END-IF                                                               
000182                                                                          
000183     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
000184     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
000185     MOVE MFS-IDTRANS TO W-IDTRANS                                        
000186                                                                          
000187     MOVE LOW-VALUE TO MSG-AREA                                           
000188     MOVE 'W5O131N1' TO MFS-IDMOD                                         
000189     MOVE '5131' TO MOD-IDTRANS                                           
000190     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
000191                                                                          
000192     IF EGEN-MID OR HELP-MID                                              
000193       CONTINUE                                                           
000194     ELSE                                                                 
000195       MOVE SPACE TO MFS-KDTRTYP                                          
000196       MOVE '7' TO MFS-IDPFK                                              
000197     END-IF                                                               
000198     .                                                                    
000199     EJECT                                                                
000200 B-KOLLA-NYCKLAR SECTION.                                                 
000201                                                                          
000202     MOVE ALL '+'           TO MSGI-WMSGINIT                              
000203     MOVE '001'             TO MSGI-KDCALL                                
000204     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
000205     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
000206     MOVE '5131'            TO MSGI-IDTRANS                               
000207                                                                          
000208     IF EGEN-MID                                                          
000209       MOVE MID-IDPARTNR-IN      TO MSGI-IDPARTNR                         
000210       MOVE MID-IDFTG-IN         TO MSGI-IDFTG-KEY                        
000211     END-IF                                                               
000212                                                                          
000213     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
000214                                                                          
000215*    - SPRÅK SOM SKA ANVÄNDAS AV WMEDKONV                                 
000216     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
000217                                                                          
000218     MOVE JA TO NYCKLAR-SW                                                
000219                                                                          
000220                                                                          
000221*    -- KONTROLL AV IDPARTNR                                              
000222     MOVE MFS-RENSA-FAELT        TO MOD-IDPARTNR-IN                       
000223                                                                          
000224     IF MID-IDPARTNR-IN NOT = ALL '+'                                     
000225       MOVE '7'                  TO MFS-IDPFK                             
000226       MOVE SPACE                TO MFS-KDTRTYP                           
000227     END-IF                                                               
000228                                                                          
000229     MOVE MSGI-IDPARTNR          TO W-IDPARTNR                            
000230     IF MSGI-IDPARTNR = SPACE                                             
000231       MOVE NEJ                  TO NYCKLAR-SW                            
000232     END-IF                                                               
000233*    -- KONTROLL AV IDFTG                                                 
000234     MOVE MFS-RENSA-FAELT        TO MOD-IDFTG-IN                          
000235                                                                          
000236     IF EGEN-MID                                                          
000237       IF MID-IDFTG-IN NOT = ALL '+'                                      
000238         MOVE '7'                TO MFS-IDPFK                             
000239         MOVE SPACE              TO MFS-KDTRTYP                           
000240       END-IF                                                             
000241       MOVE MSGI-IDFTG-KEY       TO WS-IDFTG                              
000242     ELSE                                                                 
000243       MOVE MSGI-IDFTG           TO WS-IDFTG                              
000244     END-IF                                                               
000245                                                                          
000246     INSPECT WS-IDFTG REPLACING LEADING SPACE BY ZERO                     
000247                                                                          
000248     IF WS-IDFTG = ZERO                                                   
000249       MOVE MSGI-IDFTG           TO WS-IDFTG                              
000250     END-IF                                                               
000251                                                                          
000252     IF WS-IDFTG NUMERIC AND WS-IDFTG > ZERO                              
000253       MOVE WS-IDFTG             TO W-IDFTG                               
000254     ELSE                                                                 
000255       MOVE NEJ                  TO NYCKLAR-SW                            
000256     END-IF                                                               
000257                                                                          
000258     IF EGEN-MID OR GODK-MID                                              
000259       MOVE W-IDPARTNR           TO MOD-IDPARTNR-UT                       
000260       MOVE WS-IDFTG             TO MOD-IDFTG-UT                          
000261       INSPECT MOD-IDFTG-UT REPLACING LEADING ZERO BY SPACE               
000262     ELSE                                                                 
000263       MOVE MFS-RENSA-FAELT      TO MOD-IDPARTNR-UT                       
000264                                    MOD-IDFTG-UT                          
000265       MOVE NEJ                  TO NYCKLAR-SW                            
000266     END-IF                                                               
000267                                                                          
000268     IF NYCKLAR-FEL                                                       
000269       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
000270       CALL WMEDKONV USING MED-WMEDAREA                                   
000271       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
000272       PERFORM MFS-RENSA-FAELT-IN                                         
000273       PERFORM MFS-RENSA-FAELT-UT                                         
000274     END-IF                                                               
000275     .                                                                    
000276     EJECT                                                                
000277 F-LAES-VISA-INFO SECTION.                                                
000278                                                                          
000279     PERFORM IMS-GET-WDB101                                               
000280                                                                          
000281     IF SEGMENT-SAKNAS                                                    
000282        MOVE URVAL-SAKNAS TO MED-IDMFSFEL                                 
000283        CALL WMEDKONV USING MED-WMEDAREA                                  
000284        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
000285        PERFORM MFS-RENSA-FAELT-UT                                        
000286     ELSE                                                                 
000287       MOVE BET-BEBETRAD-1     TO MOD-BEBETRAD-1                          
000288       MOVE BET-BEBETRAD-2     TO MOD-BEBETRAD-2                          
000289       MOVE BET-ADBETRAD-1     TO MOD-ADBETRAD-1                          
000290       MOVE BET-ADBETRAD-2     TO MOD-ADBETRAD-2                          
000291       MOVE BET-BELAND-SVE     TO MOD-BELAND-SVE                          
000292       MOVE BET-TISTADAT       TO MOD-TISTADAT                            
000293       MOVE BET-TIUPPDAT       TO MOD-TIUPPDAT                            
000294       MOVE BET-TISTODAT       TO MOD-TIBETUPH                            
000295       MOVE BET-IDUSER         TO MOD-IDUSER                              
000296       MOVE BET-IDLANDX2       TO MOD-IDLANDX2                            
000297       MOVE BET-KDVALISO       TO MOD-KDVALISO                            
000298       MOVE BET-KDVALTYP       TO MOD-KDVALTYP                            
000299       MOVE BET-KDTRADP        TO MOD-KDTRADP                             
000300       MOVE BET-RESERAVG       TO MOD-RESERAVG-IN                         
000301       IF BET-FLDIRVAT = 'J'                                              
000302         IF MFS-KDMFSFOR = '2'                                            
000303           MOVE 'Y'              TO MOD-FLDIRVAT-IN                       
000304         ELSE                                                             
000305            MOVE 'J'              TO MOD-FLDIRVAT-IN                      
000306         END-IF                                                           
000307       ELSE                                                               
000308         MOVE BET-FLDIRVAT     TO MOD-FLDIRVAT-IN                         
000309       END-IF                                                             
000310                                                                          
000311       IF BET-FLINVGRP = 'J'                                              
000312         IF MFS-KDMFSFOR = '2'                                            
000313           MOVE 'Y'              TO MOD-FLINVGRP-IN                       
000314         ELSE                                                             
000315           MOVE 'J'              TO MOD-FLINVGRP-IN                       
000316         END-IF                                                           
000317       ELSE                                                               
000318         MOVE BET-FLINVGRP     TO MOD-FLINVGRP-IN                         
000319       END-IF                                                             
000320       MOVE BET-IDLEVNR-FIN    TO MOD-IDLEVNR-FIN-IN                      
000321       MOVE BET-KDKREDSP       TO MOD-KDKREDSP                            
000322       MOVE BET-IDVAT          TO MOD-IDVAT                               
000323       MOVE BET-KDBETVIL       TO MOD-KDBETVIL                            
000324       MOVE BET-BEBETVIL       TO MOD-BEBETVIL                            
000325       MOVE BET-RELANDCO       TO MOD-RELANDCO                            
000326       MOVE BET-IDPROMR        TO MOD-IDPROMR                             
000327       IF   BET-IDMARKBO = 'A' OR 'B' OR 'C' OR 'D' OR 'E'                
000328       OR   'F' OR 'G'                                                    
000329         MOVE 'SEK' TO MOD-KDVALIS2                                       
000330       ELSE                                                               
000331         MOVE '   ' TO MOD-KDVALIS2                                       
000332       END-IF                                                             
000333                                                                          
000334     END-IF                                                               
000335     .                                                                    
000336     EJECT                                                                
000337                                                                          
000338 G-KOLLA-INPUT SECTION.                                                   
000339                                                                          
000340     MOVE JA  TO INDATA-SW                                                
000341     IF MID-RESERAVG-IN  = ALL '+'                                        
000342     AND MID-FLDIRVAT-IN  = ALL '+'                                       
000343     AND MID-FLINVGRP-IN  = ALL '+'                                       
000344     AND MID-IDLEVNR-FIN-IN  = ALL '+'                                    
000345       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
000346       CALL WMEDKONV USING MED-WMEDAREA                                   
000347       MOVE MED-MFSFEL          TO MOD-TEMFSFEL                           
000348       PERFORM MFS-ROER-EJ-FAELT-UT                                       
000349       MOVE NEJ                 TO INDATA-SW                              
000350     ELSE                                                                 
000351       IF MID-RESERAVG-IN = ALL '+'                                       
000352         CONTINUE                                                         
000353       ELSE                                                               
000354         PERFORM GA-CHECK-DB                                              
000355       END-IF                                                             
000356       IF MID-FLDIRVAT-IN = ALL '+'                                       
000357        CONTINUE                                                          
000358       ELSE                                                               
000359        IF MID-FLDIRVAT-IN = 'Y' OR 'J'                                   
000360          MOVE 'J'               TO W-FLDIRVAT                            
000361          MOVE 'Y'               TO MOD-FLDIRVAT-IN                       
000362          PERFORM GA-CHECK-DB                                             
000363        ELSE                                                              
000364          IF MID-FLDIRVAT-IN = 'N'                                        
000365            MOVE MID-FLDIRVAT-IN TO W-FLDIRVAT                            
000366                                    MOD-FLDIRVAT-IN                       
000367            PERFORM GA-CHECK-DB                                           
000368          ELSE                                                            
000369           IF MID-FLDIRVAT-IN NOT = 'Y' OR 'J' OR 'N'                     
000370            MOVE ERR-WRONG-KEY       TO MED-IDMFSFEL                      
000371            CALL WMEDKONV USING MED-WMEDAREA                              
000372            MOVE MED-MFSFEL          TO MOD-TEMFSFEL                      
000373            MOVE MFS-ALFA-FAELT-FEL  TO MOD-FLDIRVAT-IN-ATTR              
000374            MOVE NEJ                 TO INDATA-SW                         
000375            PERFORM MFS-ROER-EJ-FAELT-UT                                  
000376           END-IF                                                         
000377          END-IF                                                          
000378        END-IF                                                            
000379       END-IF                                                             
000380       IF MID-FLINVGRP-IN = ALL '+'                                       
000381        CONTINUE                                                          
000382       ELSE                                                               
000383        IF MID-FLINVGRP-IN = 'Y' OR 'J'                                   
000384          MOVE 'J'               TO W-FLINVGRP                            
000385          MOVE 'Y'               TO MOD-FLINVGRP-IN                       
000386          PERFORM GA-CHECK-DB                                             
000387        ELSE                                                              
000388          IF MID-FLINVGRP-IN = 'N'                                        
000389            MOVE MID-FLINVGRP-IN TO W-FLINVGRP                            
000390                                    MOD-FLINVGRP-IN                       
000391            PERFORM GA-CHECK-DB                                           
000392          ELSE                                                            
000393           IF MID-FLINVGRP-IN NOT = 'Y' OR 'J' OR 'N'                     
000394            MOVE ERR-WRONG-KEY       TO MED-IDMFSFEL                      
000395            CALL WMEDKONV USING MED-WMEDAREA                              
000396            MOVE MED-MFSFEL          TO MOD-TEMFSFEL                      
000397            MOVE MFS-ALFA-FAELT-FEL  TO MOD-FLINVGRP-IN-ATTR              
000398            MOVE NEJ                 TO INDATA-SW                         
000399            PERFORM MFS-ROER-EJ-FAELT-UT                                  
000400           END-IF                                                         
000401          END-IF                                                          
000402        END-IF                                                            
000404       END-IF                                                             
000405       IF MID-IDLEVNR-FIN-IN = ALL '+'                                    
000406        CONTINUE                                                          
000407       ELSE                                                               
000408         MOVE MID-IDLEVNR-FIN-IN    TO W-IDLEVNR-FIN                      
000409         MOVE MID-IDLEVNR-FIN-IN    TO MOD-IDLEVNR-FIN-IN                 
000410       END-IF                                                             
000411     END-IF                                                               
000412     .                                                                    
000413     EJECT                                                                
000414 GA-CHECK-DB SECTION.                                                     
000415                                                                          
000416     PERFORM IMS-GU-WDB101                                                
000417     IF SEGMENT-FINNS                                                     
000418      IF MID-RESERAVG-IN NOT = ALL '+'                                    
000419       MOVE MID-RESERAVG-IN     TO DEC-IDFRIDATA                          
000420       MOVE 2                   TO DEC-KVHELTAL                           
000421       MOVE 1                   TO DEC-KVDECIMAL                          
000422       CALL WDECEDIT USING DEC-WDECAREA                                   
000423       IF DEC-KDSVAR-OK                                                   
000424         MOVE MFS-NUM-FAELT-RAETT      TO                                 
000425                                   MOD-RESERAVG-IN-ATTR                   
000426         MOVE DEC-IDEDITDATA           TO W-RESERAVG                      
000427       ELSE                                                               
000428         MOVE NEJ                      TO INDATA-SW                       
000429         MOVE MFS-NUM-FAELT-FEL        TO MOD-RESERAVG-IN-ATTR            
000430         MOVE MFS-ROER-EJ-FAELT        TO MOD-RESERAVG-IN                 
000431         MOVE ERR-NOT-NUMERIC          TO MED-IDMFSFEL                    
000432         CALL WMEDKONV USING MED-WMEDAREA                                 
000433         MOVE MED-MFSFEL               TO MOD-TEMFSFEL                    
000434         PERFORM MFS-ROER-EJ-FAELT-UT                                     
000435       END-IF                                                             
000436      END-IF                                                              
000437     ELSE                                                                 
000438       MOVE INF-CUSTOMER-MISSING TO MED-IDMFSINF                          
000439       CALL WMEDKONV USING MED-WMEDAREA                                   
000440       MOVE MED-MFSINF          TO MOD-TEMFSINF                           
000441       PERFORM MFS-RENSA-FAELT-IN                                         
000442       MOVE NEJ                 TO INDATA-SW                              
000443     END-IF                                                               
000444     .                                                                    
000445     EJECT                                                                
000446 H-UPPDATERA-KUNDREG SECTION.                                             
000447                                                                          
000448     PERFORM IMS-GHU-WDB101                                               
000449     IF SEGMENT-FINNS                                                     
000450       IF MID-RESERAVG-IN          = ALL '+'                              
000451         MOVE BET-RESERAVG        TO MOD-RESERAVG-IN                      
000452       ELSE                                                               
000453         MOVE W-RESERAVG          TO BET-RESERAVG                         
000454                                     MOD-RESERAVG-IN                      
000455       END-IF                                                             
000456       IF MID-FLDIRVAT-IN          = ALL '+'                              
000457         IF BET-FLDIRVAT = 'J'                                            
000458           MOVE 'Y'               TO MOD-FLDIRVAT-IN                      
000459         ELSE                                                             
000460           MOVE BET-FLDIRVAT      TO MOD-FLDIRVAT-IN                      
000461         END-IF                                                           
000462       ELSE                                                               
000463         MOVE W-FLDIRVAT          TO BET-FLDIRVAT                         
000464       END-IF                                                             
000465                                                                          
000466       IF MID-FLINVGRP-IN          = ALL '+'                              
000467         IF BET-FLINVGRP = 'J'                                            
000468           MOVE 'Y'               TO MOD-FLINVGRP-IN                      
000469         ELSE                                                             
000470           MOVE BET-FLINVGRP      TO MOD-FLINVGRP-IN                      
000471         END-IF                                                           
000472       ELSE                                                               
000473         MOVE W-FLINVGRP          TO BET-FLINVGRP                         
000474       END-IF                                                             
000475       IF MID-IDLEVNR-FIN-IN       = ALL '+'                              
000476         MOVE BET-IDLEVNR-FIN     TO MOD-IDLEVNR-FIN-IN                   
000477       ELSE                                                               
000478         MOVE W-IDLEVNR-FIN       TO BET-IDLEVNR-FIN                      
000479       END-IF                                                             
000480       PERFORM IMS-REPL-WDB101                                            
000481                                                                          
000482       MOVE INF-UPDATE-DONE       TO MED-IDMFSINF                         
000483       CALL WMEDKONV USING MED-WMEDAREA                                   
000484       MOVE MED-MFSINF            TO MOD-TEMFSINF                         
000485       PERFORM MFS-ROER-EJ-FAELT-UT                                       
000486     END-IF                                                               
000487     .                                                                    
000488     EJECT                                                                
000489 MFS-RENSA-FAELT-IN SECTION.                                              
000490                                                                          
000491*    --- ALLA INDATA-FÄLT                                                 
000492     MOVE MFS-RENSA-FAELT    TO MOD-IDPARTNR-IN                           
000493     MOVE MFS-RENSA-FAELT    TO MOD-IDFTG-IN                              
000494     MOVE MFS-RENSA-FAELT    TO MOD-RESERAVG-IN                           
000495     MOVE MFS-RENSA-FAELT    TO MOD-FLDIRVAT-IN                           
000496     MOVE MFS-RENSA-FAELT    TO MOD-FLINVGRP-IN                           
000497     MOVE MFS-RENSA-FAELT    TO MOD-IDLEVNR-FIN-IN                        
000498     .                                                                    
000499     EJECT                                                                
000500 MFS-RENSA-FAELT-UT SECTION.                                              
000501                                                                          
000502*    --- ALLA UTDATA-FÄLT                                                 
000503     MOVE MFS-RENSA-FAELT    TO MOD-BEBETRAD-1                            
000504                                MOD-BEBETRAD-2                            
000505                                MOD-ADBETRAD-1                            
000506                                MOD-ADBETRAD-2                            
000507                                MOD-BELAND-SVE                            
000508                                MOD-TISTADAT                              
000509                                MOD-TIUPPDAT                              
000510                                MOD-TIBETUPH                              
000511                                MOD-IDUSER                                
000512                                MOD-IDLANDX2                              
000513                                MOD-KDVALISO                              
000514                                MOD-KDVALTYP                              
000515                                MOD-KDTRADP                               
000516                                MOD-KDKREDSP                              
000517                                MOD-IDVAT                                 
000518                                MOD-KDBETVIL                              
000519                                MOD-BEBETVIL                              
000520                                MOD-RELANDCO                              
000521                                MOD-IDPROMR                               
000522                                                                          
000523                                                                          
000524     .                                                                    
000525     EJECT                                                                
000526 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
000527                                                                          
000528     MOVE MFS-ROER-EJ-FAELT     TO MOD-BEBETRAD-1                         
000529                                   MOD-TISTADAT                           
000530                                   MOD-BEBETRAD-2                         
000531                                   MOD-TIUPPDAT                           
000532                                   MOD-ADBETRAD-1                         
000533                                   MOD-TIBETUPH                           
000534                                   MOD-ADBETRAD-2                         
000535                                   MOD-IDUSER                             
000536                                   MOD-BELAND-SVE                         
000537                                   MOD-IDLANDX2                           
000538                                   MOD-KDVALISO                           
000539                                   MOD-KDVALTYP                           
000540                                   MOD-KDTRADP                            
000541                                   MOD-KDKREDSP                           
000542                                   MOD-IDVAT                              
000543                                   MOD-KDBETVIL                           
000544                                   MOD-BEBETVIL                           
000545                                   MOD-RELANDCO                           
000546                                   MOD-KDVALIS2                           
000547                                   MOD-IDPROMR                            
000548                                   MOD-RESERAVG-IN                        
000549                                   MOD-FLDIRVAT-IN                        
000550                                   MOD-FLINVGRP-IN                        
000551                                   MOD-IDLEVNR-FIN-IN                     
000552     .                                                                    
000553     EJECT                                                                
000554* --- IMS SEKTIONER ---                                                   
000555                                                                          
000556 IMS-GET-MSG SECTION.                                                     
000557                                                                          
000558     MOVE '  QC' TO GODK-STATUSKODER                                      
000559     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
000560     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
000561     PERFORM IMS-STATUSKONTROLL                                           
000562     .                                                                    
000563     SKIP3                                                                
000564 IMS-INSERT-MSG SECTION.                                                  
000565                                                                          
000566     IF MSGI-IDLAND-SPR = 'SE'                                            
000567       MOVE '0' TO MFS-KDHUVOMR                                           
000568     END-IF                                                               
000569     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
000570     MOVE SPACE TO GODK-STATUSKODER                                       
000571     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
000572     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
000573     PERFORM IMS-STATUSKONTROLL                                           
000574     .                                                                    
000575     EJECT                                                                
000576 IMS-GET-WDB101 SECTION.                                                  
000577                                                                          
000578     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
000579          DELIMITED BY SIZE INTO SSA1                                     
000580     MOVE '  GE' TO GODK-STATUSKODER                                      
000581     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
000582     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
000583     PERFORM IMS-STATUSKONTROLL                                           
000584     .                                                                    
000585     EJECT                                                                
000586 IMS-GU-WDB101 SECTION.                                                   
000587     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
000588          DELIMITED BY SIZE INTO SSA1                                     
000589     MOVE '  GE' TO GODK-STATUSKODER                                      
000590     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
000591     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
000592     PERFORM IMS-STATUSKONTROLL                                           
000593     .                                                                    
000594     EJECT                                                                
000595 IMS-GHU-WDB101 SECTION.                                                  
000596     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
000597          DELIMITED BY SIZE INTO SSA1                                     
000598     MOVE '  GE' TO GODK-STATUSKODER                                      
000599     CALL CBLTDLI USING GHU WDB1-PCB DLI-IO-WDB101 SSA1                   
000600     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
000601     PERFORM IMS-STATUSKONTROLL                                           
000602     .                                                                    
000603     EJECT                                                                
000604 IMS-REPL-WDB101 SECTION.                                                 
000605                                                                          
000606     MOVE '  ' TO GODK-STATUSKODER                                        
000607     CALL CBLTDLI USING REPL WDB1-PCB DLI-IO-WDB101                       
000608     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
000609     PERFORM IMS-STATUSKONTROLL                                           
000610     .                                                                    
000611 IMS-STATUSKONTROLL SECTION.                                              
000612                                                                          
000613     SET STATUS-IX TO 1                                                   
000614     SEARCH GODK-STATUS                                                   
000615       AT END                                                             
000616         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
000617         DELIMITED BY SIZE INTO FELTEXT                                   
000618         CALL FELLOG                                                      
000619       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
000620         CONTINUE                                                         
000621     END-SEARCH                                                           
000630     .                                                                    
