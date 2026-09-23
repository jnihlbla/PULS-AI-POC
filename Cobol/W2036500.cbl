000001 PROCESS DYNAM                                                            
000002 ID DIVISION.                                                             
000003 PROGRAM-ID.     W2036500.                                                
000004 AUTHOR.         NIHLBLAD JOHAN.                                          
000005 DATE-WRITTEN.   06/01/31.                                                
000006 DATE-COMPILED.                                                           
000007                                                                          
000008                                                                          
000009*    FUNKTION:                                                            
000010*        GRUPPERINGSBILD FÖR VISNING PÅ 2382                              
000011*                                                                         
000012*        PROGRAMMET LÄSER             WDB6                                
000013*        PROGRAMMET UPPDATERAR TABELL TP5IDDC                             
000014*                                                                         
000015                                                                          
000016     SKIP3                                                                
000017 ENVIRONMENT DIVISION.                                                    
000018     SKIP2                                                                
000019 DATA DIVISION.                                                           
000020     SKIP3                                                                
000021 WORKING-STORAGE SECTION.                                                 
000022                                                                          
000023 77  IDPGM                       PIC X(8)    VALUE 'W2036500'.            
000024 77  JA                          PIC X       VALUE 'J'.                   
000025 77  NEJ                         PIC X       VALUE 'N'.                   
000026 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
000027 77  RAD-IX                      PIC S9(3)  VALUE ZERO  COMP-3.           
000028 77  RAD-IX-MAX                  PIC S9(3)  VALUE +6   COMP-3.            
000029                                                                          
000030 77  INDATA-SW                   PIC X       VALUE 'J'.                   
000031     88  INDATA-OK                           VALUE 'J'.                   
000032     88  INDATA-FEL                          VALUE 'N'.                   
000033                                                                          
000034 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
000035     88  NYCKLAR-OK                          VALUE 'J'.                   
000036     88  NYCKLAR-FEL                         VALUE 'N'.                   
000037                                                                          
000038 77  UPD-RAD-SW                      PIC X       VALUE 'N'.               
000039     88  UPD-RAD-JA                              VALUE 'J'.               
000040     88  UPD-RAD-NEJ                             VALUE 'N'.               
000041                                                                          
000042 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
000043     88  EGEN-MID                            VALUE '2365'.                
000044     88  GODK-MID                            VALUE '2365'.                
000045     88  HELP-MID                            VALUE '0551'.                
000046                                                                          
000047     SKIP2                                                                
000048 01  FELTEXT.                                                             
000049     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
000050     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
000051     EJECT                                                                
000052 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
000053 01  FILLER REDEFINES DAGENS-DATUM.                                       
000054     03  DAGENS-DATUM-AAR        PIC 9(2).                                
000055     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
000056     03  DAGENS-DATUM-DAG        PIC 9(2).                                
000057     EJECT                                                                
000058 01  WS-IDDC                     PIC X(2)   VALUE SPACE.                  
000059 01  WS-IDLOPNR-DC               PIC S9(7)  VALUE ZERO  COMP-3.           
000060 01  WS-IX                       PIC 9(2)    VALUE ZERO.                  
000061 01  WS-ANTAL-DB2                PIC S9(5)    VALUE ZERO COMP-3.          
000062*                                                                         
000063 01  WS.                                                                  
000064*********************************************************                 
000065*    WS-MSGI-AREA-2365                                                    
000066*           ANVÄNDS FÖR ATT SPARA PÅ NYCKELDATABASEN WDP7                 
000067*           (I MSGI-SPAR-AREA)                                            
000068*********************************************************                 
000069  05 WS-MSGI-AREA-2365.                                                   
000070    10 WS-MSGI-IDTRANS-2365      PIC X(4)    VALUE '2365'.                
000071    10 WS-MSGI-SCREEN-GRP OCCURS 11.                                      
000072      15  WS-MSGI-IDDC           PIC X(2)    VALUE SPACE.                 
000073    10 FILLER                    PIC X(907)  VALUE SPACE.                 
000074                                                                          
000075 01  SPAR-AREA.                                                           
000076     03  SPAR-IDTRANS           PIC X(4)    VALUE '2365'.                 
000077     EJECT                                                                
000078*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
000079 01  GENERELLA-SUBPROGRAM.                                                
000080     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
000081     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
000082     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000083     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000084     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
000085*    --- PARAMETRAR TILL ABEND                                            
000086                                                                          
000087 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
000088 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
000089     SKIP2                                                                
000090*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
000091*01 -COPY WMSGINIT                                                        
000092     EJECT                                                                
000093*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
000094*01 -COPY WMEDAREA                                                        
000095     SKIP3                                                                
000096 01  MESSAGE-CODES.                                                       
000097     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
000098     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
000099     03  INF-URVAL-SAKNAS        PIC X(3)    VALUE '005'.                 
000100     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
000101     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
000102     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
000103     03  INF-SISTA-SIDAN         PIC X(3)    VALUE '115'.                 
000104     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
000105     03  UPDATING-NOT-ALLOWED    PIC X(3)    VALUE '777'.                 
000106     SKIP3                                                                
000107*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
000108*                                                                         
000109 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
000110     SKIP3                                                                
000111*01  MID -COPY W2I36501                                                   
000112     EJECT                                                                
000113 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
000114     SKIP3                                                                
000115*01  -COPY WMSGAREA                                                       
000116     EJECT                                                                
000117     03  MOD REDEFINES MSG-AREA.                                          
000118*      05  -COPY W2O36501                                                 
000119     EJECT                                                                
000120*                                                                         
000121 01  FILLER                      PIC X(16)   VALUE 'MSG-KOM-AREA'.        
000122*01  -COPY WMSGKOM                                                        
000123     EJECT                                                                
000124 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
000125     SKIP3                                                                
000126*01  -COPY WMFSAREA                                                       
000127     EJECT                                                                
000128 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000129     SKIP3                                                                
000130 01  NYCKLAR-TILL-DLI.                                                    
000131     03  W-IDDC-B6-X.                                                     
000132         05  W-IDDC-B6           PIC X(2)   VALUE SPACE.                  
000133     03  W-IDDC-X.                                                        
000134         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
000135     SKIP2                                                                
000136*    --- STATUS-KOD FRÅN IMS                                              
000137 01  STATUS-WS                   PIC XX.                                  
000138     88  SEGMENT-FINNS                       VALUE '  '.                  
000139     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
000140     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000141     88  SEGMENT-SLUT                        VALUE 'GB'.                  
000142     88  IMS-EJ-OK                           VALUE 'XD'.                  
000143     SKIP2                                                                
000144 01  GODK-STATUSKODER.                                                    
000145     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000146     SKIP3                                                                
000147 01  SSA1                        PIC X(64).                               
000148 01  SSA2                        PIC X(64).                               
000149     EJECT                                                                
000150*    --- IMS FUNKTIONSKODER                                               
000151*01  -COPY W0003                                                          
000152     EJECT                                                                
000153 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
000154 01  DLI-IO-WDB601.                                                       
000155*    03  -COPY WDB601                                                     
000156     EJECT                                                                
000157     EJECT                                                                
000158 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
000159       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
000160                                                                          
000161 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
000162 01  DB2-WS.                                                              
000163     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
000164         88  CURSOR-OK                       VALUE 000.                   
000165         88  RADER-FINNS                     VALUE 000.                   
000166         88  RADER-SAKNAS                    VALUE 100.                   
000167         88  ATKOMST-FEL                     VALUE 904.                   
000168     03  GODK-SQLCODEKODER.                                               
000169         05  GODK-SQLCODE OCCURS 5                                        
000170             INDEXED BY SQLCODE-IX PIC 9(3).                              
000171*    ---  DLI INPUT-OUTPUT AREA                                           
000172                                                                          
000173     EJECT                                                                
000174 01  FILLER                      PIC X(16)  VALUE 'TP5IDDC-AREA'.         
000175                                                                          
000176*01  -COPY TP5IDDC -PRE TP5IDDC-                                          
000177     EJECT                                                                
000178     EXEC SQL INCLUDE TP5IDDC END-EXEC.                                   
000179 LINKAGE SECTION.                                                         
000180*01  -COPY W0009   -PRE MSG-                                              
000181*01  -COPY W0008   -PRE WDP7-                                             
000182     05  FILLER                  PIC X.                                   
000183                                                                          
000184*01  -COPY W0008  -PRE WDB6-                                              
000185     05  FILLER                  PIC X.                                   
000186                                                                          
000187     EJECT                                                                
000188 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDB6-PCB.                     
000189 MAIN SECTION.                                                            
000190     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDB6-PCB.                     
000191                                                                          
000192     SKIP2                                                                
000193     PERFORM IMS-GET-MSG                                                  
000194     IF SEGMENT-FINNS                                                     
000195       PERFORM A-INIT                                                     
000196       PERFORM B-KOLLA-NYCKLAR                                            
000197       IF NYCKLAR-OK                                                      
000198         IF MFS-UPDATE                                                    
000199           PERFORM G-KOLLA-INPUT                                          
000200           IF INDATA-OK                                                   
000201             PERFORM H-UPPDATERA                                          
000202           END-IF                                                         
000203         ELSE                                                             
000204           IF MFS-FIRST                                                   
000205             PERFORM C-FOERSTA-SIDA                                       
000206           ELSE                                                           
000207             PERFORM E-SAMMA-SIDA                                         
000208           END-IF                                                         
000209         END-IF                                                           
000210         IF INDATA-OK                                                     
000211            PERFORM F-LAES-VISA-INFO                                      
000212         END-IF                                                           
000213         MOVE '002'             TO MSGI-KDCALL                            
000214         MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                      
000215         MOVE MSG-SIGNON-USERID                                           
000216                              TO MSGI-IDUSER                              
000217         MOVE WS-MSGI-AREA-2365                                           
000218                              TO MSGI-SPAR-AREA                           
000219         CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                       
000220                                                                          
000221       END-IF                                                             
000222       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O36501 + 4                      
000223       PERFORM IMS-INSERT-MSG                                             
000224     END-IF                                                               
000225                                                                          
000226     MOVE ZERO TO RETURN-CODE                                             
000227     GOBACK                                                               
000228     .                                                                    
000229     EJECT                                                                
000230 A-INIT SECTION.                                                          
000231                                                                          
000232     IF MSG-DUBBLA-TRANSKODER                                             
000233       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I36501                 
000234       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
000235       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
000236     ELSE                                                                 
000237       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I36501                  
000238       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
000239       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
000240     END-IF                                                               
000241                                                                          
000242     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
000243     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
000244     MOVE MFS-IDTRANS TO W-IDTRANS                                        
000245                                                                          
000246     MOVE LOW-VALUE TO MSG-AREA                                           
000247     MOVE 'W2O365N1' TO MFS-IDMOD                                         
000248     MOVE '2365' TO MOD-IDTRANS                                           
000249     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
000250     IF EGEN-MID OR HELP-MID                                              
000251       CONTINUE                                                           
000252     ELSE                                                                 
000253       MOVE SPACE TO MFS-KDTRTYP                                          
000254       MOVE '7' TO MFS-IDPFK                                              
000255     END-IF                                                               
000256     .                                                                    
000257     EJECT                                                                
000258                                                                          
000259     INITIALIZE GODK-SQLCODEKODER                                         
000260     .                                                                    
000261     EJECT                                                                
000262 B-KOLLA-NYCKLAR SECTION.                                                 
000263                                                                          
000264     MOVE ALL '+'           TO MSGI-WMSGINIT                              
000265     MOVE '001'             TO MSGI-KDCALL                                
000266     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
000267     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
000268     MOVE '2365'            TO MSGI-IDTRANS                               
000269     IF EGEN-MID                                                          
000270       MOVE MID-IDDC-IN     TO MSGI-IDDC-KEY                              
000271     END-IF                                                               
000272     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
000273     IF EGEN-MID                                                          
000274     AND MSGI-SPAR-AREA(1:4) = '2365'                                     
000275       MOVE MSGI-SPAR-AREA   TO WS-MSGI-AREA-2365                         
000276     END-IF                                                               
000277                                                                          
000278*    - SPRÅK SOM SKA ANVÄNDAS AV WMEDKONV                                 
000279     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
000280                                                                          
000281     MOVE JA TO NYCKLAR-SW                                                
000282                                                                          
000283                                                                          
000284*    -- KONTROLL AV IDDC                                                  
000285     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
000286                                                                          
000287     IF MID-IDDC-IN NOT = ALL '+'                                         
000288       MOVE '7'         TO MFS-IDPFK                                      
000289       MOVE SPACE       TO MFS-KDTRTYP                                    
000290     END-IF                                                               
000291                                                                          
000292     MOVE MSGI-IDDC-KEY TO W-IDDC-B6                                      
000293                           W-IDDC                                         
000294     PERFORM IMS-GU-WDB601                                                
000295                                                                          
000296     IF SEGMENT-FINNS                                                     
000297     AND (DCS-SDC                                                         
000298     OR   DCS-NDC)                                                        
000305       CONTINUE                                                           
000306     ELSE                                                                 
000307       MOVE NEJ              TO NYCKLAR-SW                                
000308     END-IF                                                               
000309                                                                          
000310     MOVE W-IDDC          TO MOD-IDDC-UT                                  
000311     IF NYCKLAR-FEL                                                       
000312       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
000313       CALL WMEDKONV USING MED-WMEDAREA                                   
000314       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
000315       PERFORM MFS-RENSA-FAELT-IN                                         
000316     END-IF                                                               
000317     .                                                                    
000318     EJECT                                                                
000319 C-FOERSTA-SIDA SECTION.                                                  
000320                                                                          
000321     PERFORM MFS-RENSA-FAELT-IN                                           
000322     .                                                                    
000323     EJECT                                                                
000324 E-SAMMA-SIDA SECTION.                                                    
000325                                                                          
000326     IF SPAR-IDTRANS = '2365' OR '0551'                                   
000327       IF MID-INPUT = ALL '+'                                             
000328         PERFORM MFS-RENSA-FAELT-IN                                       
000329       ELSE                                                               
000330         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
000331         CALL WMEDKONV USING MED-WMEDAREA                                 
000332         MOVE MED-MFSINF TO MOD-TEMFSFEL                                  
000333         PERFORM EA-MID-INDATA-TILL-MOD                                   
000334       END-IF                                                             
000335     ELSE                                                                 
000336       PERFORM MFS-RENSA-FAELT-IN                                         
000337     END-IF                                                               
000338     .                                                                    
000339     EJECT                                                                
000340 EA-MID-INDATA-TILL-MOD SECTION.                                          
000341                                                                          
000342     IF MID-NY-IDDC NOT = ALL '+'                                         
000343         MOVE MID-NY-IDDC TO MOD-NY-IDDC                                  
000344         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-NY-IDDC-ATTR                  
000345     ELSE                                                                 
000346         MOVE MFS-RENSA-FAELT TO MOD-NY-IDDC                              
000347     END-IF                                                               
000348                                                                          
000349     MOVE +1 TO RAD-IX                                                    
000350     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
000351       IF MID-CMD(RAD-IX) NOT = ALL '+'                                   
000352         MOVE MID-CMD(RAD-IX)   TO MOD-CMD(RAD-IX)                        
000353         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-CMD-ATTR(RAD-IX)               
000354       ELSE                                                               
000355         MOVE MFS-RENSA-FAELT   TO MOD-CMD(RAD-IX)                        
000356       END-IF                                                             
000357       ADD +1 TO RAD-IX                                                   
000358     END-PERFORM                                                          
000359                                                                          
000360     .                                                                    
000361     EJECT                                                                
000362 F-LAES-VISA-INFO SECTION.                                                
000363                                                                          
000364     PERFORM DB2-DCL-OPN-TP5IDDC-CRS                                      
000365     PERFORM DB2-FETCH-TP5IDDC-CRS                                        
000366     MOVE +1 TO RAD-IX                                                    
000367     PERFORM UNTIL RAD-IX > RAD-IX-MAX OR RADER-SAKNAS                    
000368       MOVE TP5IDDC-IDDC       TO MOD-IDDC(RAD-IX)                        
000369                                  WS-MSGI-IDDC(RAD-IX)                    
000370       MOVE TP5IDDC-IDLOPNR-DC TO WS-IDLOPNR-DC                           
000371       PERFORM DB2-FETCH-TP5IDDC-CRS                                      
000372       ADD +1 TO RAD-IX                                                   
000373     END-PERFORM                                                          
000374     PERFORM DB2-CLOSE-TP5IDDC-CRS                                        
000375                                                                          
000376     PERFORM UNTIL RAD-IX > 11                                            
000377         MOVE MFS-STAENG-FAELT TO MOD-CMD-ATTR(RAD-IX)                    
000378         MOVE MFS-RENSA-FAELT  TO MOD-CMD(RAD-IX)                         
000379         MOVE SPACE            TO WS-MSGI-IDDC(RAD-IX)                    
000380       ADD +1 TO RAD-IX                                                   
000381     END-PERFORM                                                          
000382     .                                                                    
000383     EJECT                                                                
000384 G-KOLLA-INPUT SECTION.                                                   
000385                                                                          
000386     IF MID-INPUT      = ALL '+'                                          
000387       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
000388       MOVE NEJ TO INDATA-SW                                              
000389     ELSE                                                                 
000390       MOVE +1              TO RAD-IX                                     
000391       PERFORM UNTIL RAD-IX > RAD-IX-MAX                                  
000392         IF MID-CMD (RAD-IX) NOT = ALL '+'                                
000393           IF (MID-CMD (RAD-IX) = 'B'                                     
000394           OR MID-CMD (RAD-IX) = 'D')                                     
000395           AND MID-NY-IDDC = ALL '+'                                      
000396             MOVE MFS-ALFA-FAELT-RAETT TO MOD-CMD-ATTR(RAD-IX)            
000397             MOVE JA TO UPD-RAD-SW                                        
000398           ELSE                                                           
000399             MOVE MFS-ALFA-FAELT-FEL    TO MOD-CMD-ATTR(RAD-IX)           
000400             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
000401             MOVE NEJ TO INDATA-SW                                        
000402           END-IF                                                         
000403         END-IF                                                           
000404         ADD +1 TO RAD-IX                                                 
000405       END-PERFORM                                                        
000406                                                                          
000407       IF MID-NY-IDDC NOT = ALL '+'                                       
000408       AND UPD-RAD-NEJ                                                    
000409         IF WS-MSGI-IDDC(6) NOT = SPACE                                   
000410           MOVE NEJ TO INDATA-SW                                          
000411           MOVE 'ONLY 6 DC IN EACH GROUP' TO MOD-TEMFSINF                 
000412         ELSE                                                             
000413           MOVE MID-NY-IDDC     TO W-IDDC-B6                              
000414                                     WS-IDDC                              
000415           PERFORM IMS-GU-WDB601                                          
000416                                                                          
000417           IF SEGMENT-FINNS                                               
000418           AND (DCS-SDC                                                   
000419           OR   DCS-NDC)                                                  
000426             PERFORM DB2-SELECT-TP5IDDC-TAB                               
000427             IF SQLCODE = ZERO                                            
000428                MOVE MFS-ALFA-FAELT-FEL TO MOD-NY-IDDC-ATTR               
000429                MOVE 'DC ALREADY EXISTS IN OTHER GROUP'                   
000430                     TO MOD-TEMFSINF                                      
000431                MOVE NEJ     TO INDATA-SW                                 
000432             ELSE                                                         
000433                MOVE MOD-IDDC-UT TO W-IDDC                                
000434                PERFORM DB2-SELECT-TP5IDDC-TAB2                           
000435                IF SQLCODE > ZERO                                         
000436                   MOVE ZERO             TO WS-IDLOPNR-DC                 
000437                ELSE                                                      
000438                   MOVE TP5IDDC-IDLOPNR-DC TO WS-IDLOPNR-DC               
000439                END-IF                                                    
000440             END-IF                                                       
000441           ELSE                                                           
000442             IF NOT DCS-CDC                                               
000443               MOVE 'DC IS NOT VALID' TO MOD-TEMFSINF                     
000444             END-IF                                                       
000445             MOVE MFS-ALFA-FAELT-FEL  TO MOD-NY-IDDC-ATTR                 
000446             MOVE NEJ        TO INDATA-SW                                 
000447           END-IF                                                         
000448         END-IF                                                           
000449       END-IF                                                             
000450     END-IF                                                               
000451     IF INDATA-FEL                                                        
000452       IF DCS-CDC                                                         
000453         MOVE ERR-WRONG-KEY           TO MED-IDMFSFEL                     
000454       ELSE                                                               
000455         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
000456       END-IF                                                             
000457       CALL WMEDKONV USING MED-WMEDAREA                                   
000458       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
000459       PERFORM MFS-ROER-EJ-FAELT-UT                                       
000460       PERFORM MFS-ROER-EJ-FAELT-IN                                       
000461       PERFORM MFS-LAES-IN-IGEN                                           
000462     END-IF                                                               
000463     .                                                                    
000464     EJECT                                                                
000465 H-UPPDATERA SECTION.                                                     
000466                                                                          
000467     MOVE NEJ TO UPD-RAD-SW                                               
000468     IF MID-NY-IDDC = ALL '+'                                             
000469       MOVE +1 TO RAD-IX                                                  
000470       PERFORM UNTIL RAD-IX > RAD-IX-MAX                                  
000471         IF MID-CMD            (RAD-IX) NOT = ALL '+'                     
000472            MOVE WS-MSGI-IDDC(RAD-IX) TO WS-IDDC                          
000473            PERFORM HA-DELETA-RAD                                         
000474            MOVE JA              TO UPD-RAD-SW                            
000475         END-IF                                                           
000476         ADD 1 TO RAD-IX                                                  
000477       END-PERFORM                                                        
000478     END-IF                                                               
000479     IF UPD-RAD-NEJ                                                       
000480     AND MID-NY-IDDC NOT = ALL '+'                                        
000481       PERFORM HB-UPD-NY-DC                                               
000482     END-IF                                                               
000483     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
000484     CALL WMEDKONV USING MED-WMEDAREA                                     
000485     MOVE MED-TEMFSINF TO MOD-TEMFSINF                                    
000486     PERFORM MFS-RENSA-FAELT-IN                                           
000487                                                                          
000488     .                                                                    
000489     EJECT                                                                
000490                                                                          
000491 HA-DELETA-RAD SECTION.                                                   
000492                                                                          
000493     PERFORM DB2-DELETE-TP5IDDC-TAB                                       
000494     ADD -1 TO WS-IX                                                      
000495     .                                                                    
000496     EJECT                                                                
000497                                                                          
000498 HB-UPD-NY-DC SECTION.                                                    
000499                                                                          
000500     MOVE MID-NY-IDDC       TO WS-IDDC                                    
000501     PERFORM DB2-COUNT-TP5IDDC                                            
000502     IF WS-ANTAL-DB2 = ZERO                                               
000503       MOVE 1     TO WS-IDLOPNR-DC                                        
000504     ELSE                                                                 
000505       IF WS-IDLOPNR-DC = ZERO                                            
000506         PERFORM DB2-SELECT-TP5IDDC-MAX                                   
000507         COMPUTE WS-IDLOPNR-DC = TP5IDDC-IDLOPNR-DC + 1                   
000508       END-IF                                                             
000509     END-IF                                                               
000510     PERFORM DB2-INSERT-TP5IDDC-TAB                                       
000511     .                                                                    
000512     EJECT                                                                
000513                                                                          
000514 MFS-RENSA-FAELT-IN SECTION.                                              
000515                                                                          
000516*    --- ALLA INDATA-FÄLT                                                 
000517     MOVE +1 TO RAD-IX                                                    
000518     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
000519       MOVE MFS-RENSA-FAELT  TO MOD-CMD (RAD-IX)                          
000520       ADD +1 TO RAD-IX                                                   
000521     END-PERFORM                                                          
000522     MOVE MFS-RENSA-FAELT TO MOD-NY-IDDC                                  
000523     .                                                                    
000524     EJECT                                                                
000525 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
000526                                                                          
000527*    --- ALLA INDATA-FÄLT                                                 
000528     MOVE +1 TO RAD-IX                                                    
000529     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
000530       MOVE MFS-ROER-EJ-FAELT       TO MOD-IDDC (RAD-IX)                  
000531       ADD +1 TO RAD-IX                                                   
000532     END-PERFORM                                                          
000533*    MOVE MFS-ROER-EJ-FAELT TO MOD-TEMFSINF                               
000534     .                                                                    
000535     EJECT                                                                
000536 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
000537                                                                          
000538*    --- ALLA INDATA-FÄLT                                                 
000539     MOVE +1 TO RAD-IX                                                    
000540     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
000541       MOVE MFS-ROER-EJ-FAELT       TO MOD-CMD (RAD-IX)                   
000542       ADD +1 TO RAD-IX                                                   
000543     END-PERFORM                                                          
000544     MOVE MFS-ROER-EJ-FAELT TO MOD-NY-IDDC                                
000545     .                                                                    
000546     EJECT                                                                
000547 MFS-LAES-IN-IGEN SECTION.                                                
000548                                                                          
000549*    --- ALLA INDATA-FÄLT                                                 
000550     MOVE +1 TO RAD-IX                                                    
000551     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
000552       MOVE MFS-ADD-LAES-IN-FAELT TO     MOD-CMD-ATTR (RAD-IX)            
000553       ADD +1 TO RAD-IX                                                   
000554     END-PERFORM                                                          
000555     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-NY-IDDC-ATTR                       
000556* --- IMS SEKTIONER ---                                                   
000557                                                                          
000558     EJECT                                                                
000559     .                                                                    
000560 DB2-COUNT-TP5IDDC  SECTION.                                              
000561                                                                          
000562     MOVE 000100  TO GODK-SQLCODEKODER                                    
000563     EXEC SQL                                                             
000564         SELECT  COUNT (*)                                                
000565                                                                          
000566         INTO   :WS-ANTAL-DB2                                             
000567         FROM    TP5IDDC                                                  
000568     END-EXEC                                                             
000569                                                                          
000570     MOVE SQLCODE TO SQLCODE-WS                                           
000571     PERFORM DB2-STATUS-KONTROLL                                          
000572     .                                                                    
000573     EJECT                                                                
000574 DB2-SELECT-TP5IDDC-TAB  SECTION.                                         
000575                                                                          
000576     MOVE 000100  TO GODK-SQLCODEKODER                                    
000577     EXEC SQL                                                             
000578         SELECT  IDDC                                                     
000579                                                                          
000580         INTO   :TP5IDDC-IDDC                                             
000581                                                                          
000582         FROM    TP5IDDC                                                  
000583         WHERE   IDDC = :WS-IDDC                                          
000584     END-EXEC                                                             
000585                                                                          
000586     MOVE SQLCODE TO SQLCODE-WS                                           
000587     PERFORM DB2-STATUS-KONTROLL                                          
000588     .                                                                    
000589     EJECT                                                                
000590 DB2-SELECT-TP5IDDC-TAB2 SECTION.                                         
000591                                                                          
000592     MOVE 000100  TO GODK-SQLCODEKODER                                    
000593     EXEC SQL                                                             
000594         SELECT  IDDC, IDLOPNR_DC                                         
000595                                                                          
000596         INTO   :TP5IDDC-IDDC                                             
000597               ,:TP5IDDC-IDLOPNR-DC                                       
000598                                                                          
000599         FROM    TP5IDDC                                                  
000600         WHERE   IDDC = :W-IDDC                                           
000601     END-EXEC                                                             
000602                                                                          
000603     MOVE SQLCODE TO SQLCODE-WS                                           
000604     PERFORM DB2-STATUS-KONTROLL                                          
000605     .                                                                    
000606     EJECT                                                                
000607 DB2-SELECT-TP5IDDC-MAX  SECTION.                                         
000608                                                                          
000609     MOVE 000100  TO GODK-SQLCODEKODER                                    
000610     EXEC SQL                                                             
000611         SELECT  MAX(IDLOPNR_DC)                                          
000612                                                                          
000613         INTO   :TP5IDDC-IDLOPNR-DC                                       
000614                                                                          
000615         FROM    TP5IDDC                                                  
000616     END-EXEC                                                             
000617                                                                          
000618     MOVE SQLCODE TO SQLCODE-WS                                           
000619     PERFORM DB2-STATUS-KONTROLL                                          
000620     .                                                                    
000621     EJECT                                                                
000622 DB2-DCL-OPN-TP5IDDC-CRS  SECTION.                                        
000623                                                                          
000624     MOVE 000100  TO GODK-SQLCODEKODER                                    
000625                                                                          
000626     EXEC SQL                                                             
000627         DECLARE TP5IDDC-CRS CURSOR FOR                                   
000628                                                                          
000629           SELECT  IDDC, IDLOPNR_DC                                       
000630                                                                          
000631           FROM    TP5IDDC                                                
000632           WHERE   IDLOPNR_DC = (SELECT IDLOPNR_DC                        
000633                                 FROM TP5IDDC                             
000634                                 WHERE IDDC = :W-IDDC)                    
000635           ORDER BY IDDC                                                  
000636     END-EXEC                                                             
000637                                                                          
000638     MOVE 000100  TO GODK-SQLCODEKODER                                    
000639     EXEC SQL OPEN TP5IDDC-CRS END-EXEC                                   
000640                                                                          
000641     .                                                                    
000642     SKIP3                                                                
000643 DB2-FETCH-TP5IDDC-CRS  SECTION.                                          
000644     SKIP2                                                                
000645     MOVE 000100  TO GODK-SQLCODEKODER                                    
000646     EXEC SQL                                                             
000647         FETCH TP5IDDC-CRS INTO :TP5IDDC-IDDC                             
000648                               ,:TP5IDDC-IDLOPNR-DC                       
000649     END-EXEC                                                             
000650                                                                          
000651     MOVE SQLCODE TO SQLCODE-WS                                           
000652     PERFORM DB2-STATUS-KONTROLL                                          
000653     .                                                                    
000654     SKIP3                                                                
000655 DB2-CLOSE-TP5IDDC-CRS  SECTION.                                          
000656                                                                          
000657     EXEC SQL CLOSE TP5IDDC-CRS END-EXEC                                  
000658     .                                                                    
000659     EJECT                                                                
000660 DB2-DELETE-TP5IDDC-TAB  SECTION.                                         
000661                                                                          
000662     MOVE 000     TO GODK-SQLCODEKODER                                    
000663     EXEC SQL                                                             
000664         DELETE FROM TP5IDDC                                              
000665         WHERE   IDDC = :WS-IDDC                                          
000666     END-EXEC                                                             
000667                                                                          
000668     MOVE SQLCODE TO SQLCODE-WS                                           
000669     PERFORM DB2-STATUS-KONTROLL                                          
000670     .                                                                    
000671     EJECT                                                                
000672 DB2-INSERT-TP5IDDC-TAB  SECTION.                                         
000673     SKIP2                                                                
000674     MOVE 000   TO GODK-SQLCODEKODER                                      
000675     EXEC SQL                                                             
000676         INSERT INTO TP5IDDC                                              
000677         (IDLOPNR_DC,IDDC)                                                
000678         VALUES(:WS-IDLOPNR-DC                                            
000679               ,:WS-IDDC)                                                 
000680     END-EXEC                                                             
000681                                                                          
000682     MOVE SQLCODE TO SQLCODE-WS                                           
000683     PERFORM DB2-STATUS-KONTROLL                                          
000684     .                                                                    
000685     EJECT                                                                
000686 DB2-STATUS-KONTROLL  SECTION.                                            
000687                                                                          
000688     SET SQLCODE-IX TO 1                                                  
000689     SEARCH GODK-SQLCODE                                                  
000690       AT END                                                             
000691          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
000692          DELIMITED BY SIZE INTO FELTEXT                                  
000693          CALL ABEND USING RKOD-ABEND-DB2                                 
000694       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
000695     END-SEARCH                                                           
000696     .                                                                    
000697 IMS-GET-MSG SECTION.                                                     
000698                                                                          
000699     MOVE '  QC' TO GODK-STATUSKODER                                      
000700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
000701     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
000702     PERFORM IMS-STATUSKONTROLL                                           
000703     .                                                                    
000704     SKIP3                                                                
000705 IMS-INSERT-MSG SECTION.                                                  
000706                                                                          
000707     MOVE 'N' TO MFS-KDHUVOMR                                             
000708     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
000709     MOVE SPACE TO GODK-STATUSKODER                                       
000710     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
000711     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
000712     PERFORM IMS-STATUSKONTROLL                                           
000713     .                                                                    
000714     EJECT                                                                
000715 IMS-GU-WDB601    SECTION.                                                
000716     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
000717          DELIMITED BY SIZE INTO SSA1                                     
000718     MOVE '  GE' TO GODK-STATUSKODER                                      
000719     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601  SSA1                   
000720     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
000721     PERFORM IMS-STATUSKONTROLL                                           
000722     .                                                                    
000723     EJECT                                                                
000724 IMS-STATUSKONTROLL SECTION.                                              
000725                                                                          
000726     SET STATUS-IX TO 1                                                   
000727     SEARCH GODK-STATUS                                                   
000728       AT END                                                             
000729         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
000730         DELIMITED BY SIZE INTO FELTEXT                                   
000731         CALL FELLOG                                                      
000732       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
000733         CONTINUE                                                         
000734     END-SEARCH                                                           
000735     .                                                                    
