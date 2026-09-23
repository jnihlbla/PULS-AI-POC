000001 ID DIVISION.                                                             
000002                                                                          
000003 PROGRAM-ID.     W6014800.                                                
000004 AUTHOR.         ANNELIE ENGLUND/UMESH JAIN                               
000005 DATE-WRITTEN.   94/07/12./ DEC 2011                                      
000006 DATE-COMPILED.                                                           
000007                                                                          
000008*    FUNKTION:                                                            
000009*        ALLMÄN BESKRIVNING:                                              
000010*        BILDEN ANVÄNDS FÖR ATT TITTA PÅ OCH GODKÄNNA                     
000011*        GODS SOM HAR TRANSPORTANMÄRKNING, SOM I DETTA                    
000012*        FALLET INNEBÄR ATT GODSET INTE KOMMIT TILL                       
000013*        LAGRET.                                                          
000014*                                                                         
000015*        THIS IS A DRIVER PGM FOR TRANSACTIONS W6T148, W6T148U            
000016*                                                                         
000017*        IT TAKES CARE OF ALL TECHNICAL DETAILS RELATED TO WHELP          
000018*        AND 3270 FORMATS AND CALLS SUBPROGRAM W6014810 WHICH             
000019*        CONTAINS ALL BUSINESS LOGIC FOR THESE TRANSACTIONS.              
000020*                                                                         
000021*        A SIMILAR DRIVER PROGRAM FOR INVOCATIONS FROM THE WEB            
000022*        EXISTS - W6W14800 (TRANSACTIONS W6T148, W6T148U)                 
000023*                                                                         
000024     SKIP3                                                                
000025 ENVIRONMENT DIVISION.                                                    
000026     EJECT                                                                
000027 DATA DIVISION.                                                           
000028 WORKING-STORAGE SECTION.                                                 
000029                                                                          
000030*    -- CHECKED BY WY2000                                                 
000031 77  IDPGM                       PIC X(08)   VALUE 'W6014800'.            
000032                                                                          
000033*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
000034 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
000035                                                                          
000036 77  JA                          PIC X       VALUE 'J'.                   
000037 77  NEJ                         PIC X       VALUE 'N'.                   
000038                                                                          
000039*    --- INDEX FÖR BLÄDDRINGSRADER                                        
000040 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
000041 77  MAX-INDX                    PIC S9(4)  VALUE +12   COMP SYNC.        
000042 77  MAX-REQU-INDX               PIC S9(4)  VALUE +50   COMP SYNC.        
000043 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
000044 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +628  COMP SYNC.        
000045 77  WS-IDLOPNRM                 PIC 9(8)   VALUE ZERO.                   
000046 77  WS-IDLEVNR                  PIC X(5)   VALUE SPACE.                  
000047 77  WS-IDOKOLLI                 PIC 9(9)   VALUE ZERO.                   
000048                                                                          
000049*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
000050                                                                          
000051 77  INDATA-SW                   PIC X       VALUE 'J'.                   
000052     88  INDATA-OK                           VALUE 'J'.                   
000053     88  INDATA-FEL                          VALUE 'N'.                   
000054                                                                          
000055 77  NKL-SW                      PIC X       VALUE '0'.                   
000056     88  PARTI-NKL                           VALUE '1'.                   
000057     88  LEVKLI-NKL                          VALUE '2'.                   
000058                                                                          
000059 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
000060     88  EGEN-MID                            VALUE '6148'.                
000061     88  GODK-MID                            VALUE '6148'.                
000062     88  HELP-MID                            VALUE '0551'.                
000063                                                                          
000064     EJECT                                                                
000065*      --- VALID IDDC CODES                                               
000066*                                                                         
000067*01    -COPY WWDC99                                                       
000068       EJECT                                                              
000069*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
000070 01  GENERELLA-SUBPROGRAM.                                                
000071     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
000072     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000073     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000074     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
000075     03  W6014810                PIC X(8)    VALUE 'W6014810'.            
000076     EJECT                                                                
000077*01 -COPY WMSGINIT                                                        
000078     EJECT                                                                
000079*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
000080*01 -COPY WMEDAREA                                                        
000081     SKIP3                                                                
000082 01 WS-IDMSG-ERROR         PIC X(3).                                      
000083    88 WRONG-KEY           VALUE '022'.                                   
000084    88 CORR-HILITE-FLDS    VALUE '020'.                                   
000085    88 DIVERSEKOLLI        VALUE '354'.                                   
000086    88 PF11-AND-NO-DATA    VALUE '014'.                                   
000087    88 UPDATE-NOT-ALLOWED  VALUE '007'.                                   
000088    88 NOT-IN-REG          VALUE '027'.                                   
000089    88 OTILLATEN-UPPD      VALUE '007'.                                   
000090                                                                          
000091 01 WS-IDMSG-INFO          PIC X(3).                                      
000092    88 FIRST-PAGE          VALUE '010'.                                   
000093    88 PRESS-PF11          VALUE '013'.                                   
000094    88 UPDATE-DONE         VALUE '001'.                                   
000095    88 MORE-INFO-EXISTS    VALUE '011'.                                   
000096                                                                          
000097 01  MESSAGE-CODES.                                                       
000098     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
000099     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
000100     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
000101     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
000102     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
000103     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
000104     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
000105     03  ERR-NOT-IN-REG          PIC X(3)    VALUE '010'.                 
000106     03  ERR-OTILLATEN-UPPD      PIC X(3)    VALUE '007'.                 
000107     03  ERR-DIVERSEKOLLI        PIC X(3)    VALUE '182'.                 
000108     EJECT                                                                
000109*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
000110*                                                                         
000111 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
000112     SKIP3                                                                
000113*01  MID -COPY W6I14801                                                   
000114     EJECT                                                                
000115 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
000116     SKIP3                                                                
000117*01  -COPY WMSGAREA                                                       
000118     EJECT                                                                
000119     03  MOD REDEFINES MSG-AREA.                                          
000120*      05  -COPY W6O14801                                                 
000121     EJECT                                                                
000122 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
000123     SKIP3                                                                
000124*01  -COPY WMFSAREA                                                       
000125     EJECT                                                                
000126 01  KOM-MSG-IO-AREA.                                                     
000127*03  -COPY WMSGKOM                                                        
000128     EJECT                                                                
000129 01  FILLER             PIC X(16)  VALUE 'MSG/KOM-AREA'.                  
000130     SKIP3                                                                
000131*01  -COPY WMSGSNUF     -PRE P-TO-P-                                      
000132     EJECT                                                                
000133 01      FILLER                  PIC X(24)   VALUE                        
000134                                 'MOD6191-MID-W6I19101'.                  
000135     SKIP2                                                                
000136     -COPY W6I19101 -PRE MOD6191-                                         
000137     EJECT                                                                
000138 01      FILLER                  PIC X(24)   VALUE                        
000139                                 'MOD6193-MID-W6I19301'.                  
000140     SKIP2                                                                
000141     -COPY W6I19301 -PRE MOD6193-                                         
000142     EJECT                                                                
000143******************************************************************        
000144*                                                                         
000145*                AREOR FÖR ANROP TILL W6014810                            
000146*                                                                         
000147 01  FILLER                    PIC X(16) VALUE 'REQU-AREA'.               
000148 01  REQU-AREA.                                                           
000149     03 -COPY WZ01REQU                                                    
000150     03 -COPY W60148I1                                                    
000151 77  MAX-KVRADER               PIC S9(4) COMP VALUE +12.                  
000152                                                                          
000153 01  FILLER                    PIC X(16) VALUE 'RESP-AREA'.               
000154 01  RESP-AREA.                                                           
000155     03 -COPY WZ01RESP                                                    
000156     03 -COPY W60148O1                                                    
000157                                                                          
000158******************************************************************        
000159*    --- STATUS-KOD FRÅN IMS                                              
000160 01  STATUS-WS                   PIC XX.                                  
000161     88  SEGMENT-FINNS                       VALUE '  '.                  
000162     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
000163     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000164     88  SEGMENT-SLUT                        VALUE 'GB'.                  
000165     SKIP2                                                                
000166 01  GODK-STATUSKODER.                                                    
000167     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000168     SKIP3                                                                
000169 01  SSA1                        PIC X(64).                               
000170 01  SSA2                        PIC X(64).                               
000171     EJECT                                                                
000172*    --- IMS FUNKTIONSKODER                                               
000173*01  -COPY W0003                                                          
000174     EJECT                                                                
000175 LINKAGE SECTION.                                                         
000176                                                                          
000177*01  -COPY W0009   -PRE MSG-                                              
000178 01  ALT1-PCB                 PIC X.                                      
000179 01  DISP-PCB                 PIC X.                                      
000180*01  -COPY W0008  -PRE USEA-                                              
000181     05  FILLER               PIC X.                                      
000182 01  INLBSEQ-PCB              PIC X.                                      
000183 01  INLCSEQ-PCB              PIC X.                                      
000184 01  KOMA-PCB                 PIC X.                                      
000185 01  WDD3-PCB                 PIC X.                                      
       01  WDB6-PCB                 PIC X.                                      
000186                                                                          
000187                                                                          
000188 PROCEDURE DIVISION  USING MSG-PCB                                        
000189                           ALT1-PCB                                       
000190                           DISP-PCB                                       
000191                           USEA-PCB                                       
000192                           INLBSEQ-PCB                                    
000193                           INLCSEQ-PCB                                    
000194                           KOMA-PCB                                       
000195                           WDD3-PCB                                       
                                 WDB6-PCB.                                      
000196                                                                          
000197     ENTRY 'DLITCBL' USING MSG-PCB                                        
000198                           ALT1-PCB                                       
000199                           DISP-PCB                                       
000200                           USEA-PCB                                       
000201                           INLBSEQ-PCB                                    
000202                           INLCSEQ-PCB                                    
000203                           KOMA-PCB                                       
000204                           WDD3-PCB                                       
                                 WDB6-PCB.                                      
000205                                                                          
000206     PERFORM IMS-GET-MSG                                                  
000207     IF SEGMENT-FINNS                                                     
000208       PERFORM A-INIT                                                     
000209       PERFORM B-INIT-KEYS                                                
000210       PERFORM C-INIT-REQU                                                
000211       IF MFS-UPDATE                                                      
000212         SET REQU-UPDATE TO TRUE                                          
000213         MOVE MID-IDRADNR-ENTER  TO REQU-IDRADNR-START                    
000214       ELSE                                                               
000215         IF MFS-UPD-V                                                     
000216           SET REQU-UPD-V TO TRUE                                         
000217           MOVE MID-IDRADNR-ENTER  TO REQU-IDRADNR-START                  
000218         ELSE                                                             
000219           IF MFS-FIRST                                                   
000220             SET REQU-FIRST TO TRUE                                       
000221             MOVE ZERO                 TO REQU-IDRADNR-START              
000222           ELSE                                                           
000223             IF MFS-NEXT                                                  
000224               SET REQU-NEXT  TO TRUE                                     
000225               MOVE MID-IDRADNR-NEXT   TO REQU-IDRADNR-START              
000226             ELSE                                                         
000227               SET REQU-QUERY TO TRUE                                     
000228               MOVE MID-IDRADNR-ENTER  TO REQU-IDRADNR-START              
000229             END-IF                                                       
000230           END-IF                                                         
000231         END-IF                                                           
000232       END-IF                                                             
000233       PERFORM F-BUSINESS-LOGIC-W6014810                                  
000234*                                                                         
000235       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
000236       PERFORM IMS-INSERT-MSG                                             
000237     END-IF                                                               
000238                                                                          
000239     MOVE ZERO TO RETURN-CODE                                             
000240     GOBACK                                                               
000241     .                                                                    
000242     EJECT                                                                
000243 A-INIT SECTION.                                                          
000244                                                                          
000245     IF MSG-DUBBLA-TRANSKODER                                             
000246       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I14801                 
000247       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
000248       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
000249     ELSE                                                                 
000250       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I14801                  
000251       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
000252       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
000253     END-IF                                                               
000254                                                                          
000255     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
000256     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
000257     MOVE MFS-IDTRANS TO W-IDTRANS                                        
000258                                                                          
000259     MOVE LOW-VALUE TO MSG-AREA                                           
000260     MOVE 'W6O148N1' TO MFS-IDMOD                                         
000261     MOVE '6148' TO MOD-IDTRANS                                           
000262     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
000263                                                                          
000264     IF EGEN-MID OR HELP-MID                                              
000265       CONTINUE                                                           
000266     ELSE                                                                 
000267       MOVE SPACE TO MFS-KDTRTYP                                          
000268       MOVE '7' TO MFS-IDPFK                                              
000269     END-IF                                                               
000270                                                                          
000271     PERFORM AA-INIT-NYCKLAR                                              
000272                                                                          
000273     IF MSGI-IDLAND-SPR = 'GB'                                            
000274       MOVE +2 TO SPRAK-IX                                                
000275       MOVE 'GB ' TO MED-IDSKYLT                                          
000276     ELSE                                                                 
000277       MOVE +1 TO SPRAK-IX                                                
000278       MOVE 'S  ' TO MED-IDSKYLT                                          
000279     END-IF                                                               
000280*                                                                         
000281*    VERSION CODE '101' TO DETERMINE THAT IT IS CALL FROM CLASSIC         
000282*    PROGRAM IN THE SUBPROGRAM W6014810                                   
000283     MOVE '101'                 TO REQU-IDMSGVER                          
000285     .                                                                    
000286     EJECT                                                                
000287 AA-INIT-NYCKLAR SECTION.                                                 
000288     MOVE ALL '+' TO MSGI-WMSGINIT                                        
000289     MOVE '001'   TO MSGI-KDCALL                                          
000290     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
000291     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
000292     MOVE '6148'                 TO MSGI-IDTRANS                          
000293     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
000294     .                                                                    
000295     EJECT                                                                
000296 B-INIT-KEYS     SECTION.                                                 
000297     IF EGEN-MID OR GODK-MID                                              
000298       MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                
000299       IF MID-IDDC-IN = ALL '+'                                           
000300         MOVE MSGI-IDDC   TO WS-IDDC                                      
000301                             REQU-IDDC-KEY                                
000302       ELSE                                                               
000303         MOVE MID-IDDC-IN TO WS-IDDC                                      
000304                             REQU-IDDC-KEY                                
000305       END-IF                                                             
000306                                                                          
000307*    -- KONTROLL AV IDLOPNRM                                              
000308       MOVE MFS-RENSA-FAELT TO MOD-IDLOPNRM-IN                            
000309                                                                          
000310       IF MID-IDLOPNRM-IN = ALL '+'                                       
000311         MOVE MID-IDLOPNRM-UT TO WS-IDLOPNRM                              
000312         INSPECT WS-IDLOPNRM REPLACING LEADING SPACE BY ZERO              
000313         MOVE WS-IDLOPNRM     TO REQU-IDLOPNRM-KEY                        
000314       ELSE                                                               
000315         MOVE '1' TO NKL-SW                                               
000316         MOVE MID-IDLOPNRM-IN TO WS-IDLOPNRM                              
000317                                 REQU-IDLOPNRM-KEY                        
000318         MOVE SPACE           TO REQU-IDLEVNR-KEY                         
000319         MOVE ZERO            TO REQU-IDOKOLLI-KEY                        
000320         MOVE '7'             TO MFS-IDPFK                                
000321         MOVE SPACE           TO MFS-KDTRTYP                              
000322       END-IF                                                             
000323                                                                          
000324*    -- KONTROLL AV IDLEVNR                                               
000325       MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-KOLLI-IN                       
000326                               MOD-IDOKOLLI-IN                            
000327                                                                          
000328       IF NOT PARTI-NKL                                                   
000329         IF MID-IDLEVNR-IN = ALL '+'                                      
000330           MOVE MID-IDLEVNR-UT TO WS-IDLEVNR                              
000331                                  REQU-IDLEVNR-KEY                        
000332         ELSE                                                             
000333           MOVE '2' TO NKL-SW                                             
000334           MOVE MID-IDLEVNR-IN TO WS-IDLEVNR                              
000335                                  REQU-IDLEVNR-KEY                        
000336           MOVE '7'            TO MFS-IDPFK                               
000337           MOVE SPACE          TO MFS-KDTRTYP                             
000338         END-IF                                                           
000339                                                                          
000340*    -- KONTROLL AV IDOKOLLI                                              
000341         IF MID-IDOKOLLI-IN = ALL '+'                                     
000342           MOVE MID-IDOKOLLI-UT TO WS-IDOKOLLI                            
000343           INSPECT WS-IDOKOLLI REPLACING LEADING SPACE BY ZERO            
000344           MOVE WS-IDOKOLLI     TO REQU-IDOKOLLI-KEY                      
000345         ELSE                                                             
000346           MOVE '2' TO NKL-SW                                             
000347           MOVE MID-IDOKOLLI-IN TO WS-IDOKOLLI                            
000348                                   REQU-IDOKOLLI-KEY                      
000349           MOVE '7'             TO MFS-IDPFK                              
000350           MOVE SPACE           TO MFS-KDTRTYP                            
000351         END-IF                                                           
000352                                                                          
000353         IF GODK-MID                                                      
000354           IF WS-IDLEVNR NOT = SPACE AND WS-IDOKOLLI > ZERO AND           
000355                                                 NOT PARTI-NKL            
000356             MOVE ZERO  TO WS-IDLOPNRM                                    
000357                           REQU-IDLOPNRM-KEY                              
000358           END-IF                                                         
000359         END-IF                                                           
000360       END-IF                                                             
000361                                                                          
000362       MOVE WS-IDDC     TO MOD-IDDC-UT                                    
000363       MOVE WS-IDLOPNRM TO MOD-IDLOPNRM-UT                                
000364       INSPECT MOD-IDLOPNRM-UT REPLACING LEADING ZERO BY SPACE            
000365       MOVE WS-IDLEVNR TO MOD-IDLEVNR-KOLLI-UT                            
000366       MOVE WS-IDOKOLLI TO MOD-IDOKOLLI-UT                                
000367       INSPECT MOD-IDOKOLLI-UT REPLACING LEADING ZERO BY SPACE            
000368     ELSE                                                                 
000369       MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                
000370                               MOD-IDDC-UT                                
000371                               MOD-IDLOPNRM-IN                            
000372                               MOD-IDLOPNRM-UT                            
000373                               MOD-IDLEVNR-KOLLI-IN                       
000374                               MOD-IDLEVNR-KOLLI-UT                       
000375                               MOD-IDOKOLLI-IN                            
000376                               MOD-IDOKOLLI-UT                            
000377     END-IF                                                               
000378     .                                                                    
000379     EJECT                                                                
000380 C-INIT-REQU   SECTION.                                                   
000381     MOVE MAX-KVRADER      TO REQU-KVRADER                                
000382*                                                                         
000383     MOVE +1               TO INDX                                        
000384     PERFORM UNTIL INDX > MAX-INDX                                        
000385       MOVE MID-KDCMDVAL(INDX)     TO REQU-KDCMDVAL-LINE(INDX)            
000386       MOVE MID-IDRADNR(INDX)      TO REQU-IDRADNR-LINE(INDX)             
000387       ADD +1 TO INDX                                                     
000388     END-PERFORM                                                          
000389     PERFORM UNTIL INDX > MAX-REQU-INDX                                   
000390       MOVE ALL '+'        TO REQU-KDCMDVAL-LINE(INDX)                    
000391**                            REQU-IDRADNR-LINE(INDX)                     
000392       ADD +1 TO INDX                                                     
000393     END-PERFORM                                                          
000394                                                                          
000395     IF MSGI-IDLAND-SPR = 'SE'                                            
000396       MOVE 'SV'              TO REQU-IDSPRAK                             
000397     ELSE                                                                 
000398       MOVE 'EN'              TO REQU-IDSPRAK                             
000399     END-IF                                                               
000400                                                                          
000401     .                                                                    
000402     EJECT                                                                
000403 F-BUSINESS-LOGIC-W6014810    SECTION.                                    
000404     CALL W6014810 USING                                                  
000405          REQU-AREA RESP-AREA MAX-KVRADER                                 
000406          MSG-PCB                                                         
000407          ALT1-PCB                                                        
000408          DISP-PCB                                                        
000409          USEA-PCB                                                        
000410          INLBSEQ-PCB                                                     
000411          INLCSEQ-PCB                                                     
000412          KOMA-PCB                                                        
000413          WDD3-PCB                                                        
                WDB6-PCB.                                                       
000414                                                                          
000415     PERFORM FA-SET-MSG-AND-HILIGHT                                       
000416     PERFORM FB-MOVE-RESP-TO-MOD                                          
000417     .                                                                    
000418     EJECT                                                                
000419                                                                          
000420 FA-SET-MSG-AND-HILIGHT   SECTION.                                        
000421     MOVE RESP-IDMSG-ERROR TO WS-IDMSG-ERROR                              
000422     MOVE RESP-IDMSG-INFO  TO WS-IDMSG-INFO                               
000423                                                                          
000424     IF WRONG-KEY                                                         
000425       MOVE NEJ                   TO INDATA-SW                            
000426       MOVE ERR-WRONG-KEY         TO MED-IDMFSFEL                         
000427     END-IF                                                               
000428                                                                          
000429     IF DIVERSEKOLLI                                                      
000430       MOVE ERR-DIVERSEKOLLI      TO MED-IDMFSFEL                         
000431     END-IF                                                               
000432                                                                          
000433     IF OTILLATEN-UPPD                                                    
000434       MOVE ERR-OTILLATEN-UPPD    TO MED-IDMFSFEL                         
000435     END-IF                                                               
000436                                                                          
000437     IF PF11-AND-NO-DATA                                                  
000438       MOVE ERR-PF11-AND-NO-DATA  TO MED-IDMFSFEL                         
000439     END-IF                                                               
000440                                                                          
000441     IF NOT-IN-REG                                                        
000442       MOVE ERR-NOT-IN-REG        TO MED-IDMFSFEL                         
000443     END-IF                                                               
000444                                                                          
000445     IF CORR-HILITE-FLDS                                                  
000446       MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                         
000447     END-IF                                                               
000448                                                                          
000449     CALL WMEDKONV USING MED-WMEDAREA                                     
000450     MOVE MED-TEMFSFEL    TO MOD-TEMFSFEL                                 
000451     PERFORM MFS-ROER-EJ-FAELT-UT                                         
000452     PERFORM MFS-ROER-EJ-FAELT-IN                                         
000453*                                                                         
000454     IF FIRST-PAGE                                                        
000455       MOVE INF-FIRST-PAGE      TO MED-IDMFSINF                           
000456     END-IF                                                               
000457                                                                          
000458     IF FIRST-PAGE                                                        
000459       MOVE INF-FIRST-PAGE      TO MED-IDMFSINF                           
000460     END-IF                                                               
000461                                                                          
000462     IF MORE-INFO-EXISTS                                                  
000463       MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                          
000464     END-IF                                                               
000465                                                                          
000466     IF PRESS-PF11                                                        
000467       MOVE INF-PRESS-PF11      TO MED-IDMFSINF                           
000468     END-IF                                                               
000469                                                                          
000470     IF UPDATE-DONE                                                       
000471       MOVE INF-UPDATE-DONE     TO MED-IDMFSINF                           
000472     END-IF                                                               
000473                                                                          
000474     CALL WMEDKONV USING MED-WMEDAREA                                     
000475     MOVE MED-TEMFSINF  TO MOD-TEMFSINF                                   
000476                                                                          
000477     .                                                                    
000478     EJECT                                                                
000479                                                                          
000480 FB-MOVE-RESP-TO-MOD      SECTION.                                        
000481     MOVE +1 TO INDX                                                      
000482     IF NOT WRONG-KEY                                                     
000483     MOVE RESP-IDRADNR-START    TO MOD-IDRADNR-ENTER                      
000484     MOVE RESP-IDRADNR-NEXT     TO MOD-IDRADNR-NEXT                       
000485     IF WS-IDLEVNR NOT = SPACE AND WS-IDOKOLLI > ZERO                     
000486       MOVE RESP-IDLOPNRM       TO MOD-IDLOPNRM-UT                        
000487     END-IF                                                               
000488*                                                                         
000489     IF RESP-IDARTNR = ALL '+'                                            
000490       MOVE MFS-ROER-EJ-FAELT  TO MOD-IDARTNR                             
000491     ELSE                                                                 
000492       MOVE RESP-IDARTNR       TO MOD-IDARTNR                             
000493     END-IF                                                               
000494*                                                                         
000495     IF RESP-KVAVIS = ALL '+'                                             
000496       MOVE MFS-ROER-EJ-FAELT  TO MOD-KVAVIS                              
000497     ELSE                                                                 
000498       MOVE RESP-KVAVIS        TO MOD-KVAVIS                              
000499     END-IF                                                               
000500*                                                                         
000501     IF RESP-BEART = ALL '+'                                              
000502       MOVE MFS-ROER-EJ-FAELT  TO MOD-BEART                               
000503     ELSE                                                                 
000504       MOVE RESP-BEART         TO MOD-BEART                               
000505     END-IF                                                               
000506*                                                                         
000507     MOVE +1 TO INDX                                                      
000508     PERFORM UNTIL INDX > RESP-KVRADER                                    
000509*                                                                         
000510       MOVE RESP-KDCMDVAL-LINE-ATTR (INDX)                                
000511                                   TO MOD-KDCMDVAL-IN-ATTR (INDX)         
000512       IF RESP-KDCMDVAL-LINE (INDX) = ALL '+'                             
000513         MOVE MFS-ROER-EJ-FAELT         TO MOD-KDCMDVAL-IN (INDX)         
000514       ELSE                                                               
000515         MOVE RESP-KDCMDVAL-LINE (INDX) TO MOD-KDCMDVAL-IN (INDX)         
000516       END-IF                                                             
000517*                                                                         
000518       IF RESP-IDRADNR-LINE (INDX) = ALL '+'                              
000519         MOVE MFS-ROER-EJ-FAELT        TO MOD-IDRADNR (INDX)              
000520       ELSE                                                               
000521         MOVE RESP-IDRADNR-LINE (INDX) TO MOD-IDRADNR (INDX)              
000522       END-IF                                                             
000523*                                                                         
000524       IF RESP-KVINLART-LINE (INDX) = ALL '+'                             
000525         MOVE MFS-ROER-EJ-FAELT        TO MOD-KVINLART (INDX)             
000526       ELSE                                                               
000527         MOVE RESP-KVINLART-LINE (INDX)                                   
000528                                       TO MOD-KVINLART (INDX)             
000529       END-IF                                                             
000530*                                                                         
000531       IF RESP-ADINLOMR-LINE (INDX) = ALL '+'                             
000532         MOVE MFS-ROER-EJ-FAELT       TO MOD-ADINLOMR (INDX)              
000533       ELSE                                                               
000534         MOVE RESP-ADINLOMR-LINE (INDX)                                   
000535                                      TO MOD-ADINLOMR (INDX)              
000536       END-IF                                                             
000537*                                                                         
000538       IF RESP-KDINLSTA-LINE (INDX) = ALL '+'                             
000539         MOVE MFS-ROER-EJ-FAELT       TO MOD-KDINLSTA (INDX)              
000540       ELSE                                                               
000541         MOVE RESP-KDINLSTA-LINE (INDX) TO MOD-KDINLSTA (INDX)            
000542       END-IF                                                             
000543*                                                                         
000544       IF RESP-IDLEVNR-KOLLI-LINE(INDX) = ALL '+'                         
000545         MOVE MFS-ROER-EJ-FAELT   TO MOD-IDLEVNR-KOLLI(INDX)              
000546       ELSE                                                               
000547         MOVE RESP-IDLEVNR-KOLLI-LINE(INDX)                               
000548                                  TO MOD-IDLEVNR-KOLLI(INDX)              
000549       END-IF                                                             
000550*                                                                         
000551       IF RESP-IDOKOLLI-LINE (INDX) = ALL '+'                             
000552         MOVE MFS-ROER-EJ-FAELT   TO MOD-IDOKOLLI (INDX)                  
000553       ELSE                                                               
000554         MOVE RESP-IDOKOLLI-LINE (INDX)                                   
000555                                  TO MOD-IDOKOLLI (INDX)                  
000556       END-IF                                                             
000557*                                                                         
000558       IF RESP-FLPRIO-LINE (INDX)  = ALL '+'                              
000559         MOVE MFS-ROER-EJ-FAELT   TO MOD-FLPRIO (INDX)                    
000560       ELSE                                                               
000561         MOVE RESP-FLPRIO-LINE (INDX)                                     
000562                                  TO MOD-FLPRIO (INDX)                    
000563       END-IF                                                             
000564*                                                                         
000565       ADD +1 TO INDX                                                     
000566     END-PERFORM                                                          
000567     END-IF                                                               
000568                                                                          
000569*                                                                         
000570* CLOSE/CLEAR THE REMAINING LINES ON THE SCREEN                           
000571     PERFORM UNTIL INDX    >  MAX-INDX                                    
000572       MOVE MFS-STAENG-FAELT TO MOD-KDCMDVAL-IN-ATTR(INDX)                
000573                                                                          
000574       MOVE MFS-RENSA-FAELT TO MOD-IDRADNR  (INDX)                        
000575                               MOD-KVINLART (INDX)                        
000576                               MOD-ADINLOMR (INDX)                        
000577                               MOD-KDINLSTA (INDX)                        
000578                               MOD-IDLEVNR-KOLLI(INDX)                    
000579                               MOD-IDOKOLLI (INDX)                        
000580                               MOD-FLPRIO   (INDX)                        
000581       ADD +1 TO INDX                                                     
000582     END-PERFORM                                                          
000583                                                                          
000584     IF UPDATE-DONE                                                       
000585       PERFORM MFS-FORM-ATTR                                              
000586       PERFORM MFS-RENSA-FAELT-IN                                         
000587     END-IF                                                               
000588     .                                                                    
000589     EJECT                                                                
000590                                                                          
000591 MFS-RENSA-FAELT-UT SECTION.                                              
000592                                                                          
000593*    --- ALLA UTDATA-FÄLT                                                 
000594*    --- INKL. BLÄDDRINGSNYCKLAR                                          
000595     MOVE MFS-RENSA-FAELT TO MOD-IDRADNR-ENTER                            
000596                             MOD-IDRADNR-NEXT                             
000597                             MOD-IDARTNR                                  
000598                             MOD-KVAVIS                                   
000599                             MOD-BEART                                    
000600                                                                          
000601                                                                          
000602     MOVE +1 TO INDX                                                      
000603     PERFORM UNTIL INDX > MAX-INDX                                        
000604       MOVE MFS-RENSA-FAELT TO MOD-KDCMDVAL-IN (INDX)                     
000605                               MOD-IDRADNR (INDX)                         
000606                               MOD-IDLEVNR-KOLLI (INDX)                   
000607                               MOD-IDOKOLLI (INDX)                        
000608                               MOD-KVINLART (INDX)                        
000609                               MOD-ADINLOMR (INDX)                        
000610                               MOD-FLPRIO (INDX)                          
000611                               MOD-KDINLSTA (INDX)                        
000612       ADD +1 TO INDX                                                     
000613     END-PERFORM                                                          
000614     .                                                                    
000615     SKIP3                                                                
000616 MFS-RENSA-FAELT-IN SECTION.                                              
000617                                                                          
000618*    --- ALLA INDATA-FÄLT                                                 
000619     MOVE +1 TO INDX                                                      
000620     PERFORM UNTIL INDX > MAX-KVRADER                                     
000621       MOVE MFS-RENSA-FAELT TO MOD-KDCMDVAL-IN (INDX)                     
000622       ADD +1 TO INDX                                                     
000623     END-PERFORM                                                          
000624     .                                                                    
000625     EJECT                                                                
000626 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
000627                                                                          
000628*    --- ALLA UTDATA-FÄLT                                                 
000629*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
000630                                                                          
000631     MOVE MFS-ROER-EJ-FAELT TO MOD-IDRADNR-ENTER                          
000632                               MOD-IDRADNR-NEXT                           
000633     MOVE +1 TO INDX                                                      
000634     PERFORM UNTIL INDX > MAX-INDX                                        
000635       MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMDVAL-IN (INDX)                   
000636                                 MOD-IDRADNR (INDX)                       
000637                                 MOD-IDLEVNR-KOLLI (INDX)                 
000638                                 MOD-IDOKOLLI (INDX)                      
000639                                 MOD-KVINLART (INDX)                      
000640                                 MOD-ADINLOMR (INDX)                      
000641                                 MOD-FLPRIO (INDX)                        
000642                                 MOD-KDINLSTA (INDX)                      
000643       ADD +1 TO INDX                                                     
000644     END-PERFORM                                                          
000645     .                                                                    
000646     SKIP2                                                                
000647 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
000648                                                                          
000649*    --- ALLA INDATA-FÄLT                                                 
000650     MOVE +1 TO INDX                                                      
000651     PERFORM UNTIL INDX > MAX-INDX                                        
000652       MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMDVAL-IN (INDX)                   
000653       ADD +1 TO INDX                                                     
000654     END-PERFORM                                                          
000655     .                                                                    
000656     EJECT                                                                
000657 MFS-FORM-ATTR SECTION.                                                   
000658                                                                          
000659*    --- ALLA INDATA-FÄLT                                                 
000660     MOVE +1 TO INDX                                                      
000661     PERFORM UNTIL INDX > MAX-INDX                                        
000662       MOVE MFS-FORMATETS-ATTR TO MOD-KDCMDVAL-IN-ATTR (INDX)             
000663       ADD +1 TO INDX                                                     
000664     END-PERFORM                                                          
000665     .                                                                    
000666     SKIP2                                                                
000667 MFS-LAES-IN-IGEN SECTION.                                                
000668                                                                          
000669*    --- ALLA INDATA-FÄLT                                                 
000670     MOVE +1 TO INDX                                                      
000671     PERFORM UNTIL INDX > MAX-INDX                                        
000672       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMDVAL-IN-ATTR (INDX)          
000673       ADD +1 TO INDX                                                     
000674     END-PERFORM                                                          
000675     .                                                                    
000676     EJECT                                                                
000677* --- IMS SEKTIONER ---                                                   
000678     SKIP3                                                                
000679 IMS-GET-MSG SECTION.                                                     
000680                                                                          
000681     MOVE '  QC' TO GODK-STATUSKODER                                      
000682     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
000683     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
000684     PERFORM IMS-STATUSKONTROLL                                           
000685     .                                                                    
000686     SKIP3                                                                
000687 IMS-INSERT-MSG SECTION.                                                  
000688                                                                          
000689     IF MSGI-IDLAND-SPR = 'SE'                                            
000690       MOVE '0' TO MFS-KDHUVOMR                                           
000691     END-IF                                                               
000692     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
000693     MOVE SPACE TO GODK-STATUSKODER                                       
000694     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
000695     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
000696     PERFORM IMS-STATUSKONTROLL                                           
000697     .                                                                    
000698     EJECT                                                                
000699 IMS-STATUSKONTROLL SECTION.                                              
000700                                                                          
000701     SET STATUS-IX TO 1                                                   
000702     SEARCH GODK-STATUS                                                   
000703       AT END                                                             
000704         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
000705         DELIMITED BY SIZE INTO FELTEXT                                   
000706         CALL FELLOG                                                      
000707       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
000708         CONTINUE                                                         
000709     END-SEARCH                                                           
000710     .                                                                    
