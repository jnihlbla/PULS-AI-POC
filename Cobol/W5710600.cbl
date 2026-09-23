000001 ID DIVISION.                                                             
000002 PROGRAM-ID.     W5710600.                                                
000003 AUTHOR.         ARCHANA BHAT.                                            
000004 DATE-WRITTEN.   11/11/14.                                                
000005 DATE-COMPILED.                                                           
000006                                                                          
000007                                                                          
000008*    FUNCTION:                                                            
000009*        RELOAD ACS                                                       
000010*                                                                         
000011*        THE PROGRAM READS     WDK6                                       
000012*        THE PROGRAM READS     WDB6                                       
000013*        THE PROGRAM UPDATES   WDH7                                       
000014*        THE PROGRAM UPDATES   WDK7                                       
000015*        THE PROGRAM UPDATES   WDR8                                       
000016*        THE PROGRAM UPDATES   WDR9                                       
000017*        THE PROGRAM UPDATES   WDJ7                                       
000018*        THE PROGRAM UPDATES   WDL9                                       
000019*        THE PROGRAM UPDATES   WDR2                                       
000020*                                                                         
000021                                                                          
000022     SKIP3                                                                
000023 ENVIRONMENT DIVISION.                                                    
000024     SKIP2                                                                
000025 INPUT-OUTPUT SECTION.                                                    
000026                                                                          
000027 FILE-CONTROL.                                                            
000028     SKIP2                                                                
000029*          --- INVENTORY INFO                                             
000030     SELECT W571D1                     ASSIGN TO W57106D1.                
000031     EJECT                                                                
000032 DATA DIVISION.                                                           
000033     SKIP3                                                                
000034 FILE SECTION.                                                            
000035     SKIP3                                                                
000036 FD  W571D1                                                               
000037     RECORDING       F                                                    
000038     BLOCK CONTAINS  0.                                                   
000039                                                                          
000040*01  -COPY W57105      -L.                                                
000041     EJECT                                                                
000042 WORKING-STORAGE SECTION.                                                 
000043                                                                          
000044 77  IDPGM                       PIC X(8)    VALUE 'W5710600'.            
000045 77  WS-CURRENT-DATE             PIC X(8)    VALUE SPACE.                 
000046 77  WS-CURRENT-TIME             PIC 9(6)    VALUE ZERO.                  
000047 77  WS-SAVE-IDDC                PIC X(2)    VALUE SPACE.                 
000048 01  CHKP-VAR.                                                            
000049     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
000050     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
000051     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
000052     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
000053     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
000054     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
000055 77  YES                         PIC X       VALUE 'J'.                   
000056 77  NOO                         PIC X       VALUE 'N'.                   
000057 77  W-KVLS                      PIC S9(7)   VALUE ZERO.                  
000058 77  KVJUSTKV-WS                 PIC S9(7)   VALUE ZERO.                  
000059 77  WS-DATE-IN-WEEKS            PIC 9(5)    VALUE ZERO.                  
000060 77  WS-POS-DIFF                 PIC S9(9)V99    VALUE ZERO.              
000061 77  WS-NEG-DIFF                 PIC S9(9)V99    VALUE ZERO.              
000062 77  WS-TIREGDAT                 PIC 9(6)    VALUE ZERO.                  
000063 77  WLOGG-TID                   PIC S9(9)   VALUE ZERO.                  
000064 77  LOGG-DATUM                  PIC S9(8)   VALUE ZERO.                  
000065 77  WLOGG-KVJUST                PIC  9(7)   VALUE ZERO.                  
000066     SKIP2                                                                
000067 01  MISC.                                                                
000068     03  W-EKH-IDARTNR           PIC X(9)    VALUE SPACE.                 
000069                                                                          
000070 01  ERROR-TEXT.                                                          
000071     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
000072     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
000073                                                                          
000074 01  WS-INS-DEL-CNT.                                                      
000075     03 WS-ISRT-WDR8             PIC  9(5)   VALUE ZERO.                  
000076     03 WS-ISRT-WDR9             PIC  9(5)   VALUE ZERO.                  
000077     03 WS-ISRT-WDH7             PIC  9(5)   VALUE ZERO.                  
000078     03 WS-ISRT-WDL9             PIC  9(5)   VALUE ZERO.                  
000079     03 WS-REPL-WDK711           PIC  9(5)   VALUE ZERO.                  
000080 77  WS-UPLOAD-ACS-SW            PIC X       VALUE 'N'.                   
000081     88  WS-UPLOAD-ACS                       VALUE 'J'.                   
000082 77  W571D1-EOF-SW               PIC X       VALUE 'N'.                   
000083     88  END-OF-W571D1                       VALUE 'Y'.                   
000084     EJECT                                                                
000085 01  GENERAL-SUBPROGRAMS.                                                 
000086*                                                                         
000087     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000088     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000089     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
000090     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
000091     03  W009CIA                 PIC X(8)    VALUE 'W009CIA '.            
000092                                                                          
000093*    --- PARAMETRAR TILL SUBPROGRAM W009CIA                               
000094*01 -COPY W009CIA                                                         
000095     EJECT                                                                
000096                                                                          
000097*    --- VALID IDDC CODES                                                 
000098*                                                                         
000099*01  -COPY WWDC99                                                         
000100     EJECT                                                                
000101*    --- PARAMETRAR TILL POSTSUM                                          
000102*                                                                         
000103*01  -COPY W0005   -PRE  POSTSUM-                                         
000104     EJECT                                                                
000105                                                                          
000106 01  WS-FIX-DATUM.                                                        
000107     03  WS-FIX-TISEGKEY     PIC 9(9).                                    
000108     03  WS-FILLER1 REDEFINES WS-FIX-TISEGKEY.                            
000109         05 WS-FILLER1-1-2   PIC 9(2).                                    
000110         05 WS-TISEGKEY-3-8  PIC 9(6).                                    
000111         05 WS-FILLER1-9     PIC 9(1).                                    
000112     03  WS-FILLER2 REDEFINES WS-FIX-TISEGKEY.                            
000113         05 WS-TISEGKEY-1-8  PIC 9(8).                                    
000114         05 WS-FILLER2-9     PIC 9(1).                                    
000115     SKIP2                                                                
000116                                                                          
000117 01  WS-TISEGKEYAREA.                                                     
000118     03  WS-TIAAAAMMDDL      PIC 9(9) VALUE ZERO.                         
000119     03  FILLER REDEFINES WS-TIAAAAMMDDL.                                 
000120         05  WS-TIAAAAMMDD   PIC 9(8).                                    
000121         05  WS-LOPNR        PIC 9(1).                                    
000122     03  WS-TISEGKEY         PIC S9(9)  VALUE ZERO COMP-3.                
000123                                                                          
000124     SKIP2                                                                
000125                                                                          
000126 01  W-TISEGKEY-X.                                                        
000127     03  W-TISEGKEY          PIC S9(9)  VALUE +999999999 COMP-3.          
000128     EJECT                                                                
000129                                                                          
000130 01  FILLER                      PIC X(16)  VALUE 'WDATAREA'.             
000131*01  -COPY WDATAREA                                                       
000132 01  W571D1-AREA-START           PIC X(24)   VALUE                        
000133                                             'W571D1-AREA-START'.         
000134     SKIP2                                                                
000135*01  AREA -COPY W57105     -PRE W57105-                                   
000136*                                                                         
000137 01  FILLER                    PIC X(24)     VALUE                        
000138                                             'A08-AREA-START'.            
000139     SKIP2                                                                
000140*01  -COPY W510A08         -PRE A08-                                      
000141*                                                                         
000142     EJECT                                                                
000143 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000144     SKIP3                                                                
000145 01  KEYS-TILL-DLI.                                                       
000146     03  W-IDDC-B6-X.                                                     
000147         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
000148     03  W-WDGXKEY-5104-X.                                                
000149          05 W-IDHTYP-5103       PIC X(4)    VALUE '5103'.                
000150          05 W-LOWVALUE          PIC X(26)   VALUE LOW-VALUE.             
000151     03  W-IDDC-5104-X.                                                   
000152         05  W-IDDC-5104         PIC X(2)    VALUE SPACE.                 
000153     03  W-IDARTNR-X.                                                     
000154         05  W-IDARTNR           PIC S9(9)   VALUE ZERO  COMP-3.          
000155     03  W-IDDC-X.                                                        
000156         05  W-IDDC              PIC X(2).                                
000157     03  W-WDR801-KY-X.                                                   
000158         05  W-WDR801-IDPGM      PIC X(8)  VALUE SPACE.                   
000159         05  W-WDR801-TIREGDAT   PIC S9(7) VALUE ZERO  COMP-3.            
000160         05  W-WDR801-TIKLOCK    PIC S9(9) VALUE ZERO  COMP-3.            
000161         05  W-WDR801-IDSEKVNR   PIC S9(3) VALUE ZERO  COMP-3.            
000162         05  W-WDR801-IDCPYTXT   PIC X(8)  VALUE SPACE.                   
000163     03  W-WDJ701-X.                                                      
000164         05  W-WDJ701-IDDC       PIC X(2)    VALUE SPACE.                 
000165         05  W-WDJ701-IDARTNR    PIC S9(9)   VALUE ZERO   COMP-3.         
000166     SKIP2                                                                
000167*    --- STATUS-KOD FRÅN IMS                                              
000168 01  STATUS-WS                   PIC XX.                                  
000169     88  SEGMENT-FOUND                       VALUE '  '.                  
000170     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
000171     88  SEGMENT-MISSING                     VALUE 'GE'.                  
000172     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
000173     88  IMS-NOT-OK                          VALUE 'XD'.                  
000174     SKIP2                                                                
000175 01  GOOD-STATUSCODES.                                                    
000176     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000177     SKIP3                                                                
000178 01  SSA1                        PIC X(64).                               
000179 01  SSA2                        PIC X(64).                               
000180     EJECT                                                                
000181*    --- IMS FUNCTION CODES                                               
000182*01  -COPY W0003                                                          
000183     EJECT                                                                
000184*    ---  DLI INPUT-OUTPUT AREA                                           
000185                                                                          
000186 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
000187 01  DLI-IO-WDK601.                                                       
000188*    03  -COPY WDK601                                                     
000189     EJECT                                                                
000190 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
000191 01  DLI-IO-WDK611.                                                       
000192*    03  -COPY WDK611                                                     
000193 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH701'.                      
000194 01  DLI-IO-WDH701.                                                       
000195*    03  -COPY WDH701                                                     
000196     EJECT                                                                
000197 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH711'.                      
000198 01  DLI-IO-WDH711.                                                       
000199*    03  -COPY WDH711                                                     
000200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
000201 01  DLI-IO-WDK701.                                                       
000202*    03  -COPY WDK701                                                     
000203     EJECT                                                                
000204 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
000205 01  DLI-IO-WDK711.                                                       
000206*    03  -COPY WDK711                                                     
000207 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR801'.                      
000208*01  DLI-IO-WDR801  -COPY WDR801                                          
000209*    05  -COPY W510EKHA -RED FIL-WDR801-DATA                              
000210 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDJ701'.                      
000211 01  DLI-IO-WDJ701.                                                       
000212*    03  -COPY WDJ701                                                     
000213 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL901'.                      
000214 01  DLI-IO-WDL901.                                                       
000215*    03  -COPY WDL901                                                     
000216 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX5104'.                    
000217 01  DLI-IO-WDGX5104.                                                     
000218*    03  -COPY WDGX5104                                                   
000219 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
000220 01  DLI-IO-WDB601.                                                       
000221*    03  -COPY WDB601                                                     
000222 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR901'.                      
000223*01  DLI-IO-WDR901  -COPY WDR901 -PRE WDR9-                               
000224*    05  -COPY W510EKHA -RED WDR9-FIL-WDR901-DATA -PRE WDR9-              
000225                                                                          
000226     EJECT                                                                
000227 LINKAGE SECTION.                                                         
000228                                                                          
000229*01  -COPY W0009  -PRE MSG-                                      0        
000230                                                                          
000231*01  -COPY W0008  -PRE WDK6-                                              
000232     05  FILLER                  PIC X.                                   
000233                                                                          
000234*01  -COPY W0008  -PRE WDH7-                                              
000235     05  FILLER                  PIC X.                                   
000236                                                                          
000237*01  -COPY W0008  -PRE WDK7-                                              
000238     05  FILLER                  PIC X.                                   
000239                                                                          
000240*01  -COPY W0008  -PRE WDR8-                                              
000241     05  FILLER                  PIC X.                                   
000242                                                                          
000243*01  -COPY W0008  -PRE WDJ7-                                              
000244     05  FILLER                  PIC X.                                   
000245                                                                          
000246*01  -COPY W0008  -PRE WDL9-                                              
000247     05  FILLER                  PIC X.                                   
000248                                                                          
000249*01  -COPY W0008  -PRE 5104-                                              
000250     05  FILLER                  PIC X.                                   
000251                                                                          
000252*01  -COPY W0008  -PRE WDB6-                                              
000253     05  FILLER                  PIC X.                                   
000254                                                                          
000255*01  -COPY W0008  -PRE WDR9-                                              
000256     05  FILLER                  PIC X.                                   
000257                                                                          
000258 PROCEDURE DIVISION  USING MSG-PCB WDK6-PCB WDH7-PCB                      
000259       WDK7-PCB WDR8-PCB WDJ7-PCB WDL9-PCB 5104-PCB WDB6-PCB              
000260       WDR9-PCB.                                                          
000261                                                                          
000262 MAIN SECTION.                                                            
000263     ENTRY 'DLITCBL' USING MSG-PCB WDK6-PCB WDH7-PCB                      
000264       WDK7-PCB WDR8-PCB WDJ7-PCB WDL9-PCB 5104-PCB WDB6-PCB              
000265       WDR9-PCB.                                                          
000266                                                                          
000267     PERFORM A-INIT                                                       
000268     PERFORM S01-READ-W571D1                                              
000269     PERFORM B-READ-WDR2                                                  
000270     IF WS-UPLOAD-ACS                                                     
000271       PERFORM UNTIL END-OF-W571D1                                        
000272         IF W57105-ADLAGOMR NOT = LOW-VALUES                              
000273            IF CHKP-ANT > CHKP-MAX                                        
000274               PERFORM X-TAKE-CHECKPOINT                                  
000275            END-IF                                                        
000276            PERFORM C-PROCESS-PARA                                        
000277         ELSE                                                             
000278            MOVE W57105-IDDC TO WS-SAVE-IDDC                              
000279         END-IF                                                           
000280         PERFORM S01-READ-W571D1                                          
000281       END-PERFORM                                                        
000282     END-IF                                                               
000283                                                                          
000284     DISPLAY 'POSITIV DIFF=' WS-POS-DIFF                                  
000285     DISPLAY 'NEGATIV DIFF=' WS-NEG-DIFF                                  
000286     PERFORM D-DELETE-DUMMY-RECORD                                        
000287     PERFORM Z-FINIT                                                      
000288                                                                          
000289     MOVE ZERO TO RETURN-CODE                                             
000290     GOBACK                                                               
000291     .                                                                    
000292     EJECT                                                                
000293                                                                          
000294 A-INIT SECTION.                                                          
000295     PERFORM IMS-RESTART                                                  
000296     OPEN INPUT W571D1                                                    
000297                                                                          
000298     MOVE FUNCTION CURRENT-DATE (1:8)  TO WS-CURRENT-DATE                 
000299     MOVE FUNCTION CURRENT-DATE (9:6)  TO WS-CURRENT-TIME                 
000300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
000301     MOVE 'IDAG  '      TO DAT-KDDATFORM                                  
000302     CALL  WDATKONV  USING DAT-KDDATFORM                                  
000303                           DAT-I-TIDATUM                                  
000304                           DAT-O-TIDATUM                                  
000305                           DAT-KDSVAR                                     
000306     IF DAT-KDSVAR-OK                                                     
000307         CONTINUE                                                         
000308     ELSE                                                                 
000309         CALL  FELLOG                                                     
000310     END-IF                                                               
000311     MOVE DAT-TIAAVVD  TO WS-DATE-IN-WEEKS                                
000312     .                                                                    
000313     EJECT                                                                
000314                                                                          
000315 B-READ-WDR2 SECTION.                                                     
000316     MOVE W57105-IDDC           TO W-IDDC-5104                            
000317     PERFORM IMS-GU-WDGX5104                                              
000318     IF SEGMENT-FOUND                                                     
000319        IF 5104-KDACS = 'U'                                               
000320           SET WS-UPLOAD-ACS    TO TRUE                                   
000321        END-IF                                                            
000322     END-IF                                                               
000323     .                                                                    
000324                                                                          
000325 C-PROCESS-PARA SECTION.                                                  
000326     PERFORM CA-WRITE-DUMMY-RECORD                                        
000327                                                                          
000328     IF W57105-IDCOUNTER-T-REG NOT = SPACE                                
000329       MOVE W57105-KVTCOUNT                 TO W-KVLS                     
000330       MOVE W57105-TIREGDAT-TCOUNT          TO WS-TIREGDAT                
000331       MOVE W57105-IDCOUNTER-T-REG(1:8)     TO LOGG-IDUSER                
000332                                                                          
000333     ELSE                                                                 
000334       IF W57105-IDCOUNTER-R-REG NOT = SPACE                              
000335          MOVE W57105-KVRCOUNT              TO W-KVLS                     
000336          MOVE W57105-TIREGDAT-RCOUNT       TO WS-TIREGDAT                
000337          MOVE W57105-IDCOUNTER-R-REG(1:8)  TO LOGG-IDUSER                
000338       ELSE                                                               
000339          MOVE W57105-KVPCOUNT              TO W-KVLS                     
000340          MOVE W57105-TIREGDAT-PCOUNT       TO WS-TIREGDAT                
000341          MOVE W57105-IDCOUNTER-P-REG(1:8)  TO LOGG-IDUSER                
000342       END-IF                                                             
000343     END-IF                                                               
000344                                                                          
000345     SUBTRACT W57105-KVLS FROM W-KVLS GIVING KVJUSTKV-WS                  
000346                                                                          
000347     MOVE W57105-IDARTNR                TO W-IDARTNR                      
000348     MOVE W57105-IDDC                   TO W-IDDC                         
000349                                           W-IDDC-B6                      
000350                                           WS-IDDC                        
000351     MOVE KVJUSTKV-WS                   TO WLOGG-KVJUST                   
000352     PERFORM IMS-GHU-WDK711                                               
000353     IF SEGMENT-FOUND                                                     
000354        MOVE W-KVLS                     TO SLAG-KVLS                      
000355        MOVE WS-DATE-IN-WEEKS           TO SLAG-TIINVDAT                  
000356        PERFORM IMS-REPL-WDK711                                           
000357        ADD 1                           TO WS-REPL-WDK711                 
000358                                                                          
000359                                                                          
000360        IF KVJUSTKV-WS NOT = 0                                            
000361          IF KVJUSTKV-WS > 0                                              
000362            COMPUTE WS-POS-DIFF = WS-POS-DIFF +                           
000363                                (KVJUSTKV-WS * W57105-PRAVCOST)           
000364          ELSE                                                            
000365            COMPUTE WS-NEG-DIFF = WS-NEG-DIFF +                           
000366                                (KVJUSTKV-WS * W57105-PRAVCOST)           
000367          END-IF                                                          
000368          PERFORM CB-CREATE-BALANCE-LOG                                   
000369                                                                          
000370          MOVE SPACES                   TO DCS-KDDC                       
000371          PERFORM IMS-GU-WDB601                                           
000372          IF DCS-LAND-NON-VCC-OWNED                                       
000373             PERFORM CD-CREATE-WDR801-TRANS                               
000374          ELSE                                                            
000375             IF DCS-NDC-NA                                                
000376                PERFORM CE-SKAPA-LABPOST                                  
000377             ELSE                                                         
000378                PERFORM CF-CREATE-WDR901-TRANS                            
000379             END-IF                                                       
000380          END-IF                                                          
000381        END-IF                                                            
000382                                                                          
000383        PERFORM CC-UPDATE-INV-HISTORY                                     
000384     END-IF                                                               
000385     .                                                                    
000386                                                                          
000387 CA-WRITE-DUMMY-RECORD SECTION.                                           
000388     MOVE SPACE         TO ACS-WDJ701                                     
000389     MOVE W57105-IDDC   TO ACS-IDDC                                       
000390     MOVE +888888888    TO ACS-IDARTNR                                    
000391     MOVE ZERO          TO ACS-PRAVCOST                                   
000392                           ACS-KVLS                                       
000393                           ACS-ADLAGOMR                                   
000394                           ACS-ADGANG                                     
000395                           ACS-ADPLATS                                    
000396                           ACS-TIORDREG                                   
000397                           ACS-TIINVDAT                                   
000398                           ACS-TIAVCOST                                   
000399                           ACS-TIRETUR-BEORD                              
000400                           ACS-TISKROT-BEORD                              
000401                           ACS-KDPRODSL                                   
000402                           ACS-KDPSLLOC                                   
000403                           ACS-KVPCOUNT                                   
000404                           ACS-KVRCOUNT                                   
000405                           ACS-KVTCOUNT                                   
000406                           ACS-IDACSNR-P                                  
000407                           ACS-IDACSNR-R                                  
000408                           ACS-IDACSNR-T                                  
000409                           ACS-TIREGDAT-PCOUNT                            
000410                           ACS-TIREGDAT-PCOUNT-REG                        
000411                           ACS-TIREGDAT-RCOUNT                            
000412                           ACS-TIREGDAT-RCOUNT-REG                        
000413                           ACS-TIREGDAT-TCOUNT                            
000414                           ACS-TIREGDAT-TCOUNT-REG                        
000415                           ACS-TIREGTID-PCOUNT                            
000416                           ACS-TIREGTID-PCOUNT-REG                        
000417                           ACS-TIREGTID-RCOUNT                            
000418                           ACS-TIREGTID-RCOUNT-REG                        
000419                           ACS-TIREGTID-TCOUNT                            
000420                           ACS-TIREGTID-TCOUNT-REG                        
000421***************                                                           
000422*************** INSERT A "DUMMY"-RECORD TO INDICATE THE                   
000423*************** START OF RELOAD PROCESS                                   
000424***************                                                           
000425     PERFORM IMS-ISRT-WDJ701                                              
000426     .                                                                    
000427                                                                          
000428 CB-CREATE-BALANCE-LOG SECTION.                                           
000429     IF KVJUSTKV-WS > 0                                                   
000430      MOVE '+' TO LOGG-IDTECKEN-KVLS                                      
000431     ELSE                                                                 
000432      MOVE '-' TO LOGG-IDTECKEN-KVLS                                      
000433     END-IF                                                               
000434                                                                          
000435     MOVE W57105-IDARTNR            TO LOGG-IDARTNR                       
000436     MOVE 9                         TO LOGG-IDSEKVNR                      
000437     MOVE W-IDDC                    TO LOGG-IDDC                          
000438     MOVE 'MISC'                    TO LOGG-IDHUVTYP                      
000439     MOVE 'ADJ'                     TO LOGG-IDSUBTYP                      
000440     MOVE 'W5710600'                TO LOGG-IDPGM                         
000441     MOVE '0000'                    TO LOGG-IDTRANS                       
000442     MOVE SPACE                     TO LOGG-REF                           
000443     MOVE SPACE                     TO LOGG-IDTECKEN-KVAKS                
000444     MOVE SPACE                     TO LOGG-IDTECKEN-KVAKS-PAV            
000445     MOVE SPACE                     TO LOGG-IDTECKEN-KVEFRS               
000446     MOVE WLOGG-KVJUST              TO LOGG-KVART-SALDO                   
000447     IF SLAG-KVAKS-SDC NOT NUMERIC                                        
000448        MOVE 0                      TO LOGG-KVAKS                         
000449     ELSE                                                                 
000450        MOVE SLAG-KVAKS-SDC         TO LOGG-KVAKS                         
000451     END-IF                                                               
000452     IF SLAG-KVAKS-PAV NOT NUMERIC                                        
000453        MOVE 0                      TO LOGG-KVAKS-PAV                     
000454     ELSE                                                                 
000455        MOVE SLAG-KVAKS-PAV         TO LOGG-KVAKS-PAV                     
000456     END-IF                                                               
000457     IF SLAG-KVEFRS NOT NUMERIC                                           
000458        MOVE 0                      TO LOGG-KVEFRS                        
000459     ELSE                                                                 
000460        MOVE SLAG-KVEFRS            TO LOGG-KVEFRS                        
000461     END-IF                                                               
000462     MOVE W-KVLS                    TO LOGG-KVLS                          
000463     MOVE ZERO                       TO LOGG-DAREGDAT-LADD                
000464     MOVE FUNCTION CURRENT-DATE(1:8) TO LOGG-DATUM                        
000465     COMPUTE LOGG-DAREGDAT-9KOMPL = 999999999 - LOGG-DATUM                
000466     MOVE FUNCTION CURRENT-DATE(9:8) TO WLOGG-TID                         
000467     COMPUTE LOGG-TIKLOCK-9KOMPL = 999999999 - WLOGG-TID                  
000468                                                                          
000469     PERFORM IMS-ISRT-WDL901                                              
000470     ADD 1                           TO WS-ISRT-WDL9                      
000471     IF SEGMENT-FOUND-EXISTS                                              
000472       PERFORM UNTIL NOT SEGMENT-FOUND-EXISTS                             
000473          ADD -1 TO LOGG-IDSEKVNR                                         
000474          PERFORM IMS-ISRT-WDL901                                         
000475       END-PERFORM                                                        
000476     END-IF                                                               
000477     .                                                                    
000478     EJECT                                                                
000479                                                                          
000480 CC-UPDATE-INV-HISTORY SECTION.                                           
000481     PERFORM IMS-GHU-WDH701                                               
000482     IF SEGMENT-NOMORE                                                    
000483       MOVE W-IDARTNR TO INVA-IDARTNR                                     
000484       PERFORM IMS-ISRT-WDH701                                            
000485     END-IF                                                               
000486                                                                          
000487     MOVE FUNCTION CURRENT-DATE(1:8) TO INVH-DAREGDAT-CRE                 
000488                                        INVH-DAREGDAT-CLO                 
000489                                        WS-TIAAAAMMDD                     
000490     MOVE 9                          TO WS-LOPNR                          
000491     COMPUTE WS-TISEGKEY = 999999999 - WS-TIAAAAMMDDL                     
000492     MOVE WS-TISEGKEY                TO INVH-TISEGKEY                     
000493     MOVE KVJUSTKV-WS                TO INVH-KVJUSTKV                     
000494     MOVE W-IDDC                     TO INVH-IDDC                         
000495     MOVE 9                          TO INVH-KDJUSTYP                     
000496     MOVE 'W57106'                   TO INVH-IDUSER-CLO                   
000497     MOVE 'W57106'                   TO INVH-IDUSER-CRE                   
000498     MOVE SPACE                      TO INVH-IDPW                         
000499     MOVE NOO                        TO INVH-FLAUTLSJ                     
000500     MOVE ZERO                       TO INVH-PRARTSTD                     
000501                                        INVH-DAREGDAT-PR1                 
000502                                        INVH-DAREGDAT-PR2                 
000503                                        INVH-DAREGDAT-PR3                 
000504     MOVE SPACE                      TO INVH-IDUSER-PR1                   
000505                                        INVH-IDUSER-PR2                   
000506                                        INVH-IDUSER-PR3                   
000507     MOVE +0                         TO INVH-KVANTAL                      
000508                                                                          
000509     PERFORM IMS-ISRT-WDH711                                              
000510     ADD 1              TO WS-ISRT-WDH7                                   
000511                                                                          
000512     PERFORM UNTIL NOT SEGMENT-FOUND-EXISTS                               
000513       IF SEGMENT-FOUND-EXISTS                                            
000514         SUBTRACT 1 FROM INVH-TISEGKEY                                    
000515         PERFORM IMS-ISRT-WDH711                                          
000516       END-IF                                                             
000517     END-PERFORM                                                          
000518     .                                                                    
000519     EJECT                                                                
000520                                                                          
000521 CD-CREATE-WDR801-TRANS SECTION.                                          
000522     MOVE SPACE             TO FIL-WDR801                                 
000523     MOVE SPACE             TO FIL-WDR801-DATA                            
000524                                                                          
000525     MOVE IDPGM             TO FIL-IDPGM                                  
000526     MOVE FUNCTION CURRENT-DATE(3:6)                                      
000527                            TO FIL-TIREGDAT                               
000528     MOVE FUNCTION CURRENT-DATE(9:8)                                      
000529                            TO FIL-TIKLOCK                                
000530     MOVE +1                TO FIL-IDSEKVNR                               
000531                                                                          
000532     PERFORM IMS-GU-WDK601                                                
000534     MOVE W57105-IDARTNR    TO EKH-IDARTNR                                
000535     MOVE '403'             TO EKH-KDEKHHT                                
000536     MOVE '409'             TO EKH-KDEKSHT                                
000537     MOVE 'DET'             TO EKH-KDEKNIVA                               
000538     MOVE W-IDDC            TO EKH-IDDC-SEND                              
000539                               EKH-IDDC-REC                               
000540     MOVE ZERO              TO EKH-IDDISTR                                
000541     MOVE ZERO              TO EKH-IDKUNDNR                               
000542     MOVE W57105-IDARTNR    TO W-EKH-IDARTNR                              
000543     MOVE W57105-KDPSLLOC   TO EKH-KDPSLLOC                               
000544                                                                          
000545     MOVE 'VO'              TO CIA-IDARTPRE-IN                            
000546     MOVE W-EKH-IDARTNR     TO CIA-IDARTBET-IN                            
000547     CALL W009CIA USING CIA-W009CIA                                       
000548     MOVE CIA-IDARTBET-UT   TO EKH-IDVERGL                                
000549                                                                          
000550     MOVE WS-CURRENT-DATE   TO EKH-DAVERDAT                               
000551     MOVE ART-KDPRODSL      TO EKH-KDPRODSL                               
000552     MOVE SPACE             TO EKH-FLLSBOK                                
000553     MOVE 1.00              TO EKH-PRKURS                                 
000554     MOVE ZERO              TO EKH-PRARTNTO                               
000555     MOVE ZERO              TO EKH-PRARTSJK                               
000556     MOVE ZERO              TO EKH-PRHEMTAG                               
000557     IF SLAG-PRAVCOST NOT NUMERIC                                         
000558       MOVE 0               TO EKH-PRARTSTD                               
000559     ELSE                                                                 
000560       MOVE SLAG-PRAVCOST   TO EKH-PRARTSTD                               
000561     END-IF                                                               
000562     MOVE ZERO              TO EKH-PRLANDCO                               
000563     MOVE ZERO              TO EKH-PRINK                                  
000564     MOVE ZERO              TO EKH-PRDIRLON                               
000565     MOVE ZERO              TO EKH-PRDMTRL                                
000566     MOVE ZERO              TO EKH-PROVRPAL                               
000567     MOVE ZERO              TO EKH-SUBEL                                  
000568     IF KVJUSTKV-WS NOT NUMERIC                                           
000569       MOVE ZERO TO KVJUSTKV-WS                                           
000570     END-IF                                                               
000571     MOVE KVJUSTKV-WS       TO EKH-KVANTAL                                
000572     MOVE '    '            TO EKH-IDTRANS                                
000573     MOVE ZERO              TO EKH-BEVAT                                  
000574                               EKH-IDANALYS                               
000575                               EKH-IDKONTO                                
000576                               EKH-KDANMORS                               
000577                               EKH-KDFRAKT                                
000578                               EKH-SUVAT                                  
000579     MOVE ZERO              TO EKH-DAAVIDAT                               
000580                               EKH-IDAVINR                                
000581                               EKH-KDAVVTYP                               
000582                               EKH-KDRT                                   
000583                               EKH-KVANTMOT                               
000584                               EKH-KVAVIS                                 
000585     MOVE ART-KDSORT        TO EKH-KDSORT                                 
000586     MOVE NOO               TO EKH-FLDCET                                 
000587     MOVE SPACE             TO EKH-IDLEVNR                                
000588     MOVE SPACE             TO EKH-IDKUNDRF                               
000589                               EKH-IDKST                                  
000590     MOVE SPACE             TO EKH-IDFAKT-EXP                             
000591     MOVE DCS-KDVALISO      TO EKH-KDVALISO                               
000592     MOVE DCS-KDTRADP       TO EKH-KDTRADP                                
000593     IF NDC-CN                                                            
000594       MOVE 'W570'          TO FIL-IDCPYTXT(1:4)                          
000595     ELSE                                                                 
000596       IF NDC-IN                                                          
000597         MOVE 'W515'        TO FIL-IDCPYTXT(1:4)                          
000598       ELSE                                                               
000599         MOVE DCS-KDTRADP   TO FIL-IDCPYTXT(1:4)                          
000600       END-IF                                                             
000601     END-IF                                                               
000602     MOVE 'EKHA'            TO FIL-IDCPYTXT(5:4)                          
000603                                                                          
000604     PERFORM IMS-ISRT-WDR801                                              
000605     ADD 1                  TO WS-ISRT-WDR8                               
000606                                                                          
000607     IF SEGMENT-FOUND-EXISTS                                              
000608       PERFORM UNTIL SEGMENT-FOUND                                        
000609         ADD +1             TO FIL-IDSEKVNR                               
000610         PERFORM IMS-ISRT-WDR801                                          
000611       END-PERFORM                                                        
000612     END-IF                                                               
000614     .                                                                    
000615     EJECT                                                                
000616                                                                          
000617 CE-SKAPA-LABPOST    SECTION.                                             
000618                                                                          
000619*  SKAPAR LAB-TRANSAKTIONER                                               
000620                                                                          
000621     MOVE SPACE             TO FIL-WDR801                                 
000622     MOVE SPACE             TO A08-W510A08                                
000623                                                                          
000624     MOVE IDPGM             TO FIL-IDPGM                                  
000625     MOVE FUNCTION CURRENT-DATE(1:8) TO FIL-TIREGDAT                      
000626     ACCEPT FIL-TIKLOCK FROM TIME                                         
000627     MOVE +1                TO FIL-IDSEKVNR                               
000628     MOVE 'W510'            TO FIL-CT-IDSYSTEM                            
000629     MOVE 'A08'             TO FIL-CT-IDPTYP                              
000630     MOVE ' '               TO FIL-CT-IDVTYP                              
000631                                                                          
000632     MOVE 'A08'             TO A08-IDPTYP                                 
000633     MOVE 'M11'             TO A08-KDEKOHT                                
000634     IF DCS-FTG-US                                                        
000635       MOVE 53              TO A08-IDFTG                                  
000636     ELSE                                                                 
000637       IF DCS-FTG-CA                                                      
000638          MOVE 54           TO A08-IDFTG                                  
000639       END-IF                                                             
000640     END-IF                                                               
000641     MOVE W-IDDC            TO A08-IDDC-SEND                              
000642                               A08-IDDC-REC                               
000643     PERFORM IMS-GU-WDK601                                                
000644     PERFORM IMS-GNP-WDK611                                               
000645     MOVE FUNCTION CURRENT-DATE(1:8)                                      
000646                            TO A08-DAJUSTDA                               
000647     MOVE W-IDARTNR         TO A08-IDARTNR                                
000648     MOVE ART-KDPRODSL      TO A08-KDPRODSL                               
000649     MOVE CLAG-KDPSLLOC     TO A08-KDPSLLOC                               
000650     MOVE KVJUSTKV-WS       TO A08-KVJUSTKV                               
000651     MOVE SLAG-PRAVCOST     TO A08-PRAVCOST                               
000652     MOVE 09                TO A08-KDINVKAT                               
000653     MOVE SPACE             TO A08-TEINVANM                               
000654     MOVE A08-W510A08       TO FIL-WDR801-DATA                            
000655     PERFORM IMS-ISRT-WDR801                                              
000656     ADD 1                  TO WS-ISRT-WDR8                               
000657                                                                          
000658     IF SEGMENT-FOUND-EXISTS                                              
000659       PERFORM UNTIL SEGMENT-FOUND                                        
000660         ADD +1             TO FIL-IDSEKVNR                               
000661         PERFORM IMS-ISRT-WDR801                                          
000662       END-PERFORM                                                        
000663     END-IF                                                               
000664     .                                                                    
000665     EJECT                                                                
000666                                                                          
000667 CF-CREATE-WDR901-TRANS SECTION.                                          
000668     MOVE SPACE             TO WDR9-FIL-WDR901                            
000669     MOVE SPACE             TO WDR9-FIL-WDR901-DATA                       
000670                                                                          
000671     MOVE IDPGM             TO WDR9-FIL-IDPGM                             
000673     MOVE WS-CURRENT-DATE   TO WDR9-FIL-DAREGDAT                          
000674     MOVE FUNCTION CURRENT-DATE(9:8)                                      
000675                            TO WDR9-FIL-TIKLOCK                           
000676     MOVE +1                TO WDR9-FIL-IDSEKVNR                          
000677     MOVE 'W510EKHA'        TO WDR9-FIL-IDCPYTXT                          
000678                                                                          
000679     PERFORM IMS-GU-WDK601                                                
000680     PERFORM IMS-GNP-WDK611                                               
000682     MOVE W57105-IDARTNR    TO WDR9-EKH-IDARTNR                           
000683     MOVE '403'             TO WDR9-EKH-KDEKHHT                           
000684     MOVE '409'             TO WDR9-EKH-KDEKSHT                           
000685     MOVE 'DET'             TO WDR9-EKH-KDEKNIVA                          
000686     MOVE W-IDDC            TO WDR9-EKH-IDDC-SEND                         
000687                               WDR9-EKH-IDDC-REC                          
000688     MOVE ZERO              TO WDR9-EKH-IDDISTR                           
000689     MOVE ZERO              TO WDR9-EKH-IDKUNDNR                          
000690     MOVE W57105-IDARTNR    TO W-EKH-IDARTNR                              
000691     MOVE W57105-KDPSLLOC   TO WDR9-EKH-KDPSLLOC                          
000692                                                                          
000693     MOVE 'VO'              TO CIA-IDARTPRE-IN                            
000694     MOVE W-EKH-IDARTNR     TO CIA-IDARTBET-IN                            
000695     CALL W009CIA USING CIA-W009CIA                                       
000696     MOVE CIA-IDARTBET-UT   TO WDR9-EKH-IDVERGL                           
000697                                                                          
000698     MOVE WS-CURRENT-DATE   TO WDR9-EKH-DAVERDAT                          
000699     MOVE ART-KDPRODSL      TO WDR9-EKH-KDPRODSL                          
000700     MOVE SPACE             TO WDR9-EKH-FLLSBOK                           
000701     MOVE 1.00              TO WDR9-EKH-PRKURS                            
000702     MOVE ZERO              TO WDR9-EKH-PRARTNTO                          
000703     MOVE ZERO              TO WDR9-EKH-PRARTSJK                          
000704     MOVE ZERO              TO WDR9-EKH-PRHEMTAG                          
000705     MOVE CLAG-PRARTSTD     TO WDR9-EKH-PRARTSTD                          
000709     MOVE ZERO              TO WDR9-EKH-PRLANDCO                          
000710     MOVE ZERO              TO WDR9-EKH-PRINK                             
000711     MOVE ZERO              TO WDR9-EKH-PRDIRLON                          
000712     MOVE ZERO              TO WDR9-EKH-PRDMTRL                           
000713     MOVE ZERO              TO WDR9-EKH-PROVRPAL                          
000714     MOVE ZERO              TO WDR9-EKH-SUBEL                             
000715     IF KVJUSTKV-WS NOT NUMERIC                                           
000716       MOVE ZERO TO KVJUSTKV-WS                                           
000717     END-IF                                                               
000718     MOVE KVJUSTKV-WS       TO WDR9-EKH-KVANTAL                           
000719     MOVE '    '            TO WDR9-EKH-IDTRANS                           
000720     MOVE ZERO              TO WDR9-EKH-BEVAT                             
000721                               WDR9-EKH-IDANALYS                          
000722                               WDR9-EKH-IDKONTO                           
000723                               WDR9-EKH-KDANMORS                          
000724                               WDR9-EKH-KDFRAKT                           
000725                               WDR9-EKH-SUVAT                             
000726     MOVE ZERO              TO WDR9-EKH-DAAVIDAT                          
000727                               WDR9-EKH-IDAVINR                           
000728                               WDR9-EKH-KDAVVTYP                          
000729                               WDR9-EKH-KDRT                              
000730                               WDR9-EKH-KVANTMOT                          
000731                               WDR9-EKH-KVAVIS                            
000732     MOVE ART-KDSORT        TO WDR9-EKH-KDSORT                            
000733     MOVE NOO               TO WDR9-EKH-FLDCET                            
000734     MOVE SPACE             TO WDR9-EKH-IDLEVNR                           
000735     MOVE SPACE             TO WDR9-EKH-IDKUNDRF                          
000736                               WDR9-EKH-IDKST                             
000737     MOVE SPACE             TO WDR9-EKH-IDFAKT-EXP                        
000738     MOVE 'SEK'             TO WDR9-EKH-KDVALISO                          
000739     MOVE DCS-KDTRADP       TO WDR9-EKH-KDTRADP                           
000749                                                                          
000750     PERFORM IMS-ISRT-WDR901                                              
000751     ADD 1                  TO WS-ISRT-WDR9                               
000752                                                                          
000753     IF SEGMENT-FOUND-EXISTS                                              
000754       PERFORM UNTIL SEGMENT-FOUND                                        
000755         ADD +1             TO WDR9-FIL-IDSEKVNR                          
000756         PERFORM IMS-ISRT-WDR901                                          
000757       END-PERFORM                                                        
000758     END-IF                                                               
000760     .                                                                    
000761     EJECT                                                                
000762                                                                          
000763 D-DELETE-DUMMY-RECORD SECTION.                                           
000764     MOVE SPACES                 TO W-WDJ701-IDDC                         
000765     IF WS-SAVE-IDDC NOT = SPACES                                         
000766        MOVE WS-SAVE-IDDC        TO W-WDJ701-IDDC                         
000767     ELSE                                                                 
000768        MOVE W57105-IDDC         TO W-WDJ701-IDDC                         
000769     END-IF                                                               
000770     MOVE +888888888             TO W-WDJ701-IDARTNR                      
000771     PERFORM IMS-GHU-WDJ701                                               
000772     IF SEGMENT-FOUND                                                     
000773        PERFORM IMS-DLET-WDJ701                                           
000774     END-IF                                                               
000775     .                                                                    
000776                                                                          
000777 Z-FINIT SECTION.                                                         
000778     CLOSE W571D1                                                         
000779                                                                          
000780     MOVE 'S' TO POSTSUM-OPKOD                                            
000781     CALL POSTSUM USING POSTSUM-PARM                                      
000782     DISPLAY 'NUMBER OF REPL WDK711-SEGM ' WS-REPL-WDK711                 
000783     DISPLAY 'NUMBER OF ISRT WDR8   SEGM ' WS-ISRT-WDR8                   
000784     DISPLAY 'NUMBER OF ISRT WDR9   SEGM ' WS-ISRT-WDR9                   
000785     DISPLAY 'NUMBER OF ISRT WDH7   SEGM ' WS-ISRT-WDH7                   
000786     DISPLAY 'NUMBER OF ISRT WDL9   SEGM ' WS-ISRT-WDL9                   
000787     .                                                                    
000788     EJECT                                                                
000789                                                                          
000790 S01-READ-W571D1  SECTION.                                                
000791     SKIP2                                                                
000792     READ W571D1 INTO W57105-AREA                                         
000793     AT END                                                               
000794        SET END-OF-W571D1 TO TRUE                                         
000795                                                                          
000796     NOT AT END                                                           
000797        MOVE 'W571D1' TO POSTSUM-FDNAMN                                   
000798        MOVE 'W57106D1' TO POSTSUM-DDNAMN2                                
000799        CALL POSTSUM USING POSTSUM-PARM                                   
000800     END-READ                                                             
000801     .                                                                    
000802     EJECT                                                                
000803                                                                          
000804 X-TAKE-CHECKPOINT   SECTION.                                             
000805     PERFORM IMS-CHECKPOINT                                               
000806     MOVE ZERO TO CHKP-ANT                                                
000807     .                                                                    
000808     EJECT                                                                
000809* --- IMS SECTIONS  ---                                                   
000810                                                                          
000811 IMS-RESTART SECTION.                                                     
000812     SKIP2                                                                
000813     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
000814     MOVE '  ' TO GOOD-STATUSCODES                                        
000815     CALL CBLTDLI USING XRST MSG-PCB                                      
000816                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
000817                        CHKP-AREA-LENGTH CHKP-AREA                        
000818     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
000819     PERFORM IMS-STATUSCHECK                                              
000820     .                                                                    
000821     SKIP3                                                                
000822                                                                          
000823 IMS-CHECKPOINT SECTION.                                                  
000824     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
000825     MOVE '  XD' TO GOOD-STATUSCODES                                      
000826     CALL CBLTDLI USING CHKP MSG-PCB                                      
000827                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
000828                        CHKP-AREA-LENGTH CHKP-AREA                        
000829     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
000830     PERFORM IMS-STATUSCHECK                                              
000831                                                                          
000832     IF IMS-NOT-OK                                                        
000833       MOVE 'IMS CONTROL REGION IS NOT ACCESSIBLE' TO                     
000834                             ERROR-TEXT-STR                               
000835       DISPLAY ERROR-TEXT                                                 
000836       CALL FELLOG                                                        
000837     END-IF                                                               
000838     .                                                                    
000839     EJECT                                                                
000840                                                                          
000841 IMS-GU-WDK601 SECTION.                                                   
000842     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
000843          DELIMITED BY SIZE INTO SSA1                                     
000844     MOVE '  ' TO GOOD-STATUSCODES                                        
000845     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
000846     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
000847     PERFORM IMS-STATUSCHECK                                              
000848     .                                                                    
000849     EJECT                                                                
000850                                                                          
000851 IMS-GNP-WDK611 SECTION.                                                  
000852     MOVE SPACES TO SSA1                                                  
000853     STRING 'WDK611  ' DELIMITED BY SIZE INTO SSA1                        
000854     MOVE '  ' TO GOOD-STATUSCODES                                        
000855     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
000856     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
000857     PERFORM IMS-STATUSCHECK                                              
000858     .                                                                    
000859     EJECT                                                                
000860                                                                          
000861 IMS-GHU-WDH701 SECTION.                                                  
000862     STRING 'WDH701  (IDARTNR  =' W-IDARTNR-X ')'                         
000863          DELIMITED BY SIZE INTO SSA1                                     
000864     MOVE '  GE' TO GOOD-STATUSCODES                                      
000865     CALL CBLTDLI USING GU WDH7-PCB DLI-IO-WDH701 SSA1                    
000866     MOVE WDH7-STATUS-CODE TO STATUS-WS                                   
000867     PERFORM IMS-STATUSCHECK                                              
000868     .                                                                    
000869     EJECT                                                                
000870                                                                          
000871 IMS-GHU-WDJ701 SECTION.                                                  
000872     STRING  'WDJ701  (WDJ701KY =' W-WDJ701-X ')'                         
000873              DELIMITED BY SIZE INTO SSA1                                 
000874     MOVE '  GE'           TO GOOD-STATUSCODES                            
000875     CALL CBLTDLI USING GHU  WDJ7-PCB DLI-IO-WDJ701 SSA1                  
000876     MOVE WDJ7-STATUS-CODE TO STATUS-WS                                   
000877     PERFORM IMS-STATUSCHECK                                              
000878     .                                                                    
000879                                                                          
000880 IMS-DLET-WDJ701 SECTION.                                                 
000881     MOVE '  ' TO GOOD-STATUSCODES                                        
000882     CALL CBLTDLI USING DLET WDJ7-PCB DLI-IO-WDJ701                       
000883     MOVE WDJ7-STATUS-CODE TO STATUS-WS                                   
000884     PERFORM IMS-STATUSCHECK                                              
000885     ADD +1 TO CHKP-ANT                                                   
000886     .                                                                    
000887                                                                          
000888 IMS-ISRT-WDH701 SECTION.                                                 
000889     MOVE 'WDH701  '  TO SSA1                                             
000890     MOVE '  ' TO GOOD-STATUSCODES                                        
000891     CALL  CBLTDLI  USING ISRT WDH7-PCB DLI-IO-WDH701 SSA1                
000892     MOVE WDH7-STATUS-CODE TO STATUS-WS                                   
000893     PERFORM IMS-STATUSCHECK                                              
000894     ADD +1 TO CHKP-ANT                                                   
000895     .                                                                    
000896                                                                          
000897 IMS-ISRT-WDH711 SECTION.                                                 
000898     MOVE 'WDH711  '  TO SSA1                                             
000899     MOVE '  II' TO GOOD-STATUSCODES                                      
000900     CALL  CBLTDLI  USING ISRT WDH7-PCB DLI-IO-WDH711 SSA1                
000901     MOVE WDH7-STATUS-CODE TO STATUS-WS                                   
000902     PERFORM IMS-STATUSCHECK                                              
000903     ADD +1 TO CHKP-ANT                                                   
000904     .                                                                    
000905                                                                          
000906 IMS-ISRT-WDR801 SECTION.                                                 
000907     MOVE 'WDR801  ' TO SSA1                                              
000908     MOVE '  II' TO GOOD-STATUSCODES                                      
000909     CALL CBLTDLI USING ISRT WDR8-PCB DLI-IO-WDR801 SSA1                  
000910     MOVE WDR8-STATUS-CODE TO STATUS-WS                                   
000911     PERFORM IMS-STATUSCHECK                                              
000912     ADD +1 TO CHKP-ANT                                                   
000913     .                                                                    
000914     SKIP3                                                                
000915                                                                          
000916 IMS-ISRT-WDJ701 SECTION.                                                 
000917     MOVE 'WDJ701  ' TO SSA1                                              
000918     MOVE '  II' TO GOOD-STATUSCODES                                      
000919     CALL CBLTDLI USING ISRT WDJ7-PCB DLI-IO-WDJ701 SSA1                  
000920     MOVE WDJ7-STATUS-CODE TO STATUS-WS                                   
000921     PERFORM IMS-STATUSCHECK                                              
000922     ADD +1 TO CHKP-ANT                                                   
000923     .                                                                    
000924     SKIP3                                                                
000925                                                                          
000926 IMS-GHU-WDK711 SECTION.                                                  
000927     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
000928            DELIMITED BY SIZE INTO SSA1                                   
000929     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
000930            DELIMITED BY SIZE INTO SSA2                                   
000931     MOVE '  GE' TO GOOD-STATUSCODES                                      
000932     CALL  CBLTDLI  USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2            
000933     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
000934     PERFORM IMS-STATUSCHECK                                              
000935     .                                                                    
000936     SKIP2                                                                
000937                                                                          
000938 IMS-REPL-WDK711 SECTION.                                                 
000939     MOVE '  ' TO GOOD-STATUSCODES                                        
000940     CALL  CBLTDLI  USING REPL WDK7-PCB DLI-IO-WDK711                     
000941     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
000942     PERFORM IMS-STATUSCHECK                                              
000943     ADD +1 TO CHKP-ANT                                                   
000944     .                                                                    
000945                                                                          
000946 IMS-ISRT-WDL901 SECTION.                                                 
000947     MOVE 'WDL901  ' TO SSA1                                              
000948     MOVE '  II' TO GOOD-STATUSCODES                                      
000949     CALL CBLTDLI USING ISRT WDL9-PCB DLI-IO-WDL901 SSA1                  
000950     MOVE WDL9-STATUS-CODE TO STATUS-WS                                   
000951     PERFORM IMS-STATUSCHECK                                              
000952     ADD +1 TO CHKP-ANT                                                   
000953     .                                                                    
000954     SKIP3                                                                
000955                                                                          
000956 IMS-GU-WDGX5104 SECTION.                                                 
000957     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-5104-X ')'                    
000958          DELIMITED BY SIZE INTO SSA1                                     
000959     STRING 'WDGX5104(IDDC     =' W-IDDC-5104-X ')'                       
000960          DELIMITED BY SIZE INTO SSA2                                     
000961     MOVE '  GE' TO GOOD-STATUSCODES                                      
000962     CALL CBLTDLI USING GU 5104-PCB DLI-IO-WDGX5104 SSA1 SSA2             
000963     MOVE 5104-STATUS-CODE    TO STATUS-WS                                
000964     PERFORM IMS-STATUSCHECK                                              
000965     .                                                                    
000966                                                                          
000967 IMS-GU-WDB601 SECTION.                                                   
000968                                                                          
000969     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
000970          DELIMITED BY SIZE INTO SSA1                                     
000971     MOVE '  GE' TO GOOD-STATUSCODES                                      
000972     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
000973     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
000974     PERFORM IMS-STATUSCHECK                                              
000975     .                                                                    
000976     SKIP3                                                                
000977 IMS-ISRT-WDR901 SECTION.                                                 
000978                                                                          
000979     MOVE 'WDR901  ' TO SSA1                                              
000980     MOVE '  II' TO GOOD-STATUSCODES                                      
000981     CALL CBLTDLI USING ISRT WDR9-PCB WDR9-DLI-IO-WDR901 SSA1             
000982     MOVE WDR9-STATUS-CODE TO STATUS-WS                                   
000983     PERFORM IMS-STATUSCHECK                                              
000984     .                                                                    
000985     SKIP3                                                                
000986                                                                          
000987 IMS-STATUSCHECK SECTION.                                                 
000988     SET STATUS-IX TO 1                                                   
000989     SEARCH GOOD-STATUS                                                   
000990       AT END                                                             
000991         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
000992           DELIMITED BY SIZE INTO ERROR-TEXT                              
000993         DISPLAY ERROR-TEXT                                               
000994         CALL FELLOG                                                      
000995       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
000996         CONTINUE                                                         
000997     END-SEARCH                                                           
001000     .                                                                    
