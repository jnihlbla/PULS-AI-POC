001300 ID DIVISION.                                                             
001400                                                                          
001500 PROGRAM-ID.     W4070300.                                                
001600 AUTHOR.         LARS CALAIS.                                             
001700 DATE-WRITTEN.   95/10/24.                                                
001800 DATE-COMPILED.                                                           
001900                                                                          
002000*    FUNCTION:                                                            
002100*        ORDER LIST TO BE PRINTED VIA SOP.                                
002200*                                                                         
002310*        THE PROGRAM READS     WLARTC (WDK6)                              
002320*                              WDB6                                       
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSACTION: W4T703                                              
002700*        MID:         W4I70301                                            
002800*                                                                         
002900*    OUTDATA.                                                             
003000*        MOD:         W4O70301                                            
003100                                                                          
003200                                                                          
003300 ENVIRONMENT DIVISION.                                                    
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600 WORKING-STORAGE SECTION.                                                 
003601*    -- CHECKED BY WY2000                                                 
003610     SKIP3                                                                
003700 77  IDPGM                       PIC X(08)   VALUE 'W4070300'.            
003800                                                                          
003900*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004000 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
004100                                                                          
004200 77  YES                         PIC X       VALUE 'Y'.                   
004300 77  NOO                         PIC X       VALUE 'N'.                   
004301 77  WS-IDARTNR                  PIC X(9).                                
004303 77  WS-IDFTG                    PIC X(2).                                
004304 77  WS-IDDISTR                  PIC X(4).                                
004330 77  WS-IDKUNDNR                 PIC X(6).                                
004331 77  WS-IDLISTTYP                PIC X(3).                                
004340 77  WS-KDANMORS                 PIC X(2).                                
004350 77  WS-TIAAPP-FOM               PIC X(4).                                
004360 77  WS-TIAAPP-TOM               PIC X(4).                                
004361 77  WS-TIAAMMDD-FOM             PIC X(6).                                
004362 77  WS-TIAAMMDD-TOM             PIC X(6).                                
004370 77  WS-IDNODE                   PIC X(8).                                
004400                                                                          
004600*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
005000                                                                          
005320 77  KEYS-SW                     PIC X       VALUE 'J'.                   
005330     88  KEYS-OK                             VALUE 'Y'.                   
005340     88  KEYS-WRONG                          VALUE 'N'.                   
005400                                                                          
005410 77  INPUT-SW                    PIC X       VALUE 'Y'.                   
005420     88  INPUT-OK                            VALUE 'Y'.                   
005430     88  INPUT-WRONG                         VALUE 'N'.                   
005440                                                                          
005500 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005600     88  OWN-MID                             VALUE '4703'.                
006200     88  HELP-MID                            VALUE '0551'.                
006300     EJECT                                                                
006400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006500 01  GENERAL-SUBPROGRAM.                                                  
006600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007000     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
007100     EJECT                                                                
007200*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
007300*01 -COPY WMEDAREA                                                        
007400                                                                          
007410*    --- PARAMETERS FOR SUBPROGRAM W006PRT                                
007420*01 -COPY W006PRT                                                         
007430                                                                          
007500 01  MESSAGE-CODES.                                                       
007800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007900     03  ERR-PARTNO-MISSING      PIC X(3)    VALUE '769'.                 
007920     03  ERR-HIGHL-FIELDS        PIC X(3)    VALUE '409'.                 
007930     03  INF-LIST-QUEUED         PIC X(3)    VALUE '246'.                 
008000     EJECT                                                                
008100*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
008200*                                                                         
008300 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008400                                                                          
008500*01 -COPY WMSGINIT                                                        
008600                                                                          
008700*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
008800*                                                                         
008900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009000                                                                          
009100*01  MID -COPY W4I70301                                                   
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009400                                                                          
009500*01  -COPY WMSGAREA                                                       
009600     EJECT                                                                
009700     03  MOD REDEFINES MSG-AREA.                                          
009800*      05  -COPY W4O70301                                                 
009900     EJECT                                                                
010000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010100                                                                          
010200*01  -COPY WMFSAREA                                                       
010300     EJECT                                                                
010301 01  FILLER                      PIC X(16)  VALUE 'WMSGSOP-AREA'.         
010310 01    W-PROG-TO-PROG-SW.                                                 
010320*  03    -COPY WMSGSOP                                                    
010330     EJECT                                                                
010400*    --- WORK-AREAS FOR IMS-SECTIONS                                      
010500*                                                                         
010600     EJECT                                                                
010700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011100                                                                          
011200 01  KEYS-TO-DLI.                                                         
011301     03  W-IDARTNR-X.                                                     
011310         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
011400                                                                          
011410     03  W-IDDC-B6-X.                                                     
011420         05 W-IDDC-B6            PIC X(2).                                
011430                                                                          
011500*    --- STATUS-KOD FRÅN IMS                                              
011600 01  STATUS-WS                   PIC XX.                                  
011700     88  SEGMENT-FOUND                       VALUE '  '.                  
011800     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
011900     88  SEGMENT-MISSING                     VALUE 'GE'.                  
012000                                                                          
012100 01  GOOD-STATUSCODES.                                                    
012200     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012300                                                                          
012400 01  SSA1                        PIC X(64).                               
012500 01  SSA2                        PIC X(64).                               
012600     EJECT                                                                
012700*    --- IMS FUNCTION CODES                                               
012800*01  -COPY W0003                                                          
013000     EJECT                                                                
013100*    ---  DLI INPUT-OUTPUT AREA                                           
013200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
013300                                                                          
013400 01  DLI-IO-AREA.                                                         
013500     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
013601                                                                          
013602     03  WLARTC01 REDEFINES IO-AREA.                                      
013610*        05  -COPY WDK601                                                 
013620                                                                          
013630 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
013640 01   DLI-IO-AREA-B601.                                                   
013650*     03  -COPY WDB601                                                    
013660                                                                          
013900     EJECT                                                                
014000 LINKAGE SECTION.                                                         
014100                                                                          
014200*01  -COPY W0009   -PRE MSG-                                              
014201     EJECT                                                                
014210*01  -COPY W0009   -PRE ALT-                                              
014220     EJECT                                                                
014300*01  -COPY W0008   -PRE USEA-                                             
014400     05  FILLER                  PIC X.                                   
014501     EJECT                                                                
014502*01  -COPY W0008  -PRE ARTC-                                              
014510     05  FILLER                  PIC X.                                   
014600     EJECT                                                                
014700*01  -COPY W0008  -PRE WDB6-                                              
014701     05  FILLER                  PIC X.                                   
014702     EJECT                                                                
014703 PROCEDURE DIVISION  USING MSG-PCB                                        
014704                           ALT-PCB                                        
014705                           USEA-PCB                                       
014706                           ARTC-PCB                                       
014707                           WDB6-PCB.                                      
014708 MAIN SECTION.                                                            
014710     ENTRY 'DLITCBL' USING MSG-PCB                                        
014720                           ALT-PCB                                        
014730                           USEA-PCB                                       
014740                           ARTC-PCB                                       
014750                           WDB6-PCB.                                      
014800                                                                          
015000     PERFORM IMS-GET-MSG                                                  
015100     IF SEGMENT-FOUND                                                     
015200       PERFORM A-INIT                                                     
015300       PERFORM B-CHECK-KEYS                                               
015301       IF KEYS-OK                                                         
015310          IF MFS-FIRST OR MFS-ENTER                                       
015320             CONTINUE                                                     
016100          ELSE                                                            
016101             PERFORM C-CHECK-DATA-VALUES                                  
016110             IF INPUT-OK AND OWN-MID                                      
016120               PERFORM D-ORDER-LIST                                       
016130             ELSE                                                         
016131                MOVE ERR-HIGHL-FIELDS   TO MED-IDMFSFEL                   
016133             END-IF                                                       
016140          END-IF                                                          
016150       ELSE                                                               
016151          IF SEGMENT-MISSING                                              
016152             MOVE ERR-PARTNO-MISSING    TO MED-IDMFSFEL                   
016155          ELSE                                                            
016156             MOVE ERR-WRONG-KEY         TO MED-IDMFSFEL                   
016159          END-IF                                                          
016160       END-IF                                                             
016300     END-IF                                                               
016500                                                                          
016510     PERFORM Z-FINIT                                                      
016520                                                                          
016600     MOVE ZERO                          TO RETURN-CODE                    
016700     GOBACK                                                               
016800     .                                                                    
016900     EJECT                                                                
016920                                                                          
017000 A-INIT                         SECTION.                                  
017100                                                                          
017200     IF MSG-DOUBLE-TRANSACTIONS                                           
017300       MOVE MSG-INDATA-MINUS-2-TRANSACT TO MID-W4I70301                   
017400       MOVE MSG-IDTRANS-2               TO MFS-IDTRANS                    
017500       MOVE MSG-KDMFSFOR-2              TO MFS-KDMFSFOR                   
017600     ELSE                                                                 
017700       MOVE MSG-INDATA-MINUS-1-TRANSACT TO MID-W4I70301                   
017800       MOVE MSG-IDTRANS-1               TO MFS-IDTRANS                    
017900       MOVE MSG-KDMFSFOR-1              TO MFS-KDMFSFOR                   
018000     END-IF                                                               
018100                                                                          
018101     MOVE '4703'                        TO MSGSOP-IDTRANS                 
018102     MOVE MFS-KDMFSFOR                  TO MSGSOP-KDMFSFOR                
018103     MOVE 'W428S1'                      TO MSGSOP-IDPROCESS               
018104     MOVE 'O'                           TO MSGSOP-KDSOPFUNK               
018110                                                                          
018200     MOVE MSG-KDTRTYP                   TO MFS-KDTRTYP                    
018300     MOVE MSG-IDPFK                     TO MFS-IDPFK                      
018400     MOVE MFS-IDTRANS                   TO W-IDTRANS                      
018500                                                                          
018600     MOVE LOW-VALUE                     TO MSG-AREA                       
018700     MOVE 'W4O70301'                    TO MFS-IDMOD                      
018800     MOVE '4703'                        TO MOD-IDTRANS                    
018900     MOVE SPACE                         TO MOD-TEMFSFEL                   
018910                                           MOD-TEMFSINF                   
018920                                           MED-IDMFSFEL                   
018930                                           MED-IDMFSINF                   
019000                                                                          
019300     COMPUTE MSG-KVLL = LENGTH OF MOD-W4O70301 + 4                        
019400                                                                          
019500     IF OWN-MID OR HELP-MID                                               
019600       CONTINUE                                                           
019700     ELSE                                                                 
019800       MOVE SPACE                       TO MFS-KDTRTYP                    
019900       MOVE '7'                         TO MFS-IDPFK                      
020000     END-IF                                                               
020100                                                                          
020200     MOVE ALL '+'                       TO MSGI-WMSGINIT                  
020210     MOVE '001'                         TO MSGI-KDCALL                    
020220     MOVE MSG-SIGNON-USERID             TO MSGI-IDUSER                    
020230     MOVE '4703'                        TO MSGI-IDTRANS                   
020240     MOVE MSG-LTERM-NAME                TO MSGI-IDLTERM-USER              
020250     IF OWN-MID                                                           
020260         MOVE MID-IDARTNR-IN            TO MSGI-IDARTNR                   
020280     END-IF                                                               
020290     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
020291                                                                          
020294     MOVE MSGI-IDFTG                    TO WS-IDFTG                       
020295                                                                          
020297     IF MSGI-IDLAND-SPR = 'GB'                                            
020298       MOVE 'GB '                       TO MED-IDSKYLT                    
020299     ELSE                                                                 
020301       MOVE 'S  '                       TO MED-IDSKYLT                    
020302     END-IF                                                               
020303                                                                          
020304     PERFORM MFS-ERASE-FIELD-IN                                           
020305     PERFORM MFS-ERASE-FIELD-OUT                                          
020310     .                                                                    
020400     EJECT                                                                
020500 B-CHECK-KEYS                   SECTION.                                  
021705                                                                          
021706     MOVE YES                           TO KEYS-SW                        
021707                                                                          
021708*    -- CHECK OF IDARTNR                                                  
021709     IF MID-IDARTNR-IN NOT = ALL '+'                                      
021710       MOVE '7'                         TO MFS-IDPFK                      
021711       MOVE SPACE                       TO MFS-KDTRTYP                    
021712     END-IF                                                               
021713                                                                          
021714     IF MSGI-IDARTNR NUMERIC                                              
021715       MOVE MSGI-IDARTNR                TO W-IDARTNR                      
021716                                           WS-IDARTNR                     
021717                                           MOD-IDARTNR-UT                 
021718       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
021719     ELSE                                                                 
021720       MOVE NOO                         TO KEYS-SW                        
021730     END-IF                                                               
021731                                                                          
021740     PERFORM IMS-GET-ARTC01                                               
021750     IF SEGMENT-MISSING                                                   
021760       MOVE NOO                         TO KEYS-SW                        
021770     END-IF                                                               
021801                                                                          
021802*    -- CHECK OF IDDC                                                     
021804     IF MID-IDDC-IN NOT = ALL '+' AND OWN-MID                             
021806       MOVE '7'                         TO MFS-IDPFK                      
021807       MOVE SPACE                       TO MFS-KDTRTYP                    
021810     END-IF                                                               
021811                                                                          
021812     IF MID-IDDC-IN NOT = ALL '+'                                         
021813        MOVE MID-IDDC-IN                TO W-IDDC-B6                      
021814                                           MOD-IDDC-UT                    
021816     ELSE                                                                 
021817        MOVE MID-IDDC-UT                TO W-IDDC-B6                      
021818                                           MOD-IDDC-UT                    
021820     END-IF                                                               
021821     PERFORM IMS-GU-WDB601                                                
021822                                                                          
021824     IF DCS-KDDC = SPACE OR DCS-CDC-TR OR DCS-NDC-PF                      
021827       MOVE NOO                         TO KEYS-SW                        
021830     END-IF                                                               
022700     .                                                                    
022900     EJECT                                                                
023100 C-CHECK-DATA-VALUES           SECTION.                                   
023101                                                                          
023102     MOVE YES                           TO INPUT-SW                       
023103                                                                          
023104     PERFORM CAA-CHECK-LISTA                                              
023105     PERFORM CAB-CHECK-DISTR                                              
023106     PERFORM CAC-CHECK-KUNDNR                                             
023107     PERFORM CAD-CHECK-ANMORS                                             
023108     PERFORM CAE-CHECK-DATE                                               
023109     PERFORM CAF-CHECK-NODE                                               
023110     .                                                                    
023111     EJECT                                                                
023112                                                                          
023113 CAA-CHECK-LISTA                SECTION.                                  
023114                                                                          
023135     IF MID-TIAAMMDD-FOM NOT = ALL '+' AND                                
023136        MID-TIAAMMDD-TOM NOT = ALL '+' AND                                
023137        MID-TIAAPP-TOM       = ALL '+'                                    
023138        MOVE 'INV'                      TO WS-IDLISTTYP                   
023141     ELSE                                                                 
023142       IF MID-TIAAMMDD-FOM   = ALL '+' AND                                
023143          MID-TIAAMMDD-TOM   = ALL '+' AND                                
023144          MID-TIAAPP-TOM NOT = ALL '+'                                    
023145          MOVE 'FOR'                    TO WS-IDLISTTYP                   
023148       ELSE                                                               
023149          MOVE 'FEL'                    TO WS-IDLISTTYP                   
023151       END-IF                                                             
023152     END-IF                                                               
023153     .                                                                    
023154     EJECT                                                                
023155                                                                          
023203 CAB-CHECK-DISTR                SECTION.                                  
023204                                                                          
023205     IF MID-IDDISTR-IN = ALL '+'                                          
023206       MOVE MID-IDDISTR-UT              TO WS-IDDISTR                     
023207       INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                 
023208     ELSE                                                                 
023209       MOVE MID-IDDISTR-IN              TO WS-IDDISTR                     
023210     END-IF                                                               
023211                                                                          
023212     IF WS-IDDISTR NOT NUMERIC                                            
023213       MOVE NOO                         TO INPUT-SW                       
023214     ELSE                                                                 
023215       MOVE WS-IDDISTR                  TO MOD-IDDISTR-UT                 
023216       INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE             
023217     END-IF                                                               
023218     .                                                                    
023219     EJECT                                                                
023220                                                                          
023221 CAC-CHECK-KUNDNR               SECTION.                                  
023222                                                                          
023223     IF MID-IDKUNDNR-IN = ALL '+'                                         
023224       MOVE MID-IDKUNDNR-UT             TO WS-IDKUNDNR                    
023225       INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO                
023226     ELSE                                                                 
023227       MOVE MID-IDKUNDNR-IN             TO WS-IDKUNDNR                    
023228     END-IF                                                               
023229     IF WS-IDKUNDNR NOT NUMERIC                                           
023230       MOVE NOO                         TO INPUT-SW                       
023231     ELSE                                                                 
023232       MOVE WS-IDKUNDNR                 TO MOD-IDKUNDNR-UT                
023233       INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE            
023234     END-IF                                                               
023235     .                                                                    
023236     EJECT                                                                
023237                                                                          
023238 CAD-CHECK-ANMORS               SECTION.                                  
023239                                                                          
023240     IF MID-KDANMORS-IN = ALL '+'                                         
023241       MOVE MID-KDANMORS-UT             TO WS-KDANMORS                    
023242     ELSE                                                                 
023243       MOVE MID-KDANMORS-IN             TO WS-KDANMORS                    
023244     END-IF                                                               
023245     MOVE WS-KDANMORS                   TO MOD-KDANMORS-UT                
023246     IF WS-KDANMORS NOT NUMERIC                                           
023247       MOVE 'XX'                        TO WS-KDANMORS                    
023248     END-IF                                                               
023249     .                                                                    
023250     EJECT                                                                
023251                                                                          
023252 CAE-CHECK-DATE                 SECTION.                                  
023253                                                                          
023254     IF MID-TIAAPP-TOM NOT = ALL '+'                                      
023262       IF MID-TIAAPP-TOM NOT NUMERIC OR                                   
023263          MID-TIAAPP-TOM (3:2) > '12'                                     
023264         MOVE NOO                       TO INPUT-SW                       
023265         MOVE MFS-ALFA-FAELT-FEL        TO MOD-TIAAPP-TOM-ATTR            
023266         MOVE MFS-DO-NOT-TOUCH-FIELD    TO MOD-TIAAPP-TOM                 
023267       ELSE                                                               
023268         MOVE MID-TIAAPP-TOM            TO WS-TIAAPP-TOM                  
023269         MOVE MFS-ALPHA-FIELD-OK        TO MOD-TIAAPP-TOM-ATTR            
023270         MOVE MFS-DO-NOT-TOUCH-FIELD    TO MOD-TIAAPP-TOM                 
023271       END-IF                                                             
023272     ELSE                                                                 
023273         MOVE '++++'                    TO WS-TIAAPP-TOM                  
023274     END-IF                                                               
023275                                                                          
023276     IF MID-TIAAMMDD-FOM NOT = ALL '+'                                    
023281       IF MID-TIAAMMDD-FOM NOT NUMERIC                                    
023282         MOVE NOO                       TO INPUT-SW                       
023283         MOVE MFS-ALFA-FAELT-FEL        TO MOD-TIAAMMDD-FOM-ATTR          
023284         MOVE MFS-DO-NOT-TOUCH-FIELD    TO MOD-TIAAMMDD-FOM               
023285       ELSE                                                               
023286         MOVE MID-TIAAMMDD-FOM          TO WS-TIAAMMDD-FOM                
023287         MOVE MFS-ALPHA-FIELD-OK        TO MOD-TIAAMMDD-FOM-ATTR          
023288         MOVE MFS-DO-NOT-TOUCH-FIELD    TO MOD-TIAAMMDD-FOM               
023289       END-IF                                                             
023290     ELSE                                                                 
023291         MOVE '++++++'                  TO WS-TIAAMMDD-FOM                
023293     END-IF                                                               
023299                                                                          
023300     IF MID-TIAAMMDD-TOM NOT = ALL '+'                                    
023301       IF MID-TIAAMMDD-TOM NOT NUMERIC                                    
023302         MOVE NOO                       TO INPUT-SW                       
023303         MOVE MFS-ALFA-FAELT-FEL        TO MOD-TIAAMMDD-TOM-ATTR          
023304         MOVE MFS-DO-NOT-TOUCH-FIELD    TO MOD-TIAAMMDD-TOM               
023305       ELSE                                                               
023306         MOVE MID-TIAAMMDD-TOM          TO WS-TIAAMMDD-TOM                
023307         MOVE MFS-ALPHA-FIELD-OK        TO MOD-TIAAMMDD-TOM-ATTR          
023308         MOVE MFS-DO-NOT-TOUCH-FIELD    TO MOD-TIAAMMDD-TOM               
023309       END-IF                                                             
023310     ELSE                                                                 
023311         MOVE '++++++'                  TO WS-TIAAMMDD-TOM                
023312     END-IF                                                               
023313     .                                                                    
023314     EJECT                                                                
023315                                                                          
023316 CAF-CHECK-NODE                 SECTION.                                  
023317                                                                          
023318     IF MID-IDNODE     = ALL '+'                                          
023319       MOVE NOO                         TO INPUT-SW                       
023320       MOVE MFS-ALFA-FAELT-FEL          TO MOD-IDNODE-ATTR                
023321       MOVE MFS-DO-NOT-TOUCH-FIELD      TO MOD-IDNODE                     
023322     ELSE                                                                 
023323       MOVE '004'                       TO PRT-KDCALL                     
023324*      MOVE SPACE                       TO PRT-IDPRTLST                   
023325*      MOVE '4LALA1'                    TO PRT-IDPRTLST(1:6)              
023326       MOVE MID-IDNODE                  TO PRT-IDNODE                     
023327       CALL W006PRT USING PRT-W006PRT                                     
023328       IF PRT-KDSVAR = 'R'                                                
023329         MOVE MID-IDNODE                TO WS-IDNODE                      
023330         MOVE MFS-ALPHA-FIELD-OK        TO MOD-IDNODE-ATTR                
023331         MOVE MFS-DO-NOT-TOUCH-FIELD    TO MOD-IDNODE                     
023332       ELSE                                                               
023333         MOVE NOO                       TO INPUT-SW                       
023334         MOVE MFS-ALFA-FAELT-FEL        TO MOD-IDNODE-ATTR                
023335         MOVE MFS-DO-NOT-TOUCH-FIELD    TO MOD-IDNODE                     
023336       END-IF                                                             
023337     END-IF                                                               
023338     .                                                                    
023339     EJECT                                                                
023340                                                                          
023350 D-ORDER-LIST                   SECTION.                                  
023400                                                                          
024100     PERFORM DA-ORDER-LIST                                                
024101                                                                          
024110     MOVE INF-LIST-QUEUED               TO MED-IDMFSINF                   
024140                                                                          
024300     .                                                                    
024400     EJECT                                                                
024500 DA-ORDER-LIST                  SECTION.                                  
024600                                                                          
024610     STRING 'ART(' WS-IDARTNR ')'                                         
024611     ' DC(' DCS-IDDC ')'                                                  
024620     ' DIST(' WS-IDDISTR ')'                                              
024621     ' KUND(' WS-IDKUNDNR ')'                                             
024622     ' ANMORS(' WS-KDANMORS ')'                                           
024625     ' AAPPT(' WS-TIAAPP-TOM ')'                                          
024626     ' LISTA(' WS-IDLISTTYP ')'                                           
024627     ' FTG(' WS-IDFTG ')'                                                 
024628     ' AAMMDDF(' WS-TIAAMMDD-FOM ')'                                      
024629     ' AAMMDDT(' WS-TIAAMMDD-TOM ')'                                      
024630     ' IDNODE('                                                           
024631               DELIMITED BY SIZE                                          
024632     WS-IDNODE DELIMITED BY SPACE                                         
024633     ')'       DELIMITED BY SIZE                                          
024634      INTO MSGSOP-TESYMBV                                                 
024640     PERFORM IMS-INSERT-ALTMSG                                            
024650     .                                                                    
024660     EJECT                                                                
024670 Z-FINIT SECTION.                                                         
024680                                                                          
024690     IF MED-IDMFSFEL NOT = SPACE OR MED-IDMFSINF NOT = SPACE              
024700       CALL WMEDKONV USING MED-WMEDAREA                                   
024800       MOVE MED-MFSFEL                TO MOD-TEMFSFEL                     
024900       MOVE MED-MFSINF                TO MOD-TEMFSINF                     
025000     END-IF                                                               
025100                                                                          
025300     PERFORM IMS-INSERT-MSG                                               
025400     .                                                                    
025500     EJECT                                                                
026500 MFS-ERASE-FIELD-IN             SECTION.                                  
026600                                                                          
026700*    --- ALLA INDATA-FÄLT                                                 
026710     MOVE MFS-ERASE-FIELD       TO MOD-IDARTNR-IN                         
026720                                   MOD-IDDC-IN                            
026730                                   MOD-IDDISTR-IN                         
026740                                   MOD-IDKUNDNR-IN                        
026750                                   MOD-KDANMORS-IN                        
027000     .                                                                    
027100     EJECT                                                                
027200 MFS-ERASE-FIELD-OUT            SECTION.                                  
027300                                                                          
027400*    --- ALLA INDATA-FÄLT                                                 
027500     MOVE MFS-ERASE-FIELD       TO MOD-IDARTNR-UT                         
027600                                   MOD-IDDC-UT                            
027700                                   MOD-IDDISTR-UT                         
027800                                   MOD-IDKUNDNR-UT                        
027900                                   MOD-KDANMORS-UT                        
028200     .                                                                    
028300     EJECT                                                                
030300* --- IMS SECTIONS ---                                                    
030400                                                                          
030500 IMS-GET-MSG                    SECTION.                                  
030600                                                                          
030700     MOVE '  QC' TO GOOD-STATUSCODES                                      
030800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
030900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031000     PERFORM IMS-STATUSCHECK                                              
031100     .                                                                    
031200                                                                          
031300 IMS-INSERT-MSG                 SECTION.                                  
031400                                                                          
031510     IF MSGI-IDLAND-SPR = 'GB'                                            
031600       MOVE 'N' TO MFS-KDHUVOMR                                           
031700     END-IF                                                               
031800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
031900     MOVE SPACE TO GOOD-STATUSCODES                                       
032000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
032100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
032200     PERFORM IMS-STATUSCHECK                                              
032300     .                                                                    
032401     EJECT                                                                
032402 IMS-INSERT-ALTMSG SECTION.                                               
032403                                                                          
032404     MOVE '  '  TO GOOD-STATUSCODES                                       
032405     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
032406     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
032407     PERFORM IMS-STATUSCHECK                                              
032408     .                                                                    
032409     EJECT                                                                
032410 IMS-GET-ARTC01                 SECTION.                                  
032411                                                                          
032412     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
032413          DELIMITED BY SIZE INTO SSA1                                     
032414     MOVE '  GE' TO GOOD-STATUSCODES                                      
032415     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
032416     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
032417     PERFORM IMS-STATUSCHECK                                              
032420     .                                                                    
032500     EJECT                                                                
032510 IMS-GU-WDB601    SECTION.                                                
032520     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
032530          DELIMITED BY SIZE INTO SSA1                                     
032540     MOVE '  GE' TO GOOD-STATUSCODES                                      
032550     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
032560     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
032570     PERFORM IMS-STATUSCHECK                                              
032580     IF SEGMENT-MISSING                                                   
032590         MOVE SPACE TO DCS-KDDC                                           
032591     END-IF                                                               
032592     .                                                                    
032600 IMS-STATUSCHECK                SECTION.                                  
032700                                                                          
032800     SET STATUS-IX TO 1                                                   
032900     SEARCH GOOD-STATUS                                                   
033000       AT END                                                             
033100         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
033200         DELIMITED BY SIZE INTO ERROR-TEXT                                
033300         CALL FELLOG                                                      
033400       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
033500         CONTINUE                                                         
033600     END-SEARCH                                                           
033700     .                                                                    
