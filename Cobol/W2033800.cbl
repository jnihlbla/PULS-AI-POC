000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2033800.                                                
000300 AUTHOR.         MARKUS ASPFJÄLL                                          
000400 DATE-WRITTEN.   99/02/04.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        FRÅGEBILD PÅ LEVERANSSPÄRRAR                                     
000900*        ANGE ARTIKELNUMMER, OCH EV DISTRIKT                              
000910*        OM ARTIKELN INGÅR I NÅGON GRUPP                                  
001000*        VISAS BILDEN ANNARS FÅS "PART MISSING IN GROUP"                  
001100*                                                                         
001200*        PROGRAMMET LÄSER      WDF8                                       
001300*                                                                         
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W2T338                                              
001700*        MID:         W2I33801                                            
001800*                                                                         
001900*    UTDATA.                                                              
002000*        MOD:         W2O33801                                            
002100                                                                          
002200                                                                          
002300 ENVIRONMENT DIVISION.                                                    
002400                                                                          
002500 DATA DIVISION.                                                           
002600 WORKING-STORAGE SECTION.                                                 
002700                                                                          
002800*    -- CHECKED BY WY2000                                                 
002900 77  IDPGM                       PIC X(08)   VALUE 'W2033800'.            
003000                                                                          
003100*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003200 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
003300                                                                          
003400 77  JA                          PIC X       VALUE 'J'.                   
003500 77  NEJ                         PIC X       VALUE 'N'.                   
003650                                                                          
003651 01  DAGENS-DATUM                PIC 9(6).                                
003660                                                                          
003670                                                                          
003700*    --- INDEX FÖR BLÄDDRINGSRADER                                        
003800 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
003900 77  MAX-INDX                    PIC S9(4)  VALUE +14   COMP SYNC.        
004000*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004100                                                                          
004200                                                                          
004300 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004400     88  NYCKLAR-OK                          VALUE 'J'.                   
004500     88  NYCKLAR-FEL                         VALUE 'N'.                   
004600                                                                          
004700 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
004800     88  EGEN-MID                            VALUE '2338'.                
004900     88  GODK-MID                            VALUE '2331' '2332'          
005000                                                   '2333' '2334'          
005100                                                   '2335' '2336'          
005200                                                   '2337' '2338'          
005300                                                   '2339'.                
005400     88  HELP-MID                            VALUE '0551'.                
005500                                                                          
005510                                                                          
005520*    --- ARBETSFÄLT                                                       
005530 01  FILLER                      PIC X(24)   VALUE                        
005540                                 'WS-CURRENT-SECTION'.                    
005550 01  WS-CURRENT-SECTION          PIC X(24)   VALUE SPACE.                 
005560 01  FILLER                      PIC X(24)   VALUE                        
005570                                 'WS-CURRENT-IMS-SECTION'.                
005580 01  WS-CURRENT-IMS-SECTION      PIC X(24)   VALUE SPACE.                 
005590                                                                          
005591 01  W-SPARRAD                   PIC X       VALUE 'N'.                   
005592 01  W-SPARR-SEGM                PIC X(3)    VALUE SPACE.                 
005593                                                                          
005594 01  WS-CURRENT-DATE             PIC 9(8)    VALUE ZERO.                  
005595 01  FILLER REDEFINES WS-CURRENT-DATE.                                    
005596     03  FILLER                  PIC 9(2).                                
005597     03  WS-CURRENT-YYMMDD       PIC 9(6).                                
005598 01  WS-CURRENT-TIME             PIC 9(8)    VALUE ZERO.                  
005599 01  FILLER REDEFINES WS-CURRENT-TIME.                                    
005600     03  WS-CURRENT-HHMM         PIC 9(4).                                
005601     03  FILLER                  PIC 9(4).                                
005602                                                                          
005603                                                                          
005610*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
005700 01  GENERELLA-SUBPROGRAM.                                                
005800     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
005900     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006200                                                                          
006300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
006400*01 -COPY WMEDAREA                                                        
006500                                                                          
006600 01  MESSAGE-CODES.                                                       
006700     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
006800     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
006900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007000     03  PART-MISSING            PIC X(3)    VALUE '279'.                 
007100                                                                          
007200*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
007300*                                                                         
007400 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
007500                                                                          
007600*01 -COPY WMSGINIT                                                        
007700                                                                          
007800*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
007900*                                                                         
008000 01  SPAR-AREA.                                                           
008100     03  SPAR-IDTRANS             PIC X(4)   VALUE '2338'.                
008530     03  SPAR-IDSPRGRP-NEXT       PIC X(10) VALUE SPACE.                  
008650     03  SPAR-IDSPRGRP-ENTER      PIC X(10) VALUE SPACE.                  
008660                                                                          
008700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008800*                                                                         
008900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009000                                                                          
009100*01  MID -COPY W2I33801                                                   
009200                                                                          
009300 01  FILLER                      PIC X(16)   VALUE 'MSG/MOD-AREA'.        
009400                                                                          
009500*01  -COPY WMSGAREA                                                       
009600                                                                          
009700     03  MOD REDEFINES MSG-AREA.                                          
009800*      05  -COPY W2O33801                                                 
009900                                                                          
010000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010100                                                                          
010200*01  -COPY WMFSAREA                                                       
010300                                                                          
010400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010500*                                                                         
010600                                                                          
010700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010800                                                                          
010900 01  NYCKLAR-TILL-DLI.                                                    
012570                                                                          
012571     03  W-IDSPRGRP-X            PIC X(10)   VALUE SPACE.                 
012572     03  W-IDDISTR-X.                                                     
012573         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
012574                                                                          
012576     03  W-813-KDARTURS          PIC X(2)    VALUE SPACE.                 
012577                                                                          
012598                                                                          
012599     03  W-WDF814-KDPRODSL-X.                                             
012600         05  W-814-KDPRODSL      PIC S9(3)   VALUE ZERO COMP-3.           
012601                                                                          
012602     03  W-WDF815-IDFKNGRP-X.                                             
012603         05  W-815-IDFKNGRP      PIC S9(5)   VALUE ZERO COMP-3.           
012604                                                                          
012605     03  W-IDARTNR-X.                                                     
012606         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
012607                                                                          
012610                                                                          
012700*    --- STATUS-KOD FRÅN IMS                                              
012800 01  STATUS-WS                   PIC XX.                                  
012900     88  SEGMENT-FINNS                       VALUE '  '.                  
013000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
013100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013200     88  BAS-SLUT                            VALUE 'GB'.                  
013300                                                                          
013400 01  GODK-STATUSKODER.                                                    
013500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013600                                                                          
013700 01  ALL-SSA.                                                             
013710   03  SSA1                        PIC X(64).                             
013720   03  SSA2                        PIC X(64).                             
013900                                                                          
014000*    --- IMS FUNKTIONSKODER                                               
014100*01  -COPY W0003                                                          
014200                                                                          
014300*    ---  DLI INPUT-OUTPUT AREA                                           
014400                                                                          
014500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF801  '.                    
014600 01  DLI-IO-WDF801.                                                       
014700*    03  -COPY WDF801                                                     
014800                                                                          
014900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF811  '.                    
015000 01  DLI-IO-WDF811.                                                       
015001*    03  -COPY WDF811                                                     
015002                                                                          
015003 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF812  '.                    
015004 01  DLI-IO-WDF812.                                                       
015005*    03  -COPY WDF812                                                     
015006                                                                          
015007 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF813  '.                    
015008 01  DLI-IO-WDF813.                                                       
015009*    03  -COPY WDF813                                                     
015010                                                                          
015011 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF814  '.                    
015012 01  DLI-IO-WDF814.                                                       
015013*    03  -COPY WDF814.                                                    
015014                                                                          
015015 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF815  '.                    
015016 01  DLI-IO-WDF815.                                                       
015017*    03  -COPY WDF815.                                                    
015018                                                                          
015019 01  FILLER                      PIC X(16)   VALUE 'WDK601-AREA'.         
015020 01  DLI-IO-WDK601.                                                       
015030     03  WDK601.                                                          
015040*        05  -COPY WDK601                                                 
015050                                                                          
015060                                                                          
015070 01  FILLER                      PIC X(16)   VALUE 'WDK611-AREA'.         
015080 01  DLI-IO-WDK611.                                                       
015090     03  WDK611.                                                          
015091*        05  -COPY WDK611                                                 
015092                                                                          
015100                                                                          
015200 LINKAGE SECTION.                                                         
015300*01  -COPY W0009   -PRE MSG-                                              
015400*01  -COPY W0008   -PRE USEA-                                             
015500     05  FILLER                     PIC X.                                
015600                                                                          
015700*01  -COPY W0008  -PRE WDF8-                                              
015800     05  FILLER                     PIC X.                                
015900                                                                          
016110*01  -COPY W0008  -PRE WDF8B-                                             
016111     05  FILLER                     PIC X.                                
016117                                                                          
016118*01  -COPY W0008   -PRE WDK6-                                             
016120     05  FILLER                     PIC X.                                
016200                                                                          
016300 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB WDF8-PCB                      
016400                           WDF8B-PCB                                      
016401                           WDK6-PCB.                                      
016410 MAIN SECTION.                                                            
016500     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDF8-PCB                      
016520                           WDF8B-PCB                                      
016530                           WDK6-PCB.                                      
016600                                                                          
016601     MOVE FUNCTION CURRENT-DATE(1:8)                                      
016602                                 TO WS-CURRENT-DATE                       
016603     MOVE FUNCTION CURRENT-DATE(9:8)                                      
016604                                 TO WS-CURRENT-TIME                       
016620                                                                          
016700     PERFORM IMS-GET-MSG                                                  
016800     IF SEGMENT-FINNS                                                     
016900       PERFORM A-INIT                                                     
017000       PERFORM B-KOLLA-NYCKLAR                                            
017100       IF NYCKLAR-OK                                                      
017200           IF MFS-FIRST                                                   
017300             PERFORM C-FOERSTA-SIDA                                       
017400           ELSE                                                           
017500             IF MFS-NEXT                                                  
017600               PERFORM D-NAESTA-SIDA                                      
017700             ELSE                                                         
017800               PERFORM E-SAMMA-SIDA                                       
017900             END-IF                                                       
018000           END-IF                                                         
018010                                                                          
018100         PERFORM F-LAES-VISA-INFO                                         
018200       END-IF                                                             
018300       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O33801 + 4                      
018400       PERFORM IMS-INSERT-MSG                                             
018500     END-IF                                                               
018600                                                                          
018700     MOVE ZERO TO RETURN-CODE                                             
018800     GOBACK                                                               
018900     .                                                                    
019000                                                                          
019100 A-INIT SECTION.                                                          
019110     MOVE 'A-INIT            '      TO WS-CURRENT-SECTION                 
019200                                                                          
019300     IF MSG-DUBBLA-TRANSKODER                                             
019400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I33801                 
019500       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
019600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
019700     ELSE                                                                 
019800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I33801                  
019900       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
020000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
020100     END-IF                                                               
020200                                                                          
020300     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
020400     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
020500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
020630                                                                          
020700     MOVE LOW-VALUE  TO MSG-AREA                                          
020800     MOVE 'W2O338N1' TO MFS-IDMOD                                         
020900     MOVE '2338'     TO MOD-IDTRANS                                       
021000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
021100                                                                          
021200     IF EGEN-MID OR HELP-MID                                              
021210      IF MID-IDARTNR-IN NOT = ALL '+' OR                                  
021211        MID-IDDISTR-IN NOT = ALL '+'                                      
021220       MOVE SPACE TO MFS-KDTRTYP                                          
021230       MOVE '7'   TO MFS-IDPFK                                            
021310      END-IF                                                              
021400     ELSE                                                                 
021500       MOVE SPACE TO MFS-KDTRTYP                                          
021600       MOVE '7'   TO MFS-IDPFK                                            
021700     END-IF                                                               
021710                                                                          
021720     MOVE FUNCTION CURRENT-DATE(3:6) TO DAGENS-DATUM                      
021800     .                                                                    
021900                                                                          
022000 B-KOLLA-NYCKLAR SECTION.                                                 
022010     MOVE 'B-KOLLA-NYCKLAR   '      TO WS-CURRENT-SECTION                 
022100                                                                          
022400     MOVE ALL '+'             TO MSGI-WMSGINIT                            
022500     MOVE '001'               TO MSGI-KDCALL                              
022600     MOVE MSG-LTERM-NAME      TO MSGI-IDLTERM-USER                        
022700     MOVE MSG-SIGNON-USERID   TO MSGI-IDUSER                              
022800     MOVE '2338'              TO MSGI-IDTRANS                             
022900     IF EGEN-MID                                                          
023000       MOVE MID-IDARTNR-IN    TO MSGI-IDARTNR                             
023010       MOVE MID-IDDISTR-IN    TO MSGI-IDDISTR                             
023100     ELSE                                                                 
023200       IF MID-IDARTNR-IN NUMERIC                                          
023300         MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                             
023400       END-IF                                                             
023410*      IF MID-IDDISTR-IN NUMERIC                                          
023420*        MOVE MID-IDDISTR-IN  TO MSGI-IDDISTR                             
023430*      END-IF                                                             
023500     END-IF                                                               
023510                                                                          
023600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
023700     MOVE MSGI-SPAR-AREA      TO SPAR-AREA                                
023830                                                                          
023900     IF MSGI-IDLAND-SPR = 'GB'                                            
024000       MOVE 'GB' TO MED-IDSKYLT                                           
024100     ELSE                                                                 
024200       MOVE 'S' TO MED-IDSKYLT                                            
024300     END-IF                                                               
024400                                                                          
024500     MOVE JA TO NYCKLAR-SW                                                
024600     MOVE SPACE TO MED-IDMFSFEL                                           
024700     MOVE SPACE TO MED-IDMFSINF                                           
024800                                                                          
024900*    -- KONTROLL AV IDARTNR                                               
025000     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
025100                                                                          
025200     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
025300     IF MSGI-IDARTNR NUMERIC                                              
025400       MOVE MSGI-IDARTNR    TO W-IDARTNR                                  
025700     ELSE                                                                 
025800       MOVE NEJ             TO NYCKLAR-SW                                 
025900     END-IF                                                               
026000                                                                          
026001*    -- KONTROLL AV IDDISTR                                               
026010     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
026011                                                                          
026012     INSPECT MSGI-IDDISTR REPLACING LEADING SPACE BY ZERO                 
026013     IF MSGI-IDDISTR NUMERIC                                              
026014        IF EGEN-MID                                                       
026015           MOVE MSGI-IDDISTR  TO W-IDDISTR                                
026016        ELSE                                                              
026017           MOVE ZERO          TO W-IDDISTR                                
026018        END-IF                                                            
026020     ELSE                                                                 
026021       MOVE NEJ               TO NYCKLAR-SW                               
026022     END-IF                                                               
026023                                                                          
026100     IF GODK-MID OR NYCKLAR-OK                                            
026200       MOVE MSGI-IDARTNR    TO MOD-IDARTNR-UT                             
026300       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
026301       IF EGEN-MID                                                        
026310          MOVE MSGI-IDDISTR    TO MOD-IDDISTR-UT                          
026320          INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE          
026321       ELSE                                                               
026323          MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                          
026330       END-IF                                                             
026400     ELSE                                                                 
026500       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
026510                               MOD-IDDISTR-UT                             
026600     END-IF                                                               
026700                                                                          
026800     IF NYCKLAR-FEL                                                       
026900       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
027000       CALL WMEDKONV USING MED-WMEDAREA                                   
027100       MOVE MED-MFSFEL    TO MOD-TEMFSFEL                                 
027200       PERFORM MFS-RENSA-FAELT-IN                                         
027300       PERFORM MFS-RENSA-FAELT-UT                                         
027400     END-IF                                                               
027500     .                                                                    
027600                                                                          
027700 C-FOERSTA-SIDA SECTION.                                                  
027710     MOVE 'C-FOERSTA-SIDA    '      TO WS-CURRENT-SECTION                 
027800                                                                          
027900     MOVE INF-FIRST-PAGE TO MED-IDMFSFEL                                  
028000     CALL WMEDKONV USING MED-WMEDAREA                                     
028100     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
028200                                                                          
028300     PERFORM MFS-RENSA-FAELT-IN                                           
028400     .                                                                    
028500                                                                          
028600 D-NAESTA-SIDA SECTION.                                                   
028610     MOVE 'D-NAESTA-SIDA     '      TO WS-CURRENT-SECTION                 
028700                                                                          
028800     IF SPAR-IDTRANS = '2338'                                             
029118       MOVE SPAR-IDSPRGRP-NEXT     TO W-IDSPRGRP-X                        
029200     ELSE                                                                 
029300       PERFORM MFS-RENSA-FAELT-IN                                         
029400     END-IF                                                               
029500     .                                                                    
029600                                                                          
029700 E-SAMMA-SIDA SECTION.                                                    
029710     MOVE 'E-SAMMA-SIDA      '      TO WS-CURRENT-SECTION                 
029800                                                                          
029900     IF SPAR-IDTRANS = '2338' OR '0551'                                   
029950       MOVE SPAR-IDSPRGRP-ENTER       TO W-IDSPRGRP-X                     
030100     ELSE                                                                 
030200       PERFORM MFS-RENSA-FAELT-IN                                         
030300     END-IF                                                               
030400     .                                                                    
030500                                                                          
030600 F-LAES-VISA-INFO SECTION.                                                
030601     MOVE 'F-LAES-VISA-INFO  '      TO WS-CURRENT-SECTION                 
030610                                                                          
030614     IF W-IDDISTR > ZERO                                                  
030616       PERFORM FA-LAES-WDF801-811                                         
030618     ELSE                                                                 
030619       PERFORM FB-LAES-WDF801                                             
030620     END-IF                                                               
030621                                                                          
030622     IF INDX = 1                                                          
030623       MOVE PART-MISSING TO MED-IDMFSFEL                                  
030624       CALL WMEDKONV USING MED-WMEDAREA                                   
030625       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
030626       PERFORM MFS-RENSA-FAELT-UT                                         
030627     END-IF                                                               
030628     .                                                                    
030629                                                                          
035220                                                                          
035230 FA-LAES-WDF801-811 SECTION.                                              
035240     MOVE 'FA-LAES-WDF801-811'      TO WS-CURRENT-SECTION                 
035251                                                                          
035260     PERFORM IMS-GU-WDF8B1-START                                          
035270                                                                          
035293     MOVE GSPR-IDSPRGRP           TO SPAR-IDSPRGRP-ENTER                  
035294                                                                          
035295     IF SEGMENT-SAKNAS                                                    
035300       MOVE SPACE                 TO SPAR-IDSPRGRP-NEXT                   
035301                                                                          
035302       MOVE PART-MISSING TO MED-IDMFSFEL                                  
035303       CALL WMEDKONV USING MED-WMEDAREA                                   
035304       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
035305       PERFORM MFS-RENSA-FAELT-UT                                         
035306     ELSE                                                                 
035307                                                                          
035309       MOVE +1 TO INDX                                                    
035310       PERFORM UNTIL INDX > MAX-INDX OR SEGMENT-SAKNAS                    
035311       OR BAS-SLUT                                                        
035312         IF SEGMENT-FINNS                                                 
035313           MOVE GSPR-IDSPRGRP TO W-IDSPRGRP-X                             
035316* LÄS 801 O 811-SEGMENT O SE OM DISTR FINNS (ÄR SPÄRRAD)                  
035317                                                                          
035318           PERFORM IMS-GU-WDF801                                          
035319           IF SEGMENT-FINNS                                               
035321             PERFORM IMS-GNP-WDF811-KVAL                                  
035322             IF SEGMENT-FINNS                                             
035324               IF GSPR-FLAUTUPD = JA                                      
035326                 PERFORM S01-AUTOUPD                                      
035327               ELSE                                                       
035329                 PERFORM S02-EJ-AUTOUPD                                   
035330               END-IF                                                     
035331                                                                          
035335               IF W-SPARRAD = JA                                          
035336                 PERFORM S03-REDIGERA-RAD                                 
035337                 ADD +1 TO INDX                                           
035338               END-IF                                                     
035339             END-IF                                                       
035340           END-IF                                                         
035341         END-IF                                                           
035342                                                                          
035343         PERFORM IMS-GN-WDF8B1-NEXT                                       
035344       END-PERFORM                                                        
035345                                                                          
035346       IF SEGMENT-FINNS                                                   
035352         MOVE GSPR-IDSPRGRP         TO SPAR-IDSPRGRP-NEXT                 
035353         MOVE INF-MORE-INFO-EXISTS  TO MED-IDMFSINF                       
035354         CALL WMEDKONV USING MED-WMEDAREA                                 
035355         MOVE MED-TEMFSINF          TO MOD-TEMFSINF                       
035356       ELSE                                                               
035362         MOVE SPAR-IDSPRGRP-ENTER   TO SPAR-IDSPRGRP-NEXT                 
035363       END-IF                                                             
035364     END-IF                                                               
035365                                                                          
035366     MOVE '002'     TO MSGI-KDCALL                                        
035367     MOVE '2338'    TO SPAR-IDTRANS                                       
035368     MOVE SPAR-AREA TO MSGI-SPAR-AREA                                     
035369     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
035370     .                                                                    
035371                                                                          
035372                                                                          
035373 FB-LAES-WDF801    SECTION.                                               
035374     MOVE 'FB-LAES-WDF801    '      TO WS-CURRENT-SECTION                 
035380                                                                          
035391     PERFORM IMS-GU-WDF8B1-START                                          
035397     MOVE GSPR-IDSPRGRP           TO SPAR-IDSPRGRP-ENTER                  
035400                                                                          
035401     IF SEGMENT-SAKNAS                                                    
035406       MOVE SPACE                 TO SPAR-IDSPRGRP-NEXT                   
035409                                                                          
035410       MOVE PART-MISSING TO MED-IDMFSFEL                                  
035411       CALL WMEDKONV USING MED-WMEDAREA                                   
035412       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
035413       PERFORM MFS-RENSA-FAELT-UT                                         
035414     ELSE                                                                 
035415                                                                          
035416       MOVE +1 TO INDX                                                    
035417       PERFORM UNTIL INDX > MAX-INDX OR SEGMENT-SAKNAS                    
035418       OR BAS-SLUT                                                        
035419         IF SEGMENT-FINNS                                                 
035420           MOVE GSPR-IDSPRGRP TO W-IDSPRGRP-X                             
035421           IF GSPR-FLAUTUPD = JA                                          
035422             PERFORM S01-AUTOUPD                                          
035423             CONTINUE                                                     
035424           ELSE                                                           
035425             PERFORM S02-EJ-AUTOUPD                                       
035426           END-IF                                                         
035427                                                                          
035430                                                                          
035431           IF W-SPARRAD = JA                                              
035432             PERFORM S03-REDIGERA-RAD                                     
035433             ADD +1 TO INDX                                               
035434           END-IF                                                         
035435                                                                          
035436         END-IF                                                           
035437                                                                          
035438         PERFORM IMS-GN-WDF8B1-NEXT                                       
035442       END-PERFORM                                                        
035443                                                                          
035444       IF SEGMENT-FINNS                                                   
035450         MOVE GSPR-IDSPRGRP         TO SPAR-IDSPRGRP-NEXT                 
035452         MOVE INF-MORE-INFO-EXISTS  TO MED-IDMFSINF                       
035453         CALL WMEDKONV USING MED-WMEDAREA                                 
035454         MOVE MED-TEMFSINF          TO MOD-TEMFSINF                       
035455       ELSE                                                               
035461         MOVE SPAR-IDSPRGRP-ENTER   TO SPAR-IDSPRGRP-NEXT                 
035466       END-IF                                                             
035467     END-IF                                                               
035468                                                                          
035469     MOVE '002'     TO MSGI-KDCALL                                        
035470     MOVE '2338'    TO SPAR-IDTRANS                                       
035471     MOVE SPAR-AREA TO MSGI-SPAR-AREA                                     
035472     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
035473     .                                                                    
035554                                                                          
035555 S01-AUTOUPD SECTION.                                                     
035556     MOVE 'S01-AUTOUPD       '      TO WS-CURRENT-SECTION                 
035558                                                                          
035559     MOVE NEJ            TO W-SPARRAD                                     
035560                                                                          
035561     PERFORM IMS-GU-WDK601                                                
035562                                                                          
035563     IF SEGMENT-FINNS                                                     
035564       MOVE ART-KDPRODSL TO W-814-KDPRODSL                                
035565       PERFORM IMS-GU-WDF814                                              
035566                                                                          
035567       IF SEGMENT-FINNS                                                   
035568         MOVE JA         TO W-SPARRAD                                     
035569         MOVE '814'      TO W-SPARR-SEGM                                  
035570         MOVE PSPR-TISTADAT TO MOD-TISTADAT-ART (INDX)                    
035571         IF PSPR-TISTADAT > DAGENS-DATUM                                  
035572            MOVE MFS-ADD-LYS-UPP-FAELT TO                                 
035573                 MOD-TISTADAT-ART-ATTR (INDX)                             
035574         END-IF                                                           
035575       ELSE                                                               
035576         MOVE ART-IDFKNGRP TO W-815-IDFKNGRP                              
035577         PERFORM IMS-GU-WDF815                                            
035578                                                                          
035579         IF SEGMENT-FINNS                                                 
035580           MOVE JA       TO W-SPARRAD                                     
035581           MOVE '815'    TO W-SPARR-SEGM                                  
035582           MOVE FSPR-TISTADAT TO MOD-TISTADAT-ART (INDX)                  
035583           IF FSPR-TISTADAT > DAGENS-DATUM                                
035584              MOVE MFS-ADD-LYS-UPP-FAELT TO                               
035585                   MOD-TISTADAT-ART-ATTR (INDX)                           
035586           END-IF                                                         
035587         ELSE                                                             
035588                                                                          
035589           PERFORM IMS-GNP-WDK611                                         
035590                                                                          
035591           IF SEGMENT-FINNS                                               
035592             MOVE CLAG-KDARTURS TO W-813-KDARTURS                         
035593             PERFORM IMS-GU-WDF813                                        
035594                                                                          
035595             IF SEGMENT-FINNS                                             
035596               MOVE JA   TO W-SPARRAD                                     
035597               MOVE '813' TO W-SPARR-SEGM                                 
035598               MOVE USPR-TISTADAT TO MOD-TISTADAT-ART (INDX)              
035599               IF USPR-TISTADAT > DAGENS-DATUM                            
035600                  MOVE MFS-ADD-LYS-UPP-FAELT TO                           
035601                       MOD-TISTADAT-ART-ATTR (INDX)                       
035602               END-IF                                                     
035603             END-IF                                                       
035604           END-IF                                                         
035605         END-IF                                                           
035606       END-IF                                                             
035607     END-IF                                                               
035608     .                                                                    
035609                                                                          
035610 S02-EJ-AUTOUPD SECTION.                                                  
035700     MOVE 'S02-EJ-AUTOUPD    '      TO WS-CURRENT-SECTION                 
035701                                                                          
035710     MOVE NEJ            TO W-SPARRAD                                     
035750     PERFORM IMS-GU-WDF812                                                
035751                                                                          
035760     IF SEGMENT-FINNS                                                     
035770       MOVE JA           TO W-SPARRAD                                     
035780       MOVE '812'        TO W-SPARR-SEGM                                  
035790       MOVE ASPR-TISTADAT TO MOD-TISTADAT-ART (INDX)                      
035800       IF ASPR-TISTADAT > DAGENS-DATUM                                    
035801          MOVE MFS-ADD-LYS-UPP-FAELT TO                                   
035802               MOD-TISTADAT-ART-ATTR (INDX)                               
035803       END-IF                                                             
035808     END-IF                                                               
035900                                                                          
036000     .                                                                    
036100                                                                          
036200 S03-REDIGERA-RAD SECTION.                                                
036210     MOVE 'S03-REDIGERA-RAD  '      TO WS-CURRENT-SECTION                 
036220                                                                          
036230     MOVE GSPR-IDSPRGRP  TO MOD-IDDIRGRP     (INDX)                       
036240     MOVE GSPR-FLAUTUPD  TO MOD-FLAUTUPD     (INDX)                       
036250     MOVE GSPR-TISTADAT  TO MOD-TISTADAT-GRP (INDX)                       
036251     IF GSPR-TISTADAT > DAGENS-DATUM                                      
036252        MOVE MFS-ADD-LYS-UPP-FAELT TO                                     
036253             MOD-TISTADAT-GRP-ATTR (INDX)                                 
036254     END-IF                                                               
036260     MOVE GSPR-TENOTE    TO MOD-TENOTE40     (INDX)                       
036270                                                                          
036280     .                                                                    
036300 MFS-RENSA-FAELT-UT SECTION.                                              
036310     MOVE 'MFS-RENSA-FAELT-UT'      TO WS-CURRENT-SECTION                 
036400                                                                          
036500*    --- ALLA UTDATA-FÄLT                                                 
036600*    --- INKL. BLÄDDRINGSNYCKLAR                                          
036700                                                                          
036800     MOVE +1 TO INDX                                                      
036900     PERFORM UNTIL INDX > MAX-INDX                                        
037000       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
037100       ADD +1 TO INDX                                                     
037200     END-PERFORM                                                          
037300     .                                                                    
037400                                                                          
037500 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
037510     MOVE 'MFS-RENSA-RAD-FAE '      TO WS-CURRENT-SECTION                 
037600                                                                          
037700*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
037800     MOVE MFS-RENSA-FAELT TO MOD-IDDIRGRP     (INDX)                      
037900                             MOD-FLAUTUPD     (INDX)                      
037910                             MOD-TISTADAT-GRP (INDX)                      
038000                             MOD-TISTADAT-ART (INDX)                      
038010                             MOD-TENOTE40     (INDX)                      
038100                                                                          
038200     .                                                                    
038300                                                                          
038400 MFS-RENSA-FAELT-IN SECTION.                                              
038410     MOVE 'MFS-RENSA-FAELT-IN'      TO WS-CURRENT-SECTION                 
038500                                                                          
038600*    --- ALLA INDATA-FÄLT                                                 
038700     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
038710                             MOD-IDDISTR-IN                               
038800     .                                                                    
038900                                                                          
039000* --- IMS SEKTIONER ---                                                   
039100                                                                          
039200 IMS-GET-MSG SECTION.                                                     
039210     MOVE 'IMS-GET-MSG       '      TO WS-CURRENT-IMS-SECTION             
039300                                                                          
039400     MOVE '  QC' TO GODK-STATUSKODER                                      
039500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
039600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
039700     PERFORM IMS-STATUSKONTROLL                                           
039800     .                                                                    
039900                                                                          
040000 IMS-INSERT-MSG SECTION.                                                  
040010     MOVE 'IMS-INSERT-MSG    '      TO WS-CURRENT-IMS-SECTION             
040100                                                                          
040200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
040300     MOVE SPACE TO GODK-STATUSKODER                                       
040400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
040500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
040600     PERFORM IMS-STATUSKONTROLL                                           
040700     .                                                                    
040800                                                                          
043200                                                                          
043201 IMS-GU-WDF8B1-START SECTION.                                             
043202     MOVE 'GU-WDF8B1-START '  TO WS-CURRENT-IMS-SECTION                   
043203                                                                          
043204     MOVE SPACE               TO ALL-SSA                                  
043205                                                                          
043206     STRING 'WDF801  (WDF8BSEQ>=' W-IDSPRGRP-X ')'                        
043207          DELIMITED BY SIZE INTO SSA1                                     
043208     MOVE '  GE'              TO GODK-STATUSKODER                         
043209     CALL CBLTDLI USING GU WDF8B-PCB DLI-IO-WDF801 SSA1                   
043210     MOVE WDF8B-STATUS-CODE   TO STATUS-WS                                
043214     PERFORM IMS-STATUSKONTROLL                                           
043215     .                                                                    
043216                                                                          
043217                                                                          
043218 IMS-GN-WDF8B1-NEXT SECTION.                                              
043219     MOVE 'GN-WDF8B1-NEXT  '  TO WS-CURRENT-IMS-SECTION                   
043220                                                                          
043221     MOVE SPACE               TO ALL-SSA                                  
043222                                                                          
043223     STRING 'WDF801  (WDF8BSEQ>=' W-IDSPRGRP-X ')'                        
043224          DELIMITED BY SIZE INTO SSA1                                     
043225     MOVE '  GEGB'            TO GODK-STATUSKODER                         
043226     CALL CBLTDLI USING GN WDF8B-PCB DLI-IO-WDF801 SSA1                   
043227     MOVE WDF8B-STATUS-CODE   TO STATUS-WS                                
043231     PERFORM IMS-STATUSKONTROLL                                           
043232     .                                                                    
043233                                                                          
043234                                                                          
043235 IMS-GU-WDF801 SECTION.                                                   
043236     MOVE 'IMS-GU-WDF801     '      TO WS-CURRENT-IMS-SECTION             
043237                                                                          
043240     MOVE SPACE TO ALL-SSA                                                
043300                                                                          
043410     STRING 'WDF801  (IDSPRGRP =' W-IDSPRGRP-X ')'                        
043500          DELIMITED BY SIZE INTO SSA1                                     
043600     MOVE '  GE' TO GODK-STATUSKODER                                      
043700     CALL CBLTDLI USING GU WDF8-PCB DLI-IO-WDF801 SSA1                    
043800     MOVE WDF8-STATUS-CODE TO STATUS-WS                                   
043801                                                                          
043900     PERFORM IMS-STATUSKONTROLL                                           
044000     .                                                                    
044027 IMS-GNP-WDF811-KVAL SECTION.                                             
044028     MOVE 'IMS-GNP-WDF811-KVAL'     TO WS-CURRENT-IMS-SECTION             
044029                                                                          
044030     MOVE SPACE TO ALL-SSA                                                
044034                                                                          
044037     STRING 'WDF811  (IDDISTRF<=' W-IDDISTR-X                             
044038                    '&IDDISTRT>=' W-IDDISTR-X ')'                         
044039          DELIMITED BY SIZE INTO SSA1                                     
044040     MOVE '  GEGB' TO GODK-STATUSKODER                                    
044041     CALL CBLTDLI USING GNP WDF8-PCB DLI-IO-WDF811 SSA1                   
044042     MOVE WDF8-STATUS-CODE TO STATUS-WS                                   
044047     PERFORM IMS-STATUSKONTROLL                                           
044048     .                                                                    
044049 IMS-GU-WDF812 SECTION.                                                   
044050     MOVE 'IMS-GU-WDF812     '      TO WS-CURRENT-IMS-SECTION             
044051                                                                          
044052     MOVE SPACE TO ALL-SSA                                                
044053                                                                          
044054     STRING 'WDF801  (IDSPRGRP =' W-IDSPRGRP-X ')'                        
044055          DELIMITED BY SIZE INTO SSA1                                     
044056     STRING 'WDF812  (IDARTNR  =' W-IDARTNR-X ')'                         
044057          DELIMITED BY SIZE INTO SSA2                                     
044058     MOVE '  GE' TO GODK-STATUSKODER                                      
044059     CALL CBLTDLI USING GU WDF8-PCB DLI-IO-WDF812 SSA1 SSA2               
044060     MOVE WDF8-STATUS-CODE TO STATUS-WS                                   
044061                                                                          
044062     PERFORM IMS-STATUSKONTROLL                                           
044063     .                                                                    
044064                                                                          
044065 IMS-GU-WDF813 SECTION.                                                   
044066     MOVE 'IMS-GU-WDF813     '      TO WS-CURRENT-IMS-SECTION             
044067                                                                          
044068     MOVE SPACE TO ALL-SSA                                                
044069                                                                          
044070     STRING 'WDF801  (IDSPRGRP =' W-IDSPRGRP-X ')'                        
044071          DELIMITED BY SIZE INTO SSA1                                     
044072     STRING 'WDF813  (KDARTURS =' W-813-KDARTURS ')'                      
044073          DELIMITED BY SIZE INTO SSA2                                     
044074     MOVE '  GE' TO GODK-STATUSKODER                                      
044075     CALL CBLTDLI USING GU WDF8-PCB DLI-IO-WDF813 SSA1 SSA2               
044076     MOVE WDF8-STATUS-CODE TO STATUS-WS                                   
044077     PERFORM IMS-STATUSKONTROLL                                           
044078     .                                                                    
044079 IMS-GU-WDF814 SECTION.                                                   
044080     MOVE 'IMS-GU-WDF814     '      TO WS-CURRENT-IMS-SECTION             
044081                                                                          
044082     MOVE SPACE TO ALL-SSA                                                
044083                                                                          
044084     STRING 'WDF801  (IDSPRGRP =' W-IDSPRGRP-X ')'                        
044085          DELIMITED BY SIZE INTO SSA1                                     
044086     STRING 'WDF814  (KDPRODSF<=' W-WDF814-KDPRODSL-X                     
044087                    '&KDPRODST>=' W-WDF814-KDPRODSL-X ')'                 
044088          DELIMITED BY SIZE INTO SSA2                                     
044089     MOVE '  GE' TO GODK-STATUSKODER                                      
044090     CALL CBLTDLI USING GU WDF8-PCB DLI-IO-WDF814 SSA1 SSA2               
044091     MOVE WDF8-STATUS-CODE TO STATUS-WS                                   
044092     PERFORM IMS-STATUSKONTROLL                                           
044093     .                                                                    
044094                                                                          
044095                                                                          
044096 IMS-GU-WDF815 SECTION.                                                   
044097     MOVE 'IMS-GU-WDF815     '      TO WS-CURRENT-IMS-SECTION             
044098                                                                          
044099     MOVE SPACE TO ALL-SSA                                                
044100                                                                          
044101     STRING 'WDF801  (IDSPRGRP =' W-IDSPRGRP-X ')'                        
044102          DELIMITED BY SIZE INTO SSA1                                     
044103     STRING 'WDF815  (IDFKNGRF<=' W-WDF815-IDFKNGRP-X                     
044104                    '&IDFKNGRT>=' W-WDF815-IDFKNGRP-X ')'                 
044105          DELIMITED BY SIZE INTO SSA2                                     
044106     MOVE '  GE' TO GODK-STATUSKODER                                      
044107     CALL CBLTDLI USING GU WDF8-PCB DLI-IO-WDF815 SSA1 SSA2               
044108     MOVE WDF8-STATUS-CODE TO STATUS-WS                                   
044109     PERFORM IMS-STATUSKONTROLL                                           
044110     .                                                                    
044111 IMS-GU-WDK601                 SECTION.                                   
044120     MOVE 'IMS-GU-WDK601     '      TO WS-CURRENT-IMS-SECTION             
044121                                                                          
044122     MOVE SPACE TO ALL-SSA                                                
044130                                                                          
044140     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
044150            DELIMITED BY SIZE INTO SSA1                                   
044160     MOVE '  GE'                 TO GODK-STATUSKODER                      
044170     CALL  CBLTDLI  USING GU   WDK6-PCB DLI-IO-WDK601 SSA1                
044180     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
044191     PERFORM IMS-STATUSKONTROLL                                           
044192     .                                                                    
044193                                                                          
044194 IMS-GNP-WDK611               SECTION.                                    
044196     MOVE 'IMS-GNP-WDK611   '      TO WS-CURRENT-IMS-SECTION              
044197                                                                          
044198     MOVE SPACE TO ALL-SSA                                                
044199                                                                          
044200     MOVE 'WDK611  '           TO SSA1                                    
044201     MOVE '  GE'               TO GODK-STATUSKODER                        
044202     CALL  CBLTDLI  USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                 
044203     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
044204     PERFORM IMS-STATUSKONTROLL                                           
044205     .                                                                    
044220 IMS-STATUSKONTROLL SECTION.                                              
044300                                                                          
044400     SET STATUS-IX TO 1                                                   
044500     SEARCH GODK-STATUS                                                   
044600       AT END                                                             
044700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
044800         DELIMITED BY SIZE INTO FELTEXT                                   
044900         CALL FELLOG                                                      
045000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
045100         CONTINUE                                                         
045200     END-SEARCH                                                           
045300     .                                                                    
