000100 PROCESS DYNAM                                                            
001300 ID DIVISION.                                                             
001400 PROGRAM-ID.     W3032200.                                                
001500 AUTHOR.         JONNY SANDSTEN.                                          
001600 DATE-WRITTEN.   98/03/25.                                                
001700 DATE-COMPILED.                                                           
001800                                                                          
001900*    FUNKTION:                                                            
002000*        PRISBOK ONLINE                                                   
002100*                                                                         
002201*        PROGRAMMET LÄSER      TABELL FSG2                                
002202*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
002210*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSAKTION: W3T322                                              
002600*        MID:         W3I32201                                            
002700*                                                                         
002800*    UTDATA.                                                              
002900*        MOD:         W3O32201                                            
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003400 DATA DIVISION.                                                           
003410     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003501                                                                          
003510*    -- CHECKED BY WY2000                                                 
003600 77  IDPGM                       PIC X(08)   VALUE 'W3032200'.            
003700                                                                          
003800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004000                                                                          
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300                                                                          
004401*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004402 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004410 77  MAX-INDX                    PIC S9(4)  VALUE +12   COMP SYNC.        
004411                                                                          
004412*    --- BYT-SW                                                           
004420 77  BYT-SW                      PIC X       VALUE 'J'.                   
004430     88  BYT-OK                              VALUE 'J'.                   
004440                                                                          
004500*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005100     88  NYCKLAR-OK                          VALUE 'J'.                   
005200     88  NYCKLAR-FEL                         VALUE 'N'.                   
005300                                                                          
005400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005500     88  EGEN-MID                            VALUE '3322'.                
005600     88  GODK-MID                            VALUE '3321' '3322'          
005700                                                   '3323' '3324'          
005800                                                   '3325' '3326'          
005900                                                   '3327' '3328'          
006000                                                   '3329'.                
006100     88  HELP-MID                            VALUE '0551'.                
006200     EJECT                                                                
006300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006400 01  GENERELLA-SUBPROGRAM.                                                
006500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007000     EJECT                                                                
007100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007200*01 -COPY WMEDAREA                                                        
007300     SKIP3                                                                
007400 01  MESSAGE-CODES.                                                       
007601     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
007610     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
007700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007800     03  ART-MISSING             PIC X(3)    VALUE '017'.                 
007900     EJECT                                                                
008000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008100*                                                                         
008200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008300     SKIP3                                                                
008400*01 -COPY WMSGINIT                                                        
008501     EJECT                                                                
008502*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
008503*                                                                         
008504 01  SPAR-AREA.                                                           
008505     03  SPAR-IDTRANS              PIC X(4)     VALUE '3322'.             
008523     03  SPAR-WDK6BSEQ-ENTER       PIC X(22)    VALUE SPACE.              
008524     03  SPAR-WDK6BSEQ-NEXT        PIC X(22)    VALUE SPACE.              
008600     EJECT                                                                
008700*    --- GENERELLA ARBETSAREROR                                           
008702 01  WS-AF2-TG                     PIC S9(2)V9(2) VALUE ZERO.             
008703 01  WS-IDARTNR                    PIC X(9)       VALUE ZERO.             
008704 01  WS-IDLEVNR                    PIC X(5)       VALUE SPACE.            
008705 01  WS-KDERS                      PIC X(3)       VALUE ZERO.             
008707 01  WS-TIURPROD                   PIC X(5)       VALUE ZERO.             
008708     EJECT                                                                
008710*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008800*                                                                         
008900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009000     SKIP3                                                                
009100*01  MID -COPY W3I32201                                                   
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009400     SKIP3                                                                
009500*01  -COPY WMSGAREA                                                       
009600     EJECT                                                                
009700     03  MOD REDEFINES MSG-AREA.                                          
009800       05 -COPY W3O32201                                                  
009900     EJECT                                                                
010000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010100     SKIP3                                                                
010200*01  -COPY WMFSAREA                                                       
010300     EJECT                                                                
010400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010500*                                                                         
010600     EJECT                                                                
010700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010800     SKIP3                                                                
010900 01  NYCKLAR-TILL-DLI.                                                    
011001*    --- VÄRDE PÅ BLÄDDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN            
011007     03  W-WDK6BSEQ.                                                      
011008         05  W-IDFKNGRP          PIC S9(5) VALUE ZERO COMP-3.             
011009         05  W-KDPRODSL          PIC S9(3) VALUE ZERO COMP-3.             
011010         05  W-IDLEVNR           PIC  X(5) VALUE SPACE.                   
011011         05  W-IDARTNR           PIC S9(9) VALUE ZERO COMP-3.             
011012     03  W-WDK6BSEQ-MIN-X.                                                
011013         05  W-IDFKNGRP-MIN      PIC S9(5) VALUE ZERO COMP-3.             
011014         05  W-KDPRODSL-MIN      PIC S9(3) VALUE ZERO COMP-3.             
011015         05  W-IDLEVNR-MIN       PIC  X(5) VALUE SPACE.                   
011016         05  W-IDARTNR-MIN       PIC S9(9) VALUE ZERO COMP-3.             
011017     03  W-WDK6BSEQ-MAX-X.                                                
011018         05  W-IDFKNGRP-MAX      PIC S9(5) VALUE ZERO COMP-3.             
011019         05  W-KDPRODSL-MAX      PIC S9(3) VALUE ZERO COMP-3.             
011020         05  W-IDLEVNR-MAX       PIC  X(5) VALUE SPACE.                   
011021         05  W-IDARTNR-MAX       PIC S9(9) VALUE ZERO COMP-3.             
011022     03  W-KDSEGKEY-X.                                                    
011023         05  W-KDSEGKEY          PIC X(1)  VALUE SPACE.                   
011024     SKIP2                                                                
011025     03  W-IDLEVNR-X.                                                     
011028         05  W-IDLEVNR-SOK       PIC  X(5) VALUE SPACE.                   
011029     SKIP2                                                                
011030     03  W-IDARTNR-WDD3-X.                                                
011031         05  W-IDARTNR-WDD3      PIC S9(9)  VALUE ZERO COMP-3.            
011032     03  W-IDSKYLT-X.                                                     
011040         05  W-IDSKYLT           PIC X(3)   VALUE SPACE.                  
011100     SKIP2                                                                
011200*    --- STATUS-KOD FRÅN IMS                                              
011300 01  STATUS-WS                   PIC XX.                                  
011400     88  SEGMENT-FINNS                       VALUE '  '.                  
011500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011700     SKIP2                                                                
011800 01  GODK-STATUSKODER.                                                    
011900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012000     SKIP3                                                                
012100 01  SSA1                        PIC X(100).                              
012200 01  SSA2                        PIC X(64).                               
012300     EJECT                                                                
012400*    --- IMS FUNKTIONSKODER                                               
012500*01  -COPY W0003                                                          
012601     EJECT                                                                
012602 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
012603       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
012604                                                                          
012605 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
012606 01  DB2-WS.                                                              
012607     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
012608         88  CURSOR-OK                       VALUE 000.                   
012609         88  RADER-FINNS                     VALUE 000.                   
012610         88  RADER-SAKNAS                    VALUE 100.                   
012611         88  ATKOMST-FEL                     VALUE 904.                   
012612     03  GODK-SQLCODEKODER.                                               
012613         05  GODK-SQLCODE OCCURS 5                                        
012614             INDEXED BY SQLCODE-IX PIC 9(3).                              
012620                                                                          
012700     EJECT                                                                
012800*    ---  DLI INPUT-OUTPUT AREA                                           
012900                                                                          
013001 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTC01'.                    
013002 01  DLI-IO-WLARTC01.                                                     
013003*    03  -COPY WDK601                                                     
013004     EJECT                                                                
013005 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTC11'.                    
013006 01  DLI-IO-WLARTC11.                                                     
013007*    03  -COPY WDK611                                                     
013101     EJECT                                                                
013102 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLBENA01'.                    
013103 01  DLI-IO-WLBENA01.                                                     
013104*    03  -COPY WDD311                                                     
013105     EJECT                                                                
013106*01  FILLER -COPY FSG2 -PRE FSG-                                          
013107     EJECT                                                                
013108 01  FILLER                      PIC X(16)   VALUE 'FSG-AREA'.            
013109       EXEC SQL INCLUDE FSG2      END-EXEC.                               
013300     EJECT                                                                
013400 LINKAGE SECTION.                                                         
013500*01  -COPY W0009   -PRE MSG-                                              
013600*01  -COPY W0008   -PRE USEA-                                             
013700     05  FILLER                  PIC X.                                   
013801                                                                          
013802*01  -COPY W0008  -PRE ARTC-                                              
013803     05  FILLER                  PIC X.                                   
013804                                                                          
013805*01  -COPY W0008  -PRE BENA-                                              
013810     05  FILLER                  PIC X.                                   
013900     EJECT                                                                
014001 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB ARTC-PCB BENA-PCB.            
014002 MAIN SECTION.                                                            
014010     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB ARTC-PCB BENA-PCB.            
014100                                                                          
014300     PERFORM IMS-GET-MSG                                                  
014400     IF SEGMENT-FINNS                                                     
014500       PERFORM A-INIT                                                     
014600       PERFORM B-KOLLA-NYCKLAR                                            
014700       IF NYCKLAR-OK                                                      
014901           IF MFS-FIRST                                                   
014902             PERFORM C-FOERSTA-SIDA                                       
014903           ELSE                                                           
014904             IF MFS-NEXT                                                  
014905               PERFORM D-NAESTA-SIDA                                      
014906             ELSE                                                         
014907               PERFORM E-SAMMA-SIDA                                       
014908             END-IF                                                       
014910           END-IF                                                         
015200         PERFORM F-LAES-VISA-INFO                                         
015300       END-IF                                                             
015600       COMPUTE MSG-KVLL = LENGTH OF MOD-W3O32201 + 4                      
015700       PERFORM IMS-INSERT-MSG                                             
015800     END-IF                                                               
016000                                                                          
016100     MOVE ZERO TO RETURN-CODE                                             
016200     GOBACK                                                               
016300     .                                                                    
016400     EJECT                                                                
016500 A-INIT SECTION.                                                          
016600                                                                          
016700     IF MSG-DUBBLA-TRANSKODER                                             
016800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I32201                 
016900       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017100     ELSE                                                                 
017200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W3I32201                  
017300       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
017400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
017500     END-IF                                                               
017600                                                                          
017700     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
017800     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
017900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018000                                                                          
018100     MOVE LOW-VALUE  TO MSG-AREA                                          
018200     MOVE 'W3O32201' TO MFS-IDMOD                                         
018300     MOVE '3322'     TO MOD-IDTRANS                                       
018400     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
018500                                                                          
018600     IF EGEN-MID OR HELP-MID                                              
018700       CONTINUE                                                           
018800     ELSE                                                                 
018900       MOVE SPACE TO MFS-KDTRTYP                                          
019000       MOVE '7' TO MFS-IDPFK                                              
019100     END-IF                                                               
019201                                                                          
019202     IF MID-IDFKNGRP-IN NOT = ALL '+'                                     
019203       MOVE '7'         TO MFS-IDPFK                                      
019204       MOVE SPACE       TO MFS-KDTRTYP                                    
019205     END-IF                                                               
019206                                                                          
019207     IF MID-KDPRODSL-IN NOT = ALL '+'                                     
019208       MOVE '7'         TO MFS-IDPFK                                      
019209       MOVE SPACE       TO MFS-KDTRTYP                                    
019210     END-IF                                                               
019211                                                                          
019212     IF MID-IDLEVNR-IN NOT = ALL '+'                                      
019213       MOVE '7'         TO MFS-IDPFK                                      
019214       MOVE SPACE       TO MFS-KDTRTYP                                    
019215     END-IF                                                               
019216                                                                          
019220     INITIALIZE GODK-SQLCODEKODER                                         
019400     .                                                                    
019500     EJECT                                                                
019600 B-KOLLA-NYCKLAR SECTION.                                                 
019700                                                                          
019705     MOVE LOW-VALUE           TO W-WDK6BSEQ-MIN-X                         
019706     MOVE HIGH-VALUE          TO W-WDK6BSEQ-MAX-X                         
019710     MOVE JA TO NYCKLAR-SW                                                
019720                                                                          
019810     MOVE ALL '+'             TO MSGI-WMSGINIT                            
019900     MOVE '001'               TO MSGI-KDCALL                              
020000     MOVE MSG-LTERM-NAME      TO MSGI-IDLTERM-USER                        
020100     MOVE MSG-SIGNON-USERID   TO MSGI-IDUSER                              
020200     MOVE '3322'              TO MSGI-IDTRANS                             
020300                                                                          
020400     IF EGEN-MID                                                          
020700        MOVE MID-IDFKNGRP-IN  TO MSGI-IDFKNGRP                            
020701        MOVE MID-KDPRODSL-IN  TO MSGI-KDPRODSL                            
020702        MOVE MID-IDLEVNR-IN   TO MSGI-IDLEVNR                             
020708     ELSE                                                                 
020709       IF MID-IDFKNGRP-IN NUMERIC                                         
020710         MOVE MID-IDFKNGRP-IN TO MSGI-IDFKNGRP                            
020711       END-IF                                                             
020712       IF MID-KDPRODSL-IN NUMERIC                                         
020713         MOVE MID-KDPRODSL-IN TO MSGI-KDPRODSL                            
020714       END-IF                                                             
020716         MOVE MID-IDLEVNR-IN  TO MSGI-IDLEVNR                             
020718     END-IF                                                               
020790                                                                          
021198     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
021343     MOVE MSGI-SPAR-AREA      TO SPAR-AREA                                
021344*---KONTROLL AV SPRÅK---*                                                 
021345     IF  MSGI-IDLAND-SPR = 'GB'                                           
021346       MOVE 'GB ' TO MED-IDSKYLT                                          
021347                     W-IDSKYLT                                            
021348     ELSE                                                                 
021349       MOVE 'S  ' TO MED-IDSKYLT                                          
021350                     W-IDSKYLT                                            
021351     END-IF                                                               
021352                                                                          
021353*---KONTROLL AV NYCKLAR---*                                               
021357     MOVE MFS-RENSA-FAELT   TO MOD-IDFKNGRP-IN                            
021358                               MOD-KDPRODSL-IN                            
021359                               MOD-IDLEVNR-IN                             
021360                                                                          
021362     IF MSGI-IDFKNGRP NUMERIC AND MSGI-IDFKNGRP > ZERO                    
021363       MOVE MSGI-IDFKNGRP   TO W-IDFKNGRP-MIN                             
021364                               W-IDFKNGRP-MAX                             
021366     ELSE                                                                 
021367       MOVE NEJ             TO NYCKLAR-SW                                 
021368     END-IF                                                               
021370                                                                          
021383     IF MSGI-KDPRODSL NUMERIC                                             
021384       IF MSGI-KDPRODSL > ZERO                                            
021385         MOVE MSGI-KDPRODSL TO W-KDPRODSL-MIN                             
021386                               W-KDPRODSL-MAX                             
021387       END-IF                                                             
021390     ELSE                                                                 
021391       MOVE NEJ           TO NYCKLAR-SW                                   
021393     END-IF                                                               
021399                                                                          
021403       IF MSGI-IDLEVNR NOT = SPACE                                        
021404         IF MSGI-KDPRODSL > ZERO                                          
021405           MOVE MSGI-IDLEVNR  TO W-IDLEVNR-MIN                            
021406                                 W-IDLEVNR-MAX                            
021407                                 W-IDLEVNR-SOK                            
021408         ELSE                                                             
021409           MOVE MSGI-IDLEVNR  TO W-IDLEVNR-SOK                            
021410         END-IF                                                           
021411       END-IF                                                             
021457     IF GODK-MID OR NYCKLAR-OK                                            
021458       MOVE MSGI-IDFKNGRP   TO MOD-IDFKNGRP-UT                            
021459       INSPECT MOD-IDFKNGRP-UT REPLACING LEADING ZERO BY SPACE            
021460                                                                          
021461       MOVE MSGI-KDPRODSL   TO MOD-KDPRODSL-UT                            
021462       INSPECT MOD-KDPRODSL-UT REPLACING LEADING ZERO BY SPACE            
021463                                                                          
021464       MOVE MSGI-IDLEVNR    TO MOD-IDLEVNR-UT                             
021466     ELSE                                                                 
021467       MOVE MFS-RENSA-FAELT TO MOD-IDFKNGRP-UT                            
021468                               MOD-KDPRODSL-UT                            
021469                               MOD-IDLEVNR-UT                             
021470     END-IF                                                               
021471                                                                          
021472     IF NYCKLAR-FEL                                                       
021473*---FÖR ATT INTE FÅ NYCKLAR FEL NÄR MAN KOMMER FRÅN EN ICKE               
021474*---GODKÄND BILD                                                          
021475       IF GODK-MID                                                        
021476         MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                               
021477         CALL WMEDKONV USING MED-WMEDAREA                                 
021478         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
021479         MOVE +1 TO INDX                                                  
021480         PERFORM UNTIL INDX > MAX-INDX                                    
021481           PERFORM MFS-RENSA-FAELT-UT                                     
021482           ADD +1 TO INDX                                                 
021483         END-PERFORM                                                      
021484       END-IF                                                             
021490     END-IF                                                               
021500                                                                          
022000     .                                                                    
022101     EJECT                                                                
022102 C-FOERSTA-SIDA SECTION.                                                  
022103                                                                          
022104     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
022105     CALL WMEDKONV USING MED-WMEDAREA                                     
022106     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
022109     .                                                                    
022110     EJECT                                                                
022111 D-NAESTA-SIDA SECTION.                                                   
022112                                                                          
022113     IF SPAR-IDTRANS = '3322'                                             
022115       MOVE SPAR-WDK6BSEQ-NEXT  TO W-WDK6BSEQ                             
022116*---ANVÄNDS FÖR ATT FÅ TRÄFF MED HJÄLP AV SÖKNING                         
022117*---PÅ LEVERANTÖRSNUMMER                                                  
022118       IF MSGI-IDLEVNR NOT = SPACE                                        
022119         MOVE MSGI-IDLEVNR TO W-IDLEVNR                                   
022120       END-IF                                                             
022121     END-IF                                                               
022125     .                                                                    
022126     EJECT                                                                
022127 E-SAMMA-SIDA SECTION.                                                    
022128                                                                          
022129     IF SPAR-IDTRANS = '3322' OR '0551'                                   
022130       MOVE SPAR-WDK6BSEQ-ENTER TO W-WDK6BSEQ                             
022142     END-IF                                                               
022146     .                                                                    
022150     EJECT                                                                
022400 F-LAES-VISA-INFO SECTION.                                                
022500                                                                          
022600*---TEST SOM UTFÖRS BEROENDE PÅ OM MAN SKALL SÖKA MED---*                 
022700*---LEVNR ELLER INTE OM MAN SÖKER MED LEVNR OCH MAN---*                   
022710*---HAR MER ÄN EN SIDAS INFO ANVÄNDER MAN SIG AV ---*                     
022711*---IMS-GU-SEQB ANNARS IMS-GET-IDLEVNR---*                                
022712     IF MSGI-IDLEVNR NOT = SPACE AND MFS-NEXT                             
022713       PERFORM IMS-GU-SEQB                                                
022714     ELSE                                                                 
022715       IF MSGI-IDLEVNR NOT = SPACE                                        
022716         PERFORM IMS-GET-IDLEVNR                                          
022717       ELSE                                                               
022718         IF MFS-NEXT OR MFS-ENTER                                         
022719           PERFORM IMS-GU-SEQB                                            
022720         ELSE                                                             
022721           PERFORM IMS-GET-SEQB                                           
022722         END-IF                                                           
022723       END-IF                                                             
022724     END-IF                                                               
022730                                                                          
022800     IF SEGMENT-SAKNAS                                                    
022901        MOVE W-WDK6BSEQ-MIN-X TO SPAR-WDK6BSEQ-ENTER                      
022910        MOVE ART-MISSING  TO MED-IDMFSFEL                                 
023000        CALL WMEDKONV USING MED-WMEDAREA                                  
023100        MOVE MED-MFSFEL   TO MOD-TEMFSFEL                                 
023110        MOVE +1 TO INDX                                                   
023200        PERFORM MFS-RENSA-FAELT-UT                                        
023300     ELSE                                                                 
023301      MOVE +1 TO INDX                                                     
023419       PERFORM UNTIL INDX > MAX-INDX                                      
023421         IF SEGMENT-FINNS                                                 
023422           IF ART-KDERS-UTG = 0                                           
023423             IF INDX = 1                                                  
023424               MOVE ART-IDFKNGRP TO W-IDFKNGRP                            
023425               MOVE ART-KDPRODSL TO W-KDPRODSL                            
023426               MOVE ART-IDLEVNR  TO W-IDLEVNR                             
023427               MOVE ART-IDARTNR  TO W-IDARTNR                             
023428               MOVE W-WDK6BSEQ   TO SPAR-WDK6BSEQ-ENTER                   
023429             END-IF                                                       
023432             MOVE ART-IDARTNR  TO WS-IDARTNR                              
023433                                  W-IDARTNR-WDD3                          
023434                                  W-IDARTNR-MIN                           
023435             INSPECT WS-IDARTNR REPLACING LEADING ZERO BY SPACE           
023436             MOVE WS-IDARTNR   TO MOD-IDARTNR (INDX)                      
023437                                                                          
023438             MOVE ART-IDLEVNR   TO WS-IDLEVNR                             
023440             MOVE WS-IDLEVNR    TO MOD-IDLEVNR (INDX)                     
023441             MOVE ART-TIURPROD  TO WS-TIURPROD                            
023442             INSPECT WS-TIURPROD REPLACING LEADING ZERO BY SPACE          
023443             MOVE WS-TIURPROD(2:4) TO MOD-TIURPROD (INDX)                 
023444             PERFORM FB-LAES-RADDATA                                      
023445           END-IF                                                         
023446         ELSE                                                             
023447           PERFORM MFS-RENSA-UT-RAD                                       
023450         END-IF                                                           
023451*---TEST SOM UTFÖRS BEROENDE PÅ OM MAN SKALL SÖKA MED---*                 
023452*---LEVNR ELLER INTE---*                                                  
023453         IF SEGMENT-FINNS                                                 
023454           IF MSGI-IDLEVNR NOT = SPACE                                    
023455             PERFORM IMS-GET-IDLEVNR                                      
023456           ELSE                                                           
023457             PERFORM IMS-GET-SEQB                                         
023458           END-IF                                                         
023459         END-IF                                                           
023460       END-PERFORM                                                        
023461                                                                          
023462       IF SEGMENT-FINNS                                                   
023463         MOVE ART-IDFKNGRP TO W-IDFKNGRP                                  
023464         MOVE ART-KDPRODSL TO W-KDPRODSL                                  
023465         MOVE ART-IDLEVNR  TO W-IDLEVNR                                   
023466         MOVE ART-IDARTNR  TO W-IDARTNR                                   
023467         MOVE W-WDK6BSEQ   TO SPAR-WDK6BSEQ-NEXT                          
023468         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
023469         CALL WMEDKONV USING MED-WMEDAREA                                 
023470         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
023471       END-IF                                                             
023472                                                                          
023473       MOVE '002'          TO MSGI-KDCALL                                 
023474       MOVE '3322'         TO SPAR-IDTRANS                                
023475       MOVE SPAR-AREA      TO MSGI-SPAR-AREA                              
023480       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
023500     END-IF                                                               
023600     .                                                                    
023700     EJECT                                                                
023800 FB-LAES-RADDATA SECTION.                                                 
023810     PERFORM IMS-GET-CLAG                                                 
023822                                                                          
023823     MOVE CLAG-VKART       TO MOD-VKART (INDX)                            
023830     MOVE CLAG-KDERS       TO WS-KDERS                                    
023831     MOVE WS-KDERS(2:2)    TO MOD-KDERS (INDX)                            
023843     MOVE CLAG-PRARTSJK    TO MOD-PRARTSJK (INDX)                         
023870                                                                          
023871*---HÄMTAR UPP ARTIKELBENÄMNING---*                                       
023873     PERFORM IMS-GET-BENA                                                 
023874     IF SEGMENT-FINNS                                                     
023880       MOVE TEXT-BEART     TO MOD-BEART (INDX)                            
023890     ELSE                                                                 
023891       MOVE 'UNKNOWN'      TO MOD-BEART (INDX)                            
023892     END-IF                                                               
023893                                                                          
023912     PERFORM DB2-SELECT-FSG2-TAB                                          
023913     IF RADER-FINNS                                                       
023918*---RÄKNAR UT TÄCKNINGSGRAD---*                                           
023919       IF FSG-SUTOTBV-RAAR = ZERO                                         
023920        OR FSG-SUARTFSG-RAAR = ZERO                                       
023921         MOVE ZERO              TO MOD-AF2-TG (INDX)                      
023922         MOVE ZERO              TO MOD-AF2-ST (INDX)                      
023923       ELSE                                                               
023924         COMPUTE WS-AF2-TG ROUNDED =                                      
023925                  (FSG-SUTOTBV-RAAR / FSG-SUARTFSG-RAAR) * 100            
023926         MOVE WS-AF2-TG         TO MOD-AF2-TG (INDX)                      
023927         MOVE FSG-SULEVANT-RAAR TO MOD-AF2-ST (INDX)                      
023928       END-IF                                                             
023929     ELSE                                                                 
023930       MOVE ZERO              TO MOD-AF2-TG (INDX)                        
023931       MOVE ZERO              TO MOD-AF2-ST (INDX)                        
023933     END-IF                                                               
023934                                                                          
023935     ADD +1 TO INDX                                                       
023940     .                                                                    
024000     EJECT                                                                
024810 MFS-RENSA-UT-RAD   SECTION.                                              
024900                                                                          
025000*    --- UTDATA-FÄLT                                                      
025210     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR (INDX)                           
025220                             MOD-IDLEVNR (INDX)                           
025221                             MOD-BEART (INDX)                             
025230                             MOD-AF2-ST (INDX)                            
025240                             MOD-VKART (INDX)                             
025250                             MOD-KDERS (INDX)                             
025260                             MOD-PRARTSJK (INDX)                          
025270                             MOD-AF2-TG (INDX)                            
025280                             MOD-TIURPROD (INDX)                          
025290     ADD +1 TO INDX                                                       
025400     .                                                                    
025501     SKIP3                                                                
025502 MFS-RENSA-FAELT-UT SECTION.                                              
025503                                                                          
025504*    --- ALLA UTDATA-FÄLT                                                 
025505*    --- INKL. BLÄDDRINGSNYCKLAR                                          
025506     PERFORM UNTIL INDX > MAX-INDX                                        
025507       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR (INDX)                         
025508                               MOD-IDLEVNR (INDX)                         
025509                               MOD-BEART (INDX)                           
025510                               MOD-AF2-ST (INDX)                          
025520                               MOD-VKART (INDX)                           
025530                               MOD-KDERS (INDX)                           
025540                               MOD-PRARTSJK (INDX)                        
025550                               MOD-AF2-TG (INDX)                          
025560                               MOD-TIURPROD (INDX)                        
025570       ADD +1 TO INDX                                                     
025580     END-PERFORM                                                          
025590     .                                                                    
025600     SKIP3                                                                
029400* --- IMS SEKTIONER ---                                                   
029500     SKIP3                                                                
029600 IMS-GET-MSG SECTION.                                                     
029700                                                                          
029800     MOVE '  QC' TO GODK-STATUSKODER                                      
029900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
030000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
030100     PERFORM IMS-STATUSKONTROLL                                           
030200     .                                                                    
030300     SKIP3                                                                
030400 IMS-INSERT-MSG SECTION.                                                  
030500                                                                          
030600     IF MSGI-IDLAND-SPR  = 'GB'                                           
030700       MOVE 'N' TO MFS-KDHUVOMR                                           
030800     END-IF                                                               
030900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
031000     MOVE SPACE TO GODK-STATUSKODER                                       
031100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
031200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031300     PERFORM IMS-STATUSKONTROLL                                           
031400     .                                                                    
031501     EJECT                                                                
031502 IMS-GET-SEQB SECTION.                                                    
031503                                                                          
031504     STRING 'WLARTC01(WDK6BSEQ>=' W-WDK6BSEQ-MIN-X                        
031505                    '&WDK6BSEQ<=' W-WDK6BSEQ-MAX-X ')'                    
031507          DELIMITED BY SIZE INTO SSA1                                     
031508     MOVE '  GE' TO GODK-STATUSKODER                                      
031509     CALL CBLTDLI USING GN  ARTC-PCB DLI-IO-WLARTC01 SSA1                 
031510     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
031511     PERFORM IMS-STATUSKONTROLL                                           
031512     .                                                                    
031513     EJECT                                                                
031526 IMS-GET-CLAG SECTION.                                                    
031527                                                                          
031528     MOVE 'WLARTC11' TO SSA1                                              
031529     MOVE '    ' TO GODK-STATUSKODER                                      
031530     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-WLARTC11 SSA1                 
031531     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
031532     PERFORM IMS-STATUSKONTROLL                                           
031533     .                                                                    
031534     EJECT                                                                
031535 IMS-GET-IDLEVNR SECTION.                                                 
031536                                                                          
031537     STRING 'WLARTC01(WDK6BSEQ>=' W-WDK6BSEQ-MIN-X                        
031538                    '&WDK6BSEQ<=' W-WDK6BSEQ-MAX-X                        
031539                    '&IDLEVNR  =' W-IDLEVNR-X ')'                         
031540          DELIMITED BY SIZE INTO SSA1                                     
031541     MOVE '  GE' TO GODK-STATUSKODER                                      
031542     CALL CBLTDLI USING GN  ARTC-PCB DLI-IO-WLARTC01 SSA1                 
031543     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
031544     PERFORM IMS-STATUSKONTROLL                                           
031545     .                                                                    
031546     EJECT                                                                
031547 IMS-GU-SEQB SECTION.                                                     
031548     STRING 'WLARTC01(WDK6BSEQ =' W-WDK6BSEQ ')'                          
031549          DELIMITED BY SIZE INTO SSA1                                     
031550     MOVE '  GE' TO GODK-STATUSKODER                                      
031551     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WLARTC01 SSA1                  
031552     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
031553     PERFORM IMS-STATUSKONTROLL                                           
031554     .                                                                    
031555     EJECT                                                                
031556 IMS-GET-BENA SECTION.                                                    
031557                                                                          
031558     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-WDD3-X ')'                    
031559          DELIMITED BY SIZE INTO SSA1                                     
031560     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
031561          DELIMITED BY SIZE INTO SSA2                                     
031562     MOVE '  GE' TO GODK-STATUSKODER                                      
031563     CALL CBLTDLI USING GU BENA-PCB DLI-IO-WLBENA01 SSA1 SSA2             
031564     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
031565     PERFORM IMS-STATUSKONTROLL                                           
031566     .                                                                    
031570     EJECT                                                                
031700 IMS-STATUSKONTROLL SECTION.                                              
031800                                                                          
031900     SET STATUS-IX TO 1                                                   
032000     SEARCH GODK-STATUS                                                   
032100       AT END                                                             
032200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
032300         DELIMITED BY SIZE INTO FELTEXT                                   
032400         CALL FELLOG                                                      
032500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
032600         CONTINUE                                                         
032700     END-SEARCH                                                           
032800     .                                                                    
032901     EJECT                                                                
032902 DB2-SELECT-FSG2-TAB  SECTION.                                            
032903                                                                          
032904     MOVE 000100  TO GODK-SQLCODEKODER                                    
032905     EXEC SQL                                                             
032906         SELECT  SULEVANT_RAAR,                                           
032907                 SUARTFSG_RAAR,                                           
032908                 SUTOTBV_RAAR                                             
032909         INTO   :FSG-SULEVANT-RAAR,                                       
032910                :FSG-SUARTFSG-RAAR,                                       
032920                :FSG-SUTOTBV-RAAR                                         
032930         FROM    FSG2                                                     
032940         WHERE   IDARTNR = :W-IDARTNR-MIN                                 
032950     END-EXEC                                                             
032960                                                                          
032970     MOVE SQLCODE TO SQLCODE-WS                                           
032980     PERFORM DB2-STATUSKONTROLL                                           
032990     .                                                                    
033000     EJECT                                                                
033100 DB2-STATUSKONTROLL  SECTION.                                             
033200                                                                          
033300     SET SQLCODE-IX TO 1                                                  
033400     SEARCH GODK-SQLCODE                                                  
033500       AT END CALL FELLOG                                                 
033600       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
033700     END-SEARCH                                                           
033800     .                                                                    
