001300 ID DIVISION.                                                             
001400 PROGRAM-ID.     W4065400.                                                
001500 AUTHOR.         BO HAMMARIN.                                             
001600 DATE-WRITTEN.   97/02/25.                                                
001700 DATE-COMPILED.                                                           
001800                                                                          
001900*    FUNKTION:                                                            
002000*                                                                         
002010*        FRÅGE-MPP SOM VISAR                                              
002020*        - DIVERSE ANTAL PER PRC                                          
002030*        - GRAND-TOTAL PÅ RAD 14 OAVSETT VILKEN SIDA MAN                  
002040*          BEFINNER SIG PÅ                                                
002041*        - ENDAST ENGELSKT FORMAT                                         
002042                                                                          
002043*        INMATNINGSREGLER                                                 
002044*        1 IDDC,     OM IFYLLT                                            
002045*                    -FRÅN SKÄRM                                          
002046*                    OM EJ IFYLLT                                         
002047*                    -FRÅN USERDB                                         
002048*                                                                         
002049*        2 TIRFSDAT, OM IFYLLT                                            
002050*                    -FRÅN SKÄRM                                          
002051*                    OM EJ IFYLLT                                         
002052*                    -FRÅN USERDB (LOKALT DAGENS-DATUM)                   
002053*                                                                         
002054*        3 IDPRC-FR, OM IFYLLT                                            
002055*                    -FRÅN SKÄRM                                          
002056*                    OM EJ IFYLLT                                         
002057*                    -VÄRDE= '000 '                                       
002058*                                                                         
002059*        4 IDPRC-TO, OM IFYLLT                                            
002060*                    -FRÅN SKÄRM                                          
002061*                    OM EJ IFYLLT OCH IDPRC-FR IFYLLT                     
002062*                    -FRÅN IDPRC-FR                                       
002063*                    OM EJ IFYLLT OCH IDPRC-FR EJ IFYLLT                  
002064*                    -KODAT VÄRDE= '9999'                                 
002065*                                                                         
002066*        LÄSNING GÖRS AV                                                  
002070*        - WLORQS/WLORQS01  (WDQ3F/WDQ3F1)                                
002071*        - WDB6  /WDB601                                                  
002080*                                                                         
002400*    INDATA.                                                              
002500*        TRANSAKTION: W4T654                                              
002600*        MID:         W4I65401                                            
002700*                                                                         
002800*    UTDATA.                                                              
002900*        MOD:         W4O65401                                            
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500 WORKING-STORAGE SECTION.                                                 
003501                                                                          
003510*    -- CHECKED BY WY2000                                                 
003600 77  IDPGM                       PIC X(08)   VALUE 'W4065400'.            
003700                                                                          
003800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003900 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
004000                                                                          
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300                                                                          
004401*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004402 77  MOD-INDX                    PIC S9(4)   VALUE +0   COMP SYNC.        
004403 77  INDX                        PIC S9(4)   VALUE +0   COMP SYNC.        
004404 77  IX1                         PIC S9(4)   VALUE +0   COMP SYNC.        
004410 77  MAX-MOD-INDX                PIC S9(4)   VALUE +12  COMP SYNC.        
004420                                                                          
004500*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005004                                                                          
005010 77  SPAR-IDPFK                  PIC X.                                   
005011     88  SPAR-ENTER                          VALUE ' '.                   
005012     88  SPAR-FIRST                          VALUE '7'.                   
005013     88  SPAR-NEXT                           VALUE '8'.                   
005014                                                                          
005020 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005100     88  NYCKLAR-OK                          VALUE 'J'.                   
005200     88  NYCKLAR-FEL                         VALUE 'N'.                   
005300                                                                          
005400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005500     88  EGEN-MID                            VALUE '4654'.                
005600     88  GODK-MID                            VALUE '4653' '4654'.         
006100     88  HELP-MID                            VALUE '0551'.                
006120 77  WS-DARFSDAT                 PIC 9(8).                                
006130 77  WS-JFR-IDPRC                PIC X(4).                                
006131 77  WS-IDPRC-FR                 PIC X(4).                                
006140 77  WS-IDPRC-TO                 PIC X(4).                                
006200     SKIP2                                                                
006201 77  WS-REPROCENT                PIC 9(3)V9(2).                           
006210     EJECT                                                                
006307*    --- AREA FÖR WORK-TABELL TILL MOD                                    
006308 01  FILLER                      PIC X(16)   VALUE 'TABELL  '.            
006309 01  WS-MOD-TABELL.                                                       
006310     03  MOD-TABELL OCCURS 12.                                            
006311         05  TAB-IDPRC               PIC X(4).                            
006312         05  TAB-KVRADER             PIC 9(5) COMP-3.                     
006313         05  TAB-KVRADER-UTSKR       PIC 9(5) COMP-3.                     
006314         05  TAB-REPROCENT-UTSKR     PIC 9(3) COMP-3.                     
006315         05  TAB-KVRADER-PACK        PIC 9(5) COMP-3.                     
006316         05  TAB-REPROCENT-PACK      PIC 9(3) COMP-3.                     
006317     EJECT                                                                
006318 01  FILLER                      PIC X(16)   VALUE 'TOTAL   '.            
006319 01  WS-TOTALER.                                                          
006322     03  TOT-KVRADER             PIC 9(5) COMP-3 VALUE ZERO.              
006323     03  TOT-KVRADER-UTSKR       PIC 9(5) COMP-3 VALUE ZERO.              
006324     03  TOT-REPROCENT-UTSKR     PIC 9(3) COMP-3 VALUE ZERO.              
006325     03  TOT-KVRADER-PACK        PIC 9(5) COMP-3 VALUE ZERO.              
006326     03  TOT-REPROCENT-PACK      PIC 9(3) COMP-3 VALUE ZERO.              
006327     EJECT                                                                
006330*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006400 01  GENERELLA-SUBPROGRAM.                                                
006500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006902     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007000     EJECT                                                                
007100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007200*01 -COPY WMEDAREA                                                        
007300     SKIP3                                                                
007400 01  MESSAGE-CODES.                                                       
007601     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
007610     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
007620     03  INF-LAST-PAGE-SHOWN     PIC X(3)    VALUE '106'.                 
007700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007800     03  ERR-RECORD-MISSING      PIC X(3)    VALUE '029'.                 
007803     EJECT                                                                
007804*01  -COPY WDATAREA                                                       
007809     EJECT                                                                
008000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008100*                                                                         
008200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008300     SKIP3                                                                
008400*01 -COPY WMSGINIT                                                        
008501     EJECT                                                                
008502*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
008503*                                                                         
008504 01  FILLER                      PIC X(16)   VALUE 'SPAR-ARE'.            
008505 01  SPAR-AREA.                                                           
008506     03  SPAR-IDTRANS           PIC X(4)    VALUE '4654'.                 
008507     03  SPAR-IDDC-ENTER        PIC X(2).                                 
008508     03  SPAR-IDDC-NEXT         PIC X(2).                                 
008509     03  SPAR-DARFSDAT-ENTER    PIC 9(8).                                 
008510     03  SPAR-DARFSDAT-NEXT     PIC 9(8).                                 
008511     03  SPAR-IDPRC-ENTER       PIC X(4).                                 
008512     03  SPAR-IDPRC-NEXT        PIC X(4).                                 
008513     03  SPAR-IDORDER-ENTER     PIC S9(7)        COMP-3.                  
008514     03  SPAR-IDORDER-NEXT      PIC S9(7)        COMP-3.                  
008515     03  SPAR-IDPRODNR-ENTER    PIC S9(7)        COMP-3.                  
008516     03  SPAR-IDPRODNR-NEXT     PIC S9(7)        COMP-3.                  
008517     03  SPAR-IDPLKLST-ENTER    PIC S9(3)        COMP-3.                  
008520     03  SPAR-IDPLKLST-NEXT     PIC S9(3)        COMP-3.                  
008530     03  SPAR-KVORDRAD          PIC S9(5)        COMP-3.                  
008540     03  SPAR-KVORDRAD-UTSKR    PIC S9(5)        COMP-3.                  
008550     03  SPAR-REPROCENT-UTSKR   PIC S9(3)        COMP-3.                  
008560     03  SPAR-KVORDRAD-PACK     PIC S9(5)        COMP-3.                  
008570     03  SPAR-REPROCENT-PACK    PIC S9(3)        COMP-3.                  
008700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008800*                                                                         
008900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009000     SKIP3                                                                
009100*01  MID -COPY W4I65401                                                   
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009400     SKIP3                                                                
009500*01  -COPY WMSGAREA                                                       
009600     EJECT                                                                
009700     03  MOD REDEFINES MSG-AREA.                                          
009800*      05  -COPY W4O65401                                                 
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
011028     03  W-WDQ3F1KY-MIN-X.                                                
011029         05  W-IDDC-MIN          PIC X(2).                                
011030         05  W-DARFSDAT-MIN      PIC 9(8).                                
011031         05  W-IDPRC-MIN         PIC X(4).                                
011032         05  W-IDORDER-MIN       PIC S9(7)  COMP-3 VALUE ZERO.            
011033         05  W-IDPRODNR-MIN      PIC S9(7)  COMP-3 VALUE ZERO.            
011034         05  W-IDPLKLST-MIN      PIC S9(3)  COMP-3 VALUE ZERO.            
011035     03  W-WDQ3F1KY-MAX-X.                                                
011036         05  W-IDDC-MAX          PIC X(2).                                
011037         05  W-DARFSDAT-MAX      PIC 9(8).                                
011038         05  W-IDPRC-MAX         PIC X(4).                                
011040         05  W-IDORDER-MAX       PIC S9(7)  COMP-3 VALUE +9999999.        
011050         05  W-IDPRODNR-MAX      PIC S9(7)  COMP-3 VALUE +9999999.        
011060         05  W-IDPLKLST-MAX      PIC S9(3)  COMP-3 VALUE +999.            
011070                                                                          
011080     03  W-IDDC-B6-X.                                                     
011090         05 W-IDDC-B6            PIC X(2).                                
011091                                                                          
011100     SKIP2                                                                
011200*    --- STATUS-KOD FRÅN IMS                                              
011300 01  STATUS-WS                   PIC XX.                                  
011400     88  SEGMENT-FINNS                       VALUE '  '.                  
011600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011610     88  BASEN-SLUT                          VALUE 'GB'.                  
011700     SKIP2                                                                
011800 01  GODK-STATUSKODER.                                                    
011900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012000     SKIP3                                                                
012100 01  SSA1                        PIC X(128).                              
012200 01  SSA2                        PIC X(64).                               
012300     EJECT                                                                
012400*    --- IMS FUNKTIONSKODER                                               
012500*01  -COPY W0003                                                          
012700     EJECT                                                                
012800*    ---  DLI INPUT-OUTPUT AREA                                           
012900                                                                          
013001 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLORQS01'.                    
013002 01  DLI-IO-WLORQS01.                                                     
013010*    03  -COPY WDQ3F1   -PRE ORQS-                                        
013020                                                                          
013030 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
013040 01   DLI-IO-AREA-B601.                                                   
013050*     03  -COPY WDB601                                                    
013300     EJECT                                                                
013400 LINKAGE SECTION.                                                         
013500*01  -COPY W0009   -PRE MSG-                                              
013600*01  -COPY W0008   -PRE USEA-                                             
013700     05  FILLER                  PIC X.                                   
013801     EJECT                                                                
013802*01  -COPY W0008  -PRE ORQS-                                              
013810     05  FILLER                  PIC X.                                   
013860                                                                          
013870*01  -COPY W0008  -PRE WDB6-                                              
013880     05  FILLER                  PIC X.                                   
013890                                                                          
013900     EJECT                                                                
014001 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB ORQS-PCB WDB6-PCB.            
014002 MAIN SECTION.                                                            
014010     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB ORQS-PCB WDB6-PCB.            
014266                                                                          
014300     PERFORM IMS-GET-MSG                                                  
014310                                                                          
014400     IF SEGMENT-FINNS                                                     
014500        PERFORM A-INIT                                                    
014610        PERFORM B-KOLLA-NYCKLAR                                           
014700        IF NYCKLAR-OK                                                     
014901           IF MFS-FIRST                                                   
014902              PERFORM C-FOERSTA-SIDA                                      
014903           ELSE                                                           
014905              IF MFS-NEXT                                                 
014906                 PERFORM D-NAESTA-SIDA                                    
014907              ELSE                                                        
014909                 PERFORM E-SAMMA-SIDA                                     
014910              END-IF                                                      
014911           END-IF                                                         
015200           PERFORM F-LAES-VISA-INFO                                       
015300        END-IF                                                            
015600        COMPUTE MSG-KVLL = LENGTH OF MOD-W4O65401 + 4                     
015700        PERFORM IMS-INSERT-MSG                                            
015800     END-IF                                                               
016000                                                                          
016100     MOVE ZERO TO RETURN-CODE                                             
016200     GOBACK                                                               
016300     .                                                                    
016400     EJECT                                                                
016500 A-INIT SECTION.                                                          
016600                                                                          
016700     IF MSG-DUBBLA-TRANSKODER                                             
016800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I65401                 
016900       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
017000       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
017100     ELSE                                                                 
017200       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W4I65401                 
017300       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
017400       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
017500     END-IF                                                               
017600                                                                          
017700     MOVE MSG-KDTRTYP                     TO MFS-KDTRTYP                  
017800     MOVE MSG-IDPFK                       TO MFS-IDPFK                    
017810                                             SPAR-IDPFK                   
017900     MOVE MFS-IDTRANS                     TO W-IDTRANS                    
018000                                                                          
018100     MOVE LOW-VALUE                       TO MSG-AREA                     
018200     MOVE 'W4O654N1'                      TO MFS-IDMOD                    
018300     MOVE '4654'                          TO MOD-IDTRANS                  
018400     MOVE MFS-RENSA-FAELT                 TO MOD-TEMFSFEL                 
018410                                             MOD-TEMFSINF                 
018500                                                                          
018600     IF EGEN-MID OR HELP-MID                                              
018700       CONTINUE                                                           
018800     ELSE                                                                 
018900       MOVE SPACE                         TO MFS-KDTRTYP                  
019000       MOVE '7'                           TO MFS-IDPFK                    
019100     END-IF                                                               
019200                                                                          
019300     MOVE 'GB'                            TO MED-IDSKYLT                  
019410     .                                                                    
019500     EJECT                                                                
019600 B-KOLLA-NYCKLAR SECTION.                                                 
019601                                                                          
019610     MOVE JA                      TO NYCKLAR-SW                           
019700                                                                          
019800     MOVE ALL '+'                 TO MSGI-WMSGINIT                        
019900     MOVE '001'                   TO MSGI-KDCALL                          
020000     MOVE MSG-LTERM-NAME          TO MSGI-IDLTERM-USER                    
020100     MOVE MSG-SIGNON-USERID       TO MSGI-IDUSER                          
020200     MOVE '4654'                  TO MSGI-IDTRANS                         
020210                                                                          
020700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
020710                                                                          
020720     MOVE MSGI-SPAR-AREA          TO SPAR-AREA                            
020730                                                                          
020800     IF GODK-MID                                                          
020801       CONTINUE                                                           
020802     ELSE                                                                 
020803       MOVE SPACE                 TO W-IDDC-MIN                           
020804                                     W-IDPRC-MIN                          
020808       MOVE ZERO                  TO W-DARFSDAT-MIN                       
020812     END-IF                                                               
020813                                                                          
021000     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
021001                             MOD-TIRFSDAT-IN                              
021002                             MOD-IDPRC-FR-IN                              
021003                             MOD-IDPRC-TO-IN                              
021004                                                                          
021022     PERFORM BA-KOLLA-IDDC                                                
021023     PERFORM BB-KOLLA-TIRFSDAT                                            
021024     PERFORM BC-KOLLA-IDPRC-FR                                            
021025     PERFORM BD-KOLLA-IDPRC-TO                                            
021026     PERFORM BE-KOLLA-SAMBAND-IDPRC                                       
021030                                                                          
021103     IF GODK-MID OR NYCKLAR-OK                                            
021105       MOVE DCS-IDDC        TO MOD-IDDC-UT                                
021106       MOVE WS-DARFSDAT (3:6) TO MOD-TIRFSDAT-UT                          
021107       MOVE WS-IDPRC-FR     TO MOD-IDPRC-FR-UT                            
021108       MOVE WS-IDPRC-TO     TO MOD-IDPRC-TO-UT                            
021115     ELSE                                                                 
021116       MOVE MFS-RENSA-FAELT TO MOD-IDDC-UT                                
021117                               MOD-TIRFSDAT-UT                            
021118                               MOD-IDPRC-FR-UT                            
021119                               MOD-IDPRC-TO-UT                            
021120     END-IF                                                               
021200                                                                          
021300     IF NYCKLAR-FEL                                                       
021410       MOVE ERR-WRONG-KEY        TO MED-IDMFSFEL                          
021500       CALL WMEDKONV USING MED-WMEDAREA                                   
021600       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
021800       PERFORM MFS-RENSA-FAELT-UT                                         
021900     END-IF                                                               
021901                                                                          
021903     IF NYCKLAR-OK                                                        
021904       MOVE DCS-IDDC             TO SPAR-IDDC-ENTER                       
021905       MOVE WS-DARFSDAT          TO SPAR-DARFSDAT-ENTER                   
021906*      IF MFS-ENTER                                                       
021907*        MOVE ZERO               TO SPAR-IDPRC-ENTER                      
021913*      ELSE                                                               
021914         MOVE WS-IDPRC-FR        TO SPAR-IDPRC-ENTER                      
021919*      END-IF                                                             
021920       MOVE ZERO                 TO SPAR-IDORDER-ENTER                    
021921                                    SPAR-IDPRODNR-ENTER                   
021922                                    SPAR-IDORDER-ENTER                    
021923                                    SPAR-IDPLKLST-ENTER                   
021930     END-IF                                                               
022100     .                                                                    
022101     SKIP2                                                                
022102 BA-KOLLA-IDDC     SECTION.                                               
022103                                                                          
022120     IF MID-IDDC-IN NOT = ALL '+'                                         
022121       MOVE MID-IDDC-IN      TO MSGI-IDDC                                 
022122     ELSE                                                                 
022123       IF SPAR-FIRST OR SPAR-NEXT                                         
022125         MOVE MID-IDDC-UT    TO MSGI-IDDC                                 
022127       END-IF                                                             
022128     END-IF                                                               
022129                                                                          
022130     IF MID-IDDC-IN NOT = ALL '+' AND EGEN-MID                            
022131       MOVE    '7'           TO MFS-IDPFK                                 
022132       MOVE    SPACE         TO MFS-KDTRTYP                               
022133     END-IF                                                               
022134                                                                          
022136     MOVE MSGI-IDDC        TO W-IDDC-B6                                   
022137     PERFORM IMS-GU-WDB601                                                
022138     IF DCS-KDDC = SPACE OR DCS-CDC-TR OR DCS-DDC                         
022139        MOVE NEJ            TO NYCKLAR-SW                                 
022140        MOVE SPACE          TO DCS-IDDC                                   
022141     ELSE                                                                 
022142        MOVE MSGI-IDDC     TO W-IDDC-MIN                                  
022143                              W-IDDC-MAX                                  
022144                              MID-IDDC-IN                                 
022146     END-IF                                                               
022150     .                                                                    
022151     EJECT                                                                
022152 BB-KOLLA-TIRFSDAT SECTION.                                               
022153                                                                          
022154     IF MID-TIRFSDAT-IN = ALL '+'                                         
022155       IF SPAR-FIRST OR SPAR-NEXT                                         
022156         MOVE MID-TIRFSDAT-UT  TO WS-DARFSDAT                             
022157       ELSE                                                               
022168         MOVE MSGI-TILOKDAT    TO WS-DARFSDAT                             
022170       END-IF                                                             
022171     ELSE                                                                 
022172       MOVE MID-TIRFSDAT-IN    TO WS-DARFSDAT                             
022173       MOVE '7'                TO MFS-IDPFK                               
022174       MOVE SPACE              TO MFS-KDTRTYP                             
022175     END-IF                                                               
022176                                                                          
022177     MOVE 'AAMMDD'             TO DAT-KDDATFORM                           
022178     MOVE WS-DARFSDAT (3:6)    TO DAT-I-TIDATUM                           
022179                                                                          
022180     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
022181                   DAT-O-TIDATUM DAT-KDSVAR                               
022182                                                                          
022183     IF DAT-KDSVAR-OK                                                     
022184       MOVE DAT-TISEKEL        TO WS-DARFSDAT (1:2)                       
022185       MOVE WS-DARFSDAT        TO W-DARFSDAT-MIN                          
022186                                  W-DARFSDAT-MAX                          
022187     ELSE                                                                 
022188       MOVE NEJ                TO NYCKLAR-SW                              
022189     END-IF                                                               
022190     .                                                                    
022191     EJECT                                                                
022192 BC-KOLLA-IDPRC-FR SECTION.                                               
022193                                                                          
022194     IF MID-IDPRC-FR-IN = ALL '+'                                         
022195       IF SPAR-FIRST OR SPAR-NEXT                                         
022196         MOVE MID-IDPRC-FR-UT TO WS-IDPRC-FR                              
022197       ELSE                                                               
022207         MOVE '000 '         TO WS-IDPRC-FR                               
022209       END-IF                                                             
022210     ELSE                                                                 
022211       MOVE MID-IDPRC-FR-IN   TO WS-IDPRC-FR                              
022212       MOVE '7'               TO MFS-IDPFK                                
022213       MOVE SPACE             TO MFS-KDTRTYP                              
022214     END-IF                                                               
022215                                                                          
022216     INSPECT WS-IDPRC-FR REPLACING LEADING SPACE BY ZERO                  
022217                                                                          
022218     IF WS-IDPRC-FR (1:3) NOT NUMERIC                                     
022219       MOVE NEJ               TO NYCKLAR-SW                               
022220     ELSE                                                                 
022221       MOVE WS-IDPRC-FR       TO W-IDPRC-MIN                              
022222     END-IF                                                               
022223     .                                                                    
022224     EJECT                                                                
022225 BD-KOLLA-IDPRC-TO SECTION.                                               
022226                                                                          
022227     IF MID-IDPRC-TO-IN = ALL '+'                                         
022228       IF SPAR-FIRST OR SPAR-NEXT                                         
022229         MOVE MID-IDPRC-TO-UT TO WS-IDPRC-TO                              
022230       ELSE                                                               
022240         MOVE '9999'          TO WS-IDPRC-TO                              
022242       END-IF                                                             
022243     ELSE                                                                 
022244       MOVE MID-IDPRC-TO-IN   TO WS-IDPRC-TO                              
022245       MOVE '7'               TO MFS-IDPFK                                
022246       MOVE SPACE             TO MFS-KDTRTYP                              
022247     END-IF                                                               
022248                                                                          
022249     INSPECT WS-IDPRC-TO REPLACING LEADING SPACE BY ZERO                  
022250                                                                          
022251     IF WS-IDPRC-TO (1:3) NOT NUMERIC                                     
022252       MOVE NEJ               TO NYCKLAR-SW                               
022253     ELSE                                                                 
022254       MOVE WS-IDPRC-TO       TO W-IDPRC-MAX                              
022255     END-IF                                                               
022256     .                                                                    
022257     EJECT                                                                
022258 BE-KOLLA-SAMBAND-IDPRC SECTION.                                          
022259                                                                          
022260     IF W-IDPRC-MIN > W-IDPRC-MAX AND                                     
022261       W-IDPRC-MAX NOT = '0000'                                           
022262       MOVE NEJ               TO NYCKLAR-SW                               
022263     ELSE                                                                 
022264       IF W-IDPRC-MAX = '0000'                                            
022265         MOVE WS-IDPRC-FR     TO WS-IDPRC-TO                              
022266         MOVE W-IDPRC-MIN     TO W-IDPRC-MAX                              
022267       END-IF                                                             
022268     END-IF                                                               
022269     .                                                                    
022270     EJECT                                                                
022271 C-FOERSTA-SIDA SECTION.                                                  
022272                                                                          
022273     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
022274     CALL WMEDKONV USING MED-WMEDAREA                                     
022275     MOVE MED-MFSINF     TO MOD-TEMFSFEL                                  
022276     .                                                                    
022277     EJECT                                                                
022278 D-NAESTA-SIDA SECTION.                                                   
022279                                                                          
022280     IF SPAR-IDTRANS = '4654'                                             
022281*    IF W-IDTRANS = '4654'                                                
022282       MOVE SPAR-IDDC-NEXT     TO W-IDDC-MIN                              
022283       MOVE SPAR-DARFSDAT-NEXT TO W-DARFSDAT-MIN                          
022284       MOVE SPAR-IDPRC-NEXT    TO W-IDPRC-MIN                             
022285       MOVE SPAR-IDORDER-NEXT  TO W-IDORDER-MIN                           
022286       MOVE SPAR-IDPRODNR-NEXT TO W-IDPRODNR-MIN                          
022287       MOVE SPAR-IDPLKLST-NEXT TO W-IDPLKLST-MIN                          
022288     ELSE                                                                 
022289       PERFORM MFS-RENSA-FAELT-IN                                         
022290     END-IF                                                               
022291     .                                                                    
022292     EJECT                                                                
022293 E-SAMMA-SIDA SECTION.                                                    
022294                                                                          
022295     IF SPAR-IDTRANS = '4654' OR '0551'                                   
022296*    IF W-IDTRANS = '4654' OR '0551'                                      
022297       MOVE SPAR-IDDC-ENTER     TO W-IDDC-MIN                             
022298       MOVE SPAR-DARFSDAT-ENTER TO W-DARFSDAT-MIN                         
022299       MOVE SPAR-IDPRC-ENTER    TO W-IDPRC-MIN                            
022300       MOVE SPAR-IDORDER-ENTER  TO W-IDORDER-MIN                          
022301       MOVE SPAR-IDPRODNR-ENTER TO W-IDPRODNR-MIN                         
022302       MOVE SPAR-IDPLKLST-ENTER TO W-IDPLKLST-MIN                         
022303       IF MID-IDDC-IN = ALL '+'                                           
022304         PERFORM MFS-RENSA-FAELT-IN                                       
022305       END-IF                                                             
022306     ELSE                                                                 
022307       PERFORM MFS-RENSA-FAELT-IN                                         
022308     END-IF                                                               
022309     .                                                                    
022310     EJECT                                                                
022400 F-LAES-VISA-INFO SECTION.                                                
022500                                                                          
022554     MOVE +0                   TO MOD-INDX                                
022555     PERFORM IMS-GN-ORQS01                                                
022556     IF SEGMENT-FINNS                                                     
022557       MOVE +1                 TO MOD-INDX                                
022558       MOVE +0                 TO TAB-KVRADER       (MOD-INDX)            
022559                                  TAB-KVRADER-UTSKR (MOD-INDX)            
022560                                  TAB-KVRADER-PACK  (MOD-INDX)            
022571         MOVE ORQS-SEQF-IDDC   TO SPAR-IDDC-NEXT                          
022572         MOVE ORQS-SEQF-DARFSDAT TO SPAR-DARFSDAT-NEXT                    
022573         MOVE ORQS-SEQF-IDPRC  TO SPAR-IDPRC-NEXT                         
022574         MOVE ORQS-SEQF-IDORDER TO SPAR-IDORDER-NEXT                      
022575         MOVE ORQS-SEQF-IDPRODNR TO SPAR-IDPRODNR-NEXT                    
022576         MOVE ORQS-SEQF-IDPLKLST TO SPAR-IDPLKLST-NEXT                    
022578     ELSE                                                                 
022579       MOVE W-IDDC-MIN         TO SPAR-IDDC-NEXT                          
022580       MOVE W-DARFSDAT-MIN     TO SPAR-DARFSDAT-NEXT                      
022581       MOVE W-IDPRC-MIN        TO SPAR-IDPRC-NEXT                         
022582       MOVE W-IDORDER-MIN      TO SPAR-IDORDER-NEXT                       
022583       MOVE W-IDPRODNR-MIN     TO SPAR-IDPRODNR-NEXT                      
022584       MOVE W-IDPLKLST-MIN     TO SPAR-IDPLKLST-NEXT                      
022585     END-IF                                                               
022586                                                                          
022587     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
022588                   BASEN-SLUT     OR                                      
022590                   MOD-INDX > MAX-MOD-INDX                                
022593       MOVE ORQS-SEQF-IDPRC      TO TAB-IDPRC (MOD-INDX)                  
022594                                    WS-JFR-IDPRC                          
022595       ADD ORQS-SEQF-KVRADER     TO TAB-KVRADER (MOD-INDX)                
022596                                    TOT-KVRADER                           
022597       IF ORQS-SEQF-KDODELSTA = 'U'                                       
022598         ADD ORQS-SEQF-KVRADER   TO TAB-KVRADER-UTSKR (MOD-INDX)          
022599                                    TOT-KVRADER-UTSKR                     
022600       ELSE                                                               
022601         IF ORQS-SEQF-KDODELSTA = 'P'                                     
022602           ADD ORQS-SEQF-KVRADER TO TAB-KVRADER-PACK (MOD-INDX)           
022603                                    TOT-KVRADER-PACK                      
022604                                    TAB-KVRADER-UTSKR (MOD-INDX)          
022605                                    TOT-KVRADER-UTSKR                     
022606         END-IF                                                           
022607       END-IF                                                             
022608                                                                          
022609       PERFORM IMS-GN-ORQS01                                              
022610       IF (SEGMENT-FINNS                       AND                        
022611           WS-JFR-IDPRC NOT = ORQS-SEQF-IDPRC) OR                         
022612          (NOT SEGMENT-FINNS)                                             
022613         IF TAB-KVRADER (MOD-INDX) > 0                                    
022614           COMPUTE WS-REPROCENT ROUNDED           =                       
022615                   TAB-KVRADER-UTSKR (MOD-INDX) /                         
022616                  (TAB-KVRADER (MOD-INDX) / 100)                          
022617           MOVE WS-REPROCENT TO TAB-REPROCENT-UTSKR (MOD-INDX)            
022618           COMPUTE WS-REPROCENT                   =                       
022619                   TAB-KVRADER-PACK  (MOD-INDX) /                         
022620                  (TAB-KVRADER (MOD-INDX) / 100)                          
022621           MOVE WS-REPROCENT TO TAB-REPROCENT-PACK  (MOD-INDX)            
022622         END-IF                                                           
022624         ADD  +1             TO MOD-INDX                                  
022625         IF MOD-INDX < +13                                                
022626           MOVE +0           TO TAB-KVRADER       (MOD-INDX)              
022627                                TAB-KVRADER-UTSKR (MOD-INDX)              
022628                                TAB-KVRADER-PACK  (MOD-INDX)              
022629         END-IF                                                           
022630       END-IF                                                             
022631     END-PERFORM                                                          
022632                                                                          
022633     PERFORM FA-SUMMERA-RESTEN                                            
022634     PERFORM FB-BYGG-DETRADER-TOTRAD                                      
023501                                                                          
023502     MOVE '002'                 TO MSGI-KDCALL                            
023503     MOVE '4654'                TO MSGI-IDTRANS                           
023504                                   SPAR-IDTRANS                           
023505                                                                          
023506     IF MFS-ENTER OR MFS-FIRST                                            
023507       MOVE TOT-KVRADER         TO SPAR-KVORDRAD                          
023508       MOVE TOT-KVRADER-UTSKR   TO SPAR-KVORDRAD-UTSKR                    
023509       MOVE TOT-REPROCENT-UTSKR TO SPAR-REPROCENT-UTSKR                   
023510       MOVE TOT-KVRADER-PACK    TO SPAR-KVORDRAD-PACK                     
023511       MOVE TOT-REPROCENT-PACK  TO SPAR-REPROCENT-PACK                    
023512     END-IF                                                               
023513                                                                          
023514     MOVE SPAR-AREA             TO MSGI-SPAR-AREA                         
023520     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
023600     .                                                                    
023700     EJECT                                                                
023800 FA-SUMMERA-RESTEN SECTION.                                               
023900                                                                          
024000     IF MOD-INDX = 13 AND                                                 
024100       SEGMENT-FINNS                                                      
024200       MOVE ORQS-SEQF-IDDC         TO SPAR-IDDC-NEXT                      
024300       MOVE ORQS-SEQF-DARFSDAT     TO SPAR-DARFSDAT-NEXT                  
024400       MOVE ORQS-SEQF-IDPRC        TO SPAR-IDPRC-NEXT                     
024500       MOVE ORQS-SEQF-IDORDER      TO SPAR-IDORDER-NEXT                   
024600       MOVE ORQS-SEQF-IDPRODNR     TO SPAR-IDPRODNR-NEXT                  
024700       MOVE ORQS-SEQF-IDPLKLST     TO SPAR-IDPLKLST-NEXT                  
024710       MOVE INF-MORE-INFO-EXISTS   TO MED-IDMFSINF                        
024720       CALL WMEDKONV USING MED-WMEDAREA                                   
024730       MOVE MED-MFSINF             TO MOD-TEMFSINF                        
024740       PERFORM UNTIL SEGMENT-SAKNAS OR                                    
024750                     BASEN-SLUT                                           
024760         ADD ORQS-SEQF-KVRADER     TO TOT-KVRADER                         
024770         IF ORQS-SEQF-KDODELSTA = 'U'                                     
024780           ADD ORQS-SEQF-KVRADER   TO TOT-KVRADER-UTSKR                   
024790         ELSE                                                             
024791           IF ORQS-SEQF-KDODELSTA = 'P'                                   
024792             ADD ORQS-SEQF-KVRADER TO TOT-KVRADER-PACK                    
024793                                      TOT-KVRADER-UTSKR                   
024794           END-IF                                                         
024795         END-IF                                                           
024796         PERFORM IMS-GN-ORQS01                                            
024797       END-PERFORM                                                        
024798     END-IF                                                               
024799     .                                                                    
024800     EJECT                                                                
024801 FB-BYGG-DETRADER-TOTRAD SECTION.                                         
024802                                                                          
024803     IF MOD-INDX > 0                                                      
024804       COMPUTE MAX-MOD-INDX = MOD-INDX - 1                                
024805       MOVE +1               TO MOD-INDX                                  
024806       PERFORM UNTIL MOD-INDX > MAX-MOD-INDX                              
024807         MOVE TAB-IDPRC   (MOD-INDX)                                      
024808                             TO MOD-IDPRC    (MOD-INDX)                   
024809         MOVE TAB-KVRADER (MOD-INDX)                                      
024810                             TO MOD-KVORDRAD (MOD-INDX)                   
024811         MOVE TAB-KVRADER-UTSKR (MOD-INDX)                                
024812                             TO MOD-KVORDRAD-UTSKR (MOD-INDX)             
024813         MOVE TAB-REPROCENT-UTSKR (MOD-INDX)                              
024814                             TO MOD-REPROCENT-UTSKR (MOD-INDX)            
024815         MOVE TAB-KVRADER-PACK (MOD-INDX)                                 
024816                             TO MOD-KVORDRAD-PACK (MOD-INDX)              
024817         MOVE TAB-REPROCENT-PACK (MOD-INDX)                               
024818                             TO MOD-REPROCENT-PACK (MOD-INDX)             
024819         ADD +1 TO MOD-INDX                                               
024820       END-PERFORM                                                        
024821     END-IF                                                               
024822                                                                          
024823     IF MOD-INDX < 13 AND                                                 
024824        MFS-NEXT                                                          
024825       MOVE INF-LAST-PAGE-SHOWN TO MED-IDMFSFEL                           
024826       CALL WMEDKONV USING MED-WMEDAREA                                   
024827       MOVE MED-MFSFEL          TO MOD-TEMFSFEL                           
024828     END-IF                                                               
024829                                                                          
024830     IF MOD-INDX = 0                                                      
024831       MOVE ERR-RECORD-MISSING  TO MED-IDMFSFEL                           
024832       CALL WMEDKONV USING MED-WMEDAREA                                   
024833       MOVE MED-MFSFEL          TO MOD-TEMFSFEL                           
024834       MOVE SPACE               TO MOD-TEMFSINF                           
024835     ELSE                                                                 
024836       IF TOT-KVRADER > 0                                                 
024837         COMPUTE WS-REPROCENT ROUNDED           =                         
024838                 TOT-KVRADER-UTSKR /                                      
024839                (TOT-KVRADER / 100)                                       
024840         MOVE WS-REPROCENT      TO TOT-REPROCENT-UTSKR                    
024841         COMPUTE WS-REPROCENT                   =                         
024842                 TOT-KVRADER-PACK /                                       
024843                (TOT-KVRADER / 100)                                       
024844         MOVE WS-REPROCENT      TO TOT-REPROCENT-PACK                     
024845       END-IF                                                             
024846                                                                          
024847       IF MFS-ENTER OR MFS-FIRST                                          
024848         MOVE TOT-KVRADER          TO MOD-KVORDRAD-TOT                    
024849         MOVE TOT-KVRADER-UTSKR    TO MOD-KVORDRAD-UTSKR-TOT              
024850         MOVE TOT-REPROCENT-UTSKR  TO MOD-REPROCENT-UTSKR-TOT             
024851         MOVE TOT-KVRADER-PACK     TO MOD-KVORDRAD-PACK-TOT               
024852         MOVE TOT-REPROCENT-PACK   TO MOD-REPROCENT-PACK-TOT              
024853       ELSE                                                               
024854         MOVE SPAR-KVORDRAD        TO MOD-KVORDRAD-TOT                    
024855         MOVE SPAR-KVORDRAD-UTSKR  TO MOD-KVORDRAD-UTSKR-TOT              
024856         MOVE SPAR-REPROCENT-UTSKR TO MOD-REPROCENT-UTSKR-TOT             
024857         MOVE SPAR-KVORDRAD-PACK   TO MOD-KVORDRAD-PACK-TOT               
024858         MOVE SPAR-REPROCENT-PACK  TO MOD-REPROCENT-PACK-TOT              
024859       END-IF                                                             
024860     END-IF                                                               
024861     .                                                                    
024862     EJECT                                                                
024870 MFS-RENSA-FAELT-UT SECTION.                                              
024900                                                                          
025000     MOVE 1 TO IX1                                                        
025100     PERFORM UNTIL IX1 > MAX-MOD-INDX                                     
025200        MOVE MFS-RENSA-FAELT TO MOD-IDPRC           (IX1)                 
025300                                MOD-KVORDRAD        (IX1)                 
025310                                MOD-KVORDRAD-UTSKR  (IX1)                 
025311                                MOD-REPROCENT-UTSKR (IX1)                 
025312                                MOD-KVORDRAD-PACK   (IX1)                 
025313                                MOD-REPROCENT-PACK  (IX1)                 
025370        ADD 1 TO IX1                                                      
025380     END-PERFORM                                                          
025390     MOVE MFS-RENSA-FAELT    TO MOD-KVORDRAD-TOT                          
025391                                MOD-KVORDRAD-UTSKR-TOT                    
025392                                MOD-REPROCENT-UTSKR-TOT                   
025393                                MOD-KVORDRAD-PACK-TOT                     
025394                                MOD-REPROCENT-PACK-TOT                    
025400     .                                                                    
025501     SKIP3                                                                
025700 MFS-RENSA-FAELT-IN SECTION.                                              
025800                                                                          
025900*    --- ALLA INDATA-FÄLT                                                 
026000     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
026100                             MOD-IDDC-UT                                  
026110                             MOD-TIRFSDAT-IN                              
026120                             MOD-TIRFSDAT-UT                              
026130                             MOD-IDPRC-FR-IN                              
026140                             MOD-IDPRC-FR-UT                              
026150                             MOD-IDPRC-TO-IN                              
026160                             MOD-IDPRC-TO-UT                              
026200     .                                                                    
026300     EJECT                                                                
029400*---- IMS SEKTIONER ---                                                   
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
030600     IF MSGI-IDLAND-SPR = 'GB'                                            
030700       MOVE 'N' TO MFS-KDHUVOMR                                           
030800     END-IF                                                               
030900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
031000     MOVE SPACE TO GODK-STATUSKODER                                       
031100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
031200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031300     PERFORM IMS-STATUSKONTROLL                                           
031400     .                                                                    
031501     EJECT                                                                
031502 IMS-GN-ORQS01    SECTION.                                                
031503                                                                          
031504     STRING 'WLORQS01(WDQ3F1KY>=' W-WDQ3F1KY-MIN-X                        
031505                    '&WDQ3F1KY<=' W-WDQ3F1KY-MAX-X ')'                    
031510          DELIMITED BY SIZE INTO SSA1                                     
031511     MOVE '  GEGB' TO GODK-STATUSKODER                                    
031512     CALL CBLTDLI USING GN ORQS-PCB DLI-IO-WLORQS01 SSA1                  
031513     MOVE ORQS-STATUS-CODE TO STATUS-WS                                   
031514     PERFORM IMS-STATUSKONTROLL                                           
031515     .                                                                    
031516 IMS-GU-WDB601    SECTION.                                                
031517     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
031518          DELIMITED BY SIZE INTO SSA1                                     
031519     MOVE '  GE' TO GODK-STATUSKODER                                      
031520     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
031530     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
031540     PERFORM IMS-STATUSKONTROLL                                           
031550     IF SEGMENT-SAKNAS                                                    
031560         MOVE SPACE TO DCS-KDDC                                           
031570     END-IF                                                               
031580     .                                                                    
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
