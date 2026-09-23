000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3715B00.                                                
000300 AUTHOR.         INGVAR SKJELBRED.                                        
000400 DATE-WRITTEN.   98/03/11.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        SKAPAR UPPFÖLJNINGSUNDERLAG AV BYTESRAPPORTER SOM                
001000* SEDAN KOMMER ATT JÄMFÖRAS MOT FÖRSÄLJNINGSSTATISTIKEN                   
001100*                                                                         
001110*        PROGRAMMET LÄSER      WLXXCP (WDGX)                              
001120*                                                                         
001200*    ABENDKODER:                                                          
001300*        U0016 -  . . . .                                                 
001400*        U1000 -  . . . .                                                 
001500*                                                                         
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002301     SKIP2                                                                
002302*          --- HISTORIKFIL FÖR GAMLA BYTESRAPPORTER                       
002310     SELECT W37138                     ASSIGN TO W3715BD1.                
002316     SKIP2                                                                
002317*          --- NY UPPFÖLJNINGSFIL FÖR BYTES                               
002320     SELECT W3715B                     ASSIGN TO W3715BD2.                
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700     SKIP3                                                                
002800 FILE SECTION.                                                            
002901     SKIP3                                                                
002902 FD  W37138                                                               
002903     RECORDING       F                                                    
002904     BLOCK CONTAINS  0.                                                   
002905                                                                          
002910*01  -COPY W37138      -L.                                                
003000     EJECT                                                                
003013 FD  W3715B                                                               
003020     RECORDING       F                                                    
003030     BLOCK CONTAINS  0.                                                   
003040                                                                          
003041*01  POST -COPY W3715B -PRE  UT-  -L.                                     
003060     EJECT                                                                
003100 WORKING-STORAGE SECTION.                                                 
003101                                                                          
003110*    -- CHECKED BY WY2000                                                 
003200                                                                          
003300 77  IDPGM                       PIC X(8)    VALUE 'W3715B00'.            
003400 77  JA                          PIC X       VALUE 'J'.                   
003500 77  NEJ                         PIC X       VALUE 'N'.                   
003701                                                                          
003702 77  W37138-EOF-SW               PIC X       VALUE 'N'.                   
003710     88  END-OF-W37138                       VALUE 'J'.                   
003817                                                                          
003818 01  FILLER                      PIC X(24)   VALUE                        
003819                                 'WS-SEKTION     '.                       
003820 01  WS-SEKTION                  PIC X(30) VALUE SPACE.                   
003822                                                                          
003823 01  SPAR-REGVECKA-2000          PIC 9(7).                                
003824 01  FILLER  REDEFINES SPAR-REGVECKA-2000.                                
003825     03  FILLER                  PIC 9.                                   
003826     03  REGV-SS-2000            PIC 9(2).                                
003827     03  REGV-AA-2000            PIC 9(2).                                
003828     03  REGV-VV-2000            PIC 9(2).                                
003829                                                                          
003830 01  SPAR-TIFSGVV               PIC 9(5).                                 
003831 01  FILLER REDEFINES SPAR-TIFSGVV.                                       
003832     03  FILLER                  PIC 9.                                   
003833     03  FIRST-AA                PIC 9(2).                                
003834     03  FIRST-VV                PIC 9(2).                                
003835                                                                          
003836 01  DAGENS-SSAAVV               PIC 9(7).                                
003837 01  FILLER     REDEFINES DAGENS-SSAAVV.                                  
003840     03  FILLER                  PIC 9.                                   
003850     03  DAGENS-SS-VV            PIC 9(2).                                
003860     03  DAGENS-AA-VV            PIC 9(2).                                
003870     03  DAGENS-VV               PIC 9(2).                                
003880                                                                          
003900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004000 01  FILLER REDEFINES DAGENS-DATUM.                                       
004100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
004200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
004300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
004400     EJECT                                                                
004500 01  DYNAMISKA-SUBPROGRAM.                                                
004600*                                                                         
004700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
004710     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
004720     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
004800     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
004901     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
004910     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
005000     SKIP2                                                                
005100*    --- PARAMETRAR TILL ABEND                                            
005200                                                                          
005300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
005500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
005600     SKIP2                                                                
005700 01  FELTEXT.                                                             
005800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006000     EJECT                                                                
006100*    --- PARAMETRAR TILL DATKORT                                          
006200*                                                                         
006300 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W3715B'.              
006400     SKIP2                                                                
006500 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
006600     SKIP2                                                                
006700*01  -COPY WDATKORT                                                       
006801     EJECT                                                                
006802*    --- PARAMETRAR TILL POSTSUM                                          
006803*                                                                         
006810*01  -COPY W0005   -PRE  POSTSUM-                                         
006901     EJECT                                                                
006902 01  TEST-IDARTNR              PIC 9(9) COMP-3.                           
006903*01  FILLER -COPY WWBYT20    -RED TEST-IDARTNR                            
006904     EJECT                                                                
006905 01  SPAR-IDARTNR              PIC S9(9) VALUE ZERO COMP-3.               
006906     EJECT                                                                
006910*01  -COPY WDATAREA                                                       
007001     EJECT                                                                
007002 01  IN-AREA-START               PIC X(24)   VALUE                        
007003                                 'IN-AREA-START  '.                       
007004     SKIP2                                                                
007005                                                                          
007010*01  AREA -COPY W37138     -PRE IN-                                       
007100     EJECT                                                                
007131                                                                          
007144 01  UT-AREA-START               PIC X(24)   VALUE                        
007145                                 'UT-AREA-START  '.                       
007146     SKIP2                                                                
007147                                                                          
007150*01  AREA -COPY W3715B     -PRE UT-                                       
007160     EJECT                                                                
007170*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007180*                                                                         
007190     EJECT                                                                
007191 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007192     SKIP3                                                                
007193 01  NYCKLAR-TILL-DLI.                                                    
007194     03  W-WDGXKEY-X.                                                     
007195         05   FILLER             PIC X(4)    VALUE '3139'.                
007196         05   FILLER             PIC X(26)   VALUE LOW-VALUE.             
007197     03  W-WDGXKEY-LOW-X.                                                 
007198         05  W-IDARTNR-LOW-X     PIC S9(9)   VALUE ZERO COMP-3.           
007199         05  W-IDDISTR-LOW-X     PIC S9(5)   VALUE ZERO COMP-3.           
007200         05  W-IDTABNR-LOW-X     PIC S9(3)   VALUE ZERO COMP-3.           
007201         SKIP2                                                            
007202     03  W-WDGXKEY-HIGH-X.                                                
007203         05  W-IDARTNR-HIGH-X    PIC S9(9)   VALUE ZERO COMP-3.           
007204         05  W-IDDISTR-HIGH-X    PIC S9(5)   VALUE ZERO COMP-3.           
007205         05  W-IDTABNR-HIGH-X    PIC S9(3)   VALUE ZERO COMP-3.           
007206                                                                          
007207     SKIP2                                                                
007208*    --- STATUS-KOD FRÅN IMS                                              
007209 01  STATUS-WS                   PIC XX.                                  
007210     88  SEGMENT-FINNS                       VALUE '  '.                  
007211     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
007212     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
007213     SKIP2                                                                
007214 01  GODK-STATUSKODER.                                                    
007215     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
007216     SKIP3                                                                
007217 01  SSA1                        PIC X(64).                               
007218 01  SSA2                        PIC X(64).                               
007219     EJECT                                                                
007220*    --- IMS FUNKTIONSKODER                                               
007221*01  -COPY W0003                                                          
007222     EJECT                                                                
007223*    ---  DLI INPUT-OUTPUT AREA                                           
007224 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLXXCP11'.                    
007225 01  DLI-IO-WLXXCP11.                                                     
007226*    03  -COPY WDGX3140 -PRE XXCP-                                        
007227     EJECT                                                                
007228 LINKAGE SECTION.                                                         
007229                                                                          
007230                                                                          
007231*01  -COPY W0008  -PRE XXCP-                                              
007232     05  FILLER                  PIC X.                                   
007233     EJECT                                                                
007234 PROCEDURE DIVISION  USING XXCP-PCB.                                      
007235 MAIN SECTION.                                                            
007236     ENTRY 'DLITCBL' USING XXCP-PCB.                                      
007237                                                                          
007600                                                                          
007700     PERFORM A-INIT                                                       
007810     PERFORM S01-LAES-W37138                                              
007900     PERFORM UNTIL END-OF-W37138                                          
007910       PERFORM BA-KONV-DATE-TILL-VECKA                                    
007920       IF DAGENS-SSAAVV = SPAR-REGVECKA-2000                              
007930          IF IN-IDARTNR-OBJ > ZERO                                        
007931             MOVE IN-IDARTNR-OBJ  TO UT-IDARTNR-OBJ                       
007932                                  UT-IDARTNR                              
007933                                  TEST-IDARTNR                            
007934                                  SPAR-IDARTNR                            
007937                                                                          
007939                                                                          
007940             IF BYT20-BYTES                                               
007941             OR BYT20-RADIO                                               
007942                IF BYT20-BYTES                                            
007943                   SUBTRACT 6000 FROM UT-IDARTNR                          
007944                ELSE                                                      
007945                   IF BYT20-RADIO                                         
007946                      SUBTRACT 1000 FROM UT-IDARTNR                       
007947                   END-IF                                                 
007948                END-IF                                                    
007949             ELSE                                                         
007950               MOVE IN-IDARTNR-OBJ TO UT-IDARTNR                          
007951             END-IF                                                       
007954             PERFORM BC-HMTA-TABELLNR                                     
007955             MOVE SPAR-REGVECKA-2000 TO UT-TIFSGVV                        
007956             MOVE IN-IDDISTR   TO UT-IDDISTR                              
007957             MOVE IN-KVRETUR-URSP TO UT-KVRETUR-URSP                      
007960             MOVE IN-KVRETUR-GODK TO UT-KVRETUR-GODK                      
008010             MOVE IN-IDBYTRAP     TO UT-IDBYTRAP                          
008200             MOVE IN-IDDC         TO UT-IDDC                              
008400             MOVE IN-IDFKNGRP     TO UT-IDFKNGRP                          
008405             MOVE ZERO            TO UT-KDPRODSL                          
008406             MOVE SPACE           TO UT-BEART                             
008410             PERFORM S11-SKRIV-W3715B                                     
008500          END-IF                                                          
008600       END-IF                                                             
008610       PERFORM S01-LAES-W37138                                            
008700     END-PERFORM                                                          
008800                                                                          
008900                                                                          
009000     PERFORM Z-FINIT                                                      
009100                                                                          
009200     MOVE ZERO TO RETURN-CODE                                             
009300     GOBACK                                                               
009400     .                                                                    
009500     EJECT                                                                
009600 A-INIT SECTION.                                                          
009701                                                                          
009710     OPEN INPUT  W37138                                                   
009713                                                                          
009720     OPEN OUTPUT W3715B                                                   
009730                                                                          
009900     SKIP2                                                                
010000     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
010100     MOVE D-AAR     TO  DAGENS-DATUM-AAR                                  
010110                        DAGENS-AA-VV                                      
010200     MOVE D-MAANAD  TO  DAGENS-DATUM-MAANAD                               
010300     MOVE D-DAG     TO  DAGENS-DATUM-DAG                                  
010320     MOVE D-VECKA   TO  DAGENS-VV                                         
010330     SUBTRACT 1 FROM  DAGENS-VV                                           
010400                                                                          
010401     IF D-AAR > 60                                                        
010402        MOVE 19      TO DAGENS-SS-VV                                      
010403     ELSE                                                                 
010404        MOVE 20      TO DAGENS-SS-VV                                      
010405     END-IF                                                               
010406                                                                          
010407     DISPLAY 'DAGENS-SSAAVV ' DAGENS-SSAAVV                               
010410     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
010500     .                                                                    
010600     EJECT                                                                
010601                                                                          
010602 BA-KONV-DATE-TILL-VECKA SECTION.                                         
010604                                                                          
010605     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
010606                                                                          
010608     MOVE IN-TIREGDAT-GODK                                                
010609                                   TO DAT-I-TIDATUM                       
010610                                                                          
010611     CALL WDATKONV USING DAT-KDDATFORM                                    
010612                         DAT-I-TIDATUM                                    
010613                         DAT-O-TIDATUM                                    
010614                         DAT-KDSVAR                                       
010615                                                                          
010616     IF  DAT-KDSVAR-OK                                                    
010619         MOVE DAT-TIAA-VECKA        TO REGV-AA-2000                       
010622         MOVE DAT-TIVV              TO REGV-VV-2000                       
010625         IF REGV-AA-2000 > 60                                             
010626            MOVE 19      TO REGV-SS-2000                                  
010627         ELSE                                                             
010628            MOVE 20      TO REGV-SS-2000                                  
010629         END-IF                                                           
010630     ELSE                                                                 
010631       MOVE 'FEL I WDATKONV' TO   FELTEXT-STR                             
010632       PERFORM S99-ABEND                                                  
010633     END-IF                                                               
010634                                                                          
010635     .                                                                    
010636     EJECT                                                                
010680                                                                          
010690 BC-HMTA-TABELLNR SECTION.                                                
010692                                                                          
010693     MOVE UT-IDARTNR  TO W-IDARTNR-LOW-X                                  
010694                         W-IDARTNR-HIGH-X                                 
010695     MOVE ZERO        TO UT-IDTABNR                                       
010696     MOVE ZERO        TO W-IDDISTR-LOW-X                                  
010697     MOVE ZERO        TO W-IDTABNR-LOW-X                                  
010698     MOVE +99999      TO W-IDDISTR-HIGH-X                                 
010699     MOVE +999        TO W-IDTABNR-HIGH-X                                 
010700                                                                          
010701     PERFORM IMS-GET-ARTIKEL-WDGX                                         
010702     IF SEGMENT-FINNS                                                     
010703       MOVE XXCP-3140-IDTABNR     TO UT-IDTABNR                           
010730     END-IF                                                               
010740                                                                          
010750     .                                                                    
010760     EJECT                                                                
010770                                                                          
010800 Z-FINIT SECTION.                                                         
010810     CLOSE W37138                                                         
010820           W3715B                                                         
010901     SKIP2                                                                
010902     MOVE 'S' TO POSTSUM-OPKOD                                            
010910     CALL POSTSUM USING POSTSUM-PARM                                      
011000     .                                                                    
011101     EJECT                                                                
011102                                                                          
011103 S01-LAES-W37138  SECTION.                                                
011104     READ W37138 INTO IN-AREA                                             
011105     AT END                                                               
011106        MOVE HIGH-VALUE TO IN-AREA                                        
011107        SET END-OF-W37138 TO TRUE                                         
011108                                                                          
011109     NOT AT END                                                           
011110        MOVE 'W37138' TO POSTSUM-FDNAMN                                   
011111        MOVE 'W3715BD1' TO POSTSUM-DDNAMN2                                
011114        CALL POSTSUM USING POSTSUM-PARM                                   
011115     END-READ                                                             
011120     .                                                                    
011400     EJECT                                                                
011401                                                                          
011435 S11-SKRIV-W3715B SECTION.                                                
011437                                                                          
011438     WRITE UT-POST FROM UT-AREA                                           
011440                                                                          
011450     MOVE 'W3715B' TO POSTSUM-FDNAMN                                      
011460     MOVE 'W3715BD2' TO POSTSUM-DDNAMN2                                   
011470     CALL POSTSUM USING POSTSUM-PARM                                      
011480     .                                                                    
011490     EJECT                                                                
011500 S99-ABEND SECTION.                                                       
011600                                                                          
011701     SKIP2                                                                
011702     MOVE 'S' TO POSTSUM-OPKOD                                            
011710     CALL POSTSUM USING POSTSUM-PARM                                      
011800     CALL ABEND USING RKOD-ABEND                                          
011900     .                                                                    
012000* --- IMS SEKTIONER ---                                                   
012100                                                                          
012200     EJECT                                                                
012300                                                                          
012400 IMS-GET-ARTIKEL-WDGX SECTION.                                            
012500                                                                          
012700     STRING 'WLXXCP01(WDGXKEY  =' W-WDGXKEY-X ')'                         
012800          DELIMITED BY SIZE INTO SSA1                                     
012900     STRING 'WLXXCP11(WDGXKEY =>' W-WDGXKEY-LOW-X                         
013000                    '&WDGXKEY <=' W-WDGXKEY-HIGH-X ')'                    
013100          DELIMITED BY SIZE INTO SSA2                                     
013200                                                                          
013300     MOVE '  GE' TO GODK-STATUSKODER                                      
013400     CALL CBLTDLI USING GU XXCP-PCB DLI-IO-WLXXCP11 SSA1 SSA2             
013500     MOVE XXCP-STATUS-CODE TO STATUS-WS                                   
013600     PERFORM IMS-STATUSKONTROLL                                           
013800     .                                                                    
013900     EJECT                                                                
014000 IMS-STATUSKONTROLL SECTION.                                              
014100                                                                          
014200     SET STATUS-IX TO 1                                                   
014300     SEARCH GODK-STATUS                                                   
014400       AT END                                                             
014500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
014600           DELIMITED BY SIZE INTO FELTEXT                                 
014700         DISPLAY FELTEXT                                                  
014800         CALL FELLOG                                                      
014900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
015000         CONTINUE                                                         
015100     END-SEARCH                                                           
015200     .                                                                    
