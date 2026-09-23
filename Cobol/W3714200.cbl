000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W3714200.                                                
000400*AUTHOR.         RONNY STENHOLM.                                          
000500*DATE-WRITTEN.   92/08/26.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        ALLA RAPPORTER MED STATUS 2 OCH REGISTRERINGSDATUM               
001100*        ÄLDRE ÄN 12  MÅNADER NOLL STÄLLES OCH SÄTTS I STATUS 4           
001200*                                                                         
001300*        PROGRAMMET UPPDATERAR WLBYTF (WDM6)                              
001400*                                                                         
001500*    ABENDKODER:                                                          
001600*        U0016 -  . . . .                                                 
001700*        U1000 -  . . . .                                                 
001800*                                                                         
001810*    CHANGE LOG:                                                          
001820*                                                                         
001830*    DIGAMBAR/20021003                                                    
001840*    WDM6E INDEX IS CHANGED TO REFER THE IDBYTRAP-9KOMPL INSTEAD          
001850*    OF THE IDBYTRAP. THIS IS TO SHOW THE DETAILS IN DESCENDING           
001860*    ORDER OF THE IDBYTRAP.IDBYTRAP-9KOMPL FIELD IS ADDED IN              
001870*    WDM611                                                               
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- FIL MED NYCKLAR FÖR ATT TA BORT RAPPORTERNA                
002800     SELECT W37125                     ASSIGN TO W37142D1.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W37125                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700     SKIP2                                                                
003800*01  -COPY W37125        -L.                                              
004000 WORKING-STORAGE SECTION.                                                 
004100     SKIP2                                                                
004101*    -- CHECKED BY WY2000                                                 
004110     SKIP3                                                                
004200 77  IDPGM                       PIC X(8)    VALUE 'W3714200'.            
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004410 77  WS-KDBYTBEK                 PIC X       VALUE ' '.                   
004500     SKIP2                                                                
004510 01  CHKP-VAR.                                                            
004520 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
004530 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
004540 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
004550 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
004551 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
004553 03  CHKP-MAX                    PIC S9(3)   VALUE +100.                  
004590     SKIP2                                                                
004600 01  FELTEXT.                                                             
004700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
004800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
004900                                                                          
005000 77  W37125-EOF-SW               PIC X       VALUE 'N'.                   
005100     88  END-OF-W37125                       VALUE 'J'.                   
005101                                                                          
005102 77  W-IDBYTRAP-9KOMPL           PIC S9(7)   VALUE ZERO COMP-3.           
005103 77  W-9KOMPL                    PIC  9(7)   VALUE 9999999.               
005104                                                                          
005105 01  FILLER                  PIC X(16)   VALUE 'WS-SEKTION'.              
005106 01  WS-SEKTION                  PIC X(30)   VALUE SPACE.                 
005107 01  FILLER                  PIC X(16)   VALUE 'WS-IMS-SEKTION'.          
005108 01  WS-IMS-SEKTION              PIC X(30)   VALUE SPACE.                 
005109 01  FILLER                  PIC X(16)   VALUE 'WS-FIL-SEKTION'.          
005110 01  WS-FIL-SEKTION              PIC X(30)   VALUE SPACE.                 
005111                                                                          
005120                                                                          
005200     EJECT                                                                
005300 01  DAGENS-DATUM                PIC 9(8)    VALUE ZERO.                  
005400 01  FILLER REDEFINES DAGENS-DATUM.                                       
005500     03  DAGENS-DATUM-AAAA       PIC 9(4).                                
005600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006500     EJECT                                                                
006600 01  DYNAMISKA-SUBPROGRAM.                                                
006700*                                                                         
006800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007200     EJECT                                                                
007300*    - - - - - - - - - - - - - - PARAMETRAR TILL POSTSUM                  
007400*                                                                         
007500*01  -COPY W0005   -PRE  POSTSUM-                                         
007600     EJECT                                                                
008100 01  IN-AREA-START               PIC X(24)   VALUE                        
008200                                             'IN-AREA-START'.             
008300     SKIP2                                                                
008400                                                                          
008500*01  AREA -COPY W37125       -PRE IN-                                     
008600*                                                                         
008700     EJECT                                                                
008710 01  TEST-IDARTNR              PIC 9(9) COMP-3.                           
008720*01  FILLER -COPY WWBYT19    -RED TEST-IDARTNR                            
008730     EJECT                                                                
008800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008900     SKIP3                                                                
009000 01  NYCKLAR-TILL-DLI.                                                    
009100     03  W-WDM601KY-X.                                                    
009110         05  W-IDDISTR           PIC S9(5)    VALUE ZERO COMP-3.          
009200         05  W-IDBYTRAP          PIC S9(7)    VALUE ZERO COMP-3.          
009300                                                                          
009400     03  W-IDBYTRAD-X.                                                    
009410         05  W-IDBYTRAD          PIC S9(5)   VALUE ZERO COMP-3.           
009420                                                                          
009430     03  W-IDBYTRAD-MIN-X.                                                
009440         05  W-IDBYTRAD-MIN      PIC S9(5)   VALUE ZERO COMP-3.           
009450                                                                          
009460     03  W-IDBYTRAD-MAX-X.                                                
009470         05  W-IDBYTRAD-MAX      PIC S9(5)   VALUE +99999 COMP-3.         
009480                                                                          
009490                                                                          
009491     03  W-WDM611KY-X.                                                    
009492         05  W-IDARTNR-OBJ       PIC S9(9)   VALUE ZERO COMP-3.           
009493       04    W-IDTABNR-X.                                                 
009494         05  W-IDTABNR           PIC S9(3)   VALUE ZERO COMP-3.           
009495                                                                          
009496     03  W-WDGXKEY-X.                                                     
009497         05   FILLER             PIC X(4)    VALUE '3139'.                
009498         05   FILLER             PIC X(26)   VALUE LOW-VALUE.             
009499     03  W-WDGXKEY-LOW-X.                                                 
009500         05  W-IDARTNR-LOW-X     PIC S9(9)   VALUE ZERO COMP-3.           
009501         05  W-IDDISTR-LOW-X     PIC S9(5)   VALUE ZERO COMP-3.           
009502         05  W-IDTABNR-LOW-X     PIC S9(3)   VALUE ZERO COMP-3.           
009503         SKIP2                                                            
009504     03  W-WDGXKEY-HIGH-X.                                                
009505         05  W-IDARTNR-HIGH-X    PIC S9(9)   VALUE ZERO COMP-3.           
009506         05  W-IDDISTR-HIGH-X    PIC S9(5)   VALUE ZERO COMP-3.           
009507         05  W-IDTABNR-HIGH-X    PIC S9(3)   VALUE ZERO COMP-3.           
009508                                                                          
009509     SKIP2                                                                
009510     SKIP2                                                                
009600*    --- STATUS-KOD FRÅN IMS                                              
009700 01  STATUS-WS                   PIC XX.                                  
009800     88  SEGMENT-FINNS                       VALUE '  '.                  
009900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
010000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010200     88  IMS-EJ-OK                           VALUE 'XD'.                  
010300     SKIP2                                                                
010400 01  GODK-STATUSKODER.                                                    
010500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010600     SKIP3                                                                
010700 01  SSA1                        PIC X(64).                               
010800 01  SSA2                        PIC X(64).                               
010900     EJECT                                                                
011000*    --- IMS FUNKTIONSKODER                                               
011100*01  -COPY W0003                                                          
011200     EJECT                                                                
011300*    ---  DLI INPUT-OUTPUT AREA                                           
011510 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLBYTF01'.                    
011511 01  DLI-IO-WLBYTF01.                                                     
011514*    03  -COPY WDM601  -PRE BYTF-                                         
011515     EJECT                                                                
011516 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLBYTF11'.                    
011517 01  DLI-IO-WLBYTF11.                                                     
011518*    03  -COPY WDM611  -PRE BYTF-                                         
012100     SKIP3                                                                
012200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLXXCP11'.                    
012300 01  DLI-IO-WLXXCP11.                                                     
012310*    03  -COPY WDGX3140 -PRE XXCP-                                        
012400     EJECT                                                                
012500 LINKAGE SECTION.                                                         
012600                                                                          
012700*01  -COPY W0009   -PRE MSG-                                              
012810     EJECT                                                                
012900*01  -COPY W0008  -PRE BYTF-                                              
013000     05  FILLER                  PIC X.                                   
013010*01  -COPY W0008  -PRE XXCP-                                              
013020     05  FILLER                  PIC X.                                   
013100     EJECT                                                                
013200 PROCEDURE DIVISION  USING MSG-PCB BYTF-PCB XXCP-PCB.                     
013300     ENTRY 'DLITCBL' USING MSG-PCB BYTF-PCB XXCP-PCB.                     
013400                                                                          
013500     SKIP2                                                                
013510*                                                                         
013600     PERFORM A-INIT                                                       
013610*                                                                         
013710     PERFORM S01-LAES-W37125                                              
013800     PERFORM UNTIL END-OF-W37125                                          
013810       IF CHKP-ANT > CHKP-MAX                                             
013820          PERFORM S02-TAG-CHECKPOINT                                      
013830       END-IF                                                             
013900       PERFORM B-UPPDATERA                                                
014500       PERFORM S01-LAES-W37125                                            
014600     END-PERFORM                                                          
014610*                                                                         
014700                                                                          
014800                                                                          
014900     PERFORM Z-FINIT                                                      
015000                                                                          
015100     MOVE ZERO TO RETURN-CODE                                             
015200     GOBACK                                                               
015300     .                                                                    
015400     EJECT                                                                
015500 A-INIT SECTION.                                                          
015510     MOVE 'A-INIT'         TO WS-SEKTION                                  
015600     SKIP2                                                                
015700                                                                          
015800     OPEN INPUT W37125                                                    
015900                                                                          
016100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016110     PERFORM IMS-RESTART                                                  
016111     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM                     
016150                                                                          
016200     .                                                                    
016210     EJECT                                                                
016300                                                                          
016400                                                                          
019301 B-UPPDATERA SECTION.                                                     
019303     MOVE 'B-UPPDATERA'    TO WS-SEKTION                                  
019304     SKIP2                                                                
019305                                                                          
019308     MOVE IN-IDDISTR       TO W-IDDISTR                                   
019309     MOVE IN-IDBYTRAP      TO W-IDBYTRAP                                  
019310     COMPUTE W-IDBYTRAP-9KOMPL = W-9KOMPL -  W-IDBYTRAP                   
019311                                                                          
019312     PERFORM IMS-GU-BYTF01                                                
019313                                                                          
019314     IF SEGMENT-FINNS                                                     
019317          PERFORM IMS-GHN-BYTF11                                          
019318          PERFORM UNTIL SEGMENT-SAKNAS                                    
019319                  OR  SEGMENT-SLUT                                        
019322            IF BYTF-OBJ-IDBYTRAD < 100                                    
019324               MOVE ZERO       TO BYTF-OBJ-KVRETUR-GODK                   
019325               MOVE 'C'        TO BYTF-OBJ-KDBYTSTA-OBJ                   
019326               PERFORM IMS-REPL-BYTF11                                    
019327               ADD +1 TO  CHKP-ANT                                        
019328******** OLD VIPS USER ********                                           
019329            ELSE                                                          
019331******** NEW VIPS USER ********                                           
019334               IF BYTF-OBJ-IDARTNR-OBJ > ZERO                             
019365                  MOVE ZERO       TO BYTF-OBJ-KVRETUR-GODK                
019366                  MOVE 'C'        TO BYTF-OBJ-KDBYTSTA-OBJ                
019367                  PERFORM IMS-REPL-BYTF11                                 
019368                  ADD +1          TO BYTF-OBJ-IDBYTRAD                    
019369                  MOVE BYTF-OBJ-KVRETUR-URSP                              
019370                             TO BYTF-OBJ-KVRETUR-GODK                     
019371                  MOVE ZERO       TO BYTF-OBJ-KVRETUR-URSP                
019372                  MOVE '029'      TO BYTF-OBJ-KDBYTREF                    
019373                  MOVE 'N'        TO BYTF-OBJ-KDBYTSTA-OBJ                
019374                  MOVE SPACE      TO BYTF-OBJ-KDBYTSTA-AVL                
019375                  MOVE SPACE      TO BYTF-OBJ-FLSKROT                     
019376                  MOVE W-IDBYTRAP-9KOMPL TO                               
019377                                     BYTF-OBJ-IDBYTRAP-9KOMPL             
019378                  PERFORM IMS-ISRT-BYTF11                                 
019379                  ADD +2 TO  CHKP-ANT                                     
019380               ELSE                                                       
019381                  PERFORM IMS-GET-WDGX01                                  
019384                  MOVE BYTF-OBJ-IDTABNR TO W-IDTABNR                      
019390                  MOVE ZERO       TO BYTF-OBJ-KVRETUR-GODK                
019391                  MOVE 'C'        TO BYTF-OBJ-KDBYTSTA-OBJ                
019393                  PERFORM IMS-REPL-BYTF11                                 
019394                  PERFORM IMS-GN-ARTIKEL-WDGX                             
019395                  IF SEGMENT-FINNS                                        
019398                     ADD +1          TO BYTF-OBJ-IDBYTRAD                 
019399                     MOVE XXCP-3140-IDARTNR-BYT TO                        
019400                                      BYTF-OBJ-IDARTNR-OBJ                
019401                                      TEST-IDARTNR                        
019403                     IF BYT19-BYTES                                       
019404                     OR BYT19-RADIO                                       
019405                        IF BYT19-BYTES                                    
019406                           ADD +6000 TO BYTF-OBJ-IDARTNR-OBJ              
019407                        ELSE                                              
019408                           IF BYT19-RADIO                                 
019409                              ADD +1000 TO BYTF-OBJ-IDARTNR-OBJ           
019410                           END-IF                                         
019411                        END-IF                                            
019412                     END-IF                                               
019413                     MOVE BYTF-OBJ-KVRETUR-URSP                           
019414                                     TO BYTF-OBJ-KVRETUR-GODK             
019415                     MOVE ZERO       TO BYTF-OBJ-KVRETUR-URSP             
019416                     MOVE ZERO       TO BYTF-OBJ-IDTABNR                  
019417                     MOVE '029'      TO BYTF-OBJ-KDBYTREF                 
019418                     MOVE 'N'        TO BYTF-OBJ-KDBYTSTA-OBJ             
019419                     MOVE SPACE      TO BYTF-OBJ-KDBYTSTA-AVL             
019420                     MOVE SPACE      TO BYTF-OBJ-FLSKROT                  
019421                     MOVE W-IDBYTRAP-9KOMPL TO                            
019422                                     BYTF-OBJ-IDBYTRAP-9KOMPL             
019423                     PERFORM IMS-ISRT-BYTF11                              
019424                     ADD +2 TO  CHKP-ANT                                  
019425                  END-IF                                                  
019426               END-IF                                                     
019427            END-IF                                                        
019428            PERFORM IMS-GHN-BYTF11                                        
019429          END-PERFORM                                                     
019430          PERFORM IMS-GHU-BYTF                                            
019431          MOVE '4'             TO BYTF-RAPP-KDBYTSTA-RAPP                 
019432          MOVE 'J'             TO BYTF-RAPP-FLBYGODK                      
019433          MOVE DAGENS-DATUM    TO BYTF-RAPP-DAREGDAT-GODK                 
019434                                  BYTF-RAPP-DAANKDAG                      
019435          MOVE SPACE           TO BYTF-RAPP-ADBYTANK                      
019436          PERFORM IMS-REPL-BYTF                                           
019438     END-IF                                                               
019439     .                                                                    
019440     EJECT                                                                
019441                                                                          
019442 Z-FINIT SECTION.                                                         
019450     MOVE 'Z-FINIT'        TO WS-SEKTION                                  
019500                                                                          
019600                                                                          
019700     CLOSE W37125                                                         
019800     SKIP2                                                                
019900     MOVE 'S' TO POSTSUM-OPKOD                                            
020000     CALL POSTSUM USING POSTSUM-PARM                                      
020100     .                                                                    
020200     EJECT                                                                
020300 S01-LAES-W37125  SECTION.                                                
020310     MOVE 'S01-LAES-W37125' TO WS-FIL-SEKTION                             
020400     SKIP2                                                                
020500     READ W37125 INTO IN-AREA                                             
020600     AT END                                                               
020800        SET END-OF-W37125 TO TRUE                                         
020900                                                                          
021000     NOT AT END                                                           
021100        MOVE 'W37125' TO POSTSUM-FDNAMN                                   
021200        MOVE 'W37142D1' TO POSTSUM-DDNAMN2                                
021300        MOVE 'STAT' TO POSTSUM-TRANSTYP                                   
021400        CALL POSTSUM USING POSTSUM-PARM                                   
021500     END-READ                                                             
021600     .                                                                    
021700     EJECT                                                                
021710 S02-TAG-CHECKPOINT SECTION.                                              
021711     MOVE 'S02-TAG-CHECKPOINT' TO WS-FIL-SEKTION                          
021717                                                                          
021720     PERFORM IMS-CHECKPOINT                                               
021722     MOVE ZERO TO  CHKP-ANT                                               
021723                                                                          
021725     .                                                                    
021730     EJECT                                                                
021800* --- IMS SEKTIONER ---                                                   
021900     SKIP3                                                                
022000     EJECT                                                                
022010 IMS-GU-BYTF01 SECTION.                                                   
022011     MOVE 'IMS-GU-BYTF01'     TO WS-IMS-SEKTION                           
022020     STRING 'WLBYTF01(WDM601KY =' W-WDM601KY-X ')'                        
022030          DELIMITED BY SIZE INTO SSA1                                     
022040     MOVE '  GE' TO GODK-STATUSKODER                                      
022050     CALL CBLTDLI USING GU BYTF-PCB DLI-IO-WLBYTF01 SSA1                  
022060     MOVE BYTF-STATUS-CODE TO STATUS-WS                                   
022070     PERFORM IMS-STATUSKONTROLL                                           
022080     .                                                                    
022090     SKIP3                                                                
022100 IMS-GHU-BYTF SECTION.                                                    
022110     MOVE 'IMS-GHU-BYTF'       TO WS-IMS-SEKTION                          
022200     STRING 'WLBYTF01(WDM601KY =' W-WDM601KY-X ')'                        
022300          DELIMITED BY SIZE INTO SSA1                                     
022400     MOVE '  GE' TO GODK-STATUSKODER                                      
022500     CALL CBLTDLI USING GHU BYTF-PCB DLI-IO-WLBYTF01 SSA1                 
022600     MOVE BYTF-STATUS-CODE TO STATUS-WS                                   
022700     PERFORM IMS-STATUSKONTROLL                                           
022800     .                                                                    
022900     EJECT                                                                
023000 IMS-GHN-BYTF11 SECTION.                                                  
023004     MOVE 'IMS-GHN-BYTF11'       TO WS-IMS-SEKTION                        
023005     STRING 'WLBYTF11(IDBYTRAD >' W-IDBYTRAD-MIN-X                        
023006                    '&IDBYTRAD <' W-IDBYTRAD-MAX-X ')'                    
023007          DELIMITED BY SIZE INTO SSA1                                     
023030     MOVE '  GEGB' TO GODK-STATUSKODER                                    
023040     CALL CBLTDLI USING GHNP BYTF-PCB DLI-IO-WLBYTF11 SSA1                
023050     MOVE BYTF-STATUS-CODE TO STATUS-WS                                   
023060     PERFORM IMS-STATUSKONTROLL                                           
023100     .                                                                    
023200     EJECT                                                                
024601 IMS-REPL-BYTF SECTION.                                                   
024603     MOVE 'IMS-REPL-BYTF'        TO WS-IMS-SEKTION                        
024604                                                                          
024605     MOVE '  ' TO GODK-STATUSKODER                                        
024606     CALL CBLTDLI USING REPL BYTF-PCB DLI-IO-WLBYTF01                     
024608     MOVE BYTF-STATUS-CODE TO STATUS-WS                                   
024609     PERFORM IMS-STATUSKONTROLL                                           
024610     .                                                                    
024611     SKIP3                                                                
024612 IMS-REPL-BYTF11 SECTION.                                                 
024614     MOVE 'IMS-REPL-BYTF11'      TO WS-IMS-SEKTION                        
024615                                                                          
024616     MOVE '  ' TO GODK-STATUSKODER                                        
024618     CALL CBLTDLI USING REPL BYTF-PCB DLI-IO-WLBYTF11                     
024619     MOVE BYTF-STATUS-CODE TO STATUS-WS                                   
024620     PERFORM IMS-STATUSKONTROLL                                           
024621     .                                                                    
024622     SKIP3                                                                
024623 IMS-ISRT-BYTF11 SECTION.                                                 
024625     MOVE 'IMS-ISRT-BYTF11'      TO WS-IMS-SEKTION                        
024628     STRING 'WLBYTF01(WDM601KY =' W-WDM601KY-X ')'                        
024629                      DELIMITED BY SIZE INTO SSA1                         
024630     MOVE 'WLBYTF11 ' TO SSA2                                             
024631     MOVE '  ' TO GODK-STATUSKODER                                        
024632     CALL CBLTDLI USING ISRT BYTF-PCB DLI-IO-WLBYTF11 SSA1 SSA2           
024634     MOVE BYTF-STATUS-CODE TO STATUS-WS                                   
024635     PERFORM IMS-STATUSKONTROLL                                           
024636     .                                                                    
024637     SKIP3                                                                
024638 IMS-GET-WDGX01 SECTION.                                                  
024639     MOVE 'IMS-GET-WDGX01'      TO WS-IMS-SEKTION                         
024640     STRING 'WLXXCP01(WDGXKEY  =' W-WDGXKEY-X ')'                         
024641          DELIMITED BY SIZE INTO SSA1                                     
024642     MOVE '  ' TO GODK-STATUSKODER                                        
024643     CALL CBLTDLI USING GU XXCP-PCB DLI-IO-WLXXCP11 SSA1                  
024644     MOVE XXCP-STATUS-CODE TO STATUS-WS                                   
024645     PERFORM IMS-STATUSKONTROLL                                           
024646     .                                                                    
024647     EJECT                                                                
024648 IMS-GN-ARTIKEL-WDGX SECTION.                                             
024649     MOVE 'IMS-GET-ARTIKEL-WDGX' TO WS-IMS-SEKTION                        
024653     STRING 'WLXXCP11(IDTABNR = ' W-IDTABNR-X ')'                         
024654          DELIMITED BY SIZE INTO SSA1                                     
024656     MOVE '  GEGB' TO GODK-STATUSKODER                                    
024657     CALL CBLTDLI USING GN XXCP-PCB DLI-IO-WLXXCP11 SSA1                  
024659     MOVE XXCP-STATUS-CODE TO STATUS-WS                                   
024660     PERFORM IMS-STATUSKONTROLL                                           
024661     .                                                                    
024662     EJECT                                                                
024663 IMS-RESTART SECTION.                                                     
024664     MOVE 'IMS-RESTART'          TO WS-IMS-SEKTION                        
024665     SKIP2                                                                
024666     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
024667     MOVE '  ' TO GODK-STATUSKODER                                        
024668     CALL CBLTDLI USING XRST MSG-PCB                                      
024669                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
024670                        CHKP-AREA-LENGTH CHKP-AREA                        
024680     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
024690     PERFORM IMS-STATUSKONTROLL                                           
024691     .                                                                    
024697 IMS-CHECKPOINT SECTION.                                                  
024698     MOVE 'IMS-CHECKPOINT'       TO WS-IMS-SEKTION                        
024699     SKIP2                                                                
024700     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
024701     MOVE '  XD' TO GODK-STATUSKODER                                      
024702     CALL CBLTDLI USING CHKP MSG-PCB                                      
024703                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
024704                        CHKP-AREA-LENGTH CHKP-AREA                        
024705     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
024707     PERFORM IMS-STATUSKONTROLL                                           
024708                                                                          
024709     IF IMS-EJ-OK                                                         
024710       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
024711       DISPLAY FELTEXT                                                    
024712       CALL FELLOG                                                        
024713     END-IF                                                               
024716     .                                                                    
024717     EJECT                                                                
024740 IMS-STATUSKONTROLL SECTION.                                              
024800     SKIP2                                                                
024900     SET STATUS-IX TO 1                                                   
025000     SEARCH GODK-STATUS                                                   
025100       AT END                                                             
025200         MOVE 'XXXIMSABENDXXX' TO FELTEXT-STR                             
025300         DISPLAY FELTEXT                                                  
025400         CALL FELLOG                                                      
025500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
025600         CONTINUE                                                         
025700     END-SEARCH                                                           
025800     .                                                                    
