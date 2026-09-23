001300 ID DIVISION.                                                             
001400 PROGRAM-ID.     W3031800.                                                
001500 AUTHOR.         INGVAR SKJELBRED.                                        
001600 DATE-WRITTEN.   96/08/02.                                                
001700 DATE-COMPILED.                                                           
001800                                                                          
001900*    FUNKTION:                                                            
002000*        LÄSER, SKAPAR ELLER ÄNDRAR_PROCENTPÅLÄGG PÅ                      
002100*        STANDARD OCH SJÄLVKOSTNAD-PRISER                                 
002200*                                                                         
002310*        PROGRAMMET LÄSER      WLPRIB (WDC2)                              
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSAKTION: W3T318                                              
002700*        MID:         W3I31801                                            
002800*                                                                         
002900*    UTDATA.                                                              
003000*        MOD:         W3O31801                                            
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600 WORKING-STORAGE SECTION.                                                 
003601                                                                          
003610*    -- CHECKED BY WY2000                                                 
003700 77  IDPGM                       PIC X(08)   VALUE 'W3031800'.            
003800                                                                          
003900*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004100                                                                          
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004400                                                                          
004502*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004503 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004510 77  MAX-INDX                    PIC S9(4)  VALUE +99   COMP SYNC.        
004520                                                                          
004530 77  RED-INDX1                   PIC S9(1)   VALUE ZERO  COMP-3.          
004540 77  RED-INDX2                   PIC S9(1)   VALUE ZERO  COMP-3.          
004550                                                                          
004600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004700 01  DAGENS-DATUM                PIC 9(8)    VALUE ZERO.                  
004701                                                                          
004710 01  WS-KDARTRAB                 PIC 9(2)    VALUE ZERO.                  
004711                                                                          
004712 01  WS-DASTADAT                 PIC 9(8)   VALUE ZERO.                   
004713                                                                          
004714 01  RED-TISTADAT                PIC X(7).                                
004715 01  RED-TISTADAT-2 REDEFINES RED-TISTADAT.                               
004716     03  RED-1                   PIC X(1).                                
004717     03  RED-2-TISTADAT          PIC X(6).                                
004718                                                                          
004720 01  SUM-REARTRAB                PIC S9(2)V99   COMP-3.                   
004721                                                                          
004730 01  WS-REARTRAB                 PIC S9(2)      COMP-3.                   
004731 01  WS-REARTRAB-DEC             PIC S9(2)      COMP-3.                   
004741                                                                          
004742 01  RED-TAB1.                                                            
004743     03  WS-RED-TAB1 OCCURS 2 PIC X.                                      
004744                                                                          
004745 01  RED-TAB2.                                                            
004746     03  WS-RED-TAB2 OCCURS 2 PIC X.                                      
004747                                                                          
004750 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004760     88  INDATA-OK                           VALUE 'J'.                   
004770     88  INDATA-FEL                          VALUE 'N'.                   
004780                                                                          
005000                                                                          
005100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005200     88  NYCKLAR-OK                          VALUE 'J'.                   
005300     88  NYCKLAR-FEL                         VALUE 'N'.                   
005400                                                                          
005500 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005600     88  EGEN-MID                            VALUE '3318'.                
005700     88  GODK-MID                            VALUE '3311' '3312'          
005800                                                   '3313' '3314'          
005900                                                   '3315' '3316'          
006000                                                   '3317' '3318'          
006100                                                   '3319'.                
006200     88  HELP-MID                            VALUE '0551'.                
006300     EJECT                                                                
006400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006500 01  GENERELLA-SUBPROGRAM.                                                
006600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007010     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007100     EJECT                                                                
007200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007300*01 -COPY WMEDAREA                                                        
007400     SKIP3                                                                
007500 01  MESSAGE-CODES.                                                       
007600     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007702     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007703     03  ERR-INFO-MISSING        PIC X(3)    VALUE '005'.                 
007704     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
007705     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
007706     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
007707     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
007710     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
007800     03  MISSING-PRICE-AREA      PIC X(3)    VALUE '236'.                 
007900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007908     03  INFO-MISSING            PIC X(3)    VALUE '760'.                 
007909     EJECT                                                                
007910*01  -COPY WDATAREA                                                       
008000     EJECT                                                                
008100*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008200*                                                                         
008300 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008400     SKIP3                                                                
008500*01 -COPY WMSGINIT                                                        
008600     SKIP3                                                                
008700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008800*                                                                         
008900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009000     SKIP3                                                                
009100*01  MID -COPY W3I31801                                                   
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009400     SKIP3                                                                
009500*01  -COPY WMSGAREA                                                       
009600     EJECT                                                                
009700     03  MOD REDEFINES MSG-AREA.                                          
009800*      05  -COPY W3O31801                                                 
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
011200 01  NYCKLAR-TILL-DLI.                                                    
011301     03  W-IDPROMR-X.                                                     
011310         05  W-IDPROMR           PIC X(3)    VALUE SPACE.                 
011400     SKIP2                                                                
011410                                                                          
011420     03  W-DASTADAT-X.                                                    
011430         05  W-DASTADAT          PIC 9(8)   VALUE ZERO.                   
011440                                                                          
011450     SKIP2                                                                
011500*    --- STATUS-KOD FRÅN IMS                                              
011600 01  STATUS-WS                   PIC XX.                                  
011700     88  SEGMENT-FINNS                       VALUE '  '.                  
011800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012000     SKIP2                                                                
012100 01  GODK-STATUSKODER.                                                    
012200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012300     SKIP3                                                                
012400 01  SSA1                        PIC X(120).                              
012500 01  SSA2                        PIC X(120).                              
012600     EJECT                                                                
012700*    --- IMS FUNKTIONSKODER                                               
012800*01  -COPY W0003                                                          
013000     EJECT                                                                
013100*    ---  DLI INPUT-OUTPUT AREA                                           
013200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
013300     SKIP3                                                                
013400 01  DLI-IO-AREA.                                                         
013500     03  IO-AREA                 PIC X(850)  VALUE SPACE.                 
013601     SKIP3                                                                
013602     03  WLPRIB01 REDEFINES IO-AREA.                                      
013610*        05  -COPY WDC201  -PRE PRIB-                                     
013620     EJECT                                                                
013690     03  WLPRIB13 REDEFINES IO-AREA.                                      
013700*        05  -COPY WDC213  -PRE PRIB-                                     
013900     EJECT                                                                
014000 LINKAGE SECTION.                                                         
014100*01  -COPY W0009   -PRE MSG-                                              
014200*01  -COPY W0008   -PRE USEA-                                             
014300     05  FILLER                  PIC X.                                   
014401     EJECT                                                                
014402*01  -COPY W0008  -PRE PRIB-                                              
014410     05  FILLER                  PIC X.                                   
014500     EJECT                                                                
014601 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB PRIB-PCB.                     
014602 MAIN SECTION.                                                            
014610     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB PRIB-PCB.                     
014700                                                                          
014900     PERFORM IMS-GET-MSG                                                  
015000     IF SEGMENT-FINNS                                                     
015100       PERFORM A-INIT                                                     
015200       PERFORM B-KOLLA-NYCKLAR                                            
015300       IF NYCKLAR-OK                                                      
015501          IF MFS-UPDATE                                                   
015502             PERFORM G-KOLLA-INPUT                                        
015503             IF INDATA-OK                                                 
015504                PERFORM H-UPPDATERA                                       
015505             END-IF                                                       
015506          ELSE                                                            
015507            IF MFS-FIRST                                                  
015508               PERFORM C-FOERSTA-SIDA                                     
015509            ELSE                                                          
015510               PERFORM E-SAMMA-SIDA                                       
015511            END-IF                                                        
015520          END-IF                                                          
015800         PERFORM F-LAES-VISA-INFO                                         
015900       END-IF                                                             
016200       COMPUTE MSG-KVLL = LENGTH OF MOD-W3O31801 + 4                      
016300       PERFORM IMS-INSERT-MSG                                             
016400     END-IF                                                               
016600                                                                          
016700     MOVE ZERO TO RETURN-CODE                                             
016800     GOBACK                                                               
016900     .                                                                    
017000     EJECT                                                                
017100 A-INIT SECTION.                                                          
017200                                                                          
017300     IF MSG-DUBBLA-TRANSKODER                                             
017400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I31801                 
017500       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017700     ELSE                                                                 
017800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W3I31801                  
017900       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
018000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
018100     END-IF                                                               
018200                                                                          
018300     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
018400     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
018500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018600                                                                          
018700     MOVE LOW-VALUE TO MSG-AREA                                           
018800     MOVE 'W3O318N1' TO MFS-IDMOD                                         
018900     MOVE '3318' TO MOD-IDTRANS                                           
019000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
019100                                                                          
019200     IF EGEN-MID OR HELP-MID                                              
019300       CONTINUE                                                           
019400     ELSE                                                                 
019500       MOVE SPACE TO MFS-KDTRTYP                                          
019600       MOVE '7' TO MFS-IDPFK                                              
019700     END-IF                                                               
019710                                                                          
019800     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM                      
019900     MOVE DAGENS-DATUM TO WS-DASTADAT                                     
019910                           W-DASTADAT                                     
020000     .                                                                    
020100     EJECT                                                                
020200 B-KOLLA-NYCKLAR SECTION.                                                 
020300                                                                          
020400     MOVE ALL '+'           TO MSGI-WMSGINIT                              
020500     MOVE '001'             TO MSGI-KDCALL                                
020600     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
020700     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
020800     MOVE '3318'            TO MSGI-IDTRANS                               
020900     IF GODK-MID                                                          
021010         MOVE MID-IDPROMR-IN     TO MSGI-IDPROMR                          
021100     END-IF                                                               
021200     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
021300                                                                          
021400     MOVE JA TO NYCKLAR-SW                                                
021500                                                                          
021601                                                                          
021602*    -- KONTROLL AV IDPROMR                                               
021603     MOVE MFS-RENSA-FAELT TO MOD-IDPROMR-IN                               
021604                                                                          
021605     IF MID-IDPROMR-IN NOT = ALL '+'                                      
021606       MOVE '7'         TO MFS-IDPFK                                      
021607       MOVE SPACE       TO MFS-KDTRTYP                                    
021608     END-IF                                                               
021611                                                                          
021620     IF MSGI-IDMARKBO = 'X'                                               
021630     OR MSGI-IDMARKBO = 'Y'                                               
021640        IF MSGI-IDPROMRN NUMERIC                                          
021641           MOVE MSGI-IDPROMR TO W-IDPROMR                                 
021642           IF MSGI-IDMARKBO = 'X'                                         
021643              MOVE 'SJÄLVKOSTNAD' TO MOD-TEXT1                            
021644           END-IF                                                         
021645           IF MSGI-IDMARKBO = 'Y'                                         
021646              MOVE 'STANDARPRIS'  TO MOD-TEXT1                            
021647           END-IF                                                         
021648        ELSE                                                              
021649           MOVE NEJ     TO NYCKLAR-SW                                     
021650        END-IF                                                            
021651     ELSE                                                                 
021652        MOVE NEJ        TO NYCKLAR-SW                                     
021660     END-IF                                                               
021701                                                                          
021702     IF GODK-MID OR NYCKLAR-OK                                            
021704       MOVE MSGI-IDPROMR        TO MOD-IDPROMR-UT                         
021713     ELSE                                                                 
021714       MOVE MFS-RENSA-FAELT TO MOD-IDPROMR-UT                             
021720     END-IF                                                               
021800                                                                          
021900     IF NYCKLAR-FEL                                                       
022000       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
022100       CALL WMEDKONV USING MED-WMEDAREA                                   
022200       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
022300       PERFORM MFS-RENSA-FAELT-IN                                         
022500     END-IF                                                               
022600     .                                                                    
022701     EJECT                                                                
022702 C-FOERSTA-SIDA SECTION.                                                  
022704                                                                          
022708     PERFORM MFS-RENSA-FAELT-IN                                           
022709     .                                                                    
022710     EJECT                                                                
022711 E-SAMMA-SIDA SECTION.                                                    
022713                                                                          
022714     IF EGEN-MID OR HELP-MID                                              
022715       IF MID-REARTRAB-UT = ALL '+'                                       
022716         PERFORM MFS-RENSA-FAELT-IN                                       
022717       ELSE                                                               
022718         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
022719         CALL WMEDKONV USING MED-WMEDAREA                                 
022720         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
022721         PERFORM EA-MID-INDATA-TILL-MOD                                   
022722       END-IF                                                             
022723     ELSE                                                                 
022724       PERFORM MFS-RENSA-FAELT-IN                                         
022725     END-IF                                                               
022726     .                                                                    
022727     EJECT                                                                
022728 EA-MID-INDATA-TILL-MOD SECTION.                                          
022730                                                                          
022731        IF MID-REARTRAB-UT = ALL '+'                                      
022733           MOVE MFS-RENSA-FAELT   TO MOD-REARTRAB-UT                      
022734           MOVE MFS-ALFA-FAELT-RAETT TO                                   
022735                               MOD-REARTRAB-ATTR                          
022736        ELSE                                                              
022737           MOVE MFS-ROER-EJ-FAELT TO MOD-REARTRAB-UT                      
022738           MOVE MFS-ADD-LAES-IN-FAELT TO                                  
022739                               MOD-REARTRAB-ATTR                          
022740        END-IF                                                            
022752     .                                                                    
022753     EJECT                                                                
022754 F-LAES-VISA-INFO SECTION.                                                
022756                                                                          
022757     PERFORM FA-LAES-GRUNDDATA                                            
022758                                                                          
022759     IF SEGMENT-SAKNAS                                                    
022760        MOVE MISSING-PRICE-AREA TO MED-IDMFSFEL                           
022764        CALL WMEDKONV USING MED-WMEDAREA                                  
022765        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
022766        PERFORM MFS-RENSA-FAELT-IN                                        
022767        MOVE SPACE    TO MOD-REARTRAB-IN                                  
022768        MOVE SPACE    TO MOD-TISTADAT-IN                                  
022769     ELSE                                                                 
022770       PERFORM FB-LAES-RADDATA                                            
022771       IF SEGMENT-SAKNAS                                                  
022772                                                                          
022774          MOVE SPACE    TO MOD-REARTRAB-IN                                
022775          MOVE SPACE    TO MOD-TISTADAT-IN                                
022777       ELSE                                                               
022778          PERFORM UNTIL SEGMENT-SAKNAS                                    
022780             PERFORM FB-LAES-RADDATA                                      
022781          END-PERFORM                                                     
022783          MOVE ZERO     TO WS-REARTRAB                                    
022784          MOVE ZERO     TO SUM-REARTRAB                                   
022785          MOVE PRIB-RAB-DASTADAT(3:6) TO RED-TISTADAT                     
022786          MOVE RED-2-TISTADAT     TO MOD-TISTADAT-IN                      
022788          MOVE PRIB-RAB-REARTRAB-DO (1) TO SUM-REARTRAB                   
022789          COMPUTE WS-REARTRAB = (SUM-REARTRAB * 2) - 100                  
022791          MOVE WS-REARTRAB TO MOD-REARTRAB-IN                             
022792       END-IF                                                             
022793     END-IF                                                               
022794     .                                                                    
022795     EJECT                                                                
022796 FA-LAES-GRUNDDATA SECTION.                                               
022797                                                                          
022798     PERFORM IMS-GU-WDC201                                                
022799     .                                                                    
022800     EJECT                                                                
022801 FB-LAES-RADDATA SECTION.                                                 
022802                                                                          
022805     PERFORM IMS-GNP-WDC213                                               
022806                                                                          
022807     .                                                                    
022808     EJECT                                                                
022809 G-KOLLA-INPUT SECTION.                                                   
022811                                                                          
022812     MOVE JA             TO INDATA-SW                                     
022813     IF MID-REARTRAB-UT = ALL '+'                                         
022814        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
022815        CALL WMEDKONV USING MED-WMEDAREA                                  
022816        MOVE MED-MFSFEL  TO MOD-TEMFSFEL                                  
022817        PERFORM MFS-ROER-EJ-FAELT-IN                                      
022818        MOVE NEJ         TO INDATA-SW                                     
022819     ELSE                                                                 
022820        IF MID-REARTRAB-UT NOT = ALL '+'                                  
022821           MOVE ZERO           TO WS-REARTRAB-DEC                         
022823           INSPECT MID-REARTRAB-UT                                        
022824                          REPLACING LEADING SPACE BY ZERO                 
022825           PERFORM GG-RED-INFALT                                          
022831           MOVE MID-REARTRAB-UT TO WS-REARTRAB-DEC                        
022835           MOVE MFS-NUM-FAELT-RAETT                                       
022836                                TO                                        
022837                                   MOD-REARTRAB-ATTR                      
022845        ELSE                                                              
022846           MOVE NEJ         TO INDATA-SW                                  
022847           MOVE MFS-NUM-FAELT-FEL TO MOD-REARTRAB-ATTR                    
022848        END-IF                                                            
022849        IF INDATA-FEL                                                     
022850           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
022851           CALL WMEDKONV USING MED-WMEDAREA                               
022852           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
022853           PERFORM MFS-ROER-EJ-FAELT-IN                                   
022854        END-IF                                                            
022855     END-IF                                                               
022856     .                                                                    
022857     EJECT                                                                
022858 GG-RED-INFALT SECTION.                                                   
022859                                                                          
022860     MOVE SPACE                    TO RED-TAB1                            
022861     MOVE SPACE                    TO RED-TAB2                            
022862     MOVE 2                        TO RED-INDX1                           
022863     MOVE 2                        TO RED-INDX2                           
022864                                                                          
022866     MOVE MID-REARTRAB-UT          TO RED-TAB1                            
022871                                                                          
022872     PERFORM UNTIL  RED-INDX1 < 1                                         
022873        IF WS-RED-TAB1(RED-INDX1) = SPACE                                 
022874           SUBTRACT +1 FROM RED-INDX1                                     
022875        ELSE                                                              
022876           IF WS-RED-TAB1(RED-INDX1) NUMERIC                              
022877              MOVE WS-RED-TAB1(RED-INDX1) TO                              
022878                    WS-RED-TAB2(RED-INDX2)                                
022879              SUBTRACT +1 FROM RED-INDX1                                  
022880              SUBTRACT +1 FROM RED-INDX2                                  
022881           ELSE                                                           
022882              MOVE WS-RED-TAB1(RED-INDX1) TO                              
022883                 WS-RED-TAB2(RED-INDX2)                                   
022884              SUBTRACT +1 FROM RED-INDX1                                  
022885              SUBTRACT +1 FROM RED-INDX2                                  
022886           END-IF                                                         
022887        END-IF                                                            
022888                                                                          
022889     END-PERFORM                                                          
022890                                                                          
022891     MOVE RED-TAB2 TO                                                     
022892                             MID-REARTRAB-UT                              
022893     INSPECT MID-REARTRAB-UT                                              
022894                      REPLACING LEADING SPACE BY ZERO                     
022895     .                                                                    
022896     EJECT                                                                
022897 H-UPPDATERA SECTION.                                                     
022899                                                                          
022900     PERFORM IMS-GU-WDC201                                                
022901     IF SEGMENT-FINNS                                                     
022902        PERFORM IMS-GNP-WDC213                                            
022905        IF SEGMENT-FINNS                                                  
022906           PERFORM UNTIL SEGMENT-SAKNAS                                   
022907              PERFORM IMS-GNP-WDC213                                      
022908           END-PERFORM                                                    
022909           IF PRIB-RAB-DASTADAT = WS-DASTADAT                             
022912              PERFORM IMS-GHU-WDC213                                      
022914              PERFORM HC-UPPDATERA                                        
022915              PERFORM IMS-REPL-WDC213                                     
022916           ELSE                                                           
022918              PERFORM HA-UPPDATERA                                        
022919              PERFORM IMS-ISRT-WDC213                                     
022920           END-IF                                                         
022921        ELSE                                                              
022922           PERFORM HA-UPPDATERA                                           
022923           PERFORM IMS-ISRT-WDC213                                        
022924        END-IF                                                            
022925        MOVE INF-UPDATE-DONE TO MED-IDMFSINF                              
022926        CALL WMEDKONV USING MED-WMEDAREA                                  
022927        MOVE MED-MFSINF TO MOD-TEMFSINF                                   
022928     ELSE                                                                 
022929******** SAKNAS SEGMENT WDC201 LÄGG UPP DET DÅ *******                    
022930         PERFORM HB-UPPDATERA                                             
022931         PERFORM IMS-ISRT-WDC201                                          
022932         PERFORM HA-UPPDATERA                                             
022933         PERFORM IMS-ISRT-WDC213                                          
022934     END-IF                                                               
022935     .                                                                    
022936     EJECT                                                                
022937 HA-UPPDATERA SECTION.                                                    
022939                                                                          
022940     MOVE 01              TO WS-KDARTRAB                                  
022941     MOVE +1              TO INDX                                         
022944     MOVE ZERO            TO PRIB-RAB-DASTADAT                            
022945     MOVE WS-DASTADAT     TO PRIB-RAB-DASTADAT                            
022947     MOVE WS-KDARTRAB     TO PRIB-RAB-KDARTRAB (1)                        
022948     MOVE ZERO            TO PRIB-RAB-REARTRAB-DO (1)                     
022949     MOVE ZERO            TO PRIB-RAB-REARTRAB-BULK (1)                   
022950     MOVE ZERO            TO SUM-REARTRAB                                 
022951     MOVE ZERO            TO WS-REARTRAB                                  
022953     MOVE WS-REARTRAB-DEC TO WS-REARTRAB                                  
022955     COMPUTE SUM-REARTRAB = 100 - (50 + (WS-REARTRAB / 2))                
022957     MOVE SUM-REARTRAB    TO PRIB-RAB-REARTRAB-DO (1)                     
022958                             PRIB-RAB-REARTRAB-BULK (1)                   
022960                                                                          
022961     MOVE 02   TO WS-KDARTRAB                                             
022962     MOVE +2   TO INDX                                                    
022963     PERFORM UNTIL INDX > MAX-INDX                                        
022964        MOVE WS-KDARTRAB TO PRIB-RAB-KDARTRAB (INDX)                      
022966        MOVE ZERO   TO PRIB-RAB-REARTRAB-DO (INDX)                        
022967        MOVE ZERO   TO PRIB-RAB-REARTRAB-BULK (INDX)                      
022968        ADD  +1   TO INDX                                                 
022969        ADD  1    TO WS-KDARTRAB                                          
022970     END-PERFORM                                                          
022971     .                                                                    
022972     EJECT                                                                
022973 HB-UPPDATERA SECTION.                                                    
022975                                                                          
022976     MOVE MSGI-IDPROMR     TO PRIB-PRO-IDPROMR                            
022980     MOVE +2              TO PRIB-PRO-KDORDKL-DOG                         
023900                                                                          
025000     .                                                                    
025100     EJECT                                                                
025200 HC-UPPDATERA SECTION.                                                    
025400                                                                          
026200     MOVE ZERO            TO SUM-REARTRAB                                 
026300     MOVE ZERO            TO WS-REARTRAB                                  
026310     MOVE WS-REARTRAB-DEC TO WS-REARTRAB                                  
026320     COMPUTE SUM-REARTRAB = 100 - (50 + (WS-REARTRAB / 2))                
026330     MOVE SUM-REARTRAB    TO PRIB-RAB-REARTRAB-DO (1)                     
026340                             PRIB-RAB-REARTRAB-BULK (1)                   
026350     MOVE 02   TO WS-KDARTRAB                                             
026360     MOVE +2   TO INDX                                                    
026370     PERFORM UNTIL INDX > MAX-INDX                                        
026380        MOVE WS-KDARTRAB TO PRIB-RAB-KDARTRAB (INDX)                      
026390        MOVE ZERO   TO PRIB-RAB-REARTRAB-DO (INDX)                        
026391        MOVE ZERO   TO PRIB-RAB-REARTRAB-BULK (INDX)                      
026392        ADD  +1   TO INDX                                                 
026393        ADD  1    TO WS-KDARTRAB                                          
026394     END-PERFORM                                                          
026397     .                                                                    
026398     EJECT                                                                
026400 MFS-RENSA-FAELT-IN SECTION.                                              
026500                                                                          
026600*    --- ALLA INDATA-FÄLT                                                 
026700     MOVE MFS-RENSA-FAELT TO MOD-REARTRAB-IN                              
026710                             MOD-REARTRAB-UT                              
026800                             MOD-TISTADAT-IN                              
026810                             MOD-IDPROMR-IN                               
026900     .                                                                    
027000     EJECT                                                                
028100 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
028200                                                                          
028300*    --- ALLA INDATA-FÄLT                                                 
028400     MOVE MFS-ROER-EJ-FAELT TO MOD-REARTRAB-UT                            
028600     .                                                                    
028700     EJECT                                                                
030200* --- IMS SEKTIONER ---                                                   
030300     SKIP3                                                                
030400 IMS-GET-MSG SECTION.                                                     
030500                                                                          
030600     MOVE '  QC' TO GODK-STATUSKODER                                      
030700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
030800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
030900     PERFORM IMS-STATUSKONTROLL                                           
031000     .                                                                    
031100     SKIP3                                                                
031200 IMS-INSERT-MSG SECTION.                                                  
031300                                                                          
031400     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
031500       MOVE '0' TO MFS-KDHUVOMR                                           
031600     END-IF                                                               
031700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
031800     MOVE SPACE TO GODK-STATUSKODER                                       
031900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
032000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
032100     PERFORM IMS-STATUSKONTROLL                                           
032200     .                                                                    
032301     EJECT                                                                
032302 IMS-GU-WDC201 SECTION.                                                   
032304     STRING 'WLPRIB01(IDPROMR  =' W-IDPROMR-X ')'                         
032305          DELIMITED BY SIZE INTO SSA1                                     
032306     MOVE '  GE' TO GODK-STATUSKODER                                      
032307     CALL CBLTDLI USING GU PRIB-PCB DLI-IO-AREA   SSA1                    
032308     MOVE PRIB-STATUS-CODE TO STATUS-WS                                   
032310     PERFORM IMS-STATUSKONTROLL                                           
032311     .                                                                    
032312     SKIP3                                                                
032313 IMS-GHU-WDC213 SECTION.                                                  
032315     STRING 'WLPRIB01(IDPROMR  =' W-IDPROMR-X ')'                         
032316          DELIMITED BY SIZE INTO SSA1                                     
032317     STRING 'WLPRIB13(DASTADAT =' W-DASTADAT-X ')'                        
032318          DELIMITED BY SIZE INTO SSA2                                     
032319     MOVE '  GE' TO GODK-STATUSKODER                                      
032320     CALL CBLTDLI USING GHU PRIB-PCB DLI-IO-AREA   SSA1 SSA2              
032321     MOVE PRIB-STATUS-CODE TO STATUS-WS                                   
032323     PERFORM IMS-STATUSKONTROLL                                           
032324     .                                                                    
032325     EJECT                                                                
032326 IMS-GNP-WDC213 SECTION.                                                  
032328     STRING 'WLPRIB13(DASTADAT<=' W-DASTADAT-X ')'                        
032329          DELIMITED BY SIZE INTO SSA1                                     
032330     MOVE '  GE' TO GODK-STATUSKODER                                      
032331     CALL CBLTDLI USING GNP  PRIB-PCB DLI-IO-AREA   SSA1                  
032332     MOVE PRIB-STATUS-CODE TO STATUS-WS                                   
032334     PERFORM IMS-STATUSKONTROLL                                           
032335     .                                                                    
032340     EJECT                                                                
032410 IMS-ISRT-WDC201 SECTION.                                                 
032420                                                                          
032450     MOVE 'WLPRIB01 ' TO SSA1                                             
032460     MOVE '  II' TO GODK-STATUSKODER                                      
032470     CALL CBLTDLI USING ISRT PRIB-PCB DLI-IO-AREA SSA1                    
032480     MOVE PRIB-STATUS-CODE TO STATUS-WS                                   
032490     PERFORM IMS-STATUSKONTROLL                                           
032491     .                                                                    
032492     EJECT                                                                
032493 IMS-ISRT-WDC213 SECTION.                                                 
032495                                                                          
032496     STRING 'WLPRIB01(IDPROMR  =' W-IDPROMR-X ')'                         
032497          DELIMITED BY SIZE INTO SSA1                                     
032498     MOVE 'WLPRIB13 ' TO SSA2                                             
032499     MOVE '  II' TO GODK-STATUSKODER                                      
032500     CALL CBLTDLI USING ISRT PRIB-PCB DLI-IO-AREA SSA1 SSA2               
032501     MOVE PRIB-STATUS-CODE TO STATUS-WS                                   
032503     PERFORM IMS-STATUSKONTROLL                                           
032504     .                                                                    
032505     EJECT                                                                
032506 IMS-REPL-WDC213 SECTION.                                                 
032510                                                                          
032511     MOVE 'WLPRIB13 ' TO SSA1                                             
032512     MOVE '  '   TO GODK-STATUSKODER                                      
032513     CALL CBLTDLI USING REPL PRIB-PCB DLI-IO-AREA SSA1                    
032514     MOVE PRIB-STATUS-CODE TO STATUS-WS                                   
032516     PERFORM IMS-STATUSKONTROLL                                           
032517     .                                                                    
032518     EJECT                                                                
032520 IMS-STATUSKONTROLL SECTION.                                              
032600                                                                          
032700     SET STATUS-IX TO 1                                                   
032800     SEARCH GODK-STATUS                                                   
032900       AT END                                                             
033000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
033100         DELIMITED BY SIZE INTO FELTEXT                                   
033200         CALL FELLOG                                                      
033300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
033400         CONTINUE                                                         
033500     END-SEARCH                                                           
033600     .                                                                    
