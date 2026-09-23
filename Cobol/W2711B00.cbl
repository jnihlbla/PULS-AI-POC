001100 ID DIVISION.                                                             
001200 PROGRAM-ID.     W2711B00.                                                
001300 AUTHOR.         STENING INGER.                                           
001400 DATE-WRITTEN.   20/05/20.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700*    FUNCTION:                                                            
001800*        CREATE EXTRACT FILE WITH KDREFORS=P                              
001900*                                                                         
002010*        THE PROGRAM READS     WDK6                                       
002100*                              WDK7                                       
002101*                              WDL7                                       
002110*                                                                         
002200*    ABENDCODES:                                                          
002300*        U0016 -  . . . .                                                 
002400*        U1000 -  . . . .                                                 
002500*                                                                         
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003301     SKIP2                                                                
003302*          --- W27110 FILE                                                
003303     SELECT W27110                     ASSIGN TO W2711BD1.                
003304     SKIP2                                                                
003305*          --- EXTRACT WITH KDREFORS=P                                    
003310     SELECT W2711B                     ASSIGN TO W2711BD2.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP2                                                                
003800 FILE SECTION.                                                            
003901     SKIP3                                                                
003902 FD  W27110                                                               
003903     RECORDING       F                                                    
003904     BLOCK CONTAINS  0.                                                   
003905                                                                          
003906*01  -COPY W27111      -L.                                                
003907     SKIP3                                                                
003908 FD  W2711B                                                               
003909     RECORDING       F                                                    
003910     BLOCK CONTAINS  0.                                                   
003911                                                                          
003920*01  RECORD -COPY W2711B -PRE  OUT-  -L.                                  
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300 77  IDPGM                       PIC X(8)    VALUE 'W2711B00'.            
004400 77  YES                         PIC X       VALUE 'J'.                   
004500 77  NOO                         PIC X       VALUE 'N'.                   
004701                                                                          
004702 77  IX1                         PIC 9(4)    VALUE ZERO.                  
004703 77  IX1-MAX                     PIC 9(4)    VALUE 12.                    
004704                                                                          
004705*01    -COPY WWDC99                                                       
004706*01    -COPY WWDCLAND                                                     
004707                                                                          
004708 77  W27110-EOF-SW               PIC X       VALUE 'N'.                   
004710     88  END-OF-W27110                       VALUE 'J'.                   
004800     EJECT                                                                
004900 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
005000 01  FILLER REDEFINES TODAYS-DATE.                                        
005100     03  TODAYS-DATE-YEAR        PIC 9(2).                                
005200     03  TODAYS-DATE-MONTH       PIC 9(2).                                
005300     03  TODAYS-DATE-DAY         PIC 9(2).                                
005400     EJECT                                                                
005401 01  FILLER                      PIC X(16) VALUE 'REFILLFÖRSLAG'.         
005403*01  -COPY W271RTXT                                                       
005410     EJECT                                                                
005500 01  GENERAL-SUBPROGRAMS.                                                 
005600*                                                                         
005700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006010     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006100     SKIP2                                                                
006200*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
006300                                                                          
006400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006500 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006600 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006700     SKIP2                                                                
006800 01  ERROR-TEXT.                                                          
006900     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
007000     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
007101     EJECT                                                                
007102*    --- PARAMETRAR TILL POSTSUM                                          
007103*                                                                         
007110*01  -COPY W0005   -PRE  POSTSUM-                                         
007301     EJECT                                                                
007302 01  IN-AREA-START               PIC X(24)   VALUE                        
007303                                 'IN-AREA-START  '.                       
007304     SKIP2                                                                
007305                                                                          
007306*01  AREA -COPY W27111     -PRE IN-                                       
007307     EJECT                                                                
007308 01  OUT-AREA-START              PIC X(24)   VALUE                        
007309                                 'OUT-AREA-START  '.                      
007310     SKIP2                                                                
007311                                                                          
007320*01  AREA -COPY W2711B     -PRE OUT-                                      
007400     EJECT                                                                
007500*    --- AREAS FOR IMS-SECTIONS                                           
007600*                                                                         
007700     EJECT                                                                
007800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007900     SKIP3                                                                
008000 01  KEYS-FOR-DLI.                                                        
008101     03  W-IDARTNR-X.                                                     
008110         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008120     03  W-IDDC-X.                                                        
008130         05  W-IDDC              PIC X(02)   VALUE SPACE.                 
008131     03  W-KDSEGKEY-X.                                                    
008132         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
008140     03  W-IDLAND-X.                                                      
008150         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
008160                                                                          
008200     SKIP2                                                                
008300*    --- STATUS-KOD FRÅN IMS                                              
008400 01  STATUS-WS                   PIC XX.                                  
008500     88  SEGMENT-FOUND                       VALUE '  '.                  
008600     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
008700     88  SEGMENT-MISSING                     VALUE 'GE'.                  
008800     SKIP2                                                                
008900 01  GOOD-STATUSCODES.                                                    
009000     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009100     SKIP3                                                                
009200 01  SSA1                        PIC X(128).                              
009300 01  SSA2                        PIC X(128).                              
009400     EJECT                                                                
009500*    --- IMS FUNCTION CODES                                               
009600*01  -COPY W0003                                                          
009700     EJECT                                                                
009900*    ---  DLI INPUT-OUTPUT AREA                                           
010001 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
010002 01  DLI-IO-WDK601.                                                       
010010*    03  -COPY WDK601                                                     
010300     EJECT                                                                
010301 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
010302 01  DLI-IO-WDK611.                                                       
010303*    03  -COPY WDK611                                                     
010304     EJECT                                                                
010310 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
010320 01  DLI-IO-WDK711.                                                       
010330*    03  -COPY WDK711                                                     
010340     EJECT                                                                
010341 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK712'.                      
010342 01  DLI-IO-WDK712.                                                       
010343*    03  -COPY WDK712                                                     
010344     EJECT                                                                
010350 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK727'.                      
010360 01  DLI-IO-WDK727.                                                       
010370*    03  -COPY WDK727                                                     
010380     EJECT                                                                
010390 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL711'.                      
010391 01  DLI-IO-WDL711.                                                       
010392*    03  -COPY WDL711                                                     
010393     EJECT                                                                
010400 LINKAGE SECTION.                                                         
010500                                                                          
010601                                                                          
010602*01  -COPY W0008  -PRE WDK6-                                              
010610     05  FILLER                  PIC X.                                   
010611                                                                          
010620*01  -COPY W0008  -PRE WDK7-                                              
010630     05  FILLER                  PIC X.                                   
010700     EJECT                                                                
010800*01  -COPY W0008  -PRE WDL7-                                              
010801     05  FILLER                  PIC X.                                   
010802     EJECT                                                                
010803 PROCEDURE DIVISION  USING WDK6-PCB WDK7-PCB WDL7-PCB.                    
010804 MAIN SECTION.                                                            
010810     ENTRY 'DLITCBL' USING WDK6-PCB WDK7-PCB WDL7-PCB.                    
010900                                                                          
011100                                                                          
011200     PERFORM A-INIT                                                       
011300                                                                          
011410     PERFORM S01-READ-W27110                                              
011500     PERFORM UNTIL END-OF-W27110                                          
011600                                                                          
011610       IF IN-KDREFORS = 'P'                                               
011700          PERFORM C-CREATE-EXTRACTFILE                                    
011800       END-IF                                                             
012100                                                                          
012210       PERFORM S01-READ-W27110                                            
012300     END-PERFORM                                                          
012400                                                                          
012500                                                                          
012600     PERFORM Z-FINIT                                                      
012700                                                                          
012800     MOVE ZERO TO RETURN-CODE                                             
012900     GOBACK                                                               
013000     .                                                                    
013100     EJECT                                                                
013200 A-INIT SECTION.                                                          
013301                                                                          
013310     OPEN INPUT  W27110                                                   
013401                                                                          
013410     OPEN OUTPUT W2711B                                                   
013500                                                                          
013600     ACCEPT TODAYS-DATE  FROM DATE                                        
013710     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
013900     .                                                                    
014000     EJECT                                                                
014010 C-CREATE-EXTRACTFILE SECTION.                                            
014015                                                                          
014017     MOVE IN-IDARTNR                 TO W-IDARTNR                         
014018     MOVE IN-IDDC                    TO W-IDDC                            
014019     PERFORM IMS-GU-WDK601                                                
014020     IF SEGMENT-FOUND                                                     
014021       PERFORM IMS-GNP-WDK611                                             
014022       IF SEGMENT-FOUND                                                   
014023         PERFORM IMS-GU-WDK711                                            
014024         IF SEGMENT-FOUND                                                 
014025           MOVE SLAG-FLREFBEO        TO OUT-FLREFBEO                      
014026           MOVE SLAG-FLFLYG          TO OUT-FLFLYG                        
014027           MOVE IN-IDARTNR           TO OUT-IDARTNR                       
014028           MOVE IN-IDDC              TO OUT-IDDC                          
014029           MOVE SLAG-IDDC-REF        TO OUT-IDDC-REF                      
014030           MOVE IN-IDPERSON-BUY      TO OUT-IDPERSON-BUY                  
014031           MOVE CLAG-KDERS           TO OUT-KDERS                         
014032           MOVE ART-KDPRODSL         TO OUT-KDPRODSL                      
014033           MOVE IN-KDREFTYP          TO OUT-KDREFTYP                      
014034           MOVE SPACE                TO OUT-TEREFTXT                      
014035           MOVE +1                   TO IX1                               
014036           PERFORM UNTIL IX1 > REF-TEXT-TABMAX                            
014037              IF IN-KDREFTXT = REF-TEXT-KDREFTEXT (IX1)                   
014038                MOVE REF-TEXT (IX1)  TO OUT-TEREFTXT                      
014039                MOVE REF-TEXT-TABMAX TO IX1                               
014040              END-IF                                                      
014041              ADD +1                 TO IX1                               
014042           END-PERFORM                                                    
014043           MOVE SLAG-KVAKS-PAV       TO OUT-KVAKS-PAV                     
014044           MOVE SLAG-KVAKS-SDC       TO OUT-KVAKS-SDC                     
014045           COMPUTE OUT-KVDISP ROUNDED = SLAG-KVLS       -                 
014046                                        SLAG-KVROS-BULK -                 
014047                                        SLAG-KVROS-DAG  -                 
014048                                        SLAG-KVOKS-BULK -                 
014049                                        SLAG-KVOKS-DAG                    
014050           MOVE IN-KVBEART           TO OUT-KVBEART-PROP                  
014051           MOVE SLAG-KVBEART         TO OUT-KVBEART                       
014052           PERFORM IMS-GNP-WDK727                                         
014053           MOVE NOO                  TO OUT-FLPB-JUST                     
014054           IF SEGMENT-FOUND                                               
014055             IF PROG-KVPB-JUST(01) > +0                                   
014056             OR PROG-KVPB-JUST(02) > +0                                   
014057               MOVE YES              TO OUT-FLPB-JUST                     
014058             END-IF                                                       
014059           END-IF                                                         
014060           MOVE SLAG-KVPB-REF        TO OUT-KVPB-REF                      
014061           MOVE SLAG-KVPBREOI        TO OUT-KVPBREOI                      
014062           COMPUTE OUT-KVROS = SLAG-KVROS-BULK + SLAG-KVROS-DAG           
014064           MOVE SLAG-KVREFBER        TO OUT-KVREFBER                      
014065           MOVE SLAG-KVREFPKT        TO OUT-KVREFPKT                      
014066           MOVE IN-IDDC              TO WS-IDDC                           
014067           IF NDC-CN OR NDC-US OR NDC-CA                                  
014068              PERFORM S03-SEARCH-IDLAND-DC                                
014070              PERFORM IMS-GU-WDK712                                       
014071              IF SEGMENT-FOUND                                            
014072                 MOVE LART-PRMATRL   TO OUT-PRMATRL                       
014074              ELSE                                                        
014075                 MOVE +0             TO OUT-PRMATRL                       
014077              END-IF                                                      
014078           ELSE                                                           
014079              MOVE CLAG-PRARTSTD     TO OUT-PRMATRL                       
014081           END-IF                                                         
014082           MOVE NOO                  TO OUT-FLSEASON                      
014083           MOVE +1                   TO IX1                               
014084           PERFORM UNTIL IX1 > IX1-MAX                                    
014085             IF SLAG-RESEASON(IX1) NOT = 1.00                             
014086                MOVE YES             TO OUT-FLSEASON                      
014087                MOVE IX1-MAX         TO IX1                               
014088             END-IF                                                       
014089             ADD +1                  TO IX1                               
014090           END-PERFORM                                                    
014091           MOVE SLAG-TIORDREG        TO OUT-TIORDREG                      
014092           PERFORM IMS-GU-WDL711                                          
014093           IF SEGMENT-FOUND                                               
014094             MOVE DC-TIREFEFT        TO OUT-TIREFEFT                      
014095           ELSE                                                           
014096             MOVE ZERO               TO OUT-TIREFEFT                      
014097           END-IF                                                         
014098                                                                          
014099           PERFORM S11-WRITE-W2711B                                       
014100         END-IF                                                           
014101       END-IF                                                             
014102     END-IF                                                               
014103     .                                                                    
014110     EJECT                                                                
014200 Z-FINIT SECTION.                                                         
014201     CLOSE W27110                                                         
014210           W2711B                                                         
014301     SKIP2                                                                
014302     MOVE 'S' TO POSTSUM-OPKOD                                            
014310     CALL POSTSUM USING POSTSUM-PARM                                      
014400     .                                                                    
014501     EJECT                                                                
014502 S01-READ-W27110  SECTION.                                                
014503     READ W27110 INTO IN-AREA                                             
014504     AT END                                                               
014505        MOVE HIGH-VALUE TO IN-AREA                                        
014506        SET END-OF-W27110 TO TRUE                                         
014507                                                                          
014508     NOT AT END                                                           
014509        MOVE 'W27110'   TO POSTSUM-FDNAMN                                 
014510        MOVE 'W2711BD1' TO POSTSUM-DDNAMN2                                
014512        MOVE SPACE      TO POSTSUM-TRANSTYP                               
014513        CALL POSTSUM USING POSTSUM-PARM                                   
014514     END-READ                                                             
014520     .                                                                    
014601     EJECT                                                                
014602 S03-SEARCH-IDLAND-DC SECTION.                                            
014603                                                                          
014604     SEARCH ALL DC-LAND                                                   
014605        AT END                                                            
014606           MOVE SPACE          TO W-IDLAND                                
014607        WHEN DCLAND-IDDC (DCLAND-IX) = IN-IDDC                            
014608           MOVE DCLAND-IDLANDX2 (DCLAND-IX)                               
014609                               TO W-IDLAND                                
014610     END-SEARCH                                                           
014612     .                                                                    
014613     EJECT                                                                
014614 S11-WRITE-W2711B SECTION.                                                
014615                                                                          
014616     WRITE OUT-RECORD FROM OUT-AREA                                       
014617                                                                          
014618     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
014619     MOVE 'W2711B'   TO POSTSUM-FDNAMN                                    
014620     MOVE 'W2711BD2' TO POSTSUM-DDNAMN2                                   
014621     CALL POSTSUM USING POSTSUM-PARM                                      
014630     .                                                                    
014800     EJECT                                                                
014900 S99-ABEND SECTION.                                                       
015000                                                                          
015101     SKIP2                                                                
015102     MOVE 'S' TO POSTSUM-OPKOD                                            
015110     CALL POSTSUM USING POSTSUM-PARM                                      
015200     CALL ABEND USING RKOD-ABEND                                          
015300     .                                                                    
015400     EJECT                                                                
015500* --- IMS SECTIONS  ---                                                   
015600                                                                          
015701     EJECT                                                                
015702 IMS-GU-WDK601 SECTION.                                                   
015704                                                                          
015705     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
015706            DELIMITED BY SIZE INTO SSA1                                   
015707     MOVE '  GE'           TO GOOD-STATUSCODES                            
015708     CALL CBLTDLI       USING GU  WDK6-PCB DLI-IO-WDK601 SSA1             
015709     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
015710     PERFORM IMS-STATUSCHECK                                              
015711     .                                                                    
015722     EJECT                                                                
015723 IMS-GNP-WDK611      SECTION.                                             
015725                                                                          
015726     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
015727          DELIMITED BY SIZE INTO SSA1                                     
015728     MOVE '  GE'              TO GOOD-STATUSCODES                         
015729     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
015730     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
015731     PERFORM IMS-STATUSCHECK                                              
015732     .                                                                    
015733     EJECT                                                                
015734 IMS-GU-WDK711    SECTION.                                                
015735                                                                          
015736     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
015737          DELIMITED BY SIZE INTO SSA1                                     
015738     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
015739          DELIMITED BY SIZE INTO SSA2                                     
015740     MOVE '  GE'              TO GOOD-STATUSCODES                         
015741     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
015742     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
015743     PERFORM IMS-STATUSCHECK                                              
015744     .                                                                    
015745                                                                          
015746 IMS-GU-WDK712 SECTION.                                                   
015748                                                                          
015749     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
015750          DELIMITED BY SIZE INTO SSA1                                     
015751     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
015752          DELIMITED BY SIZE INTO SSA2                                     
015753     MOVE '  GE'              TO GOOD-STATUSCODES                         
015754     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2               
015755     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
015756     PERFORM IMS-STATUSCHECK                                              
015757     .                                                                    
015758                                                                          
015759 IMS-GNP-WDK727 SECTION.                                                  
015761                                                                          
015762     MOVE 'WDK727 ' TO SSA1                                               
015763     MOVE '  GEGP'            TO GOOD-STATUSCODES                         
015764     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK727 SSA1                   
015765     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
015766     PERFORM IMS-STATUSCHECK                                              
015767     .                                                                    
015768     SKIP3                                                                
015769     EJECT                                                                
015770 IMS-GU-WDL711 SECTION.                                                   
015772                                                                          
015780     STRING 'WDL701  (IDARTNR  =' W-IDARTNR-X ')'                         
015790          DELIMITED BY SIZE INTO SSA1                                     
015800     STRING 'WDL711  (IDDC     =' W-IDDC-X ')'                            
015900          DELIMITED BY SIZE INTO SSA2                                     
016000     MOVE '  GE'              TO GOOD-STATUSCODES                         
016100     CALL CBLTDLI USING GU WDL7-PCB DLI-IO-WDL711 SSA1 SSA2               
016200     MOVE WDL7-STATUS-CODE    TO STATUS-WS                                
016300     PERFORM IMS-STATUSCHECK                                              
016400     .                                                                    
016500     EJECT                                                                
020263                                                                          
020264 IMS-STATUSCHECK SECTION.                                                 
020265                                                                          
020266     SET STATUS-IX TO 1                                                   
020267     SEARCH GOOD-STATUS                                                   
020268       AT END                                                             
020269         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
020270           DELIMITED BY SIZE INTO ERROR-TEXT                              
020271         DISPLAY ERROR-TEXT                                               
020272         CALL FELLOG                                                      
020273       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
020274         CONTINUE                                                         
020280     END-SEARCH                                                           
020300     .                                                                    
