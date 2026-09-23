001300 ID DIVISION.                                                             
001500 PROGRAM-ID.     W1053300.                                                
001600 AUTHOR.         STEFAN ANDREASSON, FRONTEC.                              
001700 DATE-WRITTEN.   95/10/25.                                                
001800 DATE-COMPILED.                                                           
001900                                                                          
002000*    FUNKTION:                                                            
002100*        TIDKODER FÖR VADIS GENERERING                                    
002200*                                                                         
002310*        PROGRAMMET UPPDATERAR WLKATM (WDN1)                              
002400*                                                                         
002401*    ÄNDRINGAR:                                                           
002402*        ÄNDRAT DATUMFÄLT I MOD TILL ATT VARA X(6) I.ST.F. Z(6)           
002403*        GöR ATT MAN SER ÅRTALET NÄR DATUM ÄR 000101   /CONNY             
002404*                                                                         
002410*                                                                         
002500*    INDATA.                                                              
002600*        TRANSAKTION: W1T533             ENTER                            
002610*                     W1T533U            PF11                             
002700*        MID:         W1I53301                                            
002800*                                                                         
002900*    UTDATA.                                                              
003000*        MOD:         W1O53301                                            
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600 WORKING-STORAGE SECTION.                                                 
003601*    -COPY WY2000W1                                                       
003610     SKIP3                                                                
003700 77  IDPGM                       PIC X(08)   VALUE 'W1053300'.            
003800                                                                          
003900*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004100                                                                          
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004310                                                                          
004325 01  DAGENS-AAR                  PIC 9(4)    VALUE ZERO.                  
004326 01  DAGENS-AAR-MINUS-1          PIC 9(4)    VALUE ZERO.                  
004327 01  DAGENS-AAR-PLUS-1           PIC 9(4)    VALUE ZERO.                  
004330 01  WS-6NUM                     PIC 9(6)    VALUE 999999.                
004331 01  FILLER                      PIC X(16)   VALUE 'DAGENS-DATUM'.        
004332 01  DAGENS-DATUM                PIC 9(6).                                
004333 01  DAGENS-DATUM-X              REDEFINES DAGENS-DATUM.                  
004340     03  INNEVARANDE-AAR         PIC 9(2).                                
004350     03  MAANAD-DAG              PIC 9(4).                                
004360                                                                          
004370 01  W-PERIOD.                                                            
004380     03  W-PERIOD-AA             PIC 9(2)    VALUE ZERO.                  
004390     03  W-PERIOD-PP             PIC 9(2)    VALUE ZERO.                  
004391 01  W-PERIOD-NUM                REDEFINES W-PERIOD                       
004392                                 PIC 9(4).                                
004393 01  FILLER                      PIC X(16)   VALUE 'W-VECKA'.             
004394 01  W-VECKA.                                                             
004395     03  W-VECKA-AA              PIC 9(2)    VALUE ZERO.                  
004396     03  W-VECKA-VV              PIC 9(2)    VALUE ZERO.                  
004397     03  W-VECKA-D               PIC 9       VALUE ZERO.                  
004398 01  W-VECKA-NUM                 REDEFINES W-VECKA                        
004399                                 PIC 9(5).                                
004400 01  FILLER                      PIC X(16)   VALUE 'INMAT-VECKA'.         
004401 01  W-INMATAD-VECKA             PIC X(4).                                
004402 01  W-INMATAD-VECKA-NUM         REDEFINES W-INMATAD-VECKA                
004403                                 PIC 9(4).                                
004404                                                                          
004405*    --- INDEX-NYCKLAR                                                    
004406 77  SPRAK-IX                    PIC S9(9)  VALUE +0   COMP SYNC.         
004411 77  RAD-IX                      PIC S9(9)  VALUE +0   COMP SYNC.         
004412 77  KOL-IX                      PIC S9(9)  VALUE +0   COMP SYNC.         
004420                                                                          
004430*    --- ARBETSFÄLT FÖR TEST                                              
004500 01  FILLER                      PIC X(16)   VALUE 'W-TESTFAELT'.         
004501 01  W-TESTFAELT.                                                         
004502     03  W-TESTFAELT1            PIC X(5).                                
004503     03  FILLER                  PIC X      VALUE SPACE.                  
004504     03  W-TESTFAELT2            PIC X(5).                                
004505     03  FILLER                  PIC X      VALUE SPACE.                  
004506     03  W-TESTFAELT3            PIC X(5).                                
004507     03  FILLER                  PIC X      VALUE SPACE.                  
004508     03  W-TESTFAELT4            PIC X(5).                                
004509     03  FILLER                  PIC X      VALUE SPACE.                  
004510     03  W-TESTFAELT5            PIC X(5).                                
004511     03  FILLER                  PIC X      VALUE SPACE.                  
004512 01  W-TESTAAR                   PIC 9(2)   VALUE ZERO.                   
004600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004800                                                                          
004801 01  FILLER                      PIC X(16)   VALUE 'WS-CATNR'.            
004810 77  WS-IDCATNR-FROM             PIC X(5)    VALUE SPACE.                 
004820 77  WS-IDCATNR-TO               PIC X(5)    VALUE SPACE.                 
004900                                                                          
004901 77  INPUT-FINNS-SW              PIC X       VALUE 'N'.                   
004902     88  INPUT-FINNS                         VALUE 'J'.                   
004903     88  INPUT-SAKNAS                        VALUE 'N'.                   
004904                                                                          
004905 77  INPUT-CATNR-TO-SW           PIC X       VALUE 'N'.                   
004906     88  CATNR-TO-FINNS                      VALUE 'J'.                   
004907     88  CATNR-TO-SAKNAS                     VALUE 'N'.                   
004908                                                                          
004909 77  INPUT-FLVADGEN-SW           PIC X       VALUE 'N'.                   
004910     88  FLVADGEN-FINNS                      VALUE 'J'.                   
004911     88  FLVADGEN-SAKNAS                     VALUE 'N'.                   
004912                                                                          
004913 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004914     88  INDATA-OK                           VALUE 'J'.                   
004920     88  INDATA-FEL                          VALUE 'N'.                   
005000                                                                          
005100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005200     88  NYCKLAR-OK                          VALUE 'J'.                   
005300     88  NYCKLAR-FEL                         VALUE 'N'.                   
005400                                                                          
005500 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005600     88  EGEN-MID                            VALUE '1533'.                
005700     88  GODK-MID                            VALUE '1533'.                
006200     88  HELP-MID                            VALUE '0551'.                
006300     EJECT                                                                
006400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006500 01  GENERELLA-SUBPROGRAM.                                                
006600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007010     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007100     EJECT                                                                
007200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007300*01 -COPY WMEDAREA                                                        
007301                                                                          
007310********* NEDANSTÅENDE FELMEDDELANDE SKA LIGGA I WMEDAREA ****            
007320 01  FEL-1                       PIC X(55)                                
007330            VALUE 'INTE TILLÅTET ATT KOPIERA FRÅN DENNA KATALOG'.         
007400     SKIP3                                                                
007500 01  MESSAGE-CODES.                                                       
007601     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007602     03  ERR-CONFLICT            PIC X(3)    VALUE '002'.                 
007603     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007604     03  ERR-NOT-REGISTERED      PIC X(3)    VALUE '010'.                 
007605     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
007610     03  INF-UPDATE-NOT-DONE     PIC X(3)    VALUE '034'.                 
007620     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
007800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007900                                                                          
007910     EJECT                                                                
007920*01  -COPY WDATAREA                                                       
008000     EJECT                                                                
008600     SKIP3                                                                
008700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008800*                                                                         
008900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009000     SKIP3                                                                
009100*01  MID -COPY W1I53301                                                   
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009400     SKIP3                                                                
009500*01  -COPY WMSGAREA                                                       
009600     EJECT                                                                
009800*    03  -COPY W1O53301 -RED MSG-AREA.                                    
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
011301     03  W-IDCATNR-X.                                                     
011302         05  W-IDCATNR           PIC 9(5)    VALUE ZERO.                  
011303     03  W-TIAAAA-X.                                                      
011310         05  W-TIAAAA            PIC 9(4)    VALUE ZERO.                  
011400     SKIP2                                                                
011500*    --- STATUS-KOD FRÅN IMS                                              
011600 01  STATUS-WS                   PIC XX.                                  
011700     88  SEGMENT-FINNS                       VALUE '  '.                  
011800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012000     SKIP2                                                                
012100 01  GODK-STATUSKODER.                                                    
012200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012300     SKIP3                                                                
012400 01  SSA1                        PIC X(64).                               
012500 01  SSA2                        PIC X(64).                               
012510 01  SSA3                        PIC X(64).                               
012520 01  SSA4                        PIC X(64).                               
012600     EJECT                                                                
012700*    --- IMS FUNKTIONSKODER                                               
012800*01  -COPY W0003                                                          
013000     EJECT                                                                
013100*    ---  DLI INPUT-OUTPUT AREA                                           
013200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
013300     SKIP3                                                                
013400 01  DLI-IO-AREA.                                                         
013410                                                                          
013420     03  FILLER                  PIC X(16)   VALUE 'WDN101-AREA'.         
013500     03  IO-AREA-1               PIC X(600)  VALUE SPACE.                 
013601     SKIP3                                                                
013602     03  WLKATM01 REDEFINES IO-AREA-1.                                    
013603*        05  -COPY WDN101  -PRE KATM-                                     
013604                                                                          
013605     03  FILLER                  PIC X(16)   VALUE 'WDN111-AREA'.         
013606     03  IO-AREA-2               PIC X(600)  VALUE SPACE.                 
013607     SKIP3                                                                
013608     03  WLKATM11 REDEFINES IO-AREA-2.                                    
013610*        05  -COPY WDN111  -PRE KATM-                                     
013611                                                                          
013620     03  FILLER                  PIC X(16)                                
013621                                         VALUE 'WDN101-AREA KOP'.         
013630     03  IO-AREA-3               PIC X(600)  VALUE SPACE.                 
013640     SKIP3                                                                
013650     03  WLKATM01 REDEFINES IO-AREA-3.                                    
013660*        05  -COPY WDN101  -PRE KATMK-                                    
013670                                                                          
013680     03  FILLER                  PIC X(16)                                
013681                                         VALUE 'WDN111-AREA KOP'.         
013690     03  IO-AREA-4               PIC X(600)  VALUE SPACE.                 
013700     SKIP3                                                                
013800     03  WLKATM11 REDEFINES IO-AREA-4.                                    
013810*        05  -COPY WDN111  -PRE KATMK-                                    
013900     EJECT                                                                
014000 LINKAGE SECTION.                                                         
014100                                                                          
014200*01  -COPY W0009   -PRE MSG-                                              
014501     EJECT                                                                
014502*01  -COPY W0008  -PRE KATM-                                              
014510     05  FILLER                  PIC X.                                   
014600     EJECT                                                                
014701 PROCEDURE DIVISION  USING MSG-PCB KATM-PCB.                              
014702 MAIN SECTION.                                                            
014710     ENTRY 'DLITCBL' USING MSG-PCB KATM-PCB.                              
014800                                                                          
015000     PERFORM IMS-GET-MSG                                                  
015100     IF SEGMENT-FINNS                                                     
015200       PERFORM A-INIT                                                     
015210       IF GODK-MID                                                        
015300         PERFORM B-KOLLA-NYCKLAR                                          
015400         IF NYCKLAR-OK                                                    
015501           IF MFS-UPDATE                                                  
015502             PERFORM C-KOLLA-INPUT                                        
015503             IF INDATA-OK                                                 
015504               PERFORM D-UPPDATERA                                        
015505               PERFORM F-LAES-BAS-VISA-INFO                               
015507             ELSE                                                         
015510               MOVE INF-UPDATE-NOT-DONE TO MED-IDMFSINF                   
015511               CALL WMEDKONV USING         MED-WMEDAREA                   
015512               MOVE MED-TEMFSINF        TO MOD-TEMFSINF                   
015513             END-IF                                                       
015520           ELSE                                                           
015541               PERFORM E-ENTER-TRYCKNING                                  
015542               PERFORM F-LAES-BAS-VISA-INFO                               
015820           END-IF                                                         
016000         END-IF                                                           
016001       ELSE                                                               
016002         PERFORM MFS-RENSA-MOD-FAELT-IN                                   
016003         PERFORM MFS-RENSA-MOD-FAELT-UT                                   
016004       END-IF                                                             
016005       MOVE LENGTH OF MOD-W1O53301 TO MSG-KVLL                            
016006       ADD +4                      TO MSG-KVLL                            
016200       PERFORM IMS-INSERT-MSG                                             
016300     END-IF                                                               
016500                                                                          
016600     MOVE ZERO TO RETURN-CODE                                             
016700     GOBACK                                                               
016800     .                                                                    
016900     EJECT                                                                
017000 A-INIT SECTION.                                                          
017100                                                                          
017200     IF MSG-DUBBLA-TRANSKODER                                             
017300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I53301                 
017400       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017500       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017600     ELSE                                                                 
017700       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W1I53301                  
017800       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
017900       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
018000     END-IF                                                               
018100                                                                          
018200     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
018300     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
018400     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018500                                                                          
018600     MOVE LOW-VALUE TO MSG-AREA                                           
018700     MOVE 'W1O53301' TO MFS-IDMOD                                         
018800     MOVE '1533' TO MOD-IDTRANS                                           
018900     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
019000                                                                          
019500     IF EGEN-MID OR HELP-MID                                              
019600       CONTINUE                                                           
019700     ELSE                                                                 
019800       MOVE SPACE TO MFS-KDTRTYP                                          
020000     END-IF                                                               
020001                                                                          
020002     IF MFS-IDPFK = '7' OR '8'                                            
020003       MOVE SPACE TO MFS-IDPFK                                            
020004     END-IF                                                               
020005                                                                          
020006     IF ENGLISH-TEXT                                                      
020007       MOVE +2 TO SPRAK-IX                                                
020008       MOVE 'GB ' TO MED-IDSKYLT                                          
020009     ELSE                                                                 
020010       MOVE +1 TO SPRAK-IX                                                
020011       MOVE 'S  ' TO MED-IDSKYLT                                          
020012     END-IF                                                               
020020                                                                          
020100     ACCEPT DAGENS-DATUM FROM DATE                                        
020102                                                                          
020103     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
020104     MOVE DAGENS-DATUM                                                    
020105                   TO DAT-I-TIDATUM                                       
020106                                                                          
020107     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
020108                     DAT-O-TIDATUM DAT-KDSVAR                             
020109                                                                          
020110     IF DAT-KDSVAR-OK                                                     
020111****             HÄMTA SEKELSIFFROR                                       
020112       MOVE DAT-TISEKEL    TO DAGENS-AAR(1:2)                             
020132     ELSE                                                                 
020133         STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS                
020134         DELIMITED BY SIZE INTO FELTEXT                                   
020135         CALL FELLOG                                                      
020136     END-IF                                                               
020138                                                                          
020139     MOVE INNEVARANDE-AAR    TO DAGENS-AAR(3:2)                           
020140                                                                          
020150     SUBTRACT +1 FROM DAGENS-AAR GIVING DAGENS-AAR-MINUS-1                
020213     ADD +1 DAGENS-AAR GIVING DAGENS-AAR-PLUS-1                           
020300     .                                                                    
020400     EJECT                                                                
020500 B-KOLLA-NYCKLAR SECTION.                                                 
020700                                                                          
020800     MOVE JA TO NYCKLAR-SW                                                
020810                                                                          
021502*    -- KONTROLL AV IDCATNR                                               
021503     MOVE MFS-RENSA-FAELT TO MOD-IDCATNR-FROM-IN                          
021504     IF MID-IDCATNR-FROM-IN = ALL '+'                                     
021505       MOVE MID-IDCATNR-FROM-UT TO WS-IDCATNR-FROM                        
021506       INSPECT WS-IDCATNR-FROM REPLACING LEADING SPACE BY ZERO            
021507     ELSE                                                                 
021509       MOVE MID-IDCATNR-FROM-IN TO WS-IDCATNR-FROM                        
021510       MOVE SPACE          TO MFS-IDPFK   MFS-KDTRTYP                     
021511     END-IF                                                               
021512     IF WS-IDCATNR-FROM NUMERIC AND WS-IDCATNR-FROM > ZERO                
021513       MOVE WS-IDCATNR-FROM TO W-IDCATNR                                  
021514     ELSE                                                                 
021515       MOVE NEJ TO NYCKLAR-SW                                             
021516     END-IF                                                               
021517     MOVE WS-IDCATNR-FROM TO MOD-IDCATNR-FROM-UT                          
021518     INSPECT MOD-IDCATNR-FROM-UT REPLACING LEADING ZERO BY SPACE          
021519     IF NYCKLAR-FEL OR (NOT GODK-MID )                                    
021520       MOVE ERR-WRONG-KEY   TO MED-IDMFSFEL                               
021521       CALL WMEDKONV USING  MED-WMEDAREA                                  
021522       MOVE MED-TEMFSFEL    TO MOD-TEMFSFEL                               
021548       PERFORM MFS-RENSA-MOD-FAELT-IN                                     
021549       PERFORM MFS-RENSA-MOD-FAELT-UT                                     
021550     ELSE                                                                 
021551       PERFORM IMS-GET-KATM01-FROM                                        
021552       IF SEGMENT-SAKNAS                                                  
021553         MOVE NEJ TO NYCKLAR-SW                                           
021554         MOVE ERR-NOT-REGISTERED TO MED-IDMFSFEL                          
021555         CALL WMEDKONV USING      MED-WMEDAREA                            
021556         MOVE MED-TEMFSFEL     TO MOD-TEMFSFEL                            
021557         PERFORM MFS-RENSA-MOD-FAELT-IN                                   
021558         PERFORM MFS-RENSA-MOD-FAELT-UT                                   
021559       ELSE                                                               
021560         IF KATM-KAT-FLKOPIE = NEJ                                        
021561           MOVE NEJ TO NYCKLAR-SW                                         
021562           MOVE FEL-1            TO MOD-TEMFSFEL                          
021563           PERFORM MFS-RENSA-MOD-FAELT-IN                                 
021564           PERFORM MFS-RENSA-MOD-FAELT-UT                                 
021565         END-IF                                                           
021566       END-IF                                                             
021567     END-IF                                                               
021568                                                                          
021569*    -- KONTROLL AV IDCATNR-TO                                            
021570     MOVE MFS-RENSA-FAELT TO MOD-IDCATNR-TO-IN                            
021571     IF MID-IDCATNR-TO-IN = ALL '+'                                       
021572       CONTINUE                                                           
021573     ELSE                                                                 
021574                                                                          
021575       MOVE DAGENS-AAR         TO W-TIAAAA                                
021576       PERFORM IMS-GET-KATM11-FROM                                        
021577       IF SEGMENT-SAKNAS                                                  
021578         MOVE NEJ TO NYCKLAR-SW                                           
021579         MOVE ERR-NOT-REGISTERED TO MED-IDMFSFEL                          
021580         CALL WMEDKONV USING      MED-WMEDAREA                            
021581         MOVE MED-TEMFSFEL     TO MOD-TEMFSFEL                            
021582         PERFORM MFS-RENSA-MOD-FAELT-IN                                   
021583         PERFORM MFS-RENSA-MOD-FAELT-UT                                   
021587       ELSE                                                               
021589                                                                          
021590         MOVE MID-IDCATNR-TO-IN TO WS-IDCATNR-TO                          
021591         IF WS-IDCATNR-TO NUMERIC AND WS-IDCATNR-TO > ZERO                
021592           MOVE WS-IDCATNR-TO TO W-IDCATNR                                
021593         ELSE                                                             
021594           MOVE NEJ TO NYCKLAR-SW                                         
021595         END-IF                                                           
021596         IF NYCKLAR-FEL OR (NOT GODK-MID )                                
021597           MOVE ERR-WRONG-KEY   TO MED-IDMFSFEL                           
021598           CALL WMEDKONV USING  MED-WMEDAREA                              
021599           MOVE MED-TEMFSFEL    TO MOD-TEMFSFEL                           
021600           PERFORM MFS-RENSA-MOD-FAELT-IN                                 
021601           PERFORM MFS-RENSA-MOD-FAELT-UT                                 
021602         ELSE                                                             
021603           MOVE WS-IDCATNR-TO TO MOD-IDCATNR-TO-UT                        
021604           INSPECT MOD-IDCATNR-TO-UT                                      
021605                           REPLACING LEADING ZERO BY SPACE                
021606           PERFORM IMS-GET-KATM01-TO                                      
021607           IF SEGMENT-SAKNAS                                              
021608             MOVE NEJ TO NYCKLAR-SW                                       
021609             MOVE ERR-NOT-REGISTERED TO MED-IDMFSFEL                      
021610             CALL WMEDKONV USING      MED-WMEDAREA                        
021611             MOVE MED-TEMFSFEL     TO MOD-TEMFSFEL                        
021612             PERFORM MFS-RENSA-MOD-FAELT-IN                               
021613             PERFORM MFS-RENSA-MOD-FAELT-UT                               
021614           END-IF                                                         
021615         END-IF                                                           
021616       END-IF                                                             
021617     END-IF                                                               
021620                                                                          
021700     .                                                                    
022900     EJECT                                                                
023130 C-KOLLA-INPUT SECTION.                                                   
023131                                                                          
023132       MOVE JA               TO INDATA-SW                                 
023133       MOVE NEJ              TO INPUT-FINNS-SW                            
023134                                INPUT-CATNR-TO-SW                         
023135                                INPUT-FLVADGEN-SW                         
023136                                                                          
023137**   KATM01 LÄST I B- SECTION.                                            
023138                                                                          
023139       IF  MID-IDCATNR-TO-IN = ALL '+'                                    
023142         CONTINUE                                                         
023143       ELSE                                                               
023144         MOVE JA         TO INPUT-FINNS-SW                                
023145                            INPUT-CATNR-TO-SW                             
023146       END-IF                                                             
023147                                                                          
023148       MOVE +1 TO KOL-IX                                                  
023150       PERFORM UNTIL (KOL-IX > +2 OR                                      
023151                     FLVADGEN-FINNS)                                      
023152         MOVE +1 TO RAD-IX                                                
023153         PERFORM UNTIL (RAD-IX > +12 OR                                   
023154                        FLVADGEN-FINNS)                                   
023156           IF  MID-FLVADGEN (KOL-IX, RAD-IX) = ALL '+'                    
023157             CONTINUE                                                     
023158           ELSE                                                           
023159             MOVE JA         TO INPUT-FINNS-SW                            
023160                                INPUT-FLVADGEN-SW                         
023161           END-IF                                                         
023162           ADD +1 TO RAD-IX                                               
023163         END-PERFORM                                                      
023164         ADD +1 TO KOL-IX                                                 
023165       END-PERFORM                                                        
023166                                                                          
023167     IF INPUT-SAKNAS                                                      
023168       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
023169       CALL WMEDKONV USING MED-WMEDAREA                                   
023170       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
023171       PERFORM MFS-ROER-EJ-FAELT-UT                                       
023172       MOVE NEJ       TO INDATA-SW                                        
023173                                                                          
023174     ELSE                                                                 
023175                                                                          
023178       IF CATNR-TO-FINNS                                                  
023179       AND FLVADGEN-FINNS                                                 
023180                                                                          
023181**   KONFLIKT MELLAN ATT KOPIERA TILL EN NY KATALOG                       
023182**   SAMT ATT FÖRSÖKA UPPDATERA PUBKOD PÅ BEFINTLIG KATALOG               
023183                                                                          
023184         MOVE MFS-ALFA-FAELT-FEL                                          
023185                          TO MOD-IDCATNR-TO-ATTR                          
023186         MOVE +1 TO KOL-IX                                                
023187         MOVE +1 TO RAD-IX                                                
023188         PERFORM UNTIL (RAD-IX > +12)                                     
023189           IF  MID-FLVADGEN (KOL-IX, RAD-IX) = ALL '+'                    
023190             CONTINUE                                                     
023191           ELSE                                                           
023192             MOVE MFS-ALFA-FAELT-FEL                                      
023193                        TO MOD-KOL2-FLVADGEN-ATTR (RAD-IX)                
023194           END-IF                                                         
023195           ADD +1 TO RAD-IX                                               
023198         END-PERFORM                                                      
023213                                                                          
023214         MOVE +2 TO KOL-IX                                                
023215         MOVE +1 TO RAD-IX                                                
023216         PERFORM UNTIL (RAD-IX > +12)                                     
023217           IF  MID-FLVADGEN (KOL-IX, RAD-IX) = ALL '+'                    
023218             CONTINUE                                                     
023219           ELSE                                                           
023220             MOVE MFS-ALFA-FAELT-FEL                                      
023221                        TO MOD-KOL3-FLVADGEN-ATTR (RAD-IX)                
023222           END-IF                                                         
023223           ADD +1 TO RAD-IX                                               
023224         END-PERFORM                                                      
023225                                                                          
023226         MOVE ERR-CONFLICT         TO MED-IDMFSFEL                        
023227         CALL WMEDKONV USING MED-WMEDAREA                                 
023228         MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                
023229         PERFORM MFS-ROER-EJ-FAELT-IN                                     
023230         PERFORM MFS-ROER-EJ-FAELT-UT                                     
023231         MOVE NEJ       TO INDATA-SW                                      
023232                                                                          
023233       ELSE                                                               
023234                                                                          
023235         MOVE +1 TO KOL-IX                                                
023236                    RAD-IX                                                
023237         PERFORM UNTIL  RAD-IX > +12                                      
023238           IF MID-FLVADGEN (KOL-IX, RAD-IX) = ALL '+'                     
023239           OR MID-FLVADGEN (KOL-IX, RAD-IX) = SPACE                       
023240           OR MID-FLVADGEN (KOL-IX, RAD-IX) = 'A'                         
023241           OR MID-FLVADGEN (KOL-IX, RAD-IX) = 'a'                         
023242              MOVE MFS-ALFA-FAELT-RAETT                                   
023243                               TO MOD-KOL2-FLVADGEN-ATTR (RAD-IX)         
023244           ELSE                                                           
023276              MOVE NEJ       TO INDATA-SW                                 
023277              MOVE MFS-ALFA-FAELT-FEL                                     
023278                             TO MOD-KOL2-FLVADGEN-ATTR (RAD-IX)           
023280           END-IF                                                         
023281           ADD +1 TO RAD-IX                                               
023282         END-PERFORM                                                      
023283                                                                          
023284         MOVE +2 TO KOL-IX                                                
023285         MOVE +1 TO RAD-IX                                                
023286         PERFORM UNTIL  RAD-IX > +12                                      
023287           IF MID-FLVADGEN (KOL-IX, RAD-IX) = ALL '+'                     
023288           OR MID-FLVADGEN (KOL-IX, RAD-IX) = SPACE                       
023289           OR MID-FLVADGEN (KOL-IX, RAD-IX) = 'A'                         
023290           OR MID-FLVADGEN (KOL-IX, RAD-IX) = 'a'                         
023291              MOVE MFS-ALFA-FAELT-RAETT                                   
023292                             TO MOD-KOL3-FLVADGEN-ATTR (RAD-IX)           
023293           ELSE                                                           
023323              MOVE NEJ       TO INDATA-SW                                 
023324              MOVE MFS-ALFA-FAELT-FEL                                     
023325                             TO MOD-KOL3-FLVADGEN-ATTR (RAD-IX)           
023334           END-IF                                                         
023335           ADD +1 TO RAD-IX                                               
023336         END-PERFORM                                                      
023337         IF INDATA-FEL                                                    
023338             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
023339             CALL WMEDKONV USING MED-WMEDAREA                             
023340             MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                            
023341             PERFORM MFS-ROER-EJ-FAELT-UT                                 
023343         END-IF                                                           
023344       END-IF                                                             
023345     END-IF                                                               
023346     .                                                                    
023347     EJECT                                                                
023348 D-UPPDATERA SECTION.                                                     
023351                                                                          
023352     IF  MID-IDCATNR-TO-IN NOT = ALL '+'                                  
023353                                                                          
023354**   KOPIERA GENERERINGSTABELL FRÅN KATALOG                               
023355                                                                          
023356**   KOPIERA AKTUELLT ÅR                                                  
023357                                                                          
023358       MOVE WS-IDCATNR-FROM TO W-IDCATNR                                  
023359       PERFORM IMS-GET-KATM01-FROM                                        
023360                                                                          
023361       IF SEGMENT-SAKNAS                                                  
023362         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
023363         DELIMITED BY SIZE INTO FELTEXT                                   
023364         CALL FELLOG                                                      
023365       END-IF                                                             
023366                                                                          
023367       MOVE DAGENS-AAR         TO W-TIAAAA                                
023368       PERFORM IMS-GET-KATM11-FROM                                        
023369       IF SEGMENT-SAKNAS                                                  
023370         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
023371         DELIMITED BY SIZE INTO FELTEXT                                   
023372         CALL FELLOG                                                      
023373       END-IF                                                             
023374                                                                          
023375       MOVE WS-IDCATNR-TO   TO W-IDCATNR                                  
023376       PERFORM IMS-GET-KATM01-TO                                          
023377                                                                          
023378       IF SEGMENT-SAKNAS                                                  
023379         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
023380         DELIMITED BY SIZE INTO FELTEXT                                   
023381         CALL FELLOG                                                      
023382       END-IF                                                             
023383                                                                          
023384       PERFORM IMS-GET-KATM11-TO                                          
023385                                                                          
023386       IF SEGMENT-FINNS                                                   
023387         MOVE +1 TO RAD-IX                                                
023388         PERFORM UNTIL  RAD-IX > +12                                      
023389           MOVE KATM-TAB-TIVADGEN-PLAN (RAD-IX) TO TMP1-YYMMDD            
023390           MOVE DAGENS-DATUM                    TO TMP2-YYMMDD            
023391           PERFORM WY2000P1                                               
023392           IF TMP1-YYMMDD < TMP2-YYMMDD                                   
023393             CONTINUE                                                     
023394           ELSE                                                           
023395             MOVE MSG-SIGNON-USERID                                       
023396                                 TO KATMK-TAB-IDUSER (RAD-IX)             
023397             MOVE KATM-TAB-FLVADGEN (RAD-IX)                              
023398                                 TO KATMK-TAB-FLVADGEN (RAD-IX)           
023399             MOVE KATM-TAB-KDCATPUB-FOM (RAD-IX)                          
023400                             TO KATMK-TAB-KDCATPUB-FOM (RAD-IX)           
023401             MOVE KATM-TAB-KDCATPUB-TOM (RAD-IX)                          
023402                             TO KATMK-TAB-KDCATPUB-TOM (RAD-IX)           
023403             MOVE DAGENS-DATUM   TO KATMK-TAB-TIUPPDAT (RAD-IX)           
023404             MOVE KATM-TAB-TIOMBRYT (RAD-IX)                              
023405                                 TO KATMK-TAB-TIOMBRYT (RAD-IX)           
023406             MOVE KATM-TAB-TIVADGEN-PLAN (RAD-IX)                         
023407                              TO KATMK-TAB-TIVADGEN-PLAN (RAD-IX)         
023408             MOVE ZERO        TO KATMK-TAB-TIVADGEN-UPPD (RAD-IX)         
023409           END-IF                                                         
023410           ADD +1 TO RAD-IX                                               
023411         END-PERFORM                                                      
023412                                                                          
023413         PERFORM IMS-REPL-KATM11-TO                                       
023414                                                                          
023415       ELSE                                                               
023416         MOVE KATM-TAB-TIAAAA    TO KATMK-TAB-TIAAAA                      
023417         MOVE +1 TO RAD-IX                                                
023418         PERFORM UNTIL  RAD-IX > +12                                      
023419           MOVE KATM-TAB-TIVADGEN-PLAN (RAD-IX) TO TMP1-YYMMDD            
023420           MOVE DAGENS-DATUM                    TO TMP2-YYMMDD            
023421           PERFORM WY2000P1                                               
023422           IF TMP1-YYMMDD < TMP2-YYMMDD                                   
023423             MOVE MSG-SIGNON-USERID                                       
023424                                 TO KATMK-TAB-IDUSER (RAD-IX)             
023425             MOVE SPACE          TO KATMK-TAB-FLVADGEN (RAD-IX)           
023426                                  KATMK-TAB-KDCATPUB-FOM (RAD-IX)         
023427                                  KATMK-TAB-KDCATPUB-TOM (RAD-IX)         
023428             MOVE DAGENS-DATUM   TO KATMK-TAB-TIUPPDAT (RAD-IX)           
023429             MOVE ZERO         TO KATMK-TAB-TIOMBRYT (RAD-IX)             
023430                                  KATMK-TAB-TIVADGEN-PLAN (RAD-IX)        
023431                                  KATMK-TAB-TIVADGEN-UPPD (RAD-IX)        
023432           ELSE                                                           
023433             MOVE MSG-SIGNON-USERID                                       
023434                                 TO KATMK-TAB-IDUSER (RAD-IX)             
023435             MOVE KATM-TAB-FLVADGEN (RAD-IX)                              
023436                                 TO KATMK-TAB-FLVADGEN (RAD-IX)           
023437             MOVE KATM-TAB-KDCATPUB-FOM (RAD-IX)                          
023438                             TO KATMK-TAB-KDCATPUB-FOM (RAD-IX)           
023439             MOVE KATM-TAB-KDCATPUB-TOM (RAD-IX)                          
023440                             TO KATMK-TAB-KDCATPUB-TOM (RAD-IX)           
023442             MOVE DAGENS-DATUM   TO KATMK-TAB-TIUPPDAT (RAD-IX)           
023443             MOVE KATM-TAB-TIOMBRYT (RAD-IX)                              
023444                                 TO KATMK-TAB-TIOMBRYT (RAD-IX)           
023445             MOVE KATM-TAB-TIVADGEN-PLAN (RAD-IX)                         
023446                              TO KATMK-TAB-TIVADGEN-PLAN (RAD-IX)         
023447             MOVE ZERO        TO KATMK-TAB-TIVADGEN-UPPD (RAD-IX)         
023448           END-IF                                                         
023449           ADD +1 TO RAD-IX                                               
023450         END-PERFORM                                                      
023451         PERFORM IMS-ISRT-KATM11-TO                                       
023452       END-IF                                                             
023453                                                                          
023454**   KOPIERA NÄSTA ÅR                                                     
023455                                                                          
023456       MOVE WS-IDCATNR-FROM TO W-IDCATNR                                  
023457       PERFORM IMS-GET-KATM01-FROM                                        
023458                                                                          
023459       IF SEGMENT-SAKNAS                                                  
023460         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
023461         DELIMITED BY SIZE INTO FELTEXT                                   
023462         CALL FELLOG                                                      
023463       END-IF                                                             
023464                                                                          
023465       MOVE DAGENS-AAR-PLUS-1  TO W-TIAAAA                                
023466       PERFORM IMS-GET-KATM11-FROM                                        
023467       IF SEGMENT-SAKNAS                                                  
023468         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
023469         DELIMITED BY SIZE INTO FELTEXT                                   
023470         CALL FELLOG                                                      
023471       END-IF                                                             
023472                                                                          
023473       MOVE WS-IDCATNR-TO   TO W-IDCATNR                                  
023474       PERFORM IMS-GET-KATM01-TO                                          
023475                                                                          
023476       IF SEGMENT-SAKNAS                                                  
023477         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
023478         DELIMITED BY SIZE INTO FELTEXT                                   
023479         CALL FELLOG                                                      
023480       END-IF                                                             
023481                                                                          
023482       PERFORM IMS-GET-KATM11-TO                                          
023483                                                                          
023484       MOVE KATM-TAB-TIAAAA    TO KATMK-TAB-TIAAAA                        
023485       MOVE +1 TO RAD-IX                                                  
023486       PERFORM UNTIL  RAD-IX > +12                                        
023487         MOVE MSG-SIGNON-USERID                                           
023488                             TO KATMK-TAB-IDUSER (RAD-IX)                 
023489         MOVE KATM-TAB-FLVADGEN (RAD-IX)                                  
023490                             TO KATMK-TAB-FLVADGEN (RAD-IX)               
023491         MOVE KATM-TAB-KDCATPUB-FOM (RAD-IX)                              
023492                             TO KATMK-TAB-KDCATPUB-FOM (RAD-IX)           
023493         MOVE KATM-TAB-KDCATPUB-TOM (RAD-IX)                              
023494                             TO KATMK-TAB-KDCATPUB-TOM (RAD-IX)           
023497         MOVE DAGENS-DATUM   TO KATMK-TAB-TIUPPDAT (RAD-IX)               
023498         MOVE KATM-TAB-TIOMBRYT (RAD-IX)                                  
023499                             TO KATMK-TAB-TIOMBRYT (RAD-IX)               
023500         MOVE KATM-TAB-TIVADGEN-PLAN (RAD-IX)                             
023501                       TO KATMK-TAB-TIVADGEN-PLAN (RAD-IX)                
023502         MOVE ZERO     TO KATMK-TAB-TIVADGEN-UPPD (RAD-IX)                
023503         ADD +1 TO RAD-IX                                                 
023504       END-PERFORM                                                        
023505                                                                          
023506       IF SEGMENT-FINNS                                                   
023507         PERFORM IMS-REPL-KATM11-TO                                       
023508       ELSE                                                               
023509         PERFORM IMS-ISRT-KATM11-TO                                       
023510       END-IF                                                             
023511                                                                          
023512       MOVE MFS-RENSA-FAELT   TO MOD-IDCATNR-TO-IN                        
023513                                 MOD-IDCATNR-TO-UT                        
023514       MOVE MID-IDCATNR-TO-IN TO MOD-IDCATNR-FROM-UT                      
023515                                 WS-IDCATNR-FROM                          
023516                                 W-IDCATNR                                
023517       INSPECT MOD-IDCATNR-FROM-UT REPLACING LEADING ZERO BY SPACE        
023518                                                                          
023519       MOVE INF-UPDATE-DONE     TO MED-IDMFSINF                           
023520       CALL WMEDKONV USING         MED-WMEDAREA                           
023521       MOVE MED-TEMFSINF        TO MOD-TEMFSINF                           
023522                                                                          
023523     ELSE                                                                 
023524                                                                          
023525**   UPPDATERA GENERERINGSTABELL                                          
023526                                                                          
023527       MOVE DAGENS-AAR         TO W-TIAAAA                                
023528       PERFORM IMS-GET-KATM11-FROM                                        
023529                                                                          
023530       IF SEGMENT-SAKNAS                                                  
023531         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
023532         DELIMITED BY SIZE INTO FELTEXT                                   
023533         CALL FELLOG                                                      
023534       END-IF                                                             
023535                                                                          
023536       MOVE +1 TO KOL-IX                                                  
023537                  RAD-IX                                                  
023538       PERFORM UNTIL  RAD-IX > +12                                        
023539         IF MID-FLVADGEN (KOL-IX, RAD-IX) = ALL '+'                       
023540           CONTINUE                                                       
023541         ELSE                                                             
023542           IF MID-FLVADGEN (KOL-IX, RAD-IX) = 'A'                         
023543           OR MID-FLVADGEN (KOL-IX, RAD-IX) = 'a'                         
023544             MOVE JA       TO KATM-TAB-FLVADGEN (RAD-IX)                  
023545           ELSE                                                           
023546             MOVE NEJ      TO KATM-TAB-FLVADGEN (RAD-IX)                  
023547           END-IF                                                         
023548         END-IF                                                           
023549         ADD +1 TO RAD-IX                                                 
023550       END-PERFORM                                                        
023551                                                                          
023552       PERFORM IMS-REPL-KATM11-FROM                                       
023553                                                                          
023554       MOVE DAGENS-AAR-PLUS-1  TO W-TIAAAA                                
023555       PERFORM IMS-GET-KATM11-FROM                                        
023556                                                                          
023557       IF SEGMENT-SAKNAS                                                  
023558         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
023559         DELIMITED BY SIZE INTO FELTEXT                                   
023560         CALL FELLOG                                                      
023561       END-IF                                                             
023562                                                                          
023563       MOVE +2 TO KOL-IX                                                  
023564       MOVE +1 TO RAD-IX                                                  
023565       PERFORM UNTIL  RAD-IX > +12                                        
023566         IF MID-FLVADGEN (KOL-IX, RAD-IX) = ALL '+'                       
023567           CONTINUE                                                       
023568         ELSE                                                             
023569           IF MID-FLVADGEN (KOL-IX, RAD-IX) = 'A'                         
023570           OR MID-FLVADGEN (KOL-IX, RAD-IX) = 'a'                         
023571             MOVE JA       TO KATM-TAB-FLVADGEN (RAD-IX)                  
023572           ELSE                                                           
023573             MOVE NEJ      TO KATM-TAB-FLVADGEN (RAD-IX)                  
023574           END-IF                                                         
023576         END-IF                                                           
023577         ADD +1 TO RAD-IX                                                 
023578       END-PERFORM                                                        
023579                                                                          
023580       PERFORM IMS-REPL-KATM11-FROM                                       
023581                                                                          
023582       MOVE INF-UPDATE-DONE     TO MED-IDMFSINF                           
023583       CALL WMEDKONV USING         MED-WMEDAREA                           
023584       MOVE MED-TEMFSINF        TO MOD-TEMFSINF                           
023585     END-IF                                                               
023586     .                                                                    
023587     EJECT                                                                
023588 E-ENTER-TRYCKNING SECTION.                                               
023589                                                                          
023590     MOVE NEJ              TO INPUT-FINNS-SW                              
023591                                                                          
023592     IF EGEN-MID OR HELP-MID                                              
023593                                                                          
023594       IF  MID-IDCATNR-TO-IN = ALL '+'                                    
023595         CONTINUE                                                         
023596       ELSE                                                               
023597         MOVE JA         TO INPUT-FINNS-SW                                
023598       END-IF                                                             
023599                                                                          
023600       MOVE +1 TO KOL-IX                                                  
023601       PERFORM UNTIL (KOL-IX > +2 OR                                      
023602                     INPUT-FINNS)                                         
023603         MOVE +1 TO RAD-IX                                                
023604         PERFORM UNTIL (RAD-IX > +12 OR                                   
023605                       INPUT-FINNS)                                       
023606           IF  MID-FLVADGEN (KOL-IX, RAD-IX) = ALL '+'                    
023607             CONTINUE                                                     
023608           ELSE                                                           
023609             MOVE JA         TO INPUT-FINNS-SW                            
023610           END-IF                                                         
023611           ADD +1 TO RAD-IX                                               
023612         END-PERFORM                                                      
023613         ADD +1 TO KOL-IX                                                 
023614       END-PERFORM                                                        
023615                                                                          
023616       IF INPUT-FINNS                                                     
023617         MOVE INF-PRESS-PF11   TO MED-IDMFSINF                            
023618         CALL WMEDKONV USING      MED-WMEDAREA                            
023619         MOVE MED-TEMFSINF     TO MOD-TEMFSINF                            
023620         PERFORM MFS-ROER-EJ-FAELT-UT                                     
023621                                                                          
023622       END-IF                                                             
023623     ELSE                                                                 
023624       PERFORM MFS-RENSA-MOD-FAELT-IN                                     
023625     END-IF                                                               
023626                                                                          
023627     .                                                                    
023628     EJECT                                                                
023629 F-LAES-BAS-VISA-INFO SECTION.                                            
023630                                                                          
023631     MOVE WS-IDCATNR-FROM TO W-IDCATNR                                    
023632     PERFORM IMS-GET-KATM01-FROM                                          
023633                                                                          
023640     IF SEGMENT-SAKNAS                                                    
023650       STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                  
023660       DELIMITED BY SIZE INTO FELTEXT                                     
023670       CALL FELLOG                                                        
023680     END-IF                                                               
023700                                                                          
023800     MOVE KATM-KAT-FLKATVAD  TO MOD-FLKATVAD                              
023900                                                                          
024001     MOVE DAGENS-AAR-MINUS-1 TO W-TIAAAA                                  
024100     PERFORM IMS-GET-KATM11-FROM                                          
024200                                                                          
024210     IF SEGMENT-FINNS                                                     
024300       MOVE +1 TO RAD-IX                                                  
024400       PERFORM UNTIL  RAD-IX > +12                                        
024700         MOVE KATM-TAB-TIVADGEN-UPPD(RAD-IX) TO WS-6NUM                   
024800         MOVE WS-6NUM         TO MOD-KOL1-TIVADGEN-UPPD(RAD-IX)           
024810         IF MOD-KOL1-TIVADGEN-UPPD(RAD-IX) = ZEROES                       
024811           MOVE SPACES        TO MOD-KOL1-TIVADGEN-UPPD(RAD-IX)           
024820         END-IF                                                           
024900         MOVE KATM-TAB-TIOMBRYT(RAD-IX)      TO WS-6NUM                   
025000         MOVE WS-6NUM         TO MOD-KOL1-TIOMBRYT(RAD-IX)                
025001         IF MOD-KOL1-TIOMBRYT(RAD-IX) = ZEROES                            
025002           MOVE SPACES        TO MOD-KOL1-TIOMBRYT(RAD-IX)                
025003         END-IF                                                           
025010         IF KATM-TAB-FLVADGEN (RAD-IX) = JA                               
025020            MOVE 'A'         TO MOD-KOL1-FLVADGEN (RAD-IX)                
025021            MOVE KATM-TAB-KDCATPUB-FOM (RAD-IX) (4:3)                     
025022                             TO MOD-KOL1-KDCATPUB-R (RAD-IX)              
025030         ELSE                                                             
025040            MOVE SPACE       TO MOD-KOL1-FLVADGEN (RAD-IX)                
025042                                MOD-KOL1-KDCATPUB-R (RAD-IX)              
025050         END-IF                                                           
025100         ADD +1 TO RAD-IX                                                 
025200       END-PERFORM                                                        
025210     ELSE                                                                 
025211       MOVE +1 TO RAD-IX                                                  
025212       PERFORM UNTIL  RAD-IX > +12                                        
025214         MOVE SPACE          TO MOD-KOL1-KDCATPUB-R (RAD-IX)              
025215                                MOD-KOL1-FLVADGEN (RAD-IX)                
025216         MOVE SPACE          TO MOD-KOL1-TIVADGEN-UPPD (RAD-IX)           
025218                                MOD-KOL1-TIOMBRYT (RAD-IX)                
025219         ADD +1 TO RAD-IX                                                 
025220       END-PERFORM                                                        
025230     END-IF                                                               
025300                                                                          
025500     MOVE DAGENS-AAR         TO W-TIAAAA                                  
025600     PERFORM IMS-GET-KATM11-FROM                                          
025601                                                                          
025602     IF SEGMENT-FINNS                                                     
025603       MOVE +1 TO RAD-IX                                                  
025604       PERFORM UNTIL  RAD-IX > +12                                        
025605         MOVE KATM-TAB-TIVADGEN-PLAN(RAD-IX) TO WS-6NUM                   
025606         MOVE WS-6NUM        TO MOD-KOL2-TIVADGEN-PLAN (RAD-IX)           
025607         IF MOD-KOL2-TIVADGEN-PLAN(RAD-IX) = ZEROES                       
025608           MOVE SPACES        TO MOD-KOL2-TIVADGEN-PLAN(RAD-IX)           
025609         END-IF                                                           
025610         MOVE KATM-TAB-TIVADGEN-UPPD(RAD-IX) TO WS-6NUM                   
025611         MOVE WS-6NUM        TO MOD-KOL2-TIVADGEN-UPPD (RAD-IX)           
025612         IF MOD-KOL2-TIVADGEN-UPPD(RAD-IX) = ZEROES                       
025613           MOVE SPACES        TO MOD-KOL2-TIVADGEN-UPPD(RAD-IX)           
025614         END-IF                                                           
025615         MOVE KATM-TAB-TIOMBRYT (RAD-IX)     TO WS-6NUM                   
025616         MOVE WS-6NUM        TO MOD-KOL2-TIOMBRYT (RAD-IX)                
025617         IF MOD-KOL2-TIOMBRYT(RAD-IX) = ZEROES                            
025618           MOVE SPACES        TO MOD-KOL2-TIOMBRYT(RAD-IX)                
025619         END-IF                                                           
025620         IF KATM-TAB-FLVADGEN (RAD-IX) = JA                               
025621            MOVE 'A'         TO MOD-KOL2-FLVADGEN (RAD-IX)                
025622            MOVE KATM-TAB-KDCATPUB-FOM (RAD-IX) (4:3)                     
025623                             TO MOD-KOL2-KDCATPUB-R (RAD-IX)              
025624         ELSE                                                             
025625            MOVE SPACE       TO MOD-KOL2-FLVADGEN (RAD-IX)                
025626                                MOD-KOL2-KDCATPUB-R (RAD-IX)              
025627         END-IF                                                           
025628         MOVE KATM-TAB-TIVADGEN-PLAN (RAD-IX) TO TMP1-YYMMDD              
025629         MOVE DAGENS-DATUM                    TO TMP2-YYMMDD              
025630         PERFORM WY2000P1                                                 
025631         IF KATM-TAB-TIVADGEN-UPPD (RAD-IX) > ZERO                        
025632         OR (TMP1-YYMMDD < TMP2-YYMMDD                                    
025633         AND KATM-TAB-FLVADGEN (RAD-IX) = NEJ)                            
025634           MOVE MFS-STAENG-FAELT-NOMOD                                    
025635                             TO MOD-KOL2-FLVADGEN-ATTR (RAD-IX)           
025636         END-IF                                                           
025637                                                                          
025638         ADD +1 TO RAD-IX                                                 
025639       END-PERFORM                                                        
025640       MOVE DAGENS-AAR-PLUS-1  TO W-TIAAAA                                
025641       PERFORM IMS-GET-KATM11-FROM                                        
025642                                                                          
025643       IF SEGMENT-FINNS                                                   
025644                                                                          
025645         MOVE +1 TO RAD-IX                                                
025646         PERFORM UNTIL  RAD-IX > +12                                      
025647           MOVE KATM-TAB-TIVADGEN-PLAN (RAD-IX) TO WS-6NUM                
025648           MOVE WS-6NUM        TO MOD-KOL3-TIVADGEN-PLAN(RAD-IX)          
025649           IF MOD-KOL3-TIVADGEN-PLAN(RAD-IX) = ZEROES                     
025650             MOVE SPACES      TO MOD-KOL3-TIVADGEN-PLAN(RAD-IX)           
025651           END-IF                                                         
025652                                                                          
025653           IF KATM-TAB-FLVADGEN (RAD-IX) = JA                             
025654              MOVE 'A'         TO MOD-KOL3-FLVADGEN (RAD-IX)              
025655              MOVE KATM-TAB-KDCATPUB-FOM (RAD-IX) (4:3)                   
025656                               TO MOD-KOL3-KDCATPUB-R (RAD-IX)            
025657           ELSE                                                           
025658              MOVE SPACE       TO MOD-KOL3-FLVADGEN (RAD-IX)              
025659                                  MOD-KOL3-KDCATPUB-R (RAD-IX)            
025660           END-IF                                                         
025661           ADD +1 TO RAD-IX                                               
025662         END-PERFORM                                                      
025663                                                                          
025664       ELSE                                                               
025665                                                                          
025666       MOVE +1 TO RAD-IX                                                  
025667       PERFORM UNTIL  RAD-IX > +12                                        
025668         MOVE SPACE          TO MOD-KOL3-KDCATPUB-R (RAD-IX)              
025669                                MOD-KOL3-FLVADGEN (RAD-IX)                
025670         MOVE SPACE          TO MOD-KOL3-TIVADGEN-PLAN (RAD-IX)           
025671         MOVE MFS-STAENG-FAELT-NOMOD                                      
025672                             TO MOD-KOL3-FLVADGEN-ATTR (RAD-IX)           
025673                                                                          
025674         ADD +1 TO RAD-IX                                                 
025680       END-PERFORM                                                        
025707                                                                          
025708       END-IF                                                             
025709     ELSE                                                                 
025710       MOVE +1 TO RAD-IX                                                  
025711       PERFORM UNTIL  RAD-IX > +12                                        
025712         MOVE SPACE          TO MOD-KOL2-KDCATPUB-R (RAD-IX)              
025713                                MOD-KOL2-FLVADGEN (RAD-IX)                
025714         MOVE SPACE          TO MOD-KOL2-TIVADGEN-PLAN (RAD-IX)           
025715                                MOD-KOL2-TIVADGEN-UPPD (RAD-IX)           
025716                                MOD-KOL2-TIOMBRYT (RAD-IX)                
025717         MOVE MFS-STAENG-FAELT-NOMOD                                      
025718                             TO MOD-KOL2-FLVADGEN-ATTR (RAD-IX)           
025719                                                                          
025720         ADD +1 TO RAD-IX                                                 
025721       END-PERFORM                                                        
025722                                                                          
025723       MOVE +1 TO RAD-IX                                                  
025724       PERFORM UNTIL  RAD-IX > +12                                        
025725         MOVE SPACE          TO MOD-KOL3-KDCATPUB-R (RAD-IX)              
025726                                MOD-KOL3-FLVADGEN (RAD-IX)                
025727         MOVE SPACE          TO MOD-KOL3-TIVADGEN-PLAN (RAD-IX)           
025728         MOVE MFS-STAENG-FAELT-NOMOD                                      
025729                             TO MOD-KOL3-FLVADGEN-ATTR (RAD-IX)           
025730                                                                          
025731         ADD +1 TO RAD-IX                                                 
025732       END-PERFORM                                                        
025733     END-IF                                                               
025734                                                                          
025735                                                                          
025736                                                                          
025737     .                                                                    
025738     EJECT                                                                
025739 MFS-RENSA-MOD-FAELT-IN SECTION.                                          
025740                                                                          
025741     MOVE MFS-RENSA-FAELT TO MOD-IDCATNR-FROM-IN                          
025742                             MOD-IDCATNR-TO-IN                            
025743                                                                          
025744     MOVE +1 TO RAD-IX                                                    
025745     PERFORM UNTIL RAD-IX > +12                                           
025746       MOVE MFS-RENSA-FAELT  TO MOD-KOL2-FLVADGEN (RAD-IX)                
025747       ADD +1 TO RAD-IX                                                   
025748     END-PERFORM                                                          
025749                                                                          
025750     MOVE +1 TO RAD-IX                                                    
025751     PERFORM UNTIL RAD-IX > +12                                           
025752       MOVE MFS-RENSA-FAELT  TO MOD-KOL3-FLVADGEN (RAD-IX)                
025753       ADD +1 TO RAD-IX                                                   
025760     END-PERFORM                                                          
026200     .                                                                    
026400     SKIP3                                                                
026500 MFS-RENSA-MOD-FAELT-UT SECTION.                                          
026600                                                                          
026700*    --- ALLA UTDATA-FÄLT                                                 
026800     MOVE MFS-RENSA-FAELT TO MOD-FLKATVAD                                 
026900                             MOD-IDCATNR-FROM-UT                          
027000                             MOD-IDCATNR-TO-UT                            
027130     MOVE +1 TO RAD-IX                                                    
027140     PERFORM UNTIL RAD-IX > +12                                           
027150       MOVE MFS-RENSA-FAELT TO MOD-KOL1-KDCATPUB-R (RAD-IX)               
027151                               MOD-KOL1-FLVADGEN (RAD-IX)                 
027152                               MOD-KOL1-TIVADGEN-UPPD (RAD-IX)            
027153                               MOD-KOL1-TIOMBRYT (RAD-IX)                 
027191       ADD +1 TO RAD-IX                                                   
027192     END-PERFORM                                                          
027193                                                                          
027194     MOVE +1 TO RAD-IX                                                    
027195     PERFORM UNTIL RAD-IX > +12                                           
027196       MOVE MFS-RENSA-FAELT TO MOD-KOL2-TIVADGEN-PLAN (RAD-IX)            
027197                               MOD-KOL2-KDCATPUB-R (RAD-IX)               
027198                               MOD-KOL2-FLVADGEN (RAD-IX)                 
027199                               MOD-KOL2-TIVADGEN-UPPD (RAD-IX)            
027200                               MOD-KOL2-TIOMBRYT (RAD-IX)                 
027201       ADD +1 TO RAD-IX                                                   
027202     END-PERFORM                                                          
027203                                                                          
027204     MOVE +1 TO RAD-IX                                                    
027205     PERFORM UNTIL RAD-IX > +12                                           
027206       MOVE MFS-RENSA-FAELT TO MOD-KOL3-TIVADGEN-PLAN (RAD-IX)            
027207                               MOD-KOL3-KDCATPUB-R (RAD-IX)               
027208                               MOD-KOL3-KDCATPUB-R (RAD-IX)               
027209       ADD +1 TO RAD-IX                                                   
027210     END-PERFORM                                                          
027211     .                                                                    
027212     EJECT                                                                
027220 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
027300                                                                          
027400*    --- ALLA UTDATA-FÄLT                                                 
027500                                                                          
027510     MOVE MFS-ROER-EJ-FAELT TO MOD-FLKATVAD                               
027540     MOVE +1 TO RAD-IX                                                    
027550     PERFORM UNTIL RAD-IX > +12                                           
027560       MOVE MFS-ROER-EJ-FAELT TO MOD-KOL1-KDCATPUB-R (RAD-IX)             
027570                                 MOD-KOL1-FLVADGEN (RAD-IX)               
027571                                 MOD-KOL1-TIVADGEN-UPPD (RAD-IX)          
027580                                 MOD-KOL1-TIOMBRYT (RAD-IX)               
027590       ADD +1 TO RAD-IX                                                   
027591     END-PERFORM                                                          
027592                                                                          
027593     MOVE +1 TO RAD-IX                                                    
027594     PERFORM UNTIL RAD-IX > +12                                           
027595       MOVE MFS-ROER-EJ-FAELT  TO MOD-KOL2-TIVADGEN-PLAN (RAD-IX)         
027596                                  MOD-KOL2-KDCATPUB-R (RAD-IX)            
027597                                  MOD-KOL2-FLVADGEN (RAD-IX)              
027598                                  MOD-KOL2-TIVADGEN-UPPD (RAD-IX)         
027599                                  MOD-KOL2-TIOMBRYT (RAD-IX)              
027600       ADD +1 TO RAD-IX                                                   
027601     END-PERFORM                                                          
027602                                                                          
027603     MOVE +1 TO RAD-IX                                                    
027604     PERFORM UNTIL RAD-IX > +12                                           
027605       MOVE MFS-ROER-EJ-FAELT TO MOD-KOL3-TIVADGEN-PLAN (RAD-IX)          
027606                                 MOD-KOL3-KDCATPUB-R (RAD-IX)             
027607                                 MOD-KOL3-FLVADGEN (RAD-IX)               
027608       ADD +1 TO RAD-IX                                                   
027609     END-PERFORM                                                          
027610     .                                                                    
027620     SKIP3                                                                
028200 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
028300                                                                          
028400*    --- ALLA INDATA-FÄLT                                                 
028410     MOVE MFS-ROER-EJ-FAELT TO MOD-IDCATNR-FROM-IN                        
028420                               MOD-IDCATNR-TO-IN                          
028700     .                                                                    
028800     EJECT                                                                
029510 MFS-FORM-ATTR SECTION.                                                   
029520                                                                          
029530*    --- ALLA INDATA-FÄLT                                                 
029540     MOVE MFS-FORMATETS-ATTR   TO MOD-IDCATNR-FROM-ATTR                   
029541                                  MOD-IDCATNR-TO-ATTR                     
029542     MOVE +1 TO RAD-IX                                                    
029543     PERFORM UNTIL RAD-IX > +12                                           
029544       MOVE MFS-FORMATETS-ATTR                                            
029545                             TO MOD-KOL2-FLVADGEN-ATTR (RAD-IX)           
029546       ADD +1 TO RAD-IX                                                   
029547     END-PERFORM                                                          
029548                                                                          
029549     MOVE +1 TO RAD-IX                                                    
029550     PERFORM UNTIL RAD-IX > +12                                           
029551       MOVE MFS-FORMATETS-ATTR                                            
029552                             TO MOD-KOL3-FLVADGEN-ATTR (RAD-IX)           
029553       ADD +1 TO RAD-IX                                                   
029554     END-PERFORM                                                          
029594     .                                                                    
029595     SKIP2                                                                
030300* --- IMS SEKTIONER ---                                                   
030400     SKIP3                                                                
030500 IMS-GET-MSG SECTION.                                                     
030600                                                                          
030700     MOVE '  QC' TO GODK-STATUSKODER                                      
030800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
030900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031000     PERFORM IMS-STATUSKONTROLL                                           
031100     .                                                                    
031200     SKIP3                                                                
031300 IMS-INSERT-MSG SECTION.                                                  
031400                                                                          
031500     IF ENGLISH-TEXT                                                      
031600       MOVE 'N' TO MFS-KDHUVOMR                                           
031700     END-IF                                                               
031800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
031900     MOVE SPACE TO GODK-STATUSKODER                                       
032000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
032100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
032200     PERFORM IMS-STATUSKONTROLL                                           
032300     .                                                                    
032401     EJECT                                                                
032402 IMS-GET-KATM01-FROM SECTION.                                             
032403     STRING 'WLKATM01(IDCATNR  =' W-IDCATNR-X ')'                         
032404          DELIMITED BY SIZE INTO SSA1                                     
032405     MOVE '  GE' TO GODK-STATUSKODER                                      
032406     CALL CBLTDLI USING GHU KATM-PCB     IO-AREA-1 SSA1                   
032407     MOVE KATM-STATUS-CODE TO STATUS-WS                                   
032408     PERFORM IMS-STATUSKONTROLL                                           
032409     .                                                                    
032410     SKIP3                                                                
032420 IMS-GET-KATM01-TO SECTION.                                               
032421     STRING 'WLKATM01(IDCATNR  =' W-IDCATNR-X ')'                         
032422          DELIMITED BY SIZE INTO SSA3                                     
032423     MOVE '  GE' TO GODK-STATUSKODER                                      
032424     CALL CBLTDLI USING GHU KATM-PCB     IO-AREA-3 SSA3                   
032425     MOVE KATM-STATUS-CODE TO STATUS-WS                                   
032426     PERFORM IMS-STATUSKONTROLL                                           
032427     .                                                                    
032428     SKIP3                                                                
032429 IMS-GET-KATM11-FROM SECTION.                                             
032430                                                                          
032432     STRING 'WLKATM11(TIAAAA   =' W-TIAAAA-X ')'                          
032434          DELIMITED BY SIZE INTO SSA1                                     
032435     MOVE '  GE' TO GODK-STATUSKODER                                      
032436     CALL CBLTDLI USING GHU KATM-PCB     IO-AREA-2 SSA1                   
032437     MOVE KATM-STATUS-CODE TO STATUS-WS                                   
032438     PERFORM IMS-STATUSKONTROLL                                           
032439     .                                                                    
032440     SKIP3                                                                
032441 IMS-GET-KATM11-TO SECTION.                                               
032442                                                                          
032443     STRING 'WLKATM11(TIAAAA   =' W-TIAAAA-X ')'                          
032444          DELIMITED BY SIZE INTO SSA3                                     
032445     MOVE '  GE' TO GODK-STATUSKODER                                      
032446     CALL CBLTDLI USING GHU KATM-PCB     IO-AREA-4 SSA3                   
032447     MOVE KATM-STATUS-CODE TO STATUS-WS                                   
032448     PERFORM IMS-STATUSKONTROLL                                           
032449     .                                                                    
032450     SKIP3                                                                
032451 IMS-ISRT-KATM11-FROM SECTION.                                            
032452                                                                          
032453     STRING 'WLKATM01(IDCATNR  =' W-IDCATNR-X ')'                         
032454          DELIMITED BY SIZE INTO SSA1                                     
032455     MOVE 'WLKATM11 ' TO SSA2                                             
032456     MOVE '  '   TO GODK-STATUSKODER                                      
032457     CALL CBLTDLI USING ISRT KATM-PCB IO-AREA-2 SSA1 SSA2                 
032458     MOVE KATM-STATUS-CODE TO STATUS-WS                                   
032459     PERFORM IMS-STATUSKONTROLL                                           
032460     .                                                                    
032461     SKIP3                                                                
032462 IMS-ISRT-KATM11-TO SECTION.                                              
032463                                                                          
032464     STRING 'WLKATM01(IDCATNR  =' W-IDCATNR-X ')'                         
032465          DELIMITED BY SIZE INTO SSA3                                     
032466     MOVE 'WLKATM11 ' TO SSA4                                             
032467     MOVE '  '   TO GODK-STATUSKODER                                      
032468     CALL CBLTDLI USING ISRT KATM-PCB IO-AREA-4 SSA3 SSA4                 
032469     MOVE KATM-STATUS-CODE TO STATUS-WS                                   
032470     PERFORM IMS-STATUSKONTROLL                                           
032471     .                                                                    
032472     SKIP3                                                                
032473 IMS-REPL-KATM11-FROM SECTION.                                            
032474                                                                          
032475     MOVE '  ' TO GODK-STATUSKODER                                        
032476     CALL CBLTDLI USING REPL KATM-PCB     IO-AREA-2                       
032477     MOVE KATM-STATUS-CODE TO STATUS-WS                                   
032478     PERFORM IMS-STATUSKONTROLL                                           
032480     .                                                                    
032500     EJECT                                                                
032510 IMS-REPL-KATM11-TO SECTION.                                              
032520                                                                          
032530     MOVE '  ' TO GODK-STATUSKODER                                        
032540     CALL CBLTDLI USING REPL KATM-PCB     IO-AREA-4                       
032550     MOVE KATM-STATUS-CODE TO STATUS-WS                                   
032560     PERFORM IMS-STATUSKONTROLL                                           
032570     .                                                                    
032580     EJECT                                                                
032600 IMS-STATUSKONTROLL SECTION.                                              
032700                                                                          
032800     SET STATUS-IX TO 1                                                   
032900     SEARCH GODK-STATUS                                                   
033000       AT END                                                             
033100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
033200         DELIMITED BY SIZE INTO FELTEXT                                   
033300         CALL FELLOG                                                      
033400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
033500         CONTINUE                                                         
033600     END-SEARCH                                                           
033700     .                                                                    
033710     EJECT                                                                
033800*    -COPY WY2000P1                                                       
