001300 ID DIVISION.                                                             
001400 PROGRAM-ID.     W5020200.                                                
001500 AUTHOR.         INGVAR SKJELBRED.                                        
001600 DATE-WRITTEN.   96/11/21.                                                
001700 DATE-COMPILED.                                                           
001800                                                                          
001900*    FUNKTION:                                                            
002000*        BILD 5202 - UPPDATERING AV LOKAL PRODUKT KOD                     
002100*                                                                         
002210*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSAKTION: W5T202                                              
002600*        MID:         W5I20201                                            
002700*                                                                         
002800*    UTDATA.                                                              
002900*        MOD:         W5O20201                                            
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500 WORKING-STORAGE SECTION.                                                 
003501                                                                          
003510*    -- CHECKED BY WY2000                                                 
003600 77  IDPGM                       PIC X(08)   VALUE 'W5020200'.            
003700                                                                          
003800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004000                                                                          
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300                                                                          
004401*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004402                                                                          
004403 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004404 77  MAX-INDX                    PIC S9(4)  VALUE +12   COMP SYNC.        
004405 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004406 01  RED-IDARTNR                 PIC Z(8)9.                               
004407                                                                          
004500*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004700                                                                          
004801 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004802     88  INDATA-OK                           VALUE 'J'.                   
004810     88  INDATA-FEL                          VALUE 'N'.                   
004900                                                                          
005000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005100     88  NYCKLAR-OK                          VALUE 'J'.                   
005200     88  NYCKLAR-FEL                         VALUE 'N'.                   
005300                                                                          
005320 77  ARTIKEL-SW                  PIC X       VALUE 'J'.                   
005330     88  SOK-ARTIKEL                         VALUE 'J'.                   
005340     88  SOK-LISTA                           VALUE 'N'.                   
005350                                                                          
005400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005500     88  EGEN-MID                            VALUE '5202'.                
005600     88  GODK-MID                            VALUE '5201' '5202'          
005700                                                   '5203' '5204'          
005800                                                   '5205' '5206'          
005900                                                   '5207' '5208'          
006000                                                   '5209'.                
006100     88  HELP-MID                            VALUE '0551'.                
006210     EJECT                                                                
006220 01  FILLER               PIC X(16)   VALUE 'LOCAL-PRODUCT'.              
006230 01  TEST-KDPSLLOC               PIC 9(2).                                
006240*01  FILLER -COPY WWLOCPC   -RED TEST-KDPSLLOC                            
006250                                                                          
006300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006400 01  GENERELLA-SUBPROGRAM.                                                
006500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006910     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007000     EJECT                                                                
007100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007200*01 -COPY WMEDAREA                                                        
007300     SKIP3                                                                
007400 01  MESSAGE-CODES.                                                       
007501     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007502     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007503     03  ERR-INFO-MISSING        PIC X(3)    VALUE '005'.                 
007504     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
007510     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
007601     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
007610     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
007700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007801     EJECT                                                                
007810*01  -COPY WDATAREA                                                       
007900     EJECT                                                                
008000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008100*                                                                         
008200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008300     SKIP3                                                                
008400*01 -COPY WMSGINIT                                                        
008501     EJECT                                                                
008700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008800*                                                                         
008900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009000     SKIP3                                                                
009100*01  MID -COPY W5I20201                                                   
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009400     SKIP3                                                                
009500*01  -COPY WMSGAREA                                                       
009600     EJECT                                                                
009700     03  MOD REDEFINES MSG-AREA.                                          
009800*      05  -COPY W5O20201                                                 
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
010810 01  SPAR-AREA.                                                           
010820     03  SPAR-IDTRANS            PIC X(4)    VALUE '5202'.                
010830     03  SPAR-IDARTNR-ENTER      PIC X(9).                                
010840     03  SPAR-IDARTNR-NEXT       PIC X(9).                                
010850     SKIP3                                                                
010900 01  NYCKLAR-TILL-DLI.                                                    
011001*    --- VÄRDE PÅ BLÄDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN             
011002     03  W-IDARTNR-MIN-X.                                                 
011003         05  W-IDARTNR-MIN     PIC S9(9) VALUE ZERO COMP-3.               
011004     03  W-IDARTNR-MAX-X.                                                 
011005         05  W-IDARTNR-MAX     PIC S9(9) VALUE ZERO COMP-3.               
011010     03  W-IDSKYLT-X.                                                     
011011         05  W-IDSKYLT         PIC X(3)  VALUE SPACE.                     
011012     03  W-IDARTNR-X.                                                     
011013         05  W-IDARTNR         PIC S9(9) VALUE ZERO COMP-3.               
011017     03  W-WDGXKEY-X.                                                     
011018         05  FILLER              PIC X(4)    VALUE '5141'.                
011019         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
011020     03  W-KDSEGKEY-X.                                                    
011030         05  W-KDSEGKEY        PIC X     VALUE '1'.                       
011100     SKIP2                                                                
011200*    --- STATUS-KOD FRÅN IMS                                              
011300 01  STATUS-WS                   PIC XX.                                  
011400     88  SEGMENT-FINNS                       VALUE '  '.                  
011500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011700     SKIP2                                                                
011701 77  RAD-WS                   PIC X          VALUE 'J'.                   
011702     88  RAD-FINNS                           VALUE 'J'.                   
011703     88  RAD-SAKNAS                          VALUE 'N'.                   
011710     SKIP2                                                                
011800 01  GODK-STATUSKODER.                                                    
011900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012000     SKIP3                                                                
012100 01  SSA1                        PIC X(64).                               
012200 01  SSA2                        PIC X(64).                               
012300     EJECT                                                                
012400*    --- IMS FUNKTIONSKODER                                               
012500*01  -COPY W0003                                                          
012700     EJECT                                                                
012800*    ---  DLI INPUT-OUTPUT AREA                                           
012900                                                                          
013001 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTC01'.                    
013002 01  DLI-IO-WLARTC01.                                                     
013003*    03  -COPY WDK601  -PRE ARTC-                                         
013004     EJECT                                                                
013005 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTC11'.                    
013006 01  DLI-IO-WLARTC11.                                                     
013007*    03  -COPY WDK611  -PRE ARTC-                                         
013008     EJECT                                                                
013310 01  FILLER         PIC X(24)   VALUE 'DLI-IO-WLBENA11'.                  
013320 01  DLI-IO-WLBENA11.                                                     
013360*    03  -COPY WDD311  -PRE BENA-                                         
013361     EJECT                                                                
013362 01  FILLER         PIC X(24)   VALUE 'DLI-IO-WL514101'.                  
013363 01  DLI-IO-WL514101.                                                     
013364*    03  -COPY WDGX01   -PRE 5141-                                        
013365     EJECT                                                                
013366 01  FILLER         PIC X(24)   VALUE 'DLI-IO-WL514111'.                  
013367 01  DLI-IO-WL514111.                                                     
013368*    03  -COPY WDGX5142 -PRE 5141-                                        
013369     EJECT                                                                
013400 LINKAGE SECTION.                                                         
013500*01  -COPY W0009   -PRE MSG-                                              
013600*01  -COPY W0008   -PRE USEA-                                             
013700     05  FILLER                  PIC X.                                   
013801     EJECT                                                                
013802*01  -COPY W0008  -PRE ARTC-                                              
013810     05  FILLER                  PIC X.                                   
013900     EJECT                                                                
014003*01  -COPY W0008  -PRE BENA-                                              
014004     05  FILLER                  PIC X.                                   
014005     EJECT                                                                
014006*01  -COPY W0008  -PRE 5141-                                              
014007     05  FILLER                  PIC X.                                   
014008     EJECT                                                                
014009 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB ARTC-PCB                      
014010                            BENA-PCB 5141-PCB.                            
014011 MAIN SECTION.                                                            
014012     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB ARTC-PCB                      
014020                            BENA-PCB 5141-PCB.                            
014100                                                                          
014300     PERFORM IMS-GET-MSG                                                  
014400     IF SEGMENT-FINNS                                                     
014500       PERFORM A-INIT                                                     
014600       PERFORM B-KOLLA-NYCKLAR                                            
014700       IF NYCKLAR-OK                                                      
014801         IF MFS-UPDATE                                                    
014802           PERFORM G-KOLLA-INPUT                                          
014803           IF INDATA-OK                                                   
014804             PERFORM H-UPPDATERA                                          
014805           END-IF                                                         
014810         ELSE                                                             
014901           IF MFS-FIRST                                                   
014902             PERFORM C-FOERSTA-SIDA                                       
014903           ELSE                                                           
014904             IF MFS-NEXT                                                  
014905               PERFORM D-NAESTA-SIDA                                      
014906             ELSE                                                         
014907               PERFORM E-SAMMA-SIDA                                       
014908             END-IF                                                       
014910           END-IF                                                         
015110         END-IF                                                           
015200         PERFORM F-LAES-VISA-INFO                                         
015300       END-IF                                                             
015600       COMPUTE MSG-KVLL = LENGTH OF MOD-W5O20201 + 4                      
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
016800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I20201                 
016900       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017100     ELSE                                                                 
017200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I20201                  
017300       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
017400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
017500     END-IF                                                               
017600                                                                          
017700     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
017750     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
017900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018000                                                                          
018100     MOVE LOW-VALUE TO MSG-AREA                                           
018200     MOVE 'W5O202N1' TO MFS-IDMOD                                         
018300     MOVE '5202' TO MOD-IDTRANS                                           
018400     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
018500                                                                          
018600     IF EGEN-MID OR HELP-MID                                              
018700       CONTINUE                                                           
018800     ELSE                                                                 
018900       MOVE SPACE TO MFS-KDTRTYP                                          
019000       MOVE '7' TO MFS-IDPFK                                              
019100     END-IF                                                               
019110                                                                          
019200     IF MID-IDARTNR-IN NOT = ALL '+'                                      
019300       MOVE '7'         TO MFS-IDPFK                                      
019310       MOVE SPACE       TO MFS-KDTRTYP                                    
019320     END-IF                                                               
019321                                                                          
019322     IF MSGI-IDLAND-SPR = 'GB'                                            
019323        MOVE 'USA'     TO W-IDSKYLT                                       
019324        MOVE +2        TO SPRAK-IX                                        
019325     ELSE                                                                 
019326        MOVE 'S  '     TO W-IDSKYLT                                       
019327        MOVE +1        TO SPRAK-IX                                        
019328     END-IF                                                               
019400     .                                                                    
019500     EJECT                                                                
019600 B-KOLLA-NYCKLAR SECTION.                                                 
019700                                                                          
019800     MOVE ALL '+'           TO MSGI-WMSGINIT                              
019900     MOVE '001'             TO MSGI-KDCALL                                
020000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
020100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
020200     MOVE '5202'            TO MSGI-IDTRANS                               
020300     IF GODK-MID                                                          
020400        IF MID-IDARTNR-IN NOT = ALL '+'                                   
020410           MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                            
020420        END-IF                                                            
020500     END-IF                                                               
020600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
020610     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
020700                                                                          
020800     MOVE JA TO NYCKLAR-SW                                                
021001                                                                          
021002*    -- KONTROLL AV IDARTNR                                               
021003     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
021004                                                                          
021010     IF MSGI-IDARTNR NUMERIC                                              
021011        IF MSGI-IDARTNR > ZERO                                            
021012           MOVE MSGI-IDARTNR TO W-IDARTNR-MIN                             
021013                                W-IDARTNR-MAX                             
021014        ELSE                                                              
021015           MOVE ZERO         TO W-IDARTNR-MIN                             
021016           MOVE 999999999    TO W-IDARTNR-MAX                             
021017        END-IF                                                            
021018     ELSE                                                                 
021019       MOVE NEJ TO NYCKLAR-SW                                             
021020     END-IF                                                               
021101                                                                          
021102     IF GODK-MID OR NYCKLAR-OK                                            
021104       MOVE MSGI-IDARTNR        TO RED-IDARTNR                            
021105       MOVE RED-IDARTNR         TO MOD-IDARTNR-UT                         
021106     ELSE                                                                 
021107       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
021110     END-IF                                                               
021200                                                                          
021300     IF NYCKLAR-FEL                                                       
021400       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
021410       MOVE 'GB'          TO MED-IDSKYLT                                  
021500       CALL WMEDKONV USING MED-WMEDAREA                                   
021600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
021700       PERFORM MFS-RENSA-FAELT-IN                                         
021900     END-IF                                                               
022000     .                                                                    
022101     EJECT                                                                
022102 C-FOERSTA-SIDA SECTION.                                                  
022104                                                                          
022105     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
022106     MOVE 'GB'          TO MED-IDSKYLT                                    
022107     CALL WMEDKONV USING MED-WMEDAREA                                     
022108     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
022109                                                                          
022110     PERFORM MFS-RENSA-FAELT-IN                                           
022111     .                                                                    
022112     EJECT                                                                
022113 D-NAESTA-SIDA SECTION.                                                   
022114                                                                          
022115     IF SPAR-IDTRANS = '5202'                                             
022116       MOVE SPAR-IDARTNR-NEXT TO W-IDARTNR-MIN                            
022117       IF SPAR-IDARTNR-NEXT = ALL ZERO                                    
022118          MOVE SPAR-IDARTNR-ENTER TO W-IDARTNR-MIN                        
022119       END-IF                                                             
022120     ELSE                                                                 
022121       PERFORM MFS-RENSA-FAELT-IN                                         
022122     END-IF                                                               
022123     .                                                                    
022124     EJECT                                                                
022125 E-SAMMA-SIDA SECTION.                                                    
022127                                                                          
022128     IF SPAR-IDTRANS = '5202' OR '0551'                                   
022129       IF SPAR-IDARTNR-ENTER NUMERIC                                      
022130          MOVE SPAR-IDARTNR-ENTER TO W-IDARTNR-MIN                        
022131       ELSE                                                               
022132          MOVE ZERO               TO W-IDARTNR-MIN                        
022133       END-IF                                                             
022134       IF MID-INPUT = ALL '+'                                             
022135       AND MID-UPD = ALL '+'                                              
022136         PERFORM MFS-RENSA-FAELT-IN                                       
022137       ELSE                                                               
022138         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
022139         MOVE 'GB'          TO MED-IDSKYLT                                
022140         CALL WMEDKONV USING MED-WMEDAREA                                 
022141         MOVE MED-MFSINF TO MOD-TEMFSFEL                                  
022142         PERFORM EA-MID-INDATA-TILL-MOD                                   
022143       END-IF                                                             
022144     ELSE                                                                 
022145       PERFORM MFS-RENSA-FAELT-IN                                         
022146     END-IF                                                               
022147     .                                                                    
022148     EJECT                                                                
022149 EA-MID-INDATA-TILL-MOD SECTION.                                          
022150                                                                          
022151     MOVE +1 TO INDX                                                      
022152                                                                          
022153     PERFORM UNTIL INDX > MAX-INDX                                        
022154        IF MID-KDBEH (INDX)  = ALL '+'                                    
022155           MOVE MFS-RENSA-FAELT   TO MOD-KDBEH (INDX)                     
022156           MOVE MFS-ALFA-FAELT-RAETT TO                                   
022157                               MOD-KDBEH-ATTR (INDX)                      
022158        ELSE                                                              
022159           MOVE MFS-ROER-EJ-FAELT TO MOD-KDBEH (INDX)                     
022160           MOVE MFS-ADD-LAES-IN-FAELT TO                                  
022161                               MOD-KDBEH-ATTR (INDX)                      
022162        END-IF                                                            
022163        IF MID-KDPSLLOC-IN (INDX) = ALL '+'                               
022164           MOVE MFS-RENSA-FAELT   TO MOD-KDPSLLOC-NEW (INDX)              
022165           MOVE MFS-ALFA-FAELT-RAETT TO                                   
022166                               MOD-KDPSLLOC-NEW-ATTR (INDX)               
022167        ELSE                                                              
022168           MOVE MFS-ROER-EJ-FAELT TO MOD-KDPSLLOC-NEW (INDX)              
022169           MOVE MFS-ADD-LAES-IN-FAELT TO                                  
022170                               MOD-KDPSLLOC-NEW-ATTR (INDX)               
022171        END-IF                                                            
022172        ADD +1 TO INDX                                                    
022173     END-PERFORM                                                          
022174                                                                          
022175                                                                          
022176     IF MID-IDARTNR-UPD  = ALL '+'                                        
022177        MOVE MFS-RENSA-FAELT   TO MOD-IDARTNR-UPD                         
022178        MOVE MFS-ALFA-FAELT-RAETT TO                                      
022179                               MOD-IDARTNR-UPD-ATTR                       
022180     ELSE                                                                 
022181        MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-UPD                         
022182        MOVE MFS-ADD-LAES-IN-FAELT TO                                     
022183                               MOD-IDARTNR-UPD-ATTR                       
022184     END-IF                                                               
022185                                                                          
022186     IF MID-KDPSLLOC-UPD = ALL '+'                                        
022187        MOVE MFS-RENSA-FAELT   TO MOD-KDPSLLOC-UPD                        
022188        MOVE MFS-ALFA-FAELT-RAETT TO                                      
022189                               MOD-KDPSLLOC-UPD-ATTR                      
022190     ELSE                                                                 
022191        MOVE MFS-ROER-EJ-FAELT TO MOD-KDPSLLOC-UPD                        
022192        MOVE MFS-ADD-LAES-IN-FAELT TO                                     
022193                               MOD-KDPSLLOC-UPD-ATTR                      
022194     END-IF                                                               
022195                                                                          
022196     .                                                                    
022200     EJECT                                                                
022400 F-LAES-VISA-INFO SECTION.                                                
022401******************************************************************        
022402*** SÖKNING SKER PÅ TVÅ SÄTT. PÅ EN ENSKILD ARTIKEL OCH **********        
022403*** PÅ EN HEL LISTA(KÖ)      *************************************        
022404******************************************************************        
022500                                                                          
022610     IF MSGI-IDARTNR = ZERO                                               
022611        PERFORM FA-SKAPA-LISTA                                            
022612     ELSE                                                                 
022613        PERFORM FB-SKAPA-EN-RAD                                           
022614     END-IF                                                               
022615                                                                          
023598     MOVE '002'      TO MSGI-KDCALL                                       
023600     MOVE '5202'     TO SPAR-IDTRANS                                      
023601     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
023602     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
023603     MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                    
023604     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
023605                                                                          
023610     .                                                                    
023700     EJECT                                                                
023801 FA-SKAPA-LISTA SECTION.                                                  
023811                                                                          
023820     PERFORM IMS-GET-WL514101                                             
023821                                                                          
023830     IF SEGMENT-SAKNAS                                                    
023840        MOVE ERR-INFO-MISSING TO MED-IDMFSFEL                             
023841        MOVE 'GB'             TO MED-IDSKYLT                              
023850        CALL WMEDKONV USING MED-WMEDAREA                                  
023860        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
023870        PERFORM MFS-RENSA-FAELT-IN                                        
023880        MOVE ZERO    TO SPAR-IDARTNR-ENTER                                
023890        MOVE ZERO    TO SPAR-IDARTNR-NEXT                                 
023891        MOVE MFS-STAENG-FAELT TO MOD-KDPSLLOC-UPD-ATTR                    
023892                                 MOD-IDARTNR-UPD-ATTR                     
023893     ELSE                                                                 
023894        MOVE W-IDARTNR-MIN   TO W-IDARTNR                                 
023895        MOVE +1 TO INDX                                                   
023896        PERFORM IMS-GHNP-WL514111                                         
023897        IF SEGMENT-FINNS                                                  
023898           MOVE 5141-5142-IDARTNR TO SPAR-IDARTNR-ENTER                   
023899           MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UPD                        
023900                                 MOD-BEART-UPD                            
023901           MOVE MFS-STAENG-FAELT TO MOD-KDPSLLOC-UPD-ATTR                 
023902                                 MOD-IDARTNR-UPD-ATTR                     
023903        ELSE                                                              
023904           MOVE ERR-INFO-MISSING TO MED-IDMFSFEL                          
023905           MOVE 'GB'             TO MED-IDSKYLT                           
023906           CALL WMEDKONV USING MED-WMEDAREA                               
023907           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
023908           PERFORM MFS-RENSA-FAELT-IN                                     
023909           MOVE ZERO    TO SPAR-IDARTNR-ENTER                             
023910           MOVE ZERO    TO SPAR-IDARTNR-NEXT                              
023911           MOVE MFS-STAENG-FAELT TO MOD-KDPSLLOC-UPD-ATTR                 
023912                                 MOD-IDARTNR-UPD-ATTR                     
023914        END-IF                                                            
023916                                                                          
023917        PERFORM UNTIL INDX > MAX-INDX                                     
023918          IF SEGMENT-FINNS                                                
023920             MOVE 5141-5142-IDARTNR TO MOD-IDARTNR (INDX)                 
023921                                      W-IDARTNR                           
023923             PERFORM IMS-GET-BENA                                         
023924             IF SEGMENT-FINNS                                             
023925                MOVE BENA-TEXT-BEART TO MOD-BEART (INDX)                  
023926             ELSE                                                         
023927                MOVE SPACE           TO MOD-BEART (INDX)                  
023928             END-IF                                                       
023930             MOVE 5141-5142-KDPRODSL TO MOD-KDPRODSL (INDX)               
023931             MOVE 5141-5142-KDPSLLOC TO MOD-KDPSLLOC-OLD (INDX)           
023932             MOVE 5141-5142-IDARTNR TO MOD-IDARTNR (INDX)                 
023933             MOVE 5141-5142-KDPSLLOC-NEW                                  
023934                                     TO MOD-KDPSLLOC-NEW (INDX)           
023935             PERFORM IMS-GHNP-WL514111                                    
023940          ELSE                                                            
023942             MOVE MFS-RENSA-FAELT TO MOD-IDARTNR (INDX)                   
023943                                    MOD-KDBEH   (INDX)                    
023944                                    MOD-KDPSLLOC-OLD (INDX)               
023945                                    MOD-KDPSLLOC-NEW (INDX)               
023946                                    MOD-BEART    (INDX)                   
023947                                    MOD-KDPRODSL (INDX)                   
023948             MOVE MFS-STAENG-FAELT TO                                     
023949                                     MOD-KDPSLLOC-NEW-ATTR (INDX)         
023950                                     MOD-KDBEH-ATTR   (INDX)              
023951             MOVE 'GE'            TO STATUS-WS                            
023952          END-IF                                                          
023953          ADD 1 TO INDX                                                   
023954        END-PERFORM                                                       
023955                                                                          
023956        IF SEGMENT-FINNS                                                  
023957           MOVE 5141-5142-IDARTNR TO SPAR-IDARTNR-NEXT                    
023958           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
023959           MOVE 'GB'             TO MED-IDSKYLT                           
023960           CALL WMEDKONV USING MED-WMEDAREA                               
023961           MOVE MED-TEMFSINF TO MOD-TEMFSINF                              
023962        ELSE                                                              
023963           MOVE ZERO              TO SPAR-IDARTNR-NEXT                    
023964        END-IF                                                            
023966                                                                          
023967     END-IF                                                               
023968                                                                          
023969     .                                                                    
023970     EJECT                                                                
023980                                                                          
024000 FB-SKAPA-EN-RAD SECTION.                                                 
024100                                                                          
024513     MOVE W-IDARTNR-MIN   TO W-IDARTNR                                    
024514     MOVE +1 TO INDX                                                      
024515     PERFORM IMS-GET-WL514111                                             
024517     IF SEGMENT-FINNS                                                     
024519        MOVE 5141-5142-IDARTNR TO SPAR-IDARTNR-ENTER                      
024520        MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UPD                           
024521                                MOD-BEART-UPD                             
024522        MOVE 5141-5142-IDARTNR TO MOD-IDARTNR (INDX)                      
024523                                     W-IDARTNR                            
024525        PERFORM IMS-GET-BENA                                              
024526        IF SEGMENT-FINNS                                                  
024527           MOVE BENA-TEXT-BEART TO MOD-BEART (INDX)                       
024528        ELSE                                                              
024529           MOVE SPACE           TO MOD-BEART (INDX)                       
024530        END-IF                                                            
024532        MOVE 5141-5142-KDPRODSL TO MOD-KDPRODSL (INDX)                    
024533        MOVE 5141-5142-KDPSLLOC TO MOD-KDPSLLOC-OLD (INDX)                
024534        MOVE 5141-5142-IDARTNR TO MOD-IDARTNR (INDX)                      
024535        MOVE 5141-5142-KDPSLLOC-NEW                                       
024536                                     TO MOD-KDPSLLOC-NEW (INDX)           
024537        ADD 1 TO INDX                                                     
024538        PERFORM UNTIL INDX > MAX-INDX                                     
024539           MOVE MFS-RENSA-FAELT TO MOD-IDARTNR (INDX)                     
024540                                   MOD-KDBEH   (INDX)                     
024541                                   MOD-KDPSLLOC-OLD (INDX)                
024542                                   MOD-KDPSLLOC-NEW (INDX)                
024543                                   MOD-BEART    (INDX)                    
024544                                   MOD-KDPRODSL (INDX)                    
024545           MOVE MFS-STAENG-FAELT TO MOD-KDPSLLOC-NEW-ATTR (INDX)          
024546                                    MOD-KDBEH-ATTR   (INDX)               
024547           ADD 1 TO INDX                                                  
024548           MOVE 'GE'            TO STATUS-WS                              
024549        END-PERFORM                                                       
024550        MOVE MFS-STAENG-FAELT TO                                          
024551                                    MOD-KDPSLLOC-UPD-ATTR                 
024552                                    MOD-IDARTNR-UPD-ATTR                  
024553     ELSE                                                                 
024554        MOVE ZERO            TO W-IDARTNR-MIN                             
024555        MOVE W-IDARTNR-MIN   TO SPAR-IDARTNR-ENTER                        
024556        MOVE MSGI-IDARTNR    TO MID-IDARTNR-UPD                           
024557                                W-IDARTNR                                 
024558        MOVE MSGI-IDARTNR    TO RED-IDARTNR                               
024559        MOVE RED-IDARTNR     TO MOD-IDARTNR-UPD                           
024561        PERFORM IMS-GET-BENA                                              
024562        IF SEGMENT-FINNS                                                  
024563           MOVE BENA-TEXT-BEART TO MOD-BEART-UPD                          
024564        ELSE                                                              
024565           MOVE SPACE        TO MOD-BEART-UPD                             
024566        END-IF                                                            
024568        PERFORM IMS-GET-ARTC-WLARTC01                                     
024569        IF SEGMENT-FINNS                                                  
024570            MOVE ARTC-ART-KDPRODSL  TO MOD-KDPRODSL-UPD                   
024571            PERFORM IMS-GET-ARTC-WLARTC11                                 
024572            IF SEGMENT-FINNS                                              
024573               MOVE ARTC-CLAG-KDPSLLOC TO MOD-KDPSLLOC-OLD-UPD            
024574            ELSE                                                          
024575               MOVE ZERO               TO MOD-KDPSLLOC-OLD-UPD            
024576            END-IF                                                        
024577        ELSE                                                              
024578            MOVE ZERO               TO MOD-KDPRODSL-UPD                   
024579            MOVE ZERO               TO MOD-KDPSLLOC-OLD-UPD               
024580        END-IF                                                            
024581        MOVE MFS-ADD-LAES-IN-FAELT TO                                     
024582                                    MOD-IDARTNR-UPD-ATTR                  
024583        PERFORM UNTIL INDX > MAX-INDX                                     
024584           MOVE MFS-RENSA-FAELT TO MOD-IDARTNR (INDX)                     
024585                                   MOD-KDBEH   (INDX)                     
024586                                   MOD-KDPSLLOC-OLD (INDX)                
024587                                   MOD-KDPSLLOC-NEW (INDX)                
024588                                   MOD-BEART    (INDX)                    
024589                                   MOD-KDPRODSL (INDX)                    
024590           MOVE MFS-STAENG-FAELT TO MOD-KDPSLLOC-NEW-ATTR (INDX)          
024591                                    MOD-KDBEH-ATTR   (INDX)               
024592           MOVE 'GE'            TO STATUS-WS                              
024593           ADD 1 TO INDX                                                  
024594        END-PERFORM                                                       
024595     END-IF                                                               
024596                                                                          
024597     IF SEGMENT-FINNS                                                     
024598         MOVE 5141-5142-IDARTNR TO SPAR-IDARTNR-NEXT                      
024599         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
024600         MOVE 'GB'             TO MED-IDSKYLT                             
024601         CALL WMEDKONV USING MED-WMEDAREA                                 
024602         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
024603     ELSE                                                                 
024604         MOVE ZERO              TO SPAR-IDARTNR-NEXT                      
024605     END-IF                                                               
024606                                                                          
024607     .                                                                    
024608     EJECT                                                                
024609                                                                          
024610 G-KOLLA-INPUT SECTION.                                                   
024611                                                                          
024612     MOVE JA  TO INDATA-SW                                                
024613     IF MID-INPUT = ALL '+'                                               
024614     AND MID-UPD = ALL '+'                                                
024615       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
024616       MOVE 'GB'             TO MED-IDSKYLT                               
024617       CALL WMEDKONV USING MED-WMEDAREA                                   
024618       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
024619       PERFORM MFS-ROER-EJ-FAELT-UT                                       
024620       MOVE NEJ TO INDATA-SW                                              
024621     ELSE                                                                 
024622        IF MID-INPUT NOT = ALL '+'                                        
024623           MOVE +1 TO INDX                                                
024624           PERFORM UNTIL INDX > MAX-INDX                                  
024625                                                                          
024626              IF MID-KDBEH (INDX) NOT = ALL '+'                           
024628                 IF MID-KDBEH (INDX) = 'D'                                
024630                    MOVE MFS-NUM-FAELT-RAETT TO                           
024631                               MOD-KDBEH-ATTR (INDX)                      
024632                    IF MID-KDPSLLOC-IN (INDX) NOT = ALL '+'               
024634                       MOVE MFS-NUM-FAELT-FEL TO                          
024635                               MOD-KDPSLLOC-NEW-ATTR (INDX)               
024636                       MOVE NEJ TO INDATA-SW                              
024637                    END-IF                                                
024638                 ELSE                                                     
024639                   IF MID-KDBEH (INDX) = 'C'                              
024641                      MOVE MFS-NUM-FAELT-RAETT TO                         
024642                               MOD-KDBEH-ATTR (INDX)                      
024643                      IF MID-KDPSLLOC-IN (INDX) NOT = ALL '+'             
024645                         MOVE MID-KDPSLLOC-IN (INDX) TO                   
024646                                 TEST-KDPSLLOC                            
024647*                        IF WWLOCPS-PCODE-LAB                             
024649                            MOVE MFS-NUM-FAELT-RAETT TO                   
024650                               MOD-KDPSLLOC-NEW-ATTR (INDX)               
024651*                        ELSE                                             
024653*                           MOVE MFS-NUM-FAELT-FEL TO                     
024654*                              MOD-KDPSLLOC-NEW-ATTR (INDX)               
024655*                           MOVE NEJ TO INDATA-SW                         
024656*                        END-IF                                           
024657                      ELSE                                                
024659                          MOVE MFS-NUM-FAELT-FEL TO                       
024660                               MOD-KDPSLLOC-NEW-ATTR (INDX)               
024661                      END-IF                                              
024662                   ELSE                                                   
024664                      MOVE MFS-NUM-FAELT-FEL TO                           
024665                               MOD-KDBEH-ATTR (INDX)                      
024666                      MOVE NEJ TO INDATA-SW                               
024667                   END-IF                                                 
024668                 END-IF                                                   
024669              END-IF                                                      
024670                                                                          
024671              ADD +1 TO INDX                                              
024672                                                                          
024673           END-PERFORM                                                    
024674        END-IF                                                            
024675                                                                          
024676        IF MID-UPD NOT = ALL '+'                                          
024678           IF MID-IDARTNR-UPD NOT = ALL '+'                               
024679              MOVE MID-IDARTNR-UPD  TO W-IDARTNR                          
024680              PERFORM IMS-GET-ARTC-WLARTC01                               
024681              IF SEGMENT-SAKNAS                                           
024683                 MOVE MFS-ALFA-FAELT-FEL TO                               
024684                                   MOD-IDARTNR-UPD-ATTR                   
024685                 MOVE NEJ TO INDATA-SW                                    
024686              ELSE                                                        
024688                 IF ARTC-ART-KDERS-UTG = ZERO                             
024689                    PERFORM IMS-GET-ARTC-WLARTC11                         
024690                    IF SEGMENT-FINNS                                      
024691                       MOVE MFS-NUM-FAELT-FEL   TO                        
024692                                   MOD-IDARTNR-UPD-ATTR                   
024693                    ELSE                                                  
024695                       MOVE MFS-NUM-FAELT-RAETT TO                        
024696                                   MOD-IDARTNR-UPD-ATTR                   
024697                       MOVE NEJ TO INDATA-SW                              
024698                    END-IF                                                
024699                 ELSE                                                     
024700                    MOVE MFS-NUM-FAELT-FEL TO                             
024701                                   MOD-IDARTNR-UPD-ATTR                   
024702                    MOVE NEJ TO INDATA-SW                                 
024703                 END-IF                                                   
024704              END-IF                                                      
024705           ELSE                                                           
024706             MOVE NEJ TO INDATA-SW                                        
024707             MOVE MFS-NUM-FAELT-FEL TO                                    
024708                                   MOD-IDARTNR-UPD-ATTR                   
024709           END-IF                                                         
024710                                                                          
024711           IF MID-KDPSLLOC-UPD NOT = ALL '+'                              
024712              MOVE MID-KDPSLLOC-UPD TO                                    
024713                               TEST-KDPSLLOC                              
024714*             IF WWLOCPS-PCODE-LAB                                        
024715                MOVE MFS-NUM-FAELT-RAETT TO                               
024716                               MOD-KDPSLLOC-UPD-ATTR                      
024717*             ELSE                                                        
024718*               MOVE MFS-NUM-FAELT-FEL TO                                 
024719*                              MOD-KDPSLLOC-UPD-ATTR                      
024720*               MOVE NEJ TO INDATA-SW                                     
024721*             END-IF                                                      
024722           ELSE                                                           
024723              MOVE MFS-NUM-FAELT-FEL TO                                   
024724                               MOD-KDPSLLOC-UPD-ATTR                      
024725              MOVE NEJ TO INDATA-SW                                       
024726           END-IF                                                         
024727        END-IF                                                            
024728     END-IF                                                               
024729                                                                          
024730     IF INDATA-FEL                                                        
024731        MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                         
024732        MOVE 'GB'             TO MED-IDSKYLT                              
024733        CALL WMEDKONV USING MED-WMEDAREA                                  
024734        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
024735        PERFORM MFS-ROER-EJ-FAELT-UT                                      
024736     END-IF                                                               
024737     .                                                                    
024738     EJECT                                                                
024739 H-UPPDATERA SECTION.                                                     
024740                                                                          
024741     IF MID-INPUT NOT = ALL '+'                                           
024743        MOVE +1 TO INDX                                                   
024744        IF W-IDARTNR-MIN > ZERO                                           
024745           MOVE W-IDARTNR-MIN    TO W-IDARTNR                             
024746           PERFORM IMS-GET-WL514111                                       
024747           PERFORM HA-UPPDATERA                                           
024749        ELSE                                                              
024750           PERFORM IMS-GET-WL514101                                       
024751           PERFORM UNTIL INDX > MAX-INDX                                  
024752              PERFORM IMS-GHNP-WL514111                                   
024753              PERFORM HA-UPPDATERA                                        
024766              ADD +1 TO INDX                                              
024767           END-PERFORM                                                    
024768        END-IF                                                            
024769     END-IF                                                               
024770                                                                          
024771     IF MID-UPD NOT = ALL '+'                                             
024772                                                                          
024773        MOVE MID-IDARTNR-UPD  TO 5141-5142-IDARTNR                        
024774                                          W-IDARTNR                       
024776        PERFORM IMS-GET-BENA                                              
024777                                                                          
024778        IF SEGMENT-FINNS                                                  
024779           MOVE BENA-TEXT-BEART TO 5141-5142-BEART                        
024780        ELSE                                                              
024781           MOVE SPACE           TO 5141-5142-BEART                        
024782        END-IF                                                            
024784                                                                          
024785        MOVE ARTC-ART-KDPRODSL  TO 5141-5142-KDPRODSL                     
024786        MOVE ARTC-CLAG-KDPSLLOC TO 5141-5142-KDPSLLOC                     
024787        MOVE MID-KDPSLLOC-UPD   TO 5141-5142-KDPSLLOC-NEW                 
024788                                                                          
024789                                                                          
024790        PERFORM IMS-ISRT-WL514111                                         
024791                                                                          
024792     END-IF                                                               
024793                                                                          
024794     IF INDATA-OK                                                         
024795        MOVE INF-UPDATE-DONE TO MED-IDMFSINF                              
024796        MOVE 'GB'             TO MED-IDSKYLT                              
024797        CALL WMEDKONV USING MED-WMEDAREA                                  
024798        MOVE MED-MFSINF TO MOD-TEMFSINF                                   
024799        PERFORM MFS-FORM-ATTR                                             
024800        PERFORM MFS-RENSA-FAELT-IN                                        
024801     END-IF                                                               
024802                                                                          
024810     .                                                                    
024900     EJECT                                                                
024901                                                                          
024910 HA-UPPDATERA SECTION.                                                    
024920                                                                          
025000     IF MID-KDBEH (INDX) = 'D'                                            
025100        PERFORM IMS-DLET-WL514111                                         
025200        MOVE ZERO TO W-IDARTNR-MIN                                        
025300     END-IF                                                               
025400     IF MID-KDBEH (INDX) = 'C'                                            
025500        IF MID-KDPSLLOC-IN (INDX) NOT = ALL '+'                           
025600           MOVE MID-KDPSLLOC-IN (INDX) TO                                 
025610                          5141-5142-KDPSLLOC-NEW                          
025620        END-IF                                                            
025630        PERFORM IMS-REPL-WL514111                                         
025640     END-IF                                                               
025650                                                                          
025660     .                                                                    
025670     EJECT                                                                
025680                                                                          
025700 MFS-RENSA-FAELT-IN SECTION.                                              
025800                                                                          
025810     MOVE +1 TO INDX                                                      
025820     PERFORM UNTIL INDX > MAX-INDX                                        
025830         MOVE MFS-RENSA-FAELT TO MOD-KDBEH        (INDX)                  
025840                                 MOD-IDARTNR      (INDX)                  
025850                                 MOD-BEART        (INDX)                  
025860                                 MOD-KDPRODSL     (INDX)                  
025861                                 MOD-KDPSLLOC-OLD (INDX)                  
025862                                 MOD-KDPSLLOC-NEW (INDX)                  
025870         ADD +1 TO INDX                                                   
025880     END-PERFORM                                                          
025890     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UPD                              
026000                             MOD-BEART-UPD                                
026100                             MOD-KDPSLLOC-UPD                             
026200     .                                                                    
026300     EJECT                                                                
026400 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
026500                                                                          
026600*    --- ALLA UTDATA-FÄLT                                                 
026810     MOVE +1 TO INDX                                                      
026820     PERFORM UNTIL INDX > MAX-INDX                                        
026830         MOVE MFS-ROER-EJ-FAELT TO MOD-KDBEH        (INDX)                
026840                                   MOD-IDARTNR      (INDX)                
026850                                   MOD-BEART        (INDX)                
026860                                   MOD-KDPRODSL     (INDX)                
026870                                   MOD-KDPSLLOC-OLD (INDX)                
026880                                   MOD-KDPSLLOC-NEW (INDX)                
026890         ADD +1 TO INDX                                                   
026891     END-PERFORM                                                          
026892     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-UPD                            
026894                               MOD-BEART-UPD                              
026895                               MOD-KDPSLLOC-UPD                           
026897     .                                                                    
026898     EJECT                                                                
028000 MFS-FORM-ATTR SECTION.                                                   
028100                                                                          
028200*    --- ALLA INDATA-FÄLT                                                 
028201     MOVE +1 TO INDX                                                      
028202                                                                          
028203     PERFORM UNTIL INDX > MAX-INDX                                        
028204         MOVE MFS-FORMATETS-ATTR TO MOD-KDBEH-ATTR (INDX)                 
028205                                 MOD-KDPSLLOC-NEW-ATTR (INDX)             
028210         ADD +1 TO INDX                                                   
028220     END-PERFORM                                                          
028230                                                                          
028300     MOVE MFS-FORMATETS-ATTR TO MOD-IDARTNR-UPD-ATTR                      
028400                                MOD-KDPSLLOC-UPD-ATTR                     
028500     .                                                                    
028600     EJECT                                                                
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
030510     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
030600        MOVE '0' TO MFS-KDHUVOMR                                          
030700     END-IF                                                               
030900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
031000     MOVE SPACE TO GODK-STATUSKODER                                       
031100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
031200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031300     PERFORM IMS-STATUSKONTROLL                                           
031400     .                                                                    
031501     EJECT                                                                
031502 IMS-GET-ARTC-WLARTC01 SECTION.                                           
031505                                                                          
031506     STRING 'WLARTC01(IDARTNR = ' W-IDARTNR-X ')'                         
031507          DELIMITED BY SIZE INTO SSA1                                     
031508     MOVE '  GE' TO GODK-STATUSKODER                                      
031509     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WLARTC01 SSA1                  
031510     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
031512     PERFORM IMS-STATUSKONTROLL                                           
031513     .                                                                    
031514     EJECT                                                                
031515 IMS-GET-ARTC-WLARTC11 SECTION.                                           
031517                                                                          
031518     STRING 'WLARTC11(KDSEGKEY =1)'                                       
031519          DELIMITED BY SIZE INTO SSA1                                     
031520     MOVE '  GE' TO GODK-STATUSKODER                                      
031521     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WLARTC11 SSA1                  
031522     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
031524     PERFORM IMS-STATUSKONTROLL                                           
031525     .                                                                    
031526     EJECT                                                                
031610 IMS-GET-BENA SECTION.                                                    
031620     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
031630          DELIMITED BY SIZE INTO SSA1                                     
031640     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
031650          DELIMITED BY SIZE INTO SSA2                                     
031660     MOVE '  GE' TO GODK-STATUSKODER                                      
031670     CALL CBLTDLI USING GU BENA-PCB DLI-IO-WLBENA11 SSA1 SSA2             
031680     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
031690     PERFORM IMS-STATUSKONTROLL                                           
031691     .                                                                    
031692     SKIP2                                                                
031693 IMS-GET-WL514101 SECTION.                                                
031696                                                                          
031697     STRING 'WL514101(WDGXKEY = ' W-WDGXKEY-X ')'                         
031698          DELIMITED BY SIZE INTO SSA1                                     
031699     MOVE '  GE' TO GODK-STATUSKODER                                      
031700     CALL CBLTDLI USING GU 5141-PCB DLI-IO-WL514101 SSA1                  
031701     MOVE 5141-STATUS-CODE TO STATUS-WS                                   
031702     PERFORM IMS-STATUSKONTROLL                                           
031703     .                                                                    
031704     EJECT                                                                
031715 IMS-GET-WL514111 SECTION.                                                
031716                                                                          
031717     STRING 'WL514101(WDGXKEY = ' W-WDGXKEY-X ')'                         
031718          DELIMITED BY SIZE INTO SSA1                                     
031719     STRING 'WL514111(IDARTNR  =' W-IDARTNR-X ')'                         
031720          DELIMITED BY SIZE INTO SSA2                                     
031721     MOVE '  GE' TO GODK-STATUSKODER                                      
031722     CALL CBLTDLI USING GHU 5141-PCB DLI-IO-WL514111 SSA1 SSA2            
031723     MOVE 5141-STATUS-CODE TO STATUS-WS                                   
031724     PERFORM IMS-STATUSKONTROLL                                           
031725     .                                                                    
031726     EJECT                                                                
031727 IMS-GHNP-WL514111 SECTION.                                               
031728                                                                          
031729     STRING 'WL514111(IDARTNR =>' W-IDARTNR-MIN-X                         
031730                   '&IDARTNR =<' W-IDARTNR-MAX-X ')'                      
031731          DELIMITED BY SIZE INTO SSA1                                     
031732     MOVE '  GE' TO GODK-STATUSKODER                                      
031733     CALL CBLTDLI USING GHNP 5141-PCB DLI-IO-WL514111 SSA1                
031734     MOVE 5141-STATUS-CODE TO STATUS-WS                                   
031735     PERFORM IMS-STATUSKONTROLL                                           
031736     .                                                                    
031737     EJECT                                                                
031738 IMS-ISRT-WL514111 SECTION.                                               
031739     STRING 'WL514101(WDGXKEY  =' W-WDGXKEY-X ')'                         
031740          DELIMITED BY SIZE INTO SSA1                                     
031741     MOVE 'WL514111 ' TO SSA2                                             
031742     MOVE '  ' TO GODK-STATUSKODER                                        
031743     CALL CBLTDLI USING ISRT 5141-PCB DLI-IO-WL514111 SSA1 SSA2           
031744     MOVE 5141-STATUS-CODE TO STATUS-WS                                   
031745     PERFORM IMS-STATUSKONTROLL                                           
031748     .                                                                    
031749     EJECT                                                                
031750 IMS-DLET-WL514111 SECTION.                                               
031752                                                                          
031753     MOVE '  ' TO GODK-STATUSKODER                                        
031754     CALL CBLTDLI USING DLET 5141-PCB DLI-IO-WL514111                     
031755     MOVE 5141-STATUS-CODE TO STATUS-WS                                   
031756     PERFORM IMS-STATUSKONTROLL                                           
031757     .                                                                    
031758     EJECT                                                                
031759 IMS-REPL-WL514111 SECTION.                                               
031761                                                                          
031762     MOVE '  ' TO GODK-STATUSKODER                                        
031763     CALL CBLTDLI USING REPL 5141-PCB DLI-IO-WL514111                     
031764     MOVE 5141-STATUS-CODE TO STATUS-WS                                   
031765     PERFORM IMS-STATUSKONTROLL                                           
031766     .                                                                    
031767     EJECT                                                                
031770 IMS-STATUSKONTROLL SECTION.                                              
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
