000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W9043200.                                                
000300 AUTHOR.         PER-ANDERS HELGEGREN.                                    
000400 DATE-WRITTEN.   99/08/24.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        REGISTRERING AV FÖRPACKNINGS EMBALLAGE                           
000900*                                                                         
001000*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSAKTION: W90432T                                             
001400*        MID:         W90432I1                                            
001500*                                                                         
001600*    UTDATA.                                                              
001700*        MOD:         W90432O1                                            
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     EJECT                                                                
002200 DATA DIVISION.                                                           
002300 WORKING-STORAGE SECTION.                                                 
002400                                                                          
002500*    -- CHECKED BY WY2000                                                 
002600 77  IDPGM                       PIC X(08)   VALUE 'W9043200'.            
002700                                                                          
002800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
002900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003000                                                                          
003100 77  JA                          PIC X       VALUE 'J'.                   
003200 77  NEJ                         PIC X       VALUE 'N'.                   
003300                                                                          
003400*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
003500                                                                          
003600 77  INDATA-SW                   PIC X       VALUE 'J'.                   
003700     88  INDATA-OK                           VALUE 'J'.                   
003800     88  INDATA-FEL                          VALUE 'N'.                   
003900                                                                          
004000                                                                          
004100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004200     88  NYCKLAR-OK                          VALUE 'J'.                   
004300     88  NYCKLAR-FEL                         VALUE 'N'.                   
004400                                                                          
004500 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
004600     88  EGEN-MID                            VALUE '9432'.                
004700     88  GODK-MID                            VALUE '6181' '9432'          
004800                                                   '6183' '6184'          
004900                                                   '6185' '6186'          
005000                                                   '6187' '6188'          
005100                                                   '6189'.                
005200     88  HELP-MID                            VALUE '0551'.                
005300     EJECT                                                                
005400 01 WS-NOLL                  PIC S9(9) VALUE ZERO COMP-3.                 
005500 01 LOG-T1                   PIC X(16) VALUE 'TEST1'.                     
005600 01 LOG-T2                   PIC X(16) VALUE 'TEST2'.                     
005700 01 LOG-T3                   PIC X(16) VALUE 'TEST3'.                     
005800 01 LOG-T4                   PIC X(16) VALUE 'TEST4'.                     
005900 01 LOG-T5                   PIC X(16) VALUE 'TEST5'.                     
006000 01 LOG-T6                   PIC X(16) VALUE 'TEST6'.                     
006100 01 LOG-SSA1                 PIC X(64) VALUE 'SSA1'.                      
006200 01 LOG-STATUS               PIC X(2)  VALUE '**'.                        
006300     EJECT                                                                
006400 01  NYTT-13-SEGMENT-TABLE.                                               
006500     03  NYTT-13-SEGMENT OCCURS 14  PIC X.                                
006600                                                                          
006700 01  WQ-TAB.                                                              
006800     03  FILLER OCCURS 3.                                                 
006900         05  WQ-IDARTNR          PIC 9(9).                                
007000         05  WQ-KDEMBKOD         PIC 9(3).                                
007100     03  FILLER OCCURS 4.                                                 
007200         05  WQ-KVQPACK          PIC 9(5).                                
007300                                                                          
007400 01  WX-TAB.                                                              
007500     03  FILLER OCCURS 10.                                                
007600         05  WX-IDARTNR          PIC 9(9).                                
007700         05  WX-KVQPACK          PIC 9(5).                                
007800                                                                          
007900 01  W-IDARTNR-SPAR              PIC S9(9) COMP-3 VALUE ZERO.             
008000 01  KW-IDARTNR-SPAR             PIC S9(9) COMP-3 VALUE ZERO.             
008100 01  IX-X                        PIC S9(3) COMP-3 VALUE ZERO.             
008200 01  ARBETSAREOR.                                                         
008300     03  DAGENS-DATUM            PIC 9(6)  VALUE ZERO.                    
008400     03  WS-TIUPPDAT-EMB         PIC 9(6)  VALUE ZERO.                    
008500     03  WS-BEFT                 PIC 9(2)  VALUE ZERO.                    
008600     03  WS-KDEMBKEY             PIC X(3)  VALUE SPACE.                   
008700     03  FILLER  REDEFINES WS-KDEMBKEY.                                   
008800         05  FILLER              PIC X(1).                                
008900         05  WS-KDEMBKEY-IX      PIC 9(2).                                
009000     EJECT                                                                
009100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
009200 01  GENERELLA-SUBPROGRAM.                                                
009300     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
009400     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009700     EJECT                                                                
009800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
009900*01 -COPY WMEDAREA                                                        
010000     SKIP3                                                                
010100 01  MESSAGE-CODES.                                                       
010200     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
010300     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
010400     03  INF-PRESS-PF23          PIC X(3)    VALUE '206'.                 
010500     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
010600     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
010700     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
010800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010900     EJECT                                                                
011000 01  FELTEXTER.                                                           
011100     03  FELTEXT1 PIC X(36)                                               
011200                 VALUE 'INGEN BEHÖRIGHET FÖR PF23 UPDATERING'.            
011300*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
011400*                                                                         
011500 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
011600     SKIP3                                                                
011700*01 -COPY WMSGINIT                                                        
011800     EJECT                                                                
011900*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
012000*                                                                         
012100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
012200     SKIP3                                                                
012300*01  MID -COPY W90432I1                                                   
012400     EJECT                                                                
012500 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
012600     SKIP3                                                                
012700*01  -COPY WMSGAREA                                                       
012800     EJECT                                                                
012900     03  MOD REDEFINES MSG-AREA.                                          
013000*      05  -COPY W90432O1                                                 
013100     EJECT                                                                
013200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
013300     SKIP3                                                                
013400*01  -COPY WMFSAREA                                                       
013500     EJECT                                                                
013600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013700*                                                                         
013800     EJECT                                                                
013900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014000     SKIP3                                                                
014100 01  NYCKLAR-TILL-DLI.                                                    
014200     03  W-IDARTNR-X.                                                     
014300         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
014400     03  KW-IDARTNR-X.                                                    
014500         05  KW-IDARTNR          PIC S9(9)   VALUE ZERO COMP-3.           
014600     03  W-KDSEGKEY-X.                                                    
014700         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
014800     03  W-IDLAND-X.                                                      
015010         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
015100     03  W-KDEMBAL-X.                                                     
015200         05  W-KDEMBAL           PIC X(3)    VALUE SPACE.                 
015300     03  KW-KDEMBAL-X.                                                    
015400         05  KW-KDEMBAL          PIC X(3)    VALUE SPACE.                 
015500     SKIP2                                                                
015600*    --- STATUS-KOD FRÅN IMS                                              
015700 01  STATUS-WS                   PIC XX.                                  
015800     88  SEGMENT-FINNS                       VALUE '  '.                  
015900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
016000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
016100     SKIP2                                                                
016200 01  GODK-STATUSKODER.                                                    
016300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016400     SKIP3                                                                
016500 01  SSA1                        PIC X(64).                               
016600 01  SSA2                        PIC X(64).                               
016700     EJECT                                                                
016800*    --- IMS FUNKTIONSKODER                                               
016900*01  -COPY W0003                                                          
017000     EJECT                                                                
017100*    ---  DLI INPUT-OUTPUT AREA                                           
017200                                                                          
017300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
017400 01  DLI-IO-WDK601.                                                       
017500*    03  -COPY WDK601                                                     
017600     EJECT                                                                
017700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
017800 01  DLI-IO-WDK611.                                                       
017900*    03  -COPY WDK611                                                     
018000     EJECT                                                                
018500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK613'.                      
018600 01  DLI-IO-WDK613.                                                       
018700*    03  -COPY WDK613                                                     
018800     EJECT                                                                
018900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT301'.                      
018910 01  DLI-IO-WDT301.                                                       
018920*    03  -COPY WDT301                                                     
018930     EJECT                                                                
018940 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT311'.                      
018950 01  DLI-IO-WDT311.                                                       
018960*    03  -COPY WDT311                                                     
018970     EJECT                                                                
019000*    ---  DLI INPUT-OUTPUT AREA   FÖR KOPIERING                           
019100                                                                          
019200 01  FILLER         PIC X(16) VALUE 'DLI-IO-KWDK601'.                     
019300 01  DLI-IO-KWDK601.                                                      
019400*    03  -COPY WDK601 -PRE K                                              
019500     EJECT                                                                
019600 01  FILLER         PIC X(16) VALUE 'DLI-IO-KWDK611'.                     
019700 01  DLI-IO-KWDK611.                                                      
019800*    03  -COPY WDK611  -PRE K                                             
019900     EJECT                                                                
020000 01  FILLER         PIC X(16) VALUE 'DLI-IO-KWDK613'.                     
020100 01  DLI-IO-KWDK613.                                                      
020200*    03  -COPY WDK613  -PRE K                                             
020300     EJECT                                                                
020400 LINKAGE SECTION.                                                         
020500*01  -COPY W0009   -PRE MSG-                                              
020600     EJECT                                                                
020700*01  -COPY W0008   -PRE USEA-                                             
020800     05  FILLER                  PIC X.                                   
020900                                                                          
021000     EJECT                                                                
021100*01  -COPY W0008  -PRE WDK6-                                              
021200     05  FILLER                  PIC X.                                   
021300     EJECT                                                                
021400*01  -COPY W0008  -PRE KWDK6-                                             
021500     05  FILLER                  PIC X.                                   
021600     EJECT                                                                
021610*01  -COPY W0008  -PRE WDT3-                                              
021620     05  FILLER                  PIC X.                                   
021630     EJECT                                                                
021700 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB WDK6-PCB KWDK6-PCB            
021710                                   WDT3-PCB.                              
021800 MAIN SECTION.                                                            
021900     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDK6-PCB KWDK6-PCB            
021910                                   WDT3-PCB.                              
022000                                                                          
022100     PERFORM IMS-GET-MSG                                                  
022200     IF SEGMENT-FINNS                                                     
022300       PERFORM A-INIT                                                     
022400       PERFORM B-KOLLA-NYCKLAR                                            
022500       IF NYCKLAR-OK                                                      
022600         IF MFS-UPDATE OR MFS-UPD-V                                       
022700           PERFORM G-KOLLA-INPUT                                          
022800           IF INDATA-OK                                                   
022900             PERFORM H-UPPDATERA                                          
023000           END-IF                                                         
023100         ELSE                                                             
023200           IF MFS-FIRST                                                   
023300*            PERFORM C-FOERSTA-SIDA                                       
023400             CONTINUE                                                     
023500           ELSE                                                           
023600             PERFORM E-SAMMA-SIDA                                         
023700           END-IF                                                         
023800         END-IF                                                           
023900         PERFORM F-LAES-VISA-INFO                                         
024000       END-IF                                                             
024100       COMPUTE MSG-KVLL = LENGTH OF MOD-W90432O1 + 4                      
024200       PERFORM IMS-INSERT-MSG                                             
024300     END-IF                                                               
024400* ABEND PÅ BEGÄRAN                                                        
024500*    DIVIDE W-IDARTNR  BY WS-NOLL GIVING W-IDARTNR                        
024600* ABEND PÅ BEGÄRAN                                                        
024700                                                                          
024800     MOVE ZERO TO RETURN-CODE                                             
024900     GOBACK                                                               
025000     .                                                                    
025100     EJECT                                                                
025200 A-INIT SECTION.                                                          
025300                                                                          
025400     IF MSG-DUBBLA-TRANSKODER                                             
025500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W90432I1                 
025600       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
025700       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
025800     ELSE                                                                 
025900       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W90432I1                  
026000       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
026100       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
026200     END-IF                                                               
026300                                                                          
026400     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
026500     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
026600     MOVE MFS-IDTRANS TO W-IDTRANS                                        
026700                                                                          
026800     MOVE LOW-VALUE TO MSG-AREA                                           
026900     MOVE 'W90432O1' TO MFS-IDMOD                                         
027000     MOVE '9432' TO MOD-IDTRANS                                           
027100     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
027200                                                                          
027300*    IF MSGI-IDLAND-SPR = 'SE'                                            
027400*       MOVE '0' TO MFS-KDHUVOMR                                          
027500*    END-IF                                                               
027600                                                                          
027700     IF EGEN-MID OR HELP-MID                                              
027800       CONTINUE                                                           
027900     ELSE                                                                 
028000       MOVE SPACE TO MFS-KDTRTYP                                          
028100       MOVE '7' TO MFS-IDPFK                                              
028200     END-IF                                                               
028300                                                                          
028400     ACCEPT DAGENS-DATUM FROM DATE                                        
028500     .                                                                    
028600     EJECT                                                                
028700 B-KOLLA-NYCKLAR SECTION.                                                 
028800                                                                          
028900     MOVE ALL '+'           TO MSGI-WMSGINIT                              
029000     MOVE '001'             TO MSGI-KDCALL                                
029100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
029200                               MSGI-IDLTERM-USER                          
029300     MOVE '9432'            TO MSGI-IDTRANS                               
029400     IF GODK-MID                                                          
029500         MOVE MID-IDARTNR-IN     TO MSGI-IDARTNR                          
029600     END-IF                                                               
029700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
029800                                                                          
029900     MOVE JA TO NYCKLAR-SW                                                
030000                                                                          
030100                                                                          
030200*    -- KONTROLL AV IDARTNR                                               
030300*    MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
030400                                                                          
030500     IF MID-IDARTNR-IN NOT = ALL '+'                                      
030600       MOVE '7'         TO MFS-IDPFK                                      
030700       MOVE SPACE       TO MFS-KDTRTYP                                    
030800     END-IF                                                               
030900     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
031000     IF MSGI-IDARTNR NUMERIC                                              
031100       MOVE MSGI-IDARTNR TO W-IDARTNR W-IDARTNR-SPAR                      
031200     ELSE                                                                 
031300       MOVE NEJ TO NYCKLAR-SW                                             
031400     END-IF                                                               
031500                                                                          
031600*    IF GODK-MID OR NYCKLAR-OK                                            
031700*      MOVE MSGI-IDARTNR        TO MOD-IDARTNR-UT                         
031800*      INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
031900*    ELSE                                                                 
032000*      MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
032100*    END-IF                                                               
032200                                                                          
032300     IF NYCKLAR-FEL                                                       
032400       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
032500       CALL WMEDKONV USING MED-WMEDAREA                                   
032600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
032700*      PERFORM MFS-RENSA-FAELT-IN                                         
032800       PERFORM MFS-RENSA-FAELT-UT                                         
032900     END-IF                                                               
033000     .                                                                    
033100     EJECT                                                                
033200*C-FOERSTA-SIDA SECTION.                                                  
033300*                                                                         
033400*    PERFORM MFS-RENSA-FAELT-IN                                           
033500*    .                                                                    
033600     EJECT                                                                
033700 E-SAMMA-SIDA SECTION.                                                    
033800                                                                          
033900     IF EGEN-MID OR HELP-MID                                              
034000*      IF MID-IDARTNR-KOPI = ALL '+' AND                                  
034100       IF MID-EMB-Q        = ALL '+' AND                                  
034200          MID-EMB-X (1)    = ALL '+' AND                                  
034300          MID-EMB-X (2)    = ALL '+' AND                                  
034400          MID-EMB-X (3)    = ALL '+' AND                                  
034500          MID-EMB-X (4)    = ALL '+' AND                                  
034600          MID-EMB-X (5)    = ALL '+' AND                                  
034700          MID-EMB-X (6)    = ALL '+' AND                                  
034800          MID-EMB-X (7)    = ALL '+' AND                                  
034900          MID-EMB-X (8)    = ALL '+' AND                                  
035000          MID-EMB-X (9)    = ALL '+' AND                                  
035100          MID-EMB-X (10)   = ALL '+'                                      
035200*        PERFORM MFS-RENSA-FAELT-IN                                       
035300         CONTINUE                                                         
035400       ELSE                                                               
035500         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
035600         CALL WMEDKONV USING MED-WMEDAREA                                 
035700         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
035800         PERFORM EA-MID-INDATA-TILL-MOD                                   
035900       END-IF                                                             
036000     ELSE                                                                 
036100*      PERFORM MFS-RENSA-FAELT-IN                                         
036200       CONTINUE                                                           
036300     END-IF                                                               
036400     .                                                                    
036500     EJECT                                                                
036600 EA-MID-INDATA-TILL-MOD SECTION.                                          
036700                                                                          
036800*    IF MID-IDARTNR-KOPI            NOT = ALL '+'                         
036900*       MOVE MID-IDARTNR-KOPI       TO MOD-IDARTNR-KOPI                   
037000*       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-IDARTNR-KOPI-ATTR              
037100*    ELSE                                                                 
037200*       MOVE MFS-RENSA-FAELT        TO MOD-IDARTNR-KOPI                   
037300*    END-IF                                                               
037400                                                                          
037500*    IF MID-IDARTNR-EMBQ0-IN        NOT = ALL '+'                         
037600*       MOVE MID-IDARTNR-EMBQ0-IN   TO MOD-IDARTNR-EMBQ0-IN               
037700*       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-IDARTNR-EMBQ0-IN-ATTR          
037800*    ELSE                                                                 
037900*       MOVE MFS-RENSA-FAELT        TO MOD-IDARTNR-EMBQ0-IN               
038000*    END-IF                                                               
038100                                                                          
038200*    IF MID-KVQPACK-EMBQ0-IN        NOT = ALL '+'                         
038300*       MOVE MID-KVQPACK-EMBQ0-IN   TO MOD-KVQPACK-EMBQ0-IN               
038400*       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KVQPACK-EMBQ0-IN-ATTR          
038500*    ELSE                                                                 
038600*       MOVE MFS-RENSA-FAELT        TO MOD-KVQPACK-EMBQ0-IN               
038700*    END-IF                                                               
038800                                                                          
038900*    IF MID-KDEMBKOD-EMBQ0-IN       NOT = ALL '+'                         
039000*       MOVE MID-KDEMBKOD-EMBQ0-IN  TO MOD-KDEMBKOD-EMBQ0-IN              
039100*       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KDEMBKOD-EMBQ0-IN-ATTR         
039200*    ELSE                                                                 
039300*       MOVE MFS-RENSA-FAELT        TO MOD-KDEMBKOD-EMBQ0-IN              
039400*    END-IF                                                               
039500                                                                          
039600*    IF MID-IDARTNR-EMBQ1-IN        NOT = ALL '+'                         
039700*       MOVE MID-IDARTNR-EMBQ1-IN   TO MOD-IDARTNR-EMBQ1-IN               
039800*       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-IDARTNR-EMBQ1-IN-ATTR          
039900*    ELSE                                                                 
040000*       MOVE MFS-RENSA-FAELT        TO MOD-IDARTNR-EMBQ1-IN               
040100*    END-IF                                                               
040200                                                                          
040300*    IF MID-KVQPACK-EMBQ1-IN        NOT = ALL '+'                         
040400*       MOVE MID-KVQPACK-EMBQ1-IN   TO MOD-KVQPACK-EMBQ1-IN               
040500*       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KVQPACK-EMBQ1-IN-ATTR          
040600*    ELSE                                                                 
040700*       MOVE MFS-RENSA-FAELT        TO MOD-KVQPACK-EMBQ1-IN               
040800*    END-IF                                                               
040900                                                                          
041000*    IF MID-KDEMBKOD-EMBQ1-IN       NOT = ALL '+'                         
041100*       MOVE MID-KDEMBKOD-EMBQ1-IN  TO MOD-KDEMBKOD-EMBQ1-IN              
041200*       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KDEMBKOD-EMBQ1-IN-ATTR         
041300*    ELSE                                                                 
041400*       MOVE MFS-RENSA-FAELT        TO MOD-KDEMBKOD-EMBQ1-IN              
041500*    END-IF                                                               
041600                                                                          
041700*    IF MID-IDARTNR-EMBQ2-IN        NOT = ALL '+'                         
041800*       MOVE MID-IDARTNR-EMBQ2-IN   TO MOD-IDARTNR-EMBQ2-IN               
041900*       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-IDARTNR-EMBQ2-IN-ATTR          
042000*    ELSE                                                                 
042100*       MOVE MFS-RENSA-FAELT        TO MOD-IDARTNR-EMBQ2-IN               
042200*    END-IF                                                               
042300                                                                          
042400*    IF MID-KVQPACK-EMBQ2-IN        NOT = ALL '+'                         
042500*       MOVE MID-KVQPACK-EMBQ2-IN   TO MOD-KVQPACK-EMBQ2-IN               
042600*       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KVQPACK-EMBQ2-IN-ATTR          
042700*    ELSE                                                                 
042800*       MOVE MFS-RENSA-FAELT        TO MOD-KVQPACK-EMBQ2-IN               
042900*    END-IF                                                               
043000                                                                          
043100     IF MID-KDEMBKOD-EMBQ2-IN       NOT = ALL '+'                         
043200*       MOVE MID-KDEMBKOD-EMBQ2-IN  TO MOD-KDEMBKOD-EMBQ2-IN              
043300        MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KDEMBKOD-EMBQ2-IN-ATTR         
043400     ELSE                                                                 
043500*       MOVE MFS-RENSA-FAELT        TO MOD-KDEMBKOD-EMBQ2-IN              
043600        CONTINUE                                                          
043700     END-IF                                                               
043800                                                                          
043900*    IF MID-KVQPACK-EMBQ3-IN        NOT = ALL '+'                         
044000*       MOVE MID-KVQPACK-EMBQ3-IN   TO MOD-KVQPACK-EMBQ3-IN               
044100*       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KVQPACK-EMBQ3-IN-ATTR          
044200*    ELSE                                                                 
044300*       MOVE MFS-RENSA-FAELT        TO MOD-KVQPACK-EMBQ3-IN               
044400*    END-IF                                                               
044500     EJECT                                                                
044600     MOVE +1 TO IX-X                                                      
044700     PERFORM UNTIL IX-X > 10                                              
044800*       IF MID-IDARTNR-EMBX-IN (IX-X)  NOT = ALL '+'                      
044900*          MOVE MID-IDARTNR-EMBX-IN (IX-X)                                
045000*                              TO MOD-IDARTNR-EMBX-IN (IX-X)              
045100*          MOVE MFS-ADD-LAES-IN-FAELT                                     
045200*                              TO MOD-IDARTNR-EMBX-IN-ATTR (IX-X)         
045300*       ELSE                                                              
045400*          MOVE MFS-RENSA-FAELT                                           
045500*                              TO MOD-IDARTNR-EMBX-IN (IX-X)              
045600*       END-IF                                                            
045700                                                                          
045800*       IF MID-KVQPACK-EMBX-IN (IX-X)  NOT = ALL '+'                      
045900*          MOVE MID-KVQPACK-EMBX-IN (IX-X)                                
046000*                              TO MOD-KVQPACK-EMBX-IN (IX-X)              
046100*          MOVE MFS-ADD-LAES-IN-FAELT                                     
046200*                              TO MOD-KVQPACK-EMBX-IN-ATTR (IX-X)         
046300*       ELSE                                                              
046400*          MOVE MFS-RENSA-FAELT                                           
046500*                              TO MOD-KVQPACK-EMBX-IN (IX-X)              
046600*       END-IF                                                            
046700                                                                          
046800        ADD +1 TO IX-X                                                    
046900     END-PERFORM                                                          
047000     .                                                                    
047100     EJECT                                                                
047200 F-LAES-VISA-INFO SECTION.                                                
047300                                                                          
047400     PERFORM FA-LAES-GRUNDDATA                                            
047500                                                                          
047600     IF SEGMENT-SAKNAS                                                    
047700        MOVE ERR-PART-MISSING    TO MED-IDMFSFEL                          
047800        CALL WMEDKONV USING MED-WMEDAREA                                  
047900        MOVE MED-MFSFEL          TO MOD-TEMFSFEL                          
048000        PERFORM MFS-RENSA-FAELT-UT                                        
048100     ELSE                                                                 
048200        PERFORM MFS-RENSA-FAELT-UT                                        
048210        MOVE 'SE'                TO W-IDLAND                              
048300        PERFORM  IMS-GU-WDT311                                            
048400        IF SEGMENT-FINNS                                                  
048500           MOVE FPCK-BEFT        TO WS-BEFT                               
048600*          MOVE WS-BEFT          TO MOD-BEFT-UT                           
048700*          MOVE FPCK-KDFORP      TO MOD-KDFORP-UT                         
048800        END-IF                                                            
048900*       MOVE SPACE               TO MOD-IDARTNR-KOPI                      
049000*       MOVE SPACE               TO MOD-FLFPINST-UPP                      
049100        MOVE CLAG-TIUPPDAT-EMB   TO WS-TIUPPDAT-EMB                       
049200*       MOVE WS-TIUPPDAT-EMB     TO MOD-TIUPPDAT-UT                       
049300*       MOVE CLAG-IDUSER-EMB     TO MOD-IDUSER-UT                         
049400                                                                          
049500        PERFORM FB-LAES-EMBALLAGE                                         
049600     END-IF                                                               
049700     .                                                                    
049800     EJECT                                                                
049900 FA-LAES-GRUNDDATA SECTION.                                               
050000                                                                          
050100     MOVE W-IDARTNR-SPAR TO W-IDARTNR                                     
050200                                                                          
050300     PERFORM IMS-GU-WDK6-ROT                                              
050400     IF SEGMENT-FINNS                                                     
050500        PERFORM  IMS-GET-WDK6-CLAG                                        
050600     END-IF                                                               
050700     .                                                                    
050800     EJECT                                                                
050900 FB-LAES-EMBALLAGE SECTION.                                               
051000                                                                          
051100     PERFORM IMS-GET-WDK6-EMB                                             
051200*                                                                         
051300     PERFORM UNTIL SEGMENT-SAKNAS                                         
051400*      IF EMB-KDEMBKEY (1:1) = 'Q'                                        
051500*       IF EMB-KDEMBKEY = 'Q0'                                            
051600*          MOVE EMB-IDARTNR-EMB    TO MOD-IDARTNR-EMBQ0-UT                
051700*          MOVE EMB-KVQPACK-EMB    TO MOD-KVQPACK-EMBQ0-UT                
051800*          MOVE EMB-KDEMBKOD       TO MOD-KDEMBKOD-EMBQ0-UT               
051900*          INSPECT MOD-IDARTNR-EMBQ0-UT                                   
052000*                                  REPLACING LEADING ZERO BY SPACE        
052100*          INSPECT MOD-KVQPACK-EMBQ0-UT                                   
052200*                                  REPLACING LEADING ZERO BY SPACE        
052300*          INSPECT MOD-KDEMBKOD-EMBQ0-UT                                  
052400*                                  REPLACING LEADING ZERO BY SPACE        
052500*       END-IF                                                            
052600*       IF EMB-KDEMBKEY = 'Q1'                                            
052700*          MOVE EMB-IDARTNR-EMB    TO MOD-IDARTNR-EMBQ1-UT                
052800*          MOVE EMB-KVQPACK-EMB    TO MOD-KVQPACK-EMBQ1-UT                
052900*          MOVE EMB-KDEMBKOD       TO MOD-KDEMBKOD-EMBQ1-UT               
053000*          INSPECT MOD-IDARTNR-EMBQ1-UT                                   
053100*                                  REPLACING LEADING ZERO BY SPACE        
053200*          INSPECT MOD-KVQPACK-EMBQ1-UT                                   
053300*                                  REPLACING LEADING ZERO BY SPACE        
053400*          INSPECT MOD-KDEMBKOD-EMBQ1-UT                                  
053500*                                  REPLACING LEADING ZERO BY SPACE        
053600*       END-IF                                                            
053700        IF EMB-KDEMBKEY = 'Q2'                                            
053800*          MOVE EMB-IDARTNR-EMB    TO MOD-IDARTNR-EMBQ2-UT                
053900*          MOVE EMB-KVQPACK-EMB    TO MOD-KVQPACK-EMBQ2-UT                
054000           MOVE EMB-KDEMBKOD       TO MOD-KDEMBKOD-EMBQ2-UT               
054100*          INSPECT MOD-IDARTNR-EMBQ2-UT                                   
054200*                                  REPLACING LEADING ZERO BY SPACE        
054300*          INSPECT MOD-KVQPACK-EMBQ2-UT                                   
054400*                                  REPLACING LEADING ZERO BY SPACE        
054500           INSPECT MOD-KDEMBKOD-EMBQ2-UT                                  
054600                                   REPLACING LEADING ZERO BY SPACE        
054700        END-IF                                                            
054800*       IF EMB-KDEMBKEY = 'Q3'                                            
054900*          MOVE EMB-KVQPACK-EMB    TO MOD-KVQPACK-EMBQ3-UT                
055000*          INSPECT MOD-KVQPACK-EMBQ3-UT                                   
055100*                                  REPLACING LEADING ZERO BY SPACE        
055200*       END-IF                                                            
055300*      END-IF                                                             
055400                                                                          
055500*      IF EMB-KDEMBKEY (1:1) = 'X'                                        
055600*         MOVE EMB-KDEMBKEY        TO WS-KDEMBKEY                         
055700*         MOVE WS-KDEMBKEY-IX      TO IX-X                                
055800*         MOVE EMB-IDARTNR-EMB     TO MOD-IDARTNR-EMBX-UT (IX-X)          
055900*         MOVE EMB-KVQPACK-EMB     TO MOD-KVQPACK-EMBX-UT (IX-X)          
056000*         INSPECT MOD-IDARTNR-EMBX-UT (IX-X)                              
056100*                                  REPLACING LEADING ZERO BY SPACE        
056200*         INSPECT MOD-KVQPACK-EMBX-UT (IX-X)                              
056300*                                  REPLACING LEADING ZERO BY SPACE        
056400*      END-IF                                                             
056500*                                                                         
056600       PERFORM IMS-GET-WDK6-EMB                                           
056700     END-PERFORM                                                          
056800     .                                                                    
056900     EJECT                                                                
057000 G-KOLLA-INPUT SECTION.                                                   
057100                                                                          
057200     MOVE JA  TO INDATA-SW                                                
057300*    IF (MID-KVQPACK-EMBQ1-IN NOT = ALL '+'                               
057400*       AND MFS-UPDATE)                                                   
057500*       OR (MID-KVQPACK-EMBQ3-IN NOT = ALL '+'                            
057600*       AND MFS-UPDATE)                                                   
057700*       MOVE FELTEXT1              TO MOD-TEMFSFEL                        
057800*       PERFORM MFS-ROER-EJ-FAELT-UT                                      
057900*       PERFORM MFS-ROER-EJ-FAELT-IN                                      
058000*       PERFORM MFS-LAES-IN-IGEN                                          
058100*       MOVE NEJ                   TO INDATA-SW                           
058200*    ELSE                                                                 
058300*      IF MID-IDARTNR-KOPI = ALL '+' AND                                  
058400       IF MID-EMB-Q      = ALL '+' AND                                    
058500          MID-EMB-X (1)  = ALL '+' AND                                    
058600          MID-EMB-X (2)  = ALL '+' AND                                    
058700          MID-EMB-X (3)  = ALL '+' AND                                    
058800          MID-EMB-X (4)  = ALL '+' AND                                    
058900          MID-EMB-X (5)  = ALL '+' AND                                    
059000          MID-EMB-X (6)  = ALL '+' AND                                    
059100          MID-EMB-X (7)  = ALL '+' AND                                    
059200          MID-EMB-X (8)  = ALL '+' AND                                    
059300          MID-EMB-X (9)  = ALL '+' AND                                    
059400          MID-EMB-X (10) = ALL '+'                                        
059500         MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                        
059600         CALL WMEDKONV USING MED-WMEDAREA                                 
059700         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
059800*        PERFORM MFS-ROER-EJ-FAELT-IN                                     
059900         PERFORM MFS-ROER-EJ-FAELT-UT                                     
060000         MOVE NEJ TO INDATA-SW                                            
060100       ELSE                                                               
060200         MOVE W-IDARTNR-SPAR TO W-IDARTNR                                 
060300         PERFORM IMS-GU-WDK6-ROT                                          
060400         IF SEGMENT-FINNS                                                 
060500*           PERFORM GA-KOLLA-KOPIERING                                    
060600*           PERFORM GB-KOLLA-Q0-EMBALLAGE                                 
060700*           PERFORM GC-KOLLA-Q1-EMBALLAGE                                 
060800            PERFORM GD-KOLLA-Q2-EMBALLAGE                                 
060900*           PERFORM GE-KOLLA-Q3-EMBALLAGE                                 
061000*           PERFORM GF-KOLLA-X-EMBALLAGE                                  
061100                                                                          
061200            IF INDATA-FEL                                                 
061300              MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                   
061400              CALL WMEDKONV USING MED-WMEDAREA                            
061500              MOVE MED-MFSFEL TO MOD-TEMFSFEL                             
061600              PERFORM MFS-ROER-EJ-FAELT-UT                                
061700*             PERFORM MFS-ROER-EJ-FAELT-IN                                
061800            END-IF                                                        
061900         ELSE                                                             
062000            MOVE NEJ TO INDATA-SW                                         
062100         END-IF                                                           
062200       END-IF                                                             
062300*    END-IF                                                               
062400     .                                                                    
062500     EJECT                                                                
062600*GA-KOLLA-KOPIERING SECTION.                                              
062700*                                                                         
062800*      IF MID-IDARTNR-KOPI       NOT = ALL '+'                            
062900*        IF MID-IDARTNR-KOPI NOT NUMERIC                                  
063000*          MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-KOPI-ATTR                
063100*          MOVE NEJ TO INDATA-SW                                          
063200*        ELSE                                                             
063300*           MOVE MID-IDARTNR-KOPI TO KW-IDARTNR KW-IDARTNR-SPAR           
063400*           PERFORM IMS-GU-KWDK6-CLAG                                     
063500*           IF SEGMENT-SAKNAS                                             
063600*             MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-KOPI-ATTR             
063700*             MOVE NEJ TO INDATA-SW                                       
063800*           ELSE                                                          
063900*             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDARTNR-KOPI-ATTR           
064000*           END-IF                                                        
064100*        END-IF                                                           
064200*      END-IF                                                             
064300*    .                                                                    
064400*    EJECT                                                                
064500*GB-KOLLA-Q0-EMBALLAGE SECTION.                                           
064600*                                                                         
064700*     SER TILL SÅ ATT INGET SKRÄP LÄGGS UPP PÅ SEGMENTET                  
064800*      IF MID-IDARTNR-EMBQ0-IN   NOT = ALL '+'  OR                        
064900*         MID-KVQPACK-EMBQ0-IN   NOT = ALL '+'  OR                        
065000*         MID-KDEMBKOD-EMBQ0-IN  NOT = ALL '+'                            
065100*             MOVE 'Q0' TO W-KDEMBAL                                      
065200*             PERFORM IMS-GET-WDK6-EMB-KVAL                               
065300*             MOVE NEJ TO NYTT-13-SEGMENT (11)                            
065400*             IF SEGMENT-SAKNAS                                           
065500*                MOVE JA  TO NYTT-13-SEGMENT (11)                         
065600*                MOVE ZERO TO WQ-IDARTNR (1)                              
065700*                MOVE ZERO TO WQ-KVQPACK (1)                              
065800*                MOVE ZERO TO WQ-KDEMBKOD (1)                             
065900*             END-IF                                                      
066000*      END-IF                                                             
066100                                                                          
066200*      IF MID-IDARTNR-EMBQ0-IN NOT = ALL '+'                              
066300*        IF MID-IDARTNR-EMBQ0-IN NOT NUMERIC                              
066400*          MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-EMBQ0-IN-ATTR            
066500*          MOVE NEJ TO INDATA-SW                                          
066600*        ELSE                                                             
066700*           MOVE MID-IDARTNR-EMBQ0-IN TO KW-IDARTNR                       
066800*           PERFORM IMS-GU-KWDK6-CLAG                                     
066900*           IF SEGMENT-SAKNAS                                             
067000*            IF MID-IDARTNR-EMBQ0-IN = ZERO                               
067100*             MOVE MFS-NUM-FAELT-RAETT     TO                             
067200*                                MOD-IDARTNR-EMBQ0-IN-ATTR                
067300*             MOVE MID-IDARTNR-EMBQ0-IN    TO                             
067400*                                WQ-IDARTNR (1)                           
067500*            ELSE                                                         
067600*             MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-EMBQ0-IN-ATTR         
067700*             MOVE NEJ TO INDATA-SW                                       
067800*            END-IF                                                       
067900*           ELSE                                                          
068000*             MOVE MFS-NUM-FAELT-RAETT     TO                             
068100*                                MOD-IDARTNR-EMBQ0-IN-ATTR                
068200*             MOVE MID-IDARTNR-EMBQ0-IN    TO                             
068300*                                WQ-IDARTNR (1)                           
068400*           END-IF                                                        
068500*        END-IF                                                           
068600*      END-IF                                                             
068700*      IF MID-KVQPACK-EMBQ0-IN NOT = ALL '+'                              
068800*        IF MID-KVQPACK-EMBQ0-IN NOT NUMERIC                              
068900*          MOVE MFS-NUM-FAELT-FEL   TO MOD-KVQPACK-EMBQ0-IN-ATTR          
069000*          MOVE NEJ                 TO INDATA-SW                          
069100*        ELSE                                                             
069200*          MOVE MFS-NUM-FAELT-RAETT     TO                                
069300*                                MOD-KVQPACK-EMBQ0-IN-ATTR                
069400*          MOVE MID-KVQPACK-EMBQ0-IN    TO                                
069500*                                WQ-KVQPACK (1)                           
069600*        END-IF                                                           
069700*      END-IF                                                             
069800*      IF MID-KDEMBKOD-EMBQ0-IN NOT = ALL '+'                             
069900*        IF MID-KDEMBKOD-EMBQ0-IN NOT NUMERIC                             
070000*          MOVE MFS-NUM-FAELT-FEL   TO MOD-KDEMBKOD-EMBQ0-IN-ATTR         
070100*          MOVE NEJ                 TO INDATA-SW                          
070200*        ELSE                                                             
070300*          MOVE MFS-NUM-FAELT-RAETT     TO                                
070400*                                MOD-KDEMBKOD-EMBQ0-IN-ATTR               
070500*          MOVE MID-KDEMBKOD-EMBQ0-IN   TO                                
070600*                                WQ-KDEMBKOD (1)                          
070700*        END-IF                                                           
070800*      END-IF                                                             
070900*      IF INDATA-OK                                                       
071000**        EN KOLL OM RAD ÄR RÖRD OCH OM EJ HELA RADEN UTFYLLD,            
071100**        SÅ MÅSTE DET REDAN FINNAS ETT SEGM (REPLACE)                    
071200*             IF (MID-IDARTNR-EMBQ0-IN  NOT = ALL '+' OR                  
071300*                 MID-KVQPACK-EMBQ0-IN  NOT = ALL '+' OR                  
071400*                 MID-KDEMBKOD-EMBQ0-IN NOT = ALL '+')                    
071500*                IF (MID-IDARTNR-EMBQ0-IN   = ALL '+' OR                  
071600*                    MID-IDARTNR-EMBQ0-IN   = ALL '0' OR                  
071700*                    MID-KVQPACK-EMBQ0-IN   = ALL '+' OR                  
071800*                    MID-KDEMBKOD-EMBQ0-IN  = ALL '+')                    
071900*                   MOVE 'Q0' TO W-KDEMBAL                                
072000*                   PERFORM IMS-GET-WDK6-EMB-KVAL                         
072100*                   IF SEGMENT-SAKNAS                                     
072200*                      MOVE MFS-NUM-FAELT-FEL   TO                        
072300*                                MOD-IDARTNR-EMBQ0-IN-ATTR                
072400*                      MOVE MFS-NUM-FAELT-FEL   TO                        
072500*                                MOD-KVQPACK-EMBQ0-IN-ATTR                
072600*                      MOVE MFS-NUM-FAELT-FEL   TO                        
072700*                                MOD-KDEMBKOD-EMBQ0-IN-ATTR               
072800*                      MOVE NEJ TO INDATA-SW                              
072900*                   END-IF                                                
073000*                END-IF                                                   
073100*             END-IF                                                      
073200*      END-IF                                                             
073300*    .                                                                    
073400*    EJECT                                                                
073500*GC-KOLLA-Q1-EMBALLAGE SECTION.                                           
073600*                                                                         
073700*     SER TILL SÅ ATT INGET SKRÄP LÄGGS UPP PÅ SEGMENTET                  
073800*      IF MID-IDARTNR-EMBQ1-IN   NOT = ALL '+'  OR                        
073900*         MID-KVQPACK-EMBQ1-IN   NOT = ALL '+'  OR                        
074000*         MID-KDEMBKOD-EMBQ1-IN  NOT = ALL '+'                            
074100*             MOVE 'Q1' TO W-KDEMBAL                                      
074200*             PERFORM IMS-GET-WDK6-EMB-KVAL                               
074300*             MOVE NEJ  TO NYTT-13-SEGMENT (12)                           
074400*             IF SEGMENT-SAKNAS                                           
074500*                MOVE JA  TO NYTT-13-SEGMENT (12)                         
074600*                MOVE ZERO TO WQ-IDARTNR (2)                              
074700*                MOVE ZERO TO WQ-KVQPACK (2)                              
074800*                MOVE ZERO TO WQ-KDEMBKOD (2)                             
074900*             END-IF                                                      
075000*      END-IF                                                             
075100                                                                          
075200*      IF MID-IDARTNR-EMBQ1-IN NOT = ALL '+'                              
075300*        IF MID-IDARTNR-EMBQ1-IN NOT NUMERIC                              
075400*          MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-EMBQ1-IN-ATTR            
075500*          MOVE NEJ TO INDATA-SW                                          
075600*        ELSE                                                             
075700*           MOVE MID-IDARTNR-EMBQ1-IN TO KW-IDARTNR                       
075800*           PERFORM IMS-GU-KWDK6-CLAG                                     
075900*           IF SEGMENT-SAKNAS                                             
076000*            IF MID-IDARTNR-EMBQ1-IN = ZERO                               
076100*             MOVE MFS-NUM-FAELT-RAETT     TO                             
076200*                                MOD-IDARTNR-EMBQ1-IN-ATTR                
076300*             MOVE MID-IDARTNR-EMBQ1-IN    TO                             
076400*                                WQ-IDARTNR (2)                           
076500*            ELSE                                                         
076600*             MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-EMBQ1-IN-ATTR         
076700*             MOVE NEJ TO INDATA-SW                                       
076800*            END-IF                                                       
076900*           ELSE                                                          
077000*             MOVE MFS-NUM-FAELT-RAETT     TO                             
077100*                                MOD-IDARTNR-EMBQ1-IN-ATTR                
077200*             MOVE MID-IDARTNR-EMBQ1-IN    TO                             
077300*                                WQ-IDARTNR (2)                           
077400*           END-IF                                                        
077500*        END-IF                                                           
077600*      END-IF                                                             
077700*      IF MID-KVQPACK-EMBQ1-IN NOT = ALL '+'                              
077800*        IF MID-KVQPACK-EMBQ1-IN NOT NUMERIC                              
077900*          MOVE MFS-NUM-FAELT-FEL   TO MOD-KVQPACK-EMBQ1-IN-ATTR          
078000*          MOVE NEJ                 TO INDATA-SW                          
078100*        ELSE                                                             
078200*          MOVE MFS-NUM-FAELT-RAETT     TO                                
078300*                                MOD-KVQPACK-EMBQ1-IN-ATTR                
078400*          MOVE MID-KVQPACK-EMBQ1-IN    TO                                
078500*                                WQ-KVQPACK (2)                           
078600*        END-IF                                                           
078700*      END-IF                                                             
078800*      IF MID-KDEMBKOD-EMBQ1-IN NOT = ALL '+'                             
078900*        IF MID-KDEMBKOD-EMBQ1-IN NOT NUMERIC                             
079000*          MOVE MFS-NUM-FAELT-FEL   TO MOD-KDEMBKOD-EMBQ1-IN-ATTR         
079100*          MOVE NEJ                 TO INDATA-SW                          
079200*        ELSE                                                             
079300*          MOVE MFS-NUM-FAELT-RAETT     TO                                
079400*                                MOD-KDEMBKOD-EMBQ1-IN-ATTR               
079500*          MOVE MID-KDEMBKOD-EMBQ1-IN   TO                                
079600*                                WQ-KDEMBKOD (2)                          
079700*        END-IF                                                           
079800*      END-IF                                                             
079900*      IF INDATA-OK                                                       
080000**        EN KOLL OM RAD ÄR RÖRD OCH OM EJ HELA RADEN UTFYLLD,            
080100**        SÅ MÅSTE DET REDAN FINNAS ETT SEGM (REPLACE)                    
080200*             IF (MID-IDARTNR-EMBQ1-IN  NOT = ALL '+' OR                  
080300*                 MID-KVQPACK-EMBQ1-IN  NOT = ALL '+' OR                  
080400*                 MID-KDEMBKOD-EMBQ1-IN NOT = ALL '+')                    
080500*                IF (MID-IDARTNR-EMBQ1-IN   = ALL '+' OR                  
080600*                    MID-IDARTNR-EMBQ1-IN   = ALL '0' OR                  
080700*                    MID-KVQPACK-EMBQ1-IN   = ALL '+' OR                  
080800*                    MID-KDEMBKOD-EMBQ1-IN  = ALL '+')                    
080900*                   MOVE 'Q1' TO W-KDEMBAL                                
081000*                   PERFORM IMS-GET-WDK6-EMB-KVAL                         
081100*                   IF SEGMENT-SAKNAS                                     
081200*                      MOVE MFS-NUM-FAELT-FEL   TO                        
081300*                                MOD-IDARTNR-EMBQ1-IN-ATTR                
081400*                      MOVE MFS-NUM-FAELT-FEL   TO                        
081500*                                MOD-KVQPACK-EMBQ1-IN-ATTR                
081600*                      MOVE MFS-NUM-FAELT-FEL   TO                        
081700*                                MOD-KDEMBKOD-EMBQ1-IN-ATTR               
081800*                      MOVE NEJ TO INDATA-SW                              
081900*                   END-IF                                                
082000*                END-IF                                                   
082100*             END-IF                                                      
082200*      END-IF                                                             
082300*    .                                                                    
082400*    EJECT                                                                
082500 GD-KOLLA-Q2-EMBALLAGE SECTION.                                           
082600                                                                          
082700*     SER TILL SÅ ATT INGET SKRÄP LÄGGS UPP PÅ SEGMENTET                  
082800*      IF MID-IDARTNR-EMBQ2-IN   NOT = ALL '+'  OR                        
082900*         MID-KVQPACK-EMBQ2-IN   NOT = ALL '+'  OR                        
083000       IF MID-KDEMBKOD-EMBQ2-IN  NOT = ALL '+'                            
083100              MOVE 'Q2' TO W-KDEMBAL                                      
083200              PERFORM IMS-GET-WDK6-EMB-KVAL                               
083300              MOVE NEJ TO NYTT-13-SEGMENT (13)                            
083400              IF SEGMENT-SAKNAS                                           
083500              MOVE 'NYTT-13-SEG' TO LOG-T6                                
083600                 MOVE JA  TO NYTT-13-SEGMENT (13)                         
083700                 MOVE ZERO TO WQ-IDARTNR (3)                              
083800                 MOVE ZERO TO WQ-KVQPACK (3)                              
083900                 MOVE ZERO TO WQ-KDEMBKOD (3)                             
084000              END-IF                                                      
084100       END-IF                                                             
084200                                                                          
084300*      IF MID-IDARTNR-EMBQ2-IN NOT = ALL '+'                              
084400*        IF MID-IDARTNR-EMBQ2-IN NOT NUMERIC                              
084500*          MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-EMBQ2-IN-ATTR            
084600*          MOVE NEJ TO INDATA-SW                                          
084700*        ELSE                                                             
084800*           MOVE MID-IDARTNR-EMBQ2-IN TO KW-IDARTNR                       
084900*           PERFORM IMS-GU-KWDK6-CLAG                                     
085000*           IF SEGMENT-SAKNAS                                             
085100*            IF MID-IDARTNR-EMBQ2-IN = ZERO                               
085200*             MOVE MFS-NUM-FAELT-RAETT     TO                             
085300*                                MOD-IDARTNR-EMBQ2-IN-ATTR                
085400*             MOVE MID-IDARTNR-EMBQ2-IN    TO                             
085500*                                WQ-IDARTNR (3)                           
085600*            ELSE                                                         
085700*             MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-EMBQ2-IN-ATTR         
085800*             MOVE NEJ TO INDATA-SW                                       
085900*            END-IF                                                       
086000*           ELSE                                                          
086100*             MOVE MFS-NUM-FAELT-RAETT     TO                             
086200*                                MOD-IDARTNR-EMBQ2-IN-ATTR                
086300*             MOVE MID-IDARTNR-EMBQ2-IN    TO                             
086400*                                WQ-IDARTNR (3)                           
086500*           END-IF                                                        
086600*        END-IF                                                           
086700*      END-IF                                                             
086800*      IF MID-KVQPACK-EMBQ2-IN NOT = ALL '+'                              
086900*        IF MID-KVQPACK-EMBQ2-IN NOT NUMERIC                              
087000*          MOVE MFS-NUM-FAELT-FEL   TO MOD-KVQPACK-EMBQ2-IN-ATTR          
087100*          MOVE NEJ                 TO INDATA-SW                          
087200*        ELSE                                                             
087300*          MOVE MFS-NUM-FAELT-RAETT     TO                                
087400*                                MOD-KVQPACK-EMBQ2-IN-ATTR                
087500*          MOVE MID-KVQPACK-EMBQ2-IN    TO                                
087600*                                WQ-KVQPACK (3)                           
087700*        END-IF                                                           
087800*      END-IF                                                             
087900       IF MID-KDEMBKOD-EMBQ2-IN NOT = ALL '+'                             
088000         IF MID-KDEMBKOD-EMBQ2-IN NOT NUMERIC                             
088100           MOVE MFS-NUM-FAELT-FEL   TO MOD-KDEMBKOD-EMBQ2-IN-ATTR         
088200           MOVE NEJ                 TO INDATA-SW                          
088300         ELSE                                                             
088400           MOVE MFS-NUM-FAELT-RAETT     TO                                
088500                                 MOD-KDEMBKOD-EMBQ2-IN-ATTR               
088600           MOVE MID-KDEMBKOD-EMBQ2-IN   TO                                
088700                                 WQ-KDEMBKOD (3)                          
088800         END-IF                                                           
088900       END-IF                                                             
089000                    MOVE 'ENTRY 1       ' TO LOG-T1                       
089100       IF INDATA-OK                                                       
089200                    MOVE 'INDATA-OK     ' TO LOG-T2                       
089300*         EN KOLL OM RAD ÄR RÖRD OCH OM EJ HELA RADEN UTFYLLD,            
089400*         SÅ MÅSTE DET REDAN FINNAS ETT SEGM (REPLACE)                    
089500*             IF (MID-IDARTNR-EMBQ2-IN  NOT = ALL '+' OR                  
089600*                 MID-KVQPACK-EMBQ2-IN  NOT = ALL '+' OR                  
089700*                 MID-KDEMBKOD-EMBQ2-IN NOT = ALL '+')                    
089800              IF  MID-KDEMBKOD-EMBQ2-IN NOT = ALL '+'                     
089900*                IF (MID-IDARTNR-EMBQ2-IN   = ALL '+' OR                  
090000*                    MID-IDARTNR-EMBQ2-IN   = ALL '0' OR                  
090100*                    MID-KVQPACK-EMBQ2-IN   = ALL '+' OR                  
090200*                    MID-KDEMBKOD-EMBQ2-IN  = ALL '+')                    
090300                 IF  MID-KDEMBKOD-EMBQ2-IN  = ALL '+'                     
090400                    MOVE 'Q2' TO W-KDEMBAL                                
090500                    PERFORM IMS-GET-WDK6-EMB-KVAL                         
090600                    IF SEGMENT-SAKNAS                                     
090700                    MOVE 'SEGMENT SAKNAS' TO LOG-T5                       
090800*                      MOVE MFS-NUM-FAELT-FEL   TO                        
090900*                                MOD-IDARTNR-EMBQ2-IN-ATTR                
091000*                      MOVE MFS-NUM-FAELT-FEL   TO                        
091100*                                MOD-KVQPACK-EMBQ2-IN-ATTR                
091200                       MOVE MFS-NUM-FAELT-FEL   TO                        
091300                                 MOD-KDEMBKOD-EMBQ2-IN-ATTR               
091400                       MOVE NEJ TO INDATA-SW                              
091500                    ELSE                                                  
091600                    MOVE 'SEGMENT FINNS' TO LOG-T5                        
091700                    END-IF                                                
091800                 END-IF                                                   
091900              END-IF                                                      
092000       END-IF                                                             
092100     .                                                                    
092200     EJECT                                                                
092300 GE-KOLLA-Q3-EMBALLAGE SECTION.                                           
092400*                                                                         
092500*     SER TILL SÅ ATT INGET SKRÄP LÄGGS UPP PÅ SEGMENTET                  
092600*      IF MID-KVQPACK-EMBQ3-IN   NOT = ALL '+'                            
092700*             MOVE 'Q3' TO W-KDEMBAL                                      
092800*             PERFORM IMS-GET-WDK6-EMB-KVAL                               
092900*             MOVE NEJ TO NYTT-13-SEGMENT (14)                            
093000*             IF SEGMENT-SAKNAS                                           
093100*                MOVE JA  TO NYTT-13-SEGMENT (14)                         
093200*                MOVE ZERO TO WQ-KVQPACK (4)                              
093300*             END-IF                                                      
093400*      END-IF                                                             
093500*                                                                         
093600*      IF MID-KVQPACK-EMBQ3-IN NOT = ALL '+'                              
093700*        IF MID-KVQPACK-EMBQ3-IN NOT NUMERIC                              
093800*          MOVE MFS-NUM-FAELT-FEL   TO MOD-KVQPACK-EMBQ3-IN-ATTR          
093900*          MOVE NEJ                 TO INDATA-SW                          
094000*        ELSE                                                             
094100*          MOVE MFS-NUM-FAELT-RAETT     TO                                
094200*                                MOD-KVQPACK-EMBQ3-IN-ATTR                
094300*          MOVE MID-KVQPACK-EMBQ3-IN    TO                                
094400*                                WQ-KVQPACK (4)                           
094500*        END-IF                                                           
094600*      END-IF                                                             
094700*      IF INDATA-OK                                                       
094800*         EN KOLL OM RAD ÄR RÖRD OCH OM EJ HELA RADEN UTFYLLD,            
094900*         SÅ MÅSTE DET REDAN FINNAS ETT SEGM (REPLACE)                    
095000*             IF (MID-IDARTNR-EMBQ2-IN  NOT = ALL '+' OR                  
095100*                 MID-KVQPACK-EMBQ2-IN  NOT = ALL '+' OR                  
095200*                 MID-KDEMBKOD-EMBQ2-IN NOT = ALL '+')                    
095300*             IF  MID-KDEMBKOD-EMBQ2-IN NOT = ALL '+'                     
095400*                IF (MID-IDARTNR-EMBQ2-IN   = ALL '+' OR                  
095500*                    MID-IDARTNR-EMBQ2-IN   = ALL '0' OR                  
095600*                    MID-KVQPACK-EMBQ2-IN   = ALL '+' OR                  
095700*                    MID-KDEMBKOD-EMBQ2-IN  = ALL '+')                    
095800*                IF  MID-KDEMBKOD-EMBQ2-IN  = ALL '+'                     
095900*                   MOVE 'Q2' TO W-KDEMBAL                                
096000*                   PERFORM IMS-GET-WDK6-EMB-KVAL                         
096100*                   IF SEGMENT-SAKNAS                                     
096200*                      MOVE MFS-NUM-FAELT-FEL   TO                        
096300*                                MOD-IDARTNR-EMBQ2-IN-ATTR                
096400*                      MOVE MFS-NUM-FAELT-FEL   TO                        
096500*                                MOD-KVQPACK-EMBQ2-IN-ATTR                
096600*                      MOVE MFS-NUM-FAELT-FEL   TO                        
096700*                                MOD-KDEMBKOD-EMBQ2-IN-ATTR               
096800*                      MOVE NEJ TO INDATA-SW                              
096900*                   END-IF                                                
097000*                END-IF                                                   
097100*             END-IF                                                      
097200*      END-IF                                                             
097300*    .                                                                    
097400*    EJECT                                                                
097500*GF-KOLLA-X-EMBALLAGE SECTION.                                            
097600*                                                                         
097700*      MOVE +1 TO IX-X                                                    
097800*      PERFORM UNTIL IX-X > 10                                            
097900*                                                                         
098000*       IF MID-IDARTNR-EMBX-IN (IX-X)    NOT = ALL '+' OR                 
098100*          MID-KVQPACK-EMBX-IN (IX-X)    NOT = ALL '+'                    
098200*             MOVE 'X' TO WS-KDEMBKEY                                     
098300*             MOVE IX-X TO WS-KDEMBKEY-IX                                 
098400*             MOVE WS-KDEMBKEY TO W-KDEMBAL                               
098500*             PERFORM IMS-GET-WDK6-EMB-KVAL                               
098600*             MOVE NEJ  TO NYTT-13-SEGMENT (IX-X)                         
098700*             IF SEGMENT-SAKNAS                                           
098800*                MOVE JA   TO NYTT-13-SEGMENT (IX-X)                      
098900*                MOVE ZERO TO WX-IDARTNR (IX-X)                           
099000*                MOVE ZERO TO WX-KVQPACK (IX-X)                           
099100*             END-IF                                                      
099200*       END-IF                                                            
099300                                                                          
099400*         IF MID-IDARTNR-EMBX-IN (IX-X)  NOT = ALL '+'                    
099500*           IF MID-IDARTNR-EMBX-IN (IX-X)  NOT NUMERIC                    
099600*              MOVE MFS-NUM-FAELT-FEL            TO                       
099700*                                MOD-IDARTNR-EMBX-IN-ATTR (IX-X)          
099800*              MOVE NEJ TO INDATA-SW                                      
099900*           ELSE                                                          
100000*              MOVE MID-IDARTNR-EMBX-IN (IX-X)   TO KW-IDARTNR            
100100*              PERFORM IMS-GU-KWDK6-CLAG                                  
100200*              IF SEGMENT-SAKNAS                                          
100300*               IF MID-IDARTNR-EMBX-IN (IX-X) = ZERO                      
100400*                 MOVE MFS-NUM-FAELT-RAETT        TO                      
100500*                                MOD-IDARTNR-EMBX-IN-ATTR (IX-X)          
100600*                 MOVE MID-IDARTNR-EMBX-IN (IX-X) TO                      
100700*                                WX-IDARTNR (IX-X)                        
100800*               ELSE                                                      
100900*                 MOVE MFS-NUM-FAELT-FEL          TO                      
101000*                                MOD-IDARTNR-EMBX-IN-ATTR (IX-X)          
101100*                 MOVE NEJ TO INDATA-SW                                   
101200*               END-IF                                                    
101300*              ELSE                                                       
101400*                 MOVE MFS-NUM-FAELT-RAETT        TO                      
101500*                                MOD-IDARTNR-EMBX-IN-ATTR (IX-X)          
101600*                 MOVE MID-IDARTNR-EMBX-IN (IX-X) TO                      
101700*                                WX-IDARTNR (IX-X)                        
101800*              END-IF                                                     
101900*           END-IF                                                        
102000*         END-IF                                                          
102100*         IF MID-KVQPACK-EMBX-IN (IX-X)  NOT = ALL '+'                    
102200*           IF MID-KVQPACK-EMBX-IN (IX-X)  NOT NUMERIC                    
102300*             MOVE MFS-NUM-FAELT-FEL   TO                                 
102400*                                MOD-KVQPACK-EMBX-IN-ATTR (IX-X)          
102500*             MOVE NEJ                 TO INDATA-SW                       
102600*           ELSE                                                          
102700*             MOVE MFS-NUM-FAELT-RAETT TO                                 
102800*                                MOD-KVQPACK-EMBX-IN-ATTR (IX-X)          
102900*             MOVE MID-KVQPACK-EMBX-IN (IX-X) TO                          
103000*                                WX-KVQPACK (IX-X)                        
103100*           END-IF                                                        
103200*         END-IF                                                          
103300*         IF MID-IDARTNR-EMBX-IN (IX-X)  NOT = ALL '+' OR                 
103400*            MID-KVQPACK-EMBX-IN (IX-X)  NOT = ALL '+'                    
103500*                IF MID-IDARTNR-EMBX-IN (IX-X) = ALL '+' OR               
103600*                   MID-IDARTNR-EMBX-IN (IX-X) = ALL '0' OR               
103700*                   MID-KVQPACK-EMBX-IN (IX-X) = ALL '+'                  
103800*                   MOVE 'X'  TO WS-KDEMBKEY                              
103900*                   MOVE IX-X TO WS-KDEMBKEY-IX                           
104000*                   MOVE WS-KDEMBKEY TO W-KDEMBAL                         
104100*                   PERFORM IMS-GET-WDK6-EMB-KVAL                         
104200*                   IF SEGMENT-SAKNAS                                     
104300*                      MOVE MFS-NUM-FAELT-FEL   TO                        
104400*                                MOD-IDARTNR-EMBX-IN-ATTR (IX-X)          
104500*                      MOVE MFS-NUM-FAELT-FEL   TO                        
104600*                                MOD-KVQPACK-EMBX-IN-ATTR (IX-X)          
104700*                      MOVE NEJ TO INDATA-SW                              
104800*                   END-IF                                                
104900*                END-IF                                                   
105000*         END-IF                                                          
105100*                                                                         
105200*         ADD +1 TO IX-X                                                  
105300*      END-PERFORM                                                        
105400*    .                                                                    
105500*    EJECT                                                                
105600 H-UPPDATERA SECTION.                                                     
105700                                                                          
105800     MOVE W-IDARTNR-SPAR TO W-IDARTNR                                     
105900     PERFORM IMS-GU-WDK6-ROT                                              
106000     PERFORM IMS-GET-WDK6-CLAG                                            
106100     IF SEGMENT-FINNS                                                     
106200*       IF MID-IDARTNR-KOPI NOT = ALL '+'                                 
106300*          PERFORM HA-KOPIERA                                             
106400*       ELSE                                                              
106500          PERFORM HB-UPPD-WDK611                                          
106600          PERFORM HC-UPPD-WDK613                                          
106700*       END-IF                                                            
106800                                                                          
106900          MOVE INF-UPDATE-DONE TO MED-IDMFSINF                            
107000          CALL WMEDKONV USING MED-WMEDAREA                                
107100          MOVE MED-MFSINF TO MOD-TEMFSINF                                 
107200          PERFORM MFS-FORM-ATTR                                           
107300*         PERFORM MFS-RENSA-FAELT-IN                                      
107400* * * MFS-ROR-EJ-FAELT TILL FASTA VÄRDEN                                  
107500     END-IF                                                               
107600     .                                                                    
107700     EJECT                                                                
107800 HA-KOPIERA  SECTION.                                                     
107900                                                                          
108000*    KOPIERING FRÅN KCLAG TILL CLAG                                       
108100                                                                          
108200     MOVE KW-IDARTNR-SPAR             TO KW-IDARTNR                       
108300     PERFORM IMS-GU-KWDK6-ROT                                             
108400     PERFORM IMS-GET-KWDK6-CLAG                                           
108500     IF SEGMENT-FINNS                                                     
108600        IF CLAG-IDARTNR-EMBQ0   NOT = KCLAG-IDARTNR-EMBQ0 OR              
108700           CLAG-IDARTNR-EMBQ1   NOT = KCLAG-IDARTNR-EMBQ1 OR              
108800           CLAG-IDARTNR-EMBQ2   NOT = KCLAG-IDARTNR-EMBQ2 OR              
108900           CLAG-KVQPACK-0       NOT = KCLAG-KVQPACK-0     OR              
109000           CLAG-KVQPACK-1       NOT = KCLAG-KVQPACK-1     OR              
109100           CLAG-KVQPACK-2       NOT = KCLAG-KVQPACK-2     OR              
109200           CLAG-KVQPACK-3       NOT = KCLAG-KVQPACK-3     OR              
109300           CLAG-KDEMBKOD-0      NOT = KCLAG-KDEMBKOD-0    OR              
109400           CLAG-KDEMBKOD-1      NOT = KCLAG-KDEMBKOD-1    OR              
109500           CLAG-KDEMBKOD-2      NOT = KCLAG-KDEMBKOD-2                    
109600           MOVE KCLAG-IDARTNR-EMBQ0   TO CLAG-IDARTNR-EMBQ0               
109700           MOVE KCLAG-IDARTNR-EMBQ1   TO CLAG-IDARTNR-EMBQ1               
109800           MOVE KCLAG-IDARTNR-EMBQ2   TO CLAG-IDARTNR-EMBQ2               
109900           MOVE KCLAG-KVQPACK-0       TO CLAG-KVQPACK-0                   
110000           MOVE KCLAG-KVQPACK-1       TO CLAG-KVQPACK-1                   
110100           MOVE KCLAG-KVQPACK-2       TO CLAG-KVQPACK-2                   
110200           MOVE KCLAG-KVQPACK-3       TO CLAG-KVQPACK-3                   
110300           MOVE KCLAG-KDEMBKOD-0      TO CLAG-KDEMBKOD-0                  
110400           MOVE KCLAG-KDEMBKOD-1      TO CLAG-KDEMBKOD-1                  
110500           MOVE KCLAG-KDEMBKOD-2      TO CLAG-KDEMBKOD-2                  
110600           MOVE MSG-SIGNON-USERID     TO CLAG-IDUSER-EMB                  
110700           MOVE DAGENS-DATUM          TO CLAG-TIUPPDAT-EMB                
110800           PERFORM IMS-REPL-WDK6-CLAG                                     
110900* OBS ÄVEN WDK613 SKALL UPPDATERAS MED DESSA DATA                         
111000*          BORDE RÄCKA MED NEDANSTÅENDE (GAMLA VÄRDEN PÅ K611 ?)          
111100*                                       (KONVERTERAS BAS ?     )          
111200        END-IF                                                            
111300                                                                          
111400        PERFORM IMS-GET-KWDK6-EMB                                         
111500        PERFORM UNTIL SEGMENT-SAKNAS                                      
111600           MOVE KEMB-KDEMBKEY   TO W-KDEMBAL                              
111700           PERFORM IMS-GET-WDK6-EMB-KVAL                                  
111800           MOVE KEMB-WDK613     TO EMB-WDK613                             
111900           IF SEGMENT-FINNS                                               
112000              PERFORM IMS-REPL-WDK6-EMB                                   
112100           ELSE                                                           
112200              PERFORM IMS-ISRT-WDK6-EMB                                   
112300           END-IF                                                         
112400           PERFORM IMS-GET-KWDK6-EMB                                      
112500        END-PERFORM                                                       
112600* OBS  MÖJLIGT FEL:  ATT GAMLA SEGMENT EJ TAS BORT !                      
112700* OBS  TILLS VIDARE GÖR VI DETTA MEDVETET                                 
112800     END-IF                                                               
112900     .                                                                    
113000     EJECT                                                                
113100 HB-UPPD-WDK611  SECTION.                                                 
113200                                                                          
113300        MOVE MSG-SIGNON-USERID       TO CLAG-IDUSER-EMB                   
113400        MOVE DAGENS-DATUM            TO CLAG-TIUPPDAT-EMB                 
113500*       IF MID-IDARTNR-EMBQ0-IN   NOT = ALL '+'                           
113600*          MOVE WQ-IDARTNR (1)       TO CLAG-IDARTNR-EMBQ0                
113700*       END-IF                                                            
113800*       IF MID-IDARTNR-EMBQ1-IN   NOT = ALL '+'                           
113900*          MOVE WQ-IDARTNR (2)       TO CLAG-IDARTNR-EMBQ1                
114000*       END-IF                                                            
114100*       IF MID-IDARTNR-EMBQ2-IN   NOT = ALL '+'                           
114200*          MOVE WQ-IDARTNR (3)       TO CLAG-IDARTNR-EMBQ2                
114300*       END-IF                                                            
114400*       IF MID-KVQPACK-EMBQ0-IN   NOT = ALL '+'                           
114500*          MOVE WQ-KVQPACK (1)       TO CLAG-KVQPACK-0                    
114600*       END-IF                                                            
114700*       IF MID-KVQPACK-EMBQ1-IN   NOT = ALL '+'                           
114800*          MOVE WQ-KVQPACK (2)       TO CLAG-KVQPACK-1                    
114900*       END-IF                                                            
115000*       IF MID-KVQPACK-EMBQ2-IN   NOT = ALL '+'                           
115100*          MOVE WQ-KVQPACK (3)       TO CLAG-KVQPACK-2                    
115200*       END-IF                                                            
115300*       IF MID-KVQPACK-EMBQ3-IN   NOT = ALL '+'                           
115400*          MOVE WQ-KVQPACK (4)       TO CLAG-KVQPACK-3                    
115500*       END-IF                                                            
115600*       IF MID-KDEMBKOD-EMBQ0-IN  NOT = ALL '+'                           
115700*          MOVE WQ-KDEMBKOD(1)       TO CLAG-KDEMBKOD-0                   
115800*       END-IF                                                            
115900*       IF MID-KDEMBKOD-EMBQ1-IN  NOT = ALL '+'                           
116000*          MOVE WQ-KDEMBKOD(2)       TO CLAG-KDEMBKOD-1                   
116100*       END-IF                                                            
116200        IF MID-KDEMBKOD-EMBQ2-IN  NOT = ALL '+'                           
116300           MOVE WQ-KDEMBKOD(3)       TO CLAG-KDEMBKOD-2                   
116400        END-IF                                                            
116500                                                                          
116600        PERFORM IMS-REPL-WDK6-CLAG                                        
116700     .                                                                    
116800     EJECT                                                                
116900 HC-UPPD-WDK613  SECTION.                                                 
117000                                                                          
117100     IF MID-EMB-Q NOT = ALL '+'                                           
117200        PERFORM HCA-UPPD-WDK613-Q                                         
117300     END-IF                                                               
117400                                                                          
117500     IF MID-EMB-X (1)  NOT = ALL '+' OR                                   
117600        MID-EMB-X (2)  NOT = ALL '+' OR                                   
117700        MID-EMB-X (3)  NOT = ALL '+' OR                                   
117800        MID-EMB-X (4)  NOT = ALL '+' OR                                   
117900        MID-EMB-X (5)  NOT = ALL '+' OR                                   
118000        MID-EMB-X (6)  NOT = ALL '+' OR                                   
118100        MID-EMB-X (7)  NOT = ALL '+' OR                                   
118200        MID-EMB-X (8)  NOT = ALL '+' OR                                   
118300        MID-EMB-X (9)  NOT = ALL '+' OR                                   
118400        MID-EMB-X (10) NOT = ALL '+'                                      
118500                                                                          
118600        MOVE +1 TO IX-X                                                   
118700        PERFORM UNTIL IX-X > 10                                           
118800*          IF MID-IDARTNR-EMBX-IN (IX-X) NOT = ALL '+' OR                 
118900*             MID-KVQPACK-EMBX-IN (IX-X) NOT = ALL '+'                    
119000*             MOVE 'X'  TO WS-KDEMBKEY                                    
119100*             MOVE IX-X TO WS-KDEMBKEY-IX                                 
119200*             MOVE WS-KDEMBKEY TO W-KDEMBAL                               
119300*             PERFORM IMS-GET-WDK6-EMB-KVAL                               
119400*             IF MID-IDARTNR-EMBX-IN (IX-X) NOT = ALL '+'  OR             
119500*                NYTT-13-SEGMENT (IX-X) = JA                              
119600*                MOVE WX-IDARTNR (IX-X) TO EMB-IDARTNR-EMB                
119700*             END-IF                                                      
119800*             IF MID-KVQPACK-EMBX-IN (IX-X) NOT = ALL '+'  OR             
119900*                NYTT-13-SEGMENT (IX-X) = JA                              
120000*                MOVE WX-KVQPACK (IX-X) TO EMB-KVQPACK-EMB                
120100*             END-IF                                                      
120200*             MOVE ZERO              TO EMB-KDEMBKOD                      
120300*             IF SEGMENT-FINNS                                            
120400*                IF EMB-IDARTNR-EMB = ZERO                                
120500*                   PERFORM IMS-DLET-WDK6-EMB                             
120600*                ELSE                                                     
120700*                   PERFORM IMS-REPL-WDK6-EMB                             
120800*                END-IF                                                   
120900*             ELSE                                                        
121000*                MOVE WS-KDEMBKEY    TO EMB-KDEMBKEY                      
121100*                PERFORM IMS-ISRT-WDK6-EMB                                
121200*             END-IF                                                      
121300*          END-IF                                                         
121400           ADD +1 TO IX-X                                                 
121500        END-PERFORM                                                       
121600                                                                          
121700     END-IF                                                               
121800     .                                                                    
121900     EJECT                                                                
122000 HCA-UPPD-WDK613-Q  SECTION.                                              
122100*                                                                         
122200*       IF MID-IDARTNR-EMBQ0-IN  NOT = ALL '+' OR                         
122300*          MID-KVQPACK-EMBQ0-IN  NOT = ALL '+' OR                         
122400*          MID-KDEMBKOD-EMBQ0-IN NOT = ALL '+'                            
122500*          MOVE 'Q0' TO W-KDEMBAL                                         
122600*          PERFORM IMS-GET-WDK6-EMB-KVAL                                  
122700*          IF MID-IDARTNR-EMBQ0-IN  NOT = ALL '+' OR                      
122800*             NYTT-13-SEGMENT (11) = JA                                   
122900*             MOVE WQ-IDARTNR  (1)   TO EMB-IDARTNR-EMB                   
123000*          END-IF                                                         
123100*          IF MID-KVQPACK-EMBQ0-IN  NOT = ALL '+' OR                      
123200*             NYTT-13-SEGMENT (11) = JA                                   
123300*             MOVE WQ-KVQPACK  (1)   TO EMB-KVQPACK-EMB                   
123400*          END-IF                                                         
123500*          IF MID-KDEMBKOD-EMBQ0-IN NOT = ALL '+' OR                      
123600*             NYTT-13-SEGMENT (11) = JA                                   
123700*             MOVE WQ-KDEMBKOD (1)   TO EMB-KDEMBKOD                      
123800*          END-IF                                                         
123900*          IF SEGMENT-FINNS                                               
124000*             PERFORM IMS-REPL-WDK6-EMB                                   
124100*          ELSE                                                           
124200*             MOVE 'Q0'           TO EMB-KDEMBKEY                         
124300*             PERFORM IMS-ISRT-WDK6-EMB                                   
124400*          END-IF                                                         
124500*       END-IF                                                            
124600                                                                          
124700*       IF MID-IDARTNR-EMBQ1-IN  NOT = ALL '+' OR                         
124800*          MID-KVQPACK-EMBQ1-IN  NOT = ALL '+' OR                         
124900*          MID-KDEMBKOD-EMBQ1-IN NOT = ALL '+'                            
125000*          MOVE 'Q1' TO W-KDEMBAL                                         
125100*          PERFORM IMS-GET-WDK6-EMB-KVAL                                  
125200*          IF MID-IDARTNR-EMBQ1-IN  NOT = ALL '+' OR                      
125300*             NYTT-13-SEGMENT (12) = JA                                   
125400*             MOVE WQ-IDARTNR  (2)   TO EMB-IDARTNR-EMB                   
125500*          END-IF                                                         
125600*          IF MID-KVQPACK-EMBQ1-IN  NOT = ALL '+' OR                      
125700*             NYTT-13-SEGMENT (12) = JA                                   
125800*             MOVE WQ-KVQPACK  (2)   TO EMB-KVQPACK-EMB                   
125900*          END-IF                                                         
126000*          IF MID-KDEMBKOD-EMBQ1-IN NOT = ALL '+' OR                      
126100*             NYTT-13-SEGMENT (12) = JA                                   
126200*             MOVE WQ-KDEMBKOD (2)   TO EMB-KDEMBKOD                      
126300*          END-IF                                                         
126400*          IF SEGMENT-FINNS                                               
126500*             PERFORM IMS-REPL-WDK6-EMB                                   
126600*          ELSE                                                           
126700*             MOVE 'Q1'           TO EMB-KDEMBKEY                         
126800*             PERFORM IMS-ISRT-WDK6-EMB                                   
126900*          END-IF                                                         
127000*       END-IF                                                            
127100                                                                          
127200*       IF MID-IDARTNR-EMBQ2-IN  NOT = ALL '+' OR                         
127300*          MID-KVQPACK-EMBQ2-IN  NOT = ALL '+' OR                         
127400*          MID-KDEMBKOD-EMBQ2-IN NOT = ALL '+'                            
127500        IF MID-KDEMBKOD-EMBQ2-IN NOT = ALL '+'                            
127600           MOVE 'Q2' TO W-KDEMBAL                                         
127700           PERFORM IMS-GET-WDK6-EMB-KVAL                                  
127800*          IF MID-IDARTNR-EMBQ2-IN  NOT = ALL '+' OR                      
127900*             NYTT-13-SEGMENT (13) = JA                                   
128000*             MOVE WQ-IDARTNR  (3)   TO EMB-IDARTNR-EMB                   
128100*          END-IF                                                         
128200*          IF MID-KVQPACK-EMBQ2-IN  NOT = ALL '+' OR                      
128300*             NYTT-13-SEGMENT (13) = JA                                   
128400*             MOVE WQ-KVQPACK  (3)   TO EMB-KVQPACK-EMB                   
128500*          END-IF                                                         
128600           IF MID-KDEMBKOD-EMBQ2-IN NOT = ALL '+' OR                      
128700              NYTT-13-SEGMENT (13) = JA                                   
128800              MOVE WQ-KDEMBKOD (3)   TO EMB-KDEMBKOD                      
128900           MOVE 'XYZ UPPD.' TO LOG-T3                                     
129000           END-IF                                                         
129100           IF SEGMENT-FINNS                                               
129200              PERFORM IMS-REPL-WDK6-EMB                                   
129300           MOVE 'XYZ REPL ' TO LOG-T4                                     
129400           ELSE                                                           
129500              MOVE 'Q2'           TO EMB-KDEMBKEY                         
129600              PERFORM IMS-ISRT-WDK6-EMB                                   
129700           MOVE 'XYZ ISRT ' TO LOG-T4                                     
129800           END-IF                                                         
129900        END-IF                                                            
130000*       IF MID-KVQPACK-EMBQ3-IN  NOT = ALL '+'                            
130100*          MOVE 'Q3' TO W-KDEMBAL                                         
130200*          PERFORM IMS-GET-WDK6-EMB-KVAL                                  
130300*          IF MID-KVQPACK-EMBQ3-IN  NOT = ALL '+' OR                      
130400*             NYTT-13-SEGMENT (14) = JA                                   
130500*             MOVE WQ-KVQPACK  (4)   TO EMB-KVQPACK-EMB                   
130600*          END-IF                                                         
130700*          IF SEGMENT-FINNS                                               
130800*             PERFORM IMS-REPL-WDK6-EMB                                   
130900*          ELSE                                                           
131000*             MOVE 'Q3'           TO EMB-KDEMBKEY                         
131100*             PERFORM IMS-ISRT-WDK6-EMB                                   
131200*          END-IF                                                         
131300*       END-IF                                                            
131400     .                                                                    
131500     EJECT                                                                
131600 MFS-RENSA-FAELT-UT SECTION.                                              
131700*                                                                         
131800*    --- ALLA UTDATA-FÄLT                                                 
131900*    MOVE MFS-RENSA-FAELT TO MOD-BEFT-UT                                  
132000*                            MOD-KDFORP-UT                                
132100*                            MOD-FLFPINST-UPP                             
132200*                            MOD-TIUPPDAT-UT                              
132300*                            MOD-IDUSER-UT                                
132400*                            MOD-IDARTNR-EMBQ0-UT                         
132500*                            MOD-IDARTNR-EMBQ1-UT                         
132600*                            MOD-IDARTNR-EMBQ2-UT                         
132700*                            MOD-KVQPACK-EMBQ0-UT                         
132800*                            MOD-KVQPACK-EMBQ1-UT                         
132900*                            MOD-KVQPACK-EMBQ2-UT                         
133000*                            MOD-KVQPACK-EMBQ3-UT                         
133100*                            MOD-KDEMBKOD-EMBQ0-UT                        
133200*                            MOD-KDEMBKOD-EMBQ1-UT                        
133300     MOVE MFS-RENSA-FAELT TO MOD-KDEMBKOD-EMBQ2-UT                        
133400*    MOVE +1 TO IX-X                                                      
133500*    PERFORM UNTIL IX-X > 10                                              
133600*       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-EMBX-UT (IX-X)                
133700*                               MOD-KVQPACK-EMBX-UT (IX-X)                
133800*       ADD +1 TO IX-X                                                    
133900*    END-PERFORM                                                          
134000*                                                                         
134100     .                                                                    
134200     EJECT                                                                
134300*MFS-RENSA-FAELT-IN SECTION.                                              
134400*                                                                         
134500*    --- ALLA INDATA-FÄLT                                                 
134600*    MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-KOPI                             
134700*    MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-EMBQ0-IN                         
134800*                            MOD-IDARTNR-EMBQ1-IN                         
134900*                            MOD-IDARTNR-EMBQ2-IN                         
135000*                            MOD-KVQPACK-EMBQ0-IN                         
135100*                            MOD-KVQPACK-EMBQ1-IN                         
135200*                            MOD-KVQPACK-EMBQ2-IN                         
135300*                            MOD-KVQPACK-EMBQ3-IN                         
135400*                            MOD-KDEMBKOD-EMBQ0-IN                        
135500*                            MOD-KDEMBKOD-EMBQ1-IN                        
135600*                            MOD-KDEMBKOD-EMBQ2-IN                        
135700*    MOVE +1 TO IX-X                                                      
135800*    PERFORM UNTIL IX-X > 10                                              
135900*       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-EMBX-IN (IX-X)                
136000*                               MOD-KVQPACK-EMBX-IN (IX-X)                
136100*       ADD +1 TO IX-X                                                    
136200*    END-PERFORM                                                          
136300*    .                                                                    
136400*    EJECT                                                                
136500 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
136600*                                                                         
136700*    --- ALLA UTDATA-FÄLT                                                 
136800*    MOVE MFS-ROER-EJ-FAELT TO MOD-BEFT-UT                                
136900*                              MOD-KDFORP-UT                              
137000*                              MOD-FLFPINST-UPP                           
137100*                              MOD-TIUPPDAT-UT                            
137200*                              MOD-IDUSER-UT                              
137300*                              MOD-IDARTNR-EMBQ0-UT                       
137400*                              MOD-IDARTNR-EMBQ1-UT                       
137500*                              MOD-IDARTNR-EMBQ2-UT                       
137600*                              MOD-KVQPACK-EMBQ0-UT                       
137700*                              MOD-KVQPACK-EMBQ1-UT                       
137800*                              MOD-KVQPACK-EMBQ2-UT                       
137900*                              MOD-KVQPACK-EMBQ3-UT                       
138000*                              MOD-KDEMBKOD-EMBQ0-UT                      
138100*                              MOD-KDEMBKOD-EMBQ1-UT                      
138200     MOVE MFS-ROER-EJ-FAELT TO MOD-KDEMBKOD-EMBQ2-UT                      
138300*    MOVE +1 TO IX-X                                                      
138400*    PERFORM UNTIL IX-X > 10                                              
138500*       MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-EMBX-UT (IX-X)              
138600*                                 MOD-KVQPACK-EMBX-UT (IX-X)              
138700*       ADD +1 TO IX-X                                                    
138800*    END-PERFORM                                                          
138900     .                                                                    
139000     SKIP3                                                                
139100*MFS-ROER-EJ-FAELT-IN  SECTION.                                           
139200*                                                                         
139300*    --- ALLA INDATA-FÄLT                                                 
139400*    MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-KOPI                           
139500*    MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-EMBQ0-IN                       
139600*                              MOD-IDARTNR-EMBQ1-IN                       
139700*                              MOD-IDARTNR-EMBQ2-IN                       
139800*                              MOD-KVQPACK-EMBQ0-IN                       
139900*                              MOD-KVQPACK-EMBQ1-IN                       
140000*                              MOD-KVQPACK-EMBQ2-IN                       
140100*                              MOD-KVQPACK-EMBQ3-IN                       
140200*                              MOD-KDEMBKOD-EMBQ0-IN                      
140300*                              MOD-KDEMBKOD-EMBQ1-IN                      
140400*                              MOD-KDEMBKOD-EMBQ2-IN                      
140500*    MOVE +1 TO IX-X                                                      
140600*    PERFORM UNTIL IX-X > 10                                              
140700*       MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-EMBX-IN (IX-X)              
140800*                                 MOD-KVQPACK-EMBX-IN (IX-X)              
140900*       ADD +1 TO IX-X                                                    
141000*    END-PERFORM                                                          
141100*    .                                                                    
141200*    EJECT                                                                
141300 MFS-FORM-ATTR SECTION.                                                   
141400                                                                          
141500*    --- ALLA INDATA-FÄLT                                                 
141600*    MOVE MFS-FORMATETS-ATTR TO MOD-IDARTNR-KOPI-ATTR                     
141700*    MOVE MFS-FORMATETS-ATTR TO MOD-IDARTNR-EMBQ0-IN-ATTR                 
141800*                               MOD-IDARTNR-EMBQ1-IN-ATTR                 
141900*                               MOD-IDARTNR-EMBQ2-IN-ATTR                 
142000*                               MOD-KVQPACK-EMBQ0-IN-ATTR                 
142100*                               MOD-KVQPACK-EMBQ1-IN-ATTR                 
142200*                               MOD-KVQPACK-EMBQ2-IN-ATTR                 
142300*                               MOD-KVQPACK-EMBQ3-IN-ATTR                 
142400*                               MOD-KDEMBKOD-EMBQ0-IN-ATTR                
142500*                               MOD-KDEMBKOD-EMBQ1-IN-ATTR                
142600     MOVE MFS-FORMATETS-ATTR TO MOD-KDEMBKOD-EMBQ2-IN-ATTR                
142700     MOVE +1 TO IX-X                                                      
142800*    PERFORM UNTIL IX-X > 10                                              
142900*       MOVE MFS-FORMATETS-ATTR TO MOD-IDARTNR-EMBX-IN-ATTR (IX-X)        
143000*                                  MOD-KVQPACK-EMBX-IN-ATTR (IX-X)        
143100*       ADD +1 TO IX-X                                                    
143200*    END-PERFORM                                                          
143300     .                                                                    
143400     SKIP2                                                                
143500 MFS-LAES-IN-IGEN SECTION.                                                
143600                                                                          
143700*    --- ALLA INDATA-FÄLT                                                 
143800*    MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDARTNR-KOPI-ATTR                  
143900*    MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDARTNR-EMBQ0-IN-ATTR              
144000*                                  MOD-IDARTNR-EMBQ1-IN-ATTR              
144100*                                  MOD-IDARTNR-EMBQ2-IN-ATTR              
144200*                                  MOD-KVQPACK-EMBQ0-IN-ATTR              
144300*                                  MOD-KVQPACK-EMBQ1-IN-ATTR              
144400*                                  MOD-KVQPACK-EMBQ2-IN-ATTR              
144500*                                  MOD-KVQPACK-EMBQ3-IN-ATTR              
144600*                                  MOD-KDEMBKOD-EMBQ0-IN-ATTR             
144700*                                  MOD-KDEMBKOD-EMBQ1-IN-ATTR             
144800     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDEMBKOD-EMBQ2-IN-ATTR             
144900*    MOVE +1 TO IX-X                                                      
145000*    PERFORM UNTIL IX-X > 10                                              
145100*       MOVE MFS-ADD-LAES-IN-FAELT                                        
145200*                               TO MOD-IDARTNR-EMBX-IN-ATTR(IX-X)         
145300*                                  MOD-KVQPACK-EMBX-IN-ATTR(IX-X)         
145400*       ADD +1 TO IX-X                                                    
145500*    END-PERFORM                                                          
145600     .                                                                    
145700     EJECT                                                                
145800* --- IMS SEKTIONER ---                                                   
145900     SKIP3                                                                
146000 IMS-GET-MSG SECTION.                                                     
146100                                                                          
146200     MOVE '  QC' TO GODK-STATUSKODER                                      
146300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
146400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
146500     PERFORM IMS-STATUSKONTROLL                                           
146600     .                                                                    
146700     SKIP3                                                                
146800 IMS-INSERT-MSG SECTION.                                                  
146900                                                                          
147000*    IF MSGI-IDLAND-SPR = 'SE'                                            
147100*      MOVE '0' TO MFS-KDHUVOMR                                           
147200*    END-IF                                                               
147300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
147400     MOVE SPACE TO GODK-STATUSKODER                                       
147500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
147600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
147700     PERFORM IMS-STATUSKONTROLL                                           
147800     .                                                                    
147900     EJECT                                                                
148000 IMS-GU-WDK6-ROT SECTION.                                                 
148100                                                                          
148200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
148300          DELIMITED BY SIZE INTO SSA1                                     
148400     MOVE '  GE' TO GODK-STATUSKODER                                      
148500     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
148600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
148700     PERFORM IMS-STATUSKONTROLL                                           
148800     .                                                                    
148900     EJECT                                                                
149000 IMS-GET-WDK6-CLAG SECTION.                                               
149100                                                                          
149200     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
149300          DELIMITED BY SIZE INTO SSA1                                     
149400     MOVE '  GE' TO GODK-STATUSKODER                                      
149500     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK611 SSA1                  
149600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
149700     PERFORM IMS-STATUSKONTROLL                                           
149800     .                                                                    
149900     SKIP3                                                                
150000 IMS-REPL-WDK6-CLAG SECTION.                                              
150100                                                                          
150200     MOVE '  ' TO GODK-STATUSKODER                                        
150300     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
150400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
150500     PERFORM IMS-STATUSKONTROLL                                           
150600     .                                                                    
150700     EJECT                                                                
150800 IMS-GU-WDT311     SECTION.                                               
150900                                                                          
150910     STRING 'WDT301  (IDARTNR  =' W-IDARTNR-X ')'                         
150920          DELIMITED BY SIZE INTO SSA1                                     
151000     STRING 'WDT311  (IDLAND   =' W-IDLAND-X ')'                          
151200          DELIMITED BY SIZE INTO SSA2                                     
151300     MOVE '  GE' TO GODK-STATUSKODER                                      
151400     CALL CBLTDLI USING GU WDT3-PCB DLI-IO-WDT311 SSA1 SSA2               
151500     MOVE WDT3-STATUS-CODE TO STATUS-WS                                   
151600     PERFORM IMS-STATUSKONTROLL                                           
151700     .                                                                    
151800     EJECT                                                                
151900 IMS-GET-WDK6-EMB SECTION.                                                
152000                                                                          
152100     STRING 'WDK613    '                                                  
152200          DELIMITED BY SIZE INTO SSA1                                     
152300     MOVE '  GE' TO GODK-STATUSKODER                                      
152400     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK613 SSA1                  
152500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
152600     PERFORM IMS-STATUSKONTROLL                                           
152700     .                                                                    
152800     SKIP3                                                                
152900 IMS-GET-WDK6-EMB-KVAL SECTION.                                           
153000                                                                          
153100     STRING 'WDK613  (KDEMBAL  =' W-KDEMBAL-X ')'                         
153200          DELIMITED BY SIZE INTO SSA1                                     
153300     MOVE '  GE' TO GODK-STATUSKODER                                      
153400     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK613 SSA1                  
153500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
153600     PERFORM IMS-STATUSKONTROLL                                           
153700****** TESTER                                                             
153800     MOVE SSA1 TO LOG-SSA1                                                
153900     MOVE WDK6-STATUS-CODE TO LOG-STATUS                                  
154000****** TESTER                                                             
154100     .                                                                    
154200     SKIP3                                                                
154300 IMS-ISRT-WDK6-EMB SECTION.                                               
154400                                                                          
154500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
154600          DELIMITED BY SIZE INTO SSA1                                     
154700     MOVE 'WDK613   ' TO SSA2                                             
154800     MOVE '  II' TO GODK-STATUSKODER                                      
154900     CALL CBLTDLI USING ISRT WDK6-PCB DLI-IO-WDK613 SSA1 SSA2             
155000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
155100     PERFORM IMS-STATUSKONTROLL                                           
155200     .                                                                    
155300     SKIP3                                                                
155400 IMS-REPL-WDK6-EMB SECTION.                                               
155500                                                                          
155600     MOVE '  ' TO GODK-STATUSKODER                                        
155700     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK613                       
155800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
155900     PERFORM IMS-STATUSKONTROLL                                           
156000     .                                                                    
156100     SKIP3                                                                
156200 IMS-DLET-WDK6-EMB SECTION.                                               
156300                                                                          
156400     MOVE '  ' TO GODK-STATUSKODER                                        
156500     CALL CBLTDLI USING DLET WDK6-PCB DLI-IO-WDK613                       
156600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
156700     PERFORM IMS-STATUSKONTROLL                                           
156800     .                                                                    
156900     EJECT                                                                
157000*    ANROP FÖR KOPIERING                                                  
157100                                                                          
157200 IMS-GU-KWDK6-ROT SECTION.                                                
157300                                                                          
157400     STRING 'WDK601  (IDARTNR  =' KW-IDARTNR-X ')'                        
157500          DELIMITED BY SIZE INTO SSA1                                     
157600     MOVE '  GE' TO GODK-STATUSKODER                                      
157700     CALL CBLTDLI USING GU KWDK6-PCB DLI-IO-KWDK601 SSA1                  
157800     MOVE KWDK6-STATUS-CODE TO STATUS-WS                                  
157900     PERFORM IMS-STATUSKONTROLL                                           
158000     .                                                                    
158100     SKIP3                                                                
158200 IMS-GU-KWDK6-CLAG SECTION.                                               
158300                                                                          
158400     STRING 'WDK601  (IDARTNR  =' KW-IDARTNR-X ')'                        
158500          DELIMITED BY SIZE INTO SSA1                                     
158600     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
158700          DELIMITED BY SIZE INTO SSA2                                     
158800     MOVE '  GE' TO GODK-STATUSKODER                                      
158900     CALL CBLTDLI USING GU KWDK6-PCB DLI-IO-KWDK611 SSA1 SSA2             
159000     MOVE KWDK6-STATUS-CODE TO STATUS-WS                                  
159100     PERFORM IMS-STATUSKONTROLL                                           
159200     .                                                                    
159300     EJECT                                                                
159400 IMS-GET-KWDK6-CLAG SECTION.                                              
159500                                                                          
159600     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
159700          DELIMITED BY SIZE INTO SSA1                                     
159800     MOVE '  GE' TO GODK-STATUSKODER                                      
159900     CALL CBLTDLI USING GHNP KWDK6-PCB DLI-IO-KWDK611 SSA1                
160000     MOVE KWDK6-STATUS-CODE TO STATUS-WS                                  
160100     PERFORM IMS-STATUSKONTROLL                                           
160200     .                                                                    
160300     EJECT                                                                
160400 IMS-GET-KWDK6-EMB SECTION.                                               
160500                                                                          
160600     STRING 'WDK613    '                                                  
160700          DELIMITED BY SIZE INTO SSA1                                     
160800     MOVE '  GE' TO GODK-STATUSKODER                                      
160900     CALL CBLTDLI USING GHNP KWDK6-PCB DLI-IO-KWDK613 SSA1                
161000     MOVE KWDK6-STATUS-CODE TO STATUS-WS                                  
161100     PERFORM IMS-STATUSKONTROLL                                           
161200                                                                          
161300     .                                                                    
161400     EJECT                                                                
161500 IMS-STATUSKONTROLL SECTION.                                              
161600                                                                          
161700     SET STATUS-IX TO 1                                                   
161800     SEARCH GODK-STATUS                                                   
161900       AT END                                                             
162000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
162100         DELIMITED BY SIZE INTO FELTEXT                                   
162200         CALL FELLOG                                                      
162300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
162400         CONTINUE                                                         
162500     END-SEARCH                                                           
162600     .                                                                    
