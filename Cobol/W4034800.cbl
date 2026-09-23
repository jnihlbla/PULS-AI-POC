000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4034800.                                                
000300 AUTHOR.         BODIL LINDAHL.                                           
000400 DATE-WRITTEN.   97/10/27.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMET STARTAR RUTIN W413S2 I SOP                            
000900*        (PRINTNING AV BAR CODE ETIKETTER)                                
001000*                                                                         
001100*        PROGRAMMET LÄSER      WDE4                                       
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSAKTION: W4T348                                              
001600*        MID:         W4I348N1                                            
001700*                                                                         
001800*    UTDATA.                                                              
001900*        MOD:         W4O348N1                                            
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500 WORKING-STORAGE SECTION.                                                 
002600                                                                          
002601                                                                          
002610*    -- CHECKED BY WY2000                                                 
002700 77  IDPGM                       PIC X(08)   VALUE 'W4034800'.            
002800                                                                          
002900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003000 77  JA                          PIC X       VALUE 'J'.                   
003100 77  NEJ                         PIC X       VALUE 'N'.                   
003300 77  WS-IDDISTR                  PIC 9(4)    VALUE ZERO.                  
003400 77  WS-IDKUNDNR                 PIC 9(6)    VALUE ZERO.                  
003600 77  WS-IDPRODNR                 PIC 9(7)    VALUE ZERO.                  
003610 77  WS-SPAR-IDPRODNR            PIC 9(7)    VALUE ZERO.                  
003700 77  SW-TRAEFF                   PIC X       VALUE SPACE.                 
003800 77  WS-LINE                     PIC X       VALUE SPACE.                 
003900 77  WS-INTERVALL                PIC X       VALUE SPACE.                 
004000 77  WS-ANTAL                    PIC X       VALUE SPACE.                 
004100 77  WS-KVANTAL                  PIC 9(7)    VALUE ZERO.                  
004200 77  WS-IDRADNR                  PIC 9(3)    VALUE ZERO.                  
004300 77  WS-IDRADNR-MIN              PIC 9(3)    VALUE ZERO.                  
004400 77  WS-IDRADNR-MAX              PIC 9(3)    VALUE ZERO.                  
004500 77  PRODNR-NYCKEL-SW            PIC X       VALUE SPACE.                 
004520 77  WS-IDORDNR7                 PIC X(7)    VALUE SPACE.                 
004600                                                                          
004700 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004800     88  NYCKLAR-OK                          VALUE 'J'.                   
004900     88  NYCKLAR-FEL                         VALUE 'N'.                   
005000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005100     88  INDATA-OK                           VALUE 'J'.                   
005200     88  INDATA-FEL                          VALUE 'N'.                   
005300 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005400     88  EGEN-MID                            VALUE '4348'.                
005500     88  HELP-MID                            VALUE '0551'.                
005600     EJECT                                                                
005601 01  PROG-TO-PROG-SW.                                                     
005602*    03  -COPY WMSGSOP                                                    
005603     SKIP3                                                                
005610 01  WS-BC-PARAMETRAR.                                                    
005620     03  WS-BC.                                                           
005630         05  BC-URV-IDPRODNR     PIC 9(7)  VALUE ZERO.                    
005640         05  BC-URV-IDRADNR-MIN  PIC 9(5)  VALUE ZERO.                    
005650         05  BC-URV-IDRADNR-MAX  PIC 9(5)  VALUE ZERO.                    
005651         05  BC-URV-KVANTAL      PIC 9(7)  VALUE ZERO.                    
005660     EJECT                                                                
005700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
005800 01  GENERELLA-SUBPROGRAM.                                                
005900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006300     EJECT                                                                
006400*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
006500*01 -COPY WMEDAREA                                                        
006600     SKIP3                                                                
006700 01  MESSAGE-CODES.                                                       
006800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
006801     03  TRYCK-PF4-FOR-PRINT     PIC X(3)    VALUE '081'.                 
006810     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
006900     03  ORDER-SAKNAS            PIC X(3)    VALUE '054'.                 
007000     03  PF4-NO-INDATA           PIC X(3)    VALUE '231'.                 
007001     03  INF-PRINT-BEGAERD       PIC X(3)    VALUE '118'.                 
007010     SKIP3                                                                
007020 01  MESSAGE-TEXT.                                                        
007030     03  ORDER-FINNS             PIC X(25)   VALUE                        
007031         'ORDER OK'.                                                      
007100     EJECT                                                                
007200*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
007300*                                                                         
007400 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
007500     SKIP3                                                                
007600*01 -COPY WMSGINIT                                                        
007700     EJECT                                                                
007800*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
007900*                                                                         
008000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
008100     SKIP3                                                                
008200*01  MID -COPY W4I34801                                                   
008300     EJECT                                                                
008400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
008500     SKIP3                                                                
008600*01  -COPY WMSGAREA                                                       
008700     EJECT                                                                
008800     03  MOD REDEFINES MSG-AREA.                                          
008900*      05  -COPY W4O34801                                                 
009000     EJECT                                                                
009100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
009200     SKIP3                                                                
009300*01  -COPY WMFSAREA                                                       
009400     EJECT                                                                
009500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009600*                                                                         
009700     SKIP3                                                                
009800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009900     SKIP3                                                                
010000 01  NYCKLAR-TILL-DLI.                                                    
010100     03  W-WDE4ASEQ-MIN-X.                                                
010200         05  W-IDDISTR-MIN       PIC S9(5) VALUE ZERO COMP-3.             
010300         05  W-IDKUNDNR-MIN      PIC S9(7) VALUE ZERO COMP-3.             
010400         05  W-IDKUNDRF-MIN      PIC X(10) VALUE SPACE.                   
010700                                                                          
010800     03  W-WDE4ASEQ-MAX-X.                                                
010900         05  W-IDDISTR-MAX       PIC S9(5) VALUE ZERO COMP-3.             
011000         05  W-IDKUNDNR-MAX      PIC S9(7) VALUE ZERO COMP-3.             
011100         05  W-IDKUNDRF-MAX      PIC X(10) VALUE SPACE.                   
011400                                                                          
011500     03  W-IDPURAD-X.                                                     
011600         05  W-IDPURAD           PIC S9(5) VALUE ZERO COMP-3.             
011700                                                                          
011800     03  W-IDPRODNR-X.                                                    
011900         05  W-IDPRODNR          PIC S9(7) VALUE ZERO COMP-3.             
012300     SKIP2                                                                
012400*    --- STATUS-KOD FRÅN IMS                                              
012500 01  STATUS-WS                   PIC XX.                                  
012600     88  SEGMENT-FINNS                       VALUE '  '.                  
012700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012900     SKIP2                                                                
013000 01  GODK-STATUSKODER.                                                    
013100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013200     SKIP3                                                                
013300 01  SSA1                        PIC X(128).                              
013400 01  SSA2                        PIC X(64).                               
013500     EJECT                                                                
013600*    --- IMS FUNKTIONSKODER                                               
013700*01  -COPY W0003                                                          
013800     EJECT                                                                
013900*    ---  DLI INPUT-OUTPUT AREA                                           
014000                                                                          
014100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE401'.                      
014200 01  DLI-IO-WDE401.                                                       
014300*    03  -COPY WDE401                                                     
014400     EJECT                                                                
014500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE411'.                      
014600 01  DLI-IO-WDE411.                                                       
014700*    03  -COPY WDE411                                                     
014710     EJECT                                                                
016100 LINKAGE SECTION.                                                         
016200*01  -COPY W0009   -PRE MSG-                                              
016210     EJECT                                                                
016220*01  -COPY W0009   -PRE ALT-                                              
016230     EJECT                                                                
016300*01  -COPY W0008   -PRE WDP7-                                             
016400     05  FILLER                  PIC X.                                   
016500     EJECT                                                                
016600*01  -COPY W0008  -PRE WDE4-                                              
016700     05  FILLER                  PIC X.                                   
016800     EJECT                                                                
017110*01  -COPY W0008  -PRE WDE41-                                             
017120     05  FILLER                  PIC X.                                   
017130     EJECT                                                                
017200 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB WDP7-PCB WDE4-PCB              
017210                     WDE41-PCB.                                           
017300 MAIN SECTION.                                                            
017400     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB WDP7-PCB WDE4-PCB              
017410                     WDE41-PCB.                                           
017500                                                                          
017600     PERFORM IMS-GET-MSG                                                  
017700     IF SEGMENT-FINNS                                                     
017800       PERFORM A-INIT                                                     
017900       PERFORM B-KOLLA-NYCKLAR                                            
018000       IF NYCKLAR-OK                                                      
018100         IF MFS-PRINT                                                     
018200            PERFORM C-KOLLA-INDATA                                        
018300            IF INDATA-OK                                                  
018400               PERFORM G-STARTA-SOP                                       
018500            END-IF                                                        
018510         ELSE                                                             
018520            IF MFS-UPDATE                                                 
018530               PERFORM H-MID-INDATA-TILL-MOD                              
018600            ELSE                                                          
018700               PERFORM F-LAES                                             
018800            END-IF                                                        
018810         END-IF                                                           
018900       END-IF                                                             
019000       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O34801 + 4                      
019100       PERFORM IMS-INSERT-MSG                                             
019200     END-IF                                                               
019300                                                                          
019400     MOVE ZERO TO RETURN-CODE                                             
019500     GOBACK                                                               
019600     .                                                                    
019700     EJECT                                                                
019800 A-INIT SECTION.                                                          
019900                                                                          
020000     IF MSG-DUBBLA-TRANSKODER                                             
020100       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I34801                 
020200       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
020300       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
020400     ELSE                                                                 
020500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I34801                  
020600       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
020700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
020800     END-IF                                                               
020900                                                                          
021000     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
021100     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
021200     MOVE MFS-IDTRANS TO W-IDTRANS                                        
021300                                                                          
021400     MOVE LOW-VALUE  TO MSG-AREA                                          
021500     MOVE 'W4O348N1' TO MFS-IDMOD                                         
021600     MOVE '4348'     TO MOD-IDTRANS                                       
021700     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
021701                                                                          
021900     IF EGEN-MID OR HELP-MID                                              
022000       CONTINUE                                                           
022100     ELSE                                                                 
022200       MOVE SPACE TO MFS-KDTRTYP                                          
022300       MOVE '7' TO MFS-IDPFK                                              
022400     END-IF                                                               
022500     .                                                                    
022600     EJECT                                                                
022700 B-KOLLA-NYCKLAR SECTION.                                                 
022800                                                                          
022900     MOVE ALL '+'           TO MSGI-WMSGINIT                              
023000     MOVE '001'             TO MSGI-KDCALL                                
023100     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
023200     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
023300     MOVE '4348'            TO MSGI-IDTRANS                               
023310     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
023320     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
023400                                                                          
023410     IF MID-IDDISTR-IN NOT = ALL '+'                                      
023420        INSPECT MID-IDDISTR-IN REPLACING LEADING SPACE BY ZERO            
023430     END-IF                                                               
023440     IF MID-IDKUNDNR-IN NOT = ALL '+'                                     
023450        INSPECT MID-IDKUNDNR-IN REPLACING LEADING SPACE BY ZERO           
023460     END-IF                                                               
023470     IF MID-IDORDNR7-IN NOT = ALL '+'                                     
023480        INSPECT MID-IDORDNR7-IN REPLACING LEADING SPACE BY ZERO           
023490     END-IF                                                               
023491     IF MID-IDPRODNR-IN NOT = ALL '+'                                     
023492        INSPECT MID-IDPRODNR-IN REPLACING LEADING SPACE BY ZERO           
023493     END-IF                                                               
023494                                                                          
025300     MOVE JA TO NYCKLAR-SW                                                
025400                PRODNR-NYCKEL-SW                                          
025500                                                                          
026900     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
027100     IF MID-IDDISTR-IN = ALL '+'                                          
027310       INSPECT MID-IDDISTR-UT REPLACING                                   
027320                   LEADING SPACE BY ZERO                                  
027400       MOVE MID-IDDISTR-UT TO WS-IDDISTR                                  
027500     ELSE                                                                 
027510       MOVE '7'           TO MFS-IDPFK                                    
027520       MOVE SPACE         TO MFS-KDTRTYP                                  
027600       IF MID-IDDISTR-IN NUMERIC                                          
027700          MOVE MID-IDDISTR-IN TO WS-IDDISTR                               
027800       ELSE                                                               
027900          MOVE NEJ TO NYCKLAR-SW                                          
028000       END-IF                                                             
028100     END-IF                                                               
028200                                                                          
028300     MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-IN                              
028400     IF MID-IDKUNDNR-IN = ALL '+'                                         
028610       INSPECT MID-IDKUNDNR-UT REPLACING                                  
028620                   LEADING SPACE BY ZERO                                  
028700       MOVE MID-IDKUNDNR-UT TO WS-IDKUNDNR                                
028800     ELSE                                                                 
028810       MOVE '7'         TO MFS-IDPFK                                      
028820       MOVE SPACE       TO MFS-KDTRTYP                                    
028900       IF MID-IDKUNDNR-IN NUMERIC                                         
029000          MOVE MID-IDKUNDNR-IN TO WS-IDKUNDNR                             
029100       ELSE                                                               
029200          MOVE NEJ TO NYCKLAR-SW                                          
029300       END-IF                                                             
029310     END-IF                                                               
029400                                                                          
029500     MOVE MFS-RENSA-FAELT TO MOD-IDORDNR7-IN                              
029510     MOVE SPACE           TO WS-IDORDNR7                                  
029600     IF MID-IDORDNR7-IN = ALL '+'                                         
029700       INSPECT MID-IDORDNR7-UT REPLACING                                  
029800                   LEADING SPACE BY ZERO                                  
029820       MOVE MID-IDORDNR7-UT TO WS-IDORDNR7                                
030000     ELSE                                                                 
030100       MOVE '7'         TO MFS-IDPFK                                      
030110       MOVE SPACE       TO MFS-KDTRTYP                                    
030120       IF MID-IDORDNR7-IN NUMERIC                                         
030130          MOVE MID-IDORDNR7-IN TO WS-IDORDNR7                             
030140       ELSE                                                               
030150          MOVE NEJ TO NYCKLAR-SW                                          
030160       END-IF                                                             
030600     END-IF                                                               
030700                                                                          
030800     MOVE MFS-RENSA-FAELT TO MOD-IDPRODNR-IN                              
031300     IF MID-IDPRODNR-IN = ALL '+'                                         
031510       INSPECT MID-IDPRODNR-UT REPLACING                                  
031511                   LEADING SPACE BY ZERO                                  
031520       MOVE MID-IDPRODNR-UT TO WS-IDPRODNR                                
031530       IF WS-IDPRODNR NUMERIC                                             
031540          CONTINUE                                                        
031550       ELSE                                                               
031560          MOVE ZERO TO WS-IDPRODNR                                        
031570       END-IF                                                             
031700     ELSE                                                                 
031710       MOVE '7'         TO MFS-IDPFK                                      
031720       MOVE SPACE       TO MFS-KDTRTYP                                    
031800       IF MID-IDPRODNR-IN NUMERIC                                         
031900          MOVE MID-IDPRODNR-IN TO WS-IDPRODNR                             
032100       ELSE                                                               
032200          MOVE NEJ TO NYCKLAR-SW                                          
032300       END-IF                                                             
032400     END-IF                                                               
032700                                                                          
032800     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
032900     MOVE MSGI-IDDC       TO MOD-IDDC-UT                                  
033510                                                                          
033520     IF NYCKLAR-OK                                                        
033530       IF MID-IDPRODNR-IN = ALL '+'                                       
033531          IF MID-IDDISTR-IN NOT = ALL '+'                                 
033532             MOVE NEJ TO PRODNR-NYCKEL-SW                                 
033533          END-IF                                                          
033534          IF MID-IDKUNDNR-IN NOT = ALL '+'                                
033535             MOVE NEJ TO PRODNR-NYCKEL-SW                                 
033536          END-IF                                                          
033537          IF MID-IDORDNR7-IN NOT = ALL '+'                                
033538             MOVE NEJ TO PRODNR-NYCKEL-SW                                 
033539          END-IF                                                          
033540          IF PRODNR-NYCKEL-SW = JA                                        
033541             IF WS-IDPRODNR = ZERO                                        
033542                MOVE NEJ TO PRODNR-NYCKEL-SW                              
033543             END-IF                                                       
033544          END-IF                                                          
033547       ELSE                                                               
033550          MOVE JA TO PRODNR-NYCKEL-SW                                     
033560       END-IF                                                             
033570     END-IF                                                               
033600                                                                          
033700     IF EGEN-MID OR NYCKLAR-OK                                            
033800       IF PRODNR-NYCKEL-SW = JA                                           
033900          MOVE WS-IDPRODNR          TO MOD-IDPRODNR-UT                    
033910          INSPECT MOD-IDPRODNR-UT REPLACING LEADING ZERO BY SPACE         
034000          MOVE MFS-RENSA-FAELT      TO MOD-IDDISTR-UT                     
034100                                       MOD-IDKUNDNR-UT                    
034200                                       MOD-IDORDNR7-UT                    
034300       ELSE                                                               
034400          MOVE WS-IDDISTR           TO MOD-IDDISTR-UT                     
034410          INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE          
034500          MOVE WS-IDKUNDNR          TO MOD-IDKUNDNR-UT                    
034510          INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE         
034600          MOVE WS-IDORDNR7          TO MOD-IDORDNR7-UT                    
034610          INSPECT MOD-IDORDNR7-UT REPLACING LEADING ZERO BY SPACE         
034700          MOVE MFS-RENSA-FAELT      TO MOD-IDPRODNR-UT                    
034800       END-IF                                                             
034900     ELSE                                                                 
035000       MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                             
035100                               MOD-IDKUNDNR-UT                            
035200                               MOD-IDORDNR7-UT                            
035300                               MOD-IDPRODNR-UT                            
035500     END-IF                                                               
035600                                                                          
035700     IF NYCKLAR-FEL                                                       
035800       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
035900       CALL WMEDKONV USING MED-WMEDAREA                                   
036000       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
036200       PERFORM MFS-RENSA-FAELT-IN                                         
036300     END-IF                                                               
036400     .                                                                    
036500     EJECT                                                                
036600 C-KOLLA-INDATA SECTION.                                                  
036700                                                                          
036800     MOVE JA TO INDATA-SW                                                 
036900     MOVE NEJ  TO WS-INTERVALL                                            
037000                  WS-LINE                                                 
037100                  WS-ANTAL                                                
037200     MOVE ZERO TO WS-KVANTAL                                              
037300                                                                          
037400     IF MID-SELECT = ALL '+'                                              
037500     AND MID-IDRADNR-MIN  = ALL '+'                                       
037600     AND MID-IDRADNR-MAX = ALL '+'                                        
037700     AND MID-IDRADNR = ALL '+'                                            
037800     AND MID-KVANTAL = ALL '+'                                            
037900        MOVE PF4-NO-INDATA TO MED-IDMFSFEL                                
037910        CALL WMEDKONV USING MED-WMEDAREA                                  
037920        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
037930        PERFORM MFS-RENSA-FAELT-IN                                        
037940        MOVE NEJ TO INDATA-SW                                             
038600     ELSE                                                                 
038700        IF MID-SELECT NOT = ALL '+'                                       
038800           IF MID-SELECT = 'Y' OR 'N'                                     
038900              MOVE MFS-ALFA-FAELT-RAETT TO MOD-SELECT-ATTR                
039000           ELSE                                                           
039100              MOVE MFS-ALFA-FAELT-FEL TO MOD-SELECT-ATTR                  
039200              MOVE NEJ TO INDATA-SW                                       
039300           END-IF                                                         
039310        END-IF                                                            
039320        IF MID-SELECT = 'Y'                                               
039400           IF MID-IDRADNR-MIN NOT = ALL '+'                               
039500              MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-MIN-ATTR              
039600              MOVE NEJ TO INDATA-SW                                       
039700           END-IF                                                         
039800           IF MID-IDRADNR-MAX NOT = ALL '+'                               
039900              MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-MAX-ATTR              
040000              MOVE NEJ TO INDATA-SW                                       
040100           END-IF                                                         
040200           IF MID-IDRADNR NOT = ALL '+'                                   
040300              MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-ATTR                  
040400              MOVE NEJ TO INDATA-SW                                       
040500           END-IF                                                         
040600           IF MID-KVANTAL NOT = ALL '+'                                   
040700              MOVE MFS-NUM-FAELT-FEL TO MOD-KVANTAL-ATTR                  
040800              MOVE NEJ TO INDATA-SW                                       
040900           END-IF                                                         
040910           IF MID-KVANTAL-INTERVALL NOT = ALL '+'                         
040920              MOVE MFS-NUM-FAELT-FEL TO MOD-KVANTAL-INTERVALL-ATTR        
040930              MOVE NEJ TO INDATA-SW                                       
040940           END-IF                                                         
041000        ELSE                                                              
041100           IF MID-IDRADNR NOT = ALL '+'                                   
041110              INSPECT MID-IDRADNR REPLACING LEADING SPACE BY ZERO         
041200              IF MID-IDRADNR NUMERIC                                      
041300                 MOVE MFS-NUM-FAELT-RAETT TO MOD-IDRADNR-ATTR             
041400                 MOVE JA TO WS-LINE                                       
041500                 MOVE MID-IDRADNR TO WS-IDRADNR                           
041600              ELSE                                                        
041700                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-ATTR               
041800                 MOVE NEJ TO INDATA-SW                                    
041900              END-IF                                                      
042000              IF MID-KVANTAL NOT = ALL '+'                                
042010                 INSPECT MID-KVANTAL                                      
042020                     REPLACING LEADING SPACE BY ZERO                      
042100                 IF MID-KVANTAL NUMERIC                                   
042200                    MOVE MFS-NUM-FAELT-RAETT TO MOD-KVANTAL-ATTR          
042300                    MOVE JA TO WS-ANTAL                                   
042400                    MOVE MID-KVANTAL TO WS-KVANTAL                        
042500                 ELSE                                                     
042600                    MOVE MFS-NUM-FAELT-FEL TO MOD-KVANTAL-ATTR            
042700                    MOVE NEJ TO INDATA-SW                                 
042800                 END-IF                                                   
042900              END-IF                                                      
043000              IF MID-IDRADNR-MIN NOT = ALL '+'                            
043100                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-MIN-ATTR           
043200                 MOVE NEJ TO INDATA-SW                                    
043300              END-IF                                                      
043400              IF MID-IDRADNR-MAX NOT = ALL '+'                            
043500                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-MAX-ATTR           
043600                 MOVE NEJ TO INDATA-SW                                    
043601              END-IF                                                      
043610              IF MID-KVANTAL-INTERVALL NOT = ALL '+'                      
043620                 MOVE MFS-NUM-FAELT-FEL TO                                
043621                                    MOD-KVANTAL-INTERVALL-ATTR            
043630                 MOVE NEJ TO INDATA-SW                                    
043640              END-IF                                                      
043800           ELSE                                                           
043900              IF MID-IDRADNR-MIN NOT = ALL '+'                            
043910                 INSPECT MID-IDRADNR-MIN                                  
043920                    REPLACING LEADING SPACE BY ZERO                       
044000                 IF MID-IDRADNR-MIN NUMERIC                               
044100                    MOVE MFS-NUM-FAELT-RAETT                              
044200                         TO MOD-IDRADNR-MIN-ATTR                          
044300                    MOVE MID-IDRADNR-MIN TO WS-IDRADNR-MIN                
044400                 ELSE                                                     
044500                    MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-MIN-ATTR        
044600                    MOVE NEJ TO INDATA-SW                                 
044700                 END-IF                                                   
044800              ELSE                                                        
044900                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-MIN-ATTR           
045000                 MOVE NEJ TO INDATA-SW                                    
045100              END-IF                                                      
045200              IF MID-IDRADNR-MAX NOT = ALL '+'                            
045210                 INSPECT MID-IDRADNR-MAX                                  
045220                    REPLACING LEADING SPACE BY ZERO                       
045300                 IF MID-IDRADNR-MAX NUMERIC                               
045400                    MOVE MFS-NUM-FAELT-RAETT                              
045500                         TO MOD-IDRADNR-MAX-ATTR                          
045600                    MOVE MID-IDRADNR-MAX TO WS-IDRADNR-MAX                
045700                 ELSE                                                     
045800                    MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-MAX-ATTR        
045900                    MOVE NEJ TO INDATA-SW                                 
046000                 END-IF                                                   
046100              ELSE                                                        
046200                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-MAX-ATTR           
046300                 MOVE NEJ TO INDATA-SW                                    
046400              END-IF                                                      
046401              IF MID-KVANTAL-INTERVALL NOT = ALL '+'                      
046402                 INSPECT MID-KVANTAL-INTERVALL                            
046403                     REPLACING LEADING SPACE BY ZERO                      
046404                 IF MID-KVANTAL-INTERVALL NUMERIC                         
046405                    MOVE MFS-NUM-FAELT-RAETT                              
046406                                  TO MOD-KVANTAL-INTERVALL-ATTR           
046407                    MOVE JA TO WS-ANTAL                                   
046408                    MOVE MID-KVANTAL-INTERVALL TO WS-KVANTAL              
046409                 ELSE                                                     
046410                    MOVE MFS-NUM-FAELT-FEL                                
046411                                  TO MOD-KVANTAL-INTERVALL-ATTR           
046412                    MOVE NEJ TO INDATA-SW                                 
046413                 END-IF                                                   
046414              END-IF                                                      
046420              IF MID-KVANTAL NOT = ALL '+'                                
046430                 MOVE MFS-NUM-FAELT-FEL TO MOD-KVANTAL-ATTR               
046480                 MOVE NEJ TO INDATA-SW                                    
046490              END-IF                                                      
046500              IF MID-IDRADNR-MIN NUMERIC                                  
046510              AND MID-IDRADNR-MAX NUMERIC                                 
046530                 IF MID-IDRADNR-MIN < MID-IDRADNR-MAX                     
046540                 OR MID-IDRADNR-MIN = MID-IDRADNR-MAX                     
046600                    MOVE JA TO WS-INTERVALL                               
046610                 ELSE                                                     
046620                    MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-MIN-ATTR        
046630                    MOVE NEJ TO INDATA-SW                                 
046640                 END-IF                                                   
046700              END-IF                                                      
046800           END-IF                                                         
046900        END-IF                                                            
047100                                                                          
047200        IF INDATA-OK                                                      
047300           IF PRODNR-NYCKEL-SW = JA                                       
047400              IF WS-LINE = JA                                             
047500                 MOVE WS-IDPRODNR TO W-IDPRODNR                           
047510                 MOVE WS-IDRADNR  TO W-IDPURAD                            
047600                 PERFORM CA-LAS-WDE4ESEQ                                  
047700              ELSE                                                        
047800                 IF WS-INTERVALL = JA                                     
047900                    MOVE WS-IDPRODNR    TO W-IDPRODNR                     
048000                    MOVE WS-IDRADNR-MIN TO W-IDPURAD                      
048100                    PERFORM CA-LAS-WDE4ESEQ                               
048110                    IF INDATA-OK                                          
048200                       MOVE WS-IDRADNR-MAX TO W-IDPURAD                   
048300                       PERFORM CA-LAS-WDE4ESEQ                            
048310                    END-IF                                                
048400                 END-IF                                                   
048500              END-IF                                                      
048600           ELSE                                                           
048700              IF WS-LINE = JA                                             
048800                 MOVE WS-IDRADNR TO W-IDPURAD                             
048900                 PERFORM CB-LAS-WDE4ASEQ                                  
049000              ELSE                                                        
049100                 IF WS-INTERVALL = JA                                     
049200                    MOVE WS-IDRADNR-MIN TO W-IDPURAD                      
049300                    PERFORM CB-LAS-WDE4ASEQ                               
049301                    IF INDATA-OK                                          
049310                       MOVE WS-IDRADNR-MAX TO W-IDPURAD                   
049500                       PERFORM CB-LAS-WDE4ASEQ                            
049600                    END-IF                                                
050310                 ELSE                                                     
050311                    PERFORM CC-HAMTA-PRODNR                               
050312                 END-IF                                                   
050313              END-IF                                                      
050314           END-IF                                                         
050315           IF INDATA-FEL                                                  
050316              MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSINF                   
050317              CALL WMEDKONV USING MED-WMEDAREA                            
050318              MOVE MED-MFSINF TO MOD-TEMFSINF                             
050319              PERFORM MFS-ROER-EJ-FAELT-IN                                
050320           END-IF                                                         
050321        ELSE                                                              
050322           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
050323           CALL WMEDKONV USING MED-WMEDAREA                               
050324           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
050325           PERFORM MFS-ROER-EJ-FAELT-IN                                   
050330        END-IF                                                            
050340     END-IF                                                               
052800     .                                                                    
052900     EJECT                                                                
053000 CA-LAS-WDE4ESEQ SECTION.                                                 
053100                                                                          
053200     MOVE NEJ TO SW-TRAEFF                                                
053300     PERFORM IMS-GU-WDE401-ESEQ                                           
053400     IF SEGMENT-FINNS                                                     
053596       PERFORM IMS-GNP-WDE411-E                                           
053597       PERFORM UNTIL SEGMENT-SAKNAS OR SW-TRAEFF = JA                     
053598          IF ORAD-IDPURAD = W-IDPURAD                                     
053599              MOVE JA TO SW-TRAEFF                                        
053600              IF WS-ANTAL = JA AND WS-LINE = JA                           
053601                 IF WS-KVANTAL < ORAD-KVAVBART                            
053602                 OR WS-KVANTAL = ORAD-KVAVBART                            
053603                    CONTINUE                                              
053604                 ELSE                                                     
053605                    MOVE MFS-NUM-FAELT-FEL                                
053606                               TO MOD-KVANTAL                             
053607                    MOVE NEJ TO INDATA-SW                                 
053608                 END-IF                                                   
053609              END-IF                                                      
053610          END-IF                                                          
053611          PERFORM IMS-GNP-WDE411-E                                        
053612       END-PERFORM                                                        
055500                                                                          
055600       IF SW-TRAEFF = NEJ                                                 
055700          IF WS-LINE = JA                                                 
055800             MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-ATTR                   
055810             MOVE NEJ TO INDATA-SW                                        
055820          ELSE                                                            
055830             IF WS-INTERVALL = JA                                         
055840                IF W-IDPURAD = WS-IDRADNR-MIN                             
055850                   MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-MIN-ATTR         
055860                   MOVE NEJ TO INDATA-SW                                  
055870                ELSE                                                      
055880                   IF W-IDPURAD = WS-IDRADNR-MAX                          
055890                      MOVE MFS-NUM-FAELT-FEL                              
055891                           TO MOD-IDRADNR-MAX-ATTR                        
055892                      MOVE NEJ TO INDATA-SW                               
055893                   END-IF                                                 
055894                END-IF                                                    
055895             END-IF                                                       
055896          END-IF                                                          
055897       END-IF                                                             
055898     END-IF                                                               
055899     .                                                                    
055900     EJECT                                                                
055910 CB-LAS-WDE4ASEQ SECTION.                                                 
056000                                                                          
056100     MOVE WS-IDDISTR       TO W-IDDISTR-MIN                               
056200                              W-IDDISTR-MAX                               
056300     MOVE WS-IDKUNDNR      TO W-IDKUNDNR-MIN                              
056400                              W-IDKUNDNR-MAX                              
056410     MOVE SPACE            TO W-IDKUNDRF-MIN                              
056420                              W-IDKUNDRF-MAX                              
056500     MOVE WS-IDORDNR7(3:5) TO W-IDKUNDRF-MIN                              
056600                              W-IDKUNDRF-MAX                              
056700     MOVE NEJ TO SW-TRAEFF                                                
056800                                                                          
056810     PERFORM IMS-GU-WDE401-ASEQ                                           
056820     PERFORM UNTIL SEGMENT-SAKNAS OR SW-TRAEFF = JA                       
056830        IF KORD-IDDC = MSGI-IDDC                                          
056840           MOVE JA TO SW-TRAEFF                                           
056841           MOVE KORD-IDPRODNR TO WS-SPAR-IDPRODNR                         
056844           MOVE WS-SPAR-IDPRODNR TO W-IDPRODNR                            
056845           MOVE NEJ TO SW-TRAEFF                                          
056846           PERFORM IMS-GNP-WDE411-A                                       
056847           PERFORM UNTIL SEGMENT-SAKNAS OR SW-TRAEFF = JA                 
056848              IF ORAD-IDPURAD = W-IDPURAD                                 
056849                  MOVE JA TO SW-TRAEFF                                    
056850                  IF WS-ANTAL = JA AND WS-LINE = JA                       
056851                     IF WS-KVANTAL < ORAD-KVAVBART                        
056852                     OR WS-KVANTAL = ORAD-KVAVBART                        
056853                        CONTINUE                                          
056854                     ELSE                                                 
056855                        MOVE MFS-NUM-FAELT-FEL TO MOD-KVANTAL-ATTR        
056856                        MOVE NEJ TO INDATA-SW                             
056857                     END-IF                                               
056858                  END-IF                                                  
056859              END-IF                                                      
056860              PERFORM IMS-GNP-WDE411-A                                    
056861           END-PERFORM                                                    
056862        END-IF                                                            
056863        PERFORM IMS-GN-WDE401-ASEQ                                        
056870     END-PERFORM                                                          
057901                                                                          
057910     IF SW-TRAEFF = NEJ                                                   
057920        IF WS-LINE = JA                                                   
057930           MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-ATTR                     
057940           MOVE NEJ TO INDATA-SW                                          
057950        ELSE                                                              
057960           IF WS-INTERVALL = JA                                           
057970              IF W-IDPURAD = WS-IDRADNR-MIN                               
057980                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDRADNR-MIN-ATTR           
057990                 MOVE NEJ TO INDATA-SW                                    
057991              ELSE                                                        
057992                 IF W-IDPURAD = WS-IDRADNR-MAX                            
057993                    MOVE MFS-NUM-FAELT-FEL                                
057994                         TO MOD-IDRADNR-MAX-ATTR                          
057995                    MOVE NEJ TO INDATA-SW                                 
057996                 END-IF                                                   
057997              END-IF                                                      
057999           END-IF                                                         
058000        END-IF                                                            
058001     END-IF                                                               
058010     .                                                                    
058100     EJECT                                                                
058200 CC-HAMTA-PRODNR SECTION.                                                 
058201                                                                          
058202     MOVE WS-IDDISTR       TO W-IDDISTR-MIN                               
058203                              W-IDDISTR-MAX                               
058204     MOVE WS-IDKUNDNR      TO W-IDKUNDNR-MIN                              
058205                              W-IDKUNDNR-MAX                              
058206     MOVE SPACE            TO W-IDKUNDRF-MIN                              
058207                              W-IDKUNDRF-MAX                              
058208     MOVE WS-IDORDNR7(3:5) TO W-IDKUNDRF-MIN                              
058209                              W-IDKUNDRF-MAX                              
058210     MOVE NEJ TO SW-TRAEFF                                                
058211                                                                          
058212     PERFORM IMS-GU-WDE401-ASEQ                                           
058213     PERFORM UNTIL SEGMENT-SAKNAS OR SW-TRAEFF = JA                       
058214        IF KORD-IDDC = MSGI-IDDC                                          
058215           MOVE JA TO SW-TRAEFF                                           
058216           MOVE KORD-IDPRODNR TO WS-SPAR-IDPRODNR                         
058217        END-IF                                                            
058218        PERFORM IMS-GN-WDE401-ASEQ                                        
058219     END-PERFORM                                                          
058220     .                                                                    
058221     EJECT                                                                
058230 F-LAES SECTION.                                                          
058300                                                                          
058400     IF PRODNR-NYCKEL-SW = JA                                             
058500        MOVE WS-IDPRODNR TO W-IDPRODNR                                    
058600        PERFORM IMS-GU-WDE401-ESEQ                                        
058700        IF SEGMENT-FINNS                                                  
058800           MOVE ORDER-FINNS TO MOD-TEMFSFEL                               
058900           PERFORM MFS-RENSA-FAELT-IN                                     
059200        ELSE                                                              
059210           MOVE ORDER-SAKNAS TO MED-IDMFSFEL                              
059220           CALL WMEDKONV USING MED-WMEDAREA                               
059230           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
059240           PERFORM MFS-RENSA-FAELT-IN                                     
059250        END-IF                                                            
059300     ELSE                                                                 
059400        MOVE WS-IDDISTR       TO W-IDDISTR-MIN                            
059500                                 W-IDDISTR-MAX                            
059600        MOVE WS-IDKUNDNR      TO W-IDKUNDNR-MIN                           
059700                                 W-IDKUNDNR-MAX                           
059710        MOVE SPACE            TO W-IDKUNDRF-MIN                           
059720                                 W-IDKUNDRF-MAX                           
059730        MOVE WS-IDORDNR7(3:5) TO W-IDKUNDRF-MIN                           
059900                                 W-IDKUNDRF-MAX                           
060000        MOVE NEJ TO SW-TRAEFF                                             
060100                                                                          
060200        PERFORM IMS-GU-WDE401-ASEQ                                        
060300        PERFORM UNTIL SEGMENT-SAKNAS OR SW-TRAEFF = JA                    
060400           IF KORD-IDDC = MSGI-IDDC                                       
060500              MOVE JA TO SW-TRAEFF                                        
060600           END-IF                                                         
060700           PERFORM IMS-GN-WDE401-ASEQ                                     
060800        END-PERFORM                                                       
060900                                                                          
061000        IF SW-TRAEFF = JA                                                 
061001           MOVE ORDER-FINNS TO MOD-TEMFSFEL                               
061010           PERFORM MFS-RENSA-FAELT-IN                                     
061500        ELSE                                                              
061510           MOVE ORDER-SAKNAS TO MED-IDMFSFEL                              
061520           CALL WMEDKONV USING MED-WMEDAREA                               
061530           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
061540           PERFORM MFS-RENSA-FAELT-IN                                     
061550        END-IF                                                            
061600     END-IF                                                               
061700     .                                                                    
061800     EJECT                                                                
061801 G-STARTA-SOP SECTION.                                                    
061802                                                                          
061803     MOVE ZERO TO BC-URV-IDPRODNR                                         
061804                  BC-URV-IDRADNR-MIN                                      
061805                  BC-URV-IDRADNR-MAX                                      
061806                  BC-URV-KVANTAL                                          
061807                                                                          
061808     IF PRODNR-NYCKEL-SW = JA                                             
061809        MOVE WS-IDPRODNR      TO BC-URV-IDPRODNR                          
061810     ELSE                                                                 
061811        MOVE WS-SPAR-IDPRODNR TO BC-URV-IDPRODNR                          
061812     END-IF                                                               
061813     IF WS-LINE = JA                                                      
061814        MOVE WS-IDRADNR       TO BC-URV-IDRADNR-MIN                       
061815                                 BC-URV-IDRADNR-MAX                       
061816        MOVE WS-KVANTAL       TO BC-URV-KVANTAL                           
061817     ELSE                                                                 
061818        IF WS-INTERVALL = JA                                              
061819           MOVE WS-IDRADNR-MIN TO BC-URV-IDRADNR-MIN                      
061820           MOVE WS-IDRADNR-MAX TO BC-URV-IDRADNR-MAX                      
061821           MOVE WS-KVANTAL     TO BC-URV-KVANTAL                          
061822        ELSE                                                              
061823           MOVE ZERO           TO BC-URV-IDRADNR-MIN                      
061824           MOVE 999            TO BC-URV-IDRADNR-MAX                      
061825           MOVE 9999999        TO BC-URV-KVANTAL                          
061826        END-IF                                                            
061827     END-IF                                                               
061828                                                                          
061829     MOVE '4348'   TO MSGSOP-IDTRANS                                      
061830     MOVE '1'      TO MSGSOP-KDMFSFOR                                     
061831     MOVE 'W413S2' TO MSGSOP-IDPROCESS                                    
061832     MOVE 'O'      TO MSGSOP-KDSOPFUNK                                    
061833                                                                          
061834     STRING 'URVAL(' WS-BC ')'                                            
061835             DELIMITED BY SIZE INTO MSGSOP-TESYMBV                        
061836                                                                          
061837     PERFORM IMS-INSERT-ALTMSG-SOP                                        
061838     MOVE INF-PRINT-BEGAERD TO MED-IDMFSINF                               
061840     CALL WMEDKONV USING MED-WMEDAREA                                     
061841     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
061842     PERFORM MFS-RENSA-FAELT-IN                                           
061843     .                                                                    
061844     EJECT                                                                
061849 H-MID-INDATA-TILL-MOD SECTION.                                           
061850                                                                          
061851     IF MID-SELECT = ALL '+'                                              
061852        AND MID-IDRADNR-MIN  = ALL '+'                                    
061853        AND MID-IDRADNR-MAX = ALL '+'                                     
061854        AND MID-IDRADNR = ALL '+'                                         
061855        AND MID-KVANTAL = ALL '+'                                         
061856        AND MID-KVANTAL-INTERVALL = ALL '+'                               
061857          CONTINUE                                                        
061858     ELSE                                                                 
061859        MOVE TRYCK-PF4-FOR-PRINT TO MED-IDMFSFEL                          
061860        CALL WMEDKONV USING MED-WMEDAREA                                  
061861        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
061862                                                                          
061863        IF MID-SELECT = ALL '+'                                           
061864           MOVE MFS-RENSA-FAELT TO MOD-SELECT                             
061865        ELSE                                                              
061866           MOVE MFS-ADD-LAES-IN-FAELT TO MOD-SELECT-ATTR                  
061867           MOVE MFS-ROER-EJ-FAELT TO MOD-SELECT                           
061868        END-IF                                                            
061869        IF MID-IDRADNR = ALL '+'                                          
061870           MOVE MFS-RENSA-FAELT TO MOD-IDRADNR                            
061871        ELSE                                                              
061872           MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDRADNR-ATTR                 
061873           MOVE MFS-ROER-EJ-FAELT TO MOD-IDRADNR                          
061874        END-IF                                                            
061875        IF MID-IDRADNR-MIN = ALL '+'                                      
061876           MOVE MFS-RENSA-FAELT TO MOD-IDRADNR-MIN                        
061877        ELSE                                                              
061878           MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDRADNR-MIN-ATTR             
061879           MOVE MFS-ROER-EJ-FAELT TO MOD-IDRADNR-MIN                      
061880        END-IF                                                            
061881        IF MID-IDRADNR-MAX = ALL '+'                                      
061882           MOVE MFS-RENSA-FAELT TO MOD-IDRADNR-MAX                        
061883        ELSE                                                              
061884           MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDRADNR-MAX-ATTR             
061885           MOVE MFS-ROER-EJ-FAELT TO MOD-IDRADNR-MAX                      
061886        END-IF                                                            
061887        IF MID-KVANTAL = ALL '+'                                          
061888           MOVE MFS-RENSA-FAELT TO MOD-KVANTAL                            
061889        ELSE                                                              
061890           MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVANTAL-ATTR                 
061891           MOVE MFS-ROER-EJ-FAELT TO MOD-KVANTAL                          
061892        END-IF                                                            
061893        IF MID-KVANTAL-INTERVALL = ALL '+'                                
061894           MOVE MFS-RENSA-FAELT TO MOD-KVANTAL-INTERVALL                  
061895        ELSE                                                              
061896           MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVANTAL-ATTR                 
061897           MOVE MFS-ROER-EJ-FAELT TO MOD-KVANTAL                          
061898        END-IF                                                            
061899     END-IF                                                               
061900     .                                                                    
061910     EJECT                                                                
062000 MFS-RENSA-FAELT-IN SECTION.                                              
062100     MOVE MFS-RENSA-FAELT TO MOD-IDRADNR-MIN                              
062200                             MOD-IDRADNR-MAX                              
062300                             MOD-IDRADNR                                  
062400                             MOD-KVANTAL                                  
062401                             MOD-KVANTAL-INTERVALL                        
062410                             MOD-SELECT                                   
062500     .                                                                    
065800     SKIP3                                                                
065810 MFS-ROER-EJ-FAELT-IN SECTION.                                            
065820                                                                          
065830     MOVE MFS-ROER-EJ-FAELT TO MOD-IDRADNR-MIN                            
065840                               MOD-IDRADNR-MAX                            
065850                               MOD-IDRADNR                                
065860                               MOD-KVANTAL                                
065861                               MOD-KVANTAL-INTERVALL                      
065870                               MOD-SELECT                                 
065880     .                                                                    
065890     EJECT                                                                
065900* --- IMS SEKTIONER ---                                                   
066000     SKIP3                                                                
066100 IMS-GET-MSG SECTION.                                                     
066200     MOVE '  QC' TO GODK-STATUSKODER                                      
066300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
066400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
066500     PERFORM IMS-STATUSKONTROLL                                           
066600     .                                                                    
066700     SKIP3                                                                
066800 IMS-INSERT-MSG SECTION.                                                  
066900     IF MSGI-IDLAND-SPR = 'SE'                                            
067000       MOVE '0' TO MFS-KDHUVOMR                                           
067100     END-IF                                                               
067200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
067300     MOVE SPACE TO GODK-STATUSKODER                                       
067400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
067500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
067600     PERFORM IMS-STATUSKONTROLL                                           
067700     .                                                                    
067800     SKIP3                                                                
067810 IMS-INSERT-ALTMSG-SOP SECTION.                                           
067820     MOVE SPACE TO GODK-STATUSKODER                                       
067830     CALL CBLTDLI USING PURG ALT-PCB PROG-TO-PROG-SW                      
067840     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
067850     PERFORM IMS-STATUSKONTROLL                                           
067860     .                                                                    
067870     EJECT                                                                
067900 IMS-GU-WDE401-ASEQ SECTION.                                              
068000     STRING 'WDE401  (WDE4ASEQ>=' W-WDE4ASEQ-MIN-X                        
068100                    '&WDE4ASEQ<=' W-WDE4ASEQ-MAX-X ') '                   
068200          DELIMITED BY SIZE INTO SSA1                                     
068300     MOVE '  GE' TO GODK-STATUSKODER                                      
068400     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-WDE401 SSA1                    
068500     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
068600     PERFORM IMS-STATUSKONTROLL                                           
068700     .                                                                    
068800     SKIP3                                                                
068900 IMS-GN-WDE401-ASEQ SECTION.                                              
069100     STRING 'WDE401  (WDE4ASEQ=>' W-WDE4ASEQ-MIN-X                        
069200                    '&WDE4ASEQ=<' W-WDE4ASEQ-MAX-X ')'                    
069300          DELIMITED BY SIZE INTO SSA1                                     
069400     MOVE '  GE' TO GODK-STATUSKODER                                      
069500     CALL CBLTDLI USING GN WDE4-PCB DLI-IO-WDE401 SSA1                    
069600     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
069700     PERFORM IMS-STATUSKONTROLL                                           
069800     .                                                                    
069900     SKIP3                                                                
070000 IMS-GNP-WDE411-A SECTION.                                                
070100     MOVE 'WDE411 ' TO SSA1                                               
070200     MOVE '  GE' TO GODK-STATUSKODER                                      
070300     CALL CBLTDLI USING GNP WDE4-PCB DLI-IO-WDE411 SSA1                   
070400     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
070500     PERFORM IMS-STATUSKONTROLL                                           
070600     .                                                                    
070700     EJECT                                                                
070710 IMS-GU-WDE401-ESEQ SECTION.                                              
070720     STRING 'WDE401  (WDE4ESEQ =' W-IDPRODNR-X ')'                        
070740          DELIMITED BY SIZE INTO SSA1                                     
070750     MOVE '  GE' TO GODK-STATUSKODER                                      
070760     CALL CBLTDLI USING GU WDE41-PCB DLI-IO-WDE401 SSA1                   
070770     MOVE WDE41-STATUS-CODE TO STATUS-WS                                  
070780     PERFORM IMS-STATUSKONTROLL                                           
070790     .                                                                    
070791     SKIP3                                                                
070800 IMS-GNP-WDE411-E SECTION.                                                
070900     MOVE 'WDE411 ' TO SSA1                                               
071000     MOVE '  GE' TO GODK-STATUSKODER                                      
071100     CALL CBLTDLI USING GNP WDE41-PCB DLI-IO-WDE411 SSA1                  
071200     MOVE WDE41-STATUS-CODE TO STATUS-WS                                  
071300     PERFORM IMS-STATUSKONTROLL                                           
071400     .                                                                    
071500     EJECT                                                                
073100 IMS-STATUSKONTROLL SECTION.                                              
073200     SET STATUS-IX TO 1                                                   
073300     SEARCH GODK-STATUS                                                   
073400       AT END                                                             
073500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
073600         DELIMITED BY SIZE INTO FELTEXT                                   
073700         CALL FELLOG                                                      
073800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
073900         CONTINUE                                                         
074000     END-SEARCH                                                           
074100     .                                                                    
