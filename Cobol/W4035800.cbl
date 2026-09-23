000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4035800.                                                
000400 AUTHOR.         LARS THELL.                                              
000500 DATE-WRITTEN.   91/05/24.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        ÄNDRA BYGGTID PÅ SATSARTIKEL                                     
001100*                                                                         
001200*        PROGRAMMET LÄSER     WLBENA (WDD3)                               
001300*        PROGRAMMET UPPATERAR WLSATB (WDJ1)                               
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W4T358                                              
001700*        MID:         W4I35801                                            
001800*                                                                         
001900*    UTDATA.                                                              
002000*        MOD:         W4O35801                                            
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600 WORKING-STORAGE SECTION.                                                 
002601                                                                          
002602*    -- CHECKED BY WY2000                                                 
002603 77  IDPGM                       PIC X(08)   VALUE 'W4035800'.            
002604                                                                          
002605*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
002606 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
002607                                                                          
002608 77  JA                          PIC X       VALUE 'J'.                   
002609 77  NEJ                         PIC X       VALUE 'N'.                   
002610                                                                          
002620*    --- INDEX FÖR BLÄDDRINGSRADER                                        
002630 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
002640 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +379  COMP SYNC.        
002650*                                                                         
002660 77  W-KVBYGMIN                  PIC S9(5)  VALUE ZERO  COMP-3.           
002670 77  W-TIHH                      PIC  9(2)  VALUE ZERO.                   
002680 77  W-TIMM                      PIC  9(2)  VALUE ZERO.                   
002690                                                                          
002700*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
002800 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
002900                                                                          
003000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
003100     88  INDATA-OK                           VALUE 'J'.                   
003200     88  INDATA-FEL                          VALUE 'N'.                   
003300                                                                          
003400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
003500     88  NYCKLAR-OK                          VALUE 'J'.                   
003600     88  NYCKLAR-FEL                         VALUE 'N'.                   
003700                                                                          
003800 77  ALLT-SW                     PIC X       VALUE 'J'.                   
003900     88  ALLT-OK                             VALUE 'J'.                   
004000                                                                          
004100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
004200     88  EGEN-MID                            VALUE '4358'.                
004300     88  GODK-MID                            VALUE '4358'.                
004400     EJECT                                                                
004500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
004600 01  GENERELLA-SUBPROGRAM.                                                
004700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
004800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
004900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
005100     EJECT                                                                
005200*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
005300*01 -COPY WMSGINIT                                                        
005400     EJECT                                                                
005500*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
005600*01 -COPY WMEDAREA                                                        
005700     SKIP3                                                                
005800 01  MESSAGE-CODES.                                                       
005900     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
006000     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
006100     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
006200     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
006300     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
006400     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
006500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
006600     EJECT                                                                
006700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
006800*                                                                         
006900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
007000     SKIP3                                                                
007100*01  MID -COPY W4I35801                                                   
007200     EJECT                                                                
007300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
007400     SKIP3                                                                
007500*01  -COPY WMSGAREA                                                       
007600     EJECT                                                                
007700     03  MOD REDEFINES MSG-AREA.                                          
007800*      05  -COPY W4O35801                                                 
007900     EJECT                                                                
008000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
008100     SKIP3                                                                
008200*01  -COPY WMFSAREA                                                       
008300     EJECT                                                                
008400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008500*                                                                         
008600     EJECT                                                                
008700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008800     SKIP3                                                                
008900 01  NYCKLAR-TILL-DLI.                                                    
009000     03  W-IDARTNR-X.                                                     
009100         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
009200     SKIP2                                                                
009300     03  W-IDSKYLT-X.                                                     
009400         05  W-IDSKYLT           PIC  X(3)   VALUE SPACE.                 
009500     SKIP2                                                                
009600*    --- STATUS-KOD FRÅN IMS                                              
009700 01  STATUS-WS                   PIC XX.                                  
009800     88  SEGMENT-FINNS                       VALUE '  '.                  
009900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
010000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010100     SKIP2                                                                
010200 01  GODK-STATUSKODER.                                                    
010300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010400     SKIP3                                                                
010500 01  SSA1                        PIC X(64).                               
010600 01  SSA2                        PIC X(64).                               
010700     EJECT                                                                
010800*    --- IMS FUNKTIONSKODER                                               
010900*01  -COPY W0003                                                          
011000     EJECT                                                                
011100*    ---  DLI INPUT-OUTPUT AREA                                           
011200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
011300     SKIP3                                                                
011400 01  DLI-IO-AREA.                                                         
011500     03  IO-AREA                 PIC X(250)  VALUE SPACE.                 
011600     SKIP3                                                                
011700     03  WLSATB01 REDEFINES IO-AREA.                                      
011800*        05  -COPY WDJ101  -PRE SATB01-                                   
011900     EJECT                                                                
012000     03  WLBENA01 REDEFINES IO-AREA.                                      
012100*        05  -COPY WDD311                                                 
012200     EJECT                                                                
012300 LINKAGE SECTION.                                                         
012400                                                                          
012500*01  -COPY W0009   -PRE MSG-                                              
012600     EJECT                                                                
012700*01  -COPY W0008  -PRE USEA-                                              
012800     05  FILLER                  PIC X.                                   
012900     EJECT                                                                
013000*01  -COPY W0008  -PRE SATB-                                              
013100     05  FILLER                  PIC X.                                   
013200     EJECT                                                                
013300*01  -COPY W0008  -PRE BENA-                                              
013400     05  FILLER                  PIC X.                                   
013500     EJECT                                                                
013600 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB                               
013700                                   SATB-PCB BENA-PCB.                     
013800     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
013900                                   SATB-PCB BENA-PCB.                     
014000                                                                          
014100     PERFORM IMS-GET-MSG                                                  
014200     IF SEGMENT-FINNS                                                     
014300       PERFORM A-INIT                                                     
014400       PERFORM B-KOLLA-NYCKLAR                                            
014500       IF NYCKLAR-OK                                                      
014600         IF MFS-UPDATE                                                    
014700           PERFORM G-KOLLA-INPUT                                          
014800           IF INDATA-OK                                                   
014900             PERFORM H-UPPDATERA                                          
015000           END-IF                                                         
015100          ELSE                                                            
015200           PERFORM E-SAMMA-SIDA                                           
015300         END-IF                                                           
015400         IF ALLT-OK AND INDATA-OK                                         
015500           PERFORM F-LAES-VISA-INFO                                       
015600         END-IF                                                           
015700       END-IF                                                             
015800       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
015900       PERFORM IMS-INSERT-MSG                                             
016000     END-IF                                                               
016100                                                                          
016200     MOVE ZERO TO RETURN-CODE                                             
016300     GOBACK                                                               
016400     .                                                                    
016500     EJECT                                                                
016600 A-INIT SECTION.                                                          
016700                                                                          
016800     IF MSG-DUBBLA-TRANSKODER                                             
016900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I35801                 
017000       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
017100       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017200     ELSE                                                                 
017300       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I35801                  
017400       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
017500       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
017600     END-IF                                                               
017700                                                                          
017800     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
017900     MOVE MSG-IDPFK TO MFS-IDPFK                                          
018000     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018100                                                                          
018200     MOVE LOW-VALUE TO MSG-AREA                                           
018300     MOVE 'W4O358N1' TO MFS-IDMOD                                         
018400     MOVE '4358' TO MOD-IDTRANS                                           
018500     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
018600                                                                          
018700     IF NOT EGEN-MID                                                      
018800       MOVE SPACE TO MFS-KDTRTYP                                          
018900       MOVE '7' TO MFS-IDPFK                                              
019000     END-IF                                                               
020200     .                                                                    
020300     EJECT                                                                
020400 B-KOLLA-NYCKLAR SECTION.                                                 
020500                                                                          
020600     MOVE JA TO NYCKLAR-SW                                                
020700                                                                          
020800*    -- KONTROLL AV IDARTNR                                               
020900     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
021000                                                                          
021100     MOVE ALL '+'           TO MSGI-WMSGINIT                              
021200     MOVE '001'             TO MSGI-KDCALL                                
021300     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
021400     MOVE '4358'            TO MSGI-IDTRANS                               
021500     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
021600                                                                          
021700     IF MFS-IDTRANS = '4358'                                              
021800     OR MID-IDARTNR-IN NUMERIC                                            
021900        MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                               
022000     END-IF                                                               
022100                                                                          
022200     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
022210                                                                          
022220     IF MSGI-IDLAND-SPR = 'GB'                                            
022230        MOVE +2    TO SPRAK-IX                                            
022240        MOVE 'GB ' TO MED-IDSKYLT                                         
022250                      W-IDSKYLT                                           
022260     ELSE                                                                 
022270        MOVE +1    TO SPRAK-IX                                            
022280        MOVE 'S  ' TO MED-IDSKYLT                                         
022290                      W-IDSKYLT                                           
022291     END-IF                                                               
022300                                                                          
022400     MOVE MSGI-IDARTNR   TO WS-IDARTNR                                    
022500     INSPECT WS-IDARTNR  REPLACING LEADING SPACE BY ZERO                  
022600     MOVE WS-IDARTNR     TO MOD-IDARTNR-UT                                
022700     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
022800                                                                          
022900     IF MID-IDARTNR-IN         = ALL '+'                                  
023000       CONTINUE                                                           
023100     ELSE                                                                 
023200       MOVE '7'                TO MFS-IDPFK                               
023300       MOVE SPACE              TO MFS-KDTRTYP                             
023400     END-IF                                                               
023500                                                                          
023600     IF WS-IDARTNR NUMERIC AND  WS-IDARTNR > ZERO                         
023700         MOVE WS-IDARTNR       TO W-IDARTNR                               
023800      ELSE                                                                
023900         MOVE NEJ              TO NYCKLAR-SW                              
024000     END-IF                                                               
024100                                                                          
024200     IF NYCKLAR-FEL                                                       
024300       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
024400       CALL WMEDKONV USING MED-WMEDAREA                                   
024500       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
024600       PERFORM MFS-RENSA-FAELT-IN                                         
024700       PERFORM MFS-RENSA-FAELT-UT                                         
024800     END-IF                                                               
024900     .                                                                    
025000     EJECT                                                                
025100 E-SAMMA-SIDA     SECTION.                                                
025200                                                                          
025300     IF MFS-IDTRANS NOT = '4358'                                          
025400     OR MID-INPUT              =  ALL '+'                                 
025500         MOVE JA               TO ALLT-SW                                 
025600     ELSE                                                                 
025700         MOVE NEJ              TO ALLT-SW                                 
025800         MOVE INF-PRESS-PF11   TO MED-IDMFSINF                            
025900         CALL WMEDKONV USING MED-WMEDAREA                                 
026000         MOVE MED-MFSINF       TO MOD-TEMFSINF                            
026100         PERFORM MFS-ROER-EJ-FAELT-IN                                     
026200         PERFORM MFS-ROER-EJ-FAELT-UT                                     
026300         PERFORM MFS-LAES-IN-IGEN                                         
026400     END-IF                                                               
026500     .                                                                    
026600     EJECT                                                                
026700 F-LAES-VISA-INFO SECTION.                                                
026800                                                                          
026900     PERFORM IMS-GU-SATB01                                                
027000                                                                          
027100     IF SEGMENT-SAKNAS                                                    
027200         MOVE '005'            TO MED-IDMFSFEL                            
027300         CALL WMEDKONV USING MED-WMEDAREA                                 
027400         MOVE MED-MFSFEL       TO MOD-TEMFSFEL                            
027500         PERFORM MFS-RENSA-FAELT-UT                                       
027600      ELSE                                                                
027700         DIVIDE 60 INTO SATB01-STR-KVBYGMIN GIVING                        
027800                        MOD-TIHH-RAD  REMAINDER                           
027900                        MOD-TIMM-RAD                                      
028000         PERFORM IMS-GU-BENA11                                            
028100         IF SEGMENT-FINNS                                                 
028200             MOVE TEXT-BEART   TO MOD-BEART-RAD                           
028300          ELSE                                                            
028400             MOVE MFS-RENSA-FAELT TO MOD-BEART-RAD                        
028500         END-IF                                                           
028600     END-IF                                                               
028700     PERFORM MFS-FORM-ATTR                                                
028800     PERFORM MFS-RENSA-FAELT-IN                                           
028900     .                                                                    
029000     EJECT                                                                
029100 G-KOLLA-INPUT SECTION.                                                   
029200                                                                          
029300     MOVE JA  TO INDATA-SW                                                
029400     IF MID-INPUT = ALL '+'                                               
029500         MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                        
029600         CALL WMEDKONV USING MED-WMEDAREA                                 
029700         MOVE MED-MFSFEL           TO MOD-TEMFSFEL                        
029800         PERFORM MFS-ROER-EJ-FAELT-IN                                     
029900         PERFORM MFS-ROER-EJ-FAELT-UT                                     
030000         MOVE NEJ                  TO INDATA-SW                           
030100       ELSE                                                               
030200         IF MID-TIHH-UPD       = ALL '+'                                  
030300             MOVE ZERO         TO MID-TIHH-UPD                            
030400         END-IF                                                           
030500                                                                          
030600         IF MID-TIMM-UPD       = ALL '+'                                  
030700             MOVE ZERO         TO MID-TIMM-UPD                            
030800         END-IF                                                           
030900                                                                          
031000         IF MID-TIHH-UPD NUMERIC AND                                      
031100            MID-TIHH-UPD NUMERIC                                          
031200             MOVE MID-TIHH-UPD TO W-TIHH                                  
031300             MOVE MID-TIMM-UPD TO W-TIMM                                  
031400             COMPUTE W-KVBYGMIN = (W-TIHH * 60) + W-TIMM                  
031500             IF W-KVBYGMIN     > 999                                      
031600                 MOVE MFS-NUM-FAELT-FEL TO MOD-TIHH-ATTR                  
031700                                           MOD-TIMM-ATTR                  
031800                 MOVE NEJ           TO INDATA-SW                          
031900              ELSE                                                        
032000                 MOVE MFS-NUM-FAELT-RAETT TO MOD-TIHH-ATTR                
032100                                             MOD-TIMM-ATTR                
032200             END-IF                                                       
032300          ELSE                                                            
032400             MOVE MFS-NUM-FAELT-FEL TO MOD-TIHH-ATTR                      
032500                                       MOD-TIMM-ATTR                      
032600             MOVE NEJ               TO INDATA-SW                          
032700         END-IF                                                           
032800                                                                          
032900         IF INDATA-FEL                                                    
033000             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
033100             CALL WMEDKONV USING MED-WMEDAREA                             
033200             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
033300             PERFORM MFS-ROER-EJ-FAELT-UT                                 
033400             PERFORM MFS-ROER-EJ-FAELT-IN                                 
033500         END-IF                                                           
033600     END-IF                                                               
033700     .                                                                    
033800     EJECT                                                                
033900 H-UPPDATERA SECTION.                                                     
034000                                                                          
034100     PERFORM IMS-GHU-SATB01                                               
034200     IF SEGMENT-FINNS                                                     
034300         MOVE W-KVBYGMIN            TO SATB01-STR-KVBYGMIN                
034400         PERFORM IMS-REPL-SATB                                            
034500                                                                          
034600         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TIHH-RAD-ATTR                  
034700                                       MOD-TIMM-RAD-ATTR                  
034800         MOVE INF-UPDATE-DONE TO MED-IDMFSINF                             
034900         CALL WMEDKONV USING MED-WMEDAREA                                 
035000         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
035100         PERFORM MFS-FORM-ATTR                                            
035200         PERFORM MFS-RENSA-FAELT-IN                                       
035300      ELSE                                                                
035400         MOVE '005'            TO MED-IDMFSFEL                            
035500         CALL WMEDKONV USING MED-WMEDAREA                                 
035600         MOVE MED-MFSFEL       TO MOD-TEMFSFEL                            
035700         PERFORM MFS-RENSA-FAELT-UT                                       
035800         PERFORM MFS-RENSA-FAELT-IN                                       
035900     END-IF                                                               
036000     .                                                                    
036100     EJECT                                                                
036200 MFS-RENSA-FAELT-UT SECTION.                                              
036300                                                                          
036400     MOVE MFS-RENSA-FAELT      TO MOD-TIHH-UPD                            
036500                                  MOD-TIMM-UPD                            
036600     .                                                                    
036700     SKIP2                                                                
036800 MFS-RENSA-FAELT-IN SECTION.                                              
036900                                                                          
037000*    MOVE MFS-RENSA-FAELT      TO MOD-IDARTNR-IN                          
037100     MOVE MFS-RENSA-FAELT      TO MOD-TIHH-UPD                            
037200                                  MOD-TIMM-UPD                            
037300     .                                                                    
037400     SKIP2                                                                
037500 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
037600                                                                          
037700     MOVE MFS-ROER-EJ-FAELT    TO MOD-TIHH-RAD                            
037800                                  MOD-TIMM-RAD                            
037900     .                                                                    
038000     SKIP2                                                                
038100 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
038200                                                                          
038300*    --- ALLA INDATA-FÄLT                                                 
038400     MOVE MFS-ROER-EJ-FAELT    TO MOD-TIHH-UPD                            
038500                                  MOD-TIMM-UPD                            
038600     .                                                                    
038700     SKIP2                                                                
038800 MFS-FORM-ATTR SECTION.                                                   
038900                                                                          
039000     MOVE MFS-FORMATETS-ATTR   TO MOD-TIHH-ATTR                           
039100                                  MOD-TIMM-ATTR                           
039200     .                                                                    
039300     SKIP2                                                                
039400 MFS-LAES-IN-IGEN   SECTION.                                              
039500                                                                          
039600     MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-TIHH-ATTR                         
039700                                    MOD-TIMM-ATTR                         
039800     .                                                                    
039900     EJECT                                                                
040000* --- IMS SEKTIONER ---                                                   
040100     SKIP3                                                                
040200 IMS-GET-MSG SECTION.                                                     
040300                                                                          
040400     MOVE '  QC' TO GODK-STATUSKODER                                      
040500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
040600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
040700     PERFORM IMS-STATUSKONTROLL                                           
040800     .                                                                    
040900     SKIP3                                                                
041000 IMS-INSERT-MSG SECTION.                                                  
041100                                                                          
041110     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
041300       MOVE '0' TO MFS-KDHUVOMR                                           
041400     END-IF                                                               
041500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
041600     MOVE SPACE TO GODK-STATUSKODER                                       
041700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
041800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
041900     PERFORM IMS-STATUSKONTROLL                                           
042000     .                                                                    
042100     EJECT                                                                
042200 IMS-GU-SATB01 SECTION.                                                   
042300     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-X ')'                         
042400          DELIMITED BY SIZE INTO SSA1                                     
042500     MOVE '  GE' TO GODK-STATUSKODER                                      
042600     CALL CBLTDLI USING GU SATB-PCB DLI-IO-AREA SSA1                      
042700     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
042800     PERFORM IMS-STATUSKONTROLL                                           
042900     .                                                                    
043000     SKIP3                                                                
043100 IMS-GU-BENA11 SECTION.                                                   
043200     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
043300          DELIMITED BY SIZE INTO SSA1                                     
043400     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
043500          DELIMITED BY SIZE INTO SSA2                                     
043600     MOVE '  GE' TO GODK-STATUSKODER                                      
043700     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1 SSA2                 
043800     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
043900     PERFORM IMS-STATUSKONTROLL                                           
044000     .                                                                    
044100     SKIP3                                                                
044200 IMS-GHU-SATB01 SECTION.                                                  
044300     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-X ')'                         
044400          DELIMITED BY SIZE INTO SSA1                                     
044500     MOVE '  GE' TO GODK-STATUSKODER                                      
044600     CALL CBLTDLI USING GHU SATB-PCB DLI-IO-AREA SSA1                     
044700     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
044800     PERFORM IMS-STATUSKONTROLL                                           
044900     .                                                                    
045000     SKIP3                                                                
045100 IMS-REPL-SATB SECTION.                                                   
045200                                                                          
045300     MOVE '  ' TO GODK-STATUSKODER                                        
045400     CALL CBLTDLI USING REPL SATB-PCB DLI-IO-AREA                         
045500     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
045600     PERFORM IMS-STATUSKONTROLL                                           
045700     .                                                                    
045800     EJECT                                                                
045900 IMS-STATUSKONTROLL SECTION.                                              
046000                                                                          
046100     SET STATUS-IX TO 1                                                   
046200     SEARCH GODK-STATUS                                                   
046300       AT END                                                             
046400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
046500         DELIMITED BY SIZE INTO FELTEXT                                   
046600         CALL FELLOG                                                      
046700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
046800     END-SEARCH                                                           
046900     .                                                                    
