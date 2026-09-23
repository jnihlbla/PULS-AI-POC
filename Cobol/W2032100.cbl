000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2032100.                                                
000400 AUTHOR.         ANN JORDEBO.                                             
000500 DATE-WRITTEN.   90/11/05.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        VIA DEN HÄR BILDEN KAN ANSKAFFARE GÖRA ETT ELLER FLERA           
001100*        URVAL AV TPO:ER. URVALEN LÄGGS UPP PÅ WDM4 OCH LÄSES             
001200*        NER NÄSTFÖLJANDE NATTKÖRNING. EN KONTROLL SKER ATT RÄTT          
001300*        KOMBINATION HAR ANGIVITS.                                        
001400*                                                                         
001500*        PROGRAMMET UPPDATERAR WLURVA (WDM4)                              
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSAKTION: W2T321                                              
001810*                     W2T321U                                             
001900*        MID:         W2I32101                                            
002000*                                                                         
002100*    UTDATA.                                                              
002200*        MOD:         W2O32101                                            
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800 WORKING-STORAGE SECTION.                                                 
002801*    -COPY WY2000W1                                                       
002802     SKIP3                                                                
002803*    -COPY WY2000W3                                                       
002810     SKIP3                                                                
002900 77  IDPGM                       PIC X(08)   VALUE 'W2032100'.            
003000                                                                          
003100*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003200 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003300                                                                          
003400 77  JA                          PIC X       VALUE 'J'.                   
003500 77  NEJ                         PIC X       VALUE 'N'.                   
003600                                                                          
003700*    --- INDEX FÖR BLÄDDRINGSRADER                                        
003800 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
003900 77  INDX-PLUS-1                 PIC S9(4)  VALUE +0    COMP SYNC.        
004000 77  MAX-INDX                    PIC S9(4)  VALUE +42   COMP SYNC.        
004100 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004200 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +705  COMP SYNC.        
004300                                                                          
004400*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004500 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004600     88  INDATA-OK                           VALUE 'J'.                   
004700     88  INDATA-FEL                          VALUE 'N'.                   
004800                                                                          
004900 77  ALLT-SW                     PIC X       VALUE 'J'.                   
005000     88  ALLT-OK                             VALUE 'J'.                   
005100                                                                          
005200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005300     88  EGEN-MID                            VALUE '2321'.                
005400     88  GODK-MID                            VALUE '2321' '2322'          
005500                                                   '2323' '2324'          
005600                                                   '2325' '2326'          
005700                                                   '2327'.                
005800                                                                          
005900     EJECT                                                                
006000*    --- ARBETSFÄLT                                                       
006100 01  ARBETSFAELT.                                                         
006200     03  WS-IDUSER               PIC X(8)    VALUE SPACE.                 
006300     03  WS-TIREGDAT             PIC 9(6)    VALUE ZERO.                  
006400     03  WS-TIREGTID-TOT.                                                 
006500       05  WS-TIREGTID           PIC 9(6)    VALUE ZERO.                  
006600       05  FILLER                PIC 9(2).                                
006700     03  WS-INNEV-VECKA.                                                  
006800       05  INNEVARANDE-VECKA     PIC 9(4)    VALUE ZERO.                  
006900       05  FILLER                PIC 9(1).                                
007000     03  WS-IDANSK-FOM           PIC S9(3)   VALUE ZERO.                  
007100     03  WS-IDANSK-TOM           PIC S9(3)   VALUE ZERO.                  
007200     03  WS-IDDISTR-FOM          PIC S9(5)   VALUE ZERO.                  
007300     03  WS-IDDISTR-TOM          PIC S9(5)   VALUE ZERO.                  
007400     03  WS-IDLEVNR              PIC X(5)    VALUE SPACE.                 
007500     03  WS-KDBASLM-FOM          PIC X(6)    VALUE SPACE.                 
007600     03  WS-KDBASLM-TOM          PIC X(6)    VALUE SPACE.                 
007700     03  WS-KDPRODSL             PIC S9(3)   VALUE ZERO.                  
007800     03  WS-KDSORT1              PIC S9      VALUE ZERO.                  
007900     03  WS-KDTPOTYP-FOM         PIC S9      VALUE ZERO.                  
008000     03  WS-KDTPOTYP-TOM         PIC S9      VALUE ZERO.                  
008100     03  WS-TITPO-FOM            PIC S9(7)   VALUE ZERO.                  
008200     03  WS-TITPO-TOM            PIC S9(7)   VALUE ZERO.                  
008300     03  WS-IDARTNR              PIC S9(9)   VALUE ZERO.                  
008400 01  SWITCHAR.                                                            
008500     03  UPPDAT-FLER-ART-SW      PIC X       VALUE 'N'.                   
008600       88   UPPDAT-FLER-ART                  VALUE 'J'.                   
008700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008800 01  GENERELLA-SUBPROGRAM.                                                
008900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
009000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
009300     EJECT                                                                
009400*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
009500*   -COPY WMEDAREA                                                        
009600     SKIP3                                                                
009700 01  MESSAGE-CODES.                                                       
009800     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
009900     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
010000     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
010100     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
010200     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
010300     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
010400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010500     EJECT                                                                
010600 01  ANDRA-MEDDELANDE.                                                    
010700     03  MED-1                   PIC X(33)   VALUE                        
010800         'FYLL I ANSKAFFARE ELLER ARTIKELNR'.                             
010900     03  MED-2                   PIC X(17)   VALUE                        
011000         'URVAL EJ RELEVANT'.                                             
011100     03  MED-3                   PIC X(38)   VALUE                        
011200         'MARKNAD OBLIGATORISKT VID VAL AV TPO 3'.                        
011300     03  MED-4                   PIC X(23)   VALUE                        
011400         'FYLL I RESTERANDE URVAL'.                                       
011500     03  MED-5                   PIC X(18)   VALUE                        
011600         'FEL I SPARADE FÄLT'.                                            
011700     03  MED-6                   PIC X(27)   VALUE                        
011800         'ARTIKELNR SKA ANGES I FÖLJD'.                                   
011900*01  -COPY WDATAREA                                                       
012000     EJECT                                                                
012100*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
012200*                                                                         
012300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
012400     SKIP3                                                                
012500*01  MID -COPY W2I32101                                                   
012600     EJECT                                                                
012700 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
012800     SKIP3                                                                
012900*01  -COPY WMSGAREA                                                       
013000     EJECT                                                                
013100     03  MOD REDEFINES MSG-AREA.                                          
013200*      05  -COPY W2O32101                                                 
013300     EJECT                                                                
013400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
013500     SKIP3                                                                
013600*01  -COPY WMFSAREA                                                       
013700     EJECT                                                                
013800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013900*                                                                         
014000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014100     SKIP3                                                                
014200 01  NYCKLAR-TILL-DLI.                                                    
014300     03  W-WDM401KY-X.                                                    
014400         05  W-IDUSER            PIC X(8)    VALUE SPACE.                 
014500         05  W-TIREGDAT          PIC S9(7)   VALUE ZERO COMP-3.           
014600         05  W-TIREGTID          PIC S9(7)   VALUE ZERO COMP-3.           
014700     03  W-KDSEGKEY-X.                                                    
014800         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
015100     SKIP2                                                                
015200*    --- STATUS-KOD FRÅN IMS                                              
015300 01  STATUS-WS                   PIC XX.                                  
015400     88  SEGMENT-FINNS                       VALUE '  '.                  
015500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
015600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
015700     SKIP2                                                                
015800 01  GODK-STATUSKODER.                                                    
015900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016000     SKIP3                                                                
016100 01  SSA1                        PIC X(64).                               
016200 01  SSA2                        PIC X(64).                               
016300     EJECT                                                                
016400*    --- IMS FUNKTIONSKODER                                               
016500*01  -COPY W0003                                                          
016600     EJECT                                                                
016700*    ---  DLI INPUT-OUTPUT AREA                                           
016800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
016900     SKIP3                                                                
017000 01  DLI-IO-AREA.                                                         
017100     03  IO-AREA                 PIC X(50)  VALUE SPACE.                  
017200     SKIP3                                                                
017300     03  WLURVA01 REDEFINES IO-AREA.                                      
017400*        05  -COPY WDM401     -PRE URVA-                                  
017500     SKIP3                                                                
017600     03  WLURVA11 REDEFINES IO-AREA.                                      
017700*        05  -COPY WDM411     -PRE URVA-                                  
017800     SKIP3                                                                
017900     03  WLURVA12 REDEFINES IO-AREA.                                      
018000*        05  -COPY WDM412     -PRE URVA-                                  
018100     EJECT                                                                
018200 LINKAGE SECTION.                                                         
018300                                                                          
018400*01  -COPY W0009      -PRE MSG-                                           
018500     EJECT                                                                
018600*01  -COPY W0008      -PRE URVA-                                          
018700     05  FILLER                  PIC X.                                   
018800     EJECT                                                                
018900 PROCEDURE DIVISION  USING MSG-PCB URVA-PCB.                              
019000     ENTRY 'DLITCBL' USING MSG-PCB URVA-PCB.                              
019100                                                                          
019200     PERFORM IMS-GET-MSG                                                  
019300     IF SEGMENT-FINNS                                                     
019400       PERFORM A-INIT                                                     
019500       IF MFS-UPDATE                                                      
019600         PERFORM C-KOLLA-INPUT                                            
019700         IF INDATA-OK                                                     
019800           PERFORM D-UPPDATERA                                            
019900         END-IF                                                           
020000       ELSE                                                               
020100         IF  MID-INPUT = ALL '+'                                          
020200         AND MID-IDARTNR-GRP = ALL '+'                                    
020300           PERFORM MFS-RENSA-FAELT-BILD                                   
020400         ELSE                                                             
020500           PERFORM B-SAMMA-SIDA                                           
020600         END-IF                                                           
020700       END-IF                                                             
020800       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
020900       PERFORM IMS-INSERT-MSG                                             
021000     END-IF                                                               
021100                                                                          
021200     MOVE ZERO TO RETURN-CODE                                             
021300     GOBACK                                                               
021400     .                                                                    
021500     EJECT                                                                
021600 A-INIT SECTION.                                                          
021700                                                                          
021800     IF MSG-DUBBLA-TRANSKODER                                             
021900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I32101                 
022000       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
022100       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
022200     ELSE                                                                 
022300       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I32101                  
022400       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
022500       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
022600     END-IF                                                               
022700                                                                          
022800     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
022900     MOVE MSG-IDPFK TO MFS-IDPFK                                          
023000     MOVE MFS-IDTRANS TO W-IDTRANS                                        
023100                                                                          
023200     MOVE LOW-VALUE TO MSG-AREA                                           
023300     MOVE 'W2O32101' TO MFS-IDMOD                                         
023400     MOVE '2321' TO MOD-IDTRANS                                           
023500     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
023600                                                                          
023700     IF NOT EGEN-MID                                                      
023800       MOVE SPACE TO MFS-KDTRTYP                                          
023900       MOVE '7' TO MFS-IDPFK                                              
024000       MOVE ALL '+' TO MID-INPUT                                          
024100                       MID-IDARTNR-GRP                                    
024200     END-IF                                                               
024300                                                                          
024400     IF ENGLISH-TEXT                                                      
024500       MOVE +2 TO SPRAK-IX                                                
024600       MOVE 'GB ' TO MED-IDSKYLT                                          
024700     ELSE                                                                 
024800       MOVE +1 TO SPRAK-IX                                                
024900       MOVE 'S  ' TO MED-IDSKYLT                                          
025000     END-IF                                                               
025100                                                                          
025200     MOVE MSG-LTERM-NAME TO WS-IDUSER                                     
025300     ACCEPT WS-TIREGDAT     FROM DATE                                     
025400     ACCEPT WS-TIREGTID-TOT FROM TIME                                     
025500                                                                          
025600     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
025700     MOVE WS-TIREGDAT TO DAT-I-TIDATUM                                    
025800     CALL WDATKONV USING DAT-KDDATFORM                                    
025900                         DAT-I-TIDATUM                                    
026000                         DAT-O-TIDATUM                                    
026100                         DAT-KDSVAR                                       
026200     MOVE DAT-TIAAVVD TO WS-INNEV-VECKA                                   
026300     .                                                                    
026400     EJECT                                                                
026500 B-SAMMA-SIDA SECTION.                                                    
026600                                                                          
026700     MOVE INF-PRESS-PF11 TO MED-IDMFSINF                                  
026800     CALL WMEDKONV USING MED-WMEDAREA                                     
026900     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
027000                                                                          
027100     MOVE MFS-ROER-EJ-FAELT TO MOD-IDUSER-SPAR                            
027200                               MOD-TIREGDAT-SPAR                          
027300                               MOD-TIREGTID-SPAR                          
027400     PERFORM BA-TA-HAND-OM-INFAELT                                        
027500     PERFORM BB-TA-HAND-OM-ARTNR                                          
027600     PERFORM MFS-ROER-EJ-FAELT-UT                                         
027700     .                                                                    
027800     EJECT                                                                
027900 BA-TA-HAND-OM-INFAELT SECTION.                                           
028000                                                                          
028100     IF MID-IDANSK-FOM = ALL '+'                                          
028200       MOVE MFS-RENSA-FAELT       TO MOD-IDANSK-FOM-IN                    
028300     ELSE                                                                 
028400       MOVE MFS-ROER-EJ-FAELT     TO MOD-IDANSK-FOM-IN                    
028500       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDANSK-FOM-IN-ATTR               
028600     END-IF                                                               
028700     IF MID-IDANSK-TOM = ALL '+'                                          
028800       MOVE MFS-RENSA-FAELT       TO MOD-IDANSK-TOM-IN                    
028900     ELSE                                                                 
029000       MOVE MFS-ROER-EJ-FAELT     TO MOD-IDANSK-TOM-IN                    
029100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDANSK-TOM-IN-ATTR               
029200     END-IF                                                               
029300     IF MID-KDSORT1 = ALL '+'                                             
029400       MOVE MFS-RENSA-FAELT       TO MOD-KDSORT1-IN                       
029500     ELSE                                                                 
029600       MOVE MFS-ROER-EJ-FAELT     TO MOD-KDSORT1-IN                       
029700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDSORT1-IN-ATTR                  
029800     END-IF                                                               
029900     IF MID-IDLEVNR = ALL '+'                                             
030000       MOVE MFS-RENSA-FAELT       TO MOD-IDLEVNR-IN                       
030100     ELSE                                                                 
030200       MOVE MFS-ROER-EJ-FAELT     TO MOD-IDLEVNR-IN                       
030300       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLEVNR-IN-ATTR                  
030400     END-IF                                                               
030500     IF MID-KDPRODSL = ALL '+'                                            
030600       MOVE MFS-RENSA-FAELT       TO MOD-KDPRODSL-IN                      
030700     ELSE                                                                 
030800       MOVE MFS-ROER-EJ-FAELT     TO MOD-KDPRODSL-IN                      
030900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDPRODSL-IN-ATTR                 
031000     END-IF                                                               
031100     IF MID-IDDISTR-FOM = ALL '+'                                         
031200       MOVE MFS-RENSA-FAELT       TO MOD-IDDISTR-FOM-IN                   
031300     ELSE                                                                 
031400       MOVE MFS-ROER-EJ-FAELT     TO MOD-IDDISTR-FOM-IN                   
031500       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDDISTR-FOM-IN-ATTR              
031600     END-IF                                                               
031700     IF MID-IDDISTR-TOM = ALL '+'                                         
031800       MOVE MFS-RENSA-FAELT       TO MOD-IDDISTR-TOM-IN                   
031900     ELSE                                                                 
032000       MOVE MFS-ROER-EJ-FAELT     TO MOD-IDDISTR-TOM-IN                   
032100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDDISTR-TOM-IN-ATTR              
032200     END-IF                                                               
032300     IF MID-KDBASLM-FOM = ALL '+'                                         
032400       MOVE MFS-RENSA-FAELT       TO MOD-KDBASLM-FOM-IN                   
032500     ELSE                                                                 
032600       MOVE MFS-ROER-EJ-FAELT     TO MOD-KDBASLM-FOM-IN                   
032700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDBASLM-FOM-IN-ATTR              
032800     END-IF                                                               
032900     IF MID-KDBASLM-TOM = ALL '+'                                         
033000       MOVE MFS-RENSA-FAELT       TO MOD-KDBASLM-TOM-IN                   
033100     ELSE                                                                 
033200       MOVE MFS-ROER-EJ-FAELT     TO MOD-KDBASLM-TOM-IN                   
033300       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDBASLM-TOM-IN-ATTR              
033400     END-IF                                                               
033500     IF MID-KDTPOTYP-FOM = ALL '+'                                        
033600       MOVE MFS-RENSA-FAELT       TO MOD-KDTPOTYP-FOM-IN                  
033700     ELSE                                                                 
033800       MOVE MFS-ROER-EJ-FAELT     TO MOD-KDTPOTYP-FOM-IN                  
033900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDTPOTYP-FOM-IN-ATTR             
034000     END-IF                                                               
034100     IF MID-KDTPOTYP-TOM = ALL '+'                                        
034200       MOVE MFS-RENSA-FAELT       TO MOD-KDTPOTYP-TOM-IN                  
034300     ELSE                                                                 
034400       MOVE MFS-ROER-EJ-FAELT     TO MOD-KDTPOTYP-TOM-IN                  
034500       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDTPOTYP-TOM-IN-ATTR             
034600     END-IF                                                               
034700     IF MID-TITPO-FOM = ALL '+'                                           
034800       MOVE MFS-RENSA-FAELT       TO MOD-TITPO-FOM-IN                     
034900     ELSE                                                                 
035000       MOVE MFS-ROER-EJ-FAELT     TO MOD-TITPO-FOM-IN                     
035100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TITPO-FOM-IN-ATTR                
035200     END-IF                                                               
035300     IF MID-TITPO-TOM = ALL '+'                                           
035400       MOVE MFS-RENSA-FAELT       TO MOD-TITPO-TOM-IN                     
035500     ELSE                                                                 
035600       MOVE MFS-ROER-EJ-FAELT     TO MOD-TITPO-TOM-IN                     
035700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TITPO-TOM-IN-ATTR                
035800     END-IF                                                               
035900     .                                                                    
036000     EJECT                                                                
036100 BB-TA-HAND-OM-ARTNR SECTION.                                             
036200                                                                          
036300     MOVE +1 TO INDX                                                      
036400     PERFORM UNTIL INDX > MAX-INDX                                        
036500       IF MID-IDARTNR(INDX) = ALL '+'                                     
036600         MOVE MFS-RENSA-FAELT       TO MOD-IDARTNR(INDX)                  
036700       ELSE                                                               
036800         MOVE MFS-ROER-EJ-FAELT     TO MOD-IDARTNR(INDX)                  
036900         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDARTNR-ATTR(INDX)             
037000       END-IF                                                             
037100       ADD +1 TO INDX                                                     
037200     END-PERFORM                                                          
037300     .                                                                    
037400     EJECT                                                                
037500 C-KOLLA-INPUT SECTION.                                                   
037600                                                                          
037700     MOVE JA  TO INDATA-SW                                                
037800     IF  MID-INPUT = ALL '+'                                              
037900     AND MID-IDARTNR-GRP = ALL '+'                                        
038000       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
038100       CALL WMEDKONV USING MED-WMEDAREA                                   
038200       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
038300       PERFORM MFS-ROER-EJ-FAELT-BILD                                     
038400       MOVE NEJ TO INDATA-SW                                              
038500     ELSE                                                                 
038510       MOVE NEJ TO UPPDAT-FLER-ART-SW                                     
038600       IF  MID-IDARTNR-GRP NOT = ALL '+'                                  
038601       AND MID-IDUSER-SPAR NOT = SPACE                                    
038602         MOVE JA TO UPPDAT-FLER-ART-SW                                    
038900       END-IF                                                             
039000       PERFORM CA-NUMERIC-KONTROLL                                        
039100       PERFORM CB-FOM-TOM-OCH-OBLIG-KONTROLL                              
039200       PERFORM CC-RELATIONS-KONTROLL                                      
039300       IF INDATA-FEL                                                      
039400         PERFORM MFS-ROER-EJ-FAELT-BILD                                   
039500         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
039600         CALL WMEDKONV USING MED-WMEDAREA                                 
039700         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
039800       END-IF                                                             
039900     END-IF                                                               
040000     .                                                                    
040100     EJECT                                                                
040200 CA-NUMERIC-KONTROLL SECTION.                                             
040300                                                                          
040400     IF MID-IDANSK-FOM = ALL '+'                                          
040500       CONTINUE                                                           
040600     ELSE                                                                 
040700       IF MID-IDANSK-FOM NUMERIC                                          
040800         MOVE MID-IDANSK-FOM TO WS-IDANSK-FOM                             
040900         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDANSK-FOM-IN-ATTR               
041000       ELSE                                                               
041100         MOVE NEJ TO INDATA-SW                                            
041200         MOVE MFS-NUM-FAELT-FEL   TO MOD-IDANSK-FOM-IN-ATTR               
041300       END-IF                                                             
041400     END-IF                                                               
041500     IF MID-IDANSK-TOM = ALL '+'                                          
041600       CONTINUE                                                           
041700     ELSE                                                                 
041800       IF MID-IDANSK-TOM NUMERIC                                          
041900         MOVE MID-IDANSK-TOM TO WS-IDANSK-TOM                             
042000         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDANSK-TOM-IN-ATTR               
042100       ELSE                                                               
042200         MOVE NEJ TO INDATA-SW                                            
042300         MOVE MFS-NUM-FAELT-FEL   TO MOD-IDANSK-TOM-IN-ATTR               
042400       END-IF                                                             
042500     END-IF                                                               
042600                                                                          
042700     IF MID-KDSORT1 = ALL '+'                                             
042800       CONTINUE                                                           
042900     ELSE                                                                 
043000       IF  MID-KDSORT1 NUMERIC                                            
043100         MOVE MFS-NUM-FAELT-RAETT TO MOD-KDSORT1-IN-ATTR                  
043200         MOVE MID-KDSORT1 TO WS-KDSORT1                                   
043300         IF WS-KDSORT1 > 0 AND < 6                                        
043400           CONTINUE                                                       
043500         ELSE                                                             
043600           MOVE NEJ TO INDATA-SW                                          
043700           MOVE MFS-NUM-FAELT-FEL TO MOD-KDSORT1-IN-ATTR                  
043800         END-IF                                                           
043900       ELSE                                                               
044000         MOVE NEJ TO INDATA-SW                                            
044100         MOVE MFS-NUM-FAELT-FEL   TO MOD-KDSORT1-IN-ATTR                  
044200       END-IF                                                             
044300     END-IF                                                               
044400                                                                          
044500     IF MID-IDLEVNR = ALL '+'                                             
044600       CONTINUE                                                           
044700     ELSE                                                                 
044900         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVNR-IN-ATTR                 
045000         MOVE MID-IDLEVNR TO WS-IDLEVNR                                   
045500     END-IF                                                               
045600                                                                          
045700     IF MID-KDPRODSL = ALL '+'                                            
045800       CONTINUE                                                           
045900     ELSE                                                                 
046000       IF MID-KDPRODSL NUMERIC                                            
046100         MOVE MFS-NUM-FAELT-RAETT TO MOD-KDPRODSL-IN-ATTR                 
046200         MOVE MID-KDPRODSL TO WS-KDPRODSL                                 
046300       ELSE                                                               
046400         MOVE NEJ TO INDATA-SW                                            
046500         MOVE MFS-NUM-FAELT-FEL   TO MOD-KDPRODSL-IN-ATTR                 
046600       END-IF                                                             
046700     END-IF                                                               
046800                                                                          
046900     IF MID-IDDISTR-FOM = ALL '+'                                         
047000       CONTINUE                                                           
047100     ELSE                                                                 
047200       IF MID-IDDISTR-FOM NUMERIC                                         
047300         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDISTR-FOM-IN-ATTR              
047400         MOVE MID-IDDISTR-FOM TO WS-IDDISTR-FOM                           
047500       ELSE                                                               
047600         MOVE NEJ TO INDATA-SW                                            
047700         MOVE MFS-NUM-FAELT-FEL   TO MOD-IDDISTR-FOM-IN-ATTR              
047800       END-IF                                                             
047900     END-IF                                                               
048000     IF MID-IDDISTR-TOM = ALL '+'                                         
048100       CONTINUE                                                           
048200     ELSE                                                                 
048300       IF MID-IDDISTR-TOM NUMERIC                                         
048400         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDISTR-TOM-IN-ATTR              
048500         MOVE MID-IDDISTR-TOM TO WS-IDDISTR-TOM                           
048600       ELSE                                                               
048700         MOVE NEJ TO INDATA-SW                                            
048800         MOVE MFS-NUM-FAELT-FEL   TO MOD-IDDISTR-TOM-IN-ATTR              
048900       END-IF                                                             
049000     END-IF                                                               
049100                                                                          
049200     IF MID-KDBASLM-FOM = ALL '+'                                         
049300       CONTINUE                                                           
049400     ELSE                                                                 
049500       MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDBASLM-FOM-IN-ATTR               
049600       MOVE MID-KDBASLM-FOM TO WS-KDBASLM-FOM                             
049700     END-IF                                                               
049800     IF MID-KDBASLM-TOM = ALL '+'                                         
049900       MOVE WS-KDBASLM-FOM TO WS-KDBASLM-TOM                              
050000     ELSE                                                                 
050100       MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDBASLM-TOM-IN-ATTR               
050200       MOVE MID-KDBASLM-TOM TO WS-KDBASLM-TOM                             
050300     END-IF                                                               
050400                                                                          
050500     IF MID-KDTPOTYP-FOM = ALL '+'                                        
050600       CONTINUE                                                           
050700     ELSE                                                                 
050800       IF  MID-KDTPOTYP-FOM NUMERIC                                       
050900         MOVE MFS-NUM-FAELT-RAETT TO MOD-KDTPOTYP-FOM-IN-ATTR             
051000         MOVE MID-KDTPOTYP-FOM TO WS-KDTPOTYP-FOM                         
051100         IF WS-KDTPOTYP-FOM > 0 AND < 7                                   
051200           CONTINUE                                                       
051300         ELSE                                                             
051400           MOVE NEJ TO INDATA-SW                                          
051500           MOVE MFS-NUM-FAELT-FEL TO MOD-KDTPOTYP-FOM-IN-ATTR             
051600         END-IF                                                           
051700       ELSE                                                               
051800         MOVE NEJ TO INDATA-SW                                            
051900         MOVE MFS-NUM-FAELT-FEL   TO MOD-KDTPOTYP-FOM-IN-ATTR             
052000       END-IF                                                             
052100     END-IF                                                               
052200     IF MID-KDTPOTYP-TOM = ALL '+'                                        
052300       CONTINUE                                                           
052400     ELSE                                                                 
052500       IF  MID-KDTPOTYP-TOM NUMERIC                                       
052600         MOVE MFS-NUM-FAELT-RAETT TO MOD-KDTPOTYP-TOM-IN-ATTR             
052700         MOVE MID-KDTPOTYP-TOM TO WS-KDTPOTYP-TOM                         
052800         IF WS-KDTPOTYP-TOM > 0 AND < 7                                   
052900           CONTINUE                                                       
053000         ELSE                                                             
053100           MOVE NEJ TO INDATA-SW                                          
053200           MOVE MFS-NUM-FAELT-FEL TO MOD-KDTPOTYP-TOM-IN-ATTR             
053300         END-IF                                                           
053400       ELSE                                                               
053500         MOVE NEJ TO INDATA-SW                                            
053600         MOVE MFS-NUM-FAELT-FEL   TO MOD-KDTPOTYP-TOM-IN-ATTR             
053700       END-IF                                                             
053800     END-IF                                                               
053900                                                                          
054000     IF MID-TITPO-FOM = ALL '+'                                           
054100       CONTINUE                                                           
054200     ELSE                                                                 
054300       IF MID-TITPO-FOM NUMERIC                                           
054400         MOVE MID-TITPO-FOM TO DAT-I-TIDATUM                              
054401         MOVE DAT-I-TIDATUM       TO TMP1-YYWW                            
054402         MOVE INNEVARANDE-VECKA   TO TMP2-YYWW                            
054403         PERFORM WY2000P3                                                 
054500         IF TMP1-YYWW >= TMP2-YYWW                                        
054600           MOVE 'AAVV  ' TO DAT-KDDATFORM                                 
054700           CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                
054800                               DAT-O-TIDATUM DAT-KDSVAR                   
054900           IF  DAT-KDSVAR-OK                                              
055000             MOVE MFS-NUM-FAELT-RAETT TO MOD-TITPO-FOM-IN-ATTR            
055100             MOVE DAT-TIAAMMDD  TO WS-TITPO-FOM                           
055200           ELSE                                                           
055300             MOVE NEJ TO INDATA-SW                                        
055400             MOVE MFS-NUM-FAELT-FEL  TO MOD-TITPO-FOM-IN-ATTR             
055500           END-IF                                                         
055600         ELSE                                                             
055700           MOVE NEJ TO INDATA-SW                                          
055800           MOVE MFS-NUM-FAELT-FEL TO MOD-TITPO-FOM-IN-ATTR                
055900         END-IF                                                           
056000       ELSE                                                               
056100         MOVE NEJ TO INDATA-SW                                            
056200         MOVE MFS-NUM-FAELT-FEL   TO MOD-TITPO-FOM-IN-ATTR                
056300       END-IF                                                             
056400     END-IF                                                               
056500     IF MID-TITPO-TOM = ALL '+'                                           
056600       CONTINUE                                                           
056700     ELSE                                                                 
056800       IF MID-TITPO-TOM NUMERIC                                           
056900         MOVE MID-TITPO-TOM TO DAT-I-TIDATUM                              
057000         MOVE 'AAVV  ' TO DAT-KDDATFORM                                   
057100         CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                  
057200                             DAT-O-TIDATUM DAT-KDSVAR                     
057300         IF  DAT-KDSVAR-OK                                                
057400           MOVE MFS-NUM-FAELT-RAETT TO MOD-TITPO-TOM-IN-ATTR              
057500           MOVE DAT-TIAAMMDD  TO WS-TITPO-TOM                             
057600         ELSE                                                             
057700           MOVE NEJ TO INDATA-SW                                          
057800           MOVE MFS-NUM-FAELT-FEL  TO MOD-TITPO-TOM-IN-ATTR               
057900         END-IF                                                           
058000       ELSE                                                               
058100         MOVE NEJ TO INDATA-SW                                            
058200         MOVE MFS-NUM-FAELT-FEL   TO MOD-TITPO-TOM-IN-ATTR                
058300       END-IF                                                             
058400     END-IF                                                               
058500                                                                          
058600     IF MID-IDARTNR-GRP NOT = ALL '+'                                     
058700       MOVE +1 TO INDX                                                    
058800       PERFORM UNTIL INDX > MAX-INDX                                      
058900         IF MID-IDARTNR(INDX) = ALL '+'                                   
059000           CONTINUE                                                       
059100         ELSE                                                             
059200           IF MID-IDARTNR(INDX) NUMERIC                                   
059300             MOVE MFS-NUM-FAELT-RAETT TO MOD-IDARTNR-ATTR(INDX)           
059400           ELSE                                                           
059500             MOVE NEJ TO INDATA-SW                                        
059600             MOVE MFS-NUM-FAELT-FEL   TO MOD-IDARTNR-ATTR(INDX)           
059700           END-IF                                                         
059800         END-IF                                                           
059900         ADD +1 TO INDX                                                   
060000       END-PERFORM                                                        
060100     END-IF                                                               
060200     .                                                                    
060300     EJECT                                                                
060400 CB-FOM-TOM-OCH-OBLIG-KONTROLL SECTION.                                   
060500                                                                          
060600     IF NOT UPPDAT-FLER-ART                                               
060700       IF MID-IDANSK-FOM = ALL '+'                                        
060800         IF MID-IDARTNR-GRP = ALL '+'                                     
060900         OR MID-IDANSK-TOM NOT = ALL '+'                                  
061000           MOVE NEJ TO INDATA-SW                                          
061100           MOVE MFS-NUM-FAELT-FEL TO MOD-IDANSK-FOM-IN-ATTR               
061200         END-IF                                                           
061300       ELSE                                                               
061400         IF MID-IDANSK-TOM = ALL '+'                                      
061500           MOVE MID-IDANSK-FOM TO MID-IDANSK-TOM                          
061600                                  WS-IDANSK-TOM                           
061700         ELSE                                                             
061800           IF WS-IDANSK-FOM > WS-IDANSK-TOM                               
061900             MOVE NEJ TO INDATA-SW                                        
062000             MOVE MFS-NUM-FAELT-FEL TO MOD-IDANSK-TOM-IN-ATTR             
062100           END-IF                                                         
062200         END-IF                                                           
062300       END-IF                                                             
062400                                                                          
062500       IF MID-KDSORT1 = ALL '+'                                           
062600         MOVE NEJ TO INDATA-SW                                            
062700         MOVE MFS-NUM-FAELT-FEL TO MOD-KDSORT1-IN-ATTR                    
062800       END-IF                                                             
062900                                                                          
063000       IF MID-IDDISTR-FOM = ALL '+'                                       
063100         IF MID-IDDISTR-TOM = ALL '+'                                     
063200           CONTINUE                                                       
063300         ELSE                                                             
063400           MOVE NEJ TO INDATA-SW                                          
063500           MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-FOM-IN-ATTR              
063600         END-IF                                                           
063700       ELSE                                                               
063800         IF MID-IDDISTR-TOM = ALL '+'                                     
063900           MOVE MID-IDDISTR-FOM TO MID-IDDISTR-TOM                        
064000           MOVE WS-IDDISTR-FOM TO WS-IDDISTR-TOM                          
064100         ELSE                                                             
064200           IF WS-IDDISTR-FOM > MID-IDDISTR-TOM                            
064300             MOVE NEJ TO INDATA-SW                                        
064400             MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-TOM-IN-ATTR            
064500           END-IF                                                         
064600         END-IF                                                           
064700       END-IF                                                             
064800                                                                          
064900       IF MID-KDBASLM-FOM = ALL '+'                                       
065000         IF MID-KDBASLM-TOM = ALL '+'                                     
065100           CONTINUE                                                       
065200         ELSE                                                             
065300           MOVE NEJ TO INDATA-SW                                          
065400           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDBASLM-FOM-IN-ATTR             
065500         END-IF                                                           
065600       ELSE                                                               
065700         IF MID-KDBASLM-TOM = ALL '+'                                     
065800           MOVE MID-KDBASLM-FOM TO MID-KDBASLM-TOM                        
065900         ELSE                                                             
066000           IF MID-KDBASLM-FOM > MID-KDBASLM-TOM                           
066100             MOVE NEJ TO INDATA-SW                                        
066200             MOVE MFS-ALFA-FAELT-FEL TO MOD-KDBASLM-TOM-IN-ATTR           
066300           END-IF                                                         
066400         END-IF                                                           
066500       END-IF                                                             
066600                                                                          
066700       IF MID-KDTPOTYP-FOM = ALL '+'                                      
066800         IF MID-KDTPOTYP-TOM = ALL '+'                                    
066900           CONTINUE                                                       
067000         ELSE                                                             
067100           MOVE NEJ TO INDATA-SW                                          
067200           MOVE MFS-NUM-FAELT-FEL TO MOD-KDTPOTYP-FOM-IN-ATTR             
067300         END-IF                                                           
067400       ELSE                                                               
067500         IF MID-KDTPOTYP-TOM = ALL '+'                                    
067600           MOVE WS-KDTPOTYP-FOM TO MID-KDTPOTYP-TOM                       
067700                                   WS-KDTPOTYP-TOM                        
067800         ELSE                                                             
067900           IF WS-KDTPOTYP-FOM > WS-KDTPOTYP-TOM                           
068000             MOVE NEJ TO INDATA-SW                                        
068100             MOVE MFS-NUM-FAELT-FEL TO MOD-KDTPOTYP-FOM-IN-ATTR           
068200           END-IF                                                         
068300         END-IF                                                           
068400       END-IF                                                             
068500                                                                          
068600       IF MID-TITPO-FOM = ALL '+'                                         
068700         MOVE NEJ TO INDATA-SW                                            
068800         MOVE MFS-NUM-FAELT-FEL TO MOD-TITPO-FOM-IN-ATTR                  
068900       ELSE                                                               
069000         IF MID-TITPO-TOM = ALL '+'                                       
069100           MOVE MID-TITPO-FOM TO MID-TITPO-TOM                            
069200           MOVE WS-TITPO-FOM TO WS-TITPO-TOM                              
069300         ELSE                                                             
069301           MOVE WS-TITPO-FOM   TO TMP1-YYMMDD                             
069302           MOVE WS-TITPO-TOM   TO TMP2-YYMMDD                             
069310           PERFORM WY2000P1                                               
069400           IF TMP1-YYMMDD > TMP2-YYMMDD                                   
069500             MOVE NEJ TO INDATA-SW                                        
069600             MOVE MFS-NUM-FAELT-FEL TO MOD-TITPO-TOM-IN-ATTR              
069700           END-IF                                                         
069800         END-IF                                                           
069900       END-IF                                                             
070000     END-IF                                                               
070100     .                                                                    
070200     EJECT                                                                
070300 CC-RELATIONS-KONTROLL SECTION.                                           
070400                                                                          
070500     IF MID-IDARTNR-GRP = ALL '+'                                         
070600       IF MID-INPUT = ALL '+'                                             
070700         CONTINUE                                                         
070800       ELSE                                                               
070900         IF  MID-IDLEVNR = ALL '+'                                        
071000         AND MID-KDPRODSL = ALL '+'                                       
071100         AND MID-IDDISTR-FOM = ALL '+'                                    
071200         AND MID-KDTPOTYP-FOM = ALL '+'                                   
071300           MOVE NEJ TO INDATA-SW                                          
071310           MOVE MFS-ALFA-FAELT-FEL TO  MOD-IDLEVNR-IN-ATTR                
071500           MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-IN-ATTR                 
071600                                     MOD-IDDISTR-FOM-IN-ATTR              
071700                                     MOD-KDTPOTYP-FOM-IN-ATTR             
071800         ELSE                                                             
071810           PERFORM CCA-KOLLA-TPO-MOT-MARKN-DISTR                          
073400         END-IF                                                           
073500       END-IF                                                             
073600     ELSE                                                                 
073610       IF NOT UPPDAT-FLER-ART                                             
073700         IF MID-IDANSK-FOM NOT = ALL '+'                                  
073800           MOVE NEJ TO INDATA-SW                                          
073900           MOVE MFS-NUM-FAELT-FEL TO  MOD-IDANSK-FOM-IN-ATTR              
074000         END-IF                                                           
074100         IF MID-IDLEVNR NOT = ALL '+'                                     
074200           MOVE NEJ TO INDATA-SW                                          
074300           MOVE MFS-ALFA-FAELT-FEL TO  MOD-IDLEVNR-IN-ATTR                
074400         END-IF                                                           
074500         IF MID-KDPRODSL NOT = ALL '+'                                    
074600           MOVE NEJ TO INDATA-SW                                          
074700           MOVE MFS-NUM-FAELT-FEL TO  MOD-KDPRODSL-IN-ATTR                
074800         END-IF                                                           
074810         PERFORM CCA-KOLLA-TPO-MOT-MARKN-DISTR                            
074830       END-IF                                                             
074900       MOVE +1 TO INDX                                                    
075000       MOVE +2 TO INDX-PLUS-1                                             
075100       PERFORM UNTIL INDX > MAX-INDX                                      
075200         IF MID-IDARTNR(INDX) = ALL '+'                                   
075300           IF INDX < MAX-INDX                                             
075400             IF MID-IDARTNR(INDX-PLUS-1) NOT = ALL '+'                    
075500               MOVE MFS-NUM-FAELT-FEL TO                                  
075600                               MOD-IDARTNR-ATTR(INDX-PLUS-1)              
075700               MOVE NEJ TO INDATA-SW                                      
075800               ADD +1 TO INDX INDX-PLUS-1                                 
075900               MOVE MED-6 TO MOD-TEMFSINF                                 
076000             END-IF                                                       
076100           END-IF                                                         
076200         ELSE                                                             
076300           IF MID-IDARTNR(INDX) NUMERIC                                   
076400             MOVE MFS-NUM-FAELT-RAETT TO                                  
076500                                       MOD-IDARTNR-ATTR(INDX)             
076600           ELSE                                                           
076700             MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-ATTR(INDX)             
076800             MOVE NEJ TO INDATA-SW                                        
076900           END-IF                                                         
077000         END-IF                                                           
077100         ADD +1 TO INDX INDX-PLUS-1                                       
077200       END-PERFORM                                                        
077300     END-IF                                                               
077400     .                                                                    
077500     EJECT                                                                
077600 CCA-KOLLA-TPO-MOT-MARKN-DISTR SECTION.                                   
077610                                                                          
077700     IF MID-KDTPOTYP-FOM NOT = ALL '+'                                    
077800       IF WS-KDTPOTYP-FOM = 3                                             
077900         IF MID-KDBASLM-FOM = ALL '+'                                     
078000           MOVE NEJ TO INDATA-SW                                          
078100           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDBASLM-FOM-IN-ATTR             
078300         END-IF                                                           
078400         IF MID-IDDISTR-FOM NOT = ALL '+'                                 
078500           MOVE NEJ TO INDATA-SW                                          
078600           MOVE MFS-NUM-FAELT-FEL  TO MOD-IDDISTR-FOM-IN-ATTR             
078800         END-IF                                                           
078900       ELSE                                                               
079000         IF MID-KDBASLM-FOM NOT = ALL '+'                                 
079100           MOVE NEJ TO INDATA-SW                                          
079200           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDBASLM-FOM-IN-ATTR             
079400         END-IF                                                           
079500       END-IF                                                             
079600     END-IF                                                               
079610                                                                          
079700     IF  MID-IDDISTR-FOM NOT = ALL '+'                                    
079800     AND MID-KDBASLM-FOM NOT = ALL '+'                                    
079900       MOVE NEJ TO INDATA-SW                                              
080000       MOVE MFS-NUM-FAELT-FEL      TO MOD-IDDISTR-FOM-IN-ATTR             
080100       MOVE MFS-ALFA-FAELT-FEL     TO MOD-KDBASLM-FOM-IN-ATTR             
080200     END-IF                                                               
080300     .                                                                    
080400     EJECT                                                                
087500 D-UPPDATERA SECTION.                                                     
087600                                                                          
087700     IF MID-INPUT = ALL '+'                                               
087800       MOVE MID-IDUSER-SPAR   TO W-IDUSER                                 
087900                                 MOD-IDUSER-SPAR                          
088000       MOVE MID-TIREGDAT-SPAR TO W-TIREGDAT                               
088100                                 MOD-TIREGDAT-SPAR                        
088200       MOVE MID-TIREGTID-SPAR TO W-TIREGTID                               
088300                                 MOD-TIREGTID-SPAR                        
088400       PERFORM IMS-GET-URVA-USER                                          
088500       MOVE +1 TO INDX                                                    
088600       PERFORM UNTIL MID-IDARTNR(INDX) = ALL '+' OR                       
088700                     INDX > MAX-INDX                                      
088800         MOVE MID-IDARTNR(INDX) TO URVA-ART-IDARTNR                       
088900         PERFORM IMS-ISRT-URVA-ART                                        
089000         ADD +1 TO INDX                                                   
089100       END-PERFORM                                                        
089200     ELSE                                                                 
089300       MOVE WS-IDUSER   TO URVA-USER-IDUSER                               
089400                           MOD-IDUSER-SPAR                                
089500                           W-IDUSER                                       
089600       MOVE WS-TIREGDAT TO URVA-USER-TIREGDAT                             
089700                           MOD-TIREGDAT-SPAR                              
089800                           W-TIREGDAT                                     
089900       MOVE WS-TIREGTID TO URVA-USER-TIREGTID                             
090000                           MOD-TIREGTID-SPAR                              
090100                           W-TIREGTID                                     
090200       MOVE SPACE       TO URVA-USER-IDURVAL                              
090300       MOVE '2321'      TO URVA-USER-IDTRANS                              
090400       MOVE NEJ         TO URVA-USER-FLLISTA                              
090500       PERFORM IMS-ISRT-URVA-USER                                         
090600       MOVE '1'              TO URVA-URV1-KDSEGKEY                        
090700       MOVE WS-IDANSK-FOM    TO URVA-URV1-IDANSK-FOM                      
090800       MOVE WS-IDANSK-TOM    TO URVA-URV1-IDANSK-TOM                      
090900       MOVE WS-IDDISTR-FOM   TO URVA-URV1-IDDISTR-FOM                     
091000       MOVE WS-IDDISTR-TOM   TO URVA-URV1-IDDISTR-TOM                     
091100       MOVE WS-IDLEVNR       TO URVA-URV1-IDLEVNR                         
091200       MOVE WS-KDBASLM-FOM   TO URVA-URV1-KDBASLM-FOM                     
091300       MOVE WS-KDBASLM-TOM   TO URVA-URV1-KDBASLM-TOM                     
091400       MOVE WS-KDPRODSL      TO URVA-URV1-KDPRODSL                        
091500       MOVE WS-KDSORT1       TO URVA-URV1-KDSORT1                         
091600       MOVE WS-KDTPOTYP-FOM  TO URVA-URV1-KDTPOTYP-FOM                    
091700       MOVE WS-KDTPOTYP-TOM  TO URVA-URV1-KDTPOTYP-TOM                    
091800       MOVE WS-TITPO-FOM     TO URVA-URV1-TITPO-FOM                       
091900       MOVE WS-TITPO-TOM     TO URVA-URV1-TITPO-TOM                       
092000       PERFORM IMS-ISRT-URVA-URV1                                         
092100       MOVE +1 TO INDX                                                    
092200       PERFORM UNTIL MID-IDARTNR(INDX) = ALL '+' OR                       
092300                     INDX > MAX-INDX                                      
092400         MOVE MID-IDARTNR(INDX) TO WS-IDARTNR                             
092500         MOVE WS-IDARTNR        TO URVA-ART-IDARTNR                       
092600         PERFORM IMS-ISRT-URVA-ART                                        
092700         ADD +1 TO INDX                                                   
092800       END-PERFORM                                                        
092900     END-IF                                                               
093000                                                                          
093100     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
093200     CALL WMEDKONV USING MED-WMEDAREA                                     
093300     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
093400     PERFORM DA-VISA-BILD-EFTER-UPPDAT                                    
093500     .                                                                    
093600     EJECT                                                                
093700 DA-VISA-BILD-EFTER-UPPDAT SECTION.                                       
093800                                                                          
093900     PERFORM MFS-FORM-ATTR                                                
094000     PERFORM MFS-RENSA-FAELT-IN                                           
094100                                                                          
094200     IF MID-INPUT = ALL '+'                                               
094300       PERFORM MFS-ROER-EJ-FAELT-UT                                       
094400     ELSE                                                                 
094500       IF MID-IDANSK-FOM = ALL '+'                                        
094600         MOVE MFS-RENSA-FAELT   TO MOD-IDANSK-FOM-UT                      
094700       ELSE                                                               
094800         MOVE MID-IDANSK-FOM    TO MOD-IDANSK-FOM-UT                      
094900       END-IF                                                             
095000       IF MID-IDANSK-TOM = ALL '+'                                        
095100         MOVE MFS-RENSA-FAELT   TO MOD-IDANSK-TOM-UT                      
095200       ELSE                                                               
095300         MOVE MID-IDANSK-TOM    TO MOD-IDANSK-TOM-UT                      
095400       END-IF                                                             
095500       IF MID-KDSORT1    = ALL '+'                                        
095600         MOVE MFS-RENSA-FAELT   TO MOD-KDSORT1-UT                         
095700       ELSE                                                               
095800         MOVE MID-KDSORT1       TO MOD-KDSORT1-UT                         
095900       END-IF                                                             
096000       IF MID-IDLEVNR    = ALL '+'                                        
096100         MOVE MFS-RENSA-FAELT   TO MOD-IDLEVNR-UT                         
096200       ELSE                                                               
096300         MOVE MID-IDLEVNR       TO MOD-IDLEVNR-UT                         
096400       END-IF                                                             
096500       IF MID-KDPRODSL   = ALL '+'                                        
096600         MOVE MFS-RENSA-FAELT   TO MOD-KDPRODSL-UT                        
096700       ELSE                                                               
096800         MOVE MID-KDPRODSL      TO MOD-KDPRODSL-UT                        
096900       END-IF                                                             
097000       IF MID-IDDISTR-FOM = ALL '+'                                       
097100         MOVE MFS-RENSA-FAELT   TO MOD-IDDISTR-FOM-UT                     
097200       ELSE                                                               
097300         MOVE MID-IDDISTR-FOM   TO MOD-IDDISTR-FOM-UT                     
097400       END-IF                                                             
097500       IF MID-IDDISTR-TOM = ALL '+'                                       
097600         MOVE MFS-RENSA-FAELT   TO MOD-IDDISTR-TOM-UT                     
097700       ELSE                                                               
097800         MOVE MID-IDDISTR-TOM   TO MOD-IDDISTR-TOM-UT                     
097900       END-IF                                                             
098000       IF MID-KDBASLM-FOM = ALL '+'                                       
098100         MOVE MFS-RENSA-FAELT   TO MOD-KDBASLM-FOM-UT                     
098200       ELSE                                                               
098300         MOVE MID-KDBASLM-FOM   TO MOD-KDBASLM-FOM-UT                     
098400       END-IF                                                             
098500       IF MID-KDBASLM-TOM = ALL '+'                                       
098600         MOVE MFS-RENSA-FAELT   TO MOD-KDBASLM-TOM-UT                     
098700       ELSE                                                               
098800         MOVE MID-KDBASLM-TOM   TO MOD-KDBASLM-TOM-UT                     
098900       END-IF                                                             
099000       IF MID-KDTPOTYP-FOM = ALL '+'                                      
099100         MOVE MFS-RENSA-FAELT   TO MOD-KDTPOTYP-FOM-UT                    
099200       ELSE                                                               
099300         MOVE MID-KDTPOTYP-FOM  TO MOD-KDTPOTYP-FOM-UT                    
099400       END-IF                                                             
099500       IF MID-KDTPOTYP-TOM = ALL '+'                                      
099600         MOVE MFS-RENSA-FAELT   TO MOD-KDTPOTYP-TOM-UT                    
099700       ELSE                                                               
099800         MOVE MID-KDTPOTYP-TOM  TO MOD-KDTPOTYP-TOM-UT                    
099900       END-IF                                                             
100000       IF MID-TITPO-FOM  = ALL '+'                                        
100100         MOVE MFS-RENSA-FAELT   TO MOD-TITPO-FOM-UT                       
100200       ELSE                                                               
100300         MOVE MID-TITPO-FOM     TO MOD-TITPO-FOM-UT                       
100400       END-IF                                                             
100500       IF MID-TITPO-TOM  = ALL '+'                                        
100600         MOVE MFS-RENSA-FAELT   TO MOD-TITPO-TOM-UT                       
100700       ELSE                                                               
100800         MOVE MID-TITPO-TOM     TO MOD-TITPO-TOM-UT                       
100900       END-IF                                                             
101000     END-IF                                                               
101100     PERFORM MFS-RENSA-FAELT-ARTNR                                        
101200     .                                                                    
101300     EJECT                                                                
101400 MFS-RENSA-FAELT-BILD SECTION.                                            
101500                                                                          
101600     PERFORM MFS-RENSA-FAELT-SPAR                                         
101700     PERFORM MFS-RENSA-FAELT-IN                                           
101800     PERFORM MFS-RENSA-FAELT-UT                                           
101900     PERFORM MFS-RENSA-FAELT-ARTNR                                        
102000     .                                                                    
102100     SKIP3                                                                
102200 MFS-RENSA-FAELT-ARTNR SECTION.                                           
102300                                                                          
102400     MOVE +1 TO INDX                                                      
102500     PERFORM UNTIL INDX > MAX-INDX                                        
102600       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR(INDX)                          
102700       ADD +1 TO INDX                                                     
102800     END-PERFORM                                                          
102900     .                                                                    
103000     SKIP3                                                                
103100 MFS-RENSA-FAELT-SPAR SECTION.                                            
103200                                                                          
103300     MOVE MFS-RENSA-FAELT TO MOD-IDUSER-SPAR                              
103400                             MOD-TIREGDAT-SPAR                            
103500                             MOD-TIREGTID-SPAR                            
103600     .                                                                    
103700     SKIP3                                                                
103800 MFS-RENSA-FAELT-UT SECTION.                                              
103900                                                                          
104000     MOVE MFS-RENSA-FAELT TO MOD-IDANSK-FOM-UT                            
104100                             MOD-IDANSK-TOM-UT                            
104200                             MOD-KDSORT1-UT                               
104300                             MOD-IDLEVNR-UT                               
104400                             MOD-KDPRODSL-UT                              
104500                             MOD-IDDISTR-FOM-UT                           
104600                             MOD-IDDISTR-TOM-UT                           
104700                             MOD-KDBASLM-FOM-UT                           
104800                             MOD-KDBASLM-TOM-UT                           
104900                             MOD-KDTPOTYP-FOM-UT                          
105000                             MOD-KDTPOTYP-TOM-UT                          
105100                             MOD-TITPO-FOM-UT                             
105200                             MOD-TITPO-TOM-UT                             
105300     .                                                                    
105400     EJECT                                                                
105500 MFS-RENSA-FAELT-IN SECTION.                                              
105600                                                                          
105700     MOVE MFS-RENSA-FAELT TO MOD-IDANSK-FOM-IN                            
105800                             MOD-IDANSK-TOM-IN                            
105900                             MOD-KDSORT1-IN                               
106000                             MOD-IDLEVNR-IN                               
106100                             MOD-KDPRODSL-IN                              
106200                             MOD-IDDISTR-FOM-IN                           
106300                             MOD-IDDISTR-TOM-IN                           
106400                             MOD-KDBASLM-FOM-IN                           
106500                             MOD-KDBASLM-TOM-IN                           
106600                             MOD-KDTPOTYP-FOM-IN                          
106700                             MOD-KDTPOTYP-TOM-IN                          
106800                             MOD-TITPO-FOM-IN                             
106900                             MOD-TITPO-TOM-IN                             
107000     .                                                                    
107100     EJECT                                                                
107200 MFS-ROER-EJ-FAELT-BILD SECTION.                                          
107300                                                                          
107400     PERFORM MFS-ROER-EJ-FAELT-SPAR                                       
107500     PERFORM MFS-ROER-EJ-FAELT-IN                                         
107600     PERFORM MFS-ROER-EJ-FAELT-UT                                         
107700     MOVE +1 TO INDX                                                      
107800     PERFORM UNTIL INDX > MAX-INDX                                        
107900       MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR(INDX)                        
108000       ADD +1 TO INDX                                                     
108100     END-PERFORM                                                          
108200     .                                                                    
108300     SKIP3                                                                
108400 MFS-ROER-EJ-FAELT-SPAR SECTION.                                          
108500                                                                          
108600     MOVE MFS-ROER-EJ-FAELT TO MOD-IDUSER-SPAR                            
108700                               MOD-TIREGDAT-SPAR                          
108800                               MOD-TIREGTID-SPAR                          
108900     .                                                                    
109000     SKIP3                                                                
109100 MFS-ROER-EJ-FAELT-UT SECTION.                                            
109200                                                                          
109300     MOVE MFS-ROER-EJ-FAELT TO MOD-IDANSK-FOM-UT                          
109400                               MOD-IDANSK-TOM-UT                          
109500                               MOD-KDSORT1-UT                             
109600                               MOD-IDLEVNR-UT                             
109700                               MOD-KDPRODSL-UT                            
109800                               MOD-IDDISTR-FOM-UT                         
109900                               MOD-IDDISTR-TOM-UT                         
110000                               MOD-KDBASLM-FOM-UT                         
110100                               MOD-KDBASLM-TOM-UT                         
110200                               MOD-KDTPOTYP-FOM-UT                        
110300                               MOD-KDTPOTYP-TOM-UT                        
110400                               MOD-TITPO-FOM-UT                           
110500                               MOD-TITPO-TOM-UT                           
110600     .                                                                    
110700     EJECT                                                                
110800 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
110900                                                                          
111000     MOVE MFS-ROER-EJ-FAELT TO MOD-IDANSK-FOM-IN                          
111100                               MOD-IDANSK-TOM-IN                          
111200                               MOD-KDSORT1-IN                             
111300                               MOD-IDLEVNR-IN                             
111400                               MOD-KDPRODSL-IN                            
111500                               MOD-IDDISTR-FOM-IN                         
111600                               MOD-IDDISTR-TOM-IN                         
111700                               MOD-KDBASLM-FOM-IN                         
111800                               MOD-KDBASLM-TOM-IN                         
111900                               MOD-KDTPOTYP-FOM-IN                        
112000                               MOD-KDTPOTYP-TOM-IN                        
112100                               MOD-TITPO-FOM-IN                           
112200                               MOD-TITPO-TOM-IN                           
112300     .                                                                    
112400     EJECT                                                                
112500 MFS-FORM-ATTR SECTION.                                                   
112600                                                                          
112700     MOVE MFS-FORMATETS-ATTR TO MOD-IDANSK-FOM-IN-ATTR                    
112800                                MOD-IDANSK-TOM-IN-ATTR                    
112900                                MOD-KDSORT1-IN-ATTR                       
113000                                MOD-IDLEVNR-IN-ATTR                       
113100                                MOD-KDPRODSL-IN-ATTR                      
113200                                MOD-IDDISTR-FOM-IN-ATTR                   
113300                                MOD-IDDISTR-TOM-IN-ATTR                   
113400                                MOD-KDBASLM-FOM-IN-ATTR                   
113500                                MOD-KDBASLM-TOM-IN-ATTR                   
113600                                MOD-KDTPOTYP-FOM-IN-ATTR                  
113700                                MOD-KDTPOTYP-TOM-IN-ATTR                  
113800                                MOD-TITPO-FOM-IN-ATTR                     
113900                                MOD-TITPO-TOM-IN-ATTR                     
114000     MOVE +1 TO INDX                                                      
114100     PERFORM UNTIL INDX > MAX-INDX                                        
114200       MOVE MFS-FORMATETS-ATTR TO MOD-IDARTNR-ATTR(INDX)                  
114300       ADD +1 TO INDX                                                     
114400     END-PERFORM                                                          
114500     .                                                                    
114600     EJECT                                                                
114700* --- IMS SEKTIONER ---                                                   
114800     SKIP3                                                                
114900 IMS-GET-MSG SECTION.                                                     
115000                                                                          
115100     MOVE '  QC' TO GODK-STATUSKODER                                      
115200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
115300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
115400     PERFORM IMS-STATUSKONTROLL                                           
115500     .                                                                    
115600     SKIP3                                                                
115700 IMS-INSERT-MSG SECTION.                                                  
115800                                                                          
115900     IF ENGLISH-TEXT                                                      
116000       MOVE 'N' TO MFS-KDHUVOMR                                           
116100     END-IF                                                               
116200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
116300     MOVE SPACE TO GODK-STATUSKODER                                       
116400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
116500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
116600     PERFORM IMS-STATUSKONTROLL                                           
116700     .                                                                    
116800     EJECT                                                                
116900 IMS-GET-URVA-USER SECTION.                                               
117000                                                                          
117100     STRING 'WLURVA01(WDM401KY =' W-WDM401KY-X ')'                        
117200          DELIMITED BY SIZE INTO SSA1                                     
117300     MOVE '  ' TO GODK-STATUSKODER                                        
117400     CALL CBLTDLI USING GU   URVA-PCB DLI-IO-AREA SSA1                    
117500     MOVE URVA-STATUS-CODE TO STATUS-WS                                   
117600     PERFORM IMS-STATUSKONTROLL                                           
117700     .                                                                    
117800     EJECT                                                                
117900 IMS-ISRT-URVA-USER SECTION.                                              
118000                                                                          
118100     MOVE 'WLURVA01 ' TO SSA1                                             
118200     MOVE '  ' TO GODK-STATUSKODER                                        
118300     CALL CBLTDLI USING ISRT URVA-PCB DLI-IO-AREA SSA1                    
118400     MOVE URVA-STATUS-CODE TO STATUS-WS                                   
118500     PERFORM IMS-STATUSKONTROLL                                           
118600     .                                                                    
118700     EJECT                                                                
118800 IMS-ISRT-URVA-URV1 SECTION.                                              
118900                                                                          
119000     STRING 'WLURVA01(WDM401KY =' W-WDM401KY-X ')'                        
119100          DELIMITED BY SIZE INTO SSA1                                     
119200     MOVE 'WLURVA11 ' TO SSA2                                             
119300     MOVE '  ' TO GODK-STATUSKODER                                        
119400     CALL CBLTDLI USING ISRT URVA-PCB DLI-IO-AREA SSA1 SSA2               
119500     MOVE URVA-STATUS-CODE TO STATUS-WS                                   
119600     PERFORM IMS-STATUSKONTROLL                                           
119700     .                                                                    
119800     EJECT                                                                
119900 IMS-ISRT-URVA-ART SECTION.                                               
120000                                                                          
120100     STRING 'WLURVA01(WDM401KY =' W-WDM401KY-X ')'                        
120200          DELIMITED BY SIZE INTO SSA1                                     
120300     MOVE 'WLURVA12 ' TO SSA2                                             
120400     MOVE '  II' TO GODK-STATUSKODER                                      
120500     CALL CBLTDLI USING ISRT URVA-PCB DLI-IO-AREA SSA1 SSA2               
120600     MOVE URVA-STATUS-CODE TO STATUS-WS                                   
120700     PERFORM IMS-STATUSKONTROLL                                           
120800     .                                                                    
120900     EJECT                                                                
121000 IMS-STATUSKONTROLL SECTION.                                              
121100                                                                          
121200     SET STATUS-IX TO 1                                                   
121300     SEARCH GODK-STATUS                                                   
121400       AT END                                                             
121500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
121600         DELIMITED BY SIZE INTO FELTEXT                                   
121700         CALL FELLOG                                                      
121800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
121900     END-SEARCH                                                           
122000     .                                                                    
122010     EJECT                                                                
122100*    -COPY WY2000P3                                                       
122110     EJECT                                                                
122200*    -COPY WY2000P1                                                       
