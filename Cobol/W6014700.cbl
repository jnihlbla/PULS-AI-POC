000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6014700.                                                
000400*AUTHOR.         ANNELIE ENGLUND.                                         
000500*DATE-WRITTEN.   92/12/03.                                                
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMET ÄR EN STANSBILD FÖR ATT R32-RAPPORTERA                
000900*        INLEVERARADE PARTIER MED AVVIKELSE                               
001000*        PROGRAMMET SKICKAR TRANSAR TILL W6T191X                          
001100*                                        W6T193X VIA DISPATCHER           
001200*                                                                         
001300*        PROGRAMMET UPPDATERAR W6INLA (W6D1)                              
001310*        PROGRAMMET LÄSER      W6UPFA (W6L1)                              
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W6T147                                              
001700*        MID:         W6I14701                                            
001800*                                                                         
001900*    UTDATA.                                                              
002000*        MOD:         W6O14701                                            
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600 WORKING-STORAGE SECTION.                                                 
002601                                                                          
002610*    -- CHECKED BY WY2000                                                 
002700 77  IDPGM                       PIC X(08)   VALUE 'W6014700'.            
002800                                                                          
002900*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003100                                                                          
003200 77  JA                          PIC X       VALUE 'J'.                   
003300 77  NEJ                         PIC X       VALUE 'N'.                   
003400 77  INDX                        PIC S9(2)   VALUE ZERO.                  
003500 77  INDX2                       PIC S9(2)   VALUE ZERO.                  
003600 77  MAX-INDX                    PIC S9(2)   VALUE +13.                   
003700 77  6191-IX                     PIC S9(2)   VALUE ZERO.                  
003800 77  MAX-6191-IX                 PIC S9(2)   VALUE +24.                   
003900                                                                          
004000 77  SPAR-KVINLART-NEW           PIC S9(6)   VALUE ZERO.                  
004100 77  SPAR-ADINLOMR-NEW           PIC X(4)    VALUE SPACE.                 
004200 77  SPAR-ADINLOMR-NXT-NEW       PIC X(4)    VALUE SPACE.                 
004300 77  SPAR-KDINLSTA-NEW           PIC X(3)    VALUE SPACE.                 
004400 77  SPAR-KVINLART-OLD           PIC S9(6)   VALUE ZERO.                  
004500 77  SPAR-ADINLOMR-OLD           PIC X(4)    VALUE SPACE.                 
004600 77  SPAR-ADINLOMR-NXT-OLD       PIC X(4)    VALUE SPACE.                 
004700 77  SPAR-KDINLSTA-OLD           PIC X(3)    VALUE SPACE.                 
004800                                                                          
004900 77  W-KVTRANS                   PIC 9(2)      VALUE ZERO.                
005000 77  W-KVTRANS-IN                PIC 9(3)      VALUE ZERO.                
005100 77  W-PRARTSTD                  PIC 9(7)V9(2) VALUE ZERO.                
005200 77  W-KDINLPRIO                 PIC 9(2)      VALUE ZERO.                
005300 77  WS-IDLOPNRM                 PIC S9(9)     VALUE ZERO.                
005400 77  WS-IDRADNR                  PIC S9(5)     VALUE ZERO.                
005600 77  W-KVINLART                  PIC 9(6)      VALUE ZERO.                
005700 77  W-KVANTMOT                  PIC 9(6)      VALUE ZERO.                
005800                                                                          
005900                                                                          
006000 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
006100                                                                          
006200*   OM SVAR TILL SKÄRM: MAX-MOD-LAENGD = MOD-LÄNGD + 4                    
006300*   OM PROGRAM-TILL-PROGRAM-SWITCH:    = MOD-LÄNGD + 17                   
006400 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +917 COMP SYNC.         
006500 77  LNG-P-TO-P-PREFIX           PIC S9(4)  VALUE +17  COMP SYNC.         
006600                                                                          
006700*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
006800                                                                          
006900 77  INDATA-SW                   PIC X       VALUE 'J'.                   
007000     88  INDATA-OK                           VALUE 'J'.                   
007100     88  INDATA-FEL                          VALUE 'N'.                   
007200                                                                          
007300 77  NUMERIC-SW                  PIC X       VALUE 'J'.                   
007400     88  NUMERIC-INPUT                       VALUE 'J'.                   
007500                                                                          
007600 77  RAD-SW                      PIC X       VALUE 'J'.                   
007700     88  RAD-OK                              VALUE 'J'.                   
007800     88  RAD-FEL                             VALUE 'N'.                   
007900                                                                          
008000 77  IFYLLD-SW                   PIC X       VALUE 'N'.                   
008100     88  MID-IFYLLD                          VALUE 'J'.                   
008200                                                                          
008300 77  FOERSTA-6191-SW             PIC X       VALUE 'J'.                   
008400     88  FOERSTA-6191                        VALUE 'J'.                   
008500                                                                          
008600 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
008700     88  NYCKLAR-OK                          VALUE 'J'.                   
008800     88  NYCKLAR-FEL                         VALUE 'N'.                   
008900                                                                          
009000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
009100     88  EGEN-MID                            VALUE '6147'.                
009200     88  GODK-MID                            VALUE '6141' '6142'          
009300                                                   '6143' '6144'          
009400                                                   '6145' '6146'          
009500                                                   '6147' '6148'          
009600                                                   '6149'.                
009700     88  HELP-MID                            VALUE '0551'.                
009800     EJECT                                                                
009810*      --- VALID IDDC CODES                                               
009820*                                                                         
009830*01    -COPY WWDC99                                                       
009840       EJECT                                                              
009900*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
010000 01  GENERELLA-SUBPROGRAM.                                                
010100     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
010200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010400     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
010500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
010600     EJECT                                                                
010700*01 -COPY WMSGINIT                                                        
010800     SKIP3                                                                
010900*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
011000*01 -COPY WMEDAREA                                                        
011100     SKIP3                                                                
011200 01  MESSAGE-CODES.                                                       
011300     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
011400     03  ERR-CONFLICT            PIC X(3)    VALUE '002'.                 
011500     03  ERR-INVALID-UPD         PIC X(3)    VALUE '007'.                 
011600     03  ERR-MISSING             PIC X(3)    VALUE '010'.                 
011700     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
011800     03  ERR-NOT-NUMERIC         PIC X(3)    VALUE '020'.                 
011900     03  ERR-CASELEVEL           PIC X(3)    VALUE '178'.                 
012000     03  ERR-QUALITY             PIC X(3)    VALUE '189'.                 
012100     03  ERR-3-OR-77             PIC X(3)    VALUE '227'.                 
012200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
012300     03  ERR-PLACE-MISS          PIC X(3)    VALUE '764'.                 
012310     03  ERR-CONTROL-NOT-COMPL   PIC X(3)    VALUE '215'.                 
012320     03  ERR-WEIGHT-MISSING      PIC X(3)    VALUE '792'.                 
012330     03  ERR-VOLUME-MISSING      PIC X(3)    VALUE '793'.                 
012340     03  ERR-VOLUME-MISSING      PIC X(3)    VALUE '793'.                 
012350     03  ERR-ORIGIN-MISSING      PIC X(3)    VALUE '794'.                 
012400*                                                                         
012500     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
012600     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
012700     EJECT                                                                
012800*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
012900*                                                                         
013000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
013100     SKIP3                                                                
013200*01  MID -COPY W6I14701                                                   
013300     EJECT                                                                
013400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
013500     SKIP3                                                                
013600*01  -COPY WMSGAREA                                                       
013700     EJECT                                                                
013800     03  MOD REDEFINES MSG-AREA.                                          
013900*      05  -COPY W6O14701                                                 
014000     EJECT                                                                
014100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
014200     SKIP3                                                                
014300*01  -COPY WMFSAREA                                                       
014400     EJECT                                                                
014500 01  KOM-MSG-IO-AREA.                                                     
014600*03  -COPY WMSGKOM                                                        
014700     EJECT                                                                
014800 01  FILLER             PIC X(16)  VALUE 'MSG/KOM-AREA'.                  
014900     SKIP3                                                                
015000*01  -COPY WMSGSNUF     -PRE P-TO-P-                                      
015100     EJECT                                                                
015200 01      FILLER                  PIC X(24)   VALUE                        
015300                                 'MOD6191-MID-W6I19101'.                  
015400     SKIP2                                                                
015500     -COPY W6I19101 -PRE MOD6191-                                         
015600     EJECT                                                                
015700 01      FILLER                  PIC X(24)   VALUE                        
015800                                 'MOD6193-MID-W6I19301'.                  
015900     SKIP2                                                                
016000     -COPY W6I19301 -PRE MOD6193-                                         
016100     EJECT                                                                
016200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
016300*                                                                         
016400     EJECT                                                                
016500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016600     SKIP3                                                                
016700 01  NYCKLAR-TILL-DLI.                                                    
016800     03  W-IDLOPNRM-X.                                                    
016900         05  W-IDLOPNRM          PIC S9(9)   VALUE ZERO COMP-3.           
017000     03  W-IDRADNR-X.                                                     
017100         05  W-IDRADNR           PIC S9(5)   VALUE ZERO COMP-3.           
017200     03  W-IDUSER-X.                                                      
017300         05  W-IDUSER            PIC X(8)    VALUE SPACE.                 
017400     03  W-IDDC                  PIC X(2)    VALUE SPACE.                 
017500     SKIP2                                                                
017600*    --- STATUS-KOD FRÅN IMS                                              
017700 01  STATUS-WS                   PIC XX.                                  
017800     88  SEGMENT-FINNS                       VALUE '  '.                  
017900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
018000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
018100     SKIP2                                                                
018200 01  GODK-STATUSKODER.                                                    
018300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018400     SKIP3                                                                
018500 01  SSA1                        PIC X(64).                               
018600 01  SSA2                        PIC X(64).                               
018700     EJECT                                                                
018800*    --- IMS FUNKTIONSKODER                                               
018900*01  -COPY W0003                                                          
019000     EJECT                                                                
019100*    ---  DLI INPUT-OUTPUT AREA                                           
019200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
019300     SKIP3                                                                
019400 01  DLI-IO-AREA.                                                         
019500     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
019600     SKIP3                                                                
019700     03  W6INLA11 REDEFINES IO-AREA.                                      
019800*        05  -COPY W6D111                                                 
019900     SKIP3                                                                
020000     03  W6INLA21 REDEFINES IO-AREA.                                      
020100*        05  -COPY W6D121                                                 
020200     EJECT                                                                
020210 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-UPFA'.         
020220     SKIP3                                                                
020230 01  DLI-IO-UPFA.                                                         
020240     03  IO-UPFA                PIC X(100)  VALUE SPACE.                  
020250     03  W6UPFA01 REDEFINES IO-UPFA.                                      
020260*        05  -COPY W6L101                                                 
020270     EJECT                                                                
020280 01  DLI-IO-AREA-UPFA11.                                                  
020290     03  W6UPFA11.                                                        
020291*        05  -COPY W6L111                                                 
020292     SKIP3                                                                
020293 01  DLI-IO-AREA-UPFA12.                                                  
020294     03  W6UPFA12.                                                        
020295*        05  -COPY W6L112                                                 
020296     EJECT                                                                
020300 LINKAGE SECTION.                                                         
020400                                                                          
020500*01  -COPY W0009   -PRE MSG-                                              
020600     EJECT                                                                
020700*01  -COPY W0009   -PRE ALT1-                                             
020800     EJECT                                                                
020900*01  -COPY W0009   -PRE DISP-                                             
021000     EJECT                                                                
021100*01  -COPY W0008  -PRE USEA-                                              
021200     05  FILLER                  PIC X.                                   
021300     EJECT                                                                
021400*01  -COPY W0008  -PRE INLASEQ-                                           
021500     05  FILLER                  PIC X.                                   
021600     EJECT                                                                
021610*01  -COPY W0008  -PRE UPFA-                                              
021620     05  FILLER                  PIC X.                                   
021630     SKIP3                                                                
021700*01  -COPY W0008  -PRE KOMA-                                              
021800     05  FILLER                  PIC X.                                   
021900     EJECT                                                                
022000 PROCEDURE DIVISION  USING MSG-PCB ALT1-PCB DISP-PCB USEA-PCB             
022100                           INLASEQ-PCB UPFA-PCB KOMA-PCB.                 
022200     ENTRY 'DLITCBL' USING MSG-PCB ALT1-PCB DISP-PCB USEA-PCB             
022300                           INLASEQ-PCB UPFA-PCB KOMA-PCB.                 
022400                                                                          
022500     PERFORM IMS-GET-MSG                                                  
022600     IF SEGMENT-FINNS                                                     
022700       PERFORM A-INIT                                                     
022800       IF EGEN-MID                                                        
022900         PERFORM B-KOLLA-OM-NUMERIC                                       
023000         IF NUMERIC-INPUT AND MID-IFYLLD AND NYCKLAR-OK                   
023100           PERFORM G-KOLLA-INPUT                                          
023200           IF INDATA-OK                                                   
023300             PERFORM H-UPPDATERA                                          
023400             PERFORM F-LAES-VISA-INFO                                     
023500           ELSE                                                           
023600             PERFORM MFS-ROER-EJ-FAELT-IN                                 
023700             PERFORM MFS-ROER-EJ-FAELT-UT                                 
023800           END-IF                                                         
023900         ELSE                                                             
024000           PERFORM MFS-ROER-EJ-FAELT-IN                                   
024100           PERFORM MFS-ROER-EJ-FAELT-UT                                   
024200         END-IF                                                           
024300       ELSE                                                               
024400         IF HELP-MID                                                      
024500           PERFORM E-SAMMA-SIDA                                           
024600         ELSE                                                             
024700           PERFORM MFS-RENSA-FAELT-IN                                     
024800         END-IF                                                           
024900         PERFORM F-LAES-VISA-INFO                                         
025000       END-IF                                                             
025100       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
025200       PERFORM IMS-INSERT-MSG                                             
025300     END-IF                                                               
025400                                                                          
025500     MOVE ZERO TO RETURN-CODE                                             
025600     GOBACK                                                               
025700     .                                                                    
025800     EJECT                                                                
025900 A-INIT SECTION.                                                          
026000                                                                          
026100     IF MSG-DUBBLA-TRANSKODER                                             
026200       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I14701                 
026300       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
026400       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
026500     ELSE                                                                 
026600       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I14701                  
026700       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
026800       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
026900     END-IF                                                               
027000                                                                          
027100     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
027200     MOVE MSG-IDPFK TO MFS-IDPFK                                          
027300     MOVE MFS-IDTRANS TO W-IDTRANS                                        
027400                                                                          
027500     MOVE LOW-VALUE TO MSG-AREA                                           
027600     MOVE 'W6O147N1' TO MFS-IDMOD                                         
027700     MOVE '6147' TO MOD-IDTRANS                                           
027800     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
027900                                                                          
028000     IF EGEN-MID OR HELP-MID                                              
028100       CONTINUE                                                           
028200     ELSE                                                                 
028300       MOVE SPACE TO MFS-KDTRTYP                                          
028400       MOVE '7' TO MFS-IDPFK                                              
028500     END-IF                                                               
028600                                                                          
028610     PERFORM AA-INIT-NYCKLAR                                              
028620                                                                          
028700     IF MSGI-IDLAND-SPR = 'GB'                                            
028800       MOVE +2 TO SPRAK-IX                                                
028900       MOVE 'GB ' TO MED-IDSKYLT                                          
029000     ELSE                                                                 
029100       MOVE +1 TO SPRAK-IX                                                
029200       MOVE 'S  ' TO MED-IDSKYLT                                          
029300     END-IF                                                               
029500     .                                                                    
029600     EJECT                                                                
029700*----------------------------------------------------------------*        
029800 AA-INIT-NYCKLAR SECTION.                                                 
029900                                                                          
030000     MOVE ALL '+' TO MSGI-WMSGINIT                                        
030100     MOVE '001'                  TO MSGI-KDCALL                           
030200     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
030210     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
030220     MOVE '6147'                 TO MSGI-IDTRANS                          
030300     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
030400     .                                                                    
030500     EJECT                                                                
030600 B-KOLLA-OM-NUMERIC SECTION.                                              
030700                                                                          
030800     MOVE MFS-RENSA-FAELT  TO MOD-IDDC-IN                                 
030900                                                                          
031000     IF MID-IDDC-IN = ALL '+'                                             
031100        MOVE MSGI-IDDC     TO WS-IDDC                                     
031200     ELSE                                                                 
031300        MOVE MID-IDDC-IN   TO WS-IDDC                                     
031400     END-IF                                                               
031500                                                                          
031600     IF CDC                                                               
031900       MOVE WS-IDDC   TO W-IDDC                                           
032000     ELSE                                                                 
032100       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
032200       CALL WMEDKONV USING MED-WMEDAREA                                   
032300       MOVE MED-MFSFEL  TO MOD-TEMFSFEL                                   
032400       MOVE NEJ TO NYCKLAR-SW                                             
032500     END-IF                                                               
032600                                                                          
032700     MOVE +1 TO INDX                                                      
032800                                                                          
032900     PERFORM UNTIL INDX > MAX-INDX                                        
033000       IF MID-IDLOPNRM (INDX) = ALL '+'                                   
033100         IF MID-KVANTMOT(INDX) NOT = ALL '+'                              
033200           MOVE MFS-NUM-FAELT-FEL TO                                      
033300                     MOD-IDLOPNRM-ATTR (INDX)                             
033400           MOVE NEJ TO NUMERIC-SW                                         
033500                       RAD-SW                                             
033600           MOVE ERR-NOT-NUMERIC TO MED-IDMFSFEL                           
033700         END-IF                                                           
033800       ELSE                                                               
033900         IF MID-IDLOPNRM (INDX) NUMERIC                                   
034000           MOVE MFS-NUM-FAELT-RAETT TO                                    
034100                     MOD-IDLOPNRM-ATTR (INDX)                             
034200         ELSE                                                             
034300           MOVE MFS-NUM-FAELT-FEL TO                                      
034400                     MOD-IDLOPNRM-ATTR (INDX)                             
034500           MOVE NEJ TO NUMERIC-SW                                         
034600                       RAD-SW                                             
034700           MOVE ERR-NOT-NUMERIC TO MED-IDMFSFEL                           
034800         END-IF                                                           
034900         MOVE JA  TO IFYLLD-SW                                            
035000       END-IF                                                             
035100       IF MID-KVANTMOT (INDX) = ALL '+'                                   
035200         IF MID-IDLOPNRM(INDX) NOT = ALL '+'                              
035300           MOVE MFS-NUM-FAELT-FEL TO                                      
035400                     MOD-KVANTMOT-ATTR (INDX)                             
035500           MOVE NEJ TO NUMERIC-SW                                         
035600                       RAD-SW                                             
035700           MOVE ERR-NOT-NUMERIC TO MED-IDMFSFEL                           
035800         END-IF                                                           
035900       ELSE                                                               
036000         IF MID-KVANTMOT (INDX) NUMERIC                                   
036100           MOVE MFS-NUM-FAELT-RAETT TO                                    
036200                     MOD-KVANTMOT-ATTR (INDX)                             
036300         ELSE                                                             
036400           MOVE MFS-NUM-FAELT-FEL TO                                      
036500                     MOD-KVANTMOT-ATTR (INDX)                             
036600           MOVE NEJ TO NUMERIC-SW                                         
036700                       RAD-SW                                             
036800           MOVE ERR-NOT-NUMERIC TO MED-IDMFSFEL                           
036900         END-IF                                                           
037000         MOVE JA  TO IFYLLD-SW                                            
037100       END-IF                                                             
037200       IF MID-FLSVAR(INDX) NOT = ALL '+'                                  
037300         IF MID-FLSVAR(INDX) = JA OR NEJ                                  
037400           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLSVAR-ATTR(INDX)             
037500         ELSE                                                             
037600           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLSVAR-ATTR(INDX)               
037700           MOVE NEJ TO NUMERIC-SW                                         
037800                       RAD-SW                                             
037900           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
038000         END-IF                                                           
038100       END-IF                                                             
038200       IF RAD-FEL                                                         
038300         CALL WMEDKONV USING MED-WMEDAREA                                 
038400         MOVE MED-MFSFEL TO MOD-FELMEDD(INDX)                             
038500       END-IF                                                             
038600       MOVE JA TO RAD-SW                                                  
038700       ADD +1 TO INDX                                                     
038800     END-PERFORM                                                          
038900                                                                          
039000     IF NYCKLAR-OK OR EGEN-MID OR GODK-MID                                
039100        MOVE WS-IDDC    TO MOD-IDDC-UT                                    
039200     ELSE                                                                 
039300        MOVE MFS-RENSA-FAELT TO MOD-IDDC-UT                               
039400     END-IF                                                               
039500     .                                                                    
039600     EJECT                                                                
039700 E-SAMMA-SIDA SECTION.                                                    
039800                                                                          
039900     PERFORM EB-KOLLA-OM-IFYLLD                                           
040000     IF NOT MID-IFYLLD                                                    
040100       PERFORM MFS-RENSA-FAELT-IN                                         
040200     ELSE                                                                 
040300       PERFORM EA-MID-INDATA-TILL-MOD                                     
040400     END-IF                                                               
040500     .                                                                    
040600     EJECT                                                                
040700 EA-MID-INDATA-TILL-MOD SECTION.                                          
040800                                                                          
040900* * * * * FÖR VARJE MID-FÄLT                                              
041000* * * * * OM MID-FÄLT NOT = ALL '+' FLYTTA MID-FÄLT TILL MOD-INDAT        
041100* * * * *        FLYTTA MFS-ADD-LAES-IN-FAELT TILL MOD-INDATA-ATTR        
041200* * * * * ANNARS FLYTTA RENSA-FÄLT TILL MOD-INDATA-FÄLT                   
041300                                                                          
041400     MOVE MID-KVTRANS-IN TO MOD-KVTRANS-IN                                
041500     MOVE MID-KVTRANS-UT TO MOD-KVTRANS-UT                                
041600     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVTRANS-IN-ATTR                    
041700     MOVE +1 TO INDX                                                      
041800                                                                          
041900     PERFORM UNTIL INDX > MAX-INDX                                        
042000       IF MID-IDLOPNRM (INDX) = ALL '+'                                   
042100         MOVE MFS-RENSA-FAELT TO MOD-IDLOPNRM(INDX)                       
042200       ELSE                                                               
042300         MOVE MID-IDLOPNRM(INDX) TO MOD-IDLOPNRM(INDX)                    
042400         MOVE MFS-ADD-LAES-IN-FAELT TO                                    
042500                     MOD-IDLOPNRM-ATTR(INDX)                              
042600       END-IF                                                             
042700       IF MID-KVANTMOT (INDX) = ALL '+'                                   
042800         MOVE MFS-RENSA-FAELT TO MOD-KVANTMOT(INDX)                       
042900       ELSE                                                               
043000         MOVE MID-KVANTMOT(INDX) TO MOD-KVANTMOT(INDX)                    
043100         MOVE MFS-ADD-LAES-IN-FAELT TO                                    
043200                     MOD-KVANTMOT-ATTR(INDX)                              
043300       END-IF                                                             
043400       ADD +1 TO INDX                                                     
043500     END-PERFORM                                                          
043600     .                                                                    
043700 EB-KOLLA-OM-IFYLLD SECTION.                                              
043800                                                                          
043900     MOVE +1 TO INDX                                                      
044000                                                                          
044100     PERFORM UNTIL INDX > MAX-INDX                                        
044200       IF MID-IDLOPNRM (INDX) = ALL '+'                                   
044300         CONTINUE                                                         
044400       ELSE                                                               
044500         MOVE JA  TO IFYLLD-SW                                            
044600       END-IF                                                             
044700       IF MID-KVANTMOT (INDX) = ALL '+'                                   
044800         CONTINUE                                                         
044900       ELSE                                                               
045000         MOVE JA  TO IFYLLD-SW                                            
045100       END-IF                                                             
045200       ADD +1 TO INDX                                                     
045300     END-PERFORM                                                          
045400                                                                          
045500     .                                                                    
045600     EJECT                                                                
045700 F-LAES-VISA-INFO SECTION.                                                
045800                                                                          
045900     IF W-IDTRANS = '6146' OR '6147'                                      
046000        IF MID-KVTRANS-IN NUMERIC                                         
046100            COMPUTE W-KVTRANS = W-KVTRANS + W-KVTRANS-IN                  
046200        ELSE                                                              
046300            INSPECT MID-KVTRANS-UT                                        
046400                                 REPLACING LEADING SPACE BY ZERO          
046500            COMPUTE W-KVTRANS = W-KVTRANS + MID-KVTRANS-UT                
046600        END-IF                                                            
046700        MOVE W-KVTRANS        TO MOD-KVTRANS-UT                           
046800     ELSE                                                                 
046900       MOVE ZERO              TO MOD-KVTRANS-UT                           
047000     END-IF                                                               
047100                                                                          
047200     IF NOT HELP-MID                                                      
047300       PERFORM MFS-RENSA-FAELT-IN                                         
047400     END-IF                                                               
047500                                                                          
047600     .                                                                    
047700     EJECT                                                                
047800 G-KOLLA-INPUT SECTION.                                                   
047900                                                                          
048000     MOVE JA  TO INDATA-SW                                                
048100                 RAD-SW                                                   
048200     MOVE +1 TO INDX                                                      
048300                                                                          
048400     IF MID-KVTRANS-IN                 NOT = ALL '+'                      
048500        IF MID-KVTRANS-IN NUMERIC                                         
048600            MOVE MFS-NUM-FAELT-RAETT   TO MOD-KVTRANS-IN-ATTR             
048700            MOVE MID-KVTRANS-IN        TO W-KVTRANS-IN                    
048800        ELSE                                                              
048900           MOVE MFS-NUM-FAELT-FEL      TO MOD-KVTRANS-IN-ATTR             
049000           MOVE NEJ                    TO INDATA-SW                       
049100           MOVE ERR-CORR-HILITE-FLDS   TO MED-IDMFSFEL                    
049200        END-IF                                                            
049300     END-IF                                                               
049400                                                                          
049500     PERFORM UNTIL INDX > MAX-INDX                                        
049600       IF MID-IDLOPNRM(INDX) NUMERIC                                      
049700         MOVE MID-IDLOPNRM(INDX) TO W-IDLOPNRM                            
049800         PERFORM GA-KOLLA-OM-FINNS-INNAN                                  
049900         IF RAD-OK                                                        
050000           PERFORM GB-KOLLA-W6D1                                          
050100           IF RAD-OK                                                      
050101             PERFORM GC-PRIM-SEK-KOLL                                     
050111             IF RAD-FEL                                                   
050112               CALL WMEDKONV USING MED-WMEDAREA                           
050113               MOVE MED-MFSFEL TO MOD-FELMEDD(INDX)                       
050114             END-IF                                                       
050120           ELSE                                                           
050200             CALL WMEDKONV USING MED-WMEDAREA                             
050300             MOVE MED-MFSFEL TO MOD-FELMEDD(INDX)                         
050400           END-IF                                                         
050500         ELSE                                                             
050600           CALL WMEDKONV USING MED-WMEDAREA                               
050700           MOVE MED-MFSFEL TO MOD-FELMEDD(INDX)                           
050800         END-IF                                                           
050900       END-IF                                                             
051000       ADD +1 TO INDX                                                     
051100       MOVE JA TO RAD-SW                                                  
051200     END-PERFORM                                                          
051300     .                                                                    
051400     EJECT                                                                
051500 GA-KOLLA-OM-FINNS-INNAN SECTION.                                         
051600                                                                          
051700     MOVE JA TO RAD-SW                                                    
051800     MOVE +1 TO INDX2                                                     
051900     PERFORM UNTIL INDX2 > MAX-INDX                                       
052000       IF MID-IDLOPNRM(INDX2) NUMERIC                                     
052100         MOVE MID-IDLOPNRM(INDX2) TO WS-IDLOPNRM                          
052200         IF WS-IDLOPNRM = W-IDLOPNRM                                      
052300           IF INDX2 NOT = INDX                                            
052400             MOVE NEJ TO RAD-SW                                           
052500                         INDATA-SW                                        
052600             MOVE ERR-CONFLICT TO MED-IDMFSFEL                            
052700             CALL WMEDKONV USING MED-WMEDAREA                             
052800             MOVE MED-MFSFEL TO MOD-FELMEDD(INDX)                         
052900                                MOD-FELMEDD(INDX2)                        
053000           END-IF                                                         
053100         END-IF                                                           
053200       END-IF                                                             
053300       ADD +1 TO INDX2                                                    
053400     END-PERFORM                                                          
053500     .                                                                    
053600     EJECT                                                                
053700 GB-KOLLA-W6D1 SECTION.                                                   
053800                                                                          
053900     MOVE JA TO RAD-SW                                                    
054000     PERFORM IMS-GU-W6D1-INLA11                                           
054100     IF SEGMENT-FINNS                                                     
054200       IF ART-FLKLAR = JA                                                 
054300         MOVE ERR-MISSING  TO MED-IDMFSFEL                                
054400         MOVE NEJ TO INDATA-SW                                            
054500                     RAD-SW                                               
054600       ELSE                                                               
054700         IF ART-KDRT = +3 OR +77                                          
054800           MOVE ERR-3-OR-77 TO MED-IDMFSFEL                               
054900           MOVE NEJ TO INDATA-SW                                          
055000                       RAD-SW                                             
055100         ELSE                                                             
055200           IF ART-FLKVAFEL = JA OR ART-FLKVAKAR = JA                      
055300             MOVE ERR-QUALITY TO MED-IDMFSFEL                             
055400             MOVE NEJ TO INDATA-SW                                        
055500                         RAD-SW                                           
055600           ELSE                                                           
055610             IF NOT CDC-TR                                                
055700               IF ART-ADPLATS = ZERO AND                                  
055710                  ART-ADTRDEST(1:2) NOT = 'CD'                            
055800                 IF MID-FLSVAR(INDX) = ALL '+'                            
055900                   MOVE MFS-OEPPNA-ALFA-FAELT TO                          
056000                                         MOD-FLSVAR-ATTR(INDX)            
056100                   MOVE ERR-PLACE-MISS TO MED-IDMFSFEL                    
056200                   MOVE NEJ TO INDATA-SW                                  
056300                               RAD-SW                                     
056400                 END-IF                                                   
056410               ELSE                                                       
056420                 IF ART-VKART = ZERO                                      
056430                   MOVE NEJ    TO INDATA-SW                               
056440                                  RAD-SW                                  
056450                   MOVE '792'  TO MED-IDMFSFEL                            
056460                 ELSE                                                     
056470                   IF ART-VLARTNTO = ZERO                                 
056480                     MOVE NEJ    TO INDATA-SW                             
056490                                    RAD-SW                                
056491                     MOVE '793'  TO MED-IDMFSFEL                          
056493                   ELSE                                                   
056494                     IF ART-KDARTURS = SPACE                              
056495                       MOVE NEJ    TO INDATA-SW                           
056496                                      RAD-SW                              
056497                       MOVE '794'  TO MED-IDMFSFEL                        
056498                     END-IF                                               
056499                   END-IF                                                 
056500                 END-IF                                                   
056510               END-IF                                                     
056520             END-IF                                                       
056600           END-IF                                                         
056700         END-IF                                                           
056800       END-IF                                                             
056900                                                                          
057000       IF INDATA-OK                                                       
057100         PERFORM IMS-GNP-W6D1-INLA21-OKVAL                                
057200         IF RAD-IDRADNR NOT = +1                                          
057300           MOVE ERR-CASELEVEL TO MED-IDMFSFEL                             
057400           MOVE NEJ TO INDATA-SW                                          
057500                       RAD-SW                                             
057600         ELSE                                                             
057700           IF RAD-KDINLSTA = SPACE OR 'SAK' OR 'FPK'                      
057800             IF RAD-IDOKOLLI NOT = ZERO                                   
057900               MOVE ERR-CASELEVEL TO MED-IDMFSFEL                         
058000               MOVE NEJ TO INDATA-SW                                      
058100                           RAD-SW                                         
058200             ELSE                                                         
058300               PERFORM IMS-GNP-W6D1-INLA21-OKVAL                          
058400               PERFORM UNTIL RAD-FEL OR SEGMENT-SAKNAS                    
058500                 IF RAD-KDINLSTA = 'SAK' OR 'FPK' OR SPACE                
058600                   MOVE ERR-CASELEVEL TO MED-IDMFSFEL                     
058700                   MOVE NEJ TO INDATA-SW                                  
058800                               RAD-SW                                     
058900                 END-IF                                                   
059000                 PERFORM IMS-GNP-W6D1-INLA21-OKVAL                        
059100               END-PERFORM                                                
059200             END-IF                                                       
059300           ELSE                                                           
059400             MOVE ERR-INVALID-UPD TO MED-IDMFSFEL                         
059500             MOVE NEJ TO INDATA-SW                                        
059600                         RAD-SW                                           
059700           END-IF                                                         
059800         END-IF                                                           
059900       END-IF                                                             
060000     ELSE                                                                 
060100       MOVE ERR-MISSING  TO MED-IDMFSFEL                                  
060200       MOVE NEJ TO INDATA-SW                                              
060300                   RAD-SW                                                 
060400     END-IF                                                               
060500     .                                                                    
060600     EJECT                                                                
060610 GC-PRIM-SEK-KOLL SECTION.                                                
060620                                                                          
060630** KOLLAR OM KONTROLLERAD PÅ 6139                                         
060650     PERFORM IMS-GU-UPFA-01                                               
060660     IF SEGMENT-FINNS                                                     
060670       IF UPPF-KVKVAPRIM > 0                                              
060680** ARTIKEL UTTAGEN FÖR PRIMÄRKONTROLL                                     
060690          IF UPPF-KDKVASTA-PRI = '2' OR '3'                               
060691** PRIMÄRKONTROLL SATT SOM JA/NEJ. (OM NEJ HAR KR SKAPATS).               
060692             CONTINUE                                                     
060693          ELSE                                                            
060694             MOVE '215'     TO MED-IDMFSFEL                               
060695             MOVE NEJ       TO INDATA-SW                                  
060696                               RAD-SW                                     
060697          END-IF                                                          
060698       END-IF                                                             
060699       IF UPPF-KVKVASEK > 0                                               
060700          IF UPPF-KDKVASTA-SEK = '2' OR '3'                               
060701             CONTINUE                                                     
060702          ELSE                                                            
060703             MOVE '215'     TO MED-IDMFSFEL                               
060704             MOVE NEJ       TO INDATA-SW                                  
060705                               RAD-SW                                     
060706          END-IF                                                          
060707       END-IF                                                             
060708     END-IF                                                               
060709                                                                          
060710** KOLLAR ATT EVENTUELLT GAMLA KR BLIVIT BEDÖMDA                          
060711     IF INDATA-OK                                                         
060712       PERFORM IMS-GU-UPFA-01                                             
060713       IF SEGMENT-FINNS                                                   
060714         PERFORM IMS-GNP-UPFA11                                           
060715         PERFORM UNTIL SEGMENT-SAKNAS                                     
060716           IF RAPP-KDKVASTA-PRI = '2' OR '3'                              
060717             CONTINUE                                                     
060718           ELSE                                                           
060719             MOVE '215'     TO MED-IDMFSFEL                               
060720             MOVE NEJ       TO INDATA-SW                                  
060721                               RAD-SW                                     
060722           END-IF                                                         
060723           PERFORM IMS-GNP-UPFA11                                         
060724         END-PERFORM                                                      
060725       END-IF                                                             
060726     END-IF                                                               
060727                                                                          
060728** KOLLAR ATT EVENTUELL SPECIALKONTROLL ÄR GJORD                          
060729     IF INDATA-OK                                                         
060730       PERFORM IMS-GU-UPFA-01                                             
060731       IF SEGMENT-FINNS                                                   
060732         PERFORM IMS-GNP-UPFA12                                           
060733         PERFORM UNTIL SEGMENT-SAKNAS                                     
060734           IF SPEC-KDKVASTA-PRI = '2' OR '3'                              
060735             CONTINUE                                                     
060736           ELSE                                                           
060737             MOVE '215'     TO MED-IDMFSFEL                               
060738             MOVE NEJ       TO INDATA-SW                                  
060739                               RAD-SW                                     
060740           END-IF                                                         
060741           PERFORM IMS-GNP-UPFA12                                         
060742         END-PERFORM                                                      
060743       END-IF                                                             
060744     END-IF                                                               
060745     .                                                                    
060746     EJECT                                                                
060750 H-UPPDATERA SECTION.                                                     
060800                                                                          
060900     MOVE ZERO     TO W-KVTRANS                                           
061000     MOVE +1 TO INDX                                                      
061100                6191-IX                                                   
061200     PERFORM UNTIL INDX > MAX-INDX                                        
061300       IF MID-IDLOPNRM(INDX) NOT = ALL '+'                                
061400         MOVE MID-IDLOPNRM(INDX) TO W-IDLOPNRM                            
061500         MOVE MID-KVANTMOT(INDX) TO W-KVANTMOT                            
061600         MOVE +1                 TO W-IDRADNR                             
061700         PERFORM IMS-GU-W6D1-INLA11                                       
061800         IF (ART-ADPLATS NOT = ZERO) OR                                   
061900           (ART-ADPLATS = ZERO AND MID-FLSVAR(INDX) = JA)                 
062000           MOVE ART-PRARTSTD TO W-PRARTSTD                                
062100           MOVE ART-KDINLPRIO TO W-KDINLPRIO                              
062200           PERFORM IMS-GHNP-W6D1-INLA21-KVAL                              
062300           IF W-KVANTMOT NOT = ZERO                                       
062400             PERFORM HA-UPPDATERA-INLAEGGNING                             
062500             IF W-KVINLART NOT = W-KVANTMOT                               
062600               PERFORM HB-UPPDATERA-AVVIKELSE                             
062700             END-IF                                                       
062800           ELSE                                                           
062900             PERFORM HC-UPPDATERA-INL-AVV                                 
063000           END-IF                                                         
063100           ADD +1 TO W-KVTRANS                                            
063200         END-IF                                                           
063300       END-IF                                                             
063400       ADD +1 TO INDX                                                     
063500     END-PERFORM                                                          
063600     PERFORM HD-SKICKA-TRANS                                              
063700                                                                          
063800     IF W-KVTRANS > ZERO                                                  
063900       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
064000       CALL WMEDKONV USING MED-WMEDAREA                                   
064100       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
064200     END-IF                                                               
064300     PERFORM MFS-RENSA-FAELT-IN                                           
064400     .                                                                    
064500     EJECT                                                                
064600 HA-UPPDATERA-INLAEGGNING SECTION.                                        
064700                                                                          
064800     PERFORM IMS-DLET-W6D1-INLA21                                         
064900     MOVE RAD-KVINLART TO W-KVINLART                                      
065000                          SPAR-KVINLART-OLD                               
065100     MOVE RAD-ADINLOMR TO SPAR-ADINLOMR-OLD                               
065200                          SPAR-ADINLOMR-NXT-OLD                           
065300     MOVE RAD-KDINLSTA TO SPAR-KDINLSTA-OLD                               
065400                                                                          
065500     MOVE ZERO  TO SPAR-KVINLART-NEW                                      
065600     MOVE RAD-ADINLOMR TO SPAR-ADINLOMR-NEW                               
065700     MOVE SPACE TO SPAR-ADINLOMR-NXT-NEW                                  
065800                   SPAR-KDINLSTA-NEW                                      
065900     PERFORM S01-FYLL-I-6191TRANS                                         
066000                                                                          
066100     PERFORM IMS-GNP-W6D1-INLA21-LAST                                     
066200     ADD +1            TO RAD-IDRADNR                                     
066300     MOVE RAD-IDRADNR  TO WS-IDRADNR                                      
066400     MOVE SPACE        TO RAD-ADINLOMR                                    
066500                          RAD-ADINLOMR-NXT                                
066510                          RAD-IDLEVNR-KOLLI                               
066600     MOVE NEJ          TO RAD-FLDIVKLI                                    
066700                          RAD-FLINLFP                                     
066800                          RAD-FLKVAANT                                    
066900                          RAD-FLPRIO                                      
067000                          RAD-FLSATS                                      
067100                          RAD-FLINLFB                                     
067110                          RAD-FLSVSLS                                     
067200     MOVE ZERO         TO RAD-IDANSTNR                                    
067300                          RAD-IDILIRAD                                    
067400                          RAD-IDILIST                                     
067500                          RAD-IDINLVGN                                    
067700                          RAD-IDOKOLLI                                    
067800     MOVE +39          TO RAD-KDINLPRIO                                   
067900     MOVE 'INL'        TO RAD-KDINLSTA                                    
068000     MOVE W-KVANTMOT   TO RAD-KVINLART                                    
068100     ACCEPT RAD-TIUPPDAT  FROM DATE                                       
068200                                                                          
068300     PERFORM IMS-ISRT-W6D1-INLA21                                         
068400                                                                          
068500     MOVE RAD-KVINLART TO SPAR-KVINLART-NEW                               
068600     MOVE RAD-ADINLOMR TO SPAR-ADINLOMR-NEW                               
068700                          SPAR-ADINLOMR-NXT-NEW                           
068800     MOVE RAD-KDINLSTA TO SPAR-KDINLSTA-NEW                               
068900                                                                          
069000     MOVE ZERO  TO SPAR-KVINLART-OLD                                      
069100     MOVE RAD-ADINLOMR TO SPAR-ADINLOMR-OLD                               
069200     MOVE SPACE TO SPAR-ADINLOMR-NXT-OLD                                  
069300                   SPAR-KDINLSTA-OLD                                      
069400     PERFORM S01-FYLL-I-6191TRANS                                         
069500     PERFORM S03-SKAPA-6193-MID                                           
069600                                                                          
069700     .                                                                    
069800     EJECT                                                                
069900 HB-UPPDATERA-AVVIKELSE SECTION.                                          
070000                                                                          
070100     PERFORM IMS-GNP-W6D1-INLA21-LAST                                     
070200     ADD +1           TO RAD-IDRADNR                                      
070300     MOVE RAD-IDRADNR TO WS-IDRADNR                                       
070400     MOVE SPACE       TO RAD-ADINLOMR                                     
070500                         RAD-ADINLOMR-NXT                                 
070510                         RAD-IDLEVNR-KOLLI                                
070600     MOVE NEJ         TO RAD-FLDIVKLI                                     
070700                         RAD-FLINLFP                                      
070800                         RAD-FLKVAANT                                     
070900                         RAD-FLPRIO                                       
071000                         RAD-FLSATS                                       
071100                         RAD-FLINLFB                                      
071110                         RAD-FLSVSLS                                      
071200     MOVE ZERO        TO RAD-IDANSTNR                                     
071300                         RAD-IDILIRAD                                     
071400                         RAD-IDILIST                                      
071500                         RAD-IDINLVGN                                     
071700                         RAD-IDOKOLLI                                     
071800     MOVE +39         TO RAD-KDINLPRIO                                    
071900     MOVE 'AVV'       TO RAD-KDINLSTA                                     
072000     COMPUTE RAD-KVINLART = W-KVINLART - W-KVANTMOT                       
072100     ACCEPT RAD-TIUPPDAT FROM DATE                                        
072200                                                                          
072300     PERFORM IMS-ISRT-W6D1-INLA21                                         
072400                                                                          
072500     MOVE ZERO TO SPAR-KVINLART-OLD                                       
072600     MOVE SPACE TO SPAR-ADINLOMR-OLD                                      
072700                   SPAR-ADINLOMR-NXT-OLD                                  
072800                   SPAR-KDINLSTA-OLD                                      
072900                                                                          
073000     MOVE RAD-KVINLART TO SPAR-KVINLART-NEW                               
073100     MOVE RAD-ADINLOMR TO SPAR-ADINLOMR-NEW                               
073200                          SPAR-ADINLOMR-NXT-NEW                           
073300     MOVE RAD-KDINLSTA TO SPAR-KDINLSTA-NEW                               
073400     PERFORM S01-FYLL-I-6191TRANS                                         
073500                                                                          
073600     .                                                                    
073700     EJECT                                                                
073800 HC-UPPDATERA-INL-AVV SECTION.                                            
073900                                                                          
074000     MOVE RAD-KVINLART TO W-KVINLART                                      
074100     PERFORM IMS-GNP-W6D1-INLA21-LAST                                     
074200     ADD +1            TO RAD-IDRADNR                                     
074300     MOVE RAD-IDRADNR  TO WS-IDRADNR                                      
074400     MOVE SPACE        TO RAD-ADINLOMR                                    
074500                          RAD-ADINLOMR-NXT                                
074510                          RAD-IDLEVNR-KOLLI                               
074600     MOVE NEJ          TO RAD-FLDIVKLI                                    
074700                          RAD-FLINLFP                                     
074800                          RAD-FLKVAANT                                    
074900                          RAD-FLPRIO                                      
075000                          RAD-FLSATS                                      
075100                          RAD-FLINLFB                                     
075110                          RAD-FLSVSLS                                     
075200     MOVE ZERO         TO RAD-IDANSTNR                                    
075300                          RAD-IDILIRAD                                    
075400                          RAD-IDILIST                                     
075500                          RAD-IDINLVGN                                    
075700                          RAD-IDOKOLLI                                    
075800     MOVE +39          TO RAD-KDINLPRIO                                   
075900     MOVE 'AVV'        TO RAD-KDINLSTA                                    
076000     MOVE W-KVINLART   TO RAD-KVINLART                                    
076100     ACCEPT RAD-TIUPPDAT  FROM DATE                                       
076200                                                                          
076300     PERFORM IMS-ISRT-W6D1-INLA21                                         
076400                                                                          
076500     MOVE RAD-KVINLART   TO SPAR-KVINLART-NEW                             
076600     MOVE RAD-ADINLOMR TO SPAR-ADINLOMR-NEW                               
076700                          SPAR-ADINLOMR-NXT-NEW                           
076800     MOVE RAD-KDINLSTA TO SPAR-KDINLSTA-NEW                               
076900                                                                          
077000     MOVE ZERO  TO SPAR-KVINLART-OLD                                      
077100     MOVE SPACE TO SPAR-ADINLOMR-OLD                                      
077200                   SPAR-ADINLOMR-NXT-OLD                                  
077300                   SPAR-KDINLSTA-OLD                                      
077400     PERFORM S01-FYLL-I-6191TRANS                                         
077500                                                                          
077600     MOVE +1 TO W-IDRADNR                                                 
077700     PERFORM IMS-GHNP-W6D1-INLA21-KVAL                                    
077800     PERFORM IMS-DLET-W6D1-INLA21                                         
077900     MOVE RAD-KVINLART TO W-KVINLART                                      
078000                          SPAR-KVINLART-OLD                               
078100     MOVE RAD-ADINLOMR TO SPAR-ADINLOMR-OLD                               
078200                          SPAR-ADINLOMR-NXT-OLD                           
078300     MOVE RAD-KDINLSTA TO SPAR-KDINLSTA-OLD                               
078400                                                                          
078500     MOVE ZERO  TO SPAR-KVINLART-NEW                                      
078600     MOVE SPACE TO SPAR-ADINLOMR-NEW                                      
078700                   SPAR-ADINLOMR-NXT-NEW                                  
078800                   SPAR-KDINLSTA-NEW                                      
078900     PERFORM S01-FYLL-I-6191TRANS                                         
079000     PERFORM S03-SKAPA-6193-MID                                           
079100                                                                          
079200     .                                                                    
079300     EJECT                                                                
079400 HD-SKICKA-TRANS SECTION.                                                 
079500                                                                          
079600     IF 6191-IX                > ZERO                                     
079700         PERFORM S02-STARTA-W6T191                                        
079800     END-IF                                                               
079900                                                                          
080000     .                                                                    
080100     EJECT                                                                
080200 S01-FYLL-I-6191TRANS SECTION.                                            
080300                                                                          
080400     MOVE 'W6014700'           TO MOD6191-MID-IDPGM                       
080500     MOVE W-IDDC               TO MOD6191-MID-IDDC                        
080600     MOVE W-IDLOPNRM           TO MOD6191-MID-IDLOPNRM (6191-IX)          
080700     MOVE RAD-IDRADNR          TO MOD6191-MID-IDRADNR  (6191-IX)          
080800     MOVE W-PRARTSTD           TO MOD6191-MID-PRARTSTD (6191-IX)          
080900     MOVE W-KDINLPRIO          TO MOD6191-MID-KDINLPRIO(6191-IX)          
081000     MOVE +0                   TO MOD6191-MID-KVKOLLI  (6191-IX)          
081100     IF RAD-IDRADNR = +1                                                  
081200       MOVE 'J'                TO MOD6191-MID-FLINLI   (6191-IX)          
081300     ELSE                                                                 
081400       MOVE 'N'                TO MOD6191-MID-FLINLI   (6191-IX)          
081500     END-IF                                                               
081600                                                                          
081700     MOVE SPAR-ADINLOMR-OLD      TO MOD6191-MID-ADINLOMR-OLD              
081800                                                   (6191-IX)              
081900     MOVE SPAR-ADINLOMR-NXT-OLD                                           
082000                                 TO MOD6191-MID-ADINLOMR-NXT-OLD          
082100                                                   (6191-IX)              
082200     MOVE SPAR-KDINLSTA-OLD      TO MOD6191-MID-KDINLSTA-OLD              
082300                                                   (6191-IX)              
082400     MOVE SPAR-KVINLART-OLD      TO MOD6191-MID-KVINLART-OLD              
082500                                                   (6191-IX)              
082600                                                                          
082700     MOVE SPAR-ADINLOMR-NEW      TO MOD6191-MID-ADINLOMR-NEW              
082800                                                   (6191-IX)              
082900     MOVE SPAR-ADINLOMR-NXT-NEW  TO MOD6191-MID-ADINLOMR-NXT-NEW          
083000                                                   (6191-IX)              
083100     MOVE SPAR-KDINLSTA-NEW      TO MOD6191-MID-KDINLSTA-NEW              
083200                                                   (6191-IX)              
083300     MOVE SPAR-KVINLART-NEW      TO MOD6191-MID-KVINLART-NEW              
083400                                                   (6191-IX)              
083500     ADD +1                    TO 6191-IX                                 
083600     IF 6191-IX                > MAX-6191-IX                              
083700         PERFORM S02-STARTA-W6T191                                        
083800     END-IF                                                               
083900     .                                                                    
084000     EJECT                                                                
084100 S02-STARTA-W6T191         SECTION.                                       
084200                                                                          
084300     COMPUTE MOD6191-MID-KVPOST = 6191-IX - 1                             
084400     COMPUTE P-TO-P-MSG-KVLL    =  LNG-P-TO-P-PREFIX +                    
084500                                  17 + (MOD6191-MID-KVPOST * 64)          
084600     MOVE 'W6T191X '           TO P-TO-P-MSG-KDTRANS                      
084700     MOVE '6147'               TO P-TO-P-MSG-IDTRANS                      
084800     MOVE MFS-KDMFSFOR         TO P-TO-P-MSG-KDMFSFOR                     
084900                                                                          
085000     MOVE MOD6191-MID-W6I19101 TO P-TO-P-MSG-INDATA                       
085100                                                                          
085200     IF FOERSTA-6191                                                      
085300         PERFORM IMS-ISRT-ALT1-MSG-6191                                   
085400         MOVE NEJ               TO FOERSTA-6191-SW                        
085500      ELSE                                                                
085600         PERFORM IMS-PURG-ALT1-MSG-6191                                   
085700     END-IF                                                               
085800     MOVE +1                   TO 6191-IX                                 
085900     .                                                                    
086000     EJECT                                                                
086100 S03-SKAPA-6193-MID   SECTION.                                            
086200                                                                          
086300     MOVE SPACE                TO MSG-KOM-WMSGKOM                         
           COMPUTE MSG-KOM-KVLL = LENGTH OF MSG-KOM-WMSGKOM                     
086500     MOVE LOW-VALUE            TO MSG-KOM-KDZ1                            
086600     MOVE LOW-VALUE            TO MSG-KOM-KDZ2                            
086700     MOVE SPACE                TO MSG-KOM-KDTRANS                         
086800     MOVE 'W6I19301'           TO MSG-KOM-IDCPYTXT                        
086900     MOVE 'INLEV   '           TO MSG-KOM-IDSNDNOD                        
087000     MOVE 'W6014700'           TO MSG-KOM-IDSNDJOB                        
087100     ACCEPT MSG-KOM-TIREGDAT   FROM DATE                                  
087200     ACCEPT MSG-KOM-TIKLOCK    FROM TIME                                  
087300     MOVE SPACE                TO MSG-KOM-IDMFSMED                        
087400                                                                          
087500     MOVE W-IDLOPNRM           TO MOD6193-MID-IDLOPNRM                    
087600     MOVE WS-IDRADNR           TO MOD6193-MID-IDRADNR                     
087700                                                                          
087800     COMPUTE P-TO-P-MSG-KVLL   =  LNG-P-TO-P-PREFIX + 12                  
087900     MOVE 'W6T193X '           TO P-TO-P-MSG-KDTRANS                      
088000     MOVE '6147'               TO P-TO-P-MSG-IDTRANS                      
088100     MOVE MFS-KDMFSFOR         TO P-TO-P-MSG-KDMFSFOR                     
088200                                                                          
088300     MOVE MOD6193-MID-W6I19301 TO P-TO-P-MSG-INDATA                       
088400                                                                          
088500     CALL W006KOM USING MSG-PCB                                           
088600                        DISP-PCB                                          
088700                        KOMA-PCB                                          
088800                        MSG-KOM-WMSGKOM                                   
088900                        P-TO-P-MSG-IO-AREA-SNUF                           
089000     .                                                                    
089100     EJECT                                                                
089200 MFS-RENSA-FAELT-IN SECTION.                                              
089300                                                                          
089400*    --- ALLA INDATA-FÄLT                                                 
089500     MOVE +1 TO INDX                                                      
089600     MOVE MFS-RENSA-FAELT TO MOD-KVTRANS-IN                               
089700     PERFORM UNTIL INDX > MAX-INDX                                        
089800       MOVE MFS-RENSA-FAELT TO MOD-IDLOPNRM(INDX)                         
089900                               MOD-KVANTMOT(INDX)                         
090000                               MOD-FLSVAR(INDX)                           
090100       ADD +1 TO INDX                                                     
090200     END-PERFORM                                                          
090300                                                                          
090400     .                                                                    
090500     SKIP2                                                                
090600 MFS-ROER-EJ-FAELT-IN SECTION.                                            
090700                                                                          
090800*    --- ALLA INDATA-FÄLT                                                 
090900     MOVE MFS-ROER-EJ-FAELT TO MOD-KVTRANS-IN                             
091000     MOVE +1 TO INDX                                                      
091100     PERFORM UNTIL INDX > MAX-INDX                                        
091200       MOVE MFS-ROER-EJ-FAELT TO MOD-IDLOPNRM(INDX)                       
091300                                 MOD-KVANTMOT(INDX)                       
091400                                 MOD-FLSVAR(INDX)                         
091500       ADD +1 TO INDX                                                     
091600     END-PERFORM                                                          
091700                                                                          
091800     .                                                                    
091900     SKIP2                                                                
092000 MFS-ROER-EJ-FAELT-UT SECTION.                                            
092100                                                                          
092200*    --- ALLA UTDATA-FÄLT                                                 
092300     MOVE MFS-ROER-EJ-FAELT TO MOD-KVTRANS-UT                             
092400                                                                          
092500     .                                                                    
092600     EJECT                                                                
092700* --- IMS SEKTIONER ---                                                   
092800     SKIP3                                                                
092900 IMS-GET-MSG SECTION.                                                     
093000                                                                          
093100     MOVE '  QC' TO GODK-STATUSKODER                                      
093200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
093300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
093400     PERFORM IMS-STATUSKONTROLL                                           
093500     .                                                                    
093600     SKIP3                                                                
093700 IMS-INSERT-MSG SECTION.                                                  
093800                                                                          
093810     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
093820       MOVE '0' TO MFS-KDHUVOMR                                           
094100     END-IF                                                               
094200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
094300     MOVE SPACE TO GODK-STATUSKODER                                       
094400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
094500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
094600     PERFORM IMS-STATUSKONTROLL                                           
094700     .                                                                    
094800     EJECT                                                                
094900 IMS-ISRT-ALT1-MSG-6191  SECTION.                                         
095000     MOVE SPACE TO GODK-STATUSKODER                                       
095100     CALL  CBLTDLI  USING ISRT ALT1-PCB P-TO-P-MSG-IO-AREA-SNUF           
095200     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
095300     PERFORM IMS-STATUSKONTROLL                                           
095400     .                                                                    
095500     SKIP3                                                                
095600 IMS-PURG-ALT1-MSG-6191  SECTION.                                         
095700     MOVE SPACE TO GODK-STATUSKODER                                       
095800     CALL  CBLTDLI  USING PURG ALT1-PCB P-TO-P-MSG-IO-AREA-SNUF           
095900     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
096000     PERFORM IMS-STATUSKONTROLL                                           
096100     .                                                                    
096200     EJECT                                                                
096300 IMS-GU-W6D1-INLA11 SECTION.                                              
096400     STRING 'W6INLA11(W6D1BSEQ =' W-IDLOPNRM-X                            
096500                    '&IDDC     =' W-IDDC ')'                              
096600          DELIMITED BY SIZE INTO SSA1                                     
096700     MOVE '  GE' TO GODK-STATUSKODER                                      
096800     CALL CBLTDLI USING GU INLASEQ-PCB DLI-IO-AREA SSA1                   
096900     MOVE INLASEQ-STATUS-CODE TO STATUS-WS                                
097000     PERFORM IMS-STATUSKONTROLL                                           
097100     .                                                                    
097200     SKIP2                                                                
097300 IMS-GNP-W6D1-INLA21-OKVAL SECTION.                                       
097400     MOVE 'W6INLA21 ' TO SSA1                                             
097500     MOVE '  GE' TO GODK-STATUSKODER                                      
097600     CALL CBLTDLI USING GNP INLASEQ-PCB DLI-IO-AREA SSA1                  
097700     MOVE INLASEQ-STATUS-CODE TO STATUS-WS                                
097800     PERFORM IMS-STATUSKONTROLL                                           
097900     .                                                                    
098000     SKIP3                                                                
098100 IMS-GNP-W6D1-INLA21-LAST SECTION.                                        
098200     MOVE 'W6INLA21*L' TO SSA1                                            
098300     MOVE '  GE' TO GODK-STATUSKODER                                      
098400     CALL CBLTDLI USING GNP INLASEQ-PCB DLI-IO-AREA SSA1                  
098500     MOVE INLASEQ-STATUS-CODE TO STATUS-WS                                
098600     PERFORM IMS-STATUSKONTROLL                                           
098700     .                                                                    
098800     SKIP3                                                                
098900 IMS-GHNP-W6D1-INLA21-KVAL SECTION.                                       
099000     STRING 'W6INLA21*F(IDRADNR  =' W-IDRADNR-X ')'                       
099100          DELIMITED BY SIZE INTO SSA1                                     
099200     MOVE '  ' TO GODK-STATUSKODER                                        
099300     CALL CBLTDLI USING GHNP INLASEQ-PCB DLI-IO-AREA SSA1                 
099400     MOVE INLASEQ-STATUS-CODE TO STATUS-WS                                
099500     PERFORM IMS-STATUSKONTROLL                                           
099600     .                                                                    
099700     SKIP3                                                                
099800 IMS-DLET-W6D1-INLA21 SECTION.                                            
099900                                                                          
100000     MOVE '  ' TO GODK-STATUSKODER                                        
100100     CALL CBLTDLI USING DLET INLASEQ-PCB DLI-IO-AREA                      
100200     MOVE INLASEQ-STATUS-CODE TO STATUS-WS                                
100300     PERFORM IMS-STATUSKONTROLL                                           
100400     .                                                                    
100500     SKIP2                                                                
100600 IMS-ISRT-W6D1-INLA21 SECTION.                                            
100700     STRING 'W6INLA11(W6D1BSEQ =' W-IDLOPNRM-X ')'                        
100800          DELIMITED BY SIZE INTO SSA1                                     
100900     MOVE 'W6INLA21 ' TO SSA2                                             
101000     MOVE '  ' TO GODK-STATUSKODER                                        
101100     CALL CBLTDLI USING ISRT INLASEQ-PCB DLI-IO-AREA SSA1 SSA2            
101200     MOVE INLASEQ-STATUS-CODE TO STATUS-WS                                
101300     PERFORM IMS-STATUSKONTROLL                                           
101400     .                                                                    
101500     EJECT                                                                
101510 IMS-GU-UPFA-01  SECTION.                                                 
101520     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
101530          DELIMITED BY SIZE INTO SSA1                                     
101540     MOVE '  GE' TO GODK-STATUSKODER                                      
101550     CALL CBLTDLI USING GU UPFA-PCB DLI-IO-UPFA SSA1                      
101560     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
101570     PERFORM IMS-STATUSKONTROLL                                           
101580     .                                                                    
101590     SKIP3                                                                
101591 IMS-GNP-UPFA11 SECTION.                                                  
101592                                                                          
101593     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
101594          DELIMITED BY SIZE INTO SSA1                                     
101595     MOVE 'W6UPFA11 ' TO SSA2                                             
101596     MOVE '  GE' TO GODK-STATUSKODER                                      
101597     CALL CBLTDLI USING GNP UPFA-PCB DLI-IO-AREA-UPFA11 SSA1 SSA2         
101598     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
101599     PERFORM IMS-STATUSKONTROLL                                           
101600     .                                                                    
101601     SKIP3                                                                
101602 IMS-GNP-UPFA12 SECTION.                                                  
101603                                                                          
101604     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
101605          DELIMITED BY SIZE INTO SSA1                                     
101606     MOVE 'W6UPFA12 ' TO SSA2                                             
101607     MOVE '  GE' TO GODK-STATUSKODER                                      
101608     CALL CBLTDLI USING GNP UPFA-PCB DLI-IO-AREA-UPFA12 SSA1 SSA2         
101609     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
101610     PERFORM IMS-STATUSKONTROLL                                           
101611     .                                                                    
101612     SKIP3                                                                
101620 IMS-STATUSKONTROLL SECTION.                                              
101700                                                                          
101800     SET STATUS-IX TO 1                                                   
101900     SEARCH GODK-STATUS                                                   
102000       AT END                                                             
102100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
102200         DELIMITED BY SIZE INTO FELTEXT                                   
102300         CALL FELLOG                                                      
102400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
102500         CONTINUE                                                         
102600     END-SEARCH                                                           
102700     .                                                                    
