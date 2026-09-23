000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6014500.                                                
000400*AUTHOR.         LARS THELL.                                              
000500*DATE-WRITTEN.   92/06/18.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        RAPPORTERING AV DIVERSEKOLLI. VISAR BARA DIVERSEKOLLIN           
001100*        SOM INNEHÅLLER MAX 12 ST ARTIKLAR.                               
001200*                                                                         
001300*        PROGRAMMET          UPPDATERAR W6INLA (W6D1)                     
001400*        PROGRAMMET          LÄSER      W6PLAA (W6G1)                     
001500*        PROGRAMMET          LÄSER      W6UPFA (W6L1)                     
001600*    SUB PROGRAMMET W611PMRK UPPDATERAR W6INLA (W6D1)                     
001700*                            LÄSER      W6PLAA (W6G1)                     
001800*    SUB PROGRAMMET W006KOM  UPPDATERAR WLKOMA (WDP8)                     
001900*                                                                         
002000*    INDATA.                                                              
002100*        TRANSAKTION: W6T145                                              
002200*        MID:         W6I14501                                            
002300*                                                                         
002400*    UTDATA.                                                              
002500*        MOD:         W6O14501                                            
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100 WORKING-STORAGE SECTION.                                                 
003200                                                                          
003300*    -- CHECKED BY WY2000                                                 
003400 77  IDPGM                       PIC X(08)   VALUE 'W6014500'.            
003500                                                                          
003600*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003700 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003800                                                                          
003900 77  JA                          PIC X       VALUE 'J'.                   
004000 77  NEJ                         PIC X       VALUE 'N'.                   
004100                                                                          
004200*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004300 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004400 77  MAX-INDX                    PIC S9(4)  VALUE +12   COMP SYNC.        
004500 77  6191-IX                     PIC S9(4)  VALUE +0    COMP SYNC.        
004600 77  MAX-6191-IX                 PIC S9(9)  VALUE +24   COMP SYNC.        
004700 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004800 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +1167 COMP SYNC.        
004900 77  LNG-P-TO-P-PREFIX           PIC S9(4)  VALUE +17   COMP SYNC.        
005000                                                                          
005100 77  W-SPAR-IDLOPNRM             PIC S9(9)   VALUE ZERO  COMP-3.          
005200 77  W-KVINLART-UPD              PIC S9(7)   VALUE ZERO  COMP-3.          
005300 77  W-RADRAEKNARE               PIC S9(3)   VALUE ZERO  COMP-3.          
005400 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005500 77  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
005600                                                                          
005700*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005800                                                                          
005900 77  WS-IDLEVNR-KOLLI            PIC X(5)    VALUE SPACE.                 
006000 77  WS-IDOKOLLI                 PIC X(9)    VALUE SPACE.                 
006100 77  WS-IDLOPNRM                 PIC X(9)    VALUE SPACE.                 
006200                                                                          
006300 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006400     88  INDATA-OK                           VALUE 'J'.                   
006500     88  INDATA-FEL                          VALUE 'N'.                   
006600                                                                          
006700 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006800     88  NYCKLAR-OK                          VALUE 'J'.                   
006900     88  NYCKLAR-FEL                         VALUE 'N'.                   
007000                                                                          
007100 77  RADER-SW                    PIC X       VALUE 'J'.                   
007200     88  RADER-SAKNAS                        VALUE 'N'.                   
007300                                                                          
007400 77  RAD-SW                      PIC X       VALUE 'N'.                   
007500     88  RAD-FINNS                           VALUE 'J'.                   
007600     88  RAD-SAKNAS                          VALUE 'N'.                   
007700                                                                          
007800 77  FOERSTA-6191-SW             PIC X       VALUE 'J'.                   
007900     88  FOERSTA-6191                        VALUE 'J'.                   
008000                                                                          
008100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008200     88  EGEN-MID                            VALUE '6145'.                
008300     88  GODK-MID                            VALUE '6143' '6144'          
008400                                                   '6145'.                
008500     88  HELP-MID                            VALUE '0551'.                
008600     EJECT                                                                
008700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008800*      --- VALID IDDC CODES                                               
008900*                                                                         
009000*01    -COPY WWDC99                                                       
009100       EJECT                                                              
009200 01  GENERELLA-SUBPROGRAM.                                                
009300     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
009400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009600     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR'.             
009700     03  W611PMRK                PIC X(8)    VALUE 'W611PMRK'.            
009800     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
009900     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
010000     EJECT                                                                
010100*01 -COPY WMSGINIT                                                        
010200     SKIP3                                                                
010300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
010400*01 -COPY WMEDAREA                                                        
010500     SKIP3                                                                
010600 01  MESSAGE-CODES.                                                       
010700     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
010800     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
010900     03  ERR-NO-UPDATE           PIC X(3)    VALUE '007'.                 
011000     03  ERR-MISSING             PIC X(3)    VALUE '010'.                 
011100     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
011200     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
011300     03  ERR-KIT-CASE            PIC X(3)    VALUE '185'.                 
011400     03  ERR-QUALITY             PIC X(3)    VALUE '189'.                 
011500     03  ERR-USE-SCREEN-6141     PIC X(3)    VALUE '191'.                 
011600     03  ERR-RTYP-3-OR-77        PIC X(3)    VALUE '227'.                 
011700     03  ERR-CHECK-QUAL-FIRST    PIC X(3)    VALUE '228'.                 
011800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
011900     03  ERR-PLACE-MISSING       PIC X(3)    VALUE '764'.                 
012000     03  ERR-CONTROL-NOT-COMPL   PIC X(3)    VALUE '215'.                 
012100     03  ERR-WEIGHT-MISSING      PIC X(3)    VALUE '792'.                 
012200     03  ERR-VOLUME-MISSING      PIC X(3)    VALUE '793'.                 
012300     03  ERR-ORIGIN-MISSING      PIC X(3)    VALUE '794'.                 
012400     EJECT                                                                
012500                                                                          
012600*    --- PARAMETRAR TILL SUBPROGRAM W611PMRK                              
012700*01 -COPY W611PMRK                                                        
012800     EJECT                                                                
012900*    --- SPAR AREA  FÖR INLA21                                            
013000*01  -COPY W6D121   -PRE SPAR-                                            
013100     EJECT                                                                
013200 01  MOD-TABELL.                                                          
013300    03 W-MOD-TAB          OCCURS 12 INDEXED BY TAB-IX.                    
013400        05 TAB-SORT-FAELT.                                                
013500          07 TAB-ADLAGOMR        PIC S9(3)  COMP-3.                       
013600          07 TAB-ADGANG          PIC S9(3)  COMP-3.                       
013700          07 TAB-ADPLATS         PIC S9(5)  COMP-3.                       
013800          07 TAB-IDARTNR         PIC S9(9)  COMP-3.                       
013900        05 TAB-BEART             PIC X(10).                               
014000        05 TAB-KVINLART          PIC S9(7)  COMP-3.                       
014100        05 TAB-KVINLART-VOR      PIC S9(7)  COMP-3.                       
014200        05 TAB-IDLOPNRM          PIC S9(9)  COMP-3.                       
014300        05 TAB-IDRADNR           PIC S9(3)  COMP-3.                       
014400        05 TAB-BEFARLIG          PIC X.                                   
014500        05 TAB-KDKLIPRI          PIC X.                                   
014600        05 TAB-FLKVAFEL          PIC X.                                   
014700        05 TAB-FLKVAKAR          PIC X.                                   
014800        05 TAB-FLSATS            PIC X.                                   
014900                                                                          
015000 01  TABENTRY-PARM.                                                       
015100     03  TABENTRY-LNGD           PIC S9(9)   COMP.                        
015200     03  ANTAL-ENTRY             PIC S9(9)   COMP.                        
015300     03  SORTBGP-LNGD            PIC S9(9)   COMP.                        
015400                                                                          
015500 01  ANTAL-I-TABELL              PIC S9(9)   COMP VALUE ZERO.             
015600     EJECT                                                                
015700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
015800*                                                                         
015900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
016000     SKIP3                                                                
016100*01  MID -COPY W6I14501                                                   
016200     EJECT                                                                
016300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
016400     SKIP3                                                                
016500*01  -COPY WMSGAREA                                                       
016600     EJECT                                                                
016700     03  MOD REDEFINES MSG-AREA.                                          
016800*      05  -COPY W6O14501                                                 
016900     EJECT                                                                
017000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
017100     SKIP3                                                                
017200*01  -COPY WMFSAREA                                                       
017300     EJECT                                                                
017400 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
017500     SKIP3                                                                
017600 01  KOM-MSG-IO-AREA.                                                     
017700*03  -COPY WMSGKOM                                                        
017800     EJECT                                                                
017900 01  FILLER                   PIC X(16)   VALUE 'MSG/KOM-AREA'.           
018000     SKIP2                                                                
018100*01  -COPY WMSGSNUF           -PRE P-TO-P-                                
018200                                                                          
018300     EJECT                                                                
018400 01      FILLER                  PIC X(24)   VALUE                        
018500                                 'MOD6191-MID-W6I19101'.                  
018600     SKIP2                                                                
018700     -COPY W6I19101 -PRE MOD6191-                                         
018800     EJECT                                                                
018900 01      FILLER                  PIC X(24)   VALUE                        
019000                                 'MOD6193-MID-W6I19301'.                  
019100     SKIP2                                                                
019200     -COPY W6I19301 -PRE MOD6193-                                         
019300     EJECT                                                                
019400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
019500*                                                                         
019600                                                                          
019700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
019800     SKIP3                                                                
019900 01  NYCKLAR-TILL-DLI.                                                    
020000     03  W-W6D1CSEQ-X.                                                    
020100        05  W-D1CSEQ-IDLEVNR-KOLLI   PIC  X(5) VALUE SPACE.               
020200        05  W-D1CSEQ-IDOKOLLI        PIC  9(9) VALUE ZERO.                
020300                                                                          
020400     03  W-IDLEVNR-KOLLI-X.                                               
020500        05  W-IDLEVNR-KOLLI          PIC  X(5) VALUE SPACE.               
020600                                                                          
020700     03  W-IDOKOLLI-X.                                                    
020800        05  W-IDOKOLLI               PIC  9(9) VALUE ZERO.                
020900                                                                          
021000     03  W-IDRADNR-X.                                                     
021100        05  W-IDRADNR                PIC S9(5) VALUE ZERO COMP-3.         
021200                                                                          
021300     03  W-W6D1BSEQ-X.                                                    
021400         05  W-D1BSEQ-IDLOPNRM   PIC S9(9)    COMP-3 VALUE ZERO.          
021500                                                                          
021600     03  W-W6GXKEY-6005-X.                                                
021700         05  W-6005-IDHTYP       PIC X(4)    VALUE '6005'.                
021800         05  W-6005-IDDC         PIC X(2)    VALUE SPACE.                 
021900         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
022000                                                                          
022100     03  W-W6GXKEY-6006-X.                                                
022200         05  W-6006-ADINLOMR     PIC X(4)    VALUE SPACE.                 
022300         05  FILLER              PIC X(1)    VALUE LOW-VALUE.             
022400     03  W-IDDC                  PIC X(2)    VALUE SPACE.                 
022500                                                                          
022600*--------W6L101                                                           
022700     03  W-IDLOPNRM-X.                                                    
022800         05  W-IDLOPNRM              PIC S9(9) COMP-3 VALUE ZERO.         
022900                                                                          
023000     SKIP2                                                                
023100*    --- STATUS-KOD FRÅN IMS                                              
023200 01  STATUS-WS                   PIC XX.                                  
023300     88  SEGMENT-FINNS                       VALUE '  '.                  
023400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
023500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
023600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
023700     SKIP2                                                                
023800 01  GODK-STATUSKODER.                                                    
023900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
024000     SKIP3                                                                
024100 01  SSA1                        PIC X(64).                               
024200 01  SSA2                        PIC X(64).                               
024300     EJECT                                                                
024400*    --- IMS FUNKTIONSKODER                                               
024500*01  -COPY W0003                                                          
024600     EJECT                                                                
024700*    ---  DLI INPUT-OUTPUT AREA                                           
024800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
024900     SKIP3                                                                
025000 01  DLI-IO-AREA1.                                                        
025100     03  IO-AREA1                PIC X(150)  VALUE SPACE.                 
025200     SKIP3                                                                
025300     03  W6INLA21 REDEFINES IO-AREA1.                                     
025400*        05  -COPY W6D111                                                 
025500     EJECT                                                                
025600 01  DLI-IO-AREA2.                                                        
025700     03  IO-AREA2                PIC X(150)  VALUE SPACE.                 
025800     SKIP3                                                                
025900     03  W6INLA21 REDEFINES IO-AREA2.                                     
026000*        05  -COPY W6D121                                                 
026100     EJECT                                                                
026200 01  DLI-IO-AREA3.                                                        
026300     03  IO-AREA3                PIC X(150)  VALUE SPACE.                 
026400     SKIP3                                                                
026500     03  W6PLAA11 REDEFINES IO-AREA3.                                     
026600*        05  -COPY W6GX6006 -PRE PLAA-                                    
026700     EJECT                                                                
026800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-UPFA'.         
026900     SKIP3                                                                
027000 01  DLI-IO-UPFA.                                                         
027100     03  IO-UPFA                PIC X(100)  VALUE SPACE.                  
027200     03  W6UPFA01 REDEFINES IO-UPFA.                                      
027300*        05  -COPY W6L101                                                 
027400     EJECT                                                                
027500 01  DLI-IO-AREA-UPFA11.                                                  
027600     03  W6UPFA11.                                                        
027700*        05  -COPY W6L111                                                 
027800     SKIP3                                                                
027900 01  DLI-IO-AREA-UPFA12.                                                  
028000     03  W6UPFA12.                                                        
028100*        05  -COPY W6L112                                                 
028200     EJECT                                                                
028300 LINKAGE SECTION.                                                         
028400                                                                          
028500*01  -COPY W0009   -PRE MSG-                                              
028600     EJECT                                                                
028700*01  -COPY W0009   -PRE ALT1-                                             
028800     EJECT                                                                
028900*01  -COPY W0009   -PRE DISP-                                             
029000     EJECT                                                                
029100*01  -COPY W0008  -PRE USEA-                                              
029200     05  FILLER                  PIC X.                                   
029300     EJECT                                                                
029400*01  -COPY W0008  -PRE INLA1-                                             
029500     05  FILLER                  PIC X.                                   
029600     EJECT                                                                
029700*01  -COPY W0008  -PRE INLA2-                                             
029800     05  FILLER                  PIC X.                                   
029900     EJECT                                                                
030000*01  -COPY W0008  -PRE PLAA-                                              
030100     05  FILLER                  PIC X.                                   
030200     EJECT                                                                
030300*01  -COPY W0008  -PRE UPFA-                                              
030400     05  FILLER                  PIC X.                                   
030500     SKIP3                                                                
030600                                                                          
030700**  PCB'ER FÖR SUBPGM                                                     
030800 01  PMRK-INLB-PCB               PIC X.                                   
030900                                                                          
031000 01  PMRK-INLC-PCB               PIC X.                                   
031100                                                                          
031200 01  PMRK-PLAA-PCB               PIC X.                                   
031300                                                                          
031400 01  KOM-KOMA-PCB                PIC X.                                   
031500                                                                          
031600     EJECT                                                                
031700 PROCEDURE DIVISION  USING MSG-PCB ALT1-PCB DISP-PCB USEA-PCB             
031800                           INLA1-PCB                                      
031900                           INLA2-PCB PLAA-PCB UPFA-PCB                    
032000                           PMRK-INLB-PCB PMRK-INLC-PCB                    
032100                           PMRK-PLAA-PCB                                  
032200                           KOM-KOMA-PCB.                                  
032300     ENTRY 'DLITCBL' USING MSG-PCB ALT1-PCB DISP-PCB USEA-PCB             
032400                           INLA1-PCB                                      
032500                           INLA2-PCB PLAA-PCB UPFA-PCB                    
032600                           PMRK-INLB-PCB PMRK-INLC-PCB                    
032700                           PMRK-PLAA-PCB                                  
032800                           KOM-KOMA-PCB.                                  
032900                                                                          
033000     PERFORM IMS-GET-MSG                                                  
033100     IF SEGMENT-FINNS                                                     
033200       PERFORM A-INIT                                                     
033300       PERFORM B-KOLLA-NYCKLAR                                            
033400       IF NYCKLAR-OK                                                      
033500         IF MFS-UPDATE                                                    
033600           PERFORM G-KOLLA-INPUT                                          
033700           IF INDATA-OK                                                   
033800             PERFORM H-UPPDATERA                                          
033900           END-IF                                                         
034000         ELSE                                                             
034100           PERFORM E-SAMMA-SIDA                                           
034200         END-IF                                                           
034300         IF INDATA-OK                                                     
034400           PERFORM F-LAES-VISA-INFO                                       
034500         END-IF                                                           
034600       END-IF                                                             
034700       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
034800       PERFORM IMS-INSERT-MSG                                             
034900     END-IF                                                               
035000                                                                          
035100     MOVE ZERO TO RETURN-CODE                                             
035200     GOBACK                                                               
035300     .                                                                    
035400     EJECT                                                                
035500 A-INIT SECTION.                                                          
035600                                                                          
035700     IF MSG-DUBBLA-TRANSKODER                                             
035800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I14501                 
035900       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
036000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
036100     ELSE                                                                 
036200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I14501                  
036300       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
036400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
036500     END-IF                                                               
036600                                                                          
036700     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
036800     MOVE MSG-IDPFK TO MFS-IDPFK                                          
036900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
037000                                                                          
037100     MOVE LOW-VALUE TO MSG-AREA                                           
037200     MOVE 'W6O145N1' TO MFS-IDMOD                                         
037300     MOVE '6145' TO MOD-IDTRANS                                           
037400     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
037500     ACCEPT DAGENS-DATUM FROM DATE                                        
037600                                                                          
037700     IF MFS-ENTER OR HELP-MID                                             
037800       CONTINUE                                                           
037900     ELSE                                                                 
038000       MOVE SPACE TO MFS-KDTRTYP                                          
038100       MOVE '7' TO MFS-IDPFK                                              
038200     END-IF                                                               
038300                                                                          
038400     SET TAB-IX                TO +1                                      
038500     PERFORM UNTIL TAB-IX      >  MAX-INDX                                
038600         MOVE ZERO             TO TAB-ADLAGOMR    (TAB-IX)                
038700                                  TAB-ADPLATS     (TAB-IX)                
038800                                  TAB-ADGANG      (TAB-IX)                
038900                                  TAB-IDARTNR     (TAB-IX)                
039000                                  TAB-KVINLART    (TAB-IX)                
039100                                  TAB-KVINLART-VOR(TAB-IX)                
039200                                  TAB-IDLOPNRM    (TAB-IX)                
039300                                  TAB-IDRADNR     (TAB-IX)                
039400         MOVE SPACE            TO TAB-BEART       (TAB-IX)                
039500                                  TAB-BEFARLIG    (TAB-IX)                
039600                                  TAB-KDKLIPRI    (TAB-IX)                
039700                                  TAB-FLKVAFEL    (TAB-IX)                
039800                                  TAB-FLKVAKAR    (TAB-IX)                
039900                                  TAB-FLSATS      (TAB-IX)                
040000         SET TAB-IX UP BY +1                                              
040100     END-PERFORM                                                          
040200     MOVE JA                   TO RADER-SW                                
040300                                  FOERSTA-6191-SW                         
040400     PERFORM AA-INIT-NYCKLAR                                              
040500                                                                          
040600     IF MSGI-IDLAND-SPR = 'GB'                                            
040700       MOVE +2                 TO SPRAK-IX                                
040800       MOVE 'GB '              TO MED-IDSKYLT                             
040900     ELSE                                                                 
041000       MOVE +1                 TO SPRAK-IX                                
041100       MOVE 'S  '              TO MED-IDSKYLT                             
041200     END-IF                                                               
041300     .                                                                    
041400     EJECT                                                                
041500*----------------------------------------------------------------*        
041600 AA-INIT-NYCKLAR SECTION.                                                 
041700                                                                          
041800     MOVE ALL '+' TO MSGI-WMSGINIT                                        
041900     MOVE '001'                  TO MSGI-KDCALL                           
042000     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
042100     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
042200     MOVE '6145'                 TO MSGI-IDTRANS                          
042300     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
042400     .                                                                    
042500     EJECT                                                                
042600 B-KOLLA-NYCKLAR SECTION.                                                 
042700                                                                          
042800     MOVE JA TO NYCKLAR-SW                                                
042900                                                                          
043000     PERFORM BA-KOLLA-IDLEVNR-KOLLI                                       
043100     PERFORM BB-KOLLA-IDOKOLLI                                            
043200     PERFORM BC-FLYTTA-IDLOPNRM                                           
043300     PERFORM BD-KOLLA-IDDC                                                
043400                                                                          
043500     IF GODK-MID OR NYCKLAR-OK                                            
043600         MOVE WS-IDLEVNR-KOLLI TO MOD-IDLEVNR-KOLLI-UT                    
043700         MOVE WS-IDOKOLLI      TO MOD-IDOKOLLI-UT                         
043800         INSPECT MOD-IDOKOLLI-UT REPLACING LEADING ZERO BY SPACE          
043900         MOVE WS-IDLOPNRM      TO MOD-IDLOPNRM-UT                         
044000         INSPECT MOD-IDLOPNRM-UT REPLACING LEADING ZERO BY SPACE          
044100         MOVE WS-IDDC          TO MOD-IDDC-UT                             
044200     ELSE                                                                 
044300         MOVE MFS-RENSA-FAELT  TO MOD-IDLEVNR-KOLLI-UT                    
044400                                  MOD-IDOKOLLI-UT                         
044500                                  MOD-IDLOPNRM-UT                         
044600                                  MOD-IDDC-UT                             
044700     END-IF                                                               
044800                                                                          
044900     IF NYCKLAR-FEL                                                       
045000         MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                               
045100         CALL WMEDKONV USING MED-WMEDAREA                                 
045200         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
045300         PERFORM MFS-RENSA-FAELT-IN                                       
045400         PERFORM MFS-RENSA-FAELT-UT                                       
045500     END-IF                                                               
045600     .                                                                    
045700     EJECT                                                                
045800 BA-KOLLA-IDLEVNR-KOLLI SECTION.                                          
045900                                                                          
046000     MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-KOLLI-IN                         
046100                                                                          
046200     IF MID-IDLEVNR-KOLLI-IN   = ALL '+'                                  
046300         MOVE MID-IDLEVNR-KOLLI-UT   TO WS-IDLEVNR-KOLLI                  
046400      ELSE                                                                
046500         MOVE MID-IDLEVNR-KOLLI-IN TO WS-IDLEVNR-KOLLI                    
046600         MOVE '7'              TO MFS-IDPFK                               
046700         MOVE SPACE            TO MFS-KDTRTYP                             
046800     END-IF                                                               
046900                                                                          
047000     IF WS-IDLEVNR-KOLLI NOT = SPACE                                      
047100         CONTINUE                                                         
047200     ELSE                                                                 
047300         MOVE NEJ              TO NYCKLAR-SW                              
047400     END-IF                                                               
047500     .                                                                    
047600     EJECT                                                                
047700 BB-KOLLA-IDOKOLLI SECTION.                                               
047800                                                                          
047900     MOVE MFS-RENSA-FAELT      TO MOD-IDOKOLLI-IN                         
048000                                                                          
048100     IF MID-IDOKOLLI-IN        = ALL '+'                                  
048200         MOVE MID-IDOKOLLI-UT  TO WS-IDOKOLLI                             
048300         INSPECT WS-IDOKOLLI REPLACING LEADING SPACE BY ZERO              
048400      ELSE                                                                
048500         MOVE MID-IDOKOLLI-IN  TO WS-IDOKOLLI                             
048600         MOVE '7'              TO MFS-IDPFK                               
048700         MOVE SPACE            TO MFS-KDTRTYP                             
048800     END-IF                                                               
048900                                                                          
049000     IF WS-IDOKOLLI NUMERIC AND WS-IDOKOLLI > ZERO                        
049100         CONTINUE                                                         
049200     ELSE                                                                 
049300         MOVE NEJ              TO NYCKLAR-SW                              
049400     END-IF                                                               
049500     .                                                                    
049600     EJECT                                                                
049700 BC-FLYTTA-IDLOPNRM SECTION.                                              
049800                                                                          
049900     MOVE MFS-RENSA-FAELT      TO MOD-IDLOPNRM-IN                         
050000                                                                          
050100     IF MID-IDLOPNRM-IN        = ALL '+'                                  
050200         MOVE MID-IDLOPNRM-UT  TO WS-IDLOPNRM                             
050300         INSPECT WS-IDLOPNRM REPLACING LEADING SPACE BY ZERO              
050400      ELSE                                                                
050500         MOVE MID-IDLOPNRM-IN  TO WS-IDLOPNRM                             
050600         MOVE '7'              TO MFS-IDPFK                               
050700         MOVE SPACE            TO MFS-KDTRTYP                             
050800     END-IF                                                               
050900     .                                                                    
051000     EJECT                                                                
051100 BD-KOLLA-IDDC     SECTION.                                               
051200                                                                          
051300     MOVE MFS-RENSA-FAELT      TO MOD-IDDC-IN                             
051400                                                                          
051500     IF MID-IDDC-IN        = ALL '+'                                      
051600         MOVE MSGI-IDDC    TO WS-IDDC                                     
051700      ELSE                                                                
051800         MOVE MID-IDDC-IN  TO WS-IDDC                                     
051900         MOVE '7'              TO MFS-IDPFK                               
052000         MOVE SPACE            TO MFS-KDTRTYP                             
052100     END-IF                                                               
052200                                                                          
052300     IF CDC                                                               
052400         MOVE WS-IDDC       TO W-IDDC                                     
052500                               W-6005-IDDC                                
052600     ELSE                                                                 
052700         MOVE NEJ              TO NYCKLAR-SW                              
052800     END-IF                                                               
052900     .                                                                    
053000     EJECT                                                                
053100 E-SAMMA-SIDA SECTION.                                                    
053200                                                                          
053300     IF EGEN-MID OR HELP-MID                                              
053400       IF MID-INPUT = ALL '+'                                             
053500         PERFORM MFS-RENSA-FAELT-IN                                       
053600       ELSE                                                               
053700         MOVE INF-PRESS-PF11   TO MED-IDMFSINF                            
053800         CALL WMEDKONV USING MED-WMEDAREA                                 
053900         MOVE MED-MFSINF       TO MOD-TEMFSINF                            
054000         PERFORM EA-MID-INDATA-TILL-MOD                                   
054100       END-IF                                                             
054200     ELSE                                                                 
054300       PERFORM MFS-RENSA-FAELT-IN                                         
054400     END-IF                                                               
054500     .                                                                    
054600     EJECT                                                                
054700 EA-MID-INDATA-TILL-MOD SECTION.                                          
054800                                                                          
054900     IF MID-FLKLAR             =  ALL '+'                                 
055000         MOVE MFS-RENSA-FAELT  TO MOD-FLKLAR                              
055100      ELSE                                                                
055200         MOVE MID-FLKLAR       TO MOD-FLKLAR                              
055300         MOVE MFS-ADD-LAES-IN-FAELT                                       
055400                               TO MOD-FLKLAR-ATTR                         
055500     END-IF                                                               
055600                                                                          
055700     IF MID-IDANSTNR           =  ALL '+'                                 
055800         MOVE MFS-RENSA-FAELT  TO MOD-IDANSTNR                            
055900      ELSE                                                                
056000         MOVE MID-IDANSTNR     TO MOD-IDANSTNR                            
056100         MOVE MFS-ADD-LAES-IN-FAELT                                       
056200                               TO MOD-IDANSTNR-ATTR                       
056300     END-IF                                                               
056400                                                                          
056500     MOVE +1                        TO INDX                               
056600     PERFORM UNTIL INDX             >  MAX-INDX                           
056700         IF MID-KDCMDVAL-RAD(INDX)  =  ALL '+'                            
056800             MOVE MFS-RENSA-FAELT   TO MOD-KDCMDVAL-RAD(INDX)             
056900          ELSE                                                            
057000             MOVE MID-KDCMDVAL-RAD(INDX)                                  
057100                                    TO MOD-KDCMDVAL-RAD(INDX)             
057200             MOVE MFS-ADD-LAES-IN-FAELT                                   
057300                                    TO MOD-KDCMDVAL-RAD-ATTR(INDX)        
057400         END-IF                                                           
057500                                                                          
057600         IF MID-KVINLART-UPD(INDX)  =  ALL '+'                            
057700             MOVE MFS-RENSA-FAELT   TO MOD-KVINLART-UPD(INDX)             
057800          ELSE                                                            
057900             MOVE MID-KVINLART-UPD(INDX)                                  
058000                                    TO MOD-KVINLART-UPD(INDX)             
058100             MOVE MFS-ADD-LAES-IN-FAELT                                   
058200                                    TO MOD-KVINLART-UPD-ATTR(INDX)        
058300         END-IF                                                           
058400                                                                          
058500         IF MID-ADINLOMR-NXT-UPD(INDX) =  ALL '+'                         
058600             MOVE MFS-RENSA-FAELT  TO MOD-ADINLOMR-NXT-UPD(INDX)          
058700          ELSE                                                            
058800             MOVE MID-ADINLOMR-NXT-UPD(INDX)                              
058900                                   TO MOD-ADINLOMR-NXT-UPD(INDX)          
059000             MOVE MFS-ADD-LAES-IN-FAELT TO                                
059100                                  MOD-ADINLOMR-NXT-UPD-ATTR(INDX)         
059200         END-IF                                                           
059300                                                                          
059400         ADD +1                     TO INDX                               
059500     END-PERFORM                                                          
059600     .                                                                    
059700     EJECT                                                                
059800 F-LAES-VISA-INFO SECTION.                                                
059900                                                                          
060000     PERFORM FA-LAES-RADDATA                                              
060100                                                                          
060200     IF RADER-SAKNAS  OR                                                  
060300       (INDX                   > MAX-INDX AND SEGMENT-FINNS)              
060400         IF MFS-UPDATE                                                    
060500             CONTINUE                                                     
060600          ELSE                                                            
060700             IF RADER-SAKNAS                                              
060800                 MOVE ERR-MISSING          TO MED-IDMFSFEL                
060900              ELSE                                                        
061000                 MOVE ERR-USE-SCREEN-6141  TO MED-IDMFSFEL                
061100             END-IF                                                       
061200             CALL WMEDKONV USING MED-WMEDAREA                             
061300             MOVE MED-MFSFEL   TO MOD-TEMFSFEL                            
061400         END-IF                                                           
061500         PERFORM MFS-RENSA-FAELT-UT                                       
061600         PERFORM MFS-RENSA-FAELT-IN                                       
061700      ELSE                                                                
061800         PERFORM FB-SORTERA-MOD-TABELL                                    
061900         PERFORM FC-FLYTTA-TABELL-TILL-MOD                                
062000     END-IF                                                               
062100                                                                          
062200     IF MFS-UPDATE                                                        
062300         MOVE MFS-ROER-EJ-FAELT     TO MOD-FLKLAR                         
062400                                       MOD-IDANSTNR                       
062500         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLKLAR-ATTR                    
062600                                       MOD-IDANSTNR-ATTR                  
062700      ELSE                                                                
062800         IF MFS-FIRST                                                     
062900             MOVE MFS-RENSA-FAELT   TO MOD-FLKLAR                         
063000                                       MOD-IDANSTNR                       
063100         END-IF                                                           
063200     END-IF                                                               
063300     .                                                                    
063400     EJECT                                                                
063500 FA-LAES-RADDATA SECTION.                                                 
063600                                                                          
063700     SET TAB-IX                TO +1                                      
063800     MOVE WS-IDLEVNR-KOLLI     TO W-D1CSEQ-IDLEVNR-KOLLI                  
063900                                  W-IDLEVNR-KOLLI                         
064000     MOVE WS-IDOKOLLI          TO W-D1CSEQ-IDOKOLLI                       
064100                                  W-IDOKOLLI                              
064200     MOVE ZERO                 TO W-SPAR-IDLOPNRM                         
064300     PERFORM IMS-GU-INLA1-INLA11                                          
064400     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR                      
064500                   TAB-IX > MAX-INDX                                      
064600         IF ART-IDLOPNRM       = W-SPAR-IDLOPNRM                          
064700             CONTINUE                                                     
064800          ELSE                                                            
064900             MOVE ART-IDLOPNRM TO W-SPAR-IDLOPNRM                         
065000             PERFORM IMS-GNP-INLA1-INLA21                                 
065100             PERFORM UNTIL SEGMENT-SAKNAS    OR                           
065200                           TAB-IX > MAX-INDX                              
065300                 IF RAD-KDINLSTA  = SPACE OR 'SAK' OR 'FPK' OR            
065400                                    'VOR'                                 
065500                     PERFORM FAA-FYLL-SORT-TABELL                         
065600                     SET TAB-IX UP BY +1                                  
065700                 END-IF                                                   
065800                 PERFORM IMS-GNP-INLA1-INLA21                             
065900             END-PERFORM                                                  
066000         END-IF                                                           
066100         PERFORM IMS-GN-INLA1-INLA11                                      
066200     END-PERFORM                                                          
066300                                                                          
066400     IF TAB-IX                 =  +1                                      
066500         MOVE NEJ              TO RADER-SW                                
066600     END-IF                                                               
066700     .                                                                    
066800     EJECT                                                                
066900 FAA-FYLL-SORT-TABELL SECTION.                                            
067000                                                                          
067100                                                                          
067200      IF RAD-KDINLSTA           =  'VOR'                                  
067210         IF TAB-IX = +1                                                   
067220         MOVE RAD-KVINLART     TO TAB-KVINLART-VOR (TAB-IX)               
067221         MOVE ART-ADLAGOMR     TO TAB-ADLAGOMR     (TAB-IX)               
067222         MOVE ART-ADPLATS      TO TAB-ADPLATS      (TAB-IX)               
067223         MOVE ART-ADGANG       TO TAB-ADGANG       (TAB-IX)               
067224         MOVE ART-IDARTNR      TO TAB-IDARTNR      (TAB-IX)               
067225         MOVE ART-BEART        TO TAB-BEART        (TAB-IX)               
067226         MOVE RAD-KVINLART     TO TAB-KVINLART     (TAB-IX)               
067227         MOVE ART-IDLOPNRM     TO TAB-IDLOPNRM     (TAB-IX)               
067228         MOVE RAD-IDRADNR      TO TAB-IDRADNR      (TAB-IX)               
067229         MOVE ART-FLKVAFEL     TO TAB-FLKVAFEL     (TAB-IX)               
067230         MOVE ART-FLKVAKAR     TO TAB-FLKVAKAR     (TAB-IX)               
067231         MOVE RAD-FLSATS       TO TAB-FLSATS       (TAB-IX)               
067232         IF RAD-KDINLPRIO      <  31                                      
067233             MOVE 'P'          TO TAB-KDKLIPRI     (TAB-IX)               
067234         END-IF                                                           
067235                                                                          
067236         EVALUATE ART-KDFARLIG                                            
067237           WHEN 4                                                         
067238             IF ENGLISH-TEXT                                              
067239               MOVE 'J'        TO TAB-BEFARLIG     (TAB-IX)               
067240             ELSE                                                         
067241               MOVE 'Y'        TO TAB-BEFARLIG     (TAB-IX)               
067242             END-IF                                                       
067243           WHEN 5                                                         
067244               MOVE 'A'        TO TAB-BEFARLIG     (TAB-IX)               
067245           WHEN 6                                                         
067246             IF ENGLISH-TEXT                                              
067247               MOVE 'K'        TO TAB-BEFARLIG     (TAB-IX)               
067248             ELSE                                                         
067249               MOVE 'C'        TO TAB-BEFARLIG     (TAB-IX)               
067250             END-IF                                                       
067251           WHEN 7                                                         
067252             IF ENGLISH-TEXT                                              
067253               MOVE 'J'        TO TAB-BEFARLIG     (TAB-IX)               
067254             ELSE                                                         
067255               MOVE 'Y'        TO TAB-BEFARLIG     (TAB-IX)               
067256             END-IF                                                       
067257         END-EVALUATE                                                     
067260         ELSE                                                             
067300            SET TAB-IX DOWN BY 1                                          
067400            MOVE RAD-KVINLART     TO TAB-KVINLART-VOR (TAB-IX)            
067410            END-IF                                                        
067500      ELSE                                                                
067600         MOVE ART-ADLAGOMR     TO TAB-ADLAGOMR     (TAB-IX)               
067700         MOVE ART-ADPLATS      TO TAB-ADPLATS      (TAB-IX)               
067800         MOVE ART-ADGANG       TO TAB-ADGANG       (TAB-IX)               
067900         MOVE ART-IDARTNR      TO TAB-IDARTNR      (TAB-IX)               
068000         MOVE ART-BEART        TO TAB-BEART        (TAB-IX)               
068100         MOVE RAD-KVINLART     TO TAB-KVINLART     (TAB-IX)               
068200         MOVE ART-IDLOPNRM     TO TAB-IDLOPNRM     (TAB-IX)               
068300         MOVE RAD-IDRADNR      TO TAB-IDRADNR      (TAB-IX)               
068400         MOVE ART-FLKVAFEL     TO TAB-FLKVAFEL     (TAB-IX)               
068500         MOVE ART-FLKVAKAR     TO TAB-FLKVAKAR     (TAB-IX)               
068600         MOVE RAD-FLSATS       TO TAB-FLSATS       (TAB-IX)               
068700         IF RAD-KDINLPRIO      <  31                                      
068800             MOVE 'P'          TO TAB-KDKLIPRI     (TAB-IX)               
068900         END-IF                                                           
069000                                                                          
069100         EVALUATE ART-KDFARLIG                                            
069200           WHEN 4                                                         
069300             IF ENGLISH-TEXT                                              
069400               MOVE 'J'        TO TAB-BEFARLIG     (TAB-IX)               
069500             ELSE                                                         
069600               MOVE 'Y'        TO TAB-BEFARLIG     (TAB-IX)               
069700             END-IF                                                       
069800           WHEN 5                                                         
069900               MOVE 'A'        TO TAB-BEFARLIG     (TAB-IX)               
070000           WHEN 6                                                         
070100             IF ENGLISH-TEXT                                              
070200               MOVE 'K'        TO TAB-BEFARLIG     (TAB-IX)               
070300             ELSE                                                         
070400               MOVE 'C'        TO TAB-BEFARLIG     (TAB-IX)               
070500             END-IF                                                       
070600           WHEN 7                                                         
070700             IF ENGLISH-TEXT                                              
070800               MOVE 'J'        TO TAB-BEFARLIG     (TAB-IX)               
070900             ELSE                                                         
071000               MOVE 'Y'        TO TAB-BEFARLIG     (TAB-IX)               
071100             END-IF                                                       
071200         END-EVALUATE                                                     
071300     END-IF                                                               
071400     .                                                                    
071500     EJECT                                                                
071600 FB-SORTERA-MOD-TABELL  SECTION.                                          
071700                                                                          
071800     MOVE +42                  TO TABENTRY-LNGD                           
071900     MOVE +12                  TO SORTBGP-LNGD                            
072000     SET ANTAL-ENTRY           TO TAB-IX                                  
072100     COMPUTE ANTAL-ENTRY       = ANTAL-ENTRY - 1                          
072200     CALL WINTSOR USING MOD-TABELL TABENTRY-LNGD                          
072300                                   ANTAL-ENTRY                            
072400                                   TAB-SORT-FAELT(1)                      
072500                                   SORTBGP-LNGD                           
072600     .                                                                    
072700     EJECT                                                                
072800 FC-FLYTTA-TABELL-TILL-MOD SECTION.                                       
072900                                                                          
073000     SET TAB-IX                TO +1                                      
073100     MOVE +1                   TO INDX                                    
073200     PERFORM UNTIL TAB-IX      >  MAX-INDX OR                             
073300                   TAB-IDARTNR(INDX) =  ZERO                              
073400         IF MFS-UPDATE OR MFS-FIRST                                       
073500             MOVE MFS-RENSA-FAELT       TO MOD-KDCMDVAL-RAD(INDX)         
073600                                           MOD-KVINLART-UPD(INDX)         
073700                                       MOD-ADINLOMR-NXT-UPD(INDX)         
073800         END-IF                                                           
073900         MOVE TAB-ADLAGOMR     (TAB-IX) TO MOD-ADLAGOMR-RAD(INDX)         
074000         MOVE TAB-ADPLATS      (TAB-IX) TO MOD-ADPLATS-RAD (INDX)         
074100         MOVE TAB-ADGANG       (TAB-IX) TO MOD-ADGANG-RAD  (INDX)         
074200         MOVE TAB-IDARTNR      (TAB-IX) TO MOD-IDARTNR-RAD (INDX)         
074300         MOVE TAB-BEART        (TAB-IX) TO MOD-BEART-RAD   (INDX)         
074400         MOVE TAB-KVINLART     (TAB-IX) TO MOD-KVINLART-RAD(INDX)         
074500         MOVE TAB-KVINLART-VOR (TAB-IX) TO                                
074600                                        MOD-KVINLART-VOR-RAD(INDX)        
074700         MOVE TAB-IDLOPNRM     (TAB-IX) TO MOD-IDLOPNRM-RAD(INDX)         
074800         MOVE TAB-IDRADNR      (TAB-IX) TO MOD-IDRADNR-RAD (INDX)         
074900         MOVE TAB-BEFARLIG     (TAB-IX) TO MOD-BEFARLIG-RAD(INDX)         
075000         MOVE TAB-KDKLIPRI     (TAB-IX) TO MOD-KDKLIPRI-RAD(INDX)         
075100                                                                          
075200         IF  TAB-ADPLATS (TAB-IX) = ZERO                                  
075300             MOVE MFS-ADD-LYS-UPP-FAELT TO                                
075400                                MOD-ADLAGOMR-RAD-ATTR(INDX)               
075500             MOVE MFS-ADD-LYS-UPP-FAELT TO                                
075600                                MOD-ADGANG-RAD-ATTR(INDX)                 
075700             MOVE MFS-ADD-LYS-UPP-FAELT TO                                
075800                                MOD-ADPLATS-RAD-ATTR(INDX)                
075900             MOVE ERR-PLACE-MISSING     TO MED-IDMFSFEL                   
076000             CALL WMEDKONV USING MED-WMEDAREA                             
076100             MOVE MED-MFSFEL            TO MOD-TEMFSFEL                   
076200         END-IF                                                           
076300                                                                          
076400         IF  TAB-FLKVAFEL(TAB-IX)       = JA                              
076500             MOVE MFS-ADD-LYS-UPP-FAELT TO                                
076600                                MOD-IDARTNR-RAD-ATTR(INDX)                
076700             MOVE ERR-QUALITY           TO MED-IDMFSFEL                   
076800             CALL WMEDKONV USING MED-WMEDAREA                             
076900             MOVE MED-MFSFEL            TO MOD-TEMFSFEL                   
077000         END-IF                                                           
077100                                                                          
077200         IF  TAB-FLKVAKAR(TAB-IX)       = JA                              
077300             MOVE MFS-ADD-LYS-UPP-FAELT TO                                
077400                                MOD-IDARTNR-RAD-ATTR(INDX)                
077500             MOVE ERR-CHECK-QUAL-FIRST  TO MED-IDMFSFEL                   
077600             CALL WMEDKONV USING MED-WMEDAREA                             
077700             MOVE MED-MFSFEL            TO MOD-TEMFSFEL                   
077800         END-IF                                                           
077900                                                                          
078000         IF  TAB-FLSATS  (TAB-IX)       = JA AND                          
078100             TAB-ADLAGOMR(TAB-IX)   NOT = 30                              
078200             MOVE MFS-ADD-LYS-UPP-FAELT TO                                
078300                                MOD-IDARTNR-RAD-ATTR(INDX)                
078400             MOVE ERR-KIT-CASE          TO MED-IDMFSFEL                   
078500             CALL WMEDKONV USING MED-WMEDAREA                             
078600             MOVE MED-MFSFEL            TO MOD-TEMFSFEL                   
078700         END-IF                                                           
078800                                                                          
078900         IF TAB-BEFARLIG(TAB-IX) NOT = SPACE                              
079000             MOVE MFS-ADD-LYS-UPP-FAELT TO                                
079100                                MOD-BEFARLIG-RAD-ATTR(INDX)               
079200         END-IF                                                           
079300                                                                          
079400         IF TAB-KDKLIPRI(TAB-IX) NOT = SPACE                              
079500             MOVE MFS-ADD-LYS-UPP-FAELT TO                                
079600                                MOD-KDKLIPRI-RAD-ATTR(INDX)               
079700         END-IF                                                           
079800                                                                          
079900         SET TAB-IX UP BY +1                                              
080000         ADD +1     TO INDX                                               
080100     END-PERFORM                                                          
080200                                                                          
080300     PERFORM UNTIL INDX        >  MAX-INDX                                
080400         MOVE MFS-STAENG-FAELT TO MOD-KDCMDVAL-RAD-ATTR(INDX)             
080500                                  MOD-KVINLART-UPD-ATTR(INDX)             
080600                                  MOD-ADINLOMR-NXT-UPD-ATTR(INDX)         
080700         MOVE MFS-RENSA-FAELT  TO MOD-KDCMDVAL-RAD(INDX)                  
080800                                  MOD-KVINLART-UPD(INDX)                  
080900                                  MOD-ADINLOMR-NXT-UPD(INDX)              
081000                                  MOD-ADLAGOMR-RAD(INDX)                  
081100                                  MOD-ADPLATS-RAD (INDX)                  
081200                                  MOD-ADGANG-RAD  (INDX)                  
081300                                  MOD-IDARTNR-RAD (INDX)                  
081400                                  MOD-BEART-RAD   (INDX)                  
081500                                  MOD-KVINLART-RAD(INDX)                  
081600                                  MOD-KVINLART-VOR-RAD(INDX)              
081700                                  MOD-IDLOPNRM-RAD(INDX)                  
081800                                  MOD-IDRADNR-RAD (INDX)                  
081900                                  MOD-BEFARLIG-RAD(INDX)                  
082000                                  MOD-KDKLIPRI-RAD(INDX)                  
082100         ADD +1                TO INDX                                    
082200     END-PERFORM                                                          
082300     .                                                                    
082400     EJECT                                                                
082500 G-KOLLA-INPUT SECTION.                                                   
082600                                                                          
082700     MOVE SPACE                     TO MED-IDMFSFEL                       
082800     MOVE JA                        TO INDATA-SW                          
082900     IF MID-INPUT                   =  ALL '+' OR SPACE                   
083000         MOVE ERR-PF11-AND-NO-DATA  TO MED-IDMFSFEL                       
083100         CALL WMEDKONV USING MED-WMEDAREA                                 
083200         MOVE MED-MFSFEL            TO MOD-TEMFSFEL                       
083300         PERFORM MFS-ROER-EJ-FAELT-IN                                     
083400         PERFORM MFS-ROER-EJ-FAELT-UT                                     
083500         MOVE NEJ                   TO INDATA-SW                          
083600     ELSE                                                                 
083700         IF MID-FLKLAR              = ALL '+' OR SPACE                    
083800             PERFORM GA-KOLLA-RADBEHANDLING                               
083900          ELSE                                                            
084000             PERFORM GB-KOLLA-KOLLI-KLART                                 
084100         END-IF                                                           
084200                                                                          
084300         IF INDATA-FEL                                                    
084400             IF MED-IDMFSFEL           =  SPACE                           
084500                 MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                
084600             END-IF                                                       
084700             CALL WMEDKONV USING MED-WMEDAREA                             
084800             MOVE MED-MFSFEL         TO MOD-TEMFSFEL                      
084900             PERFORM MFS-ROER-EJ-FAELT-UT                                 
085000             PERFORM MFS-ROER-EJ-FAELT-IN                                 
085100         END-IF                                                           
085200     END-IF                                                               
085300     .                                                                    
085400     EJECT                                                                
085500 GA-KOLLA-RADBEHANDLING SECTION.                                          
085600                                                                          
085700     MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDANSTNR-ATTR                       
085800     MOVE +1                   TO INDX                                    
085900     PERFORM UNTIL INDX        >  MAX-INDX                                
086000         IF (MID-KDCMDVAL-RAD(INDX)     = ALL '+' OR SPACE) AND           
086100            (MID-KVINLART-UPD(INDX)     = ALL '+' OR SPACE) AND           
086200            (MID-ADINLOMR-NXT-UPD(INDX) = ALL '+' OR SPACE)               
086300             CONTINUE                                                     
086400          ELSE                                                            
086500             PERFORM GAA-KOLLA-ATT-RAD-FINNS                              
086600         END-IF                                                           
086700         IF RAD-FINNS                                                     
086800           PERFORM GAB-KOLLA-KDCMDVAL                                     
086900           PERFORM GAC-KOLLA-KVINLART-UPD                                 
087000           PERFORM GAD-KOLLA-ADINLOMR-NXT-UPD                             
087100           IF MID-KDCMDVAL-RAD(INDX) = 'AVV' OR 'INL' OR 'I' OR           
087200                                       'DIN' OR 'DI'  OR                  
087300                                       'BIN' OR 'BP' OR 'DEV'             
087400              PERFORM S01-KOLLA-KVAL-PLATS-SATS                           
087500           END-IF                                                         
087600                                                                          
087700           IF MID-KDCMDVAL-RAD(INDX) = 'AVV' OR 'DEV'                     
087800              PERFORM GAE-KOLLA-IDANSTNR                                  
087900              PERFORM GAF-KOLLA-RT                                        
088000           END-IF                                                         
088100         END-IF                                                           
088200         ADD +1  TO INDX                                                  
088300     END-PERFORM                                                          
088400     .                                                                    
088500     EJECT                                                                
088600 GAA-KOLLA-ATT-RAD-FINNS SECTION.                                         
088700                                                                          
088800     INSPECT MID-IDLOPNRM-RAD(INDX) REPLACING                             
088900                                       LEADING SPACE BY ZERO              
089000     INSPECT MID-IDRADNR-RAD(INDX) REPLACING                              
089100                                       LEADING SPACE BY ZERO              
089200     MOVE MID-IDLOPNRM-RAD(INDX) TO W-D1BSEQ-IDLOPNRM                     
089300     MOVE MID-IDRADNR-RAD(INDX)  TO W-IDRADNR                             
089400     PERFORM IMS-GU-INLA2-INLA21                                          
089500     IF SEGMENT-FINNS AND RAD-KDINLSTA = SPACE OR 'FPK' OR 'SAK'          
089600         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMDVAL-RAD-ATTR(INDX)         
089700         MOVE JA                   TO RAD-SW                              
089800      ELSE                                                                
089900         MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMDVAL-RAD-ATTR(INDX)         
090000         MOVE NEJ                  TO INDATA-SW                           
090100         MOVE ERR-MISSING          TO MED-IDMFSFEL                        
090200         MOVE NEJ                  TO RAD-SW                              
090300     END-IF                                                               
090400     .                                                                    
090500     EJECT                                                                
090600 GAB-KOLLA-KDCMDVAL   SECTION.                                            
090700                                                                          
090800     IF MID-KDCMDVAL-RAD(INDX)     = ALL '+' OR SPACE OR                  
090900                                     'AVV' OR 'I' OR 'INL'  OR            
091000                                     'DIN' OR 'DI' OR                     
091100                                     'BIN' OR 'BP' OR 'DEV'               
091200         CONTINUE                                                         
091300      ELSE                                                                
091400         MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMDVAL-RAD-ATTR(INDX)         
091500         MOVE NEJ                  TO INDATA-SW                           
091600     END-IF                                                               
091700     .                                                                    
091800     EJECT                                                                
091900 GAC-KOLLA-KVINLART-UPD   SECTION.                                        
092000                                                                          
092100     IF MID-KDCMDVAL-RAD(INDX) =  'AVV' OR 'DIN' OR 'DI' OR               
092200                                  'DEV' OR 'PB ' OR 'BIN'                 
092300        IF MID-KVINLART-UPD(INDX) NUMERIC                                 
092400           MOVE MID-KVINLART-UPD(INDX) TO W-KVINLART-UPD                  
092500           MOVE MFS-NUM-FAELT-RAETT                                       
092600                            TO MOD-KVINLART-UPD-ATTR(INDX)                
092700           IF W-KVINLART-UPD = RAD-KVINLART                               
092800              MOVE NEJ         TO INDATA-SW                               
092900              MOVE MFS-NUM-FAELT-FEL                                      
093000                            TO MOD-KVINLART-UPD-ATTR(INDX)                
093100           END-IF                                                         
093200           IF MID-KDCMDVAL-RAD(INDX) = 'DIN' OR 'DI' OR 'PB'              
093300              IF W-KVINLART-UPD > RAD-KVINLART OR                         
093400                 W-KVINLART-UPD = 0                                       
093500                 MOVE NEJ      TO INDATA-SW                               
093600                 MOVE MFS-NUM-FAELT-FEL                                   
093700                               TO MOD-KVINLART-UPD-ATTR(INDX)             
093800              END-IF                                                      
093900           END-IF                                                         
094000        ELSE                                                              
094100           MOVE NEJ            TO INDATA-SW                               
094200           MOVE MFS-NUM-FAELT-FEL                                         
094300                            TO MOD-KVINLART-UPD-ATTR(INDX)                
094400        END-IF                                                            
094500     ELSE                                                                 
094600        IF MID-KVINLART-UPD(INDX) = ALL '+' OR SPACE                      
094700            MOVE MFS-NUM-FAELT-RAETT                                      
094800                              TO MOD-KVINLART-UPD-ATTR(INDX)              
094900         ELSE                                                             
095000            MOVE MFS-NUM-FAELT-FEL                                        
095100                              TO MOD-KVINLART-UPD-ATTR(INDX)              
095200            MOVE NEJ             TO INDATA-SW                             
095300        END-IF                                                            
095400     END-IF                                                               
095500     .                                                                    
095600     EJECT                                                                
095700 GAD-KOLLA-ADINLOMR-NXT-UPD  SECTION.                                     
095800                                                                          
095900     IF MID-ADINLOMR-NXT-UPD(INDX) = ALL '+' OR SPACE                     
096000             MOVE MFS-ALFA-FAELT-RAETT                                    
096100                               TO MOD-ADINLOMR-NXT-UPD-ATTR(INDX)         
096200      ELSE                                                                
096300         IF MID-KDCMDVAL-RAD(INDX) = 'INL' OR 'I' OR 'AVV' OR             
096400                                     'DIN' OR 'DI' OR                     
096500                                     'BIN' OR 'PB' OR 'DEV'               
096600             MOVE MFS-ALFA-FAELT-FEL                                      
096700                               TO MOD-ADINLOMR-NXT-UPD-ATTR(INDX)         
096800             MOVE NEJ          TO INDATA-SW                               
096900          ELSE                                                            
097000             PERFORM GADA-KOLLA-PLAA                                      
097100             PERFORM GADB-KOLLA-ADINLOMR                                  
097200         END-IF                                                           
097300     END-IF                                                               
097400     .                                                                    
097500     EJECT                                                                
097600 GADA-KOLLA-PLAA              SECTION.                                    
097700                                                                          
097800     MOVE MID-ADINLOMR-NXT-UPD(INDX) TO W-6006-ADINLOMR                   
097900     PERFORM IMS-GU-PLAA-PLAA11                                           
098000     IF SEGMENT-FINNS                                                     
098100         MOVE MFS-ALFA-FAELT-RAETT                                        
098200                               TO MOD-ADINLOMR-NXT-UPD-ATTR(INDX)         
098300      ELSE                                                                
098400         MOVE MFS-ALFA-FAELT-FEL                                          
098500                               TO MOD-ADINLOMR-NXT-UPD-ATTR(INDX)         
098600         MOVE NEJ              TO INDATA-SW                               
098700     END-IF                                                               
098800     .                                                                    
098900     EJECT                                                                
099000 GADB-KOLLA-ADINLOMR          SECTION.                                    
099100                                                                          
099200     IF RAD-SAKNAS OR                                                     
099300        RAD-ADINLOMR = MID-ADINLOMR-NXT-UPD(INDX)                         
099400         MOVE MFS-ALFA-FAELT-FEL                                          
099500                               TO MOD-ADINLOMR-NXT-UPD-ATTR(INDX)         
099600         MOVE NEJ              TO INDATA-SW                               
099700     END-IF                                                               
099800     .                                                                    
099900     EJECT                                                                
100000 GAE-KOLLA-IDANSTNR          SECTION.                                     
100100                                                                          
100200     INSPECT MID-IDANSTNR REPLACING LEADING SPACE BY ZERO                 
100300     IF MID-IDANSTNR               NUMERIC                                
100400         MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDANSTNR-ATTR                   
100500      ELSE                                                                
100600         MOVE MFS-NUM-FAELT-FEL    TO MOD-IDANSTNR-ATTR                   
100700         MOVE NEJ                  TO INDATA-SW                           
100800     END-IF                                                               
100900     .                                                                    
101000     EJECT                                                                
101100 GAF-KOLLA-RT         SECTION.                                            
101200                                                                          
101300     IF ART-KDRT = +3 OR +77                                              
101400        MOVE NEJ               TO INDATA-SW                               
101500        MOVE MFS-NUM-FAELT-FEL TO MOD-KVINLART-UPD-ATTR(INDX)             
101600        MOVE ERR-RTYP-3-OR-77  TO MED-IDMFSFEL                            
101700     END-IF                                                               
101800     .                                                                    
101900     EJECT                                                                
102000 GB-KOLLA-KOLLI-KLART   SECTION.                                          
102100                                                                          
102200     IF MID-FLKLAR                 =  ALL 'J'                             
102300         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLKLAR-ATTR                     
102400      ELSE                                                                
102500         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLKLAR-ATTR                     
102600         MOVE NEJ                  TO INDATA-SW                           
102700     END-IF                                                               
102800                                                                          
102900     IF MID-IDANSTNR               =  ALL '+' OR SPACE                    
103000         MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDANSTNR-ATTR                   
103100     ELSE                                                                 
103200        INSPECT MID-IDANSTNR REPLACING LEADING SPACE BY ZERO              
103300        IF MID-IDANSTNR            NUMERIC                                
103400            MOVE MFS-NUM-FAELT-RAETT TO MOD-IDANSTNR-ATTR                 
103500         ELSE                                                             
103600            MOVE MFS-NUM-FAELT-FEL TO MOD-IDANSTNR-ATTR                   
103700        END-IF                                                            
103800     END-IF                                                               
103900                                                                          
104000     MOVE +1                   TO INDX                                    
104100     MOVE ZERO                 TO W-RADRAEKNARE                           
104200     PERFORM UNTIL INDX        >  MAX-INDX                                
104300         INSPECT MID-IDLOPNRM-RAD(INDX) REPLACING                         
104400                                    LEADING SPACE BY ZERO                 
104500         INSPECT MID-IDRADNR-RAD(INDX) REPLACING                          
104600                                    LEADING SPACE BY ZERO                 
104700         IF MID-IDLOPNRM-RAD(INDX) > ZERO                                 
104800             PERFORM GBA-KOLLA-KDCMDVAL                                   
104900             PERFORM GBB-KOLLA-KVINLART-UPD                               
105000             PERFORM GBC-KOLLA-ADINLOMR-NXT-UPD                           
105100             PERFORM GBD-KOLLA-ATT-RAD-FINNS                              
105200             PERFORM S01-KOLLA-KVAL-PLATS-SATS                            
105300         END-IF                                                           
105400         ADD +1                TO INDX                                    
105500     END-PERFORM                                                          
105600                                                                          
105700     IF W-RADRAEKNARE = ZERO                                              
105800        MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLKLAR-ATTR                      
105900        MOVE ERR-MISSING          TO MED-IDMFSFEL                         
106000        MOVE NEJ                  TO INDATA-SW                            
106100     END-IF                                                               
106200     .                                                                    
106300     EJECT                                                                
106400 GBA-KOLLA-KDCMDVAL   SECTION.                                            
106500                                                                          
106600     IF MID-KDCMDVAL-RAD(INDX)     =  ALL '+' OR SPACE                    
106700         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMDVAL-RAD-ATTR(INDX)         
106800         ADD +1                    TO W-RADRAEKNARE                       
106900      ELSE                                                                
107000         MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMDVAL-RAD-ATTR(INDX)         
107100         MOVE NEJ                  TO INDATA-SW                           
107200     END-IF                                                               
107300     .                                                                    
107400     EJECT                                                                
107500 GBB-KOLLA-KVINLART-UPD   SECTION.                                        
107600                                                                          
107700     IF MID-KVINLART-UPD(INDX)     =  ALL '+' OR SPACE                    
107800         MOVE MFS-NUM-FAELT-RAETT  TO MOD-KVINLART-UPD-ATTR(INDX)         
107900      ELSE                                                                
108000         MOVE MFS-NUM-FAELT-FEL    TO MOD-KVINLART-UPD-ATTR(INDX)         
108100         MOVE NEJ                  TO INDATA-SW                           
108200     END-IF                                                               
108300     .                                                                    
108400     EJECT                                                                
108500 GBC-KOLLA-ADINLOMR-NXT-UPD  SECTION.                                     
108600                                                                          
108700     IF MID-ADINLOMR-NXT-UPD(INDX) = ALL '+' OR SPACE                     
108800         MOVE MFS-ALFA-FAELT-RAETT                                        
108900                               TO MOD-ADINLOMR-NXT-UPD-ATTR(INDX)         
109000      ELSE                                                                
109100         MOVE MFS-ALFA-FAELT-FEL                                          
109200                               TO MOD-ADINLOMR-NXT-UPD-ATTR(INDX)         
109300         MOVE NEJ              TO INDATA-SW                               
109400     END-IF                                                               
109500     .                                                                    
109600     EJECT                                                                
109700 GBD-KOLLA-ATT-RAD-FINNS SECTION.                                         
109800                                                                          
109900     MOVE MID-IDLOPNRM-RAD(INDX) TO W-D1BSEQ-IDLOPNRM                     
110000     MOVE MID-IDRADNR-RAD(INDX)  TO W-IDRADNR                             
110100     PERFORM IMS-GU-INLA2-INLA21                                          
110200     IF SEGMENT-FINNS AND RAD-KDINLSTA = SPACE OR 'FPK' OR 'SAK'          
110300         CONTINUE                                                         
110400      ELSE                                                                
110500         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLKLAR-ATTR                     
110600         MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMDVAL-RAD-ATTR(INDX)         
110700         MOVE NEJ                  TO INDATA-SW                           
110800         MOVE ERR-MISSING          TO MED-IDMFSFEL                        
110900     END-IF                                                               
111000     .                                                                    
111100 H-UPPDATERA SECTION.                                                     
111200                                                                          
111300     MOVE +1                   TO 6191-IX                                 
111400     IF MID-FLKLAR             = ALL '+' OR SPACE                         
111500         PERFORM HA-UPPDATERA-RADER                                       
111600      ELSE                                                                
111700         PERFORM HB-UPPDATERA-HELA-KOLLIT                                 
111800     END-IF                                                               
111900                                                                          
112000     IF 6191-IX                > 1                                        
112100         PERFORM S05-STARTA-W6T191                                        
112200     END-IF                                                               
112300                                                                          
112400     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
112500     CALL WMEDKONV USING MED-WMEDAREA                                     
112600     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
112700     PERFORM MFS-FORM-ATTR                                                
112800     PERFORM MFS-RENSA-FAELT-IN                                           
112900     .                                                                    
113000     EJECT                                                                
113100 HA-UPPDATERA-RADER SECTION.                                              
113200                                                                          
113300     MOVE +1                       TO INDX                                
113400     PERFORM UNTIL INDX            >  MAX-INDX                            
113500         IF MID-KDCMDVAL-RAD(INDX) = 'AVV' OR 'DEV'                       
113600           PERFORM HAA-UPPDATERA-AVVIKELSE                                
113700         END-IF                                                           
113800                                                                          
113900         IF MID-KDCMDVAL-RAD(INDX) = 'INL' OR 'I' OR 'BIN'                
114000             PERFORM S02-UPPDATERA-INLAEGGNING                            
114100         END-IF                                                           
114200                                                                          
114300         IF MID-KDCMDVAL-RAD(INDX) = 'DIN' OR 'DI' OR 'BP'                
114400             PERFORM HAC-UPPDATERA-DELINLAEGGNING                         
114500         END-IF                                                           
114600                                                                          
114700         IF MID-ADINLOMR-NXT-UPD(INDX) = ALL '+' OR SPACE                 
114800             CONTINUE                                                     
114900          ELSE                                                            
115000             PERFORM HAB-UPPDATERA-RETUR                                  
115100         END-IF                                                           
115200         ADD +1                    TO INDX                                
115300     END-PERFORM                                                          
115400     .                                                                    
115500     EJECT                                                                
115600 HAA-UPPDATERA-AVVIKELSE SECTION.                                         
115700                                                                          
115800     PERFORM S03-LAES-INLA21                                              
115900                                                                          
116000     IF (RAD-KDINLPRIO < 31 AND RAD-FLPRIO = 'N')                         
116100        PERFORM S10-CALL-W611PMRK-PARTI                                   
116200        PERFORM S03-LAES-INLA21                                           
116300     END-IF                                                               
116400                                                                          
116500     IF RAD-IDRADNR            =  1                                       
116600                                                                          
116700** OBS INGA LÄSNINGAR MOT INLA2 PCB'ET MELLAN  -LAST OCH ISRT             
116800** DET SKULLE FÖRSTÖRA POSITIONERINGEN                                    
116900                                                                          
117000         PERFORM IMS-DLET-INLA2-INLA21                                    
117100         MOVE MID-KVINLART-UPD(INDX) TO W-KVINLART-UPD                    
117200         PERFORM S08-SKAPA-6191-MID-BORT-RAD                              
117300         PERFORM IMS-GNP-INLA2-INLA21-LAST                                
117400         IF SEGMENT-SAKNAS                                                
117500             MOVE +2                     TO SPAR-RAD-IDRADNR              
117600          ELSE                                                            
117700             COMPUTE SPAR-RAD-IDRADNR = RAD-IDRADNR + 1                   
117800         END-IF                                                           
117900         MOVE SPAR-RAD-W6D121            TO RAD-W6D121                    
118000                                                                          
118100         IF MID-KVINLART-UPD(INDX)       =  ZERO                          
118200            PERFORM HAAA-HELA-RADEN-AVV                                   
118300            PERFORM IMS-ISRT-INLA2-INLA21                                 
118400            PERFORM S07-SKAPA-6191-MID-NY-RAD                             
118500            PERFORM S06-SKAPA-6193-MID                                    
118600            IF (RAD-FLPRIO   = 'J'  AND                                   
118700                RAD-KVINLART > ZERO)                                      
118800                PERFORM S10-CALL-W611PMRK-PARTI                           
118900            END-IF                                                        
119000         ELSE                                                             
119100            MOVE MID-KVINLART-UPD(INDX)  TO RAD-KVINLART                  
119200            MOVE 'INL'                   TO RAD-KDINLSTA                  
119300            MOVE SPACE                   TO RAD-ADINLOMR                  
119400                                            RAD-ADINLOMR-NXT              
119500            MOVE ZERO                    TO RAD-IDINLVGN                  
119600                                            RAD-IDILIST                   
119700                                            RAD-IDILIRAD                  
119800            MOVE DAGENS-DATUM            TO RAD-TIUPPDAT                  
119900            PERFORM IMS-ISRT-INLA2-INLA21                                 
120000            PERFORM S07-SKAPA-6191-MID-NY-RAD                             
120100            PERFORM S06-SKAPA-6193-MID                                    
120200            PERFORM HAAB-SKAPA-NY-AVV-RAD                                 
120300            PERFORM S07-SKAPA-6191-MID-NY-RAD                             
120400         END-IF                                                           
120500      ELSE                                                                
120600         IF MID-KVINLART-UPD(INDX)       =  ZERO                          
120700            PERFORM HAAA-HELA-RADEN-AVV                                   
120800            PERFORM IMS-REPL-INLA2-INLA21                                 
120900            PERFORM S04-SKAPA-6191-MID                                    
121000            PERFORM S06-SKAPA-6193-MID                                    
121100            IF (RAD-FLPRIO   = 'J'  AND                                   
121200                RAD-KVINLART > ZERO)                                      
121300                PERFORM S10-CALL-W611PMRK-PARTI                           
121400            END-IF                                                        
121500         ELSE                                                             
121600            MOVE MID-KVINLART-UPD(INDX)  TO RAD-KVINLART                  
121700            MOVE 'INL'                   TO RAD-KDINLSTA                  
121800            MOVE SPACE                   TO RAD-ADINLOMR                  
121900                                            RAD-ADINLOMR-NXT              
122000            MOVE ZERO                    TO RAD-IDINLVGN                  
122100                                            RAD-IDILIST                   
122200                                            RAD-IDILIRAD                  
122300            MOVE DAGENS-DATUM            TO RAD-TIUPPDAT                  
122400            PERFORM IMS-REPL-INLA2-INLA21                                 
122500            PERFORM S04-SKAPA-6191-MID                                    
122600            PERFORM S06-SKAPA-6193-MID                                    
122700            PERFORM HAAB-SKAPA-NY-AVV-RAD                                 
122800            PERFORM S07-SKAPA-6191-MID-NY-RAD                             
122900         END-IF                                                           
123000     END-IF                                                               
123100     .                                                                    
123200     EJECT                                                                
123300 HAAA-HELA-RADEN-AVV   SECTION.                                           
123400                                                                          
123500     MOVE 'AVV'                  TO RAD-KDINLSTA                          
123600     MOVE SPACE                  TO RAD-ADINLOMR                          
123700                                    RAD-ADINLOMR-NXT                      
123800     MOVE MID-IDANSTNR           TO RAD-IDANSTNR                          
123900     MOVE ZERO                   TO RAD-IDILIRAD                          
124000                                    RAD-IDILIST                           
124100                                    RAD-IDINLVGN                          
124200     ACCEPT RAD-TIUPPDAT FROM DATE                                        
124300     .                                                                    
124400     EJECT                                                                
124500 HAAB-SKAPA-NY-AVV-RAD SECTION.                                           
124600                                                                          
124700** OBS INGA LÄSNINGAR MOT INLA2 PCB'ET MELLAN  -LAST OCH ISRT             
124800** DET SKULLE FÖRSTÖRA POSITIONERINGEN                                    
124900                                                                          
125000     PERFORM IMS-GNP-INLA2-INLA21-LAST                                    
125100     IF  SEGMENT-FINNS                                                    
125200         COMPUTE SPAR-RAD-IDRADNR = RAD-IDRADNR + 1                       
125300      ELSE                                                                
125400         COMPUTE SPAR-RAD-IDRADNR = SPAR-RAD-IDRADNR + 1                  
125500     END-IF                                                               
125600     MOVE SPAR-RAD-W6D121        TO RAD-W6D121                            
125700     MOVE SPACE                  TO RAD-ADINLOMR                          
125800                                    RAD-ADINLOMR-NXT                      
125900     MOVE MID-IDANSTNR           TO RAD-IDANSTNR                          
126000     MOVE ZERO                   TO RAD-IDILIRAD                          
126100                                    RAD-IDILIST                           
126200                                    RAD-IDINLVGN                          
126300     MOVE 'AVV'                  TO RAD-KDINLSTA                          
126400     MOVE MID-KVINLART-UPD(INDX) TO W-KVINLART-UPD                        
126500     COMPUTE RAD-KVINLART        =  SPAR-RAD-KVINLART -                   
126600                                    W-KVINLART-UPD                        
126700     ACCEPT RAD-TIUPPDAT FROM DATE                                        
126800                                                                          
126900     PERFORM IMS-ISRT-INLA2-INLA21                                        
127000     IF (RAD-FLPRIO          = 'J'  AND                                   
127100         RAD-KVINLART        > ZERO)                                      
127200         PERFORM S10-CALL-W611PMRK-PARTI                                  
127300     END-IF                                                               
127400     .                                                                    
127500     EJECT                                                                
127600 HAB-UPPDATERA-RETUR     SECTION.                                         
127700                                                                          
127800     PERFORM S03-LAES-INLA21                                              
127900     MOVE MID-ADINLOMR-NXT-UPD(INDX) TO RAD-ADINLOMR-NXT                  
128000     MOVE SPACE                      TO RAD-IDLEVNR-KOLLI                 
128100     MOVE ZERO                       TO RAD-IDOKOLLI                      
128200     MOVE NEJ                        TO RAD-FLDIVKLI                      
128300     PERFORM IMS-REPL-INLA2-INLA21                                        
128400     PERFORM S04-SKAPA-6191-MID                                           
128500     .                                                                    
128600     EJECT                                                                
128700 HAC-UPPDATERA-DELINLAEGGNING SECTION.                                    
128800                                                                          
128900     PERFORM S03-LAES-INLA21                                              
129000                                                                          
129100     MOVE MID-KVINLART-UPD(INDX) TO W-KVINLART-UPD                        
129200     COMPUTE RAD-KVINLART        =  RAD-KVINLART - W-KVINLART-UPD         
129300     PERFORM IMS-REPL-INLA2-INLA21                                        
129400     PERFORM S04-SKAPA-6191-MID                                           
129500     PERFORM HACA-SKAPA-NY-RAD                                            
129600     PERFORM S07-SKAPA-6191-MID-NY-RAD                                    
129700     PERFORM S06-SKAPA-6193-MID                                           
129800     .                                                                    
129900     EJECT                                                                
130000 HACA-SKAPA-NY-RAD    SECTION.                                            
130100                                                                          
130200** OBS INGA LÄSNINGAR MOT INLA2 PCB'ET MELLAN  -LAST OCH ISRT             
130300** DET SKULLE FÖRSTÖRA POSITIONERINGEN                                    
130400                                                                          
130500     PERFORM IMS-GNP-INLA2-INLA21-LAST                                    
130600     IF SEGMENT-SAKNAS                                                    
130700         COMPUTE SPAR-RAD-IDRADNR  =  SPAR-RAD-IDRADNR + 1                
130800      ELSE                                                                
130900         COMPUTE SPAR-RAD-IDRADNR  =  RAD-IDRADNR + 1                     
131000     END-IF                                                               
131100     MOVE SPAR-RAD-W6D121          TO RAD-W6D121                          
131200                                                                          
131300     MOVE MID-KVINLART-UPD(INDX)   TO RAD-KVINLART                        
131400     MOVE SPACE                    TO RAD-ADINLOMR                        
131500                                      RAD-ADINLOMR-NXT                    
131600                                      RAD-KDINLSTA                        
131700     MOVE ZERO                     TO RAD-IDINLVGN                        
131800                                      RAD-IDILIST                         
131900                                      RAD-IDILIRAD                        
132000                                      RAD-TIUPPDAT                        
132100                                                                          
132200     IF (RAD-KDINLPRIO < 31 AND RAD-FLPRIO = 'N')                         
132300        PERFORM IMS-ISRT-INLA2-INLA21                                     
132400        PERFORM S10-CALL-W611PMRK-PARTI                                   
132500        MOVE SPAR-RAD-IDRADNR   TO W-IDRADNR                              
132600        PERFORM IMS-GU-INLA2-INLA11                                       
132700        PERFORM IMS-GHNP-INLA2-INLA21                                     
132800        MOVE 'INL'              TO RAD-KDINLSTA                           
132900        MOVE DAGENS-DATUM       TO RAD-KDINLSTA                           
133000        PERFORM IMS-REPL-INLA2-INLA21                                     
133100     ELSE                                                                 
133200        MOVE 'INL'              TO RAD-KDINLSTA                           
133300        MOVE DAGENS-DATUM       TO RAD-KDINLSTA                           
133400        PERFORM IMS-ISRT-INLA2-INLA21                                     
133500     END-IF                                                               
133600     .                                                                    
133700     EJECT                                                                
133800 HB-UPPDATERA-HELA-KOLLIT SECTION.                                        
133900                                                                          
134000     MOVE +1                       TO INDX                                
134100     PERFORM UNTIL INDX            >  MAX-INDX                            
134200         IF MID-IDLOPNRM-RAD(INDX) > ZERO                                 
134300             PERFORM S02-UPPDATERA-INLAEGGNING                            
134400         END-IF                                                           
134500         ADD +1                    TO INDX                                
134600     END-PERFORM                                                          
134700     .                                                                    
134800     EJECT                                                                
134900 S01-KOLLA-KVAL-PLATS-SATS    SECTION.                                    
135000                                                                          
135100     PERFORM IMS-GU-INLA2-INLA11                                          
135200     IF SEGMENT-FINNS                                                     
135300        IF (ART-FLKVAFEL = JA OR                                          
135400            ART-FLKVAKAR = JA)                                            
135500            IF MID-FLKLAR        = ALL '+' OR SPACE                       
135600                MOVE MFS-ALFA-FAELT-FEL                                   
135700                                  TO MOD-KDCMDVAL-RAD-ATTR(INDX)          
135800             ELSE                                                         
135900                MOVE MFS-ALFA-FAELT-FEL                                   
136000                                  TO MOD-FLKLAR-ATTR                      
136100            END-IF                                                        
136200            MOVE MFS-ALFA-FAELT-FEL TO MOD-IDARTNR-RAD-ATTR(INDX)         
136300            MOVE NEJ             TO INDATA-SW                             
136400            MOVE ERR-QUALITY     TO MED-IDMFSFEL                          
136500        END-IF                                                            
136600                                                                          
136700        IF  ART-VKART = ZERO                                              
136800           MOVE NEJ                TO INDATA-SW                           
136900           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMDVAL-RAD-ATTR(INDX)         
137000           MOVE '792'              TO MED-IDMFSFEL                        
137100        END-IF                                                            
137200                                                                          
137300        IF  ART-VLARTNTO = ZERO                                           
137400           MOVE NEJ                TO INDATA-SW                           
137500           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMDVAL-RAD-ATTR(INDX)         
137600           MOVE '793'              TO MED-IDMFSFEL                        
137700        END-IF                                                            
137800                                                                          
137900        IF  ART-KDARTURS = SPACE                                          
138000           MOVE NEJ                TO INDATA-SW                           
138100           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMDVAL-RAD-ATTR(INDX)         
138200           MOVE '794'              TO MED-IDMFSFEL                        
138300        END-IF                                                            
138400                                                                          
138500                                                                          
138600** KOLLAR OM KONTROLLERAD PÅ 6139                                         
138700        IF INDATA-OK                                                      
138800           MOVE ART-IDLOPNRM     TO W-IDLOPNRM                            
138900           PERFORM IMS-GU-UPFA-01                                         
139000           IF SEGMENT-FINNS                                               
139100             IF UPPF-KVKVAPRIM > 0                                        
139200** ARTIKEL UTTAGEN FÖR PRIMÄRKONTROLL                                     
139300                IF UPPF-KDKVASTA-PRI = '2' OR '3'                         
139400** PRIMÄRKONTROLL SATT SOM JA/NEJ. (OM NEJ HAR KR SKAPATS).               
139500                   CONTINUE                                               
139600                ELSE                                                      
139700                   MOVE '215'     TO MED-IDMFSFEL                         
139800                   MOVE NEJ       TO INDATA-SW                            
139900                END-IF                                                    
140000             END-IF                                                       
140100             IF UPPF-KVKVASEK > 0                                         
140200                IF UPPF-KDKVASTA-SEK = '2' OR '3'                         
140300                   CONTINUE                                               
140400                ELSE                                                      
140500                   MOVE '215'     TO MED-IDMFSFEL                         
140600                   MOVE NEJ       TO INDATA-SW                            
140700                END-IF                                                    
140800             END-IF                                                       
140900           END-IF                                                         
141000        END-IF                                                            
141100                                                                          
141200** KOLLAR ATT EVENTUELLT GAMLA KR BLIVIT BEDÖMDA                          
141300        IF INDATA-OK                                                      
141400          PERFORM IMS-GU-UPFA-01                                          
141500          IF SEGMENT-FINNS                                                
141600            PERFORM IMS-GNP-UPFA11                                        
141700            PERFORM UNTIL SEGMENT-SAKNAS                                  
141800              IF RAPP-KDKVASTA-PRI = '2' OR '3'                           
141900                CONTINUE                                                  
142000              ELSE                                                        
142100                MOVE '215'     TO MED-IDMFSFEL                            
142200                MOVE NEJ       TO INDATA-SW                               
142300              END-IF                                                      
142400              PERFORM IMS-GNP-UPFA11                                      
142500            END-PERFORM                                                   
142600          END-IF                                                          
142700        END-IF                                                            
142800                                                                          
142900** KOLLAR ATT EVENTUELL SPECIALKONTROLL ÄR GJORD                          
143000        IF INDATA-OK                                                      
143100          PERFORM IMS-GU-UPFA-01                                          
143200          IF SEGMENT-FINNS                                                
143300            PERFORM IMS-GNP-UPFA12                                        
143400            PERFORM UNTIL SEGMENT-SAKNAS                                  
143500              IF SPEC-KDKVASTA-PRI = '2' OR '3'                           
143600                CONTINUE                                                  
143700              ELSE                                                        
143800                MOVE '215'     TO MED-IDMFSFEL                            
143900                MOVE NEJ       TO INDATA-SW                               
144000              END-IF                                                      
144100              PERFORM IMS-GNP-UPFA12                                      
144200            END-PERFORM                                                   
144300          END-IF                                                          
144400        END-IF                                                            
144500                                                                          
144600        IF NOT CDC-TR                                                     
144700          IF ART-ADPLATS = ZERO                                           
144800             IF MID-FLKLAR         = ALL '+' OR SPACE                     
144900                MOVE MFS-ALFA-FAELT-FEL                                   
145000                                   TO MOD-KDCMDVAL-RAD-ATTR(INDX)         
145100             ELSE                                                         
145200                MOVE MFS-ALFA-FAELT-FEL                                   
145300                                   TO MOD-FLKLAR-ATTR                     
145400             END-IF                                                       
145500             MOVE MFS-NUM-FAELT-FEL TO MOD-ADLAGOMR-RAD-ATTR(INDX)        
145600                                       MOD-ADPLATS-RAD-ATTR (INDX)        
145700                                       MOD-ADGANG-RAD-ATTR (INDX)         
145800             MOVE NEJ            TO INDATA-SW                             
145900             MOVE ERR-PLACE-MISSING TO MED-IDMFSFEL                       
146000          END-IF                                                          
146100        END-IF                                                            
146200                                                                          
146300        IF RAD-FLSATS = 'J' AND ART-ADLAGOMR NOT = 30                     
146400           IF MID-FLKLAR       =  ALL '+' OR SPACE                        
146500              MOVE MFS-ALFA-FAELT-FEL                                     
146600                                  TO MOD-KDCMDVAL-RAD-ATTR(INDX)          
146700           ELSE                                                           
146800              MOVE MFS-ALFA-FAELT-FEL                                     
146900                                  TO MOD-FLKLAR-ATTR                      
147000           END-IF                                                         
147100           MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-RAD-ATTR(INDX)           
147200           MOVE NEJ            TO INDATA-SW                               
147300           MOVE ERR-KIT-CASE   TO MED-IDMFSFEL                            
147400        END-IF                                                            
147500     ELSE                                                                 
147600        IF MID-FLKLAR            =  ALL '+' OR SPACE                      
147700           MOVE MFS-ALFA-FAELT-FEL                                        
147800                                 TO MOD-KDCMDVAL-RAD-ATTR(INDX)           
147900        ELSE                                                              
148000            MOVE MFS-ALFA-FAELT-FEL                                       
148100                                 TO MOD-FLKLAR-ATTR                       
148200        END-IF                                                            
148300        MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDARTNR-RAD-ATTR(INDX)            
148400        MOVE NEJ                 TO INDATA-SW                             
148500        MOVE ERR-MISSING         TO MED-IDMFSFEL                          
148600     END-IF                                                               
148700     .                                                                    
148800     EJECT                                                                
148900 S02-UPPDATERA-INLAEGGNING  SECTION.                                      
149000                                                                          
149100     PERFORM S03-LAES-INLA21                                              
149200                                                                          
149300     IF (RAD-KDINLPRIO < 31 AND RAD-FLPRIO = 'N')                         
149400        PERFORM S10-CALL-W611PMRK-PARTI                                   
149500        PERFORM S03-LAES-INLA21                                           
149600     END-IF                                                               
149700                                                                          
149800     IF RAD-IDRADNR            =  1                                       
149900                                                                          
150000** OBS INGA LÄSNINGAR MOT INLA2 PCB'ET MELLAN  -LAST OCH ISRT             
150100** DET SKULLE FÖRSTÖRA POSITIONERINGEN                                    
150200                                                                          
150300         PERFORM IMS-DLET-INLA2-INLA21                                    
150400         MOVE MID-KVINLART-UPD(INDX) TO W-KVINLART-UPD                    
150500         PERFORM S08-SKAPA-6191-MID-BORT-RAD                              
150600         PERFORM IMS-GNP-INLA2-INLA21-LAST                                
150700         IF SEGMENT-SAKNAS                                                
150800             MOVE +2           TO SPAR-RAD-IDRADNR                        
150900          ELSE                                                            
151000             COMPUTE SPAR-RAD-IDRADNR = RAD-IDRADNR + 1                   
151100         END-IF                                                           
151200         MOVE SPAR-RAD-W6D121  TO RAD-W6D121                              
151300         MOVE 'INL'            TO RAD-KDINLSTA                            
151400         MOVE SPACE            TO RAD-ADINLOMR                            
151500                                  RAD-ADINLOMR-NXT                        
151600         MOVE ZERO             TO RAD-IDINLVGN                            
151700                                  RAD-IDILIST                             
151800                                  RAD-IDILIRAD                            
151900         MOVE DAGENS-DATUM     TO RAD-TIUPPDAT                            
152000         PERFORM IMS-ISRT-INLA2-INLA21                                    
152100         PERFORM S07-SKAPA-6191-MID-NY-RAD                                
152200         PERFORM S06-SKAPA-6193-MID                                       
152300      ELSE                                                                
152400         MOVE 'INL'            TO RAD-KDINLSTA                            
152500         MOVE SPACE            TO RAD-ADINLOMR                            
152600                                  RAD-ADINLOMR-NXT                        
152700         MOVE ZERO             TO RAD-IDINLVGN                            
152800                                  RAD-IDILIST                             
152900                                  RAD-IDILIRAD                            
153000         MOVE DAGENS-DATUM     TO RAD-TIUPPDAT                            
153100         PERFORM IMS-REPL-INLA2-INLA21                                    
153200                                                                          
153300         PERFORM S04-SKAPA-6191-MID                                       
153400         PERFORM S06-SKAPA-6193-MID                                       
153500     END-IF                                                               
153600     .                                                                    
153700     EJECT                                                                
153800 S03-LAES-INLA21         SECTION.                                         
153900                                                                          
154000     INSPECT MID-IDLOPNRM-RAD(INDX) REPLACING                             
154100                                    LEADING SPACE BY ZERO                 
154200     INSPECT MID-IDRADNR-RAD(INDX)  REPLACING                             
154300                                    LEADING SPACE BY ZERO                 
154400     MOVE MID-IDLOPNRM-RAD(INDX) TO W-D1BSEQ-IDLOPNRM                     
154500     PERFORM IMS-GU-INLA2-INLA11                                          
154600     MOVE MID-IDRADNR-RAD(INDX)  TO W-IDRADNR                             
154700     PERFORM IMS-GHNP-INLA2-INLA21                                        
154800     MOVE RAD-W6D121             TO SPAR-RAD-W6D121                       
154900     .                                                                    
155000     EJECT                                                                
155100 S04-SKAPA-6191-MID   SECTION.                                            
155200                                                                          
155300     MOVE 'W6014500'           TO MOD6191-MID-IDPGM                       
155400     MOVE WS-IDDC              TO MOD6191-MID-IDDC                        
155500     MOVE ART-IDLOPNRM         TO MOD6191-MID-IDLOPNRM (6191-IX)          
155600     MOVE RAD-IDRADNR          TO MOD6191-MID-IDRADNR  (6191-IX)          
155700     MOVE ART-PRARTSTD         TO MOD6191-MID-PRARTSTD (6191-IX)          
155800     MOVE ART-KDINLPRIO        TO MOD6191-MID-KDINLPRIO(6191-IX)          
155900     MOVE +0                   TO MOD6191-MID-KVKOLLI  (6191-IX)          
156000     MOVE 'N'                  TO MOD6191-MID-FLINLI   (6191-IX)          
156100                                                                          
156200     MOVE SPAR-RAD-ADINLOMR      TO MOD6191-MID-ADINLOMR-OLD              
156300                                                   (6191-IX)              
156400     MOVE SPAR-RAD-ADINLOMR-NXT                                           
156500                                 TO MOD6191-MID-ADINLOMR-NXT-OLD          
156600                                                   (6191-IX)              
156700     MOVE SPAR-RAD-KDINLSTA      TO MOD6191-MID-KDINLSTA-OLD              
156800                                                   (6191-IX)              
156900     MOVE SPAR-RAD-KVINLART      TO MOD6191-MID-KVINLART-OLD              
157000                                                   (6191-IX)              
157100                                                                          
157200     MOVE RAD-ADINLOMR         TO MOD6191-MID-ADINLOMR-NEW                
157300                                                   (6191-IX)              
157400     MOVE RAD-ADINLOMR-NXT     TO MOD6191-MID-ADINLOMR-NXT-NEW            
157500                                                   (6191-IX)              
157600     MOVE RAD-KDINLSTA         TO MOD6191-MID-KDINLSTA-NEW                
157700                                                   (6191-IX)              
157800     MOVE RAD-KVINLART         TO MOD6191-MID-KVINLART-NEW                
157900                                                   (6191-IX)              
158000     ADD +1                    TO 6191-IX                                 
158100     IF 6191-IX                > MAX-6191-IX                              
158200         PERFORM S05-STARTA-W6T191                                        
158300     END-IF                                                               
158400     .                                                                    
158500     EJECT                                                                
158600 S05-STARTA-W6T191         SECTION.                                       
158700                                                                          
158800     COMPUTE MOD6191-MID-KVPOST = 6191-IX - 1                             
158900     COMPUTE P-TO-P-MSG-KVLL   =  LNG-P-TO-P-PREFIX +                     
159000                                  17 + (MOD6191-MID-KVPOST * 64)          
159100     MOVE 'W6T191X '           TO P-TO-P-MSG-KDTRANS                      
159200     MOVE '6145'               TO P-TO-P-MSG-IDTRANS                      
159300     MOVE MFS-KDMFSFOR         TO P-TO-P-MSG-KDMFSFOR                     
159400                                                                          
159500     MOVE MOD6191-MID-W6I19101 TO P-TO-P-MSG-INDATA                       
159600                                                                          
159700     IF FOERSTA-6191                                                      
159800         PERFORM IMS-ISRT-ALT1-MSG-6191                                   
159900         MOVE NEJ               TO FOERSTA-6191-SW                        
160000      ELSE                                                                
160100         PERFORM IMS-PURG-ALT1-MSG-6191                                   
160200     END-IF                                                               
160300     MOVE +1                   TO 6191-IX                                 
160400     .                                                                    
160500     EJECT                                                                
160600 S06-SKAPA-6193-MID   SECTION.                                            
160700                                                                          
160800     ACCEPT DAGENS-DATUM       FROM DATE                                  
160900     ACCEPT DAGENS-TID         FROM TIME                                  
161000                                                                          
161100     MOVE SPACE                TO MSG-KOM-WMSGKOM                         
161200     COMPUTE MSG-KOM-KVLL = LENGTH OF MSG-KOM-WMSGKOM                     
161300     MOVE LOW-VALUE            TO MSG-KOM-KDZ1                            
161400     MOVE LOW-VALUE            TO MSG-KOM-KDZ2                            
161500     MOVE SPACE                TO MSG-KOM-KDTRANS                         
161600     MOVE 'W6I19301'           TO MSG-KOM-IDCPYTXT                        
161700     MOVE 'INLEV   '           TO MSG-KOM-IDSNDNOD                        
161800     MOVE 'W6014500'           TO MSG-KOM-IDSNDJOB                        
161900     MOVE DAGENS-DATUM         TO MSG-KOM-TIREGDAT                        
162000     MOVE DAGENS-TID           TO MSG-KOM-TIKLOCK                         
162100     MOVE SPACE                TO MSG-KOM-IDMFSMED                        
162200                                                                          
162300     MOVE ART-IDLOPNRM         TO MOD6193-MID-IDLOPNRM                    
162400     MOVE RAD-IDRADNR          TO MOD6193-MID-IDRADNR                     
162500                                                                          
162600     COMPUTE P-TO-P-MSG-KVLL   =  LNG-P-TO-P-PREFIX + 12                  
162700     MOVE 'W6T193X '           TO P-TO-P-MSG-KDTRANS                      
162800     MOVE '6145'               TO P-TO-P-MSG-IDTRANS                      
162900     MOVE MFS-KDMFSFOR         TO P-TO-P-MSG-KDMFSFOR                     
163000                                                                          
163100     MOVE MOD6193-MID-W6I19301 TO P-TO-P-MSG-INDATA                       
163200                                                                          
163300     CALL W006KOM USING MSG-PCB                                           
163400                        DISP-PCB                                          
163500                        KOM-KOMA-PCB                                      
163600                        MSG-KOM-WMSGKOM                                   
163700                        P-TO-P-MSG-IO-AREA-SNUF                           
163800     .                                                                    
163900     EJECT                                                                
164000 S07-SKAPA-6191-MID-NY-RAD   SECTION.                                     
164100                                                                          
164200     MOVE 'W6014500'           TO MOD6191-MID-IDPGM                       
164300     MOVE W-IDDC               TO MOD6191-MID-IDDC                        
164400     MOVE ART-IDLOPNRM         TO MOD6191-MID-IDLOPNRM (6191-IX)          
164500     MOVE RAD-IDRADNR          TO MOD6191-MID-IDRADNR  (6191-IX)          
164600     MOVE ART-PRARTSTD         TO MOD6191-MID-PRARTSTD (6191-IX)          
164700     MOVE ART-KDINLPRIO        TO MOD6191-MID-KDINLPRIO(6191-IX)          
164800     MOVE +0                   TO MOD6191-MID-KVKOLLI  (6191-IX)          
164900     MOVE 'N'                  TO MOD6191-MID-FLINLI   (6191-IX)          
165000                                                                          
165100     MOVE SPACE                TO MOD6191-MID-ADINLOMR-OLD                
165200                                                   (6191-IX)              
165300                                  MOD6191-MID-ADINLOMR-NXT-OLD            
165400                                                   (6191-IX)              
165500                                  MOD6191-MID-KDINLSTA-OLD                
165600                                                   (6191-IX)              
165700     MOVE ZERO                 TO MOD6191-MID-KVINLART-OLD                
165800                                                   (6191-IX)              
165900                                                                          
166000     MOVE SPAR-RAD-ADINLOMR    TO MOD6191-MID-ADINLOMR-NEW                
166100                                                   (6191-IX)              
166200     MOVE RAD-ADINLOMR-NXT     TO MOD6191-MID-ADINLOMR-NXT-NEW            
166300                                                   (6191-IX)              
166400     MOVE RAD-KDINLSTA         TO MOD6191-MID-KDINLSTA-NEW                
166500                                                   (6191-IX)              
166600     MOVE RAD-KVINLART         TO MOD6191-MID-KVINLART-NEW                
166700                                                   (6191-IX)              
166800     ADD +1                    TO 6191-IX                                 
166900     IF 6191-IX                > MAX-6191-IX                              
167000         PERFORM S05-STARTA-W6T191                                        
167100     END-IF                                                               
167200     .                                                                    
167300     EJECT                                                                
167400 S08-SKAPA-6191-MID-BORT-RAD   SECTION.                                   
167500                                                                          
167600     MOVE 'W6014500'           TO MOD6191-MID-IDPGM                       
167700     MOVE WS-IDDC              TO MOD6191-MID-IDDC                        
167800     MOVE ART-IDLOPNRM         TO MOD6191-MID-IDLOPNRM (6191-IX)          
167900     MOVE RAD-IDRADNR          TO MOD6191-MID-IDRADNR  (6191-IX)          
168000     MOVE ART-PRARTSTD         TO MOD6191-MID-PRARTSTD (6191-IX)          
168100     MOVE ART-KDINLPRIO        TO MOD6191-MID-KDINLPRIO(6191-IX)          
168200     MOVE +0                   TO MOD6191-MID-KVKOLLI  (6191-IX)          
168300     MOVE 'J'                  TO MOD6191-MID-FLINLI   (6191-IX)          
168400                                                                          
168500     MOVE SPAR-RAD-ADINLOMR      TO MOD6191-MID-ADINLOMR-OLD              
168600                                                   (6191-IX)              
168700     MOVE SPAR-RAD-ADINLOMR-NXT  TO MOD6191-MID-ADINLOMR-NXT-OLD          
168800                                                   (6191-IX)              
168900     MOVE SPAR-RAD-KDINLSTA      TO MOD6191-MID-KDINLSTA-OLD              
169000                                                   (6191-IX)              
169100     MOVE SPAR-RAD-KVINLART      TO MOD6191-MID-KVINLART-OLD              
169200                                                   (6191-IX)              
169300                                                                          
169400     MOVE SPACE                TO MOD6191-MID-ADINLOMR-NEW                
169500                                                   (6191-IX)              
169600                                  MOD6191-MID-ADINLOMR-NXT-NEW            
169700                                                   (6191-IX)              
169800                                  MOD6191-MID-KDINLSTA-NEW                
169900                                                   (6191-IX)              
170000     MOVE ZERO                 TO MOD6191-MID-KVINLART-NEW                
170100                                                   (6191-IX)              
170200     ADD +1                    TO 6191-IX                                 
170300     IF 6191-IX                > MAX-6191-IX                              
170400         PERFORM S05-STARTA-W6T191                                        
170500     END-IF                                                               
170600     .                                                                    
170700     EJECT                                                                
170800 S10-CALL-W611PMRK-PARTI   SECTION.                                       
170900                                                                          
171000     MOVE SPACE                TO PMRK-IDLEVNR                            
171100     MOVE ZERO                 TO PMRK-IDOKOLLI                           
171200     MOVE ART-IDLOPNRM         TO PMRK-IDLOPNRM                           
171300     MOVE RAD-IDRADNR          TO PMRK-IDRADNR                            
171400     CALL W611PMRK USING PMRK-W611PMRK PMRK-INLB-PCB                      
171500                         PMRK-INLC-PCB PMRK-PLAA-PCB                      
171600     .                                                                    
171700     EJECT                                                                
171800 MFS-RENSA-FAELT-UT SECTION.                                              
171900                                                                          
172000     PERFORM MFS-RENSA-RAD-FAELT-UT                                       
172100     .                                                                    
172200     SKIP2                                                                
172300 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
172400                                                                          
172500     MOVE +1                   TO INDX                                    
172600     PERFORM UNTIL INDX        >  MAX-INDX                                
172700         MOVE MFS-RENSA-FAELT  TO MOD-ADLAGOMR-RAD(INDX)                  
172800                                  MOD-ADPLATS-RAD (INDX)                  
172900                                  MOD-ADGANG-RAD  (INDX)                  
173000                                  MOD-IDARTNR-RAD (INDX)                  
173100                                  MOD-BEART-RAD   (INDX)                  
173200                                  MOD-KVINLART-RAD(INDX)                  
173300                                  MOD-KVINLART-VOR-RAD(INDX)              
173400                                  MOD-IDLOPNRM-RAD(INDX)                  
173500                                  MOD-IDRADNR-RAD (INDX)                  
173600                                  MOD-BEFARLIG-RAD(INDX)                  
173700                                  MOD-KDKLIPRI-RAD(INDX)                  
173800         ADD +1                TO INDX                                    
173900     END-PERFORM                                                          
174000     .                                                                    
174100     EJECT                                                                
174200 MFS-RENSA-FAELT-IN SECTION.                                              
174300                                                                          
174400     MOVE MFS-RENSA-FAELT      TO MOD-FLKLAR                              
174500                                  MOD-IDANSTNR                            
174600     PERFORM MFS-RENSA-RAD-FAELT-IN                                       
174700     .                                                                    
174800     SKIP2                                                                
174900 MFS-RENSA-RAD-FAELT-IN SECTION.                                          
175000                                                                          
175100     MOVE +1                   TO INDX                                    
175200     PERFORM UNTIL INDX        >  MAX-INDX                                
175300         MOVE MFS-RENSA-FAELT  TO MOD-KDCMDVAL-RAD    (INDX)              
175400                                  MOD-KVINLART-UPD    (INDX)              
175500                                  MOD-ADINLOMR-NXT-UPD(INDX)              
175600         ADD +1                TO INDX                                    
175700     END-PERFORM                                                          
175800     .                                                                    
175900     EJECT                                                                
176000 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
176100                                                                          
176200     PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                     
176300     .                                                                    
176400     SKIP2                                                                
176500 MFS-ROER-EJ-RAD-FAELT-UT SECTION.                                        
176600                                                                          
176700     MOVE +1                    TO INDX                                   
176800     PERFORM UNTIL INDX         >  MAX-INDX                               
176900         MOVE MFS-ROER-EJ-FAELT TO MOD-ADLAGOMR-RAD(INDX)                 
177000                                   MOD-ADPLATS-RAD (INDX)                 
177100                                   MOD-ADGANG-RAD  (INDX)                 
177200                                   MOD-IDARTNR-RAD (INDX)                 
177300                                   MOD-BEART-RAD   (INDX)                 
177400                                   MOD-KVINLART-RAD(INDX)                 
177500                                   MOD-KVINLART-VOR-RAD(INDX)             
177600                                   MOD-IDLOPNRM-RAD(INDX)                 
177700                                   MOD-IDRADNR-RAD (INDX)                 
177800                                   MOD-BEFARLIG-RAD(INDX)                 
177900                                   MOD-KDKLIPRI-RAD(INDX)                 
178000         ADD +1                 TO INDX                                   
178100     END-PERFORM                                                          
178200     .                                                                    
178300     EJECT                                                                
178400 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
178500                                                                          
178600     MOVE MFS-ROER-EJ-FAELT    TO MOD-FLKLAR                              
178700                                  MOD-IDANSTNR                            
178800     PERFORM MFS-ROER-EJ-RAD-FAELT-IN                                     
178900     .                                                                    
179000     SKIP2                                                                
179100 MFS-ROER-EJ-RAD-FAELT-IN SECTION.                                        
179200                                                                          
179300     MOVE +1                    TO INDX                                   
179400     PERFORM UNTIL INDX         >  MAX-INDX                               
179500         MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMDVAL-RAD    (INDX)             
179600                                   MOD-KVINLART-UPD    (INDX)             
179700                                   MOD-ADINLOMR-NXT-UPD(INDX)             
179800         ADD +1                 TO INDX                                   
179900     END-PERFORM                                                          
180000     .                                                                    
180100     EJECT                                                                
180200 MFS-FORM-ATTR SECTION.                                                   
180300                                                                          
180400     MOVE MFS-FORMATETS-ATTR TO MOD-FLKLAR-ATTR                           
180500                                MOD-IDANSTNR-ATTR                         
180600     PERFORM MFS-FORM-ATTR-RAD                                            
180700     .                                                                    
180800     SKIP2                                                                
180900 MFS-FORM-ATTR-RAD        SECTION.                                        
181000                                                                          
181100     MOVE +1                   TO INDX                                    
181200     PERFORM UNTIL INDX        >  MAX-INDX                                
181300         MOVE MFS-FORMATETS-ATTR                                          
181400                               TO MOD-KDCMDVAL-RAD-ATTR    (INDX)         
181500                                  MOD-KVINLART-UPD-ATTR    (INDX)         
181600                                  MOD-ADINLOMR-NXT-UPD-ATTR(INDX)         
181700         ADD +1                TO INDX                                    
181800     END-PERFORM                                                          
181900     .                                                                    
182000     EJECT                                                                
182100* --- IMS SEKTIONER ---                                                   
182200     SKIP3                                                                
182300 IMS-GET-MSG SECTION.                                                     
182400                                                                          
182500     MOVE '  QC' TO GODK-STATUSKODER                                      
182600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
182700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
182800     PERFORM IMS-STATUSKONTROLL                                           
182900     .                                                                    
183000     SKIP3                                                                
183100 IMS-INSERT-MSG SECTION.                                                  
183200                                                                          
183300     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
183400       MOVE '0' TO MFS-KDHUVOMR                                           
183500     END-IF                                                               
183600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
183700     MOVE SPACE TO GODK-STATUSKODER                                       
183800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
183900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
184000     PERFORM IMS-STATUSKONTROLL                                           
184100     .                                                                    
184200     EJECT                                                                
184300 IMS-ISRT-ALT1-MSG-6191  SECTION.                                         
184400     MOVE SPACE TO GODK-STATUSKODER                                       
184500     CALL  CBLTDLI  USING ISRT ALT1-PCB P-TO-P-MSG-IO-AREA-SNUF           
184600     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
184700     PERFORM IMS-STATUSKONTROLL                                           
184800     .                                                                    
184900     SKIP3                                                                
185000 IMS-PURG-ALT1-MSG-6191  SECTION.                                         
185100     MOVE SPACE TO GODK-STATUSKODER                                       
185200     CALL  CBLTDLI  USING PURG ALT1-PCB P-TO-P-MSG-IO-AREA-SNUF           
185300     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
185400     PERFORM IMS-STATUSKONTROLL                                           
185500     .                                                                    
185600     SKIP3                                                                
185700 IMS-GU-INLA1-INLA11 SECTION.                                             
185800     STRING 'W6INLA11(W6D1CSEQ =' W-W6D1CSEQ-X                            
185900                    '&IDDC     =' W-IDDC ')'                              
186000          DELIMITED BY SIZE INTO SSA1                                     
186100     MOVE '  GE' TO GODK-STATUSKODER                                      
186200     CALL CBLTDLI USING GU INLA1-PCB DLI-IO-AREA1 SSA1                    
186300     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
186400     PERFORM IMS-STATUSKONTROLL                                           
186500     .                                                                    
186600     SKIP2                                                                
186700 IMS-GN-INLA1-INLA11 SECTION.                                             
186800     STRING 'W6INLA11(W6D1CSEQ =' W-W6D1CSEQ-X ')'                        
186900          DELIMITED BY SIZE INTO SSA1                                     
187000     MOVE '  GE' TO GODK-STATUSKODER                                      
187100     CALL CBLTDLI USING GN INLA1-PCB DLI-IO-AREA1 SSA1                    
187200     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
187300     PERFORM IMS-STATUSKONTROLL                                           
187400     .                                                                    
187500     EJECT                                                                
187600 IMS-GNP-INLA1-INLA21 SECTION.                                            
187700     STRING 'W6INLA21(IDLEVNRK =' W-IDLEVNR-KOLLI-X                       
187800                    '&IDOKOLLI =' W-IDOKOLLI-X ')'                        
187900          DELIMITED BY SIZE INTO SSA1                                     
188000     MOVE '  GE' TO GODK-STATUSKODER                                      
188100     CALL CBLTDLI USING GNP INLA1-PCB DLI-IO-AREA2 SSA1                   
188200     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
188300     PERFORM IMS-STATUSKONTROLL                                           
188400     .                                                                    
188500     EJECT                                                                
188600 IMS-GU-INLA2-INLA11 SECTION.                                             
188700     STRING 'W6INLA11(W6D1BSEQ =' W-W6D1BSEQ-X                            
188800                    '&IDDC     =' W-IDDC ')'                              
188900          DELIMITED BY SIZE INTO SSA1                                     
189000     MOVE '  GE' TO GODK-STATUSKODER                                      
189100     CALL CBLTDLI USING GU INLA2-PCB DLI-IO-AREA1 SSA1                    
189200     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
189300     PERFORM IMS-STATUSKONTROLL                                           
189400     .                                                                    
189500     SKIP3                                                                
189600 IMS-GU-INLA2-INLA21 SECTION.                                             
189700     STRING 'W6INLA11(W6D1BSEQ =' W-W6D1BSEQ-X                            
189800                    '&IDDC     =' W-IDDC ')'                              
189900          DELIMITED BY SIZE INTO SSA1                                     
190000     STRING 'W6INLA21(IDRADNR  =' W-IDRADNR-X ')'                         
190100          DELIMITED BY SIZE INTO SSA2                                     
190200     MOVE '  GE' TO GODK-STATUSKODER                                      
190300     CALL CBLTDLI USING GU INLA2-PCB DLI-IO-AREA2 SSA1 SSA2               
190400     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
190500     PERFORM IMS-STATUSKONTROLL                                           
190600     .                                                                    
190700     SKIP3                                                                
190800 IMS-GNP-INLA2-INLA21-LAST SECTION.                                       
190900     MOVE 'W6INLA21*L' TO SSA1                                            
191000     MOVE '  GE' TO GODK-STATUSKODER                                      
191100     CALL CBLTDLI USING GNP INLA2-PCB DLI-IO-AREA2 SSA1                   
191200     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
191300     PERFORM IMS-STATUSKONTROLL                                           
191400     .                                                                    
191500     SKIP3                                                                
191600 IMS-GHNP-INLA2-INLA21 SECTION.                                           
191700     STRING 'W6INLA21(IDRADNR  =' W-IDRADNR-X ')'                         
191800          DELIMITED BY SIZE INTO SSA1                                     
191900     MOVE '    ' TO GODK-STATUSKODER                                      
192000     CALL CBLTDLI USING GHNP INLA2-PCB DLI-IO-AREA2 SSA1                  
192100     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
192200     PERFORM IMS-STATUSKONTROLL                                           
192300     .                                                                    
192400     SKIP3                                                                
192500 IMS-DLET-INLA2-INLA21 SECTION.                                           
192600     MOVE '    ' TO GODK-STATUSKODER                                      
192700     CALL CBLTDLI USING DLET INLA2-PCB DLI-IO-AREA2                       
192800     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
192900     PERFORM IMS-STATUSKONTROLL                                           
193000     .                                                                    
193100     SKIP3                                                                
193200 IMS-REPL-INLA2-INLA21 SECTION.                                           
193300     MOVE '    ' TO GODK-STATUSKODER                                      
193400     CALL CBLTDLI USING REPL INLA2-PCB DLI-IO-AREA2                       
193500     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
193600     PERFORM IMS-STATUSKONTROLL                                           
193700     .                                                                    
193800     SKIP3                                                                
193900 IMS-ISRT-INLA2-INLA21 SECTION.                                           
194000     MOVE 'W6INLA21' TO SSA1                                              
194100     MOVE '    ' TO GODK-STATUSKODER                                      
194200     CALL CBLTDLI USING ISRT INLA2-PCB DLI-IO-AREA2 SSA1                  
194300     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
194400     PERFORM IMS-STATUSKONTROLL                                           
194500     .                                                                    
194600     SKIP3                                                                
194700 IMS-GU-PLAA-PLAA11 SECTION.                                              
194800     STRING 'W6PLAA01(W6GXKEY  =' W-W6GXKEY-6005-X ')'                    
194900          DELIMITED BY SIZE INTO SSA1                                     
195000     STRING 'W6PLAA11(W6GXKEY  =' W-W6GXKEY-6006-X ')'                    
195100          DELIMITED BY SIZE INTO SSA2                                     
195200     MOVE '  GE' TO GODK-STATUSKODER                                      
195300     CALL CBLTDLI USING GU PLAA-PCB DLI-IO-AREA3 SSA1 SSA2                
195400     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
195500     PERFORM IMS-STATUSKONTROLL                                           
195600     .                                                                    
195700     SKIP3                                                                
195800 IMS-GU-UPFA-01  SECTION.                                                 
195900     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
196000          DELIMITED BY SIZE INTO SSA1                                     
196100     MOVE '  GE' TO GODK-STATUSKODER                                      
196200     CALL CBLTDLI USING GU UPFA-PCB DLI-IO-UPFA SSA1                      
196300     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
196400     PERFORM IMS-STATUSKONTROLL                                           
196500     .                                                                    
196600     SKIP2                                                                
196700 IMS-GNP-UPFA11 SECTION.                                                  
196800                                                                          
196900     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
197000          DELIMITED BY SIZE INTO SSA1                                     
197100     MOVE 'W6UPFA11 ' TO SSA2                                             
197200     MOVE '  GE' TO GODK-STATUSKODER                                      
197300     CALL CBLTDLI USING GNP UPFA-PCB DLI-IO-AREA-UPFA11 SSA1 SSA2         
197400     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
197500     PERFORM IMS-STATUSKONTROLL                                           
197600     .                                                                    
197700     SKIP3                                                                
197800 IMS-GNP-UPFA12 SECTION.                                                  
197900                                                                          
198000     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
198100          DELIMITED BY SIZE INTO SSA1                                     
198200     MOVE 'W6UPFA12 ' TO SSA2                                             
198300     MOVE '  GE' TO GODK-STATUSKODER                                      
198400     CALL CBLTDLI USING GNP UPFA-PCB DLI-IO-AREA-UPFA12 SSA1 SSA2         
198500     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
198600     PERFORM IMS-STATUSKONTROLL                                           
198700     .                                                                    
198800     SKIP3                                                                
198900 IMS-STATUSKONTROLL SECTION.                                              
199000                                                                          
199100     SET STATUS-IX TO 1                                                   
199200     SEARCH GODK-STATUS                                                   
199300       AT END                                                             
199400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
199500         DELIMITED BY SIZE INTO FELTEXT                                   
199600         CALL FELLOG                                                      
199700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
199800         CONTINUE                                                         
199900     END-SEARCH                                                           
200000     .                                                                    
