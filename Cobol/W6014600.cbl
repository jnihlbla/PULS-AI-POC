000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6014600.                                                
000400*AUTHOR.         LENA EKBERG.                                             
000500*DATE-WRITTEN.   92/12/08.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        SLUTRAPPORTERING UTAN AVVIKELSE.                                 
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR W6INLA (W6D1)                              
001300*        PROGRAMMET LÄSER      W6UPFA (W6L1)                              
001400*                                                                         
001500*        SKICKAR TRANS TILL W6T191X                                       
001600*                           W6T193X                                       
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSAKTION: W6T146                                              
002000*        MID:         W6I14601                                            
002100*                                                                         
002200*    UTDATA.                                                              
002300*        MOD:         W6O14601                                            
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900 WORKING-STORAGE SECTION.                                                 
002901                                                                          
002910*    -- CHECKED BY WY2000                                                 
003000 77  IDPGM                       PIC X(08)   VALUE 'W6014600'.            
003100                                                                          
003200*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003300 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003400                                                                          
003500 77  JA                          PIC X       VALUE 'J'.                   
003510 77  YES                         PIC X       VALUE 'J'.                   
003600 77  NEJ                         PIC X       VALUE 'N'.                   
003700                                                                          
003800 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
003900 77  RAD-IX                      PIC S9(9)  VALUE +0    COMP SYNC.        
004000 77  MAX-RAD-IX                  PIC S9(9)  VALUE +13   COMP SYNC.        
004100 77  6191-IX                     PIC S9(9)  VALUE +0    COMP SYNC.        
004200 77  MAX-6191-IX                 PIC S9(9)  VALUE +24   COMP SYNC.        
004300                                                                          
004400     SKIP3                                                                
004500 01  W-TAB-INAREA.                                                        
004600     03  W-TAB-IX                PIC S9(9) VALUE ZERO COMP SYNC.          
004700     03  W-TAB-INRAD          OCCURS 13.                                  
004800         05  W-TAB-IDLOPNRM      PIC S9(9) COMP-3.                        
004900         05  W-TAB-FLSVAR        PIC X.                                   
005000 01  W-KVTRANS                   PIC S9(3) VALUE ZERO COMP-3.             
005001 01  W-KVTRANS-IN                PIC S9(3) VALUE ZERO COMP-3.             
005010 01  W-PRARTSTD             PIC S9(7)V9(2) VALUE ZERO COMP-3.             
005020 01  W-KDINLPRIO                 PIC S9(3) VALUE ZERO COMP-3.             
005030 01  WS-ADINLOMR                 PIC  X(4) VALUE SPACE.                   
005050 01  WS-ART-IDLOPNRM             PIC S9(9) VALUE ZERO COMP-3.             
005100     EJECT                                                                
005200 01  SPAR-AREA.                                                           
005300*    03  -COPY W6D121  -PRE SPAR-                                         
005400     EJECT                                                                
005500*   OM SVAR TILL SKÄRM: MAX-MOD-LAENGD = MOD-LÄNGD + 4                    
005600*   OM PROGRAM-TILL-PROGRAM-SWITCH:    = MOD-LÄNGD + 17                   
005700 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +813  COMP SYNC.        
005800 77  LNG-P-TO-P-PREFIX           PIC S9(4)  VALUE +17   COMP SYNC.        
005900                                                                          
006000*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
006100                                                                          
006200 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006300     88  INDATA-OK                           VALUE 'J'.                   
006400     88  INDATA-FEL                          VALUE 'N'.                   
006500                                                                          
006510 77  RAD-SW                      PIC X       VALUE 'J'.                   
006520     88  RAD-OK                              VALUE 'J'.                   
006530     88  RAD-FEL                             VALUE 'N'.                   
006531                                                                          
006532 77  NAGON-RAD-FELAKTIG          PIC X       VALUE 'N'.                   
006533     88  NAGON-RAD-FEL                       VALUE 'J'.                   
006540                                                                          
006600 77  FOERSTA-6191-SW             PIC X       VALUE 'J'.                   
006700     88  FOERSTA-6191                        VALUE 'J'.                   
006800                                                                          
006900 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007000     88  NYCKLAR-OK                          VALUE 'J'.                   
007100     88  NYCKLAR-FEL                         VALUE 'N'.                   
007200                                                                          
007300 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007400     88  EGEN-MID                            VALUE '6146'.                
007500     88  GODK-MID                            VALUE '6141' '6142'          
007600                                                   '6143' '6144'          
007700                                                   '6145' '6146'          
007800                                                   '6147'.                
007900     88  HELP-MID                            VALUE '0551'.                
008000     EJECT                                                                
008010*      --- VALID IDDC CODES                                               
008020*                                                                         
008030*01    -COPY WWDC99                                                       
008040       EJECT                                                              
008100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008200 01  GENERELLA-SUBPROGRAM.                                                
008300     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008600     03  W006KOM                 PIC X(8)    VALUE 'W006KOM'.             
008610     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
008700     EJECT                                                                
008710*01 -COPY WMSGINIT                                                        
008720     SKIP3                                                                
008800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
008900*01 -COPY WMEDAREA                                                        
009000     SKIP3                                                                
009100 01  MESSAGE-CODES.                                                       
009200     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
009300     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
009400     03  ERR-007                 PIC X(3)    VALUE '007'.                 
009500     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
009600     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
009700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
009800     03  ERR-MISSING             PIC X(3)    VALUE '010'.                 
009900     03  ERR-178                 PIC X(3)    VALUE '178'.                 
010000     03  ERR-QUALITY             PIC X(3)    VALUE '189'.                 
010100     03  ERR-ADPLATS             PIC X(3)    VALUE '764'.                 
010110     03  ERR-CONTROL-NOT-COMPL   PIC X(3)    VALUE '215'.                 
010120     03  ERR-WEIGHT-MISSING      PIC X(3)    VALUE '792'.                 
010130     03  ERR-VOLUME-MISSING      PIC X(3)    VALUE '793'.                 
010140     03  ERR-ORIGIN-MISSING      PIC X(3)    VALUE '794'.                 
010200     EJECT                                                                
010300*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
010400*                                                                         
010500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
010600     SKIP3                                                                
010700*01  MID -COPY W6I14601                                                   
010800     EJECT                                                                
010900 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
011000     SKIP3                                                                
011100*01  -COPY WMSGAREA                                                       
011200     EJECT                                                                
011300     03  MOD REDEFINES MSG-AREA.                                          
011400*      05  -COPY W6O14601                                                 
011500     EJECT                                                                
011600 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
011700     SKIP3                                                                
011800*01  -COPY WMFSAREA                                                       
011900     EJECT                                                                
012000 01  KOM-MSG-IO-AREA.                                                     
012100*03  -COPY WMSGKOM                                                        
012200     EJECT                                                                
012300 01  FILLER            PIC X(16)  VALUE 'MSG/KOM-AREA'.                   
012400     SKIP3                                                                
012500*01  -COPY WMSGSNUF    -PRE P-TO-P-                                       
012600     EJECT                                                                
012700 01  FILLER            PIC X(24)  VALUE                                   
012800                               'MOD6191-MID-W6I19101'.                    
012900     SKIP2                                                                
013000     -COPY W6I19101 -PRE MOD6191-                                         
013100     EJECT                                                                
013200 01  FILLER            PIC X(24)  VALUE                                   
013300                               'MOD6193-MID-W6I19301'.                    
013400     SKIP2                                                                
013500     -COPY W6I19301 -PRE MOD6193-                                         
013600     EJECT                                                                
013700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013800*                                                                         
013900     SKIP3                                                                
014000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014100     SKIP3                                                                
014200 01  NYCKLAR-TILL-DLI.                                                    
014300     03  W-W6D1BSEQ-X.                                                    
014400         05  W-IDLOPNRM          PIC S9(9)    VALUE ZERO COMP-3.          
014500     03  W-IDRADNR-X.                                                     
014600         05  W-IDRADNR           PIC S9(5)    VALUE ZERO COMP-3.          
015100     03  W-IDUSER-X.                                                      
015200         05  W-IDUSER            PIC X(08)    VALUE SPACE.                
015210     03  W-IDDC                  PIC X(2)     VALUE SPACE.                
015220*--------W6L101                                                           
015230     03  W-IDLOPNRM-X.                                                    
015240         05  W-IDLOPNRM-W6L1         PIC S9(9) COMP-3 VALUE ZERO.         
015250                                                                          
015300     SKIP2                                                                
015400*    --- STATUS-KOD FRÅN IMS                                              
015500 01  STATUS-WS                   PIC XX.                                  
015600     88  SEGMENT-FINNS                       VALUE '  '.                  
015700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
015800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
015900     SKIP2                                                                
016000 01  GODK-STATUSKODER.                                                    
016100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016200     SKIP3                                                                
016300 01  SSA1                        PIC X(64).                               
016400 01  SSA2                        PIC X(64).                               
016500     EJECT                                                                
016600*    --- IMS FUNKTIONSKODER                                               
016700*01  -COPY W0003                                                          
016800     EJECT                                                                
016900*    ---  DLI INPUT-OUTPUT AREA                                           
017000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
017100     SKIP3                                                                
017200 01  DLI-IO-AREA.                                                         
017300     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
017400     SKIP3                                                                
017500     03  W6INLA11 REDEFINES IO-AREA.                                      
017600*        05  -COPY W6D111  -PRE INLA-                                     
017700     EJECT                                                                
017800     03  W6INLA21 REDEFINES IO-AREA.                                      
017900*        05  -COPY W6D121  -PRE INLA-                                     
018000     EJECT                                                                
018100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-UPFA'.         
018200     SKIP3                                                                
018300 01  DLI-IO-UPFA.                                                         
018400     03  IO-UPFA                PIC X(100)  VALUE SPACE.                  
018500     03  W6UPFA01 REDEFINES IO-UPFA.                                      
018600*        05  -COPY W6L101                                                 
018610     EJECT                                                                
018620 01  DLI-IO-AREA-UPFA11.                                                  
018630     03  W6UPFA11.                                                        
018640*        05  -COPY W6L111                                                 
018650     SKIP3                                                                
018660 01  DLI-IO-AREA-UPFA12.                                                  
018670     03  W6UPFA12.                                                        
018680*        05  -COPY W6L112                                                 
018690     EJECT                                                                
018700 LINKAGE SECTION.                                                         
018800                                                                          
018900*01  -COPY W0009   -PRE MSG-                                              
019000     EJECT                                                                
019100*01  -COPY W0009   -PRE ALT1-                                             
019200     EJECT                                                                
019300*01  -COPY W0009   -PRE DISP-                                             
019400     EJECT                                                                
019500*01  -COPY W0008   -PRE USEA-                                             
019600     05  FILLER                  PIC X(10).                               
019700     EJECT                                                                
019710*01  -COPY W0008   -PRE INLA-                                             
019720     05  FILLER                  PIC X(10).                               
019730     EJECT                                                                
019740*01  -COPY W0008   -PRE UPFA-                                             
019750     05  FILLER                  PIC X.                                   
019760     SKIP3                                                                
020100*01  -COPY W0008   -PRE KOMA-                                             
020200     05  FILLER                  PIC X(20).                               
020300     EJECT                                                                
020400 PROCEDURE DIVISION  USING MSG-PCB ALT1-PCB DISP-PCB USEA-PCB             
020500                           INLA-PCB UPFA-PCB KOMA-PCB.                    
020600     ENTRY 'DLITCBL' USING MSG-PCB ALT1-PCB DISP-PCB USEA-PCB             
020700                           INLA-PCB UPFA-PCB KOMA-PCB.                    
020800                                                                          
020900     PERFORM IMS-GET-MSG                                                  
021000     IF SEGMENT-FINNS                                                     
021100       PERFORM A-INIT                                                     
021210       IF EGEN-MID                                                        
021220         PERFORM B-KOLLA-IDDC                                             
021230         IF NYCKLAR-OK                                                    
021300           PERFORM G-KOLLA-INPUT                                          
021400           IF INDATA-OK                                                   
021500             PERFORM H-UPPDATERA                                          
021600           END-IF                                                         
021610         END-IF                                                           
021700       ELSE                                                               
021800         PERFORM E-SAMMA-SIDA                                             
021900       END-IF                                                             
021910       PERFORM F-VISA-ANT-REG                                             
022000       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
022100       PERFORM IMS-INSERT-MSG                                             
022200     END-IF                                                               
022300                                                                          
022400     MOVE ZERO TO RETURN-CODE                                             
022500     GOBACK                                                               
022600     .                                                                    
022700     EJECT                                                                
022800 A-INIT SECTION.                                                          
022900                                                                          
023000     IF MSG-DUBBLA-TRANSKODER                                             
023100       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I14601                 
023200       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
023300       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
023400     ELSE                                                                 
023500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I14601                  
023600       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
023700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
023800     END-IF                                                               
023900                                                                          
024000     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
024100     MOVE MSG-IDPFK TO MFS-IDPFK                                          
024200     MOVE MFS-IDTRANS TO W-IDTRANS                                        
024300                                                                          
024400     MOVE LOW-VALUE TO MSG-AREA                                           
024500     MOVE 'W6O146N1' TO MFS-IDMOD                                         
024600     MOVE '6146' TO MOD-IDTRANS                                           
024700     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
024800                                                                          
024900     IF EGEN-MID OR HELP-MID                                              
025000       CONTINUE                                                           
025100     ELSE                                                                 
025200       MOVE SPACE TO MFS-KDTRTYP                                          
025300       MOVE '7' TO MFS-IDPFK                                              
025400     END-IF                                                               
025500                                                                          
026201     MOVE ZERO       TO W-KVTRANS                                         
026202                                                                          
026203     PERFORM AA-INIT-NYCKLAR                                              
026204                                                                          
026205     IF MSGI-IDLAND-SPR = 'GB'                                            
026206       MOVE +2 TO SPRAK-IX                                                
026207       MOVE 'GB ' TO MED-IDSKYLT                                          
026208     ELSE                                                                 
026209       MOVE +1 TO SPRAK-IX                                                
026210       MOVE 'S  ' TO MED-IDSKYLT                                          
026211     END-IF                                                               
026212     .                                                                    
026213     EJECT                                                                
026214*----------------------------------------------------------------*        
026215 AA-INIT-NYCKLAR SECTION.                                                 
026216                                                                          
026217     MOVE ALL '+' TO MSGI-WMSGINIT                                        
026218     MOVE '001'                  TO MSGI-KDCALL                           
026219     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
026220     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
026221     MOVE '6146'                 TO MSGI-IDTRANS                          
026230     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
026300     .                                                                    
026400     EJECT                                                                
026410 B-KOLLA-IDDC SECTION.                                                    
026420                                                                          
026430     MOVE MFS-RENSA-FAELT  TO MOD-IDDC-IN                                 
026440                                                                          
026450     IF MID-IDDC-IN = ALL '+'                                             
026460        MOVE MSGI-IDDC    TO WS-IDDC                                      
026470     ELSE                                                                 
026480        MOVE MID-IDDC-IN  TO WS-IDDC                                      
026490     END-IF                                                               
026491                                                                          
026492     IF CDC                                                               
026495       MOVE WS-IDDC        TO W-IDDC                                      
026496     ELSE                                                                 
026497       MOVE NEJ            TO NYCKLAR-SW                                  
026498       MOVE ERR-WRONG-KEY  TO MED-IDMFSFEL                                
026499       CALL WMEDKONV USING MED-WMEDAREA                                   
026500       MOVE MED-MFSFEL     TO MOD-TEMFSFEL                                
026501       PERFORM MFS-ROER-EJ-FAELT-IN                                       
026502       PERFORM MFS-ROER-EJ-FAELT-UT                                       
026503       PERFORM MFS-ADD-LAES-IN                                            
026504     END-IF                                                               
026505                                                                          
026506     IF NYCKLAR-OK OR EGEN-MID OR GODK-MID                                
026507        MOVE WS-IDDC     TO MOD-IDDC-UT                                   
026508     ELSE                                                                 
026509        MOVE MFS-RENSA-FAELT TO MOD-IDDC-UT                               
026510     END-IF                                                               
026511     .                                                                    
026512     EJECT                                                                
026520 E-SAMMA-SIDA SECTION.                                                    
026600                                                                          
026700     IF HELP-MID                                                          
026800       IF MID-INPUT = ALL '+'                                             
026900         PERFORM MFS-RENSA-FAELT-IN                                       
027000       ELSE                                                               
027100         PERFORM EA-MID-INDATA-TILL-MOD                                   
027200       END-IF                                                             
027300     ELSE                                                                 
027400       PERFORM MFS-RENSA-FAELT-IN                                         
027500     END-IF                                                               
027600     .                                                                    
027700     EJECT                                                                
027800 EA-MID-INDATA-TILL-MOD SECTION.                                          
027900                                                                          
028000* * * * * FÖR VARJE MID-FÄLT                                              
028100* * * * * OM MID-FÄLT NOT = ALL '+' FLYTTA MID-FÄLT TILL MOD-INDAT        
028200* * * * *        FLYTTA MFS-ADD-LAES-IN-FAELT TILL MOD-INDATA-ATTR        
028300* * * * * ANNARS FLYTTA RENSA-FÄLT TILL MOD-INDATA-FÄLT                   
028500     MOVE MID-KVTRANS-IN TO MOD-KVTRANS-IN                                
028510     MOVE MID-KVTRANS-UT TO MOD-KVTRANS-UT                                
028600     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVTRANS-IN-ATTR                    
029000                                                                          
029100     MOVE +1 TO RAD-IX                                                    
029200     PERFORM UNTIL RAD-IX > MAX-RAD-IX                                    
029300       IF MID-IDLOPNRM (RAD-IX) NOT = ALL '+'                             
029400         MOVE MID-IDLOPNRM (RAD-IX) TO MOD-IDLOPNRM (RAD-IX)              
029500         MOVE MFS-ADD-LAES-IN-FAELT TO                                    
029600                               MOD-IDLOPNRM-ATTR (RAD-IX)                 
029700       ELSE                                                               
029800         MOVE MFS-RENSA-FAELT TO MOD-IDLOPNRM (RAD-IX)                    
029900       END-IF                                                             
030000       IF MID-FLSVAR (RAD-IX) NOT = ALL '+'                               
030100         MOVE MID-FLSVAR (RAD-IX) TO MOD-FLSVAR (RAD-IX)                  
030200         MOVE MFS-ADD-LAES-IN-FAELT TO                                    
030300                               MOD-FLSVAR-ATTR (RAD-IX)                   
030400       ELSE                                                               
030500         MOVE MFS-RENSA-FAELT TO MOD-FLSVAR (RAD-IX)                      
030600       END-IF                                                             
030700       ADD +1 TO RAD-IX                                                   
030800     END-PERFORM                                                          
030900     .                                                                    
031000     EJECT                                                                
031010 F-VISA-ANT-REG SECTION.                                                  
031020                                                                          
031021     IF W-IDTRANS = '6146' OR '6147'                                      
031023        IF MID-KVTRANS-IN NUMERIC                                         
031026            COMPUTE W-KVTRANS = W-KVTRANS + W-KVTRANS-IN                  
031027        ELSE                                                              
031029            INSPECT MID-KVTRANS-UT                                        
031030                                 REPLACING LEADING SPACE BY ZERO          
031031            COMPUTE W-KVTRANS = W-KVTRANS + MID-KVTRANS-UT                
031032        END-IF                                                            
031033        MOVE W-KVTRANS        TO MOD-KVTRANS-UT                           
031034     ELSE                                                                 
031035       MOVE ZERO              TO MOD-KVTRANS-UT                           
031036     END-IF                                                               
031037     .                                                                    
031040     EJECT                                                                
031100 G-KOLLA-INPUT SECTION.                                                   
031200                                                                          
031300     MOVE JA  TO INDATA-SW                                                
032200     IF MID-INPUT = ALL '+'                                               
032600       PERFORM MFS-ROER-EJ-FAELT-IN                                       
032700       PERFORM MFS-ROER-EJ-FAELT-UT                                       
032800       MOVE NEJ TO INDATA-SW                                              
032900     ELSE                                                                 
032910       IF MID-KVTRANS-IN               NOT = ALL '+'                      
032920          IF MID-KVTRANS-IN NUMERIC                                       
032922              MOVE MFS-NUM-FAELT-RAETT TO MOD-KVTRANS-IN-ATTR             
032923              MOVE MID-KVTRANS-IN      TO W-KVTRANS-IN                    
032924          ELSE                                                            
032930             MOVE MFS-NUM-FAELT-FEL    TO MOD-KVTRANS-IN-ATTR             
032931             MOVE NEJ                  TO INDATA-SW                       
032932             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
032940          END-IF                                                          
032950       END-IF                                                             
033000       MOVE +1 TO RAD-IX                                                  
033100       PERFORM UNTIL RAD-IX > MAX-RAD-IX                                  
033110         MOVE JA TO RAD-SW                                                
033200         IF MID-IDLOPNRM (RAD-IX) NOT = ALL '+'                           
033300           IF MID-IDLOPNRM (RAD-IX) NOT NUMERIC                           
033400             MOVE ZERO TO W-TAB-IDLOPNRM (RAD-IX)                         
033500             MOVE MFS-NUM-FAELT-FEL TO                                    
033600                                MOD-IDLOPNRM-ATTR (RAD-IX)                
033700             MOVE NEJ TO RAD-SW                                           
033800           ELSE                                                           
033900             MOVE MID-IDLOPNRM (RAD-IX) TO W-IDLOPNRM                     
034000                                      W-TAB-IDLOPNRM (RAD-IX)             
034100             PERFORM GA-TEST-EJ-MED-PA-BILD-REDAN                         
034200             IF RAD-OK                                                    
034300               PERFORM GB-KOLLA-INLA                                      
034400               IF RAD-OK                                                  
034401                  PERFORM GC-PRIM-SEK-KOLL                                
034410                  IF RAD-OK                                               
034500                    MOVE MFS-NUM-FAELT-RAETT TO                           
034600                                      MOD-IDLOPNRM-ATTR (RAD-IX)          
034700                  ELSE                                                    
034800                    MOVE MFS-NUM-FAELT-FEL TO                             
034900                                     MOD-IDLOPNRM-ATTR (RAD-IX)           
035000                  END-IF                                                  
035100                ELSE                                                      
035200                  MOVE MFS-NUM-FAELT-FEL TO                               
035300                                   MOD-IDLOPNRM-ATTR (RAD-IX)             
035400                END-IF                                                    
035410             ELSE                                                         
035420               MOVE MFS-NUM-FAELT-FEL TO                                  
035430                                   MOD-IDLOPNRM-ATTR (RAD-IX)             
035440             END-IF                                                       
035500           END-IF                                                         
035600         ELSE                                                             
035700           MOVE ZERO TO W-TAB-IDLOPNRM (RAD-IX)                           
035800         END-IF                                                           
035900                                                                          
036000         IF MID-FLSVAR (RAD-IX) NOT = ALL '+'                             
036010           IF MID-IDLOPNRM (RAD-IX) NOT = ALL '+'                         
036100             IF MID-FLSVAR (RAD-IX) = 'J' OR 'Y'                          
036200               MOVE MID-FLSVAR (RAD-IX) TO                                
036300                                 W-TAB-FLSVAR (RAD-IX)                    
036310               MOVE MFS-ALFA-FAELT-RAETT TO                               
036320                                 MOD-FLSVAR-ATTR (RAD-IX)                 
036400             ELSE                                                         
036430               MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                  
036440               MOVE MFS-ALFA-FAELT-FEL TO                                 
036450                                 MOD-FLSVAR-ATTR (RAD-IX)                 
036460               MOVE NEJ TO RAD-SW                                         
036480                                                                          
036800             END-IF                                                       
036810           ELSE                                                           
036820             MOVE MFS-RENSA-FAELT TO MOD-FLSVAR (RAD-IX)                  
036821             MOVE NEJ TO INDATA-SW                                        
036830           END-IF                                                         
036900         END-IF                                                           
036910         IF RAD-FEL                                                       
036930           CALL WMEDKONV USING MED-WMEDAREA                               
036940           MOVE MED-MFSFEL TO MOD-FELMEDD( RAD-IX)                        
036941           MOVE JA TO NAGON-RAD-FELAKTIG                                  
036970         END-IF                                                           
037000         ADD +1 TO RAD-IX                                                 
037100       END-PERFORM                                                        
037110       IF NAGON-RAD-FEL                                                   
037200         PERFORM MFS-ROER-EJ-FAELT-UT                                     
037300         PERFORM MFS-ROER-EJ-FAELT-IN                                     
037400         MOVE NEJ TO INDATA-SW                                            
037500       END-IF                                                             
037900     END-IF                                                               
038000     .                                                                    
038100     EJECT                                                                
038200 GA-TEST-EJ-MED-PA-BILD-REDAN SECTION.                                    
038300                                                                          
038400     MOVE +1 TO W-TAB-IX                                                  
038500*    PERFORM UNTIL NOT (W-TAB-IX < RAD-IX)                                
038510     PERFORM UNTIL     (W-TAB-IX >= RAD-IX)                               
038600       IF W-IDLOPNRM = W-TAB-IDLOPNRM (W-TAB-IX)                          
038700         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
038800         MOVE MFS-NUM-FAELT-FEL TO MOD-IDLOPNRM-ATTR (RAD-IX)             
038900         MOVE NEJ TO RAD-SW                                               
039000         MOVE RAD-IX TO W-TAB-IX                                          
039100       END-IF                                                             
039200       ADD +1 TO W-TAB-IX                                                 
039300     END-PERFORM                                                          
039400     .                                                                    
039500     EJECT                                                                
039600 GB-KOLLA-INLA SECTION.                                                   
039700                                                                          
039800     PERFORM IMS-GU-INLA11                                                
039900     IF SEGMENT-FINNS                                                     
039910       MOVE INLA-ART-IDLOPNRM TO WS-ART-IDLOPNRM                          
040000       IF INLA-ART-FLKLAR = JA                                            
040100         MOVE ERR-MISSING TO MED-IDMFSFEL                                 
040200         MOVE NEJ TO RAD-SW                                               
040300       ELSE                                                               
040400         IF INLA-ART-FLKVAKAR = JA                                        
040500           MOVE ERR-QUALITY TO MED-IDMFSFEL                               
040600           MOVE NEJ TO RAD-SW                                             
040700         ELSE                                                             
040800           IF INLA-ART-FLKVAFEL = JA                                      
040900             MOVE ERR-QUALITY TO MED-IDMFSFEL                             
041000             MOVE NEJ TO RAD-SW                                           
041100           ELSE                                                           
041110             IF CDC-SE                                                    
041200               IF (INLA-ART-ADPLATS = ZERO) AND                           
041210                  (MID-FLSVAR (RAD-IX) = ALL '+') AND                     
041211                  (INLA-ART-ADTRDEST(1:2) NOT = 'CD')                     
041220                 MOVE MFS-OEPPNA-ALFA-FAELT TO                            
041230                                MOD-FLSVAR-ATTR (RAD-IX)                  
041300                 MOVE ERR-ADPLATS TO MED-IDMFSFEL                         
041400                 MOVE NEJ TO RAD-SW                                       
041500               ELSE                                                       
041510                 IF  INLA-ART-VKART = ZERO                                
041520                   MOVE NEJ    TO RAD-SW                                  
041540                   MOVE '792'  TO MED-IDMFSFEL                            
041550                 ELSE                                                     
041570                   IF INLA-ART-VLARTNTO = ZERO                            
041580                     MOVE NEJ    TO RAD-SW                                
041591                     MOVE '793'  TO MED-IDMFSFEL                          
041592                   ELSE                                                   
041594                     IF INLA-ART-KDARTURS = SPACE                         
041595                       MOVE NEJ    TO RAD-SW                              
041596                       MOVE '794'  TO MED-IDMFSFEL                        
041597                     ELSE                                                 
041600                       PERFORM GBA-KOLLA-INLA21                           
041601                     END-IF                                               
041610                   END-IF                                                 
041620                 END-IF                                                   
041700               END-IF                                                     
041710             END-IF                                                       
041800           END-IF                                                         
041900         END-IF                                                           
042000       END-IF                                                             
042100     ELSE                                                                 
042200       MOVE ERR-MISSING TO MED-IDMFSFEL                                   
042300       MOVE NEJ TO RAD-SW                                                 
042400     END-IF                                                               
042500     .                                                                    
042600     EJECT                                                                
042700 GBA-KOLLA-INLA21 SECTION.                                                
042800                                                                          
042900     PERFORM IMS-GNP-INLA-W6INLA21                                        
043000     IF SEGMENT-FINNS                                                     
043100       PERFORM IMS-GNP-INLA-W6INLA21                                      
043200* DET FINNS YTTERLIGARE EN RAD                                            
043300       IF SEGMENT-FINNS                                                   
043400         MOVE ERR-178 TO  MED-IDMFSFEL                                    
043500         MOVE NEJ TO RAD-SW                                               
043600       ELSE                                                               
043700         IF INLA-RAD-IDRADNR = +1                                         
043800           IF INLA-RAD-IDOKOLLI > ZERO                                    
043900             MOVE ERR-178 TO  MED-IDMFSFEL                                
044000             MOVE NEJ TO RAD-SW                                           
044100           ELSE                                                           
044200             IF INLA-RAD-KDINLSTA NOT = 'SAK' AND 'FPK' AND SPACE         
044300               MOVE ERR-007 TO MED-IDMFSFEL                               
044400               MOVE NEJ TO RAD-SW                                         
044500             END-IF                                                       
044600           END-IF                                                         
044700         ELSE                                                             
044800           MOVE ERR-178 TO  MED-IDMFSFEL                                  
044900           MOVE NEJ TO RAD-SW                                             
045000         END-IF                                                           
045100       END-IF                                                             
045200     ELSE                                                                 
045300       MOVE ERR-178 TO  MED-IDMFSFEL                                      
045400       MOVE NEJ TO RAD-SW                                                 
045500     END-IF                                                               
045600     .                                                                    
045700     EJECT                                                                
045702 GC-PRIM-SEK-KOLL SECTION.                                                
045703                                                                          
045704** KOLLAR OM KONTROLLERAD PÅ 6139                                         
045706     MOVE WS-ART-IDLOPNRM     TO W-IDLOPNRM-W6L1                          
045707     PERFORM IMS-GU-UPFA-01                                               
045708     IF SEGMENT-FINNS                                                     
045709       IF UPPF-KVKVAPRIM > 0                                              
045710** ARTIKEL UTTAGEN FÖR PRIMÄRKONTROLL                                     
045711          IF UPPF-KDKVASTA-PRI = '2' OR '3'                               
045712** PRIMÄRKONTROLL SATT SOM JA/NEJ. (OM NEJ HAR KR SKAPATS).               
045713             CONTINUE                                                     
045714          ELSE                                                            
045715             MOVE '215'     TO MED-IDMFSFEL                               
045716             MOVE NEJ       TO RAD-SW                                     
045717          END-IF                                                          
045718       END-IF                                                             
045719       IF UPPF-KVKVASEK > 0                                               
045720          IF UPPF-KDKVASTA-SEK = '2' OR '3'                               
045721             CONTINUE                                                     
045722          ELSE                                                            
045723             MOVE '215'     TO MED-IDMFSFEL                               
045724             MOVE NEJ       TO RAD-SW                                     
045725          END-IF                                                          
045726       END-IF                                                             
045727     END-IF                                                               
045728                                                                          
045729** KOLLAR ATT EVENTUELLT GAMLA KR BLIVIT BEDÖMDA                          
045730     IF INDATA-OK                                                         
045731       PERFORM IMS-GU-UPFA-01                                             
045732       IF SEGMENT-FINNS                                                   
045733         PERFORM IMS-GNP-UPFA11                                           
045734         PERFORM UNTIL SEGMENT-SAKNAS                                     
045735           IF RAPP-KDKVASTA-PRI = '2' OR '3'                              
045736             CONTINUE                                                     
045737           ELSE                                                           
045738             MOVE '215'     TO MED-IDMFSFEL                               
045739             MOVE NEJ       TO RAD-SW                                     
045740           END-IF                                                         
045741           PERFORM IMS-GNP-UPFA11                                         
045742         END-PERFORM                                                      
045743       END-IF                                                             
045744     END-IF                                                               
045745                                                                          
045746** KOLLAR ATT EVENTUELL SPECIALKONTROLL ÄR GJORD                          
045747     IF INDATA-OK                                                         
045748       PERFORM IMS-GU-UPFA-01                                             
045749       IF SEGMENT-FINNS                                                   
045750         PERFORM IMS-GNP-UPFA12                                           
045751         PERFORM UNTIL SEGMENT-SAKNAS                                     
045752           IF SPEC-KDKVASTA-PRI = '2' OR '3'                              
045753             CONTINUE                                                     
045754           ELSE                                                           
045755             MOVE '215'     TO MED-IDMFSFEL                               
045756             MOVE NEJ       TO RAD-SW                                     
045757           END-IF                                                         
045758           PERFORM IMS-GNP-UPFA12                                         
045759         END-PERFORM                                                      
045760       END-IF                                                             
045761     END-IF                                                               
045762     .                                                                    
045770     EJECT                                                                
045800 H-UPPDATERA SECTION.                                                     
045900                                                                          
046100     MOVE +1 TO RAD-IX                                                    
046200                W-IDRADNR                                                 
046210                6191-IX                                                   
046300     PERFORM UNTIL (RAD-IX > MAX-RAD-IX)                                  
046400       IF W-TAB-IDLOPNRM (RAD-IX) > ZERO AND                              
046500                    W-TAB-FLSVAR (RAD-IX) NOT = NEJ                       
046600          MOVE W-TAB-IDLOPNRM (RAD-IX) TO W-IDLOPNRM                      
046700          PERFORM IMS-GU-INLA11                                           
046800          MOVE INLA-ART-PRARTSTD TO W-PRARTSTD                            
046900          MOVE INLA-ART-KDINLPRIO TO W-KDINLPRIO                          
047000          PERFORM IMS-GU-INLA21-RAD1                                      
047010          MOVE INLA-RAD-ADINLOMR TO WS-ADINLOMR                           
047100          PERFORM HA-KOPIERA-RAD1-TILL-RAD2                               
047200          PERFORM IMS-ISRT-INLA-W6INLA21                                  
047400          MOVE +1 TO W-IDRADNR                                            
047500          PERFORM HC-SKAPA-6191-MID-BORT-RAD                              
047501          PERFORM HB-SKAPA-6191-MID-NY-RAD                                
047510          PERFORM IMS-GHU-W6D1-INLA21                                     
047600          PERFORM IMS-DLET-INLA                                           
047610          PERFORM S04-SKAPA-6193-MID                                      
047620          ADD +1 TO W-KVTRANS                                             
047700       END-IF                                                             
047900       ADD +1 TO RAD-IX                                                   
048000     END-PERFORM                                                          
048100                                                                          
050611     IF 6191-IX           > 1                                             
050612       PERFORM S03-STARTA-W6T191                                          
050613     END-IF                                                               
050620                                                                          
050700     MOVE INF-UPDATE-DONE TO MED-IDMFSFEL                                 
050800     CALL WMEDKONV USING MED-WMEDAREA                                     
050900     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
051000     PERFORM MFS-RENSA-FAELT-IN                                           
051200     PERFORM MFS-FORM-ATTRIBUT                                            
051300     .                                                                    
051400     EJECT                                                                
051500 HA-KOPIERA-RAD1-TILL-RAD2 SECTION.                                       
051600                                                                          
051700     MOVE INLA-RAD-W6D121  TO SPAR-RAD-W6D121                             
051800     MOVE +2               TO SPAR-RAD-IDRADNR                            
051900     MOVE 'INL'            TO SPAR-RAD-KDINLSTA                           
052000     MOVE SPACE            TO SPAR-RAD-ADINLOMR                           
052100                              SPAR-RAD-ADINLOMR-NXT                       
052200     MOVE ZERO             TO SPAR-RAD-IDINLVGN                           
052300                             SPAR-RAD-IDILIST                             
052400                             SPAR-RAD-IDILIRAD                            
052500     ACCEPT SPAR-RAD-TIUPPDAT FROM DATE                                   
052600     MOVE SPAR-RAD-W6D121  TO INLA-RAD-W6D121                             
052700     .                                                                    
052800     EJECT                                                                
052900 HB-SKAPA-6191-MID-NY-RAD   SECTION.                                      
053000                                                                          
053100     MOVE 'W6014600'           TO MOD6191-MID-IDPGM                       
053110     MOVE WS-IDDC              TO MOD6191-MID-IDDC                        
053200     MOVE W-IDLOPNRM           TO MOD6191-MID-IDLOPNRM (6191-IX)          
053300     MOVE INLA-RAD-IDRADNR     TO MOD6191-MID-IDRADNR  (6191-IX)          
053400     MOVE W-PRARTSTD           TO MOD6191-MID-PRARTSTD (6191-IX)          
053500     MOVE W-KDINLPRIO          TO MOD6191-MID-KDINLPRIO(6191-IX)          
053510     MOVE +0                   TO MOD6191-MID-KVKOLLI  (6191-IX)          
053520     MOVE 'N'                  TO MOD6191-MID-FLINLI   (6191-IX)          
053600                                                                          
053700     MOVE WS-ADINLOMR          TO MOD6191-MID-ADINLOMR-OLD                
053800                                                   (6191-IX)              
053900     MOVE SPACE                TO MOD6191-MID-ADINLOMR-NXT-OLD            
054000                                                   (6191-IX)              
054100                                  MOD6191-MID-KDINLSTA-OLD                
054200                                                   (6191-IX)              
054300     MOVE ZERO                 TO MOD6191-MID-KVINLART-OLD                
054400                                                   (6191-IX)              
054500                                                                          
054600     MOVE WS-ADINLOMR          TO MOD6191-MID-ADINLOMR-NEW                
054700                                                   (6191-IX)              
054800     MOVE INLA-RAD-ADINLOMR-NXT TO MOD6191-MID-ADINLOMR-NXT-NEW           
054900                                                   (6191-IX)              
055000     MOVE INLA-RAD-KDINLSTA    TO MOD6191-MID-KDINLSTA-NEW                
055100                                                   (6191-IX)              
055200     MOVE INLA-RAD-KVINLART    TO MOD6191-MID-KVINLART-NEW                
055300                                                   (6191-IX)              
055400     ADD +1                    TO 6191-IX                                 
055500     IF 6191-IX                > MAX-6191-IX                              
055600         PERFORM S03-STARTA-W6T191                                        
055700     END-IF                                                               
055800     .                                                                    
055900     EJECT                                                                
056000 HC-SKAPA-6191-MID-BORT-RAD   SECTION.                                    
056100                                                                          
056300     MOVE 'W6014600'           TO MOD6191-MID-IDPGM                       
056310     MOVE WS-IDDC              TO MOD6191-MID-IDDC                        
056400     MOVE W-IDLOPNRM           TO MOD6191-MID-IDLOPNRM (6191-IX)          
056500     MOVE W-IDRADNR            TO MOD6191-MID-IDRADNR  (6191-IX)          
056600     MOVE W-PRARTSTD           TO MOD6191-MID-PRARTSTD (6191-IX)          
056700     MOVE W-KDINLPRIO          TO MOD6191-MID-KDINLPRIO(6191-IX)          
056710     MOVE +0                   TO MOD6191-MID-KVKOLLI  (6191-IX)          
056720     MOVE 'J'                  TO MOD6191-MID-FLINLI   (6191-IX)          
056800                                                                          
056900     MOVE WS-ADINLOMR            TO MOD6191-MID-ADINLOMR-OLD              
057000                                                   (6191-IX)              
057100     MOVE SPAR-RAD-ADINLOMR-NXT  TO MOD6191-MID-ADINLOMR-NXT-OLD          
057200                                                   (6191-IX)              
057300     MOVE SPAR-RAD-KDINLSTA      TO MOD6191-MID-KDINLSTA-OLD              
057400                                                   (6191-IX)              
057500     MOVE SPAR-RAD-KVINLART      TO MOD6191-MID-KVINLART-OLD              
057600                                                   (6191-IX)              
057700                                                                          
057800     MOVE SPACE                TO MOD6191-MID-ADINLOMR-NEW                
057900                                                   (6191-IX)              
058000                                  MOD6191-MID-ADINLOMR-NXT-NEW            
058100                                                   (6191-IX)              
058200                                  MOD6191-MID-KDINLSTA-NEW                
058300                                                   (6191-IX)              
058400     MOVE ZERO                 TO MOD6191-MID-KVINLART-NEW                
058500                                                   (6191-IX)              
058600     ADD +1                    TO 6191-IX                                 
058700     IF 6191-IX                > MAX-6191-IX                              
058800         PERFORM S03-STARTA-W6T191                                        
058900     END-IF                                                               
059000     .                                                                    
059100     EJECT                                                                
059200 S03-STARTA-W6T191  SECTION.                                              
059300                                                                          
059400     COMPUTE MOD6191-MID-KVPOST = 6191-IX - 1                             
059500     COMPUTE P-TO-P-MSG-KVLL    =  LNG-P-TO-P-PREFIX +                    
059600                                  17 + (MOD6191-MID-KVPOST * 64)          
059700     MOVE 'W6T191X '           TO P-TO-P-MSG-KDTRANS                      
059800     MOVE '6146'               TO P-TO-P-MSG-IDTRANS                      
059900     MOVE MFS-KDMFSFOR         TO P-TO-P-MSG-KDMFSFOR                     
060000                                                                          
060100     MOVE MOD6191-MID-W6I19101 TO P-TO-P-MSG-INDATA                       
060200                                                                          
060300     IF FOERSTA-6191                                                      
060400         PERFORM IMS-ISRT-ALT1-MSG-6191                                   
060500         MOVE NEJ               TO FOERSTA-6191-SW                        
060600      ELSE                                                                
060700         PERFORM IMS-PURG-ALT1-MSG-6191                                   
060800     END-IF                                                               
060900     MOVE +1                   TO 6191-IX                                 
061000     .                                                                    
061100     EJECT                                                                
061200 S04-SKAPA-6193-MID SECTION.                                              
061300                                                                          
061400     MOVE SPACE             TO MSG-KOM-WMSGKOM                            
           COMPUTE MSG-KOM-KVLL = LENGTH OF MSG-KOM-WMSGKOM                     
061600     MOVE LOW-VALUE         TO MSG-KOM-KDZ1                               
061700     MOVE LOW-VALUE         TO MSG-KOM-KDZ2                               
061800     MOVE SPACE             TO MSG-KOM-KDTRANS                            
061900     MOVE 'W6I19301'        TO MSG-KOM-IDCPYTXT                           
062000     MOVE 'INLEV'           TO MSG-KOM-IDSNDNOD                           
062100     MOVE 'W6014600'        TO MSG-KOM-IDSNDJOB                           
062200     ACCEPT MSG-KOM-TIREGDAT FROM DATE                                    
062300     ACCEPT MSG-KOM-TIKLOCK FROM TIME                                     
062400     MOVE SPACE             TO MSG-KOM-IDMFSMED                           
062500                                                                          
062600     MOVE W-IDLOPNRM        TO MOD6193-MID-IDLOPNRM                       
062700     MOVE +2                TO MOD6193-MID-IDRADNR                        
062800                                                                          
062900     COMPUTE P-TO-P-MSG-KVLL = LNG-P-TO-P-PREFIX + 12                     
063000     MOVE 'W6T193X '          TO P-TO-P-MSG-KDTRANS                       
063100     MOVE '6147'              TO P-TO-P-MSG-IDTRANS                       
063200     MOVE MFS-KDMFSFOR        TO P-TO-P-MSG-KDMFSFOR                      
063300                                                                          
063400     MOVE MOD6193-MID-W6I19301 TO P-TO-P-MSG-INDATA                       
063500                                                                          
063600     CALL W006KOM USING MSG-PCB                                           
063700                        DISP-PCB                                          
063800                        KOMA-PCB                                          
063900                        MSG-KOM-WMSGKOM                                   
064000                        P-TO-P-MSG-IO-AREA-SNUF                           
064100     .                                                                    
064200     EJECT                                                                
064900 MFS-RENSA-FAELT-IN SECTION.                                              
065000                                                                          
065100*    --- ALLA INDATA-FÄLT                                                 
065200     MOVE MFS-RENSA-FAELT TO MOD-KVTRANS-IN                               
065300     MOVE +1 TO RAD-IX                                                    
065400     PERFORM UNTIL RAD-IX > MAX-RAD-IX                                    
065500       MOVE MFS-RENSA-FAELT TO MOD-IDLOPNRM (RAD-IX)                      
065600                               MOD-FLSVAR (RAD-IX)                        
065700       ADD +1 TO RAD-IX                                                   
065800     END-PERFORM                                                          
065900     .                                                                    
066000     EJECT                                                                
066100 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
066200                                                                          
066300*    --- ALLA UTDATA-FÄLT                                                 
066400     MOVE MFS-ROER-EJ-FAELT TO MOD-KVTRANS-UT                             
066500     .                                                                    
066600     SKIP2                                                                
066700 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
066800                                                                          
066900*    --- ALLA INDATA-FÄLT                                                 
067000     MOVE MFS-ROER-EJ-FAELT TO MOD-KVTRANS-IN                             
067100     MOVE +1 TO RAD-IX                                                    
067200     PERFORM UNTIL RAD-IX > MAX-RAD-IX                                    
067300       MOVE MFS-ROER-EJ-FAELT TO MOD-IDLOPNRM (RAD-IX)                    
067400                               MOD-FLSVAR (RAD-IX)                        
067500       ADD +1 TO RAD-IX                                                   
067600     END-PERFORM                                                          
067700     .                                                                    
067800     EJECT                                                                
067900 MFS-FORM-ATTRIBUT SECTION.                                               
068000                                                                          
068100*    --- ALLA INDATA-FÄLT                                                 
068200     MOVE MFS-FORMATETS-ATTR TO MOD-KVTRANS-IN-ATTR                       
068300     MOVE +1 TO RAD-IX                                                    
068400     PERFORM UNTIL RAD-IX > MAX-RAD-IX                                    
068500       MOVE MFS-FORMATETS-ATTR TO MOD-IDLOPNRM-ATTR (RAD-IX)              
068600                                  MOD-FLSVAR-ATTR (RAD-IX)                
068700       ADD +1 TO RAD-IX                                                   
068800     END-PERFORM                                                          
068900     .                                                                    
070200     EJECT                                                                
070210 MFS-ADD-LAES-IN   SECTION.                                               
070220                                                                          
070230*    --- ALLA INDATA-FÄLT                                                 
070240     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVTRANS-IN-ATTR                    
070250     MOVE +1 TO RAD-IX                                                    
070260     PERFORM UNTIL RAD-IX > MAX-RAD-IX                                    
070270       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLOPNRM-ATTR (RAD-IX)           
070280                                     MOD-FLSVAR-ATTR (RAD-IX)             
070290       ADD +1 TO RAD-IX                                                   
070291     END-PERFORM                                                          
070292     .                                                                    
070293     EJECT                                                                
070300* --- IMS SEKTIONER ---                                                   
070400     SKIP3                                                                
070500 IMS-GET-MSG SECTION.                                                     
070600                                                                          
070700     MOVE '  QC' TO GODK-STATUSKODER                                      
070800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
070900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
071000     PERFORM IMS-STATUSKONTROLL                                           
071100     .                                                                    
071200     SKIP3                                                                
071300 IMS-INSERT-MSG SECTION.                                                  
071400                                                                          
071410     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
071420       MOVE '0' TO MFS-KDHUVOMR                                           
071700     END-IF                                                               
071800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
071900     MOVE SPACE TO GODK-STATUSKODER                                       
072000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
072100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
072200     PERFORM IMS-STATUSKONTROLL                                           
072300     .                                                                    
072400     EJECT                                                                
072500 IMS-ISRT-ALT1-MSG-6191  SECTION.                                         
072600     MOVE SPACE TO GODK-STATUSKODER                                       
072700     CALL  CBLTDLI  USING ISRT ALT1-PCB P-TO-P-MSG-IO-AREA-SNUF           
072800     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
072900     PERFORM IMS-STATUSKONTROLL                                           
073000     .                                                                    
073100     SKIP3                                                                
073200 IMS-PURG-ALT1-MSG-6191  SECTION.                                         
073300     MOVE SPACE TO GODK-STATUSKODER                                       
073400     CALL  CBLTDLI  USING PURG ALT1-PCB P-TO-P-MSG-IO-AREA-SNUF           
073500     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
073600     PERFORM IMS-STATUSKONTROLL                                           
073700     .                                                                    
073800     EJECT                                                                
073900 IMS-GU-INLA11 SECTION.                                                   
074000                                                                          
074100     STRING 'W6INLA11(W6D1BSEQ =' W-W6D1BSEQ-X                            
074110                    '&IDDC     =' W-IDDC ')'                              
074200          DELIMITED BY SIZE INTO SSA1                                     
074300     MOVE '  GE' TO GODK-STATUSKODER                                      
074400     CALL CBLTDLI USING GU INLA-PCB DLI-IO-AREA SSA1                      
074500     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
074600     PERFORM IMS-STATUSKONTROLL                                           
074700     .                                                                    
074800     EJECT                                                                
075800 IMS-GNP-INLA-W6INLA21 SECTION.                                           
076100     MOVE 'W6INLA21' TO SSA1                                              
076300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
076400     CALL CBLTDLI USING GNP INLA-PCB DLI-IO-AREA SSA1                     
076500     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
076600     PERFORM IMS-STATUSKONTROLL                                           
076700     .                                                                    
076800     SKIP3                                                                
076900 IMS-GU-INLA21-RAD1 SECTION.                                              
077000                                                                          
077100     STRING 'W6INLA11(W6D1BSEQ =' W-W6D1BSEQ-X ')'                        
077200          DELIMITED BY SIZE INTO SSA1                                     
077300     STRING 'W6INLA21(IDRADNR  =' W-IDRADNR-X ')'                         
077400          DELIMITED BY SIZE INTO SSA2                                     
077500     MOVE '  GE' TO GODK-STATUSKODER                                      
077600     CALL CBLTDLI USING GU INLA-PCB DLI-IO-AREA SSA1 SSA2                 
077700     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
077800     PERFORM IMS-STATUSKONTROLL                                           
077900     .                                                                    
078000     SKIP3                                                                
078010 IMS-GHU-W6D1-INLA21 SECTION.                                             
078011                                                                          
078012     STRING 'W6INLA11(W6D1BSEQ =' W-W6D1BSEQ-X ')'                        
078013          DELIMITED BY SIZE INTO SSA1                                     
078014     STRING 'W6INLA21(IDRADNR  =' W-IDRADNR-X ')'                         
078015          DELIMITED BY SIZE INTO SSA2                                     
078016     MOVE '  GE' TO GODK-STATUSKODER                                      
078017     CALL CBLTDLI USING GHU INLA-PCB DLI-IO-AREA SSA1 SSA2                
078018     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
078019     PERFORM IMS-STATUSKONTROLL                                           
078020     .                                                                    
078030     SKIP3                                                                
078100 IMS-ISRT-INLA-W6INLA21 SECTION.                                          
078200                                                                          
078300     STRING 'W6INLA11(W6D1BSEQ =' W-W6D1BSEQ-X ')'                        
078400          DELIMITED BY SIZE INTO SSA1                                     
078500     MOVE 'W6INLA21 ' TO SSA2                                             
078600     MOVE '  II' TO GODK-STATUSKODER                                      
078700     CALL CBLTDLI USING ISRT INLA-PCB DLI-IO-AREA SSA1 SSA2               
078800     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
078900     PERFORM IMS-STATUSKONTROLL                                           
079000     .                                                                    
079100     SKIP3                                                                
080000 IMS-DLET-INLA SECTION.                                                   
080100                                                                          
080200     MOVE '  ' TO GODK-STATUSKODER                                        
080300     CALL CBLTDLI USING DLET INLA-PCB DLI-IO-AREA                         
080400     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
080500     PERFORM IMS-STATUSKONTROLL                                           
080600     .                                                                    
080710     SKIP3                                                                
080720 IMS-GU-UPFA-01  SECTION.                                                 
080730     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
080740          DELIMITED BY SIZE INTO SSA1                                     
080750     MOVE '  GE' TO GODK-STATUSKODER                                      
080760     CALL CBLTDLI USING GU UPFA-PCB DLI-IO-UPFA SSA1                      
080770     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
080780     PERFORM IMS-STATUSKONTROLL                                           
080790     .                                                                    
080800     SKIP3                                                                
080900 IMS-GNP-UPFA11 SECTION.                                                  
081000                                                                          
081100     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
081200          DELIMITED BY SIZE INTO SSA1                                     
081300     MOVE 'W6UPFA11 ' TO SSA2                                             
081400     MOVE '  GE' TO GODK-STATUSKODER                                      
081500     CALL CBLTDLI USING GNP UPFA-PCB DLI-IO-AREA-UPFA11 SSA1 SSA2         
081600     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
081700     PERFORM IMS-STATUSKONTROLL                                           
081800     .                                                                    
081900     SKIP3                                                                
082000 IMS-GNP-UPFA12 SECTION.                                                  
082100                                                                          
082200     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
082300          DELIMITED BY SIZE INTO SSA1                                     
082400     MOVE 'W6UPFA12 ' TO SSA2                                             
082500     MOVE '  GE' TO GODK-STATUSKODER                                      
082600     CALL CBLTDLI USING GNP UPFA-PCB DLI-IO-AREA-UPFA12 SSA1 SSA2         
082700     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
082800     PERFORM IMS-STATUSKONTROLL                                           
082900     .                                                                    
083000     SKIP3                                                                
083800 IMS-STATUSKONTROLL SECTION.                                              
083900                                                                          
084000     SET STATUS-IX TO 1                                                   
084100     SEARCH GODK-STATUS                                                   
084200       AT END                                                             
084300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
084400         DELIMITED BY SIZE INTO FELTEXT                                   
084500         CALL FELLOG                                                      
084600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
084700         CONTINUE                                                         
084800     END-SEARCH                                                           
084900     .                                                                    
