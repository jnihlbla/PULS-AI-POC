000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W4792000.                                                
000400*AUTHOR.         LARS CALAIS.                                             
000500*DATE-WRITTEN.   93/08/17.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        LÄSER WDR4 VID ALLA FÖREKOMSTER AV HÄNDELSETYP 4487 (ORDE        
001100*        RPLANNERING FÖR LAGRET), KONTROLLERAS OM DENNA PLOCKLISTA        
001200*        ÄR FÄRDIGPACKAD, ISÅFALL TAS SEGMENTET BORT                      
001300*                                                                         
001400*        PROGRAMMET LÄSER O UPPD   4487 (WDR4)                            
001500*        PROGRAMMET LÄSER        WDE601                                   
001600*                                                                         
001700*    ABENDKODER:                                                          
001800*                                                                         
001900*    CHANGE LOG                                                           
002000*                                                                         
002100*    CHANGE LOG                                                           
002200*                                                                         
002300*    DIGAMBAR/021011                                                      
002400*    STRUCTURE OF ACTION TRANSACTION 4487 IS CHANGED TO IMPROVE           
002500*    THE RESPONSE TIME OF THE SCREEN 4312. LOGIC TO RESORE THE            
002600*    DATABASE POSITION AFTER CHECKPOINT IS CORRECTED.                     
002700                                                                          
002800                                                                          
002900 ENVIRONMENT DIVISION.                                                    
003000                                                                          
003100 INPUT-OUTPUT SECTION.                                                    
003200                                                                          
003300 FILE-CONTROL.                                                            
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600                                                                          
003700 FILE SECTION.                                                            
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004100                                                                          
004200*    -- CHECKED BY WY2000                                                 
004300 77  IDPGM                       PIC X(8)    VALUE 'W4792000'.            
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004600*                                                                         
004700 01  CHKP-VAR.                                                            
004800 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
004900 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
005000 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
005100 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
005200 03  ANT-RENSADE                 PIC  9(5)   VALUE ZERO.                  
005300 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
005400 03  CHKP-MAX                    PIC S9(3)   VALUE +50.                   
005500     EJECT                                                                
005600 01  DYNAMISKA-SUBPROGRAM.                                                
005700*                                                                         
005800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006000     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
006100                                                                          
006200 01  FELTEXT.                                                             
006300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006500     EJECT                                                                
006600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
006700*                                                                         
006800     EJECT                                                                
006900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007000                                                                          
007100 01  NYCKLAR-TILL-DLI.                                                    
007200     03  W-IDPRODNR-X.                                                    
007300         05  W-IDPRODNR          PIC S9(7)   COMP-3.                      
007400*                                                                         
007500     03  W-WDGXKEY-4487-X.                                                
007600         05 W-4487-IDHTYP        PIC X(4)    VALUE '4487'.                
007700         05 W-4487-IDDC          PIC X(2).                                
007800         05 FILLER               PIC X(24)   VALUE LOW-VALUE.             
007900*                                                                         
008000     03  W-WDGXKEY-4488-X.                                                
008100         05 W-4488-KDPRCGRP      PIC X(5).                                
008200*                                                                         
008300     03  W-WDGXKEY-4490-X.                                                
008400         05  W-4490-DARFS        PIC 9(12).                               
008500         05  W-4490-IDPRODNR     PIC S9(7)   COMP-3.                      
008600         05  W-4490-IDPLKLST     PIC S9(3)   COMP-3.                      
008700*                                                                         
008800     EJECT                                                                
008900*    --- STATUS-KOD FRÅN IMS                                              
009000 01  STATUS-WS                   PIC XX.                                  
009100     88  SEGMENT-FINNS                       VALUE '  '.                  
009200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009300     88  SEGMENT-SLUT                        VALUE 'GB'.                  
009400     88  SEGMENT-EJ-OK                       VALUE 'XD'.                  
009500                                                                          
009600 01  GODK-STATUSKODER.                                                    
009700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009800                                                                          
009900 01  SSA1                        PIC X(64).                               
010000 01  SSA2                        PIC X(64).                               
010100     EJECT                                                                
010200                                                                          
010300 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
010400 01  FILLER REDEFINES TODAYS-DATE.                                        
010500     03  TODAYS-DATE-YYMMDD      PIC X(6).                                
010600                                                                          
010700 01  TODAYS-TIME                 PIC 9(8)    VALUE ZERO.                  
010800 01  FILLER REDEFINES TODAYS-TIME.                                        
010900     03  TODAYS-TIME-HHMM        PIC X(4).                                
011000     03  TODAYS-TIME-SSHD        PIC X(4).                                
011100     EJECT                                                                
011200                                                                          
011300 01  WS-TIAAMMDD-2               PIC 9(6).                                
011400 01  WS-SEKEL-2                  PIC 9(2).                                
011500     EJECT                                                                
011600*    --- PARAMETRAR TILL SUBPROGRAM WDAGKONV                              
011700*01  -COPY WDAGAREA                                                       
011800     EJECT                                                                
011900*    --- IMS FUNKTIONSKODER                                               
012000*01  -COPY W0003                                                          
012100     EJECT                                                                
012200*    ---  DLI INPUT-OUTPUT AREA1                                          
012300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA1'.        
012400                                                                          
012500 01  DLI-IO-AREA1.                                                        
012600     03  4487-88-IO-AREA1.                                                
012700                                                                          
012800         05  4487-IO-AREA1.                                               
012900*            07  -COPY WDGX4487                                           
013000                                                                          
013100         05  4488-IO-AREA1.                                               
013200*            07  -COPY WDGX4488                                           
013300                                                                          
013400     03  4490-IO-AREA1.                                                   
013500*        05  -COPY WDGX4490                                               
013600     EJECT                                                                
013700*    ---  DLI INPUT-OUTPUT AREA2                                          
013800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
013900                                                                          
014000 01  DLI-IO-AREA2.                                                        
014100     03  IO-AREA2                PIC X(300)  VALUE SPACE.                 
014200                                                                          
014300     03  WDE601 REDEFINES IO-AREA2.                                       
014400*        05  -COPY WDE601                                                 
014500     EJECT                                                                
014600 LINKAGE SECTION.                                                         
014700                                                                          
014800     EJECT                                                                
014900*01  -COPY W0009   -PRE MSG-                                              
015000     EJECT                                                                
015100*01  -COPY W0008  -PRE WDE6-                                              
015200     05  FILLER                  PIC X.                                   
015300     EJECT                                                                
015400*01  -COPY W0008  -PRE 4487-                                              
015500     05  FILLER                  PIC X.                                   
015600     EJECT                                                                
015700 PROCEDURE DIVISION  USING MSG-PCB WDE6-PCB 4487-PCB.                     
015800     ENTRY 'DLITCBL' USING MSG-PCB WDE6-PCB 4487-PCB.                     
015900                                                                          
016000     PERFORM A-INIT                                                       
016100     PERFORM IMS-GN-ROT-4488                                              
016200     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
016300       IF  4487-IDHTYP = '4487'                                           
016400          MOVE 4487-IDDC     TO W-4487-IDDC                               
016500          MOVE 4488-KDPRCGRP TO W-4488-KDPRCGRP                           
016600          PERFORM B-KOLLA-ORDER                                           
016700       END-IF                                                             
016800       PERFORM IMS-GN-ROT-4488                                            
016900     END-PERFORM                                                          
017000     DISPLAY 'ANTAL RENSADE 4490:OR ' ANT-RENSADE                         
017100                                                                          
017200     MOVE ZERO TO RETURN-CODE                                             
017300     GOBACK                                                               
017400     .                                                                    
017500     EJECT                                                                
017600 A-INIT SECTION.                                                          
017700                                                                          
017800     PERFORM IMS-RESTART                                                  
017900                                                                          
018000     ACCEPT TODAYS-DATE      FROM DATE                                    
018100     ACCEPT TODAYS-TIME      FROM TIME                                    
018200                                                                          
018300     MOVE 3                  TO DAG-KDCALL                                
018400     MOVE TODAYS-DATE        TO DAG-TIAAMMDD-TOM                          
018500     MOVE +2                 TO DAG-KVKALDAG                              
018600     IF TODAYS-DATE  > 500000                                             
018700        MOVE 19              TO DAG-TISEKEL-TOM                           
018800     ELSE                                                                 
018900        MOVE 20              TO DAG-TISEKEL-TOM                           
019000     END-IF                                                               
019100                                                                          
019200     CALL WDAGKONV USING DAG-KDCALL                                       
019300               DAG-DATUM-AREA DAG-KDSVAR                                  
019400                                                                          
019500     IF DAG-KDSVAR = SPACE                                                
019600       MOVE DAG-TIAAMMDD-FOM TO  WS-TIAAMMDD-2                            
019700       MOVE DAG-TISEKEL-FOM  TO  WS-SEKEL-2                               
019800     END-IF                                                               
019900                                                                          
019910     DISPLAY 'DAG-TIAAMMDD-TOM--> ' DAG-TIAAMMDD-TOM                      
019920     DISPLAY 'DAG-TIAAMMDD-FOM--> ' DAG-TIAAMMDD-FOM                      
020000     .                                                                    
020100     EJECT                                                                
020200 B-KOLLA-ORDER SECTION.                                                   
020300                                                                          
020400     PERFORM IMS-GHNP-WDGX4490                                            
020500     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
020600                   SEGMENT-SLUT                                           
020700        IF CHKP-ANT > CHKP-MAX                                            
020800          MOVE 4490-DARFS    TO W-4490-DARFS                              
020900          MOVE 4490-IDPRODNR TO W-4490-IDPRODNR                           
021000          MOVE 4490-IDPLKLST TO W-4490-IDPLKLST                           
021100          PERFORM X-TAG-CHECKPOINT                                        
021200        END-IF                                                            
021300        IF 4490-TIKLAR > 0                                                
021400****       AND 4490-TIKLAR < WS-TIAAMMDD-2                                
021500           PERFORM IMS-DLET-WDGX4490                                      
021600           ADD +1            TO CHKP-ANT                                  
021700                                ANT-RENSADE                               
021800        ELSE                                                              
021900          MOVE 4490-IDPRODNR   TO W-IDPRODNR                              
022000          PERFORM IMS-GET-WDE601                                          
022100          IF SEGMENT-FINNS                                                
022200             IF VORD-KDORDSTA > +2 AND                                    
022300                VORD-TIPACKN-SK < WS-TIAAMMDD-2                           
022400                PERFORM IMS-DLET-WDGX4490                                 
022500                ADD +1    TO CHKP-ANT                                     
022600                             ANT-RENSADE                                  
022700             END-IF                                                       
022800          ELSE                                                            
022900             PERFORM IMS-DLET-WDGX4490                                    
023000             ADD +1    TO CHKP-ANT                                        
023100                          ANT-RENSADE                                     
023200          END-IF                                                          
023300        END-IF                                                            
023400        PERFORM IMS-GHNP-WDGX4490                                         
023500     END-PERFORM                                                          
023600     .                                                                    
023700     EJECT                                                                
023800 X-TAG-CHECKPOINT   SECTION.                                              
023900                                                                          
024000     PERFORM IMS-CHECKPOINT                                               
024100     MOVE ZERO TO CHKP-ANT                                                
024200     PERFORM IMS-GU-4487-88                                               
024300     PERFORM IMS-GHNP-WDGX4490-UNIK                                       
024400* --- LÄS OM DATABAS OM DET BEHÖVS                                        
024500     .                                                                    
024600     EJECT                                                                
024700* --- IMS SEKTIONER ---                                                   
024800                                                                          
024900     EJECT                                                                
025000 IMS-GU-4487-88  SECTION.                                                 
025100                                                                          
025200     STRING 'WDR401  *D(WDGXKEY  =' W-WDGXKEY-4487-X ')'                  
025300         DELIMITED BY SIZE INTO SSA1                                      
025400     STRING 'WDGX4488(KDPRCGRP =' W-WDGXKEY-4488-X ')'                    
025500         DELIMITED BY SIZE INTO SSA2                                      
025600     CALL CBLTDLI USING GU 4487-PCB 4487-88-IO-AREA1 SSA1 SSA2            
025700     MOVE 4487-STATUS-CODE TO STATUS-WS                                   
025800     MOVE '  GE' TO GODK-STATUSKODER                                      
025900     PERFORM IMS-STATUSKONTROLL                                           
026000     .                                                                    
026100                                                                          
026200 IMS-GHNP-WDGX4490-UNIK   SECTION.                                        
026300                                                                          
026400     STRING 'WDGX4490(KY4490   =' W-WDGXKEY-4490-X ')'                    
026500         DELIMITED BY SIZE INTO SSA1                                      
026600     CALL CBLTDLI USING GHNP 4487-PCB 4490-IO-AREA1 SSA1                  
026700     MOVE 4487-STATUS-CODE TO STATUS-WS                                   
026800     MOVE '  GE' TO GODK-STATUSKODER                                      
026900     PERFORM IMS-STATUSKONTROLL                                           
027000     .                                                                    
027100                                                                          
027200 IMS-GN-ROT-4488    SECTION.                                              
027300                                                                          
027400     MOVE 'WDR401  *D'       TO SSA1                                      
027500     MOVE 'WDGX4488'         TO SSA2                                      
027600     CALL CBLTDLI USING GN 4487-PCB 4487-88-IO-AREA1 SSA1 SSA2            
027700     MOVE 4487-STATUS-CODE TO STATUS-WS                                   
027800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
027900     PERFORM IMS-STATUSKONTROLL                                           
028000     .                                                                    
028100                                                                          
028200 IMS-GHNP-WDGX4490  SECTION.                                              
028300                                                                          
028400     MOVE 'WDGX4490'           TO SSA1                                    
028500     CALL CBLTDLI USING GHNP 4487-PCB 4490-IO-AREA1 SSA1                  
028600     MOVE 4487-STATUS-CODE TO STATUS-WS                                   
028700     MOVE '  GEGB' TO GODK-STATUSKODER                                    
028800     PERFORM IMS-STATUSKONTROLL                                           
028900     .                                                                    
029000                                                                          
029100 IMS-DLET-WDGX4490  SECTION.                                              
029200                                                                          
029300     CALL CBLTDLI USING DLET 4487-PCB 4490-IO-AREA1                       
029400     MOVE 4487-STATUS-CODE TO STATUS-WS                                   
029500     MOVE '  ' TO GODK-STATUSKODER                                        
029600     PERFORM IMS-STATUSKONTROLL                                           
029700     .                                                                    
029800     EJECT                                                                
029900 IMS-GET-WDE601     SECTION.                                              
030000                                                                          
030100     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
030200          DELIMITED BY SIZE     INTO SSA1                                 
030300     MOVE '  GE'                  TO GODK-STATUSKODER                     
030400     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-AREA2 SSA1                     
030500     MOVE WDE6-STATUS-CODE        TO STATUS-WS                            
030600     PERFORM IMS-STATUSKONTROLL                                           
030700     .                                                                    
030800     EJECT                                                                
030900 IMS-RESTART SECTION.                                                     
031000     SKIP2                                                                
031100     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
031200     MOVE '  ' TO GODK-STATUSKODER                                        
031300     CALL CBLTDLI USING XRST MSG-PCB                                      
031400                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
031500                        CHKP-AREA-LENGTH CHKP-AREA                        
031600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031700     PERFORM IMS-STATUSKONTROLL                                           
031800     .                                                                    
031900     EJECT                                                                
032000 IMS-CHECKPOINT SECTION.                                                  
032100     SKIP2                                                                
032200     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
032300     MOVE '  XD' TO GODK-STATUSKODER                                      
032400     CALL CBLTDLI USING CHKP MSG-PCB                                      
032500                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
032600                        CHKP-AREA-LENGTH CHKP-AREA                        
032700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
032800     PERFORM IMS-STATUSKONTROLL                                           
032900                                                                          
033000     IF SEGMENT-EJ-OK                                                     
033100       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
033200       DISPLAY FELTEXT                                                    
033300       CALL FELLOG                                                        
033400     END-IF                                                               
033500     .                                                                    
033600     EJECT                                                                
033700 IMS-STATUSKONTROLL SECTION.                                              
033800                                                                          
033900     SET STATUS-IX TO 1                                                   
034000     SEARCH GODK-STATUS                                                   
034100       AT END                                                             
034200         MOVE 'FEL STATUSKOD ' TO FELTEXT-STR                             
034300         DISPLAY FELTEXT                                                  
034400         DISPLAY STATUS-WS                                                
034500         CALL FELLOG                                                      
034600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
034700         CONTINUE                                                         
034800     END-SEARCH                                                           
034900     .                                                                    
