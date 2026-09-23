000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6013700.                                                
000400*AUTHOR.         LARS THELL.                                              
000500*DATE-WRITTEN.   92/05/15.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        LÄGGER UPP PARTIER ELLER DELAR AV PARTIER I ETT                  
001100*        DIVERSEKOLLI, SAMT TAR UT FLAGGOR FÖR DIVERSEKOLLI.              
001200*                                                                         
001300*        PROGRAMMET          UPPDATERAR W6INLA (W6D1)                     
001400*        PROGRAMMET          UPPDATERAR W6LOPA (W6G1)                     
001500*        PROGRAMMET          LÄSER      W6PLAA (W6G1)                     
001510*        PROGRAMMET          LÄSER              WDB6                      
001600*    SUB PROGRAMMET W611PMRK UPPDATERAR W6INLA (W6D1)                     
001700*        PROGRAMMET          LÄSER      W6PLAA (W6G1)                     
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSAKTION: W6T137                                              
002100*        MID:         W6I13701                                            
002200*                                                                         
002300*    UTDATA.                                                              
002400*        MOD:         W6O13701                                            
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000 WORKING-STORAGE SECTION.                                                 
003001                                                                          
003010*    -- CHECKED BY WY2000                                                 
003100 77  IDPGM                       PIC X(08)   VALUE 'W6013700'.            
003200                                                                          
003300*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003400 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003500                                                                          
003600 77  JA                          PIC X       VALUE 'J'.                   
003610 77  YES                         PIC X       VALUE 'Y'.                   
003700 77  NEJ                         PIC X       VALUE 'N'.                   
003800                                                                          
003900 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004000 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +371  COMP SYNC.        
004100 77  INDX                        PIC S9(9)  VALUE +0    COMP SYNC.        
004200 77  6191-IX                     PIC S9(9)  VALUE +0    COMP SYNC.        
004300 77  MAX-RAD                     PIC S9(4)  VALUE +11   COMP SYNC.        
004400 77  W-KOLLI-ADINLOMR            PIC  X(4)  VALUE SPACE.                  
004500 77  W-KOLLI-ADINLOMR-NXT        PIC  X(4)  VALUE SPACE.                  
004600 77  W-KOLLI-IDINLVGN            PIC  9(3)  VALUE ZERO.                   
004700 77  W-KOLLI-FLPRIO              PIC  X(1)  VALUE ZERO.                   
004800 77  W-SPAR-ADINLOMR             PIC  X(4)  VALUE SPACE.                  
004900 77  W-SPAR-ADINLOMR-NXT         PIC  X(4)  VALUE SPACE.                  
005000 77  W-SPAR-KDINLSTA             PIC  X(3)  VALUE SPACE.                  
005100 77  W-SPAR-IDLEVNR-KOLLI        PIC  X(5)  VALUE SPACE.                  
005200 77  LNG-P-TO-P-PREFIX           PIC S9(4)  VALUE +17   COMP SYNC.        
005210 77  SPAR-ADINLOMR               PIC  X(4)  VALUE SPACE.                  
005220 77  SPAR-FLPRIO                 PIC  X(1)  VALUE SPACE.                  
005230 77  SPAR-FLDIVKLI               PIC  X(1)  VALUE SPACE.                  
005240 77  SPAR-ADLAGOMR               PIC  9(2)  VALUE ZERO.                   
005300                                                                          
005400*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005500 77  WS-IDLEVNR-KOLLI            PIC X(5)    VALUE SPACE.                 
005600 77  WS-IDOKOLLI                 PIC X(9)    VALUE SPACE.                 
005620 77  WS-IDOKOLLI-NUM             PIC 9(9)    VALUE ZERO.                  
005700*    --- HOPPNYCKLAR SOM EJ SYNS PÅ SKÄRMEN                               
005800 77  WS-IDINLVGN                 PIC X(3)    VALUE SPACE.                 
005900 77  WS-ADINLOMR                 PIC X(4)    VALUE SPACE.                 
006000 77  WS-ADINLOMR-NXT             PIC X(4)    VALUE SPACE.                 
006100 77  WS-KDINLQ                   PIC X(1)    VALUE SPACE.                 
006200 77  WS-BEFT-FOM                 PIC X(2)    VALUE SPACE.                 
006210 77  WS-BEFT-TOM                 PIC X(2)    VALUE SPACE.                 
006300 77  WS-FLINLFB                  PIC X(1)    VALUE SPACE.                 
006400 77  WS-IDLOPNRM                 PIC X(8)    VALUE SPACE.                 
006500*                                                                         
006600 77  W-HELP-ADINLOMR             PIC X(4)    VALUE SPACE.                 
006700                                                                          
006800 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006900     88  INDATA-OK                           VALUE 'J'.                   
007000     88  INDATA-FEL                          VALUE 'N'.                   
007100                                                                          
007200 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007300     88  NYCKLAR-OK                          VALUE 'J'.                   
007400     88  NYCKLAR-FEL                         VALUE 'N'.                   
007500                                                                          
007600 77  RAD-SW                      PIC X       VALUE 'J'.                   
007700     88  RAD-FEL                             VALUE 'N'.                   
007800                                                                          
007810 77  GAMMAL-SW                   PIC X       VALUE 'J'.                   
007820     88  GAMMAL-NYCKEL                       VALUE 'J'.                   
007830                                                                          
007831 77  KOLLI-SW                    PIC X       VALUE 'N'.                   
007832     88  KOLLI-FINNS                         VALUE 'J'.                   
007833     88  KOLLI-SAKNAS                        VALUE 'N'.                   
007834                                                                          
007840 77  STATUS-SW                   PIC X       VALUE 'N'.                   
007850     88  STATUS-OK                           VALUE 'J'.                   
007860                                                                          
007870 77  AENDRA-FLPRIO-SW            PIC X       VALUE 'N'.                   
007880     88  AENDRA-FLPRIO                       VALUE 'J'.                   
007890                                                                          
007891 77  PRIM-CONTROL-SW             PIC X       VALUE 'N'.                   
007892     88  PRIM-CONTROL-YES                    VALUE 'J'.                   
007893     88  PRIM-CONTROL-NO                     VALUE 'N'.                   
007894                                                                          
007900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008000     88  EGEN-MID                            VALUE '6137'.                
008100     88  GODK-MID                            VALUE '6131' '6132'          
008200                                                   '6133' '6134'          
008300                                                   '6135' '6136'          
008400                                                   '6137' '6138'.         
008500     88  HELP-MID                            VALUE '0551'.                
008600     EJECT                                                                
008700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008800 01  GENERELLA-SUBPROGRAM.                                                
008900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
009000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009200     03  W611PMRK                PIC X(8)    VALUE 'W611PMRK'.            
009300     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
009310     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009400     EJECT                                                                
009410*01 -COPY WMSGINIT                                                        
009420     SKIP3                                                                
009500*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
009600*01 -COPY WMEDAREA                                                        
009700     SKIP3                                                                
009800 01  MESSAGE-CODES.                                                       
009900     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
010000     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
010100     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
010110     03  INF-PRESS-PF4           PIC X(3)    VALUE '081'.                 
010200     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
010300     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010400     EJECT                                                                
010500*01  -COPY W611PMRK                                                       
010600     EJECT                                                                
010700*01  -COPY W006PRT                                                        
010800     EJECT                                                                
010900*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
011000*                                                                         
011100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
011200     SKIP3                                                                
011300*01  MID -COPY W6I13701                                                   
011400     EJECT                                                                
011500 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
011600     SKIP3                                                                
011700*01  -COPY WMSGAREA                                                       
011800     EJECT                                                                
011900     03  MOD REDEFINES MSG-AREA.                                          
012000*      05  -COPY W6O13701                                                 
012100     EJECT                                                                
012200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
012300     SKIP3                                                                
012400*01  -COPY WMFSAREA                                                       
012500     EJECT                                                                
012600 01      P-TO-P-SW.                                                       
012700                                                                          
012800  02     P-TO-P-KVLL             PIC S9(4)           COMP SYNC.           
012900  02     P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.               
013000  02     P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.               
013100  02     P-TO-P-KDTRANS          PIC X(8).                                
013200  02     P-TO-P-IDTRANS          PIC X(4).                                
013300  02     P-TO-P-KDMFSFOR         PIC X(1).                                
013400  02     P-TO-P-DATA             PIC X(1500).                             
013500     EJECT                                                                
013600 01      FILLER                  PIC X(24)   VALUE                        
013700                                 'MOD6191-MID-W6I19101'.                  
013800     SKIP2                                                                
013900     -COPY W6I19101 -PRE MOD6191-                                         
014000     EJECT                                                                
014100 01      FILLER                  PIC X(24)   VALUE                        
014200                                 'MOD6194-MID-W6I19401'.                  
014300     SKIP2                                                                
014400     -COPY W6I19401 -PRE MOD6194-                                         
014500     EJECT                                                                
014600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014700*                                                                         
014800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014900     SKIP3                                                                
015000 01  NYCKLAR-TILL-DLI.                                                    
015100     03  W-W6D1BSEQ-X.                                                    
015200         05  W-D1BSEQ-IDLOPNRM      PIC S9(9) COMP-3 VALUE ZERO.          
015300                                                                          
015400     03  W-W6D1CSEQ-X.                                                    
015500         05  W-D1CSEQ-IDLEVNR-KOLLI PIC  X(5) VALUE SPACE.                
015600         05  W-D1CSEQ-IDOKOLLI      PIC  9(9) VALUE ZERO.                 
015700                                                                          
015800     03  W-IDLEVNR-KOLLI-X.                                               
015900         05  W-IDLEVNR-KOLLI        PIC  X(5) VALUE SPACE.                
016000                                                                          
016100     03  W-IDDC-X.                                                        
016200         05  W-IDDC                 PIC  X(2) VALUE ZERO.                 
016300                                                                          
016310     03  W-IDOKOLLI-X.                                                    
016320         05  W-IDOKOLLI             PIC  9(9) VALUE ZERO.                 
016330                                                                          
016400     03  W-IDRADNR-X.                                                     
016500         05  W-IDRADNR              PIC S9(5) COMP-3 VALUE ZERO.          
016600                                                                          
016610     03  W1-IDLOPNRM-X.                                                   
016620         05  W1-IDLOPNRM          PIC S9(9)   COMP-3.                     
016630                                                                          
016700     03  W-W6GXKEY-6005-X.                                                
016800         05  W-6005-IDHTYP       PIC X(4)    VALUE '6005'.                
016810         05  W-6005-IDDC         PIC X(2)    VALUE SPACE.                 
016900         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
017000                                                                          
017100     03  W-W6GXKEY-6006-X.                                                
017200         05  W-6006-ADINLOMR     PIC X(4)    VALUE SPACE.                 
017300         05  FILLER              PIC X(1)    VALUE LOW-VALUE.             
017400                                                                          
017500     03  W-W6GXKEY-6017-X.                                                
017600         05  W-6017-IDHTYP       PIC X(4)    VALUE '6017'.                
017700         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
017800                                                                          
017900     03  W-W6GXKEY-6018-X.                                                
018000         05  W-6018-KDSEGKEY     PIC X(1)    VALUE '1'.                   
018010                                                                          
018020     03  W-IDDC-B6-X.                                                     
018030         05 W-IDDC-B6            PIC X(2).                                
018040                                                                          
018100     EJECT                                                                
018200*    --- STATUS-KOD FRÅN IMS                                              
018300 01  STATUS-WS                   PIC XX.                                  
018400     88  SEGMENT-FINNS                       VALUE '  '.                  
018500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
018600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
018700     SKIP2                                                                
018800 01  GODK-STATUSKODER.                                                    
018900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019000     SKIP3                                                                
019100 01  SSA1                        PIC X(64).                               
019200 01  SSA2                        PIC X(64).                               
019300     EJECT                                                                
019400*    --- IMS FUNKTIONSKODER                                               
019500*01  -COPY W0003                                                          
019600     EJECT                                                                
019700*    ---  DLI INPUT-OUTPUT AREA                                           
019800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA1'.        
019900     SKIP3                                                                
020000 01  DLI-IO-AREA1.                                                        
020100     03  IO-AREA1                PIC X(150)  VALUE SPACE.                 
020200     SKIP3                                                                
020300     03  W6INLA11 REDEFINES IO-AREA1.                                     
020400*        05  -COPY W6D111                                                 
020500     EJECT                                                                
020600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
020700     SKIP3                                                                
020800 01  DLI-IO-AREA2.                                                        
020900     03  IO-AREA2                PIC X(150)  VALUE SPACE.                 
021000     SKIP3                                                                
021100     03  W6INLA21 REDEFINES IO-AREA2.                                     
021200*        05  -COPY W6D121                                                 
021300     EJECT                                                                
021400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA3'.        
021500 01  DLI-IO-AREA3.                                                        
021600     03  IO-AREA3                PIC X(150)  VALUE SPACE.                 
021700     SKIP3                                                                
021800     03  W6PLAA11 REDEFINES IO-AREA3.                                     
021900*        05  -COPY W6GX6006 -PRE PLAA-                                    
022000     SKIP3                                                                
022100     03  W6LOPA11 REDEFINES IO-AREA3.                                     
022200*        05  -COPY W6GX6018 -PRE LOPA-                                    
022300     EJECT                                                                
022301                                                                          
022310 01  DLI-IO-AREA-UPFA01.                                                  
022320     03  W6UPFA01.                                                        
022330*        05  -COPY W6L101                                                 
022340     SKIP3                                                                
022350 01  DLI-IO-AREA-UPFA11.                                                  
022360     03  W6UPFA11.                                                        
022370*        05  -COPY W6L111                                                 
022380     SKIP3                                                                
022390 01  DLI-IO-AREA-UPFA12.                                                  
022391     03  W6UPFA12.                                                        
022392*        05  -COPY W6L112                                                 
022393     SKIP3                                                                
022394 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
022395 01   DLI-IO-AREA-B601.                                                   
022396*     03  -COPY WDB601                                                    
022397     SKIP3                                                                
022398                                                                          
022400 LINKAGE SECTION.                                                         
022500                                                                          
022600*01  -COPY W0009   -PRE MSG-                                              
022700     EJECT                                                                
022800*01  -COPY W0009   -PRE ALT1-                                             
022900     EJECT                                                                
023000*01  -COPY W0009   -PRE ALT2-                                             
023100     EJECT                                                                
023200*01  -COPY W0008  -PRE USEA-                                              
023300     05  FILLER                  PIC X.                                   
023400     EJECT                                                                
023410*01  -COPY W0008  -PRE INLA1-                                             
023420     05  FILLER                  PIC X.                                   
023430     EJECT                                                                
023500*01  -COPY W0008  -PRE INLA2-                                             
023600     05  FILLER                  PIC X.                                   
023700     EJECT                                                                
023800*01  -COPY W0008  -PRE PLAA-                                              
023900     05  FILLER                  PIC X.                                   
024000     EJECT                                                                
024100*01  -COPY W0008  -PRE LOPA-                                              
024200     05  FILLER                  PIC X.                                   
024300     EJECT                                                                
024310*01  -COPY W0008  -PRE UPFA-                                              
024320     05  FILLER                  PIC X.                                   
024330     EJECT                                                                
024340*01  -COPY W0008  -PRE WDB6-                                              
024350     05  FILLER                  PIC X.                                   
024360     EJECT                                                                
024400**  PCB'ER FÖR SUBPGM                                                     
024500 01  PMRK-INLB-PCB               PIC X.                                   
024600                                                                          
024700 01  PMRK-INLC-PCB               PIC X.                                   
024800                                                                          
024900 01  PMRK-PLAA-PCB               PIC X.                                   
025000 EJECT                                                                    
025100 PROCEDURE DIVISION  USING MSG-PCB ALT1-PCB  ALT2-PCB USEA-PCB            
025200                                   INLA1-PCB INLA2-PCB                    
025300                                   PLAA-PCB  LOPA-PCB UPFA-PCB            
025310                                   WDB6-PCB                               
025400                                   PMRK-INLB-PCB PMRK-INLC-PCB            
025500                                   PMRK-PLAA-PCB.                         
025600     ENTRY 'DLITCBL' USING MSG-PCB ALT1-PCB  ALT2-PCB USEA-PCB            
025700                                   INLA1-PCB INLA2-PCB                    
025800                                   PLAA-PCB  LOPA-PCB UPFA-PCB            
025810                                   WDB6-PCB                               
025900                                   PMRK-INLB-PCB PMRK-INLC-PCB            
026000                                   PMRK-PLAA-PCB.                         
026100                                                                          
026200     PERFORM IMS-GET-MSG                                                  
026300     IF SEGMENT-FINNS                                                     
026400       PERFORM A-INIT                                                     
026500       PERFORM B-KOLLA-NYCKLAR                                            
026600       IF NYCKLAR-OK                                                      
026700         IF MFS-UPDATE  OR MFS-PRINT                                      
026800           PERFORM G-KOLLA-INPUT                                          
026900           IF INDATA-OK                                                   
027000             PERFORM H-UPPDATERA                                          
027100           END-IF                                                         
027200         ELSE                                                             
027300           IF MFS-FIRST                                                   
027400             CONTINUE                                                     
027500            ELSE                                                          
027600             PERFORM E-SAMMA-SIDA                                         
027700           END-IF                                                         
027800         END-IF                                                           
027900         IF INDATA-OK                                                     
028000             IF MFS-PRINT                                                 
028100                 IF HELP-MID                                              
028200                     CONTINUE                                             
028300                  ELSE                                                    
028400                     MOVE MFS-ADD-LAES-IN-FAELT TO                        
028500                                            MOD-ADINLOMR-PRT-ATTR         
028600*                    MOVE MFS-ROER-EJ-FAELT TO MOD-ADINLOMR-PRT           
028700                 END-IF                                                   
028800              ELSE                                                        
028900                 PERFORM F-LAES-VISA-INFO                                 
029000             END-IF                                                       
029100         END-IF                                                           
029200       END-IF                                                             
029300       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
029400       PERFORM IMS-INSERT-MSG                                             
029500     END-IF                                                               
029600                                                                          
029700     MOVE ZERO TO RETURN-CODE                                             
029800     GOBACK                                                               
029900     .                                                                    
030000     EJECT                                                                
030100 A-INIT SECTION.                                                          
030200                                                                          
030300     IF MSG-DUBBLA-TRANSKODER                                             
030400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I13701                 
030500       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
030600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
030700     ELSE                                                                 
030800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I13701                  
030900       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
031000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
031100     END-IF                                                               
031200                                                                          
031300     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
031400     MOVE MSG-IDPFK TO MFS-IDPFK                                          
031500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
031600                                                                          
031700     MOVE LOW-VALUE TO MSG-AREA                                           
031800     MOVE 'W6O137N1' TO MFS-IDMOD                                         
031900     MOVE '6137' TO MOD-IDTRANS                                           
032000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
032100                                                                          
032200     IF EGEN-MID OR HELP-MID                                              
032300       CONTINUE                                                           
032400      ELSE                                                                
032500       MOVE SPACE TO MFS-KDTRTYP                                          
032600       MOVE '7' TO MFS-IDPFK                                              
032700     END-IF                                                               
032710                                                                          
032720     PERFORM AA-INIT-NYCKLAR                                              
032800                                                                          
032900     IF MSGI-IDLAND-SPR = 'GB'                                            
033000       MOVE +2                 TO SPRAK-IX                                
033100       MOVE 'GB '              TO MED-IDSKYLT                             
033200     ELSE                                                                 
033300       MOVE +1                 TO SPRAK-IX                                
033400       MOVE 'S  '              TO MED-IDSKYLT                             
033500     END-IF                                                               
033600                                                                          
033700     MOVE JA                   TO INDATA-SW                               
033800     .                                                                    
033900     EJECT                                                                
033901 AA-INIT-NYCKLAR SECTION.                                                 
033902                                                                          
033903     MOVE ALL '+' TO MSGI-WMSGINIT                                        
033904     MOVE '001'                  TO MSGI-KDCALL                           
033905     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
033906     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
033907     MOVE '6137'                 TO MSGI-IDTRANS                          
033908     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
033909     .                                                                    
033910     EJECT                                                                
034000 B-KOLLA-NYCKLAR SECTION.                                                 
034100                                                                          
034200     MOVE JA TO NYCKLAR-SW                                                
034300                                                                          
034400     PERFORM BA-KOLLA-IDLEVNR                                             
034500     PERFORM BB-KOLLA-IDOKOLLI                                            
034510     PERFORM BC-KOLLA-IDDC                                                
034600                                                                          
034700     IF GODK-MID OR HELP-MID                                              
034800       PERFORM BD-FLYTTA-OEVRIGA-NYCKLAR                                  
034810       IF MFS-PRINT                                                       
034822         MOVE DCS-IDDC           TO MOD-IDDC-UT                           
034823         INSPECT MOD-IDDC-UT REPLACING LEADING ZERO BY SPACE              
034830       ELSE                                                               
034900         MOVE WS-IDLEVNR-KOLLI   TO MOD-IDLEVNR-KOLLI-UT                  
035200         MOVE WS-IDOKOLLI        TO MOD-IDOKOLLI-UT                       
035300         INSPECT MOD-IDOKOLLI-UT REPLACING LEADING ZERO BY SPACE          
035301         MOVE DCS-IDDC           TO MOD-IDDC-UT                           
035302         INSPECT MOD-IDDC-UT REPLACING LEADING ZERO BY SPACE              
035310       END-IF                                                             
035400     ELSE                                                                 
035500       MOVE MFS-RENSA-FAELT    TO MOD-IDLEVNR-KOLLI-UT                    
035600                                  MOD-IDOKOLLI-UT                         
035700                                  MOD-IDINLVGN-UT                         
035800                                  MOD-ADINLOMR-UT                         
035900                                  MOD-ADINLOMR-NXT-UT                     
036000                                  MOD-KDINLQ-UT                           
036100                                  MOD-BEFT-FOM-UT                         
036110                                  MOD-BEFT-TOM-UT                         
036200                                  MOD-FLINLFB-UT                          
036300                                  MOD-IDLOPNRM-UT                         
036310                                  MOD-IDDC-UT                             
036400     END-IF                                                               
036500                                                                          
036510     IF (MID-ADLAGOMR-UPD NOT = ALL '+') AND (GAMMAL-NYCKEL AND           
036511                                          EGEN-MID          AND           
036512                                          (DCS-CDC OR DCS-CDC-TR))        
036513       IF MFS-UPDATE                                                      
036520         MOVE JA TO NYCKLAR-SW                                            
036521       ELSE                                                               
036522         IF NOT MFS-PRINT                                                 
036523           MOVE INF-PRESS-PF4 TO MED-IDMFSFEL                             
036524           CALL WMEDKONV USING MED-WMEDAREA                               
036525           MOVE MED-MFSFEL       TO MOD-TEMFSFEL                          
036526           PERFORM MFS-ROER-EJ-FAELT-IN                                   
036527           PERFORM MFS-LAES-IN-IGEN                                       
036528           MOVE NEJ TO NYCKLAR-SW                                         
036529         END-IF                                                           
036530       END-IF                                                             
036540     ELSE                                                                 
036600       IF NYCKLAR-FEL                                                     
036700           MOVE ERR-WRONG-KEY    TO MED-IDMFSFEL                          
036800           CALL WMEDKONV USING MED-WMEDAREA                               
036900           MOVE MED-MFSFEL       TO MOD-TEMFSFEL                          
037000           PERFORM MFS-RENSA-FAELT-IN                                     
037100           IF HELP-MID                                                    
037200               CONTINUE                                                   
037300            ELSE                                                          
037400               MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADINLOMR-PRT-ATTR        
037500*              MOVE MFS-ROER-EJ-FAELT     TO MOD-ADINLOMR-PRT             
037600           END-IF                                                         
037700       END-IF                                                             
037710     END-IF                                                               
037800     .                                                                    
037900     EJECT                                                                
038000 BA-KOLLA-IDLEVNR   SECTION.                                              
038100                                                                          
038200     MOVE MFS-RENSA-FAELT      TO MOD-IDLEVNR-KOLLI-IN                    
038300                                                                          
038400     IF MID-IDLEVNR-KOLLI-IN   =  ALL '+'                                 
038500       MOVE MID-IDLEVNR-KOLLI-UT  TO WS-IDLEVNR-KOLLI                     
038700     ELSE                                                                 
038800       MOVE MID-IDLEVNR-KOLLI-IN TO WS-IDLEVNR-KOLLI                      
038900       MOVE '7'                TO MFS-IDPFK                               
039000       MOVE SPACE              TO MFS-KDTRTYP                             
039010       MOVE NEJ                TO GAMMAL-SW                               
039100     END-IF                                                               
039200                                                                          
039300** VID UTSKRIFT AV NY FLAGGA SKA NYCKLARNA RENSAS                         
039400                                                                          
039500     IF MFS-PRINT                                                         
039600         MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-KOLLI-UT                     
039700     ELSE                                                                 
039800         IF WS-IDLEVNR-KOLLI   NOT = SPACE                                
039950           IF WS-IDLEVNR-KOLLI < '99400'      OR                          
039960              WS-IDLEVNR-KOLLI > '99599'                                  
039961             IF WS-IDLEVNR-KOLLI = '3324 '                                
039962               CONTINUE                                                   
039963             ELSE                                                         
039970               MOVE NEJ TO NYCKLAR-SW                                     
039980             END-IF                                                       
039993           END-IF                                                         
040100         ELSE                                                             
040200           MOVE NEJ          TO NYCKLAR-SW                                
040300         END-IF                                                           
040400     END-IF                                                               
040500     .                                                                    
040600     EJECT                                                                
040700 BB-KOLLA-IDOKOLLI  SECTION.                                              
040800                                                                          
040900     MOVE MFS-RENSA-FAELT      TO MOD-IDOKOLLI-IN                         
041000                                                                          
041100     IF MID-IDOKOLLI-IN        = ALL '+'                                  
041200       MOVE MID-IDOKOLLI-UT    TO WS-IDOKOLLI                             
041300       INSPECT WS-IDOKOLLI REPLACING LEADING SPACE BY ZERO                
041400     ELSE                                                                 
041500       MOVE MID-IDOKOLLI-IN    TO WS-IDOKOLLI                             
041600       MOVE '7'                TO MFS-IDPFK                               
041700       MOVE SPACE              TO MFS-KDTRTYP                             
041710       MOVE NEJ                TO GAMMAL-SW                               
041800     END-IF                                                               
041900                                                                          
042000** VID UTSKRIFT AV NY FLAGGA SKA NYCKLARNA RENSAS                         
042100                                                                          
042200     IF MFS-PRINT                                                         
042210         MOVE MFS-RENSA-FAELT TO MOD-IDOKOLLI-UT                          
042400      ELSE                                                                
042500         IF WS-IDOKOLLI        NUMERIC AND                                
042600            WS-IDOKOLLI        > ZERO                                     
042701            MOVE WS-IDOKOLLI      TO WS-IDOKOLLI-NUM                      
042710            PERFORM IMS-GU-LOPA-LOPA11                                    
042720            IF WS-IDOKOLLI-NUM > LOPA-6018-IDOKOLLI                       
042730              MOVE NEJ TO NYCKLAR-SW                                      
042780            END-IF                                                        
042800          ELSE                                                            
042900             MOVE NEJ          TO NYCKLAR-SW                              
043000         END-IF                                                           
043100     END-IF                                                               
043200     .                                                                    
043300     EJECT                                                                
043310 BC-KOLLA-IDDC      SECTION.                                              
043320                                                                          
043330     MOVE MFS-RENSA-FAELT      TO MOD-IDDC-IN                             
043340                                                                          
043350     IF MID-IDDC-IN            = ALL '+'                                  
043360       MOVE MSGI-IDDC          TO W-IDDC-B6                               
043380     ELSE                                                                 
043390       MOVE MID-IDDC-IN        TO W-IDDC-B6                               
043391       IF NOT MFS-PRINT                                                   
043392         MOVE '7'              TO MFS-IDPFK                               
043393         MOVE SPACE            TO MFS-KDTRTYP                             
043394         MOVE NEJ              TO GAMMAL-SW                               
043395       END-IF                                                             
043396     END-IF                                                               
043397     PERFORM IMS-GU-WDB601                                                
043398                                                                          
043401     IF DCS-KDDC = SPACE OR DCS-DDC                                       
043402         MOVE NEJ              TO NYCKLAR-SW                              
043403      ELSE                                                                
043404        MOVE DCS-IDDC          TO W-IDDC                                  
043405                                  W-6005-IDDC                             
043410     END-IF                                                               
043412     .                                                                    
043413     EJECT                                                                
043420 BD-FLYTTA-OEVRIGA-NYCKLAR    SECTION.                                    
043500                                                                          
043700     IF MID-ADINLOMR-PRT       = ALL '+'                                  
043800       MOVE MID-ADINLOMR-PRT   TO MOD-ADINLOMR-PRT                        
043900     ELSE                                                                 
044000       MOVE MID-ADINLOMR-PRT   TO MOD-ADINLOMR-PRT                        
044100     END-IF                                                               
044300                                                                          
044310     MOVE MFS-RENSA-FAELT      TO MOD-IDINLVGN-IN                         
044320     IF MID-IDINLVGN-IN        = ALL '+'                                  
044330       MOVE MID-IDINLVGN-UT    TO WS-IDINLVGN                             
044340     ELSE                                                                 
044350       MOVE MID-IDINLVGN-IN    TO WS-IDINLVGN                             
044360     END-IF                                                               
044370     MOVE WS-IDINLVGN          TO MOD-IDINLVGN-UT                         
044380                                                                          
044400     MOVE MFS-RENSA-FAELT      TO MOD-ADINLOMR-IN                         
044500     IF MID-ADINLOMR-IN        = ALL '+'                                  
044600       MOVE MID-ADINLOMR-UT    TO WS-ADINLOMR                             
044700     ELSE                                                                 
044800       MOVE MID-ADINLOMR-IN    TO WS-ADINLOMR                             
044900     END-IF                                                               
045000     MOVE WS-ADINLOMR          TO MOD-ADINLOMR-UT                         
045100                                                                          
045200     MOVE MFS-RENSA-FAELT       TO MOD-ADINLOMR-NXT-IN                    
045300     IF MID-ADINLOMR-NXT-IN     = ALL '+'                                 
045400       MOVE MID-ADINLOMR-NXT-UT TO WS-ADINLOMR-NXT                        
045500     ELSE                                                                 
045600       MOVE MID-ADINLOMR-NXT-IN TO WS-ADINLOMR-NXT                        
045700     END-IF                                                               
045800     MOVE WS-ADINLOMR-NXT       TO MOD-ADINLOMR-NXT-UT                    
045900                                                                          
046000     MOVE MFS-RENSA-FAELT      TO MOD-KDINLQ-IN                           
046100     IF MID-KDINLQ-IN          = ALL '+'                                  
046200       MOVE MID-KDINLQ-UT      TO WS-KDINLQ                               
046300     ELSE                                                                 
046400       MOVE MID-KDINLQ-IN      TO WS-KDINLQ                               
046500     END-IF                                                               
046600     MOVE WS-KDINLQ            TO MOD-KDINLQ-UT                           
046700                                                                          
046800     MOVE MFS-RENSA-FAELT      TO MOD-BEFT-FOM-IN                         
046900     IF MID-BEFT-FOM-IN            = ALL '+'                              
047000       MOVE MID-BEFT-FOM-UT        TO WS-BEFT-FOM                         
047100     ELSE                                                                 
047200       MOVE MID-BEFT-FOM-IN        TO WS-BEFT-FOM                         
047300     END-IF                                                               
047400     MOVE WS-BEFT-FOM              TO MOD-BEFT-FOM-UT                     
047500                                                                          
047501     MOVE MFS-RENSA-FAELT      TO MOD-BEFT-TOM-IN                         
047510     IF MID-BEFT-TOM-IN            = ALL '+'                              
047520       MOVE MID-BEFT-TOM-UT        TO WS-BEFT-TOM                         
047530     ELSE                                                                 
047540       MOVE MID-BEFT-TOM-IN        TO WS-BEFT-TOM                         
047550     END-IF                                                               
047560     MOVE WS-BEFT-TOM              TO MOD-BEFT-TOM-UT                     
047570                                                                          
047600     MOVE MFS-RENSA-FAELT      TO MOD-FLINLFB-IN                          
047700     IF MID-FLINLFB-IN         = ALL '+'                                  
047800       MOVE MID-FLINLFB-UT     TO WS-FLINLFB                              
047900     ELSE                                                                 
048000       MOVE MID-FLINLFB-IN     TO WS-FLINLFB                              
048100     END-IF                                                               
048200     MOVE WS-FLINLFB           TO MOD-FLINLFB-UT                          
048300                                                                          
048400     MOVE MFS-RENSA-FAELT      TO MOD-IDLOPNRM-IN                         
048500     IF MID-IDLOPNRM-IN        = ALL '+'                                  
048600       MOVE MID-IDLOPNRM-UT    TO WS-IDLOPNRM                             
048700     ELSE                                                                 
048800       MOVE MID-IDLOPNRM-IN    TO WS-IDLOPNRM                             
048900     END-IF                                                               
049000     MOVE WS-IDLOPNRM          TO MOD-IDLOPNRM-UT                         
049100     .                                                                    
049200     EJECT                                                                
049300 E-SAMMA-SIDA SECTION.                                                    
049400                                                                          
049500     IF EGEN-MID OR HELP-MID                                              
049600         IF MID-INPUT1         = ALL '+'  AND                             
049700            MID-INPUT2         = ALL '+'                                  
049800             PERFORM MFS-RENSA-FAELT-IN                                   
049900          ELSE                                                            
050000             MOVE INF-PRESS-PF11 TO MED-IDMFSINF                          
050100             CALL WMEDKONV USING MED-WMEDAREA                             
050200             MOVE MED-MFSINF     TO MOD-TEMFSINF                          
050300             PERFORM EA-FLYTTA-MID-TILL-MOD                               
050400             PERFORM MFS-LAES-IN-IGEN                                     
050410             MOVE NEJ TO INDATA-SW                                        
050500         END-IF                                                           
050600     END-IF                                                               
050700     .                                                                    
050800     EJECT                                                                
050900 EA-FLYTTA-MID-TILL-MOD   SECTION.                                        
051000                                                                          
051100     IF MID-ADINLOMR-UPD       =  ALL '+'                                 
051200         MOVE MFS-RENSA-FAELT  TO MOD-ADINLOMR-UPD                        
051300      ELSE                                                                
051400         MOVE MID-ADINLOMR-UPD TO MOD-ADINLOMR-UPD                        
051500     END-IF                                                               
051600                                                                          
051700     IF MID-ADLAGOMR-UPD       = ALL '+'                                  
051800         MOVE MFS-RENSA-FAELT  TO MOD-ADLAGOMR-UPD                        
051900      ELSE                                                                
052000         MOVE MID-ADLAGOMR-UPD TO MOD-ADLAGOMR-UPD                        
052100     END-IF                                                               
052200                                                                          
052300     IF MID-FLPRIO-UPD         = ALL '+'                                  
052400         MOVE MFS-RENSA-FAELT  TO MOD-FLPRIO-UPD                          
052500      ELSE                                                                
052600         MOVE MID-FLPRIO-UPD   TO MOD-FLPRIO-UPD                          
052700     END-IF                                                               
052702                                                                          
052703     IF MID-ADLAGOMR           = ALL '+'                                  
052704         MOVE MFS-RENSA-FAELT  TO MOD-ADLAGOMR                            
052705      ELSE                                                                
052740         MOVE MID-ADLAGOMR     TO MOD-ADLAGOMR                            
052750     END-IF                                                               
052800                                                                          
052900     IF MID-ADINLOMR-PRT       =  ALL '+'                                 
053000         MOVE MID-ADINLOMR-PRT TO MOD-ADINLOMR-PRT                        
053100      ELSE                                                                
053200         MOVE MID-ADINLOMR-PRT TO MOD-ADINLOMR-PRT                        
053300     END-IF                                                               
053400                                                                          
053500     MOVE +1                      TO INDX                                 
053600     PERFORM UNTIL INDX           >  MAX-RAD                              
053700         IF MID-RAD (INDX)        =  ALL '+'                              
053800             MOVE MFS-RENSA-FAELT TO MOD-RAD(INDX)                        
053900          ELSE                                                            
054000             MOVE MID-RAD(INDX)   TO MOD-RAD(INDX)                        
054100         END-IF                                                           
054200         ADD +1                   TO INDX                                 
054300     END-PERFORM                                                          
054400     .                                                                    
054500     EJECT                                                                
054600 F-LAES-VISA-INFO SECTION.                                                
054700                                                                          
054800     IF HELP-MID OR MFS-ENTER                                             
054900         CONTINUE                                                         
055000      ELSE                                                                
055100         PERFORM MFS-RENSA-FAELT-IN                                       
055200     END-IF                                                               
055300     MOVE WS-IDLEVNR-KOLLI     TO W-D1CSEQ-IDLEVNR-KOLLI                  
055400                                  W-IDLEVNR-KOLLI                         
055500     MOVE WS-IDOKOLLI          TO W-D1CSEQ-IDOKOLLI                       
055600                                  W-IDOKOLLI                              
055700     PERFORM IMS-GU-INLA2-INLA11                                          
055701     IF SEGMENT-FINNS                                                     
055702       MOVE ART-ADLAGOMR TO SPAR-ADLAGOMR                                 
055710       PERFORM IMS-GNP-INLA2-INLA21                                       
055711     END-IF                                                               
055720     PERFORM UNTIL SEGMENT-SAKNAS OR STATUS-OK                            
055730       PERFORM UNTIL SEGMENT-SAKNAS OR STATUS-OK                          
055740         IF RAD-KDINLSTA = 'SAK' OR SPACE OR 'FPK'                        
055750           MOVE JA TO STATUS-SW                                           
055760           MOVE JA TO KOLLI-SW                                            
055770           MOVE RAD-FLDIVKLI TO SPAR-FLDIVKLI                             
055780           MOVE RAD-ADINLOMR TO SPAR-ADINLOMR                             
055790           MOVE RAD-FLPRIO   TO SPAR-FLPRIO                               
055791         END-IF                                                           
055792         PERFORM IMS-GNP-INLA2-INLA21                                     
055793       END-PERFORM                                                        
055794       PERFORM IMS-GN-INLA2-INLA11                                        
055795     END-PERFORM                                                          
055800     IF KOLLI-FINNS        AND                                            
055900        SPAR-FLDIVKLI = JA AND                                            
056000        STATUS-OK                                                         
056100         MOVE SPAR-ADINLOMR            TO MOD-ADINLOMR-UPD                
056110         MOVE SPAR-ADLAGOMR            TO MOD-ADLAGOMR                    
056200         MOVE SPAR-FLPRIO              TO MOD-FLPRIO-UPD                  
056210         IF ENGLISH-TEXT                                                  
056220           IF MOD-FLPRIO-UPD = JA                                         
056230             MOVE YES                  TO MOD-FLPRIO-UPD                  
056240           END-IF                                                         
056250         END-IF                                                           
056300         MOVE MFS-STAENG-FAELT         TO MOD-ADINLOMR-UPD-ATTR           
056400         MOVE MFS-ALFA-FAELT-RAETT     TO MOD-FLPRIO-UPD-ATTR             
056500         MOVE MFS-ADD-SAETT-CURSOR   TO MOD-IDLOPNRM-RAD-ATTR(1)          
056600      ELSE                                                                
056700         MOVE 'N'                      TO MOD-FLPRIO-UPD                  
056800         IF KOLLI-SAKNAS                                                  
056900           MOVE MFS-ADD-SAETT-CURSOR TO                                   
057000                                     MOD-ADINLOMR-UPD-ATTR                
057100         ELSE                                                             
057200           IF SPAR-FLDIVKLI = NEJ AND STATUS-OK                           
057400                MOVE '188'        TO MED-IDMFSFEL                         
057500             ELSE                                                         
057600                MOVE '010'        TO MED-IDMFSFEL                         
057700            END-IF                                                        
057800            CALL WMEDKONV USING MED-WMEDAREA                              
057900            MOVE MED-MFSFEL       TO MOD-TEMFSFEL                         
058000         END-IF                                                           
058100     END-IF                                                               
058200                                                                          
058300     IF HELP-MID                                                          
058400         CONTINUE                                                         
058500      ELSE                                                                
058600         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADINLOMR-PRT-ATTR              
058800     END-IF                                                               
058900     .                                                                    
059000     EJECT                                                                
059100 G-KOLLA-INPUT SECTION.                                                   
059200                                                                          
059300     MOVE SPACE                TO MED-IDMFSFEL                            
059400     IF MFS-PRINT                                                         
059500         PERFORM GA-KOLLA-UTSKRIFT-NY-FLAGGA                              
059600      ELSE                                                                
059700         PERFORM GB-KOLLA-UPD-ARTRADER-KOLLI                              
059800     END-IF                                                               
059900     .                                                                    
060000     EJECT                                                                
060100 GA-KOLLA-UTSKRIFT-NY-FLAGGA SECTION.                                     
060200                                                                          
060300     IF MID-INPUT1 = ALL '+'                                              
060400         MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                        
060500         CALL WMEDKONV USING MED-WMEDAREA                                 
060600         MOVE MED-MFSFEL       TO MOD-TEMFSFEL                            
060700         PERFORM MFS-ROER-EJ-FAELT-IN                                     
060800         MOVE NEJ              TO INDATA-SW                               
060900      ELSE                                                                
061000         PERFORM S03-KOLLA-ADLAGOMR                                       
061100         PERFORM GAA-KOLLA-ADINLOMR-PRT                                   
061200                                                                          
061300         MOVE MFS-ALFA-FAELT-RAETT     TO MOD-FLPRIO-UPD                  
061400         MOVE +1                       TO INDX                            
061500         PERFORM UNTIL INDX            >  MAX-RAD                         
061600             MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDLOPNRM-RAD(INDX)          
061700             ADD +1                    TO INDX                            
061800         END-PERFORM                                                      
061900                                                                          
062000         IF INDATA-FEL                                                    
062100             IF MED-IDMFSFEL               = SPACE                        
062200                 MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                
062300             END-IF                                                       
062400             CALL WMEDKONV USING MED-WMEDAREA                             
062500             MOVE MED-MFSFEL               TO MOD-TEMFSFEL                
062600             PERFORM MFS-ROER-EJ-FAELT-IN                                 
062700         END-IF                                                           
062800     END-IF                                                               
062900     .                                                                    
063000     EJECT                                                                
063100 GAA-KOLLA-ADINLOMR-PRT  SECTION.                                         
063200                                                                          
063300     IF MID-ADINLOMR-PRT                 NOT = ALL '+'                    
063400         MOVE MID-ADINLOMR-PRT           TO W-6006-ADINLOMR               
063500         PERFORM IMS-GU-PLAA-PLAA11                                       
063600         IF SEGMENT-SAKNAS                                                
063700             MOVE NEJ                    TO INDATA-SW                     
063800             MOVE '772'                  TO MED-IDMFSFEL                  
063900             MOVE MFS-ALFA-FAELT-FEL     TO MOD-ADINLOMR-PRT-ATTR         
064000          ELSE                                                            
064100             MOVE PLAA-6006-IDLEVNR      TO W-SPAR-IDLEVNR-KOLLI          
064200             MOVE SPACE                  TO PRT-IDPRTLST                  
064300             MOVE '6F'                   TO PRT-IDPRTLST(1:2)             
064400             MOVE MID-ADINLOMR-PRT       TO PRT-IDPRTLST(3:6)             
064500             MOVE 1                      TO PRT-KDCALL                    
064600             CALL W006PRT  USING PRT-W006PRT                              
064700             IF PRT-KDSVAR               = 'F'                            
064800                 MOVE NEJ                TO INDATA-SW                     
064900                 MOVE '772'              TO MED-IDMFSFEL                  
065000                 MOVE MFS-ALFA-FAELT-FEL TO MOD-ADINLOMR-PRT-ATTR         
065100             ELSE                                                         
065200                 MOVE PRT-BEPRTLST       TO MOD-TEMFSFEL                  
065300                 MOVE MFS-ALFA-FAELT-RAETT                                
065400                                         TO MOD-ADINLOMR-PRT-ATTR         
065500             END-IF                                                       
065600         END-IF                                                           
065700      ELSE                                                                
065800         MOVE NEJ                TO INDATA-SW                             
065900         MOVE MFS-ALFA-FAELT-FEL TO MOD-ADINLOMR-PRT-ATTR                 
066000     END-IF                                                               
066100     .                                                                    
066200     EJECT                                                                
066300 GB-KOLLA-UPD-ARTRADER-KOLLI   SECTION.                                   
066400                                                                          
066410     IF MID-ADLAGOMR-UPD NOT = ALL '+'                                    
066420       MOVE INF-PRESS-PF4 TO MED-IDMFSFEL                                 
066421       CALL WMEDKONV USING MED-WMEDAREA                                   
066422       MOVE MED-MFSFEL       TO MOD-TEMFSFEL                              
066423       PERFORM MFS-ROER-EJ-FAELT-IN                                       
066424       MOVE NEJ              TO INDATA-SW                                 
066430     ELSE                                                                 
066500       IF MID-INPUT1 = ALL '+' OR MID-INPUT2 = ALL '+'                    
066600           MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                      
066700           CALL WMEDKONV USING MED-WMEDAREA                               
066800           MOVE MED-MFSFEL       TO MOD-TEMFSFEL                          
066900           PERFORM MFS-ROER-EJ-FAELT-IN                                   
067000           MOVE NEJ              TO INDATA-SW                             
067100        ELSE                                                              
067200           MOVE WS-IDLEVNR-KOLLI TO W-D1CSEQ-IDLEVNR-KOLLI                
067300                                        W-IDLEVNR-KOLLI                   
067400           MOVE WS-IDOKOLLI      TO W-D1CSEQ-IDOKOLLI                     
067500                                        W-IDOKOLLI                        
067600           PERFORM IMS-GU-INLA2-INLA21                                    
067700           IF SEGMENT-SAKNAS                                              
067800               MOVE MID-ADINLOMR-UPD TO W-KOLLI-ADINLOMR                  
067900                                        WS-ADINLOMR                       
068000               MOVE SPACE        TO W-KOLLI-ADINLOMR-NXT                  
068100               MOVE ZERO         TO W-KOLLI-IDINLVGN                      
068110               IF MID-FLPRIO-UPD = 'J' OR 'P' OR 'Y'                      
068200                 MOVE 'J'            TO W-KOLLI-FLPRIO                    
068210               ELSE                                                       
068211                 MOVE 'N'            TO W-KOLLI-FLPRIO                    
068220               END-IF                                                     
068300               PERFORM GBA-KOLLA-RADUPG-IFYLLDA                           
068400           ELSE                                                           
068500               IF RAD-FLDIVKLI   =  NEJ                                   
068600                   MOVE NEJ      TO INDATA-SW                             
068700                   MOVE '188'    TO MED-IDMFSFEL                          
068800               END-IF                                                     
068900               IF RAD-KDINLSTA   =  SPACE OR 'SAK' OR 'FPK'               
069000                   CONTINUE                                               
069100                ELSE                                                      
069200                   MOVE NEJ      TO INDATA-SW                             
069300                   MOVE '010'    TO MED-IDMFSFEL                          
069400               END-IF                                                     
069500               MOVE RAD-ADINLOMR TO W-KOLLI-ADINLOMR                      
069600               MOVE RAD-ADINLOMR-NXT TO W-KOLLI-ADINLOMR-NXT              
069700               MOVE RAD-IDINLVGN TO W-KOLLI-IDINLVGN                      
069701               IF (RAD-FLPRIO = JA AND                                    
069702                  MID-FLPRIO-UPD = 'J' OR 'P' OR 'Y') OR                  
069703                  (RAD-FLPRIO = NEJ AND                                   
069704                  MID-FLPRIO-UPD = 'N')                                   
069800                 MOVE RAD-FLPRIO   TO W-KOLLI-FLPRIO                      
069810               ELSE                                                       
069811                 IF MID-FLPRIO-UPD = 'J' OR 'P' OR 'Y'                    
069812                   MOVE 'J'            TO W-KOLLI-FLPRIO                  
069813                 ELSE                                                     
069814                   MOVE 'N'            TO W-KOLLI-FLPRIO                  
069815                 END-IF                                                   
069830                 MOVE JA TO AENDRA-FLPRIO-SW                              
069840               END-IF                                                     
069900           END-IF                                                         
070000                                                                          
070100           MOVE +1                       TO INDX                          
070200           PERFORM UNTIL INDX            >  MAX-RAD                       
070300               IF MID-IDLOPNRM-RAD (INDX) = ALL '+'                       
070400                   MOVE MFS-NUM-FAELT-RAETT TO                            
070500                      MOD-IDLOPNRM-RAD-ATTR(INDX)                         
070600                ELSE                                                      
070700                   PERFORM GBB-KOLLA-RADER                                
070710                   PERFORM S50-PRIM-CONTROL                               
070800               END-IF                                                     
070900               ADD +1            TO INDX                                  
071000           END-PERFORM                                                    
071100                                                                          
071200           IF INDATA-FEL                                                  
071300               IF MED-IDMFSFEL               = SPACE                      
071400                   MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL              
071500               END-IF                                                     
071600               CALL WMEDKONV USING MED-WMEDAREA                           
071700               MOVE MED-MFSFEL               TO MOD-TEMFSFEL              
071800               PERFORM MFS-ROER-EJ-FAELT-IN                               
071900           END-IF                                                         
072000       END-IF                                                             
072010     END-IF                                                               
072100     .                                                                    
072200     EJECT                                                                
072300 GBA-KOLLA-RADUPG-IFYLLDA     SECTION.                                    
072400                                                                          
072500     PERFORM S01-KOLLA-ADINLOMR                                           
072600     PERFORM S04-KOLLA-FLPRIO                                             
072700     .                                                                    
072800     EJECT                                                                
072900 GBB-KOLLA-RADER       SECTION.                                           
073000                                                                          
073100     IF MID-IDLOPNRM-RAD (INDX)     NUMERIC AND                           
073200        MID-IDLOPNRM-RAD (INDX)     > ZERO                                
073300         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDLOPNRM-RAD-ATTR(INDX)          
073400         IF MID-IDRADNR-RAD(INDX)  NUMERIC                                
073500             PERFORM GBBA-KOLLA-ETIKETT                                   
073600          ELSE                                                            
073700             PERFORM GBBB-KOLLA-PARTI                                     
073800         END-IF                                                           
073900       ELSE                                                               
074000         MOVE MFS-NUM-FAELT-FEL   TO MOD-IDLOPNRM-RAD-ATTR(INDX)          
074100         MOVE NEJ                 TO INDATA-SW                            
074200     END-IF                                                               
074300     .                                                                    
074400     EJECT                                                                
074500 GBBA-KOLLA-ETIKETT SECTION.                                              
074600                                                                          
074700     MOVE MID-IDLOPNRM-RAD (INDX) TO W-D1BSEQ-IDLOPNRM                    
074800     MOVE MID-IDRADNR-RAD  (INDX) TO W-IDRADNR                            
074900     PERFORM IMS-GU-INLA1-INLA21                                          
075000     IF SEGMENT-FINNS                                                     
075100         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDLOPNRM-RAD-ATTR(INDX)          
075200         PERFORM GBBAA-KOLLA-KDINLSTA-DIVKLI                              
075300      ELSE                                                                
075400         MOVE MFS-NUM-FAELT-FEL   TO MOD-IDLOPNRM-RAD-ATTR(INDX)          
075500         MOVE NEJ                  TO INDATA-SW                           
075600     END-IF                                                               
075700     .                                                                    
075800     EJECT                                                                
075900 GBBAA-KOLLA-KDINLSTA-DIVKLI   SECTION.                                   
076000                                                                          
076100     IF RAD-KDINLSTA               =  SPACE OR 'FPK' OR 'SAK'             
076200         IF RAD-IDOKOLLI > +0 AND RAD-FLDIVKLI = NEJ                      
076221           MOVE '007'               TO MED-IDMFSFEL                       
076222           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDLOPNRM-RAD-ATTR(INDX)        
076223           MOVE NEJ                 TO INDATA-SW                          
076230         END-IF                                                           
076300      ELSE                                                                
076400         MOVE '007'                TO MED-IDMFSFEL                        
076500         MOVE MFS-NUM-FAELT-FEL    TO MOD-IDLOPNRM-RAD-ATTR(INDX)         
076600         MOVE NEJ                  TO INDATA-SW                           
076700     END-IF                                                               
076800     .                                                                    
076900     EJECT                                                                
077000 GBBB-KOLLA-PARTI   SECTION.                                              
077100                                                                          
077200     MOVE MID-IDLOPNRM-RAD (INDX) TO W-D1BSEQ-IDLOPNRM                    
077300     MOVE +1                      TO W-IDRADNR                            
077400     PERFORM IMS-GU-INLA1-INLA21                                          
077500     IF SEGMENT-FINNS                                                     
077600         MOVE MFS-NUM-FAELT-RAETT TO MOD-IDLOPNRM-RAD-ATTR(INDX)          
077700         PERFORM GBBBA-KOLLA-ANDRA-RADER                                  
077800      ELSE                                                                
077900         MOVE MFS-NUM-FAELT-FEL   TO MOD-IDLOPNRM-RAD-ATTR(INDX)          
078000         MOVE NEJ                  TO INDATA-SW                           
078100     END-IF                                                               
078200     .                                                                    
078300     EJECT                                                                
078400 GBBBA-KOLLA-ANDRA-RADER  SECTION.                                        
078500                                                                          
078600     MOVE JA                   TO RAD-SW                                  
078700     PERFORM IMS-GNP-INLA1-INLA21                                         
078800     PERFORM UNTIL SEGMENT-SAKNAS OR RAD-FEL                              
078900         IF RAD-KDINLSTA       = 'AVV' OR 'ANT' OR 'KVA' OR 'VOR'         
079000             CONTINUE                                                     
079100          ELSE                                                            
079200             MOVE NEJ          TO RAD-SW                                  
079300             MOVE '007'        TO MED-IDMFSFEL                            
079400             MOVE MFS-NUM-FAELT-FEL                                       
079500                               TO MOD-IDLOPNRM-RAD-ATTR(INDX)             
079600             MOVE NEJ          TO INDATA-SW                               
079700         END-IF                                                           
079800         PERFORM IMS-GNP-INLA1-INLA21                                     
079900     END-PERFORM                                                          
080000     .                                                                    
080100     EJECT                                                                
080200 H-UPPDATERA SECTION.                                                     
080300                                                                          
080400     IF MFS-PRINT                                                         
080500         PERFORM HA-UPD-INLA-SKRIV-FLAGGA                                 
080600         MOVE '790'            TO MED-IDMFSINF                            
080700      ELSE                                                                
080800         PERFORM HB-UPD-DIVERSE-KOLLI                                     
080810         IF AENDRA-FLPRIO                                                 
080820           PERFORM S05-AENDRA-FLPRIO                                      
080830         END-IF                                                           
080900         MOVE INF-UPDATE-DONE  TO MED-IDMFSINF                            
081000     END-IF                                                               
081100                                                                          
081200     CALL WMEDKONV USING MED-WMEDAREA                                     
081300     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
081400     MOVE MFS-ROER-EJ-FAELT    TO MOD-ADINLOMR-UPD                        
081500                                  MOD-ADLAGOMR-UPD                        
081600                                  MOD-FLPRIO-UPD                          
081610                                  MOD-ADLAGOMR                            
081700     PERFORM MFS-FORM-ATTR                                                
081800     PERFORM MFS-RENSA-FAELT-IN                                           
081900     .                                                                    
082000     EJECT                                                                
082100 HA-UPD-INLA-SKRIV-FLAGGA SECTION.                                        
082200                                                                          
082300     PERFORM HAA-TA-FRAM-NYTT-IDOKOLLI                                    
082400                                                                          
082500     PERFORM HAB-STARTA-W60194                                            
082600     .                                                                    
082700     EJECT                                                                
082800 HAA-TA-FRAM-NYTT-IDOKOLLI SECTION.                                       
082900                                                                          
083000     PERFORM IMS-GHU-LOPA-LOPA11                                          
083100     COMPUTE LOPA-6018-IDOKOLLI = LOPA-6018-IDOKOLLI + 1                  
083200     PERFORM IMS-REPL-LOPA-LOPA11                                         
083300     .                                                                    
083400     EJECT                                                                
083500 HAB-STARTA-W60194         SECTION.                                       
083600                                                                          
083700     MOVE PRT-IDPRTLST         TO MOD6194-MID-IDPRTLST                    
083800     MOVE 'W6013700'           TO MOD6194-MID-IDPGM                       
083900     MOVE +1                   TO MOD6194-MID-KVPOST                      
084000     MOVE W-SPAR-IDLEVNR-KOLLI TO MOD6194-MID-IDLEVNR-KOLLI(1)            
084100     MOVE LOPA-6018-IDOKOLLI   TO MOD6194-MID-IDOKOLLI(1)                 
084200     MOVE MID-ADLAGOMR-UPD     TO MOD6194-MID-ADLAGOMR(1)                 
084300     MOVE ZERO                 TO MOD6194-MID-IDARTNR (1)                 
084400                                  MOD6194-MID-IDLOPNRM(1)                 
084410                                  MOD6194-MID-KVINLART(1)                 
084500                                  MOD6194-MID-TIINLMOT(1)                 
084600                                  MOD6194-MID-VKKOLLIN(1)                 
084700                                  MOD6194-MID-VKKOLLIB(1)                 
084800                                  MOD6194-MID-ADGANG  (1)                 
084900                                  MOD6194-MID-ADPLATS (1)                 
084901                                  MOD6194-MID-BEFT    (1)                 
084910     MOVE SPACE                TO MOD6194-MID-KDSORT(1)                   
085000                                                                          
085100     COMPUTE P-TO-P-KVLL       =  LNG-P-TO-P-PREFIX +                     
085200                                  23 + (MOD6194-MID-KVPOST * 67)          
085300     MOVE 'W6T194X '           TO P-TO-P-KDTRANS                          
085400     MOVE '6137'               TO P-TO-P-IDTRANS                          
085500     MOVE MFS-KDMFSFOR         TO P-TO-P-KDMFSFOR                         
085600                                                                          
085700     MOVE MOD6194-MID-W6I19401 TO P-TO-P-DATA                             
085800                                                                          
085900     PERFORM IMS-ISRT-ALT-MSG-6194                                        
086000     .                                                                    
086100     EJECT                                                                
086200 HB-UPD-DIVERSE-KOLLI SECTION.                                            
086300                                                                          
086400     MOVE +1                   TO INDX                                    
086500                                  6191-IX                                 
086600     PERFORM UNTIL INDX        >  MAX-RAD                                 
086700         IF MID-IDLOPNRM-RAD (INDX) = ALL '+'                             
086800             CONTINUE                                                     
086900          ELSE                                                            
087000             PERFORM HBA-UPPDATERA-RAD-KOLLI                              
087100             IF W-SPAR-ADINLOMR     = RAD-ADINLOMR     AND                
087200                W-SPAR-ADINLOMR-NXT = RAD-ADINLOMR-NXT AND                
087300                W-SPAR-KDINLSTA     = RAD-KDINLSTA                        
087400                 CONTINUE                                                 
087500              ELSE                                                        
087600                 PERFORM HBB-FLYTTA-TILL-6191MID                          
087700             END-IF                                                       
087800         END-IF                                                           
087900         ADD +1                TO INDX                                    
088000     END-PERFORM                                                          
088100                                                                          
088200     IF 6191-IX                >  1                                       
088300         PERFORM HBC-STARTA-W60191                                        
088400     END-IF                                                               
088500     .                                                                    
088600     EJECT                                                                
088700 HBA-UPPDATERA-RAD-KOLLI SECTION.                                         
088800                                                                          
088900     MOVE MID-IDLOPNRM-RAD    (INDX) TO W-D1BSEQ-IDLOPNRM                 
089000     IF MID-IDRADNR-RAD       (INDX) NUMERIC                              
089100         MOVE MID-IDRADNR-RAD (INDX) TO W-IDRADNR                         
089200      ELSE                                                                
089300         MOVE +1                     TO W-IDRADNR                         
089400     END-IF                                                               
089500                                                                          
089600     PERFORM IMS-GHU-INLA1-INLA21                                         
089700                                                                          
089800     MOVE RAD-ADINLOMR         TO W-SPAR-ADINLOMR                         
089900     MOVE RAD-ADINLOMR-NXT     TO W-SPAR-ADINLOMR-NXT                     
090000     MOVE RAD-KDINLSTA         TO W-SPAR-KDINLSTA                         
090100                                                                          
090200     MOVE WS-IDLEVNR-KOLLI     TO RAD-IDLEVNR-KOLLI                       
090300     MOVE WS-IDOKOLLI          TO RAD-IDOKOLLI                            
090400     MOVE W-KOLLI-ADINLOMR     TO RAD-ADINLOMR                            
090500     MOVE W-KOLLI-ADINLOMR-NXT TO RAD-ADINLOMR-NXT                        
090600     MOVE JA                   TO RAD-FLDIVKLI                            
090700     MOVE W-KOLLI-IDINLVGN     TO RAD-IDINLVGN                            
090800                                                                          
090900     IF RAD-KDINLSTA           =  'SAK'                                   
091000         MOVE SPACE            TO RAD-KDINLSTA                            
091100     END-IF                                                               
091200                                                                          
091300     PERFORM IMS-REPL-INLA1-INLA21                                        
091400                                                                          
091500     IF RAD-FLPRIO             NOT = W-KOLLI-FLPRIO                       
091600         PERFORM HBAA-CALL-W611PMRK                                       
091700     END-IF                                                               
091800     .                                                                    
091900     EJECT                                                                
092000 HBAA-CALL-W611PMRK      SECTION.                                         
092100                                                                          
092200     MOVE SPACE                  TO PMRK-IDLEVNR                          
092300     MOVE ZERO                   TO PMRK-IDOKOLLI                         
092400     MOVE MID-IDLOPNRM-RAD(INDX) TO PMRK-IDLOPNRM                         
092500     MOVE RAD-IDRADNR            TO PMRK-IDRADNR                          
092600     CALL W611PMRK USING PMRK-W611PMRK PMRK-INLB-PCB                      
092700                         PMRK-INLC-PCB PMRK-PLAA-PCB                      
092800     .                                                                    
092900     EJECT                                                                
093000 HBB-FLYTTA-TILL-6191MID SECTION.                                         
093100                                                                          
093200     PERFORM IMS-GU-INLA1-INLA11                                          
093300                                                                          
093400     MOVE ART-IDLOPNRM         TO MOD6191-MID-IDLOPNRM (6191-IX)          
093500     MOVE RAD-IDRADNR          TO MOD6191-MID-IDRADNR  (6191-IX)          
093600     MOVE RAD-KDINLPRIO        TO MOD6191-MID-KDINLPRIO(6191-IX)          
093700     MOVE ART-PRARTSTD         TO MOD6191-MID-PRARTSTD (6191-IX)          
093710     MOVE +0                   TO MOD6191-MID-KVKOLLI  (6191-IX)          
093720     MOVE 'N'                  TO MOD6191-MID-FLINLI   (6191-IX)          
093800     MOVE W-SPAR-ADINLOMR      TO MOD6191-MID-ADINLOMR-OLD                
093900                                                         (6191-IX)        
094000     MOVE W-SPAR-ADINLOMR-NXT  TO MOD6191-MID-ADINLOMR-NXT-OLD            
094100                                                         (6191-IX)        
094200     MOVE W-SPAR-KDINLSTA      TO MOD6191-MID-KDINLSTA-OLD                
094300                                                         (6191-IX)        
094400     MOVE RAD-KVINLART         TO MOD6191-MID-KVINLART-OLD                
094500                                                         (6191-IX)        
094600     MOVE RAD-ADINLOMR         TO MOD6191-MID-ADINLOMR-NEW                
094700                                                         (6191-IX)        
094800     MOVE RAD-ADINLOMR-NXT     TO MOD6191-MID-ADINLOMR-NXT-NEW            
094900                                                         (6191-IX)        
095000     MOVE RAD-KDINLSTA         TO MOD6191-MID-KDINLSTA-NEW                
095100                                                         (6191-IX)        
095200     MOVE RAD-KVINLART         TO MOD6191-MID-KVINLART-NEW                
095300                                                         (6191-IX)        
095400     ADD +1                    TO 6191-IX                                 
095500     .                                                                    
095600     EJECT                                                                
095700 HBC-STARTA-W60191      SECTION.                                          
095800                                                                          
095900     COMPUTE MOD6191-MID-KVPOST = 6191-IX - 1                             
096000     MOVE 'W6013700'           TO MOD6191-MID-IDPGM                       
096010     MOVE DCS-IDDC             TO MOD6191-MID-IDDC                        
096100     COMPUTE P-TO-P-KVLL        = LNG-P-TO-P-PREFIX +                     
096200                                  17 + (MOD6191-MID-KVPOST * 64)          
096300     MOVE 'W6T191X '           TO P-TO-P-KDTRANS                          
096400     MOVE '6137'               TO P-TO-P-IDTRANS                          
096500     MOVE MFS-KDMFSFOR         TO P-TO-P-KDMFSFOR                         
096600                                                                          
096700     MOVE MOD6191-MID-W6I19101 TO P-TO-P-DATA                             
096800                                                                          
096900     PERFORM IMS-ISRT-ALT-MSG-6191                                        
097000     .                                                                    
097100     EJECT                                                                
097200 S01-KOLLA-ADINLOMR     SECTION.                                          
097300                                                                          
097400     IF MID-ADINLOMR-UPD               NOT = ALL '+'                      
097500         MOVE MID-ADINLOMR-UPD         TO W-6006-ADINLOMR                 
097600         PERFORM IMS-GU-PLAA-PLAA11                                       
097700         IF SEGMENT-SAKNAS                                                
097800             MOVE MFS-ALFA-FAELT-FEL   TO MOD-ADINLOMR-UPD-ATTR           
097900             MOVE NEJ                  TO INDATA-SW                       
098000           ELSE                                                           
098100             MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADINLOMR-UPD-ATTR           
098200         END-IF                                                           
098300      ELSE                                                                
098400         MOVE MFS-ALFA-FAELT-FEL       TO MOD-ADINLOMR-UPD-ATTR           
098500         MOVE NEJ                      TO INDATA-SW                       
098600     END-IF                                                               
098700     .                                                                    
098800     EJECT                                                                
098900 S03-KOLLA-ADLAGOMR      SECTION.                                         
099000                                                                          
099100     IF MID-ADLAGOMR-UPD           NOT = ALL '+' AND                      
099200        MID-ADLAGOMR-UPD           NUMERIC                                
099300         MOVE MFS-NUM-FAELT-RAETT TO MOD-ADLAGOMR-UPD-ATTR                
099400      ELSE                                                                
099500         MOVE MFS-NUM-FAELT-FEL   TO MOD-ADLAGOMR-UPD-ATTR                
099600         MOVE NEJ                  TO INDATA-SW                           
099700     END-IF                                                               
099800     .                                                                    
099900     EJECT                                                                
100000 S04-KOLLA-FLPRIO      SECTION.                                           
100100                                                                          
100200     IF MID-FLPRIO-UPD                 NOT = ALL '+'                      
100300         IF MID-FLPRIO-UPD             = 'N' OR 'P' OR 'J' OR 'Y'         
100400             MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLPRIO-UPD-ATTR             
100500           ELSE                                                           
100600             MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLPRIO-UPD-ATTR             
100700             MOVE NEJ                  TO INDATA-SW                       
100800         END-IF                                                           
100900      ELSE                                                                
101000         MOVE MFS-ALFA-FAELT-FEL       TO MOD-FLPRIO-UPD-ATTR             
101100         MOVE NEJ                      TO INDATA-SW                       
101200     END-IF                                                               
101300     .                                                                    
101400     EJECT                                                                
101410 S05-AENDRA-FLPRIO SECTION.                                               
101420                                                                          
101421     MOVE WS-IDLEVNR-KOLLI     TO W-D1CSEQ-IDLEVNR-KOLLI                  
101422                                  W-IDLEVNR-KOLLI                         
101423     MOVE WS-IDOKOLLI          TO W-D1CSEQ-IDOKOLLI                       
101424                                  W-IDOKOLLI                              
101430     PERFORM IMS-GU-INLA2-INLA11                                          
101431     MOVE ART-IDLOPNRM TO PMRK-IDLOPNRM                                   
101440     PERFORM IMS-GNP-INLA2-INLA21                                         
101441     MOVE RAD-IDRADNR TO PMRK-IDRADNR                                     
101442                                                                          
101450     PERFORM UNTIL SEGMENT-SAKNAS                                         
101460       IF RAD-FLPRIO = W-KOLLI-FLPRIO                                     
101470         CONTINUE                                                         
101480       ELSE                                                               
101481         CALL W611PMRK USING PMRK-W611PMRK PMRK-INLB-PCB                  
101482                             PMRK-INLC-PCB PMRK-PLAA-PCB                  
101483       END-IF                                                             
101484       PERFORM IMS-GN-INLA2-INLA11                                        
101485       IF SEGMENT-FINNS                                                   
101486         IF ART-IDLOPNRM = PMRK-IDLOPNRM                                  
101487           PERFORM IMS-GNP-INLA2-INLA21                                   
101488           PERFORM UNTIL RAD-IDRADNR = PMRK-IDRADNR                       
101489             PERFORM IMS-GNP-INLA2-INLA21                                 
101490           END-PERFORM                                                    
101491         ELSE                                                             
101492           MOVE ART-IDLOPNRM TO PMRK-IDLOPNRM                             
101493         END-IF                                                           
101495         PERFORM IMS-GNP-INLA2-INLA21                                     
101496         MOVE RAD-IDRADNR TO PMRK-IDRADNR                                 
101497       END-IF                                                             
101498     END-PERFORM                                                          
101499     .                                                                    
101500     EJECT                                                                
101501                                                                          
101502 S50-PRIM-CONTROL SECTION.                                                
101503* THIS IS A CONTROL TO CHECK THE OLD PLACE IF FLAG FLKNTRGK = YES         
101504* IF THE FLAG IS YES WE HAVE TO DO A CHECK ON THE OLD PLACE BEFORE        
101505* WE CHECK THE NEW PLACE.                                                 
101506     MOVE RAD-ADINLOMR         TO W-6006-ADINLOMR                         
101507     PERFORM IMS-GU-PLAA-PLAA11                                           
101508     IF SEGMENT-FINNS                                                     
101509       IF PLAA-6006-FLKNTRGK = JA                                         
101511         MOVE MID-ADINLOMR-UPD TO W-6006-ADINLOMR                         
101515         PERFORM IMS-GU-PLAA-PLAA11                                       
101516         IF (PLAA-6006-FLKNTRGK = JA )                                    
101517         OR PLAA-6006-KDINLOMR = 'LPL'                                    
101518           MOVE 'N' TO PRIM-CONTROL-SW                                    
101519         ELSE                                                             
101520           MOVE 'J' TO PRIM-CONTROL-SW                                    
101521         END-IF                                                           
101522       ELSE                                                               
101523         MOVE 'N' TO PRIM-CONTROL-SW                                      
101524       END-IF                                                             
101525     ELSE                                                                 
101526       MOVE 'N' TO PRIM-CONTROL-SW                                        
101527     END-IF                                                               
101528     IF PRIM-CONTROL-YES                                                  
101529       PERFORM S51-CHECK-OLD-PLACE                                        
101530     END-IF                                                               
101531     .                                                                    
101532     EJECT                                                                
101533                                                                          
101534 S51-CHECK-OLD-PLACE     SECTION.                                         
101535     MOVE MID-IDLOPNRM-RAD(INDX) TO W1-IDLOPNRM                           
101536     PERFORM IMS-GU-UPFA01                                                
101537     IF SEGMENT-FINNS                                                     
101538       IF UPPF-KVKVAPRIM > ZERO                                           
101539         IF UPPF-KDKVASTA-PRI = '2' OR '3'                                
101540           CONTINUE                                                       
101541         ELSE                                                             
101544           MOVE '605' TO MED-IDMFSFEL                                     
101546           CALL WMEDKONV USING MED-WMEDAREA                               
101547           MOVE MFS-ALFA-FAELT-FEL                                        
101548                   TO MOD-IDLOPNRM-RAD-ATTR(INDX)                         
101549           MOVE NEJ                      TO INDATA-SW                     
101550         END-IF                                                           
101551       END-IF                                                             
101552       IF UPPF-KVKVASEK > ZERO                                            
101553         IF UPPF-KDKVASTA-PRI = '2' OR '3'                                
101554           CONTINUE                                                       
101555         ELSE                                                             
101558           MOVE '605' TO MED-IDMFSFEL                                     
101560           CALL WMEDKONV USING MED-WMEDAREA                               
101561           MOVE MFS-ALFA-FAELT-FEL                                        
101562                   TO MOD-IDLOPNRM-RAD-ATTR(INDX)                         
101563           MOVE NEJ                      TO INDATA-SW                     
101564         END-IF                                                           
101565       END-IF                                                             
101566     END-IF                                                               
101567                                                                          
101568     IF INDATA-OK                                                         
101569       PERFORM IMS-GU-UPFA01                                              
101570       IF SEGMENT-FINNS                                                   
101571         PERFORM IMS-GNP-UPFA11                                           
101572         PERFORM UNTIL SEGMENT-SAKNAS                                     
101573           IF RAPP-KDKVASTA-PRI = '2' OR '3'                              
101574             CONTINUE                                                     
101575           ELSE                                                           
101578             MOVE '605' TO MED-IDMFSFEL                                   
101579             CALL WMEDKONV USING MED-WMEDAREA                             
101581             MOVE MFS-ALFA-FAELT-FEL                                      
101582                   TO MOD-IDLOPNRM-RAD-ATTR(INDX)                         
101583             MOVE NEJ                      TO INDATA-SW                   
101584           END-IF                                                         
101585           PERFORM IMS-GNP-UPFA11                                         
101586         END-PERFORM                                                      
101587       END-IF                                                             
101588     END-IF                                                               
101589                                                                          
101590     IF INDATA-OK                                                         
101591       PERFORM IMS-GU-UPFA01                                              
101592       IF SEGMENT-FINNS                                                   
101593         PERFORM IMS-GNP-UPFA12                                           
101594         PERFORM UNTIL SEGMENT-SAKNAS                                     
101595           IF SPEC-KDKVASTA-PRI = '2' OR '3'                              
101596             CONTINUE                                                     
101597           ELSE                                                           
101600             MOVE '605' TO MED-IDMFSFEL                                   
101602             CALL WMEDKONV USING MED-WMEDAREA                             
101603             MOVE MFS-ALFA-FAELT-FEL                                      
101604                   TO MOD-IDLOPNRM-RAD-ATTR(INDX)                         
101605             MOVE NEJ                      TO INDATA-SW                   
101606           END-IF                                                         
101607           PERFORM IMS-GNP-UPFA12                                         
101608         END-PERFORM                                                      
101609       END-IF                                                             
101610     END-IF                                                               
101611     .                                                                    
101612     EJECT                                                                
101613                                                                          
101614 MFS-RENSA-FAELT-IN SECTION.                                              
101620                                                                          
101700     MOVE MFS-RENSA-FAELT      TO MOD-ADINLOMR-UPD                        
101800                                  MOD-ADLAGOMR-UPD                        
101900                                  MOD-FLPRIO-UPD                          
101910                                  MOD-ADLAGOMR                            
102000     MOVE +1 TO INDX                                                      
102100     PERFORM UNTIL INDX        >  MAX-RAD                                 
102200         MOVE MFS-RENSA-FAELT  TO MOD-IDLOPNRM-RAD (INDX)                 
102300                                  MOD-IDRADNR-RAD  (INDX)                 
102400         ADD +1                TO INDX                                    
102500     END-PERFORM                                                          
102600     .                                                                    
102700     EJECT                                                                
102800 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
102900                                                                          
103000     MOVE MFS-ROER-EJ-FAELT    TO MOD-ADINLOMR-UPD                        
103010     IF MID-ADLAGOMR-UPD = ALL '+'                                        
103100       MOVE MFS-RENSA-FAELT    TO MOD-ADLAGOMR-UPD                        
103101       CONTINUE                                                           
103110     ELSE                                                                 
103120       MOVE MID-ADLAGOMR-UPD   TO MOD-ADLAGOMR-UPD                        
103130     END-IF                                                               
103200     MOVE MFS-ROER-EJ-FAELT    TO MOD-FLPRIO-UPD                          
103300                                  MOD-ADLAGOMR                            
103310*                                 MOD-ADINLOMR-PRT                        
103400     MOVE +1                   TO INDX                                    
103500     PERFORM UNTIL INDX         >  MAX-RAD                                
103600         MOVE MFS-ROER-EJ-FAELT TO MOD-IDLOPNRM-RAD (INDX)                
103700                                   MOD-IDRADNR-RAD  (INDX)                
103800         ADD +1                 TO INDX                                   
103900     END-PERFORM                                                          
104000     .                                                                    
104100     EJECT                                                                
104200 MFS-FORM-ATTR SECTION.                                                   
104300                                                                          
104400     MOVE MFS-FORMATETS-ATTR   TO MOD-ADINLOMR-UPD-ATTR                   
104500                                  MOD-ADLAGOMR-UPD-ATTR                   
104600                                  MOD-FLPRIO-UPD-ATTR                     
104610                                  MOD-ADLAGOMR-ATTR                       
104700                                  MOD-ADINLOMR-PRT-ATTR                   
104800     MOVE +1                   TO INDX                                    
104900     PERFORM UNTIL INDX          >  MAX-RAD                               
105000         MOVE MFS-FORMATETS-ATTR TO MOD-IDLOPNRM-RAD-ATTR (INDX)          
105100         ADD +1                  TO INDX                                  
105200     END-PERFORM                                                          
105300     .                                                                    
105400     SKIP2                                                                
105500 MFS-LAES-IN-IGEN SECTION.                                                
105600                                                                          
105700     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADINLOMR-UPD-ATTR                  
105800                                   MOD-ADLAGOMR-UPD-ATTR                  
105900                                   MOD-ADLAGOMR-ATTR                      
106000                                   MOD-ADINLOMR-PRT-ATTR                  
106100     MOVE +1                    TO INDX                                   
106200     PERFORM UNTIL INDX         >  MAX-RAD                                
106300         MOVE MFS-ADD-LAES-IN-FAELT                                       
106400                                TO MOD-IDLOPNRM-RAD-ATTR (INDX)           
106500         ADD +1                 TO INDX                                   
106600     END-PERFORM                                                          
106700     .                                                                    
106800     EJECT                                                                
106900* --- IMS SEKTIONER ---                                                   
107000     SKIP3                                                                
107100 IMS-GET-MSG SECTION.                                                     
107200                                                                          
107300     MOVE '  QC' TO GODK-STATUSKODER                                      
107400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
107500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
107600     PERFORM IMS-STATUSKONTROLL                                           
107700     .                                                                    
107800     SKIP3                                                                
107900 IMS-INSERT-MSG SECTION.                                                  
108000                                                                          
108010     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
108020       MOVE '0' TO MFS-KDHUVOMR                                           
108300     END-IF                                                               
108400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
108500     MOVE SPACE TO GODK-STATUSKODER                                       
108600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
108700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
108800     PERFORM IMS-STATUSKONTROLL                                           
108900     .                                                                    
109000     EJECT                                                                
109100 IMS-ISRT-ALT-MSG-6191  SECTION.                                          
109200     MOVE SPACE TO GODK-STATUSKODER                                       
109300     CALL  CBLTDLI  USING ISRT ALT1-PCB P-TO-P-SW                         
109400     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
109500     PERFORM IMS-STATUSKONTROLL                                           
109600     .                                                                    
109700     SKIP3                                                                
109800 IMS-ISRT-ALT-MSG-6194  SECTION.                                          
109900     MOVE SPACE TO GODK-STATUSKODER                                       
110000     CALL  CBLTDLI  USING ISRT ALT2-PCB P-TO-P-SW                         
110100     MOVE ALT2-STATUS-CODE TO STATUS-WS                                   
110200     PERFORM IMS-STATUSKONTROLL                                           
110300     .                                                                    
110400     EJECT                                                                
110500 IMS-GU-INLA1-INLA11 SECTION.                                             
110600     STRING 'W6INLA11(W6D1BSEQ =' W-W6D1BSEQ-X ')'                        
110700          DELIMITED BY SIZE INTO SSA1                                     
110800     MOVE '  GE' TO GODK-STATUSKODER                                      
110900     CALL CBLTDLI USING GU INLA1-PCB DLI-IO-AREA1 SSA1                    
111000     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
111100     PERFORM IMS-STATUSKONTROLL                                           
111200     .                                                                    
111300     SKIP3                                                                
111400 IMS-GU-INLA1-INLA21 SECTION.                                             
111500     STRING 'W6INLA11*P(W6D1BSEQ =' W-W6D1BSEQ-X ')'                      
111600          DELIMITED BY SIZE INTO SSA1                                     
111700     STRING 'W6INLA21(IDRADNR  =' W-IDRADNR-X ')'                         
111800          DELIMITED BY SIZE INTO SSA2                                     
111900     MOVE '  GE' TO GODK-STATUSKODER                                      
112000     CALL CBLTDLI USING GU INLA1-PCB DLI-IO-AREA2 SSA1 SSA2               
112100     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
112200     PERFORM IMS-STATUSKONTROLL                                           
112300     .                                                                    
112400     SKIP3                                                                
112500 IMS-GHU-INLA1-INLA21 SECTION.                                            
112600     STRING 'W6INLA11(W6D1BSEQ =' W-W6D1BSEQ-X ')'                        
112700          DELIMITED BY SIZE INTO SSA1                                     
112800     STRING 'W6INLA21(IDRADNR  =' W-IDRADNR-X ')'                         
112900          DELIMITED BY SIZE INTO SSA2                                     
113000     MOVE '    ' TO GODK-STATUSKODER                                      
113100     CALL CBLTDLI USING GHU INLA1-PCB DLI-IO-AREA2 SSA1 SSA2              
113200     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
113300     PERFORM IMS-STATUSKONTROLL                                           
113400     .                                                                    
113500     EJECT                                                                
113600 IMS-REPL-INLA1-INLA21 SECTION.                                           
113700                                                                          
113800     MOVE '  ' TO GODK-STATUSKODER                                        
113900     CALL CBLTDLI USING REPL INLA1-PCB DLI-IO-AREA2                       
114000     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
114100     PERFORM IMS-STATUSKONTROLL                                           
114200     .                                                                    
114300     EJECT                                                                
114400 IMS-GNP-INLA1-INLA21 SECTION.                                            
114500     MOVE   'W6INLA21'         TO SSA1                                    
114600     MOVE '  GE' TO GODK-STATUSKODER                                      
114700     CALL CBLTDLI USING GNP INLA1-PCB DLI-IO-AREA2 SSA1                   
114800     MOVE INLA1-STATUS-CODE TO STATUS-WS                                  
114900     PERFORM IMS-STATUSKONTROLL                                           
115000     .                                                                    
115100     SKIP3                                                                
115110 IMS-GU-INLA2-INLA11 SECTION.                                             
115111     STRING 'W6INLA11(W6D1CSEQ =' W-W6D1CSEQ-X                            
115112                    '&IDDC     =' W-IDDC-X ')'                            
115113          DELIMITED BY SIZE INTO SSA1                                     
115116     MOVE '  GE' TO GODK-STATUSKODER                                      
115117     CALL CBLTDLI USING GU INLA2-PCB DLI-IO-AREA1 SSA1                    
115118     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
115119     PERFORM IMS-STATUSKONTROLL                                           
115120     .                                                                    
115121     SKIP2                                                                
115130 IMS-GN-INLA2-INLA11 SECTION.                                             
115140     STRING 'W6INLA11(W6D1CSEQ =' W-W6D1CSEQ-X ')'                        
115150          DELIMITED BY SIZE INTO SSA1                                     
115160     MOVE '  GE' TO GODK-STATUSKODER                                      
115170     CALL CBLTDLI USING GN INLA2-PCB DLI-IO-AREA1 SSA1                    
115180     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
115190     PERFORM IMS-STATUSKONTROLL                                           
115191     .                                                                    
115192     SKIP2                                                                
115200 IMS-GU-INLA2-INLA21 SECTION.                                             
115300     STRING 'W6INLA11(W6D1CSEQ =' W-W6D1CSEQ-X ')'                        
115400          DELIMITED BY SIZE INTO SSA1                                     
115500     STRING 'W6INLA21(IDLEVNRK =' W-IDLEVNR-KOLLI-X                       
115600                    '&IDOKOLLI =' W-IDOKOLLI-X ')'                        
115700          DELIMITED BY SIZE INTO SSA2                                     
115800     MOVE '  GE' TO GODK-STATUSKODER                                      
115900     CALL CBLTDLI USING GU INLA2-PCB DLI-IO-AREA2 SSA1 SSA2               
116000     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
116100     PERFORM IMS-STATUSKONTROLL                                           
116200     .                                                                    
116300     SKIP2                                                                
116310 IMS-GNP-INLA2-INLA21 SECTION.                                            
116340     STRING 'W6INLA21(IDLEVNRK =' W-IDLEVNR-KOLLI-X                       
116350                    '&IDOKOLLI =' W-IDOKOLLI-X ')'                        
116360          DELIMITED BY SIZE INTO SSA1                                     
116370     MOVE '  GE' TO GODK-STATUSKODER                                      
116380     CALL CBLTDLI USING GNP INLA2-PCB DLI-IO-AREA2 SSA1                   
116390     MOVE INLA2-STATUS-CODE TO STATUS-WS                                  
116391     PERFORM IMS-STATUSKONTROLL                                           
116392     .                                                                    
116393     EJECT                                                                
116400 IMS-GU-PLAA-PLAA11 SECTION.                                              
116500     STRING 'W6PLAA01(W6GXKEY  =' W-W6GXKEY-6005-X ')'                    
116600          DELIMITED BY SIZE INTO SSA1                                     
116700     STRING 'W6PLAA11(W6GXKEY  =' W-W6GXKEY-6006-X ')'                    
116800          DELIMITED BY SIZE INTO SSA2                                     
116900     MOVE '  GE' TO GODK-STATUSKODER                                      
117000     CALL CBLTDLI USING GU PLAA-PCB DLI-IO-AREA3 SSA1 SSA2                
117100     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
117200     PERFORM IMS-STATUSKONTROLL                                           
117300     .                                                                    
117400     SKIP3                                                                
117410 IMS-GU-LOPA-LOPA11 SECTION.                                              
117420     STRING 'W6LOPA01(W6GXKEY  =' W-W6GXKEY-6017-X ')'                    
117430          DELIMITED BY SIZE INTO SSA1                                     
117440     STRING 'W6LOPA11(KDSEGKEY =' W-W6GXKEY-6018-X ')'                    
117450          DELIMITED BY SIZE INTO SSA2                                     
117460     MOVE '  GE' TO GODK-STATUSKODER                                      
117470     CALL CBLTDLI USING GU LOPA-PCB DLI-IO-AREA3 SSA1 SSA2                
117480     MOVE LOPA-STATUS-CODE TO STATUS-WS                                   
117490     PERFORM IMS-STATUSKONTROLL                                           
117491     .                                                                    
117492     SKIP3                                                                
117500 IMS-GHU-LOPA-LOPA11 SECTION.                                             
117600     STRING 'W6LOPA01(W6GXKEY  =' W-W6GXKEY-6017-X ')'                    
117700          DELIMITED BY SIZE INTO SSA1                                     
117800     STRING 'W6LOPA11(KDSEGKEY =' W-W6GXKEY-6018-X ')'                    
117900          DELIMITED BY SIZE INTO SSA2                                     
118000     MOVE '  GE' TO GODK-STATUSKODER                                      
118100     CALL CBLTDLI USING GHU LOPA-PCB DLI-IO-AREA3 SSA1 SSA2               
118200     MOVE LOPA-STATUS-CODE TO STATUS-WS                                   
118300     PERFORM IMS-STATUSKONTROLL                                           
118400     .                                                                    
118500     SKIP3                                                                
118600 IMS-REPL-LOPA-LOPA11 SECTION.                                            
118700                                                                          
118800     MOVE '  ' TO GODK-STATUSKODER                                        
118900     CALL CBLTDLI USING REPL LOPA-PCB DLI-IO-AREA3                        
119000     MOVE LOPA-STATUS-CODE TO STATUS-WS                                   
119100     PERFORM IMS-STATUSKONTROLL                                           
119200     .                                                                    
119300     EJECT                                                                
119301                                                                          
119310 IMS-GU-UPFA01 SECTION.                                                   
119320     STRING 'W6UPFA01(IDLOPNRM =' W1-IDLOPNRM-X ')'                       
119330          DELIMITED BY SIZE INTO SSA1                                     
119340     MOVE '  GE' TO GODK-STATUSKODER                                      
119350     CALL CBLTDLI USING GU UPFA-PCB DLI-IO-AREA-UPFA01 SSA1               
119360     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
119370     PERFORM IMS-STATUSKONTROLL                                           
119380     .                                                                    
119390     SKIP3                                                                
119391                                                                          
119392 IMS-GNP-UPFA11 SECTION.                                                  
119393     STRING 'W6UPFA01(IDLOPNRM =' W1-IDLOPNRM-X ')'                       
119394          DELIMITED BY SIZE INTO SSA1                                     
119395     MOVE 'W6UPFA11 ' TO SSA2                                             
119396     MOVE '  GE' TO GODK-STATUSKODER                                      
119397     CALL CBLTDLI USING GNP UPFA-PCB DLI-IO-AREA-UPFA11 SSA1 SSA2         
119398     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
119399     PERFORM IMS-STATUSKONTROLL                                           
119400     .                                                                    
119401     SKIP3                                                                
119402                                                                          
119403 IMS-GNP-UPFA12 SECTION.                                                  
119404     STRING 'W6UPFA01(IDLOPNRM =' W1-IDLOPNRM-X ')'                       
119405          DELIMITED BY SIZE INTO SSA1                                     
119406     MOVE 'W6UPFA12 ' TO SSA2                                             
119407     MOVE '  GE' TO GODK-STATUSKODER                                      
119408     CALL CBLTDLI USING GNP UPFA-PCB DLI-IO-AREA-UPFA12 SSA1 SSA2         
119409     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
119410     PERFORM IMS-STATUSKONTROLL                                           
119411     .                                                                    
119412     SKIP3                                                                
119413                                                                          
119414 IMS-GU-WDB601    SECTION.                                                
119415     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
119416          DELIMITED BY SIZE INTO SSA1                                     
119417     MOVE '  GE' TO GODK-STATUSKODER                                      
119418     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
119419     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
119420     PERFORM IMS-STATUSKONTROLL                                           
119421     IF SEGMENT-SAKNAS                                                    
119422         MOVE SPACE TO DCS-KDDC                                           
119423     END-IF                                                               
119424     .                                                                    
119425                                                                          
119430 IMS-STATUSKONTROLL SECTION.                                              
119600     SET STATUS-IX TO 1                                                   
119700     SEARCH GODK-STATUS                                                   
119800       AT END                                                             
119900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
120000         DELIMITED BY SIZE INTO FELTEXT                                   
120100         CALL FELLOG                                                      
120200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
120300         CONTINUE                                                         
120400     END-SEARCH                                                           
120500     .                                                                    
