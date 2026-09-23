000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4130400.                                                
000400*AUTHOR.         CAO-VAN NGU.                                             
000500*DATE-WRITTEN.   90/05/28.                                                
000600*                                                                         
000700*                                                                         
000800*                                                                         
000900*    UPDATES    WDGX4471-WDGX4472  PRODUCTION TABLE                       
001000*               WDGX4537-WDGX3538  TRANSPORTS                             
001100*    INPUT      SORTED FILE W41303 FROM PROGRAM W41303                    
001200*    CHECK-POINT IS USED FOR SYNCHRNIZATION, NO RESTORE                   
001300*                                                                         
001400*                                                                         
001500     SKIP3                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700 INPUT-OUTPUT SECTION.                                                    
001800 FILE-CONTROL.                                                            
001900     SELECT W41303 ASSIGN TO W41304D0.                                    
002000*                                                                         
002100     EJECT                                                                
002200 DATA DIVISION.                                                           
002300*                                                                         
002400 FILE SECTION.                                                            
002500 FD  W41303 RECORDING F BLOCK 0 RECORDS.                                  
002600*       -COPY W41303  -PRE F0-                                            
002700     EJECT                                                                
002800 WORKING-STORAGE SECTION.                                                 
002801*    -COPY WY2000W1                                                       
002810     SKIP3                                                                
002900 77  IDPGM                       PIC X(08)   VALUE 'W4130400'.            
003000                                                                          
003100 77  JA                          PIC X       VALUE 'J'.                   
003200 77  NEJ                         PIC X       VALUE 'N'.                   
003300                                                                          
003400 77  WS-EOF-W41303               PIC X       VALUE 'N'.                   
003500     88 EOF-W41303                           VALUE 'J'.                   
003600                                                                          
003700 77  TIRFS-SW                    PIC X       VALUE 'N'.                   
003800     88 TIRFS-OK                             VALUE 'J'.                   
003900     88 TIRFS-NOT-OK                         VALUE 'N'.                   
004000     EJECT                                                                
004010*      --- VALID IDDC CODES                                               
004020*                                                                         
004030*01    -COPY WWDC99 -PRE SW-                                              
004030*01    -COPY WWDCKONS -PRE SW-                                            
004040       EJECT                                                              
004100 01  FILLER.                                                              
004200                                                                          
004300     05  WS-REF-IDPRC.                                                    
004400         10  WS-REF-IDPRCBAS       PIC X(3) VALUE SPACES.                 
004500         10  WS-REF-IDPRCVAR       PIC X(1) VALUE SPACE.                  
004600                                                                          
004700     05  WS-REF-IDTRP.                                                    
004800         10  WS-REF-IDTRPLOS       PIC X(3) VALUE SPACES.                 
004900         10  WS-REF-IDTRPVAR       PIC X(2) VALUE SPACES.                 
005000                                                                          
005100     05  WS-REF-IDGMTREF.                                                 
005200         10  WS-REF-IDDISTR        PIC S9(5) COMP-3 VALUE +0.             
005300         10  WS-REF-IDKUNDNR       PIC S9(7) COMP-3 VALUE +0.             
005400         10  WS-REF-IDKUNDRF       PIC X(10) VALUE SPACES.                
005500                                                                          
005600     05  WS-REF-KDPRCGRP           PIC X(5) VALUE SPACES.                 
005700     05  WS-REF-IDDC               PIC X(2).                              
005800     05  WS-IDDC-NUM               PIC 9.                                 
005900                                                                          
005910     05  WS-TILST-O              PIC S9(11) COMP-3 VALUE ZERO.            
005920                                                                          
006000 01  FILLER.                                                              
006100                                                                          
006200     05  WS-INDX1                PIC S9(9)  BINARY VALUE +0.              
006300     05  WS-INDX2                PIC S9(9)  BINARY VALUE +0.              
006400     05  WS-INDX5                PIC S9(9)  BINARY VALUE +0.              
006500     05  WS-INDX6                PIC S9(9)  BINARY VALUE +0.              
006600     05  WS-INDX7                PIC S9(9)  BINARY VALUE +0.              
006700     05  WS-INDXI                PIC S9(9)  BINARY VALUE +0.              
006800     05  WS-INDXJ                PIC S9(9)  BINARY VALUE +0.              
006900     05  WS-30                   PIC S9(9)  BINARY VALUE +30.             
007000     05  WS-20000                PIC S9(9)  BINARY VALUE +20000.          
007100                                                                          
007200     05  WS-DB-TIHH              PIC S9(3)  COMP-3 VALUE +0.              
007300     05  WS-DB-TIMM              PIC S9(3)  COMP-3 VALUE +0.              
007400                                                                          
007500     05  WS-QOT-TIHH             PIC S9(3)  COMP-3 VALUE +0.              
007600     05  WS-RST-TIMM             PIC S9(3)  COMP-3 VALUE +0.              
007700     05  WS-60                   PIC S9(3)  COMP-3 VALUE +60.             
007800     05  W-TIRFS-DAG             PIC  9(11) VALUE ZERO.                   
007900                                                                          
008000     05  WS-W-TIHHMM             PIC 9(3)V99 VALUE 0.                     
008100     05  FILLER REDEFINES WS-W-TIHHMM.                                    
008200         10  WS-TIHH             PIC 9(3).                                
008300         10  WS-TIMM             PIC 9(2).                                
008301                                                                          
008410 01  DC-IX                       PIC 9(2)  VALUE ZERO.                    
008500 01  FILLER                      PIC X(16) VALUE 'IDDC-TAB.'.             
008600                                                                          
008700 01  IDDC-TABELL.                                                         
008800     03 IDDC                            OCCURS 20.                        
008900       05 TAB-IDDC               PIC  X(2).                               
009000                                                                          
009010 01  FILLER                      PIC X(16) VALUE 'WS-IDPRC-RULE.'.        
009100 01  WS-IDPRC-RULES.                                                      
009200     05  WS-KVBEMAN-ORD-PRC      PIC S9(2)V9(1) COMP-3 VALUE +0.          
009300     05  WS-PRC-DB-ARBTID        PIC S9(5)V9(2) COMP-3 VALUE +0.          
009400     05  WS-PRC-TOT-ARBTID       PIC S9(5)V9(2) COMP-3 VALUE +0.          
009500     05  WS-PRD-TOT-ARBTID       PIC S9(5)V9(2) COMP-3 VALUE +0.          
009600                                                                          
009610 01  FILLER                      PIC X(16)  VALUE 'TAB-INDX.'.            
009630 01  TAB-IDORDER-INDX            PIC 9(9)   COMP-3 VALUE ZERO.            
009640                                                                          
009650 01  FILLER                      PIC X(16)  VALUE 'TAB-ARBTID'.           
009700 01  WS-TAB-ARBTID.                                                       
009800     05  TAB-KDPRCGRP OCCURS 20000 INDEXED BY WS-INDX3.                   
009900         10  WS-TAB-KDPRCGRP         PIC X(5) VALUE SPACES.               
010000         10  TAB-WAREHOUSES OCCURS 8.                                     
010100             15  WS-KDPRCGRP-ARBTID   PIC S9(6)V9(1) COMP-3               
010200                                              VALUE +0.                   
010201                                                                          
010210 01  FILLER                      PIC X(16)  VALUE 'TAB-ORDERS'.           
010300 01  WS-TAB-ORDERS.                                                       
010400     05  TAB-IDORDER OCCURS 20000 INDEXED BY WS-INDX4.                    
010500         10  WS-TAB-IDORDER          PIC S9(7) COMP-3 VALUE +0.           
010600     EJECT                                                                
010610 01  FILLER                      PIC X(16) VALUE 'WDGX4472-AREA.'.        
010700*       -COPY WDGX4472   -PRE XXWS-                                       
010800     EJECT                                                                
010810 01  FILLER                      PIC X(16) VALUE 'W41304-AREA.'.          
010900*       -COPY W41303  -PRE WS-                                            
011000     EJECT                                                                
011010 01  FILLER                      PIC X(16) VALUE 'WDGX4538-AREA.'.        
011100*       -COPY WDGX4538  -PRE XXWS-                                        
011200     EJECT                                                                
011300*    --- CALLED PROGRAMS                                                  
011400 01  GENERAL-SUBPROGRAM.                                                  
011500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011700     EJECT                                                                
011800*    --- IO-AREA TO/FROM DL/I                                             
011900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012000*                                                                         
012100 01  NYCKLAR-TILL-DLI.                                                    
012200     03  W-4447-WDGXKEY.                                                  
012300         07 W-4447-IDHTYP        PIC X(4)    VALUE '4447'.                
012400         07 W-4447-IDDC          PIC X(2).                                
012500         07 W-4447-LOW-VALUE     PIC X(24)   VALUE LOW-VALUE.             
012600                                                                          
012700     03  W-4447-WDGXKEY-MAX.                                              
012800         07 W-4447-IDHTYP-MAX    PIC X(4)    VALUE '4447'.                
012900         07 W-4447-IDDC-MAX      PIC X(2)    VALUE HIGH-VALUE.            
013000         07 W-4447-HIGH-VALUE    PIC X(24)   VALUE HIGH-VALUE.            
013100                                                                          
013200     03  W-4448-WDGXKEY.                                                  
013300         07 W-4448-IDPRC         PIC X(4)    VALUE SPACE.                 
013400         07 W-4448-LOW-VALUE     PIC X(1)    VALUE LOW-VALUE.             
013500                                                                          
013600     03  W-4471-WDGXKEY.                                                  
013700         07 W-4471-IDHTYP        PIC X(4)    VALUE '4471'.                
013800         07 W-4471-IDDC          PIC X(2).                                
013900         07 W-4471-IDPRC.                                                 
014000            11 W-4471-IDPRCBAS   PIC X(3)    VALUE SPACES.                
014100            11 W-4471-IDPRCVAR   PIC X(1)    VALUE SPACE.                 
014200         07 W-4447-LOW-VALUE     PIC X(20)   VALUE LOW-VALUE.             
014300                                                                          
014400     03  W-4472-KDSEGKEY         PIC X(1)    VALUE '1'.                   
014500                                                                          
014600     03  W-4537-WDGXKEY.                                                  
014700         07 W-4537-IDHTYP        PIC X(4)    VALUE '4537'.                
014800         07 W-4537-IDDC          PIC X(2).                                
014900         07 W-4537-IDTRPLOS      PIC X(3).                                
015000         07 W-4537-LOW-VALUE     PIC X(21)   VALUE LOW-VALUE.             
015100                                                                          
015200     03  W-4538-WDGXKEY.                                                  
015300         07 W-4538-IDTRPVAR      PIC X(2).                                
015400         07 W-4538-IDORDER       PIC S9(7)   COMP-3.                      
015500         07 W-4538-LOW-VALUE     PIC X(04)   VALUE LOW-VALUE.             
015600                                                                          
015700     03  W-XRST-CHK.                                                      
015800         07 W-XRST-LENGTH        PIC S9(9) COMP SYNC VALUE +32.           
015900         07 W-XRST-AREA          PIC X(32) VALUE SPACES.                  
016000         07 W-CHKP-LENGTH        PIC S9(9) COMP SYNC VALUE +32.           
016100         07 W-CHKP-AREA          PIC X(32) VALUE SPACES.                  
016200                                                                          
016300         07 W-INPT-CNTR          PIC S9(7) BINARY VALUE +0.               
016400     EJECT                                                                
016500                                                                          
016600 01  STATUS-WS                   PIC XX.                                  
016700     88  SEGMENT-OK                          VALUE '  '.                  
016800     88  SEGMENT-II                          VALUE 'II'.                  
016900     88  SEGMENT-GE                          VALUE 'GE'.                  
017000     88  SEGMENT-GB                          VALUE 'GB'.                  
017100     88  SEGMENT-XD                          VALUE 'XD'.                  
017200     SKIP2                                                                
017300 01  GODK-STATUSKODER.                                                    
017400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017500     SKIP3                                                                
017600 01  SSA1                        PIC X(128).                              
017700 01  SSA2                        PIC X(128).                              
017800     EJECT                                                                
017900                                                                          
018000*01  -COPY W0003                                                          
018100     EJECT                                                                
018200*    ---  DLI INPUT-OUTPUT AREA                                           
018300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
018400     SKIP3                                                                
018500 01  DLI-IO-AREA.                                                         
018600     03  IO-AREA                 PIC X(1000) VALUE SPACE.                 
018700     SKIP3                                                                
018800     EJECT                                                                
018900     03  WLXXKH01 REDEFINES IO-AREA.                                      
019000*        05  -COPY WDGX4447   -PRE XXKH-                                  
019100     EJECT                                                                
019200     03  WLXXKH11 REDEFINES IO-AREA.                                      
019300*        05  -COPY WDGX4448   -PRE XXKH-                                  
019400     EJECT                                                                
019500     03  WLXXKW01 REDEFINES IO-AREA.                                      
019600*        05  -COPY WDGX4471   -PRE XXKW-                                  
019700     EJECT                                                                
019800     03  WLXXKW11 REDEFINES IO-AREA.                                      
019900*        05  -COPY WDGX4472   -PRE XXKW-                                  
020000     EJECT                                                                
020100     03  WLXXKV01 REDEFINES IO-AREA.                                      
020200*        05  -COPY WDGX4537   -PRE XXKV-                                  
020300     EJECT                                                                
020400     03  WLXXKV11 REDEFINES IO-AREA.                                      
020500*        05  -COPY WDGX4538   -PRE XXKV-                                  
020600     EJECT                                                                
020700 LINKAGE SECTION.                                                         
020800                                                                          
020900*01  -COPY W0008      -PRE MSGE-                                          
021000     05  FILLER                  PIC X(1).                                
021100     EJECT                                                                
021200*01  -COPY W0008      -PRE XXKH-                                          
021300     05  FILLER                  PIC X(1).                                
021400     EJECT                                                                
021500*01  -COPY W0008      -PRE XXKW-                                          
021600     05  FILLER                  PIC X(1).                                
021700     EJECT                                                                
021800*01  -COPY W0008      -PRE XXKV-                                          
021900     05  FILLER                  PIC X(1).                                
022000     EJECT                                                                
022100                                                                          
022200 PROCEDURE DIVISION  USING  MSGE-PCB XXKH-PCB XXKW-PCB XXKV-PCB.          
022300                                                                          
022400 W41304 SECTION.                                                          
022500     ENTRY 'DLITCBL' USING  MSGE-PCB XXKH-PCB XXKW-PCB XXKV-PCB.          
022600     PERFORM A-INIT                                                       
022700     PERFORM B-DLET-WDGX4471-WDGX4537                                     
022800     PERFORM C-LOAD-WDGX4471-WDGX4537                                     
022900     PERFORM D-DFLT-WDGX4471-WDGX4537                                     
023000     PERFORM Z-CLOSE-FILES                                                
023100     MOVE +0 TO RETURN-CODE                                               
023200     GOBACK                                                               
023300     .                                                                    
023400     EJECT                                                                
023500                                                                          
023600 A-INIT SECTION.                                                          
023700     PERFORM IMS-RESTART                                                  
023800     OPEN INPUT W41303                                                    
023900     .                                                                    
024000      EJECT                                                               
024100 B-DLET-WDGX4471-WDGX4537 SECTION.                                        
024200                                                                          
024300     PERFORM IMS-GHN-XXKW-WDGX4471                                        
024400     PERFORM UNTIL (SEGMENT-GE) OR (SEGMENT-GB)                           
024500       PERFORM IMS-DLET-XXKW-WDGX4471                                     
024600       PERFORM IMS-GHN-XXKW-WDGX4471                                      
024700     END-PERFORM                                                          
024800     PERFORM IMS-CHECKPOINT                                               
024900                                                                          
025000     PERFORM IMS-GHN-XXKV-WDGX4537                                        
025100     PERFORM UNTIL SEGMENT-GB                                             
025200       PERFORM IMS-DLET-XXKV-WDGX4537                                     
025300       PERFORM IMS-GHN-XXKV-WDGX4537                                      
025400     END-PERFORM                                                          
025500     PERFORM IMS-CHECKPOINT                                               
025600     .                                                                    
025700      EJECT                                                               
025800 C-LOAD-WDGX4471-WDGX4537 SECTION.                                        
025900                                                                          
026000     MOVE +0 TO WS-INDX5                                                  
026100     PERFORM CA-PRCTID-WDGX4448                                           
026200     MOVE +0 TO WS-INDX1 WS-INDX2                                         
026300     PERFORM S00-READ-W41303                                              
026400                                                                          
026500     PERFORM UNTIL (EOF-W41303)                                           
026600       MOVE WS-IDDC TO WS-REF-IDDC                                        
026700       PERFORM S10-INIT-IDORDER-TAB                                       
026800                                                                          
026900       PERFORM UNTIL (WS-IDDC NOT = WS-REF-IDDC)                          
027000                  OR (EOF-W41303)                                         
027100         PERFORM CC-KDPRCGRP-ARBTID                                       
027200         MOVE +0 TO WS-PRD-TOT-ARBTID                                     
027300         MOVE WS-KDPRCGRP TO WS-REF-KDPRCGRP                              
027400                                                                          
027500         PERFORM UNTIL (WS-KDPRCGRP NOT = WS-REF-KDPRCGRP)                
027600                    OR (WS-IDDC     NOT = WS-REF-IDDC)                    
027700                    OR (EOF-W41303)                                       
027800            PERFORM S80-ISRT-WDGX4471                                     
027900            PERFORM S60-INIT-WDGX4472                                     
028000            MOVE WS-IDPRC TO WS-REF-IDPRC                                 
028100                                                                          
028200            PERFORM UNTIL (WS-IDPRC    NOT = WS-REF-IDPRC)                
028300                      OR  (WS-KDPRCGRP NOT = WS-REF-KDPRCGRP)             
028400                      OR  (WS-IDDC     NOT = WS-REF-IDDC)                 
028500                      OR  (EOF-W41303)                                    
028600               PERFORM CX-ISRT-WDGX4537                                   
028700               MOVE WS-IDTRP TO WS-REF-IDTRP                              
028800                                                                          
028900               PERFORM UNTIL (WS-IDTRPLOS NOT = WS-REF-IDTRPLOS)          
029000                         OR  (WS-IDPRC    NOT = WS-REF-IDPRC)             
029100                         OR  (WS-KDPRCGRP NOT = WS-REF-KDPRCGRP)          
029200                         OR  (WS-IDDC     NOT = WS-REF-IDDC)              
029300                         OR  (EOF-W41303)                                 
029400                  MOVE WS-IDDISTR   TO WS-REF-IDDISTR                     
029500                  MOVE WS-IDKUNDNR  TO WS-REF-IDKUNDNR                    
029600                  MOVE WS-IDKUNDRF  TO WS-REF-IDKUNDRF                    
029700                  PERFORM CB-INIT-WDGX4538                                
029800                                                                          
029900                  PERFORM UNTIL                                           
030000                             (WS-IDGMTREF NOT = WS-REF-IDGMTREF)          
030100                         OR  (WS-IDTRPLOS NOT = WS-REF-IDTRPLOS)          
030200                         OR  (WS-IDPRC    NOT = WS-REF-IDPRC)             
030300                         OR  (WS-KDPRCGRP NOT = WS-REF-KDPRCGRP)          
030400                         OR  (WS-IDDC     NOT = WS-REF-IDDC)              
030500                         OR  (EOF-W41303)                                 
030600                     PERFORM CW-CREATE-4472-4538                          
030700                     PERFORM S00-READ-W41303                              
030800                  END-PERFORM                                             
030900                                                                          
031000               END-PERFORM                                                
031100            END-PERFORM                                                   
031200            PERFORM CY-ISRT-WDGX4472                                      
031300                                                                          
031400         END-PERFORM                                                      
031500       END-PERFORM                                                        
031600     END-PERFORM                                                          
031700     .                                                                    
031800     EJECT                                                                
031900 CA-PRCTID-WDGX4448 SECTION.                                              
032000                                                                          
032100     MOVE +0 TO WS-INDX1                                                  
032200     MOVE +1 TO DC-IX                                                     
032300     MOVE SW-WC-CDC-SE TO W-4447-IDDC                                     
032400     MOVE HIGH-VALUE           TO W-4447-WDGXKEY-MAX                      
032500     MOVE 4447                 TO W-4447-IDHTYP-MAX                       
032600     PERFORM IMS-GU-XXKH-WDGX4447                                         
032700                                                                          
032800     PERFORM UNTIL SEGMENT-GE OR SEGMENT-GB                               
032900        MOVE XXKH-4447-IDDC   TO W-4447-IDDC                              
033000                                 TAB-IDDC (DC-IX)                         
033010                                 WS-IDDC                                  
033100        IF SEGMENT-OK                                                     
033200           PERFORM IMS-GNP-XXKH-WDGX4448                                  
033300        END-IF                                                            
033400        PERFORM UNTIL (SEGMENT-GE) OR (SEGMENT-GB)                        
033500           IF (XXKH-4448-KDPRODKL = 'B') OR                               
033600              (XXKH-4448-KDPRODKL = 'C')                                  
033700              PERFORM CAA-ADD-ARBTID                                      
033800           END-IF                                                         
033900           PERFORM IMS-GNP-XXKH-WDGX4448                                  
034000        END-PERFORM                                                       
034100        PERFORM IMS-GN-XXKH-WDGX4447-MIN-MAX                              
034200        ADD +1               TO DC-IX                                     
034300      END-PERFORM                                                         
034400     .                                                                    
034500     EJECT                                                                
034600 CAA-ADD-ARBTID SECTION.                                                  
034700                                                                          
034800     MOVE XXKH-4448-KVBEMAN-ORD                                           
034900                         TO  WS-KVBEMAN-ORD-PRC                           
035000     ADD  XXKH-4448-KVBEMAN-EXT                                           
035100                         TO  WS-KVBEMAN-ORD-PRC                           
035200     MULTIPLY XXKH-4448-KVARBTID                                          
035300                         BY  WS-KVBEMAN-ORD-PRC                           
035400                             GIVING WS-PRC-DB-ARBTID                      
035500     MOVE WS-IDDC (1:1)  TO WS-IDDC-NUM                                   
035600     SET WS-INDX3 TO 1                                                    
035700     SEARCH TAB-KDPRCGRP AT END PERFORM CAAA-MOVE-ARBTID                  
035800       WHEN WS-TAB-KDPRCGRP (WS-INDX3) = XXKH-4448-KDPRCGRP               
035900           ADD WS-PRC-DB-ARBTID TO                                        
036000               WS-KDPRCGRP-ARBTID (WS-INDX3, WS-IDDC-NUM)                 
036100     END-SEARCH                                                           
036200     .                                                                    
036300     EJECT                                                                
036400 CAAA-MOVE-ARBTID SECTION.                                                
036500                                                                          
036600      ADD +1 TO WS-INDX1                                                  
036700      IF WS-INDX1 > WS-20000                                              
036800           MOVE WS-20000 TO WS-INDX1                                      
036900      END-IF                                                              
037000      MOVE XXKH-4448-KDPRCGRP TO                                          
037100               WS-TAB-KDPRCGRP (WS-INDX1)                                 
037200      ADD WS-PRC-DB-ARBTID TO                                             
037300               WS-KDPRCGRP-ARBTID (WS-INDX1, WS-IDDC-NUM)                 
037400     .                                                                    
037500     EJECT                                                                
037600 CB-INIT-WDGX4538 SECTION.                                                
037700     MOVE LOW-VALUE   TO  XXWS-4538-LOW-VALUE                             
037800     MOVE WS-IDTRPVAR TO  XXWS-4538-IDTRPVAR                              
037900     MOVE WS-IDDISTR  TO  XXWS-4538-IDDISTR                               
038000     MOVE WS-IDKUNDNR TO  XXWS-4538-IDKUNDNR                              
038100     MOVE WS-IDKUNDRF TO  XXWS-4538-IDKUNDRF                              
038200     MOVE WS-TIRFS    TO  W-TIRFS-DAG                                     
038300     MOVE W-TIRFS-DAG TO  XXWS-4538-TIRFS                                 
038400     MOVE WS-TILST-O  TO  XXWS-4538-TILST-O                               
038500     MOVE WS-IDORDER  TO  XXWS-4538-IDORDER                               
038600     MOVE WS-TITRPAVT TO  XXWS-4538-TITRPAVT                              
038700     MOVE +0          TO  XXWS-4538-VLORDNTO                              
038800     MOVE +0          TO  XXWS-4538-VKORDNTO                              
038900     .                                                                    
039000     EJECT                                                                
039100 CC-KDPRCGRP-ARBTID SECTION.                                              
039200                                                                          
039210     MOVE WS-IDDC (1:1)  TO WS-IDDC-NUM                                   
039400     SET WS-INDX3 TO 1                                                    
039500     SEARCH TAB-KDPRCGRP AT END                                           
039600                   MOVE +0 TO WS-PRC-TOT-ARBTID                           
039700     WHEN WS-TAB-KDPRCGRP (WS-INDX3) = WS-KDPRCGRP                        
039800          MOVE WS-KDPRCGRP-ARBTID (WS-INDX3, WS-IDDC-NUM)                 
039900                                TO WS-PRC-TOT-ARBTID                      
040000     END-SEARCH                                                           
040100     .                                                                    
040200     EJECT                                                                
040300 CW-CREATE-4472-4538 SECTION.                                             
040400                                                                          
040500     IF WS-KDODELSTA = 'R' OR 'U'                                         
040600        PERFORM CWA-CREATE-WDGX4472                                       
040700        ADD +1 TO WS-INDX1                                                
040800        IF WS-INDX1 > WS-30                                               
040900           MOVE WS-30 TO WS-INDX1                                         
041000        END-IF                                                            
041100     END-IF                                                               
041200                                                                          
041300     MOVE +1 TO WS-INDX7                                                  
041400     PERFORM UNTIL (WS-INDX7 > WS-INDX5)                                  
041500                    OR                                                    
041600                   (WS-TAB-IDORDER (WS-INDX7) = WS-IDORDER)               
041700        ADD +1 TO WS-INDX7                                                
041800     END-PERFORM                                                          
041900     IF WS-INDX7 > WS-20000                                               
042000        MOVE WS-20000 TO WS-INDX7                                         
042100     END-IF                                                               
042200     IF WS-TAB-IDORDER (WS-INDX7) = WS-IDORDER                            
042300        PERFORM CWC-ADD-ORDER                                             
042400     ELSE                                                                 
042500        PERFORM CWB-NEW-ORDER                                             
042600     END-IF                                                               
042700     .                                                                    
042800     EJECT                                                                
042900 CWA-CREATE-WDGX4472 SECTION.                                             
043000                                                                          
043100     MOVE +1 TO WS-INDX6                                                  
043200     MOVE WS-TIRFS    TO  W-TIRFS-DAG                                     
043300     MOVE ZERO        TO  W-TIRFS-DAG (8:4)                               
043400     PERFORM UNTIL                                                        
043410         WS-INDX6                   > WS-30        OR                     
043500         XXWS-4472-TIRFS (WS-INDX6) = W-TIRFS-DAG  OR                     
043600         XXWS-4472-TIRFS (WS-INDX6) = ZERO                                
043800        ADD +1 TO WS-INDX6                                                
043900     END-PERFORM                                                          
044000     IF WS-INDX6 > WS-30                                                  
044100        MOVE 30 TO WS-INDX6                                               
044200     END-IF                                                               
044300     MOVE W-TIRFS-DAG TO  XXWS-4472-TIRFS (WS-INDX6)                      
044400     ADD  WS-KVRADER  TO  XXWS-4472-KVRADER (WS-INDX6)                    
044500     MOVE XXWS-4472-SUPTID (WS-INDX6) TO WS-W-TIHHMM                      
044600     MOVE WS-TIHH TO WS-DB-TIHH                                           
044700     MOVE WS-TIMM TO WS-DB-TIMM                                           
044800     MOVE WS-SUPTID TO WS-W-TIHHMM                                        
044900     ADD WS-TIHH TO WS-DB-TIHH                                            
045000     ADD WS-TIMM TO WS-DB-TIMM                                            
045100     DIVIDE WS-60 INTO WS-DB-TIMM GIVING WS-QOT-TIHH                      
045200     COMPUTE WS-RST-TIMM = WS-DB-TIMM - (WS-QOT-TIHH * 60)                
045300     ADD WS-QOT-TIHH TO WS-DB-TIHH                                        
045400     MOVE WS-RST-TIMM TO WS-TIMM                                          
045500     MOVE WS-DB-TIHH TO WS-TIHH                                           
045600     MOVE WS-W-TIHHMM TO XXWS-4472-SUPTID (WS-INDX6)                      
045700     .                                                                    
045800     EJECT                                                                
045900 CWB-NEW-ORDER SECTION.                                                   
046000                                                                          
046100     ADD +1 TO WS-INDX5                                                   
046200     IF WS-INDX5 > WS-20000                                               
046300        MOVE WS-20000 TO WS-INDX5                                         
046400     END-IF                                                               
046910     IF WS-PRD-TOT-ARBTID NOT > WS-PRC-TOT-ARBTID AND                     
046920        WS-PRC-TOT-ARBTID > ZERO                                          
046930        MOVE WS-IDORDER TO WS-TAB-IDORDER (WS-INDX5)                      
046940        PERFORM CWC-ADD-ORDER                                             
046950     END-IF                                                               
047000     .                                                                    
047100     EJECT                                                                
047200 CWC-ADD-ORDER SECTION.                                                   
047300                                                                          
047400     ADD  WS-VKORDNTO TO  XXWS-4538-VKORDNTO                              
047500     ADD  WS-VLORDNTO TO  XXWS-4538-VLORDNTO                              
047600     PERFORM S70-DAG-WDGX4472                                             
047700     PERFORM CWCA-ISRT-WDGX4538                                           
047800     .                                                                    
047900     EJECT                                                                
048000 CWCA-ISRT-WDGX4538 SECTION.                                              
048100                                                                          
048200     MOVE XXWS-4538-WDGX4538 TO WLXXKV11                                  
048300     PERFORM IMS-ISRT-XXKV-WDGX4538                                       
048400     IF SEGMENT-II                                                        
048500        MOVE XXWS-4538-IDTRPVAR TO W-4538-IDTRPVAR                        
048600        MOVE XXWS-4538-IDORDER  TO W-4538-IDORDER                         
048700        PERFORM IMS-GHU-XXKV-WDGX4538                                     
048800        ADD XXWS-4538-VKORDNTO TO XXKV-4538-VKORDNTO                      
048900        ADD XXWS-4538-VLORDNTO TO XXKV-4538-VLORDNTO                      
049000        PERFORM IMS-REPL-XXKV-WDGX4538                                    
049100     END-IF                                                               
049200     .                                                                    
049300     EJECT                                                                
049400 CX-ISRT-WDGX4537 SECTION.                                                
049500                                                                          
049600      MOVE WS-IDDC TO W-4537-IDDC                                         
049700      MOVE WS-IDTRPLOS TO W-4537-IDTRPLOS                                 
049800      MOVE W-4537-WDGXKEY TO WLXXKV01                                     
049900      PERFORM IMS-ISRT-XXKV-WDGX4537                                      
050000     .                                                                    
050100     EJECT                                                                
050200 CY-ISRT-WDGX4472 SECTION.                                                
050300                                                                          
050400     PERFORM IMS-GHU-XXKW-WDGX4472                                        
050500     IF SEGMENT-GE                                                        
050600         MOVE XXWS-4472-WDGX4472 TO WLXXKW11                              
050700         PERFORM IMS-ISRT-XXKW-WDGX4472                                   
050800      ELSE                                                                
050900         PERFORM CYA-REPL-WDGX4472                                        
051000     END-IF                                                               
051100     .                                                                    
051200     EJECT                                                                
051300 CYA-REPL-WDGX4472 SECTION.                                               
051400                                                                          
051500     ADD XXWS-4472-KVRADER-DAG TO                                         
051600                         XXKW-4472-KVRADER-DAG                            
051700     MOVE XXKW-4472-SUPTID-DAG TO WS-W-TIHHMM                             
051800     MOVE WS-TIHH              TO WS-DB-TIHH                              
051900     MOVE WS-TIMM              TO WS-DB-TIMM                              
052000     MOVE XXWS-4472-SUPTID-DAG TO WS-W-TIHHMM                             
052100     ADD  WS-TIHH              TO WS-DB-TIHH                              
052200     ADD  WS-TIMM              TO WS-DB-TIMM                              
052300     DIVIDE WS-60 INTO WS-DB-TIMM GIVING WS-QOT-TIHH                      
052400     COMPUTE WS-RST-TIMM = WS-DB-TIMM - (WS-QOT-TIHH * 60)                
052500     ADD  WS-QOT-TIHH          TO WS-DB-TIHH                              
052600     MOVE WS-RST-TIMM          TO WS-TIMM                                 
052700     MOVE WS-DB-TIHH           TO WS-TIHH                                 
052800     MOVE WS-W-TIHHMM          TO XXKW-4472-SUPTID-DAG                    
052900     MOVE +1                   TO WS-INDX1                                
053000                                  WS-INDX2                                
053100     PERFORM UNTIL WS-INDX1               > 30 OR                         
053200                   XXWS-4472-TIRFS(WS-INDX1) = ZERO                       
053300         MOVE NEJ                   TO TIRFS-SW                           
053400         PERFORM UNTIL WS-INDX2 > 30 OR                                   
053500                 TIRFS-OK                                                 
053600             IF XXWS-4472-TIRFS (WS-INDX1) =                              
053700                                   XXKW-4472-TIRFS(WS-INDX2) OR           
053800                WS-INDX2                       = 30                       
053900                MOVE XXKW-4472-SUPTID (WS-INDX2) TO WS-W-TIHHMM           
054000                MOVE WS-TIHH                     TO WS-DB-TIHH            
054100                MOVE WS-TIMM                     TO WS-DB-TIMM            
054200                MOVE XXWS-4472-SUPTID (WS-INDX1) TO WS-W-TIHHMM           
054300                ADD  WS-TIHH                     TO WS-DB-TIHH            
054400                ADD  WS-TIMM                     TO WS-DB-TIMM            
054500                DIVIDE WS-60 INTO WS-DB-TIMM GIVING WS-QOT-TIHH           
054600                COMPUTE WS-RST-TIMM = WS-DB-TIMM -                        
054700                                     (WS-QOT-TIHH * 60)                   
054800                ADD  WS-QOT-TIHH    TO WS-DB-TIHH                         
054900                MOVE WS-RST-TIMM    TO WS-TIMM                            
055000                MOVE WS-DB-TIHH     TO WS-TIHH                            
055100                MOVE WS-W-TIHHMM    TO XXKW-4472-SUPTID (WS-INDX2)        
055200                ADD XXWS-4472-KVRADER  (WS-INDX1) TO                      
055300                     XXKW-4472-KVRADER (WS-INDX2)                         
055400                MOVE JA                           TO TIRFS-SW             
055500             ELSE                                                         
055600                 IF XXKW-4472-TIRFS (WS-INDX2) = ZERO                     
055700                     MOVE XXWS-4472-TIRFS   (WS-INDX1) TO                 
055800                          XXKW-4472-TIRFS   (WS-INDX2)                    
055900                     MOVE XXWS-4472-SUPTID  (WS-INDX1) TO                 
056000                          XXKW-4472-SUPTID  (WS-INDX2)                    
056100                     MOVE XXWS-4472-KVRADER (WS-INDX1) TO                 
056200                          XXKW-4472-KVRADER (WS-INDX2)                    
056300                     MOVE JA   TO TIRFS-SW                                
056400                  ELSE                                                    
056500                     ADD +1    TO WS-INDX2                                
056600                 END-IF                                                   
056700             END-IF                                                       
056800         END-PERFORM                                                      
056900         ADD +1                TO WS-INDX1                                
057000     END-PERFORM                                                          
057100     PERFORM IMS-REPL-XXKW-WDGX4472                                       
057200     .                                                                    
057300     EJECT                                                                
057400 D-DFLT-WDGX4471-WDGX4537 SECTION.                                        
057500     PERFORM S60-INIT-WDGX4472                                            
057600     MOVE '9999' TO WS-IDPRC                                              
057700                                                                          
057800     MOVE 11 TO WS-IDDC                                                   
057900*    MOVE WS-IDDC        TO WS-IDDC-NUM                                   
058000                                                                          
058100     MOVE +1                 TO DC-IX                                     
058200     PERFORM UNTIL (DC-IX > +15) OR                                       
058300                   (TAB-IDDC (DC-IX) = +0)                                
058400       PERFORM S80-ISRT-WDGX4471                                          
058500       PERFORM IMS-GHU-XXKW-WDGX4472                                      
058600       IF SEGMENT-GE                                                      
058700         MOVE XXWS-4472-WDGX4472 TO XXKW-4472-WDGX4472                    
058800         PERFORM IMS-ISRT-XXKW-WDGX4472                                   
058900       END-IF                                                             
059000       ADD +1               TO DC-IX                                      
059100     END-PERFORM                                                          
059300     .                                                                    
059400     EJECT                                                                
059500 Z-CLOSE-FILES SECTION.                                                   
059600     CLOSE W41303                                                         
059700     .                                                                    
059800     EJECT                                                                
059900 S00-READ-W41303 SECTION.                                                 
060000                                                                          
060100      READ W41303 INTO WS-W41303-CTX AT END MOVE JA TO                    
060110      WS-EOF-W41303                                                       
060200      END-READ                                                            
060300                                                                          
060400     .                                                                    
060500     EJECT                                                                
060600 S10-INIT-IDORDER-TAB SECTION.                                            
060700                                                                          
060800     SET WS-INDX4              TO +1                                      
060900     PERFORM UNTIL WS-INDX4    >  20000                                   
061000        MOVE ZERO              TO WS-TAB-IDORDER (WS-INDX4)               
061100        SET WS-INDX4           UP BY +1                                   
061200     END-PERFORM                                                          
061300     MOVE ZERO TO WS-INDX5                                                
061310     SET TAB-IDORDER-INDX      TO WS-INDX4                                
061400     .                                                                    
061500     EJECT                                                                
061600 S60-INIT-WDGX4472 SECTION.                                               
061700                                                                          
061800     MOVE +1  TO WS-INDX1                                                 
061900     MOVE '1' TO  XXWS-4472-KDSEGKEY                                      
062000     MOVE +0  TO  XXWS-4472-KVRADER-DAG                                   
062100                  XXWS-4472-SUPTID-DAG                                    
062200     MOVE +1  TO  WS-INDXI                                                
062300     PERFORM UNTIL WS-INDXI > WS-30                                       
062400         MOVE +0  TO  XXWS-4472-TIRFS (WS-INDXI)                          
062500                      XXWS-4472-SUPTID (WS-INDXI)                         
062600                      XXWS-4472-KVRADER (WS-INDXI)                        
062700         MOVE +1  TO  WS-INDXJ                                            
062800         PERFORM UNTIL WS-INDXJ > 3                                       
062900            MOVE +0  TO                                                   
063000                 XXWS-4472-SUPTID-PRAPP (WS-INDXI, WS-INDXJ)              
063100                 XXWS-4472-KVRADER-PRAPP (WS-INDXI, WS-INDXJ)             
063200            IF WS-INDXJ = 1                                               
063300               MOVE '1' TO XXWS-4472-IDSHIFT (WS-INDXI, WS-INDXJ)         
063400            ELSE                                                          
063500               IF WS-INDXJ = 2                                            
063600                  MOVE '2'                                                
063700                        TO XXWS-4472-IDSHIFT (WS-INDXI, WS-INDXJ)         
063800               ELSE                                                       
063900                  IF WS-INDXJ = 3                                         
064000                     MOVE '3'                                             
064100                        TO XXWS-4472-IDSHIFT (WS-INDXI, WS-INDXJ)         
064200                  END-IF                                                  
064300               END-IF                                                     
064400            END-IF                                                        
064500            ADD +1 TO WS-INDXJ                                            
064600         END-PERFORM                                                      
064700         ADD +1 TO WS-INDXI                                               
064800     END-PERFORM                                                          
064900     .                                                                    
065000     EJECT                                                                
065100*                                                                         
065200 S70-DAG-WDGX4472 SECTION.                                                
065300                                                                          
065400     ADD  WS-KVRADER TO XXWS-4472-KVRADER-DAG                             
065500                                                                          
065600     MOVE XXWS-4472-SUPTID-DAG TO WS-W-TIHHMM                             
065700     MOVE WS-TIHH              TO WS-DB-TIHH                              
065800     MOVE WS-TIMM              TO WS-DB-TIMM                              
065900                                                                          
066000     MOVE WS-SUPTID            TO WS-W-TIHHMM                             
066100     ADD  WS-TIHH              TO WS-DB-TIHH                              
066200     ADD  WS-TIMM              TO WS-DB-TIMM                              
066300                                                                          
066400     DIVIDE WS-60 INTO WS-DB-TIMM GIVING WS-QOT-TIHH                      
066500     COMPUTE WS-RST-TIMM = WS-DB-TIMM - (WS-QOT-TIHH * 60)                
066600                                                                          
066700     ADD  WS-QOT-TIHH          TO WS-DB-TIHH                              
066800     MOVE WS-RST-TIMM          TO WS-TIMM                                 
066900     MOVE WS-DB-TIHH           TO WS-TIHH                                 
067000     MOVE WS-W-TIHHMM          TO XXWS-4472-SUPTID-DAG                    
067100                                                                          
067200     MOVE WS-PRD-TOT-ARBTID    TO WS-W-TIHHMM                             
067300     MOVE WS-TIHH              TO WS-DB-TIHH                              
067400     MOVE WS-TIMM              TO WS-DB-TIMM                              
067500                                                                          
067600     MOVE WS-SUPTID            TO WS-W-TIHHMM                             
067700     ADD  WS-TIHH              TO WS-DB-TIHH                              
067800     ADD  WS-TIMM              TO WS-DB-TIMM                              
067900                                                                          
068000     DIVIDE WS-60 INTO WS-DB-TIMM GIVING WS-QOT-TIHH                      
068100     COMPUTE WS-RST-TIMM = WS-DB-TIMM - (WS-QOT-TIHH * 60)                
068200                                                                          
068300     ADD  WS-QOT-TIHH          TO WS-DB-TIHH                              
068400     MOVE WS-RST-TIMM          TO WS-TIMM                                 
068500     MOVE WS-DB-TIHH           TO WS-TIHH                                 
068600     MOVE WS-W-TIHHMM          TO WS-PRD-TOT-ARBTID                       
068700     .                                                                    
068800     EJECT                                                                
068900 S80-ISRT-WDGX4471 SECTION.                                               
069000*                                                                         
069100     MOVE WS-IDPRC             TO W-4471-IDPRC                            
069310     MOVE WS-IDDC              TO W-4471-IDDC                             
069400     MOVE W-4471-WDGXKEY       TO WLXXKW01                                
069500     PERFORM IMS-ISRT-XXKW-WDGX4471                                       
069600     .                                                                    
069700     EJECT                                                                
069800*                                                                         
069900 IMS-RESTART SECTION.                                                     
070000                                                                          
070100     MOVE SPACES TO W-XRST-AREA                                           
070200     MOVE '  '   TO GODK-STATUSKODER                                      
070300     CALL CBLTDLI USING XRST MSGE-PCB                                     
070400                        W-XRST-LENGTH W-XRST-AREA                         
070500                        W-CHKP-LENGTH W-CHKP-AREA                         
070600     MOVE MSGE-STATUS-CODE TO STATUS-WS                                   
070700     PERFORM IMS-STATUSKONTROLL                                           
070800     .                                                                    
070900     EJECT                                                                
071000*                                                                         
071100 IMS-DLET-XXKW-WDGX4471 SECTION.                                          
071200                                                                          
071300     MOVE '    ' TO GODK-STATUSKODER                                      
071400     CALL CBLTDLI USING DLET  XXKW-PCB DLI-IO-AREA                        
071500     MOVE XXKW-STATUS-CODE TO STATUS-WS                                   
071600     PERFORM IMS-STATUSKONTROLL                                           
071700     .                                                                    
071800 IMS-DLET-XXKV-WDGX4537 SECTION.                                          
071900                                                                          
072000     MOVE '    ' TO GODK-STATUSKODER                                      
072100     CALL CBLTDLI USING DLET  XXKV-PCB DLI-IO-AREA                        
072200     MOVE XXKV-STATUS-CODE TO STATUS-WS                                   
072300     PERFORM IMS-STATUSKONTROLL                                           
072400     .                                                                    
072500     EJECT                                                                
072600                                                                          
072700 IMS-GHN-XXKV-WDGX4537 SECTION.                                           
072800                                                                          
072900     STRING 'WLXXKV01(IDHTYP   =' W-4537-IDHTYP ')'                       
073000     DELIMITED BY SIZE INTO SSA1                                          
073100     MOVE '  GB' TO GODK-STATUSKODER                                      
073200     CALL CBLTDLI USING GHN XXKV-PCB DLI-IO-AREA SSA1                     
073300     MOVE XXKV-STATUS-CODE TO STATUS-WS                                   
073400     PERFORM IMS-STATUSKONTROLL                                           
073500     .                                                                    
073600     EJECT                                                                
073700                                                                          
073800 IMS-GHN-XXKW-WDGX4471 SECTION.                                           
073900                                                                          
074000     STRING 'WLXXKW01(IDHTYP   =' W-4471-IDHTYP ')'                       
074100     DELIMITED BY SIZE INTO SSA1                                          
074200     MOVE '  GB' TO GODK-STATUSKODER                                      
074300     CALL CBLTDLI USING GHN XXKW-PCB DLI-IO-AREA SSA1                     
074400     MOVE XXKW-STATUS-CODE TO STATUS-WS                                   
074500     PERFORM IMS-STATUSKONTROLL                                           
074600     .                                                                    
074700     EJECT                                                                
074800                                                                          
074900 IMS-GHU-XXKW-WDGX4472 SECTION.                                           
075000                                                                          
075100     STRING 'WLXXKW01(WDGXKEY  =' W-4471-WDGXKEY ')'                      
075200          DELIMITED BY SIZE INTO SSA1                                     
075300     STRING 'WLXXKW11(KDSEGKEY =' W-4472-KDSEGKEY ')'                     
075400          DELIMITED BY SIZE INTO SSA2                                     
075500     MOVE '  GE' TO GODK-STATUSKODER                                      
075600     CALL CBLTDLI USING GHU XXKW-PCB DLI-IO-AREA SSA1 SSA2                
075700     MOVE XXKW-STATUS-CODE TO STATUS-WS                                   
075800     PERFORM IMS-STATUSKONTROLL                                           
075900     .                                                                    
076000*                                                                         
076100                                                                          
076200 IMS-GHU-XXKV-WDGX4538 SECTION.                                           
076300                                                                          
076400     STRING 'WLXXKV01(WDGXKEY  =' W-4537-WDGXKEY ')'                      
076500          DELIMITED BY SIZE INTO SSA1                                     
076600     STRING 'WLXXKV11(WDGXKEY  =' W-4538-WDGXKEY ')'                      
076700          DELIMITED BY SIZE INTO SSA2                                     
076800     MOVE '  GE' TO GODK-STATUSKODER                                      
076900     CALL CBLTDLI USING GHU XXKV-PCB DLI-IO-AREA SSA1 SSA2                
077000     MOVE XXKV-STATUS-CODE TO STATUS-WS                                   
077100     PERFORM IMS-STATUSKONTROLL                                           
077200     .                                                                    
077300     EJECT                                                                
077400*                                                                         
077500 IMS-GU-XXKH-WDGX4447 SECTION.                                            
077600*                                                                         
077700     STRING 'WLXXKH01(WDGXKEY  =' W-4447-WDGXKEY ')'                      
077800          DELIMITED BY SIZE INTO SSA1                                     
077900     MOVE '  GE' TO GODK-STATUSKODER                                      
078000     CALL CBLTDLI USING GU  XXKH-PCB DLI-IO-AREA SSA1                     
078100     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
078200     PERFORM IMS-STATUSKONTROLL                                           
078300     .                                                                    
078400     SKIP2                                                                
078500*                                                                         
078600 IMS-GN-XXKH-WDGX4447-MIN-MAX       SECTION.                              
078700*                                                                         
078800     STRING 'WLXXKH01(WDGXKEY  =' W-4447-WDGXKEY ')'                      
078900          DELIMITED BY SIZE INTO SSA1                                     
079000     MOVE '  GE' TO GODK-STATUSKODER                                      
079100     CALL CBLTDLI USING GN  XXKH-PCB DLI-IO-AREA SSA1                     
079200     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
079300     PERFORM IMS-STATUSKONTROLL                                           
079400     .                                                                    
079500     SKIP2                                                                
079600*                                                                         
079700 IMS-GNP-XXKH-WDGX4448 SECTION.                                           
079800*                                                                         
079900*    STRING 'WLXXKH01(WDGXKEY  =' W-4447-WDGXKEY ')'                      
080000*         DELIMITED BY SIZE INTO SSA1                                     
080100*    STRING 'WLXXKH11(WDGXKEY  =' W-4448-WDGXKEY ')'                      
080200*         DELIMITED BY SIZE INTO SSA2                                     
080300                                                                          
080400     MOVE 'WLXXKH11' TO SSA2                                              
080500     MOVE '  GEGB' TO GODK-STATUSKODER                                    
080600     CALL CBLTDLI USING GNP XXKH-PCB DLI-IO-AREA SSA2                     
080700     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
080800     PERFORM IMS-STATUSKONTROLL                                           
080900     .                                                                    
081000     EJECT                                                                
081100 IMS-ISRT-XXKW-WDGX4471 SECTION.                                          
081200                                                                          
081300     MOVE 'WLXXKW01' TO SSA1                                              
081400     MOVE '  II' TO GODK-STATUSKODER                                      
081500     CALL CBLTDLI USING ISRT  XXKW-PCB DLI-IO-AREA SSA1                   
081600     MOVE XXKW-STATUS-CODE TO STATUS-WS                                   
081700     PERFORM IMS-STATUSKONTROLL                                           
081800     IF SEGMENT-OK                                                        
081900       PERFORM IMS-CONTROL-CHECKPOINT                                     
082000     END-IF                                                               
082100     .                                                                    
082200 IMS-ISRT-XXKW-WDGX4472 SECTION.                                          
082300*                                                                         
082400     STRING 'WLXXKW01(WDGXKEY  =' W-4471-WDGXKEY ')'                      
082500          DELIMITED BY SIZE INTO SSA1                                     
082600     MOVE 'WLXXKW11' TO SSA2                                              
082700     MOVE '  II' TO GODK-STATUSKODER                                      
082800     CALL CBLTDLI USING ISRT  XXKW-PCB DLI-IO-AREA SSA1 SSA2              
082900     MOVE XXKW-STATUS-CODE TO STATUS-WS                                   
083000     PERFORM IMS-STATUSKONTROLL                                           
083100     IF SEGMENT-OK                                                        
083200       PERFORM IMS-CONTROL-CHECKPOINT                                     
083300     END-IF                                                               
083400     .                                                                    
083500*                                                                         
083600 IMS-ISRT-XXKV-WDGX4537 SECTION.                                          
083700*                                                                         
083800     MOVE 'WLXXKV01' TO SSA1                                              
083900     MOVE '  II' TO GODK-STATUSKODER                                      
084000     CALL CBLTDLI USING ISRT  XXKV-PCB DLI-IO-AREA SSA1                   
084100     MOVE XXKV-STATUS-CODE TO STATUS-WS                                   
084200     PERFORM IMS-STATUSKONTROLL                                           
084300     IF SEGMENT-OK                                                        
084400       PERFORM IMS-CONTROL-CHECKPOINT                                     
084500     END-IF                                                               
084600     .                                                                    
084700*                                                                         
084800 IMS-ISRT-XXKV-WDGX4538 SECTION.                                          
084900*                                                                         
085000     STRING 'WLXXKV01(WDGXKEY  =' W-4537-WDGXKEY ')'                      
085100          DELIMITED BY SIZE INTO SSA1                                     
085200     MOVE 'WLXXKV11' TO SSA2                                              
085300     MOVE '  II' TO GODK-STATUSKODER                                      
085400     CALL CBLTDLI USING ISRT  XXKV-PCB DLI-IO-AREA SSA1 SSA2              
085500     MOVE XXKV-STATUS-CODE TO STATUS-WS                                   
085600     PERFORM IMS-STATUSKONTROLL                                           
085700     IF SEGMENT-OK                                                        
085800       PERFORM IMS-CONTROL-CHECKPOINT                                     
085900     END-IF                                                               
086000     .                                                                    
086100*                                                                         
086200 IMS-REPL-XXKW-WDGX4472 SECTION.                                          
086300*                                                                         
086400     MOVE '    ' TO GODK-STATUSKODER                                      
086500     CALL CBLTDLI USING REPL  XXKW-PCB DLI-IO-AREA                        
086600     MOVE XXKW-STATUS-CODE TO STATUS-WS                                   
086700     PERFORM IMS-STATUSKONTROLL                                           
086800     PERFORM IMS-CONTROL-CHECKPOINT                                       
086900     .                                                                    
087000*                                                                         
087100 IMS-REPL-XXKV-WDGX4538 SECTION.                                          
087200*                                                                         
087300     MOVE '    ' TO GODK-STATUSKODER                                      
087400     CALL CBLTDLI USING REPL  XXKV-PCB DLI-IO-AREA                        
087500     MOVE XXKV-STATUS-CODE TO STATUS-WS                                   
087600     PERFORM IMS-STATUSKONTROLL                                           
087700     PERFORM IMS-CONTROL-CHECKPOINT                                       
087800     .                                                                    
087900     EJECT                                                                
088000*                                                                         
088100 IMS-CONTROL-CHECKPOINT SECTION.                                          
088200*                                                                         
088300     ADD +1 TO W-INPT-CNTR                                                
088400     IF W-INPT-CNTR > +100                                                
088500       PERFORM IMS-CHECKPOINT                                             
088600     END-IF                                                               
088700     .                                                                    
088800*                                                                         
088900 IMS-CHECKPOINT SECTION.                                                  
089000*                                                                         
089100      MOVE ZERO TO W-INPT-CNTR                                            
089200      MOVE IDPGM TO W-XRST-AREA                                           
089300      MOVE '  ' TO GODK-STATUSKODER                                       
089400      CALL CBLTDLI USING CHKP MSGE-PCB                                    
089500                         W-XRST-LENGTH W-XRST-AREA                        
089600                         W-CHKP-LENGTH W-CHKP-AREA                        
089700      MOVE MSGE-STATUS-CODE TO STATUS-WS                                  
089800      PERFORM IMS-STATUSKONTROLL                                          
089900     .                                                                    
090000     EJECT                                                                
090100*                                                                         
090200 IMS-STATUSKONTROLL SECTION.                                              
090300*                                                                         
090400     SET STATUS-IX TO 1                                                   
090500     SEARCH GODK-STATUS                                                   
090600       AT END CALL FELLOG                                                 
090700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
090800     END-SEARCH                                                           
090900     .                                                                    
090910     EJECT                                                                
091000*    -COPY WY2000P1                                                       
