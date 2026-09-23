000010                                                                          
000020******************************************************************        
000030*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0124      *        
000040******************************************************************        
000050                                                                          
000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4035700.                                                
000400 AUTHOR.         LUC FEYS.                                                
000500 DATE-WRITTEN.   90/03/20.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNCTION.                                                            
001000*        HERE A SHORT DESCRIPTION OF WHAT THE PROGAMS DOES                
001100*        SHOULD BE INSERTED. DESCRIBE ITS PURPOSE AND                     
001200*        WHICH DATABASES ARE PROCESSED.                                   
001300*                                                                         
001400*        THE PROGRAM IS AN UPDATING MPP                                   
001500*        PROGRAM UPDATES WLORQA (WDQ3)                                    
001600*                                                                         
001700*        PROGRAM READS   WLORQI (WDQ2)                                    
001800*                                                                         
001900*                                                                         
002000*    INDATA.                                                              
002100*        TRANSACTION: W4T357                                              
002200*        MID:         W4I35701                                            
002300*                                                                         
002400*   OUTDATA.                                                              
002500*        MOD:         W4O35701                                            
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100 WORKING-STORAGE SECTION.                                                 
003101*    -- CHECKED BY WY2000                                                 
003102     SKIP3                                                                
003103 77  IDPGM                       PIC X(08)   VALUE 'W4035700'.            
003104                                                                          
003105 77  YES                         PIC X       VALUE 'J'.                   
003106 77  NOO                         PIC X       VALUE 'N'.                   
003107 77  FELTEXT                     PIC X(24).                               
003108 77  WS-KDMATT                   PIC X(1).                                
003109     88 US-MATT                  VALUE 'U'.                               
003110                                                                          
003120*    --- INDEX FOR BROWSE LINES                                           
003130 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
003140 77  MAX-INDX                    PIC S9(4)  VALUE +12   COMP SYNC.        
003150                                                                          
003160 77  LANGUAGE-IX                 PIC S9(9)  VALUE +0    COMP SYNC.        
003170 77  MAX-MOD-LENGTH              PIC S9(4)  VALUE +1342 COMP SYNC.        
003180                                                                          
003190*    --- WORKFIELDS FOR ACTUAL KEYVALUES OF SCREENS                       
003200                                                                          
003300 01    WS-IDANSTNR.                                                       
003400   03  FILLER                    PIC X(3).                                
003500   03  WS-IDANSTNR-5             PIC X(5).                                
003600                                                                          
003700*      --- VALID IDDC CODES                                               
003800*                                                                         
003900*01    -COPY WWDC99                                                       
004000       EJECT                                                              
005000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005100     88  INDATA-OK                           VALUE 'J'.                   
005200     88  INDATA-WRONG                        VALUE 'N'.                   
005300                                                                          
005400 77  KEYS-SW                     PIC X       VALUE 'J'.                   
005500     88  KEYS-OK                             VALUE 'J'.                   
005600     88  KEYS-WRONG                          VALUE 'N'.                   
005700                                                                          
005800 77  ALLT-SW                     PIC X       VALUE 'J'.                   
005900     88  ALLT-OK                             VALUE 'J'.                   
006000                                                                          
006100 77  JOINED-ORDER-SW             PIC X       VALUE 'N'.                   
006200     88  JOINED-ORDER                        VALUE 'J'.                   
006300     88  NOT-JOINED-ORDER                    VALUE 'N'.                   
006400                                                                          
006500 77  IDPLKLST-SW                 PIC X       VALUE 'N'.                   
006600     88  IDPLKLST-OK                         VALUE 'J'.                   
006700     88  IDPLKLST-NOT-OK                     VALUE 'N'.                   
006800                                                                          
006900 77  PROD-INGANG-SW              PIC X       VALUE 'J'.                   
007000     88  PROD-INGANG                         VALUE 'J'.                   
007100                                                                          
007200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007300     88  OWN-MID                             VALUE '4357'.                
007400     88  GOOD-MID                            VALUE '4356' '4357'.         
007500     EJECT                                                                
007600*    --- SUBPROGRAM AND PARAMETER-AREAS                                   
007700 01  GENERAL-SUBPROGRAM.                                                  
007800     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
008200     EJECT                                                                
008300 01  FILLER                      PIC X(16)  VALUE 'WWOMVAND'.             
008400*    --- PARAMETRAR WWOMVAND                                              
008500*01 -COPY WWOMVAND                                                        
008600     EJECT                                                                
008700*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008800 01  FILLER                      PIC X(16) VALUE 'WMSGINIT-AREA'.         
008900*01 -COPY WMSGINIT                                                        
009000     EJECT                                                                
009100     EJECT                                                                
009200 01  FILLER                      PIC X(16) VALUE 'WMEDAREA-AREA'.         
009300*    --- PARAMETERS FOR  SUBPROGRAM WMEDKONV                              
009400*   -COPY WMEDAREA                                                        
009500     SKIP3                                                                
009600 01  MESSAGE-CODES.                                                       
009700     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
009800     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
009900     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
010000     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
010100     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
010200     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
010300     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010400     EJECT                                                                
010500*    --- AREAS FOR MFS AND SCREENHANDLING                                 
010600*                                                                         
010700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
010800     SKIP3                                                                
010900*01  MID -COPY W4I35701                                                   
011000     EJECT                                                                
011100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
011200     SKIP3                                                                
011300*01  -COPY WMSGAREA                                                       
011400     EJECT                                                                
011500*    03  MOD -COPY W4O35701   -RED MSG-AREA.                              
011600     EJECT                                                                
011700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
011800     SKIP3                                                                
011900*01  -COPY WMFSAREA                                                       
012000     EJECT                                                                
012100*    --- WORK-AREAS FOR IMS-SECTIONS                                      
012200*                                                                         
012300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012400     SKIP3                                                                
012500 01  KEYS-TILL-DLI.                                                       
012600*                                                                         
012700*    DIRECT KEY TO ORDERPARTSREGISTER                                     
012800*                                                                         
012900*    MIN-MAX KEY TO ORDERPARTSREGISTER                                    
013000*                                                                         
013100     03  W-WDQ301KY-MIN-X.                                                
013200            07 W-IDORDER-MIN     PIC S9(7)         COMP-3.                
013300            07 W-IDDC-MIN        PIC X(2).                                
013400         05 W-IDPRODNR-MIN-X.                                             
013500            07 W-IDPRODNR-MIN    PIC S9(7)         COMP-3.                
013600         05 W-IDPLKLST-MIN-X.                                             
013700            07 W-IDPLKLST-MIN    PIC S9(3)         COMP-3.                
013800                                                                          
013900     03  W-WDQ301KY-MAX-X.                                                
014000            07 W-IDORDER-MAX     PIC S9(7)         COMP-3.                
014100            07 W-IDDC-MAX        PIC X(2).                                
014200            07 W-IDPRODNR-MAX    PIC X(4)  VALUE HIGH-VALUE.              
014300            07 W-IDPLKLST-MAX    PIC X(2)  VALUE HIGH-VALUE.              
014400                                                                          
014500     03  W-IDORDER-X.                                                     
014600            07 W-D-IDORDER       PIC S9(7)         COMP-3.                
014700                                                                          
014800     03  W-IDPLKLST-X.                                                    
014900            07 W-IDPLKLST        PIC S9(3)         COMP-3.                
015000                                                                          
015100     03  W-IDDC-X.                                                        
015200            07 W-IDDC-WDQ2       PIC X(2).                                
015300                                                                          
015400     03  W-WDQ2CSEQ-X.                                                    
015500            07 W-IDDISTR-CSEQ    PIC S9(5)         COMP-3.                
015600            07 W-IDKUNDNR-CSEQ   PIC S9(7)         COMP-3.                
015700            07 W-IDKUNDRF-CSEQ   PIC X(10) VALUE SPACES  .                
015800            07 W-IDKUNDRF-FILLER REDEFINES W-IDKUNDRF-CSEQ.               
015900              09 W-IDORDNR7-CSEQ    PIC  9(7).                            
016000                                                                          
016100     03  W-WDQ3DSEQ-MIN-X.                                                
016200         05  W-Q3DSEQ-IDPRODNR-MIN   PIC S9(7) VALUE ZERO COMP-3.         
016300         05  W-Q3DSEQ-IDPLKLST-MIN   PIC S9(3) VALUE ZERO COMP-3.         
016400     03  W-WDQ3DSEQ-MAX-X.                                                
016500         05  W-Q3DSEQ-IDPRODNR-MAX   PIC S9(7) VALUE ZERO COMP-3.         
016600            05 W-IDPLKLST-MAX        PIC X(2)  VALUE HIGH-VALUE.          
016700                                                                          
016800     03  IDDISTR-WS              PIC 9(4).                                
016900     03  IDKUNDNR-WS             PIC 9(6).                                
017000     03  IDORDNR7-WS             PIC X(7).                                
017100     03  IDPRODNR-WS             PIC X(7).                                
017200     03  IDPRODNR-WS-NUM         PIC 9(7)    VALUE ZERO.                  
017210     03  W-TILST-OD              PIC  9(11).                              
017220     03  WS-ORQA-ODEL-TILST-OD   PIC  9(11).                              
017230     03  WS-ORQA-ODEL-IDDC       PIC  X(02).                              
017300     03  W-DARFS-X.                                                       
017400        05 W-DARFS               PIC 9(12).                               
017500     03  W-DARFS-X2 REDEFINES W-DARFS-X.                                  
017600        05 W-DARFS-DATUM         PIC 9(08).                               
017700        05 W-DARFS-KLOCKA        PIC 9(04).                               
017800     03  WS-TITRPAVG.                                                     
017900        05 WS-TIAAMMDD           PIC  9(6).                               
018000        05 WS-TIHHMM             PIC  9(4).                               
018300     03  W-TIHHMM                PIC  9(4).                               
018400*                                                                         
018500*    --- STATUS-CODE FROM IMS                                             
018600*                                                                         
018700 01  STATUS-WS                   PIC XX.                                  
018800     88  SEGMENT-FOUND                       VALUE '  '.                  
018900     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
019000     88  SEGMENT-MISSING                     VALUE 'GE'.                  
019100     88  END-OF-DATA                         VALUE 'GB'.                  
019200     SKIP2                                                                
019300 01  GOOD-STATUSCODES.                                                    
019400     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019500     SKIP3                                                                
019600 01  SSA1                        PIC X(90).                               
019700 01  SSA2                        PIC X(90).                               
019800 01  DATUM-TIME.                                                          
019900     03 JJ-MM-DD                 PIC 9(6).                                
020000     03 HH-MM                    PIC 9(4)  VALUE 0.                       
020100     EJECT                                                                
020200*    --- IMS FUNCTIONCODES                                                
020300*01  -COPY W0003                                                          
020400     EJECT                                                                
020500*    ---  DLI INPUT-OUTPUT AREA                                           
020600 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA1'.          
020700     SKIP3                                                                
020800 01  DLI-IO-AREA1.                                                        
020900     03  IO-AREA1                PIC X(256)  VALUE SPACE.                 
021000     SKIP3                                                                
021100     03  WLORQA01 REDEFINES IO-AREA1.                                     
021200*        05  -COPY WDQ301     -PRE ORQA-                                  
021300 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA2'.          
021400     SKIP3                                                                
021500 01  DLI-IO-AREA2.                                                        
021600     03  IO-AREA2                PIC X(4064)  VALUE SPACE.                
021700     SKIP3                                                                
021800     03  WLORQI01 REDEFINES IO-AREA2.                                     
021900*        05  -COPY WDQ201                                                 
022000     SKIP3                                                                
022100     03  WLORQI12 REDEFINES IO-AREA2.                                     
022200*        05  -COPY WDQ212                                                 
022300     EJECT                                                                
022400 LINKAGE SECTION.                                                         
022500                                                                          
022600*01  -COPY W0009      -PRE MSG-                                           
022700     EJECT                                                                
022800*01  -COPY W0008      -PRE USEA-                                          
022900     05  FILLER                  PIC X.                                   
023000     EJECT                                                                
023100*01  -COPY W0008      -PRE WDQ2-                                          
023200     05  FILLER                  PIC X.                                   
023300     EJECT                                                                
023400*01  -COPY W0008      -PRE ORQA-                                          
023500     05  FILLER                  PIC X.                                   
023600     EJECT                                                                
023700*01  -COPY W0008      -PRE ORQAD-                                         
023800     05  FILLER                  PIC X.                                   
023900     EJECT                                                                
024000 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB WDQ2-PCB                      
024100                           ORQA-PCB ORQAD-PCB.                            
024200     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDQ2-PCB                      
024300                           ORQA-PCB ORQAD-PCB.                            
024400                                                                          
024500     PERFORM IMS-GET-MSG                                                  
024600     IF SEGMENT-FOUND                                                     
024700       PERFORM A-INIT                                                     
024800       PERFORM B-CONTROL-KEYS                                             
024900       IF KEYS-OK                                                         
025000          IF MFS-UPDATE                                                   
025100            PERFORM G-CONTROL-INPUT                                       
025200            IF INDATA-OK                                                  
025300              PERFORM H-UPDATE                                            
025400            END-IF                                                        
025500          ELSE                                                            
025600            IF MFS-FIRST                                                  
025700              PERFORM C-FIRST-PAGE                                        
025800            ELSE                                                          
025900              IF MFS-NEXT                                                 
026000                PERFORM D-NEXT-PAGE                                       
026100              ELSE                                                        
026200                PERFORM E-SAME-PAGE                                       
026300              END-IF                                                      
026400            END-IF                                                        
026500          END-IF                                                          
026600          IF ALLT-OK AND INDATA-OK                                        
026700              PERFORM F-READ-SHOW-INFO                                    
026800          END-IF                                                          
026900       ELSE                                                               
027000         MOVE '401' TO MED-IDMFSFEL                                       
027100         CALL WMEDKONV USING MED-WMEDAREA                                 
027200         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
027300       END-IF                                                             
027400                                                                          
027500       MOVE MAX-MOD-LENGTH TO MSG-KVLL                                    
027600       PERFORM IMS-INSERT-MSG                                             
027700     END-IF                                                               
027800                                                                          
027900     MOVE ZERO TO RETURN-CODE                                             
028000     GOBACK                                                               
028100     .                                                                    
028200     EJECT                                                                
028300 A-INIT SECTION.                                                          
028400                                                                          
028500     ACCEPT JJ-MM-DD FROM DATE.                                           
028600     IF MSG-DOUBLE-TRANSACTIONS                                           
028700       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I35701                 
028800       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
028900       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
029000     ELSE                                                                 
029100       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I35701                  
029200       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
029300       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
029400     END-IF                                                               
029500                                                                          
029600     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
029700     MOVE MSG-IDPFK TO MFS-IDPFK                                          
029800     MOVE MFS-IDTRANS TO W-IDTRANS                                        
029900                                                                          
030000     MOVE LOW-VALUE TO MSG-AREA                                           
030100     MOVE 'W4O357N1' TO MFS-IDMOD                                         
030200     MOVE '4357' TO MOD-IDTRANS                                           
030300     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
030400                                                                          
030500     IF NOT OWN-MID                                                       
030600       MOVE SPACE              TO MFS-KDTRTYP                             
030700       MOVE '7'                TO MFS-IDPFK                               
030800     END-IF                                                               
031700     MOVE LOW-VALUES           TO W-WDQ301KY-MIN-X                        
031800     MOVE HIGH-VALUES          TO W-WDQ301KY-MAX-X                        
031900                                                                          
032000     MOVE 'N'                  TO MOD-FLJANEJ                             
032100     MOVE NOO                  TO PROD-INGANG-SW                          
032200     .                                                                    
032300     EJECT                                                                
032400 B-CONTROL-KEYS SECTION.                                                  
032500                                                                          
032600     MOVE ALL '+'           TO MSGI-WMSGINIT                              
032700     MOVE '001'             TO MSGI-KDCALL                                
032800     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
032900     MOVE '4357'            TO MSGI-IDTRANS                               
033000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
033100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
033200     MOVE MSGI-KDMATT       TO WS-KDMATT                                  
033210                                                                          
033220     IF MSGI-IDLAND-SPR = 'GB'                                            
033230       MOVE +2                 TO LANGUAGE-IX                             
033240       MOVE 'GB '              TO MED-IDSKYLT                             
033250     ELSE                                                                 
033260       MOVE +1                 TO LANGUAGE-IX                             
033270       MOVE 'S  '              TO MED-IDSKYLT                             
033280     END-IF                                                               
033300                                                                          
033400     MOVE YES TO KEYS-SW                                                  
033500     PERFORM MFS-ERASE-FIELD-IN                                           
033600                                                                          
033700     IF MID-IDDISTR-IN  = ALL '+'                                         
033800        MOVE MID-IDDISTR-OUT TO IDDISTR-WS                                
033900        INSPECT IDDISTR-WS REPLACING LEADING SPACE BY ZERO                
034000     ELSE                                                                 
034100        MOVE MID-IDDISTR-IN TO IDDISTR-WS                                 
034200        MOVE '7'            TO MFS-IDPFK                                  
034300        MOVE SPACE          TO MFS-KDTRTYP                                
034400     END-IF                                                               
034500                                                                          
034600     IF MID-IDKUNDNR-IN = ALL '+'                                         
034700        MOVE MID-IDKUNDNR-OUT TO IDKUNDNR-WS                              
034800        INSPECT IDKUNDNR-WS REPLACING LEADING SPACE BY ZERO               
034900     ELSE                                                                 
035000        MOVE MID-IDKUNDNR-IN TO IDKUNDNR-WS                               
035100        MOVE '7'            TO MFS-IDPFK                                  
035200        MOVE SPACE          TO MFS-KDTRTYP                                
035300     END-IF                                                               
035400                                                                          
035500     IF MID-IDORDNR7-IN = ALL '+'                                         
035600        MOVE MID-IDORDNR7-OUT TO IDORDNR7-WS                              
035700        INSPECT IDORDNR7-WS REPLACING LEADING SPACE BY ZERO               
035800     ELSE                                                                 
035900        MOVE MID-IDORDNR7-IN TO IDORDNR7-WS                               
036000        MOVE '7'            TO MFS-IDPFK                                  
036100        MOVE SPACE          TO MFS-KDTRTYP                                
036200     END-IF                                                               
036300                                                                          
036400     IF MID-IDPRODNR-IN = ALL '+'                                         
036500        MOVE ZERO             TO IDPRODNR-WS                              
036600        INSPECT IDPRODNR-WS REPLACING LEADING SPACE BY ZERO               
036700        MOVE NOO               TO PROD-INGANG-SW                          
036800     ELSE                                                                 
036900        MOVE MID-IDPRODNR-IN TO IDPRODNR-WS                               
037000        MOVE '7'               TO MFS-IDPFK                               
037100        MOVE SPACE             TO MFS-KDTRTYP                             
037200        MOVE YES               TO PROD-INGANG-SW                          
037300     END-IF                                                               
037400                                                                          
037500     IF IDPRODNR-WS NUMERIC                                               
037600       IF IDPRODNR-WS > ZERO                                              
037700         MOVE IDPRODNR-WS     TO IDPRODNR-WS-NUM                          
037800         MOVE IDPRODNR-WS-NUM TO W-Q3DSEQ-IDPRODNR-MIN                    
037900                                 W-Q3DSEQ-IDPRODNR-MAX                    
038000                                 W-IDPRODNR-MIN                           
038100                                 W-IDPRODNR-MAX                           
038200       END-IF                                                             
038300     ELSE                                                                 
038400       MOVE ZERO TO IDPRODNR-WS                                           
038500       MOVE NOO TO PROD-INGANG-SW                                         
038600     END-IF                                                               
038700                                                                          
038800     IF MID-IDDC-IN NOT = ALL '+' AND OWN-MID                             
038900       MOVE MID-IDDC-IN                   TO WS-IDDC                      
039000       MOVE '7'                           TO MFS-IDPFK                    
039100       MOVE SPACE                         TO MFS-KDTRTYP                  
039200     ELSE                                                                 
039300       MOVE MSGI-IDDC                     TO WS-IDDC                      
039400     END-IF                                                               
039500                                                                          
039600     IF WS-IDDC IS > SPACE                                                
039700       MOVE WS-IDDC              TO W-IDDC-MAX                            
039800                                    W-IDDC-MIN                            
039900                                    W-IDDC-WDQ2                           
040000     ELSE                                                                 
040100       MOVE NOO TO KEYS-SW                                                
040200     END-IF                                                               
040300                                                                          
040400     MOVE WS-IDDC     TO MOD-IDDC-OUT                                     
040500                                                                          
040600     IF IDDISTR-WS NOT NUMERIC                                            
040700     OR IDKUNDNR-WS NOT NUMERIC                                           
040800     OR IDORDNR7-WS NOT NUMERIC                                           
040900       MOVE NOO TO KEYS-SW                                                
041000     ELSE                                                                 
041100                                                                          
041200       MOVE IDDISTR-WS  TO W-IDDISTR-CSEQ                                 
041300                           MOD-IDDISTR-OUT                                
041400       MOVE IDKUNDNR-WS TO W-IDKUNDNR-CSEQ                                
041500                           MOD-IDKUNDNR-OUT                               
041600       MOVE IDORDNR7-WS TO W-IDORDNR7-CSEQ                                
041700                           MOD-IDORDNR7-OUT                               
041800       MOVE IDPRODNR-WS TO MOD-IDPRODNR-OUT                               
041900       INSPECT MOD-IDDISTR-OUT  REPLACING LEADING ZERO BY SPACE           
042000       INSPECT MOD-IDKUNDNR-OUT REPLACING LEADING ZERO BY SPACE           
042100       INSPECT MOD-IDORDNR7-OUT REPLACING LEADING ZERO BY SPACE           
042200       INSPECT MOD-IDPRODNR-OUT REPLACING LEADING ZERO BY SPACE           
042300     END-IF                                                               
042400     .                                                                    
042500     EJECT                                                                
042600 C-FIRST-PAGE SECTION.                                                    
042700                                                                          
042800     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
042900     CALL WMEDKONV USING MED-WMEDAREA                                     
043000     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
043100                                                                          
043200*    --- BLANK/ZERO OUT BROWSE-KEY                                        
043300     MOVE YES TO ALLT-SW                                                  
043400     PERFORM MFS-ERASE-FIELD-IN                                           
043500     .                                                                    
043600     EJECT                                                                
043700 D-NEXT-PAGE SECTION.                                                     
043800                                                                          
043900     MOVE MID-IDORDER-NEXT  TO  W-IDORDER-MIN                             
044000     MOVE WS-IDDC           TO W-IDDC-MIN      W-IDDC-MAX                 
044100     MOVE MID-IDPRODNR-NEXT TO W-IDPRODNR-MIN                             
044200     MOVE MID-IDPLKLST-NEXT TO W-IDPLKLST-MIN                             
044300     MOVE YES TO ALLT-SW                                                  
044400     .                                                                    
044500     EJECT                                                                
044600 E-SAME-PAGE SECTION.                                                     
044700                                                                          
044800     IF MID-IDPLKLST-UPD  = ALL '+' AND                                   
044900        MID-TIAAMMDD-UPD  = ALL '+' AND                                   
045000        MID-TIHHMM-UPD    = ALL '+' AND                                   
045100        MID-FLJANEJ       =     '+'                                       
045200                                                                          
045300        MOVE WS-IDDC            TO W-IDDC-MIN                             
045400        MOVE MID-IDORDER-ENTER  TO W-IDORDER-MIN                          
045500        MOVE MID-IDPRODNR-ENTER TO W-IDPRODNR-MIN                         
045600                                   IDPRODNR-WS                            
045700        MOVE MID-IDPLKLST-ENTER TO W-IDPLKLST-MIN                         
045800        MOVE YES TO ALLT-SW                                               
045900     ELSE                                                                 
046000        MOVE NOO TO ALLT-SW                                               
046100        MOVE INF-PRESS-PF11 TO MED-IDMFSFEL                               
046200        CALL WMEDKONV USING MED-WMEDAREA                                  
046300        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
046400        PERFORM MFS-DO-NOT-TOUCH-FIELD-IN                                 
046500        PERFORM MFS-DO-NOT-TOUCH-FIELD-OUT                                
046600        PERFORM MFS-READ-IN-OWN                                           
046700     END-IF                                                               
046800     .                                                                    
046900     EJECT                                                                
047000 F-READ-SHOW-INFO SECTION.                                                
047100                                                                          
047200     PERFORM FA-READ-BASICDATA                                            
047300     IF SEGMENT-MISSING                                                   
047400        IF MFS-NEXT                                                       
047500            MOVE '115'         TO MED-MFSFEL                              
047600         ELSE                                                             
047700            MOVE '701'         TO MED-MFSFEL                              
047800        END-IF                                                            
047900        IF PROD-INGANG                                                    
048000          MOVE MFS-RENSA-FAELT TO MOD-IDORDNR7-OUT                        
048100                                  MOD-IDDISTR-OUT                         
048200                                  MOD-IDKUNDNR-OUT                        
048300        ELSE                                                              
048400          MOVE MFS-RENSA-FAELT TO MOD-IDPRODNR-OUT                        
048500        END-IF                                                            
048600        CALL WMEDKONV USING MED-WMEDAREA                                  
048700        MOVE MED-MFSFEL        TO MOD-TEMFSFEL                            
048800        PERFORM MFS-ERASE-FIELD-OUT                                       
048900     ELSE                                                                 
049000        IF JOINED-ORDER                                                   
049100            CONTINUE                                                      
049200         ELSE                                                             
049300            IF MFS-FIRST                                                  
049400               MOVE ORQA-ODEL-IDORDER TO MOD-IDORDER-FIRST                
049500               MOVE ORQA-ODEL-IDPRODNR TO MOD-IDPRODNR-FIRST              
049600               MOVE ORQA-ODEL-IDPLKLST TO MOD-IDPLKLST-FIRST              
049700            END-IF                                                        
049800                                                                          
049900            MOVE ORQA-ODEL-DATRPAVD (3:6) TO MOD-TIAAMMDD                 
050000                                          IN MOD-TITRPAVG                 
050100            MOVE ORQA-ODEL-TIHHMM         TO W-TIHHMM                     
050200            COMPUTE MOD-TIHHMM            IN MOD-TITRPAVG                 
050300                                          =    W-TIHHMM / 100             
050400                                                                          
050500            MOVE ORQA-ODEL-IDTRP  TO MOD-IDTRP-IN                         
050600            MOVE ORQA-ODEL-IDORDER TO MOD-IDORDER-ENTER                   
050700            MOVE ORQA-ODEL-IDPLKLST TO MOD-IDPLKLST-ENTER                 
050800            MOVE ORQA-ODEL-IDPRODNR TO MOD-IDPRODNR-ENTER                 
050900                                       MOD-IDPRODNR-OUT                   
051000                                       IDPRODNR-WS                        
051100                                       IDPRODNR-WS-NUM                    
051200                                       W-Q3DSEQ-IDPRODNR-MIN              
051300                                       W-Q3DSEQ-IDPRODNR-MAX              
051400                                       W-IDPRODNR-MIN                     
051500                                       W-IDPRODNR-MAX                     
051600         INSPECT MOD-IDPRODNR-OUT REPLACING LEADING ZERO BY SPACE         
051700        END-IF                                                            
051800     END-IF                                                               
051900     MOVE +1 TO INDX                                                      
052000                                                                          
052100     PERFORM UNTIL INDX > MAX-INDX                                        
052200       IF SEGMENT-FOUND AND NOT-JOINED-ORDER                              
052300         MOVE ORQA-ODEL-IDPLKLST     TO MOD-IDPLKLST   (INDX)             
052400         MOVE ORQA-ODEL-IDPRC        TO MOD-IDPRC      (INDX)             
052500         MOVE ORQA-ODEL-KDODELSTA    TO MOD-KDODELSTA  (INDX)             
052600         MOVE ORQA-ODEL-KVRADER      TO MOD-KVRADER    (INDX)             
052700         MOVE ORQA-ODEL-KVPACKRAD-OD TO MOD-KVPACKRAD-OD (INDX)           
052800         IF US-MATT                                                       
052900            COMPUTE MOD-VKORDNTO (INDX) = ORQA-ODEL-VKORDNTO              
053000                                 * CONV-KG-TO-LB                          
053100            COMPUTE MOD-VLORDNTO (INDX) = ORQA-ODEL-VLORDNTO              
053200                                 * CONV-M3-TO-FT3                         
053300         ELSE                                                             
053400           MOVE ORQA-ODEL-VKORDNTO   TO MOD-VKORDNTO  (INDX)              
053500           MOVE ORQA-ODEL-VLORDNTO   TO MOD-VLORDNTO  (INDX)              
053600         END-IF                                                           
053700         IF ORQA-ODEL-KDODELSTA = 'U' OR 'P' OR 'F' OR 'L'                
053800           MOVE ORQA-ODEL-IDUSER     TO WS-IDANSTNR                       
053900           MOVE WS-IDANSTNR-5        TO MOD-IDANSTNR  (INDX)              
054000           INSPECT MOD-IDANSTNR (INDX)  REPLACING                         
054100                   LEADING ZERO BY SPACE                                  
054200         END-IF                                                           
054500                                                                          
054810         MOVE ORQA-ODEL-TILST-OD     TO W-TILST-OD                        
054830                                                                          
054840         MOVE W-TILST-OD (2:6)       TO MOD-TIAAMMDD                      
054850                                     IN MOD-TILST-OD (INDX)               
054860         MOVE W-TILST-OD (8:4)       TO W-TIHHMM                          
054900         COMPUTE MOD-TIHHMM          IN MOD-TILST-OD  (INDX)              
055000                                     = W-TIHHMM / 100                     
055100         MOVE ORQA-ODEL-SUPTID       TO MOD-SUPTID    (INDX)              
055200                                                                          
055600         MOVE ORQA-ODEL-DARFS (3:6)  TO MOD-TIAAMMDD                      
055700                                     IN MOD-TIRFS-OUT (INDX)              
055800         MOVE ORQA-ODEL-DARFS (9:4)  TO W-TIHHMM                          
055900         COMPUTE MOD-TIHHMM          IN MOD-TIRFS-OUT (INDX)              
056000                                     = W-TIHHMM / 100                     
056200         PERFORM FB-READ-LINEDATA                                         
056300       ELSE                                                               
056400         MOVE MFS-ERASE-FIELD TO MOD-IDPLKLST      (INDX)                 
056500                                 MOD-IDPRC         (INDX)                 
056600                                 MOD-KDODELSTA     (INDX)                 
056700                                 MOD-KVRADER       (INDX)                 
056800                                 MOD-KVPACKRAD-OD  (INDX)                 
056900                                 MOD-VKORDNTO      (INDX)                 
057000                                 MOD-VLORDNTO      (INDX)                 
057100                                 MOD-TILST-OD      (INDX)                 
057200                                 MOD-SUPTID        (INDX)                 
057300                                 MOD-TIRFS-OUT     (INDX)                 
057400                                 MOD-IDANSTNR      (INDX)                 
057500       END-IF                                                             
057600       ADD 1 TO INDX                                                      
057700     END-PERFORM                                                          
057800                                                                          
057900     IF SEGMENT-FOUND AND NOT-JOINED-ORDER                                
058000       MOVE ORQA-ODEL-IDORDER      TO MOD-IDORDER-NEXT                    
058100       MOVE ORQA-ODEL-IDPRODNR     TO MOD-IDPRODNR-NEXT                   
058200       MOVE ORQA-ODEL-IDPLKLST     TO MOD-IDPLKLST-NEXT                   
058300       MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                          
058400       CALL WMEDKONV USING MED-WMEDAREA                                   
058500       MOVE MED-TEMFSINF TO MOD-TEMFSINF                                  
058600     ELSE                                                                 
058700       MOVE 999999                 TO MOD-IDORDER-NEXT                    
058800       MOVE 9999999                TO MOD-IDPRODNR-NEXT                   
058900       MOVE 999                    TO MOD-IDPLKLST-NEXT                   
059000     END-IF                                                               
059100                                                                          
059200     PERFORM MFS-ERASE-FIELD-IN                                           
059300     .                                                                    
059400     EJECT                                                                
059500 FA-READ-BASICDATA SECTION.                                               
059600*                                                                         
059700*    READ ORDERHEAD FOR KDFRAKT AND GET KEY IDORDER FOR WDQ3              
059800*                                                                         
059900                                                                          
060000     IF PROD-INGANG                                                       
060100       PERFORM IMS-GU-ORQA01-M-Q3DSEQ                                     
060200       IF SEGMENT-FOUND                                                   
060300         MOVE ORQA-ODEL-IDDISTR  TO IDDISTR-WS                            
060400                                    W-IDDISTR-CSEQ                        
060500         MOVE IDDISTR-WS         TO MOD-IDDISTR-OUT                       
060600         MOVE ORQA-ODEL-IDKUNDNR TO IDKUNDNR-WS                           
060700                                    W-IDKUNDNR-CSEQ                       
060800         MOVE IDKUNDNR-WS        TO MOD-IDKUNDNR-OUT                      
060900         MOVE ORQA-ODEL-IDORDNR7 TO IDORDNR7-WS                           
061000                                    W-IDORDNR7-CSEQ                       
061100                                    MOD-IDORDNR7-OUT                      
061200         INSPECT MOD-IDDISTR-OUT  REPLACING LEADING ZERO BY SPACE         
061300         INSPECT MOD-IDKUNDNR-OUT REPLACING LEADING ZERO BY SPACE         
061400         INSPECT MOD-IDORDNR7-OUT REPLACING LEADING ZERO BY SPACE         
061500       END-IF                                                             
061600     END-IF                                                               
061700                                                                          
061800     IF SEGMENT-FOUND OR MFS-UPDATE                                       
061900       PERFORM IMS-GU-CSEQ-WDQ2                                           
062000                                                                          
062100       MOVE NOO                  TO JOINED-ORDER-SW                       
062200       IF SEGMENT-FOUND                                                   
062300                                                                          
062400         MOVE OHUV-IDORDER  TO W-IDORDER-MIN                              
062500                               W-IDORDER-MAX                              
062600         PERFORM IMS-GNP-WDQ2                                             
062700         IF SEGMENT-FOUND                                                 
062800            MOVE ARB-KDFRAKT TO MOD-KDFRAKT                               
062900            IF PROD-INGANG                                                
063000              CONTINUE                                                    
063100            ELSE                                                          
063200              PERFORM IMS-GU-ORQA-WDQ3-KVAL                               
063300            END-IF                                                        
063400         END-IF                                                           
063500       END-IF                                                             
063600     END-IF                                                               
063700     .                                                                    
063800     EJECT                                                                
063900 FB-READ-LINEDATA SECTION.                                                
064000                                                                          
064100     IF PROD-INGANG                                                       
064200       PERFORM IMS-GN-ORQA01-M-Q3DSEQ                                     
064300     ELSE                                                                 
064400       PERFORM IMS-GN-ORQA-WDQ3                                           
064500     END-IF                                                               
064600     .                                                                    
064700     EJECT                                                                
064800 G-CONTROL-INPUT SECTION.                                                 
064900                                                                          
065000     MOVE YES                  TO INDATA-SW                               
065100     IF MID-FLJANEJ            = '+'     AND                              
065200        MID-TIAAMMDD-UPD       = ALL '+' AND                              
065300        MID-TIHHMM-UPD         = ALL '+' AND                              
065400        MID-IDPLKLST-UPD       = ALL '+'                                  
065500         MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                        
065600         MOVE NOO              TO INDATA-SW                               
065700      ELSE                                                                
065800         PERFORM GA-CONTROL-FLJANEJ                                       
065900                                                                          
066000         PERFORM IMS-GU-CSEQ-WDQ2                                         
066100         IF SEGMENT-FOUND                                                 
066200             MOVE OHUV-IDORDER TO W-IDORDER-MIN                           
066300                                  W-IDORDER-MAX                           
066400                                  W-D-IDORDER                             
066500          ELSE                                                            
066600             MOVE NOO          TO INDATA-SW                               
066700         END-IF                                                           
066800                                                                          
066900         IF MID-FLJANEJ                  = 'J'                            
067000             PERFORM GB-CONTROL-UPDATE-ALL                                
067100          ELSE                                                            
067200             PERFORM GC-CONTROL-UPDATE-ONE-PLKLST                         
067300        END-IF                                                            
067400     END-IF                                                               
067500                                                                          
067600     IF INDATA-WRONG                                                      
067700        PERFORM MFS-DO-NOT-TOUCH-FIELD-IN                                 
067800        PERFORM MFS-DO-NOT-TOUCH-FIELD-OUT                                
067900        CALL WMEDKONV USING MED-WMEDAREA                                  
068000        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
068100     END-IF                                                               
068200     .                                                                    
068300     EJECT                                                                
068400 GA-CONTROL-FLJANEJ   SECTION.                                            
068500                                                                          
068600     IF MID-FLJANEJ                 = 'N' OR                              
068700        MID-FLJANEJ                 = 'J'                                 
068800        MOVE MFS-ALPHA-FIELD-OK     TO MOD-FLJANEJ-ATTR                   
068900     ELSE                                                                 
069000         MOVE MFS-ALPHA-FIELD-WRONG TO MOD-FLJANEJ-ATTR                   
069100         MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                       
069200         MOVE NOO                   TO INDATA-SW                          
069300     END-IF                                                               
069400     .                                                                    
069500     EJECT                                                                
069600 GB-CONTROL-UPDATE-ALL  SECTION.                                          
069700                                                                          
069800     MOVE LOW-VALUE                  TO W-IDPLKLST-X                      
069900     IF MID-IDPLKLST-UPD             = ALL '+'   AND                      
070000        MID-TIAAMMDD-UPD             NUMERIC     AND                      
070100        MID-TIHHMM-UPD               NUMERIC                              
070200         MOVE MFS-NUM-FIELD-OK       TO MOD-IDPLKLST-ATTR                 
070300                                        MOD-TIAAMMDD-ATTR                 
070400                                        MOD-TIHHMM-ATTR                   
070500         MOVE MID-TIAAMMDD-UPD       TO WS-TIAAMMDD                       
070600         MOVE MID-TIHHMM-UPD         TO WS-TIHHMM                         
070700         MOVE ZERO                   TO W-DARFS                           
070800         MOVE WS-TIAAMMDD            TO W-DARFS-DATUM                     
070810         IF WS-TIAAMMDD NOT = ZERO                                        
070820           IF WS-TIAAMMDD < 500000                                        
070830             MOVE 20                 TO W-DARFS-DATUM (1:2)               
070840           ELSE                                                           
070850             IF WS-TIAAMMDD < 999999                                      
070860               MOVE 19               TO W-DARFS-DATUM (1:2)               
070870             ELSE                                                         
070880               MOVE 99999999         TO W-DARFS-DATUM                     
070890             END-IF                                                       
070891           END-IF                                                         
070892         END-IF                                                           
070900         MOVE WS-TIHHMM              TO W-DARFS-KLOCKA                    
071000                                                                          
071100         PERFORM GBA-CONTROL-ALL-ORDERPARTS                               
071200      ELSE                                                                
071300         IF MID-TIAAMMDD-UPD          NUMERIC                             
071400             MOVE MFS-NUM-FIELD-OK    TO MOD-TIAAMMDD-ATTR                
071500          ELSE                                                            
071600             MOVE MFS-NUM-FIELD-WRONG TO MOD-TIAAMMDD-ATTR                
071700         END-IF                                                           
071800                                                                          
071900         IF MID-TIHHMM-UPD            NUMERIC                             
072000             MOVE MFS-NUM-FIELD-OK    TO MOD-TIHHMM-ATTR                  
072100          ELSE                                                            
072200             MOVE MFS-NUM-FIELD-WRONG TO MOD-TIHHMM-ATTR                  
072300         END-IF                                                           
072400                                                                          
072500         IF MID-IDPLKLST-UPD          = ALL '+'                           
072600             MOVE MFS-NUM-FIELD-OK    TO MOD-IDPLKLST-ATTR                
072700          ELSE                                                            
072800             MOVE MFS-NUM-FIELD-WRONG TO MOD-IDPLKLST-ATTR                
072900         END-IF                                                           
073000                                                                          
073100         MOVE ERR-CORR-HILITE-FLDS   TO MED-IDMFSFEL                      
073200         MOVE NOO                    TO INDATA-SW                         
073300     END-IF                                                               
073400     .                                                                    
073500     EJECT                                                                
073600 GBA-CONTROL-ALL-ORDERPARTS  SECTION.                                     
073700                                                                          
073800                                                                          
073900     PERFORM IMS-GU-ORQA-WDQ3                                             
074000     PERFORM UNTIL SEGMENT-MISSING OR END-OF-DATA OR                      
074100                   INDATA-WRONG                                           
074500        IF ORQA-ODEL-KDODELSTA = 'R' AND                                  
074600           ORQA-ODEL-DARFS     > W-DARFS                                  
074700            CONTINUE                                                      
074800         ELSE                                                             
074900            IF ORQA-ODEL-KDODELSTA = 'R'                                  
075000                MOVE MFS-NUM-FIELD-WRONG  TO MOD-TIAAMMDD-ATTR            
075100                                             MOD-TIHHMM-ATTR              
075200                MOVE '007'                TO MED-IDMFSFEL                 
075300             ELSE                                                         
075400                MOVE MFS-NUM-FIELD-OK     TO MOD-TIAAMMDD-ATTR            
075500                                             MOD-TIHHMM-ATTR              
075600                MOVE '124'                TO MED-IDMFSFEL                 
075700            END-IF                                                        
075800            MOVE NOO           TO INDATA-SW                               
075900        END-IF                                                            
076000        PERFORM IMS-GN-ORQA-WDQ3                                          
076100     END-PERFORM                                                          
076200     .                                                                    
076300     EJECT                                                                
076400 GC-CONTROL-UPDATE-ONE-PLKLST      SECTION.                               
076500                                                                          
076600     IF MID-IDPLKLST-UPD             NUMERIC AND                          
076700        MID-TIAAMMDD-UPD             NUMERIC AND                          
076800        MID-TIHHMM-UPD               NUMERIC                              
076900                                                                          
077000        MOVE MID-TIAAMMDD-UPD       TO WS-TIAAMMDD                        
077100        MOVE MID-TIHHMM-UPD         TO WS-TIHHMM                          
077200        MOVE ZERO                   TO W-DARFS                            
077300        MOVE WS-TIAAMMDD            TO W-DARFS-DATUM                      
077310        IF WS-TIAAMMDD NOT = ZERO                                         
077320          IF WS-TIAAMMDD < 500000                                         
077330            MOVE 20                 TO W-DARFS-DATUM (1:2)                
077340          ELSE                                                            
077350            IF WS-TIAAMMDD < 999999                                       
077360              MOVE 19               TO W-DARFS-DATUM (1:2)                
077370            ELSE                                                          
077371              MOVE 99999999         TO W-DARFS-DATUM                      
077372            END-IF                                                        
077373          END-IF                                                          
077374        END-IF                                                            
077400        MOVE WS-TIHHMM              TO W-DARFS-KLOCKA                     
077500                                                                          
077600        MOVE MID-IDPLKLST-UPD       TO W-IDPLKLST                         
077700        PERFORM GCA-CHECK-IDPLKLST                                        
077800        IF IDPLKLST-NOT-OK                                                
077900            MOVE MFS-NUM-FIELD-WRONG  TO MOD-IDPLKLST-ATTR                
078000            MOVE MFS-NUM-FIELD-OK     TO MOD-TIAAMMDD-ATTR                
078100                                         MOD-TIHHMM-ATTR                  
078200            MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                     
078300            MOVE NOO                  TO INDATA-SW                        
078400        ELSE                                                              
078500            PERFORM IMS-GHU-ORQA-WDQ3-KVAL                                
078900            IF ORQA-ODEL-KDODELSTA  = 'R'     AND                         
079000               ORQA-ODEL-DARFS      > W-DARFS                             
079100                MOVE YES            TO INDATA-SW                          
079200                MOVE MFS-NUM-FIELD-OK TO MOD-TIAAMMDD-UPD                 
079300                                      MOD-TIHHMM-UPD                      
079400            ELSE                                                          
079500             IF ORQA-ODEL-KDODELSTA = 'R'                                 
079600                 MOVE MFS-NUM-FIELD-WRONG TO MOD-TIAAMMDD-ATTR            
079700                                             MOD-TIHHMM-ATTR              
079800                 MOVE '007'            TO MED-IDMFSFEL                    
079900             ELSE                                                         
080000                 MOVE MFS-NUM-FIELD-OK TO MOD-TIAAMMDD-ATTR               
080100                                          MOD-TIHHMM-ATTR                 
080200                 MOVE '124'            TO MED-IDMFSFEL                    
080300             END-IF                                                       
080400             MOVE NOO              TO INDATA-SW                           
080500            END-IF                                                        
080600            MOVE MFS-NUM-FIELD-OK   TO MOD-IDPLKLST-ATTR                  
080700        END-IF                                                            
080800      ELSE                                                                
080900         IF MID-TIAAMMDD-UPD          NUMERIC                             
081000             MOVE MFS-NUM-FIELD-OK    TO MOD-TIAAMMDD-ATTR                
081100          ELSE                                                            
081200             MOVE MFS-NUM-FIELD-WRONG TO MOD-TIAAMMDD-ATTR                
081300         END-IF                                                           
081400                                                                          
081500         IF MID-TIHHMM-UPD            NUMERIC                             
081600             MOVE MFS-NUM-FIELD-OK    TO MOD-TIHHMM-ATTR                  
081700          ELSE                                                            
081800             MOVE MFS-NUM-FIELD-WRONG TO MOD-TIHHMM-ATTR                  
081900         END-IF                                                           
082000                                                                          
082100         IF MID-IDPLKLST-UPD          NUMERIC                             
082200             MOVE MFS-NUM-FIELD-OK    TO MOD-IDPLKLST-ATTR                
082300          ELSE                                                            
082400             MOVE MFS-NUM-FIELD-WRONG TO MOD-IDPLKLST-ATTR                
082500         END-IF                                                           
082600                                                                          
082700         MOVE ERR-CORR-HILITE-FLDS    TO MED-IDMFSFEL                     
082800         MOVE NOO                     TO INDATA-SW                        
082900     END-IF                                                               
083000     .                                                                    
083100     EJECT                                                                
083200 GCA-CHECK-IDPLKLST   SECTION.                                            
083300                                                                          
083400     MOVE +1                   TO INDX                                    
083500     MOVE NOO                  TO IDPLKLST-SW                             
083600     PERFORM UNTIL INDX        >  MAX-INDX OR                             
083700             IDPLKLST-OK                                                  
083800         IF MID-IDPLKLST-UPD   =  MID-IDPLKLST-RAD (INDX)                 
083900             MOVE YES          TO IDPLKLST-SW                             
084000          ELSE                                                            
084100             ADD +1            TO INDX                                    
084200         END-IF                                                           
084300     END-PERFORM                                                          
084400     .                                                                    
084500     EJECT                                                                
084600 H-UPDATE             SECTION.                                            
084700                                                                          
084800     IF MID-FLJANEJ            =  YES                                     
084900         PERFORM HA-UPDATE-ALL-ORDERPARTS                                 
085000      ELSE                                                                
085100         PERFORM HB-UPDATE-ONE-PLKLST                                     
085200     END-IF                                                               
085300     CALL WMEDKONV USING MED-WMEDAREA                                     
085400     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
085500     PERFORM MFS-FORM-ATTR                                                
085600     PERFORM MFS-ERASE-FIELD-IN                                           
085700     PERFORM MFS-ERASE-FIELD-OUT                                          
085800                                                                          
085900     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDDISTR-OUT                       
086000                                    MOD-IDKUNDNR-OUT                      
086100                                    MOD-IDORDNR7-OUT                      
086200                                    MOD-IDPRODNR-OUT                      
086300     .                                                                    
086400     EJECT                                                                
086500 HA-UPDATE-ALL-ORDERPARTS   SECTION.                                      
086600                                                                          
086700     PERFORM IMS-GHU-ORQA-WDQ3-OKVAL                                      
086800     MOVE +1                   TO INDX                                    
086900     PERFORM UNTIL  SEGMENT-MISSING OR END-OF-DATA                        
087000        MOVE W-DARFS                  TO ORQA-ODEL-DARFS                  
087100        MOVE W-DARFS-DATUM            TO ORQA-ODEL-DARFSDAT               
087200        PERFORM IMS-REPL-ORQA                                             
087300        MOVE MFS-ADD-HILIGHT-FIELD    TO  MOD-TIRFS-L-ATTR(INDX)          
087400        ADD +1                        TO INDX                             
087500        MOVE INF-UPDATE-DONE          TO MED-IDMFSINF                     
087600        PERFORM IMS-GHN-ORQA-WDQ3                                         
087700     END-PERFORM                                                          
087800     .                                                                    
087900     EJECT                                                                
088000 HB-UPDATE-ONE-PLKLST       SECTION.                                      
088100                                                                          
088200     MOVE MID-IDPLKLST-UPD         TO W-IDPLKLST                          
088300     PERFORM IMS-GHU-ORQA-WDQ3-KVAL                                       
088400     MOVE W-DARFS                  TO ORQA-ODEL-DARFS                     
088500     MOVE W-DARFS-DATUM            TO ORQA-ODEL-DARFSDAT                  
088600     MOVE MFS-ADD-HILIGHT-FIELD    TO MOD-TIRFS-L-ATTR(1)                 
088700     PERFORM IMS-REPL-ORQA                                                
088800     MOVE INF-UPDATE-DONE          TO MED-IDMFSINF                        
088900     .                                                                    
089000     EJECT                                                                
089100 MFS-ERASE-FIELD-OUT SECTION.                                             
089200                                                                          
089300*    --- ALL OUTDATA-FIELD                                                
089400*    --- INCL SCROLLKEYS                                                  
089500                                                                          
089600     MOVE MFS-ERASE-FIELD TO MOD-IDORDER-FIRST                            
089700                             MOD-IDORDER-ENTER                            
089800                             MOD-IDORDER-NEXT                             
089900                             MOD-IDPRODNR-FIRST                           
090000                             MOD-IDPRODNR-ENTER                           
090100                             MOD-IDPRODNR-NEXT                            
090200                             MOD-IDPLKLST-FIRST                           
090300                             MOD-IDPLKLST-ENTER                           
090400                             MOD-IDPLKLST-NEXT                            
090500                             MOD-IDTRP-IN                                 
090600                             MOD-TITRPAVG                                 
090700                             MOD-KDFRAKT                                  
090800     MOVE +1 TO INDX                                                      
090900     PERFORM UNTIL INDX > MAX-INDX                                        
091000       PERFORM MFS-ERASE-LINE-FIELD-OUT                                   
091100       ADD +1 TO INDX                                                     
091200     END-PERFORM                                                          
091300     .                                                                    
091400     SKIP2                                                                
091500 MFS-ERASE-LINE-FIELD-OUT SECTION.                                        
091600                                                                          
091700*    --- OUTDATA-FIELD ON SCROLL-LINES                                    
091800     MOVE MFS-ERASE-FIELD TO MOD-KVRADER      (INDX)                      
091900                             MOD-IDPLKLST     (INDX)                      
092000                             MOD-IDPRC        (INDX)                      
092100                             MOD-KDODELSTA    (INDX)                      
092200                             MOD-KVRADER      (INDX)                      
092300                             MOD-KVPACKRAD-OD (INDX)                      
092400                             MOD-VKORDNTO     (INDX)                      
092500                             MOD-VLORDNTO     (INDX)                      
092600                             MOD-TILST-OD     (INDX)                      
092700                             MOD-SUPTID       (INDX)                      
092800                             MOD-TIRFS-OUT    (INDX)                      
092900                             MOD-IDANSTNR     (INDX)                      
093000     .                                                                    
093100     SKIP2                                                                
093200 MFS-ERASE-FIELD-IN SECTION.                                              
093300                                                                          
093400*    --- ALL INPUT-FIELDS                                                 
093500     MOVE MFS-ERASE-FIELD TO MOD-IDDISTR-IN                               
093600                             MOD-IDKUNDNR-IN                              
093700                             MOD-IDORDNR7-IN                              
093800                             MOD-IDPRODNR-IN                              
093900                             MOD-IDPLKLST-UPD                             
094000                             MOD-TIAAMMDD-UPD                             
094100                             MOD-TIHHMM-UPD                               
094200     .                                                                    
094300     EJECT                                                                
094400 MFS-DO-NOT-TOUCH-FIELD-OUT SECTION.                                      
094500                                                                          
094600*    --- ALL OUTDATA-FIELDS                                               
094700*    --- INCL SCROLLKEYS AND LINE-DATA                                    
094800     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDORDER-FIRST                     
094900                                    MOD-IDORDER-ENTER                     
095000                                    MOD-IDORDER-NEXT                      
095100                                    MOD-IDPRODNR-FIRST                    
095200                                    MOD-IDPRODNR-ENTER                    
095300                                    MOD-IDPRODNR-NEXT                     
095400                                    MOD-IDPLKLST-FIRST                    
095500                                    MOD-IDPLKLST-ENTER                    
095600                                    MOD-IDPLKLST-NEXT                     
095700                                    MOD-IDTRP-IN                          
095800                                    MOD-TITRPAVG                          
095900                                    MOD-KDFRAKT                           
096000                                                                          
096100     MOVE +1 TO INDX                                                      
096200     PERFORM UNTIL INDX > MAX-INDX                                        
096300       PERFORM MFS-DO-NOT-TCH-LINE-FIELD-OUT                              
096400       ADD +1 TO INDX                                                     
096500     END-PERFORM                                                          
096600     .                                                                    
096700     SKIP2                                                                
096800 MFS-DO-NOT-TCH-LINE-FIELD-OUT SECTION.                                   
096900                                                                          
097000*    --- OUTDATA-FIELD ON SCROLL-LINES                                    
097100     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDPLKLST  (INDX)                  
097200                                    MOD-IDPRC     (INDX)                  
097300                                    MOD-KDODELSTA (INDX)                  
097400                                    MOD-KVRADER   (INDX)                  
097500                                    MOD-KVPACKRAD-OD (INDX)               
097600                                    MOD-VKORDNTO  (INDX)                  
097700                                    MOD-VLORDNTO  (INDX)                  
097800                                    MOD-TILST-OD  (INDX)                  
097900                                    MOD-SUPTID    (INDX)                  
098000                                    MOD-TIRFS-OUT (INDX)                  
098100                                    MOD-IDANSTNR  (INDX)                  
098200     .                                                                    
098300     SKIP2                                                                
098400 MFS-DO-NOT-TOUCH-FIELD-IN SECTION.                                       
098500                                                                          
098600*    --- ALL INPUT-FIELDS                                                 
098700     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDPLKLST-UPD                      
098800                                    MOD-TIAAMMDD-UPD                      
098900                                    MOD-TIHHMM-UPD                        
099000                                    MOD-FLJANEJ                           
099100                                                                          
099200     .                                                                    
099300     EJECT                                                                
099400 MFS-FORM-ATTR SECTION.                                                   
099500                                                                          
099600*    --- ALL INPUT-FIELDS                                                 
099700     MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-IDPLKLST-ATTR                    
099800                                     MOD-TIAAMMDD-ATTR                    
099900                                     MOD-TIHHMM-ATTR                      
100000                                     MOD-FLJANEJ-ATTR                     
100100     .                                                                    
100200     SKIP2                                                                
100300 MFS-READ-IN-OWN SECTION.                                                 
100400                                                                          
100500*    --- ALL INPUT-FIELD                                                  
100600     MOVE MFS-ADD-READ-FIELD TO MOD-IDPLKLST-ATTR                         
100700                                MOD-TIAAMMDD-ATTR                         
100800                                MOD-TIHHMM-ATTR                           
100900                                MOD-FLJANEJ-ATTR                          
101000     .                                                                    
101100     EJECT                                                                
101200* --- IMS SECTIONS ---                                                    
101300     SKIP3                                                                
101400 IMS-GET-MSG SECTION.                                                     
101500                                                                          
101600     MOVE '  QC' TO GOOD-STATUSCODES                                      
101700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
101800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
101900     PERFORM IMS-STATUSCONTROL                                            
102000     .                                                                    
102100     SKIP3                                                                
102200 IMS-INSERT-MSG SECTION.                                                  
102300                                                                          
102310     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
102500       MOVE '0' TO MFS-KDHUVOMR                                           
102600     END-IF                                                               
102700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
102800     MOVE SPACE TO GOOD-STATUSCODES                                       
102900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
103000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
103100     PERFORM IMS-STATUSCONTROL                                            
103200     .                                                                    
103300     SKIP2                                                                
103400 IMS-GU-CSEQ-WDQ2 SECTION.                                                
103500                                                                          
103600     STRING 'WLORQI01(WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                        
103700          DELIMITED BY SIZE INTO SSA1                                     
103800     MOVE '  GE' TO GOOD-STATUSCODES                                      
103900     CALL CBLTDLI USING GU WDQ2-PCB DLI-IO-AREA2 SSA1                     
104000     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
104100     PERFORM IMS-STATUSCONTROL                                            
104200     .                                                                    
104300                                                                          
104400 IMS-GNP-WDQ2 SECTION.                                                    
104500                                                                          
104600     STRING 'WLORQI12(IDDC     =' W-IDDC-X ')'                            
104700          DELIMITED BY SIZE INTO SSA2                                     
104800     MOVE '  GE' TO GOOD-STATUSCODES                                      
104900     CALL CBLTDLI USING GNP WDQ2-PCB DLI-IO-AREA2 SSA2                    
105000     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
105100     PERFORM IMS-STATUSCONTROL                                            
105200     .                                                                    
105300                                                                          
105400 IMS-GU-ORQA-WDQ3 SECTION.                                                
105500                                                                          
105600     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                        
105700                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
105800          DELIMITED BY SIZE INTO SSA1                                     
105900     MOVE '  GE' TO GOOD-STATUSCODES                                      
106000     CALL CBLTDLI USING GU ORQA-PCB DLI-IO-AREA1 SSA1                     
106100     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
106200     PERFORM IMS-STATUSCONTROL                                            
106300     .                                                                    
106400                                                                          
106500                                                                          
106600 IMS-GN-ORQA-WDQ3 SECTION.                                                
106700                                                                          
106800     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                        
106900                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
107000          DELIMITED BY SIZE INTO SSA2                                     
107100     MOVE '  GE' TO GOOD-STATUSCODES                                      
107200     CALL CBLTDLI USING GN ORQA-PCB DLI-IO-AREA1 SSA2                     
107300     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
107400     PERFORM IMS-STATUSCONTROL                                            
107500     .                                                                    
107600     SKIP2                                                                
107700 IMS-GU-ORQA-WDQ3-KVAL SECTION.                                           
107800                                                                          
107900     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                        
108000                    '&WDQ301KY<=' W-WDQ301KY-MAX-X                        
108100                    '&IDPLKLST>=' W-IDPLKLST-X ')'                        
108200          DELIMITED BY SIZE INTO SSA2                                     
108300     MOVE '  GE' TO GOOD-STATUSCODES                                      
108400     CALL CBLTDLI USING GU ORQA-PCB DLI-IO-AREA1 SSA2                     
108500     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
108600     PERFORM IMS-STATUSCONTROL                                            
108700     .                                                                    
108800     SKIP2                                                                
108900 IMS-GHU-ORQA-WDQ3-KVAL SECTION.                                          
109000                                                                          
109100     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                        
109200                    '&WDQ301KY<=' W-WDQ301KY-MAX-X                        
109300                    '&IDPLKLST =' W-IDPLKLST-X ')'                        
109400          DELIMITED BY SIZE INTO SSA1                                     
109500     MOVE '  ' TO GOOD-STATUSCODES                                        
109600     CALL CBLTDLI USING GHU ORQA-PCB DLI-IO-AREA1 SSA1                    
109700     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
109800     PERFORM IMS-STATUSCONTROL                                            
109900     .                                                                    
110000     SKIP2                                                                
110100 IMS-GHU-ORQA-WDQ3-OKVAL SECTION.                                         
110200                                                                          
110300     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                        
110400                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
110500          DELIMITED BY SIZE INTO SSA1                                     
110600     MOVE '  GE' TO GOOD-STATUSCODES                                      
110700     CALL CBLTDLI USING GHU ORQA-PCB DLI-IO-AREA1 SSA1                    
110800     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
110900     PERFORM IMS-STATUSCONTROL                                            
111000     .                                                                    
111100     SKIP2                                                                
111200 IMS-GHN-ORQA-WDQ3 SECTION.                                               
111300                                                                          
111400     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                        
111500                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
111600          DELIMITED BY SIZE INTO SSA2                                     
111700     MOVE '  GBGE' TO GOOD-STATUSCODES                                    
111800     CALL CBLTDLI USING GHN ORQA-PCB DLI-IO-AREA1 SSA2                    
111900     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
112000     PERFORM IMS-STATUSCONTROL                                            
112100     .                                                                    
112200     SKIP2                                                                
112300 IMS-REPL-ORQA SECTION.                                                   
112400                                                                          
112500     MOVE '  ' TO GOOD-STATUSCODES                                        
112600     CALL CBLTDLI USING REPL ORQA-PCB DLI-IO-AREA1                        
112700     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
112800     PERFORM IMS-STATUSCONTROL                                            
112900     .                                                                    
113000     SKIP2                                                                
113100 IMS-GU-ORQA01-M-Q3DSEQ SECTION.                                          
113200     STRING 'WLORQA01(WDQ3DSEQ>=' W-WDQ3DSEQ-MIN-X                        
113300                    '&WDQ3DSEQ<=' W-WDQ3DSEQ-MAX-X ')'                    
113400          DELIMITED BY SIZE INTO SSA1                                     
113500     MOVE '  GBGE' TO GOOD-STATUSCODES                                    
113600     CALL CBLTDLI USING GU ORQAD-PCB DLI-IO-AREA1 SSA1                    
113700     MOVE ORQAD-STATUS-CODE TO STATUS-WS                                  
113800     PERFORM IMS-STATUSCONTROL                                            
113900     .                                                                    
114000     SKIP2                                                                
114100 IMS-GN-ORQA01-M-Q3DSEQ SECTION.                                          
114200     STRING 'WLORQA01(WDQ3DSEQ>=' W-WDQ3DSEQ-MIN-X                        
114300                    '&WDQ3DSEQ<=' W-WDQ3DSEQ-MAX-X ')'                    
114400          DELIMITED BY SIZE INTO SSA1                                     
114500     MOVE '  GBGE' TO GOOD-STATUSCODES                                    
114600     CALL CBLTDLI USING GN ORQAD-PCB DLI-IO-AREA1 SSA1                    
114700     MOVE ORQAD-STATUS-CODE TO STATUS-WS                                  
114800     PERFORM IMS-STATUSCONTROL                                            
114900     .                                                                    
115000     SKIP2                                                                
115100 IMS-STATUSCONTROL SECTION.                                               
115200                                                                          
115300     SET STATUS-IX TO 1                                                   
115400     SEARCH GOOD-STATUS                                                   
115500       AT END CALL FELLOG                                                 
115600       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
115700     END-SEARCH                                                           
115800     .                                                                    
