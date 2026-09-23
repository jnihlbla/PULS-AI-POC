000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4038200.                                                
000400 AUTHOR.         LUC FEYS.                                                
000500 DATE-WRITTEN.   90/08/22.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNCTION.                                                            
001000*        SEE SPECIFICATIONS                                               
001100*          LÄSER DE PRCER SOM HÖR TILL EN AVD (PRCGRP), OCH VISAR         
001200*          DE ORDER SOM FINNS I PRC:ERNA I PRIORITETSORDNING.             
001300*          ANTAL RADER I VARJE PRC VISAS.                                 
001400*                                                                         
001500*        THE PROGRAM IS A  QUERY    MPP                                   
001600*        PROGRAM READS   WLXXKH (WDR1)                                    
001700*        PROGRAM READS   WLORQA (WDQ3)                                    
001800*                                                                         
001900*                                                                         
002000*    INDATA.                                                              
002100*        TRANSACTION: W4T382                                              
002200*        MID:         W4I38201                                            
002300*                                                                         
002400*   OUTDATA.                                                              
002500*        MOD:         W4O38201                                            
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100 WORKING-STORAGE SECTION.                                                 
003200                                                                          
003300*    -- CHECKED BY WY2000                                                 
003400 77  IDPGM                       PIC X(08)   VALUE 'W4038200'.            
003500                                                                          
003600 77  YES                         PIC X       VALUE 'J'.                   
003700 77  NOO                         PIC X       VALUE 'N'.                   
003800                                                                          
003900*    --- INDEX FOR BROWSE LINES                                           
004000 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004100 77  INDX2                       PIC S9(4)  VALUE +0    COMP SYNC.        
004200 77  INDX3                       PIC S9(4)  VALUE +0    COMP SYNC.        
004300 77  INDX4                       PIC S9(4)  VALUE +0    COMP SYNC.        
004400 77  MAX-INDX                    PIC S9(4)  VALUE +14   COMP SYNC.        
004500                                                                          
004600 77  W-HELP-IDORDER              PIC S9(7)  VALUE +0    COMP-3.           
004700 77  W-DARFS-NEXT                PIC  9(12) VALUE ZERO.                   
004800 77  W-DALSTORD-NEXT             PIC  9(12) VALUE ZERO.                   
004900*                                                                         
005000*      --- VALID IDDC CODES                                               
005100*                                                                         
005200*01    -COPY WWDC99                                                       
005300       EJECT                                                              
005400*    --- WORKFIELDS FOR ACTUAL KEYVALUES OF SCREENS                       
005500                                                                          
005600 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005700     88  INDATA-OK                           VALUE 'J'.                   
005800     88  INDATA-WRONG                        VALUE 'N'.                   
005900                                                                          
006000 77  KEYS-SW                     PIC X       VALUE 'J'.                   
006100     88  KEYS-OK                             VALUE 'J'.                   
006200     88  KEYS-WRONG                          VALUE 'N'.                   
006300                                                                          
006400 77  ALLT-SW                     PIC X       VALUE 'J'.                   
006500     88  ALLT-OK                             VALUE 'J'.                   
006600                                                                          
006700 77  OLD-IDORDER-SW              PIC X       VALUE 'N'.                   
006800     88  OLD-IDORDER                         VALUE 'J'.                   
006900                                                                          
007000 77  PREV-LINE-SW                PIC X       VALUE 'N'.                   
007100     88  PREV-LINE-OK                        VALUE 'J'.                   
007200                                                                          
007300 77  KDODELSTA-R-SW              PIC X       VALUE 'N'.                   
007400                                                                          
007500 77  KDODELSTA-P-SW              PIC X       VALUE 'N'.                   
007600                                                                          
007700 77  T-TABLE-SW                  PIC X       VALUE 'N'.                   
007800     88  T-TABLE-OK                          VALUE 'J'.                   
007900     88  T-TABLE-NOT-OK                      VALUE 'N'.                   
008000                                                                          
008100 77  IDPRCBAS-SW                 PIC X       VALUE 'N'.                   
008200     88  IDPRCBAS-OK                         VALUE 'J'.                   
008300                                                                          
008400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008500     88  OWN-MID                             VALUE '4382'.                
008600     88  GOOD-MID                            VALUE '4381' '4382'          
008700                                                   '4384'.                
008800     EJECT                                                                
008900*    --- SUBPROGRAM AND PARAMETER-AREAS                                   
009000 01  GENERAL-SUBPROGRAM.                                                  
009100     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
009200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009400     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR '.            
009500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009600*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
009700*01 -COPY WMSGINIT                                                        
009800     EJECT                                                                
009900*    --- PARAMETERS FOR  SUBPROGRAM WMEDKONV                              
010000*   -COPY WMEDAREA                                                        
010100     SKIP3                                                                
010200 01  MESSAGE-CODES.                                                       
010300     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
010400     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
010500     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
010600     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
010700     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
010800     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
010900     03  ERR-LAST-PAGE-SHOWED    PIC X(3)    VALUE '115'.                 
011000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
011100     EJECT                                                                
011200*    --- AREAS FOR MFS AND SCREENHANDLING                                 
011300*                                                                         
011400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
011500     SKIP3                                                                
011600*01  MID -COPY W4I38201                                                   
011700     EJECT                                                                
011800 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
011900     SKIP3                                                                
012000*01  -COPY WMSGAREA                                                       
012100     EJECT                                                                
012200*    03  MOD -COPY W4O38201   -RED MSG-AREA.                              
012300     EJECT                                                                
012400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
012500     SKIP3                                                                
012600*01  -COPY WMFSAREA                                                       
012700     EJECT                                                                
012800*    --- WORK-AREAS FOR IMS-SECTIONS                                      
012900*                                                                         
013000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013100     SKIP3                                                                
013200 01  KEYS-TILL-DLI.                                                       
013300     03  W-4447-X.                                                        
013400         05  W-IDHTYP            PIC X(4)     VALUE '4447'.               
013500         05  W-4447-IDDC         PIC X(2).                                
013600         05  W-4447-LOW-VALUE    PIC X(24)    VALUE LOW-VALUE.            
013700     03  W-4448-X.                                                        
013800         05  W-4448-IDPRC        PIC X(4)    VALUE LOW-VALUE.             
013900         05  W-4448-LOW-VALUE    PIC X(1)    VALUE LOW-VALUE.             
014000     03  KDPRCGRP-WS             PIC X(5).                                
014100     03  KDPRODKL-WS             PIC X(1).                                
014200     03  IDPRC-WS                PIC X(4).                                
014300*                                                                         
014400     03  WS-KVRADER-LINE.                                                 
014500         05 WS-KVRADER-X         PIC ZZZZZ    OCCURS 10.                  
014600*                                                                         
014700*    DIRECT KEY                                                           
014800*                                                                         
014900     03  W-WDQ301KY-MIN-X.                                                
015000         05 W-Q301KY-IDORDER-MIN     PIC S9(7)  COMP-3.                   
015100         05 W-Q301KY-IDDC-MIN        PIC X(2).                            
015200         05 W-Q301KY-IDPRODNR-MIN    PIC X(4)   VALUE LOW-VALUE.          
015300         05 W-Q301KY-IDPLKLST-MIN    PIC X(2)   VALUE LOW-VALUE.          
015400*                                                                         
015500     03  W-WDQ301KY-MAX-X.                                                
015600         05 W-Q301KY-IDORDER-MAX     PIC S9(7)  COMP-3.                   
015700         05 W-Q301KY-IDDC-MAX        PIC X(2).                            
015800         05 W-Q301KY-IDPRODNR-MAX    PIC X(4)   VALUE HIGH-VALUE.         
015900         05 W-Q301KY-IDPLKLST-MAX    PIC X(2)   VALUE HIGH-VALUE.         
016000*                                                                         
016100     03  D-WDQ3CSEQ-X.                                                    
016200         05 D-IDDC-X.                                                     
016300            07 D-IDDC            PIC X(2).                                
016400         05 D-IDPRCBAS-X.                                                 
016500            07 D-IDPRCBAS        PIC X(3).                                
016600         05 D-IDPRCVAR-X.                                                 
016700            07 D-IDPRCVAR        PIC X(1).                                
016800         05 D-DARFS-X.                                                    
016900            07 D-DARFS           PIC 9(12).                               
017000         05 D-DALSTORD-X.                                                 
017100            07 D-DALSTORD        PIC 9(12).                               
017200         05 D-IDORDER-X.                                                  
017300            07 D-IDORDER         PIC S9(7)    COMP-3.                     
017400     03  XD-IDORDER-X.                                                    
017500         05 XD-IDORDER           PIC S9(7)    COMP-3.                     
017600     03  W-KDODELST-U            PIC X(1)     VALUE 'U'.                  
017700     03  W-KDODELST-P            PIC X(1)     VALUE 'P'.                  
017800*                                                                         
017900*    SECONDARY KEY                                                        
018000*                                                                         
018100     03  W-WDQ3CSEQ-MIN.                                                  
018200         05 W-IDDC-MIN           PIC  X(2).                               
018300         05 W-IDPRCBAS-MIN       PIC  X(3).                               
018400         05 W-IDPRCVAR-MIN       PIC  X(1).                               
018500         05 W-DARFS-MIN          PIC  9(12).                              
018600         05 W-DALSTORD-MIN       PIC  9(12).                              
018700         05 W-IDORDER-MIN        PIC  S9(7)  COMP-3.                      
018800*                                                                         
018900     03  W-WDQ3CSEQ-MAX.                                                  
019000         05 W-IDDC-MAX           PIC  X(2).                               
019100         05 W-IDPRCBAS-MAX       PIC  X(3).                               
019200         05 W-IDPRCVAR-MAX       PIC  X(1).                               
019300         05 W-DARFS-MAX          PIC  X(28)  VALUE HIGH-VALUE.            
019400*                                                                         
019500     03  W-IDORDER-X.                                                     
019600         05 W-IDORDER            PIC S9(7)    COMP-3.                     
019700*                                                                         
019800     03  W-IDDC-X.                                                        
019900         05 W-IDDC               PIC  X(2).                               
020000*                                                                         
020100 01 FILLER     PIC X(16) VALUE 'ORDERPARTSPRTY'.                          
020200 01 OPARTS-TABLE.                                                         
020300     03 OPARTS-LINE OCCURS 10 INDEXED BY N.                               
020400*                                                                         
020500        05 WS-SORTFIELDS.                                                 
020600           07 WS-DARFS          PIC 9(12).                                
020700           07 WS-DALSTORD       PIC 9(12).                                
020800           07 WS-IDORDER        PIC 9(7).                                 
020900        05 WS-IDPRCBAS          PIC X(3).                                 
021000        05 WS-FULL-KEY.                                                   
021100           07 FILLER            PIC X(05).                                
021200           07 WS-IDPRCVAR       PIC X(01).                                
021300           07 FILLER            PIC X(28).                                
021400*                                                                         
021500*                                                                         
021600* SORT PARAMETERS FOR OPARTS-TABLE                                        
021700*                                                                         
021800*                                                                         
021900 01 FILLER PIC X(16) VALUE 'SORT PARAMETERS'.                             
022000 01 WS-TABLE.                                                             
022100     03 WS-LGT-RECORD           PIC S9(9)    VALUE +68 COMP SYNC.         
022200     03 WS-NUMBER-ELEM          PIC S9(9)    VALUE +10 COMP SYNC.         
022300     03 WS-LGT-SFIELDS          PIC S9(9)    VALUE +31 COMP SYNC.         
022400                                                                          
022500 01 FILLER     PIC X(16) VALUE 'IDORDERTABLE  '.                          
022600 01 IDORDER-TABLE.                                                        
022700     03 IDORDER-LINE OCCURS 15 INDEXED BY M.                              
022800*                                                                         
022900         05 T-IDORDER           PIC 9(7).                                 
023000         05 T-IDORDNR7          PIC 9(7).                                 
023100         05 T-IDDISTR           PIC 9(5).                                 
023200         05 T-IDKUNDNR          PIC 9(7).                                 
023300         05 T-IDPRODNR          PIC 9(7).                                 
023400         05 T-DARFS             PIC 9(12).                                
023500         05 T-DALSTORD          PIC 9(12).                                
023600         05 FILLER  OCCURS 10.                                            
023700           07 T-IDPRC.                                                    
023800             09 T-IDPRCBAS      PIC X(3).                                 
023900             09 T-IDPRCVAR      PIC X(1).                                 
024000           07 T-KVRADER         PIC 9(5).                                 
024100           07 T-KDODELSTA       PIC X(1).                                 
024200           07 T-STARSPAC        PIC X(1).                                 
024300*                                                                         
024400*                                                                         
024500*    --- STATUS-CODE FROM IMS                                             
024600 01  STATUS-WS                   PIC XX.                                  
024700     88  SEGMENT-FOUND                       VALUE '  '.                  
024800     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
024900     88  SEGMENT-MISSING                     VALUE 'GE'.                  
025000     88  SEGMENT-END                         VALUE 'GB'.                  
025100     SKIP2                                                                
025200 01  GOOD-STATUSCODES.                                                    
025300     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
025400     SKIP3                                                                
025500 01  SSA1                        PIC X(140).                              
025600 01  SSA2                        PIC X(140).                              
025700     EJECT                                                                
025800*    --- IMS FUNCTIONCODES                                                
025900*01  -COPY W0003                                                          
026000     EJECT                                                                
026100*    ---  DLI INPUT-OUTPUT AREA                                           
026200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
026300     SKIP3                                                                
026400 01  DLI-IO-AREA.                                                         
026500     03  IO-AREA                 PIC X(3120) VALUE SPACE.                 
026600     SKIP3                                                                
026700     03  WLXXKH01 REDEFINES IO-AREA.                                      
026800*        05  -COPY WDGX4447   -PRE XXKH-                                  
026900     EJECT                                                                
027000     03  WLXXKH11 REDEFINES IO-AREA.                                      
027100*        05  -COPY WDGX4448   -PRE XXKH-                                  
027200     SKIP3                                                                
027300     03  WLORQA01 REDEFINES IO-AREA.                                      
027400*        05  -COPY WDQ301     -PRE ORQA-                                  
027500     EJECT                                                                
027600 LINKAGE SECTION.                                                         
027700                                                                          
027800*01  -COPY W0009      -PRE MSG-                                           
027900     EJECT                                                                
028000*01  -COPY W0008      -PRE USEA-                                          
028100     05  FILLER                  PIC X.                                   
028200     EJECT                                                                
028300*01  -COPY W0008      -PRE XXKH-                                          
028400     05  FILLER                  PIC X.                                   
028500     EJECT                                                                
028600*01  -COPY W0008      -PRE ORQC-                                          
028700     05  FILLER                  PIC X(28).                               
028800     EJECT                                                                
028900*01  -COPY W0008      -PRE ORQA-                                          
029000     05  FILLER                  PIC X.                                   
029100     EJECT                                                                
029200 PROCEDURE DIVISION  USING MSG-PCB  USEA-PCB                              
029300                           XXKH-PCB ORQC-PCB ORQA-PCB.                    
029400     ENTRY 'DLITCBL' USING MSG-PCB  USEA-PCB                              
029500                           XXKH-PCB ORQC-PCB ORQA-PCB.                    
029600                                                                          
029700     PERFORM IMS-GET-MSG                                                  
029800     IF SEGMENT-FOUND                                                     
029900       PERFORM A-INIT                                                     
030000       PERFORM B-CONTROL-KEYS                                             
030100       IF KEYS-OK                                                         
030200          PERFORM BA-GET-PRCBAS                                           
030300          IF ALLT-OK                                                      
030400             IF MFS-FIRST                                                 
030500               PERFORM C-FIRST-PAGE                                       
030600             ELSE                                                         
030700               IF MFS-NEXT                                                
030800                 PERFORM D-NEXT-PAGE                                      
030900               ELSE                                                       
031000                 PERFORM E-SAME-PAGE                                      
031100               END-IF                                                     
031200             END-IF                                                       
031300             IF ALLT-OK                                                   
031400               PERFORM F-READ-SHOW-INFO                                   
031500             END-IF                                                       
031600          END-IF                                                          
031700       END-IF                                                             
031800       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O38201 + 4                      
031900       PERFORM IMS-INSERT-MSG                                             
032000     END-IF                                                               
032100                                                                          
032200     MOVE ZERO TO RETURN-CODE                                             
032300     GOBACK                                                               
032400     .                                                                    
032500     EJECT                                                                
032600 A-INIT SECTION.                                                          
032700                                                                          
032800     IF MSG-DOUBLE-TRANSACTIONS                                           
032900       MOVE MSG-INDATA-MINUS-2-TRANSKODER   TO MID-W4I38201               
033000       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
033100       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
033200     ELSE                                                                 
033300       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I38201                  
033400       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
033500       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
033600     END-IF                                                               
033700                                                                          
033800     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
033900     MOVE MSG-IDPFK TO MFS-IDPFK                                          
034000     MOVE MFS-IDTRANS TO W-IDTRANS                                        
034100                                                                          
034200     MOVE LOW-VALUE TO MSG-AREA                                           
034300     MOVE 'W4O382N1' TO MFS-IDMOD                                         
034400     MOVE '4382' TO MOD-IDTRANS                                           
034500     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
034600                                                                          
034700     IF NOT OWN-MID                                                       
034800       MOVE SPACE TO MFS-KDTRTYP                                          
034900       MOVE '7' TO MFS-IDPFK                                              
035000     END-IF                                                               
035100                                                                          
035200     MOVE HIGH-VALUE TO W-WDQ3CSEQ-MAX                                    
035300     MOVE HIGH-VALUE TO OPARTS-TABLE                                      
035400     MOVE LOW-VALUE TO W-WDQ3CSEQ-MIN                                     
035500     MOVE +1                   TO INDX2                                   
035600     PERFORM UNTIL INDX2       >  +10                                     
035700         MOVE SPACE            TO MOD-IDPRCVAR-NEXT(INDX2)                
035800                                  MOD-IDPRCVAR-ENTER(INDX2)               
035900         ADD +1                TO INDX2                                   
036000     END-PERFORM                                                          
036100                                                                          
036200     MOVE +1                   TO INDX                                    
036300     PERFORM UNTIL INDX        >  MAX-INDX                                
036400         MOVE +1               TO INDX4                                   
036500         PERFORM UNTIL INDX4   >  10                                      
036600             MOVE ZERO         TO T-KVRADER (INDX, INDX4)                 
036700             ADD +1            TO INDX4                                   
036800         END-PERFORM                                                      
036900         ADD +1                TO INDX                                    
037000     END-PERFORM                                                          
037100                                                                          
037200     MOVE +1                   TO INDX                                    
037300     PERFORM UNTIL INDX        >  MAX-INDX                                
037400         MOVE ZERO             TO T-IDORDER   (INDX)                      
037500                                  T-IDORDNR7  (INDX)                      
037600                                  T-IDDISTR   (INDX)                      
037700                                  T-IDKUNDNR  (INDX)                      
037800                                  T-IDPRODNR  (INDX)                      
037900                                  T-DARFS     (INDX)                      
038000                                  T-DALSTORD  (INDX)                      
038100         MOVE +1               TO INDX4                                   
038200         PERFORM UNTIL INDX4   >  10                                      
038300             MOVE ZERO         TO T-KVRADER   (INDX, INDX4)               
038400             MOVE SPACE        TO T-IDPRC     (INDX, INDX4)               
038500                                  T-KDODELSTA (INDX, INDX4)               
038600                                  T-STARSPAC  (INDX, INDX4)               
038700             ADD +1            TO INDX4                                   
038800         END-PERFORM                                                      
038900         ADD +1                TO INDX                                    
039000     END-PERFORM                                                          
039100     .                                                                    
039200     EJECT                                                                
039300 B-CONTROL-KEYS SECTION.                                                  
039400                                                                          
039500     MOVE ALL '+'           TO MSGI-WMSGINIT                              
039600     MOVE '001'             TO MSGI-KDCALL                                
039700     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
039800     MOVE '4382'            TO MSGI-IDTRANS                               
039900     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
040000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
040100     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
040101                                                                          
040110     MOVE MSGI-IDDC      TO WS-IDDC                                       
040200                                                                          
040300     MOVE YES TO KEYS-SW                                                  
040400                                                                          
040500     IF MID-KDPRCGRP-IN = ALL '+'                                         
040600        MOVE MID-KDPRCGRP-OUT TO KDPRCGRP-WS                              
040700        INSPECT KDPRCGRP-WS REPLACING LEADING SPACE BY ZERO               
040800     ELSE                                                                 
040900        MOVE MID-KDPRCGRP-IN TO KDPRCGRP-WS                               
041000        MOVE '7'             TO MFS-IDPFK                                 
041100        MOVE SPACE           TO MFS-KDTRTYP                               
041200     END-IF                                                               
041300                                                                          
041400     IF KDPRCGRP-WS NOT NUMERIC                                           
041500        MOVE NOO TO KEYS-SW                                               
041600     ELSE                                                                 
041700        IF KDPRCGRP-WS NOT > ZERO                                         
041800           MOVE NOO TO KEYS-SW                                            
041900        END-IF                                                            
042000     END-IF                                                               
042100                                                                          
042200     IF MID-KDPRODKL-IN =     '+'                                         
042210       IF NDC-US                                                          
042211       AND MID-KDPRODKL-OUT = SPACE                                       
042212         MOVE SPACE            TO KDPRODKL-WS                             
042220       ELSE                                                               
042300         MOVE MID-KDPRODKL-OUT TO KDPRODKL-WS                             
042310       END-IF                                                             
042400     ELSE                                                                 
042500        MOVE MID-KDPRODKL-IN TO KDPRODKL-WS                               
042600        MOVE '7'             TO MFS-IDPFK                                 
042700        MOVE SPACE           TO MFS-KDTRTYP                               
042800     END-IF                                                               
042900                                                                          
043000*    IF KDPRODKL-WS = SPACE                                               
043100*       MOVE 'B' TO KDPRODKL-WS                                           
043200*    END-IF                                                               
043300*                                                                         
043400     IF KDPRODKL-WS = SPACE                                               
043500       IF NDC-US                                                          
043600        MOVE 'A' TO KDPRODKL-WS                                           
043700       ELSE                                                               
043800        MOVE 'B' TO KDPRODKL-WS                                           
043900       END-IF                                                             
044000     END-IF                                                               
044100                                                                          
044200     IF KDPRODKL-WS ALPHABETIC                                            
044300        CONTINUE                                                          
044400     ELSE                                                                 
044500        MOVE NOO TO KEYS-SW                                               
044600     END-IF                                                               
044610                                                                          
044620*    STRING '*' KDPRODKL-WS                                               
044630*           '*' WS-IDDC                                                   
044640*    DELIMITED BY SIZE INTO MOD-TEMFSINF                                  
044700                                                                          
044800     IF MID-IDPRC-IN           = ALL '+'                                  
044900        MOVE MID-IDPRC-OUT     TO IDPRC-WS                                
045000     ELSE                                                                 
045100        MOVE MID-IDPRC-IN      TO IDPRC-WS                                
045200        MOVE '7'               TO MFS-IDPFK                               
045300        MOVE SPACE             TO MFS-KDTRTYP                             
045400     END-IF                                                               
045500                                                                          
045600     IF IDPRC-WS             = SPACE                                      
045700        MOVE LOW-VALUE       TO W-4448-IDPRC                              
045800     ELSE                                                                 
045900        MOVE IDPRC-WS        TO W-4448-IDPRC                              
046000     END-IF                                                               
046100                                                                          
046300                                                                          
046400     MOVE WS-IDDC          TO W-IDDC-MIN                                  
046500                              W-IDDC-MAX                                  
046600                              W-IDDC                                      
046700                              D-IDDC                                      
046800                              W-4447-IDDC                                 
046900                                                                          
047000     MOVE WS-IDDC        TO MOD-IDDC-OUT                                  
047100                                                                          
047200     IF GOOD-MID OR KEYS-OK                                               
047300        MOVE KDPRCGRP-WS TO MOD-KDPRCGRP-OUT                              
047400        IF NDC-US AND KDPRODKL-WS = 'A'                                   
047500          MOVE SPACE      TO MOD-KDPRODKL-OUT                             
047600        ELSE                                                              
047700          MOVE KDPRODKL-WS TO MOD-KDPRODKL-OUT                            
047800        END-IF                                                            
047900*       MOVE KDPRODKL-WS TO MOD-KDPRODKL-OUT                              
048000        MOVE IDPRC-WS    TO MOD-IDPRC-OUT                                 
048100        INSPECT MOD-IDPRC-OUT REPLACING LEADING ZERO BY SPACE             
048200     ELSE                                                                 
048300        MOVE MFS-RENSA-FAELT TO MOD-KDPRCGRP-OUT                          
048400                                MOD-KDPRODKL-OUT                          
048500                                MOD-IDPRC-OUT                             
048600     END-IF                                                               
048700                                                                          
048800     IF KEYS-WRONG                                                        
048900        MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                
049000        CALL WMEDKONV USING MED-WMEDAREA                                  
049100        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
049200        PERFORM MFS-ERASE-FIELD-IN                                        
049300        PERFORM MFS-ERASE-FIELD-OUT                                       
049400     END-IF                                                               
049500     .                                                                    
049600     EJECT                                                                
049700 BA-GET-PRCBAS SECTION.                                                   
049800                                                                          
049900     PERFORM IMS-GU-XXKH01                                                
050000     MOVE +1 TO INDX2                                                     
050100     MOVE SPACES TO MOD-IDPRC-LINE (1)                                    
050200     IF NDC-US AND KDPRODKL-WS = 'A'                                      
050300       PERFORM IMS-GNP-XXKH11-NDC-US                                      
050400     ELSE                                                                 
050500       PERFORM IMS-GNP-XXKH11                                             
050600     END-IF                                                               
050700     PERFORM UNTIL INDX2 > 10                                             
050800       IF SEGMENT-FOUND                                                   
050900          IF XXKH-4448-IDPRC (1:3) = '998' AND                            
051000             CDC                                                          
051100              CONTINUE                                                    
051200           ELSE                                                           
051300              MOVE XXKH-4448-IDPRC TO W-4448-IDPRC                        
051400                                         MOD-IDPRC-LINE (INDX2)           
051500              ADD +1 TO INDX2                                             
051600          END-IF                                                          
051700          IF NDC-US AND KDPRODKL-WS = 'A'                                 
051800            PERFORM IMS-GNP-XXKH11-NDC-US                                 
051900          ELSE                                                            
052000            PERFORM IMS-GNP-XXKH11                                        
052100          END-IF                                                          
052200       ELSE                                                               
052300          MOVE SPACES             TO MOD-IDPRC-LINE (INDX2)               
052400          ADD +1 TO INDX2                                                 
052500       END-IF                                                             
052600     END-PERFORM                                                          
052700     IF MOD-IDPRC-LINE (1) = SPACES                                       
052800        MOVE NOO TO ALLT-SW                                               
052900        MOVE '413'          TO MED-IDMFSFEL                               
053000        CALL WMEDKONV USING MED-WMEDAREA                                  
053100        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
053200     END-IF                                                               
053300     .                                                                    
053400     EJECT                                                                
053500 C-FIRST-PAGE SECTION.                                                    
053600                                                                          
053700     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
053800     CALL WMEDKONV USING MED-WMEDAREA                                     
053900     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
054000                                                                          
054100     .                                                                    
054200     EJECT                                                                
054300 D-NEXT-PAGE SECTION.                                                     
054400                                                                          
054500     IF MID-IDORDER-NEXT = 'SLUT'                                         
054600        MOVE ERR-LAST-PAGE-SHOWED TO MED-IDMFSFEL                         
054700        CALL WMEDKONV USING MED-WMEDAREA                                  
054800        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
054900        MOVE 'SLUT'       TO MOD-IDORDER-NEXT                             
055000        MOVE NOO          TO ALLT-SW                                      
055100     ELSE                                                                 
055200        MOVE MID-TIRFS-NEXT   TO W-DARFS-MIN                              
055300        IF MID-TIRFS-NEXT NOT = ZERO                                      
055400          IF MID-TIRFS-NEXT < 5000000000                                  
055500            MOVE 20           TO W-DARFS-MIN (1:2)                        
055600          ELSE                                                            
055700            IF MID-TIRFS-NEXT < 9999999999                                
055800              MOVE 19         TO W-DARFS-MIN (1:2)                        
055900            ELSE                                                          
056000              MOVE 999999999999 TO W-DARFS-MIN                            
056100            END-IF                                                        
056200          END-IF                                                          
056300        END-IF                                                            
056400        MOVE MID-TILST-O-NEXT TO W-DALSTORD-MIN                           
056500        IF MID-TILST-O-NEXT NOT = ZERO                                    
056600          IF MID-TILST-O-NEXT < 5000000000                                
056700            MOVE 20           TO W-DALSTORD-MIN (1:2)                     
056800          ELSE                                                            
056900            IF MID-TILST-O-NEXT < 9999999999                              
057000              MOVE 19         TO W-DALSTORD-MIN (1:2)                     
057100            ELSE                                                          
057200              MOVE 999999999999 TO W-DALSTORD-MIN                         
057300            END-IF                                                        
057400          END-IF                                                          
057500        END-IF                                                            
057600        MOVE MID-IDORDER-NEXT TO W-IDORDER                                
057700        MOVE YES TO ALLT-SW                                               
057800     END-IF                                                               
057900     .                                                                    
058000     EJECT                                                                
058100 E-SAME-PAGE SECTION.                                                     
058200                                                                          
058300     MOVE MID-TIRFS-ENTER   TO W-DARFS-MIN                                
058400     IF MID-TIRFS-ENTER NOT = ZERO                                        
058500       IF MID-TIRFS-ENTER < 5000000000                                    
058600         MOVE 20           TO W-DARFS-MIN (1:2)                           
058700       ELSE                                                               
058800         IF MID-TIRFS-ENTER < 9999999999                                  
058900           MOVE 19         TO W-DARFS-MIN (1:2)                           
059000         ELSE                                                             
059100           MOVE 999999999999 TO W-DARFS-MIN                               
059200         END-IF                                                           
059300       END-IF                                                             
059400     END-IF                                                               
059500     MOVE MID-TILST-O-ENTER TO W-DALSTORD-MIN                             
059600     IF MID-TILST-O-ENTER NOT = ZERO                                      
059700       IF MID-TILST-O-ENTER < 5000000000                                  
059800         MOVE 20            TO W-DALSTORD-MIN (1:2)                       
059900       ELSE                                                               
060000         IF MID-TILST-O-ENTER < 9999999999                                
060100           MOVE 19          TO W-DALSTORD-MIN (1:2)                       
060200         ELSE                                                             
060300           MOVE 999999999999 TO W-DALSTORD-MIN                            
060400         END-IF                                                           
060500       END-IF                                                             
060600     END-IF                                                               
060700     MOVE MID-IDORDER-ENTER TO W-IDORDER                                  
060800     MOVE YES TO ALLT-SW                                                  
060900     .                                                                    
061000     EJECT                                                                
061100 F-READ-SHOW-INFO SECTION.                                                
061200                                                                          
061300     IF KEYS-OK                                                           
061400         PERFORM FA-SORT-IDPRCBAS                                         
061500         PERFORM FB-SORT-ORDERPARTS                                       
061600         PERFORM FC-MOVE-TAB-TO-MOD                                       
061700     ELSE                                                                 
061800         MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                               
061900         CALL WMEDKONV USING MED-WMEDAREA                                 
062000         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
062100         PERFORM MFS-ERASE-FIELD-IN                                       
062200         PERFORM MFS-ERASE-FIELD-OUT                                      
062300     END-IF                                                               
062400                                                                          
062500     IF WS-IDORDER (1)         = HIGH-VALUES                              
062600         MOVE '106' TO MED-IDMFSINF                                       
062700         CALL WMEDKONV USING MED-WMEDAREA                                 
062800         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
062900         MOVE 'SLUT'           TO MOD-IDORDER-NEXT                        
063000         MOVE SPACES           TO MOD-TIRFS-NEXT                          
063100         MOVE SPACES           TO MOD-TILST-O-NEXT                        
063200     END-IF                                                               
063300                                                                          
063400     PERFORM MFS-ERASE-FIELD-IN                                           
063500                                                                          
063600     .                                                                    
063700     EJECT                                                                
063800 FA-SORT-IDPRCBAS        SECTION.                                         
063900                                                                          
064000     MOVE +1 TO INDX2                                                     
064100     MOVE MOD-IDPRCBAS             IN MOD-IDPRC-LINE (INDX2)              
064200                                   TO W-IDPRCBAS-MIN                      
064300                                      W-IDPRCBAS-MAX                      
064400     MOVE MOD-IDPRCVAR             IN MOD-IDPRC-LINE (INDX2)              
064500                                   TO W-IDPRCVAR-MIN                      
064600                                      W-IDPRCVAR-MAX                      
064700                                                                          
064800     IF MFS-FIRST                                                         
064900         MOVE LOW-VALUE            TO W-IDORDER-X                         
065000      ELSE                                                                
065100         PERFORM S01-CHECK-IDPRCVAR                                       
065200     END-IF                                                               
065300     PERFORM IMS-GU-WDQ3CSEQ-ORQC-OKVAL                                   
065400     PERFORM UNTIL INDX2 > 10   OR                                        
065500                     MOD-IDPRC-LINE (INDX2) = SPACES                      
065600          IF SEGMENT-FOUND                                                
065700             MOVE ORQA-ODEL-IDPRCBAS TO WS-IDPRCBAS (INDX2)               
065800             MOVE ORQA-ODEL-IDORDER  TO WS-IDORDER  (INDX2)               
065900             MOVE ORQA-ODEL-DARFS    TO WS-DARFS    (INDX2)               
066000             MOVE ORQA-ODEL-DALSTORD TO WS-DALSTORD (INDX2)               
066100             MOVE ORQC-KEY-FB-AREA   TO WS-FULL-KEY (INDX2)               
066200             MOVE WS-IDPRCVAR(INDX2) TO MOD-IDPRCVAR-ENTER(INDX2)         
066300          ELSE                                                            
066400             MOVE HIGH-VALUES        TO OPARTS-LINE (INDX2)               
066500          END-IF                                                          
066600          ADD +1 TO INDX2                                                 
066610          IF INDX2 > 10                                                   
066620            CONTINUE                                                      
066630          ELSE                                                            
066700            MOVE MOD-IDPRCBAS       IN MOD-IDPRC-LINE (INDX2)             
066800                                    TO W-IDPRCBAS-MIN                     
066900                                       W-IDPRCBAS-MAX                     
067000            MOVE MOD-IDPRCVAR       IN MOD-IDPRC-LINE (INDX2)             
067100                                    TO W-IDPRCVAR-MIN                     
067200                                       W-IDPRCVAR-MAX                     
067210          END-IF                                                          
067300         IF MFS-FIRST                                                     
067400             CONTINUE                                                     
067500          ELSE                                                            
067600             PERFORM S01-CHECK-IDPRCVAR                                   
067700         END-IF                                                           
067800         PERFORM IMS-GU-WDQ3CSEQ-ORQC-OKVAL                               
067900                                                                          
068000     END-PERFORM                                                          
068100                                                                          
068200     CALL WINTSOR USING   OPARTS-TABLE                                    
068300                          WS-LGT-RECORD                                   
068400                          WS-NUMBER-ELEM                                  
068500                          WS-SORTFIELDS (1)                               
068600                          WS-LGT-SFIELDS                                  
068700     .                                                                    
068800     EJECT                                                                
068900 FB-SORT-ORDERPARTS       SECTION.                                        
069000                                                                          
069100     MOVE ZERO                     TO INDX                                
069200     PERFORM UNTIL WS-IDORDER (1)  = HIGH-VALUES OR                       
069300                                     INDX > +13                           
069400         MOVE WS-IDORDER (1)         TO W-HELP-IDORDER                    
069500         MOVE +1 TO INDX2                                                 
069600         PERFORM UNTIL WS-IDORDER (INDX2) = HIGH-VALUES OR                
069700                     INDX  > +13                        OR                
069800                     INDX2 > 10                         OR                
069900                     WS-IDORDER (INDX2)   NOT = W-HELP-IDORDER            
070000                                                                          
070100              MOVE WS-FULL-KEY (INDX2) TO D-WDQ3CSEQ-X                    
070200              MOVE WS-IDORDER  (INDX2) TO XD-IDORDER                      
070300              MOVE D-IDDC              TO W-IDDC-MIN                      
070400                                          W-IDDC-MAX                      
070500              MOVE D-IDPRCBAS          TO W-IDPRCBAS-MIN                  
070600                                          W-IDPRCBAS-MAX                  
070700              MOVE D-IDPRCVAR          TO W-IDPRCVAR-MIN                  
070800                                          W-IDPRCVAR-MAX                  
070900                                                                          
071000              PERFORM FBA-CHECK-T-TABLE                                   
071100              IF T-TABLE-OK                                               
071200                  PERFORM FBB-SKAPA-MOD-TABELL                            
071300              END-IF                                                      
071400              PERFORM IMS-GU-WDQ3CSEQ-ORQC                                
071500              IF SEGMENT-FOUND                                            
071600                PERFORM IMS-GN-2-WDQ3CSEQ-ORQC                            
071700                IF SEGMENT-FOUND                                          
071800                   MOVE ORQA-ODEL-IDPRCBAS TO WS-IDPRCBAS (INDX2)         
071900                   MOVE ORQA-ODEL-IDORDER TO WS-IDORDER (INDX2)           
072000                   MOVE ORQA-ODEL-DARFS  TO WS-DARFS    (INDX2)           
072100                   MOVE ORQA-ODEL-DALSTORD TO WS-DALSTORD (INDX2)         
072200                   MOVE ORQC-KEY-FB-AREA TO WS-FULL-KEY (INDX2)           
072300                ELSE                                                      
072400                   MOVE HIGH-VALUES      TO OPARTS-LINE (INDX2)           
072500                END-IF                                                    
072600              ELSE                                                        
072700                 MOVE HIGH-VALUES        TO OPARTS-LINE (INDX2)           
072800              END-IF                                                      
072900           ADD +1 TO INDX2                                                
073000         END-PERFORM                                                      
073100                                                                          
073200         CALL WINTSOR USING OPARTS-TABLE                                  
073300                            WS-LGT-RECORD                                 
073400                            WS-NUMBER-ELEM                                
073500                            WS-SORTFIELDS (1)                             
073600                            WS-LGT-SFIELDS                                
073700     END-PERFORM                                                          
073800                                                                          
073900     IF SEGMENT-FOUND                                                     
074000         PERFORM FBC-SAVE-KEYS-NEXT                                       
074100      ELSE                                                                
074200         MOVE 9999999999           TO MOD-TIRFS-NEXT                      
074300                                      MOD-TILST-O-NEXT                    
074400         MOVE 9999999              TO MOD-IDORDER-NEXT                    
074500     END-IF                                                               
074600     .                                                                    
074700     EJECT                                                                
074800 FBA-CHECK-T-TABLE         SECTION.                                       
074900                                                                          
075000     MOVE YES                  TO T-TABLE-SW                              
075100     MOVE +1                   TO INDX3                                   
075200     PERFORM UNTIL INDX3       >  MAX-INDX OR                             
075300              T-IDORDER(INDX3) =  ZERO     OR                             
075400              T-TABLE-NOT-OK                                              
075500         IF WS-IDORDER(INDX2)  = T-IDORDER (INDX3)                        
075600             MOVE NOO          TO T-TABLE-SW                              
075700          ELSE                                                            
075800             ADD +1            TO INDX3                                   
075900         END-IF                                                           
076000     END-PERFORM                                                          
076100                                                                          
076200     .                                                                    
076300     EJECT                                                                
076400 FBB-SKAPA-MOD-TABELL      SECTION.                                       
076500                                                                          
076600     MOVE NOO                     TO OLD-IDORDER-SW                       
076700                                                                          
076800     MOVE HIGH-VALUE              TO W-WDQ301KY-MAX-X                     
076900     MOVE LOW-VALUE               TO W-WDQ301KY-MIN-X                     
077000     MOVE WS-IDORDER(INDX2)       TO W-Q301KY-IDORDER-MIN                 
077100                                     W-Q301KY-IDORDER-MAX                 
077200     MOVE WS-IDDC                 TO W-Q301KY-IDDC-MIN                    
077300                                     W-Q301KY-IDDC-MAX                    
077400                                                                          
077500     PERFORM IMS-GU-ORQA-WDQ301                                           
077600     PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-END                         
077700         PERFORM FBBA-CHECK-IDPRCBAS                                      
077800         IF IDPRCBAS-OK                                                   
077900             IF INDX              >  ZERO                                 
078000                 IF ORQA-ODEL-IDORDER = T-IDORDER (INDX)                  
078100                     ADD ORQA-ODEL-KVRADER                                
078200                                    TO T-KVRADER (INDX, INDX4)            
078300                     IF T-KDODELSTA (INDX, INDX4) = SPACE                 
078400                         MOVE SPACE TO T-STARSPAC (INDX, INDX4)           
078500                         MOVE ORQA-ODEL-KDODELSTA                         
078600                                      TO T-KDODELSTA(INDX, INDX4)         
078700                      ELSE                                                
078800                         PERFORM FBBB-CHECK-STATUS                        
078900                     END-IF                                               
079000                     MOVE YES     TO OLD-IDORDER-SW                       
079100                 END-IF                                                   
079200             END-IF                                                       
079300                                                                          
079400             IF OLD-IDORDER                                               
079500                 MOVE ORQA-ODEL-IDPRC                                     
079600                              TO T-IDPRC    (INDX, INDX4)                 
079700              ELSE                                                        
079800                 IF INDX          >  ZERO                                 
079900                     PERFORM FBBC-CHECK-PREV-LINE                         
080000                     IF PREV-LINE-OK                                      
080100                         ADD +1   TO INDX                                 
080200                      ELSE                                                
080300                         PERFORM FBBD-INIT-T-KDODELSTA                    
080400                     END-IF                                               
080500                  ELSE                                                    
080600                     ADD +1       TO INDX                                 
080700                 END-IF                                                   
080800                 PERFORM FBBE-MOVE-IDPRCBAS                               
080900                 MOVE ORQA-ODEL-IDORDER  TO T-IDORDER     (INDX)          
081000                 MOVE ORQA-ODEL-IDORDNR7 TO T-IDORDNR7    (INDX)          
081100                 MOVE ORQA-ODEL-IDDISTR  TO T-IDDISTR     (INDX)          
081200                 MOVE ORQA-ODEL-IDPRODNR TO T-IDPRODNR    (INDX)          
081300                 MOVE ORQA-ODEL-IDKUNDNR TO T-IDKUNDNR    (INDX)          
081400                 MOVE ORQA-ODEL-DARFS    TO T-DARFS       (INDX)          
081500                 MOVE ORQA-ODEL-DALSTORD TO T-DALSTORD    (INDX)          
081600                 MOVE ORQA-ODEL-IDPRC                                     
081700                                    TO T-IDPRC    (INDX, INDX4)           
081800                 MOVE ORQA-ODEL-KVRADER                                   
081900                                    TO T-KVRADER  (INDX, INDX4)           
082000                 MOVE ORQA-ODEL-KDODELSTA                                 
082100                                    TO T-KDODELSTA (INDX, INDX4)          
082200                 MOVE SPACE         TO T-STARSPAC  (INDX, INDX4)          
082300             END-IF                                                       
082400         END-IF                                                           
082500         PERFORM IMS-GN-ORQA-WDQ301                                       
082600     END-PERFORM                                                          
082700                                                                          
082800     IF INDX > 0                                                          
082900        PERFORM FBBC-CHECK-PREV-LINE                                      
083000        IF NOT PREV-LINE-OK                                               
083100           PERFORM FBBD-INIT-T-KDODELSTA                                  
083200           MOVE 0 TO T-IDORDNR7 (INDX)                                    
083300        END-IF                                                            
083400     END-IF                                                               
083500     .                                                                    
083600     EJECT                                                                
083700 FBBA-CHECK-IDPRCBAS         SECTION.                                     
083800                                                                          
083900     MOVE YES                     TO IDPRCBAS-SW                          
084000     MOVE +1                      TO INDX4                                
084100     PERFORM UNTIL INDX4          > 10          OR                        
084200             ORQA-ODEL-IDPRC      =  MOD-IDPRC-LINE (INDX4)               
084300         ADD +1                   TO INDX4                                
084400     END-PERFORM                                                          
084500                                                                          
084600     IF INDX4                     = +11                                   
084700         MOVE NOO                 TO IDPRCBAS-SW                          
084800     END-IF                                                               
084900     .                                                                    
085000     EJECT                                                                
085100 FBBB-CHECK-STATUS           SECTION.                                     
085200                                                                          
085300     EVALUATE ORQA-ODEL-KDODELSTA                                         
085400     WHEN 'R'                                                             
085500        IF T-KDODELSTA(INDX, INDX4) =  SPACE                              
085600            MOVE 'R'                TO T-KDODELSTA(INDX, INDX4)           
085700            MOVE SPACE              TO T-STARSPAC (INDX, INDX4)           
085800         ELSE                                                             
085900            IF T-KDODELSTA(INDX, INDX4) = 'U' OR 'P'                      
086000                MOVE 'R'            TO T-KDODELSTA(INDX, INDX4)           
086100                MOVE '*'            TO T-STARSPAC (INDX, INDX4)           
086200            END-IF                                                        
086300        END-IF                                                            
086400                                                                          
086500     WHEN 'U'                                                             
086600        EVALUATE T-KDODELSTA(INDX, INDX4)                                 
086700          WHEN SPACE                                                      
086800             MOVE 'U'          TO T-KDODELSTA(INDX, INDX4)                
086900             MOVE SPACE        TO T-STARSPAC (INDX, INDX4)                
087000          WHEN 'R'                                                        
087100             MOVE 'R'          TO T-KDODELSTA(INDX, INDX4)                
087200             MOVE '*'          TO T-STARSPAC (INDX, INDX4)                
087300          WHEN 'P'                                                        
087400             MOVE 'U'          TO T-KDODELSTA(INDX, INDX4)                
087500             MOVE '*'          TO T-STARSPAC (INDX, INDX4)                
087600        END-EVALUATE                                                      
087700                                                                          
087800     WHEN 'P'                                                             
087900        EVALUATE T-KDODELSTA(INDX, INDX4)                                 
088000          WHEN  SPACE                                                     
088100             MOVE 'P'          TO T-KDODELSTA(INDX, INDX4)                
088200             MOVE SPACE        TO T-STARSPAC (INDX, INDX4)                
088300          WHEN 'R'                                                        
088400             MOVE 'R'          TO T-KDODELSTA(INDX, INDX4)                
088500             MOVE '*'          TO T-STARSPAC (INDX, INDX4)                
088600          WHEN 'U'                                                        
088700             MOVE 'U'          TO T-KDODELSTA(INDX, INDX4)                
088800             MOVE '*'          TO T-STARSPAC (INDX, INDX4)                
088900        END-EVALUATE                                                      
089000     END-EVALUATE                                                         
089100     .                                                                    
089200     EJECT                                                                
089300 FBBC-CHECK-PREV-LINE        SECTION.                                     
089400                                                                          
089500*    CHECK IF PREVIOUS LINE SHALL BE SHOWN. LINES WITH                    
089600*    ONLY STATUS R OR P SHALL NOT BE SHOWN                                
089700                                                                          
089800     MOVE NOO                     TO PREV-LINE-SW                         
089900                                     KDODELSTA-R-SW                       
090000                                     KDODELSTA-P-SW                       
090100     MOVE +1                      TO INDX3                                
090200     PERFORM UNTIL INDX3          > 10 OR PREV-LINE-OK                    
090300         IF ((T-KDODELSTA (INDX, INDX3) = 'R' OR 'P') AND                 
090400              T-STARSPAC  (INDX, INDX3) = SPACE)          OR              
090500              T-KDODELSTA (INDX, INDX3) = SPACE                           
090600              IF T-KDODELSTA (INDX, INDX3) = 'R'                          
090700                 MOVE YES TO KDODELSTA-R-SW                               
090800              END-IF                                                      
090900              IF T-KDODELSTA (INDX, INDX3) = 'P'                          
091000                 MOVE YES TO KDODELSTA-P-SW                               
091100              END-IF                                                      
091200              ADD +1              TO INDX3                                
091300          ELSE                                                            
091400              MOVE YES            TO PREV-LINE-SW                         
091500         END-IF                                                           
091600     END-PERFORM                                                          
091700                                                                          
091800     IF PREV-LINE-SW = NOO                                                
091900        IF KDODELSTA-R-SW = YES AND KDODELSTA-P-SW = YES                  
092000           MOVE YES            TO PREV-LINE-SW                            
092100        END-IF                                                            
092200     END-IF                                                               
092300     .                                                                    
092400     EJECT                                                                
092500 FBBD-INIT-T-KDODELSTA      SECTION.                                      
092600                                                                          
092700     MOVE +1                      TO INDX3                                
092800     PERFORM UNTIL INDX3          > 10                                    
092900         MOVE ZERO                TO T-KVRADER  (INDX, INDX3)             
093000         MOVE SPACE               TO T-KDODELSTA(INDX, INDX3)             
093100                                     T-STARSPAC (INDX, INDX3)             
093200         ADD +1                   TO INDX3                                
093300     END-PERFORM                                                          
093400     .                                                                    
093500     EJECT                                                                
093600 FBBE-MOVE-IDPRCBAS         SECTION.                                      
093700                                                                          
093800     MOVE +1                      TO INDX3                                
093900     PERFORM UNTIL INDX3          > 10 OR                                 
094000                   MOD-IDPRC-LINE(INDX3) = SPACE                          
094100         MOVE MOD-IDPRC-LINE (INDX3) TO T-IDPRC   (INDX, INDX3)           
094200         ADD +1                   TO INDX3                                
094300     END-PERFORM                                                          
094400     .                                                                    
094500     EJECT                                                                
094600 FBC-SAVE-KEYS-NEXT      SECTION.                                         
094700                                                                          
094800     MOVE ORQA-ODEL-IDORDER    TO MOD-IDORDER-NEXT                        
094900     MOVE ORQA-ODEL-DARFS      TO W-DARFS-NEXT                            
095000     MOVE W-DARFS-NEXT (3:10)  TO MOD-TIRFS-NEXT                          
095100     MOVE ORQA-ODEL-DALSTORD   TO W-DALSTORD-NEXT                         
095200     MOVE W-DALSTORD-NEXT (3:10) TO MOD-TILST-O-NEXT                      
095300     MOVE +1                   TO INDX3                                   
095400     PERFORM UNTIL INDX3       >  +10                                     
095500         MOVE +1               TO INDX4                                   
095600         PERFORM UNTIL INDX4   >  +10                                     
095700             IF MOD-IDPRCBAS        IN MOD-IDPRC-LINE(INDX3)              
095800                                    = WS-IDPRCBAS(INDX4) AND              
095900                MOD-IDPRCVAR        IN MOD-IDPRC-LINE(INDX3)              
096000                                    = WS-IDPRCVAR(INDX4)                  
096100                 MOVE WS-IDPRCVAR(INDX4) TO                               
096200                                    MOD-IDPRCVAR-NEXT(INDX3)              
096300                 MOVE +11      TO INDX4                                   
096400              ELSE                                                        
096500                 ADD +1        TO INDX4                                   
096600             END-IF                                                       
096700         END-PERFORM                                                      
096800         ADD +1                TO INDX3                                   
096900     END-PERFORM                                                          
097000     MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                            
097100     CALL WMEDKONV USING MED-WMEDAREA                                     
097200     MOVE MED-MFSINF           TO MOD-TEMFSINF                            
097300     .                                                                    
097400 FC-MOVE-TAB-TO-MOD      SECTION.                                         
097500                                                                          
097600     MOVE +1 TO INDX                                                      
097700                                                                          
097800     PERFORM UNTIL INDX        > MAX-INDX                                 
097900         IF INDX                  =  1                                    
098000           MOVE T-IDORDER(INDX)   TO MOD-IDORDER-ENTER                    
098100           MOVE T-DARFS(INDX)     TO WS-DARFS (1)                         
098200           MOVE WS-DARFS (1) (3:10) TO MOD-TIRFS-ENTER                    
098300           MOVE T-DALSTORD (INDX) TO WS-DALSTORD (1)                      
098400           MOVE WS-DALSTORD (1) (3:10) TO MOD-TILST-O-ENTER               
098500         END-IF                                                           
098600         IF T-IDORDNR7 (INDX)      NOT = ZERO                             
098700             MOVE T-IDORDNR7 (INDX) TO MOD-IDORDNR7 (INDX)                
098800             MOVE T-IDDISTR  (INDX) TO MOD-IDDISTR  (INDX)                
098900             MOVE T-IDPRODNR (INDX) TO MOD-IDPRODNR (INDX)                
099000             MOVE T-IDKUNDNR (INDX) TO MOD-IDKUNDNR (INDX)                
099100                                                                          
099200             MOVE +1 TO INDX2                                             
099300             PERFORM UNTIL INDX2    > 10                                  
099400                 MOVE T-KVRADER   (INDX, INDX2) TO                        
099500                      WS-KVRADER-X  (INDX2)                               
099600                 ADD +1             TO INDX2                              
099700             END-PERFORM                                                  
099800             MOVE WS-KVRADER-LINE   TO MOD-KVRADER-LINE (INDX)            
099900          ELSE                                                            
100000             MOVE MFS-RENSA-FAELT   TO MOD-IDORDNR7 (INDX)                
100100                                       MOD-IDDISTR  (INDX)                
100200                                       MOD-IDPRODNR (INDX)                
100300                                       MOD-IDKUNDNR (INDX)                
100400         END-IF                                                           
100500         ADD +1 TO INDX                                                   
100600     END-PERFORM                                                          
100700     .                                                                    
100800     EJECT                                                                
100900                                                                          
101000 S01-CHECK-IDPRCVAR  SECTION.                                             
101100                                                                          
101101     IF INDX2 > 10                                                        
101103       MOVE HIGH-VALUE                      TO W-IDPRCVAR-MIN             
101104       MOVE SPACE                           TO W-IDPRCVAR-MIN             
101105     ELSE                                                                 
101200       IF MFS-ENTER                                                       
101300           IF MID-IDPRCVAR-ENTER(INDX2) = SPACE                           
101400               MOVE HIGH-VALUE              TO W-IDPRCVAR-MIN             
101500            ELSE                                                          
101600               MOVE MID-IDPRCVAR-ENTER(INDX2) TO W-IDPRCVAR-MIN           
101700           END-IF                                                         
101800        ELSE                                                              
101900           IF MID-IDPRCVAR-NEXT(INDX2)      = SPACE                       
102000               MOVE HIGH-VALUE              TO W-IDPRCVAR-MIN             
102100            ELSE                                                          
102200               MOVE MID-IDPRCVAR-NEXT(INDX2) TO W-IDPRCVAR-MIN            
102300           END-IF                                                         
102400       END-IF                                                             
102410     END-IF                                                               
102500     .                                                                    
102600 MFS-ERASE-FIELD-OUT SECTION.                                             
102700                                                                          
102800*    --- ALL OUTDATA-FIELD                                                
102900*    --- INCL SCROLLKEYS                                                  
103000     MOVE MFS-ERASE-FIELD TO MOD-KDPRCGRP-OUT                             
103100                             MOD-IDORDER-ENTER                            
103200                             MOD-IDORDER-NEXT                             
103300                             MOD-TIRFS-ENTER                              
103400                             MOD-TIRFS-NEXT                               
103500                             MOD-TILST-O-ENTER                            
103600                             MOD-TILST-O-NEXT                             
103700     .                                                                    
103800     EJECT                                                                
103900 MFS-ERASE-FIELD-IN SECTION.                                              
104000                                                                          
104100*    --- ALL INPUT-FIELDS                                                 
104200     MOVE MFS-ERASE-FIELD TO MOD-KDPRCGRP-IN                              
104300                             MOD-KDPRODKL-IN                              
104400                             MOD-IDPRC-IN                                 
104500                             MOD-IDDC-IN                                  
104600     .                                                                    
104700     EJECT                                                                
104800* --- IMS SECTIONS ---                                                    
104900     SKIP3                                                                
105000 IMS-GET-MSG SECTION.                                                     
105100                                                                          
105200     MOVE '  QC' TO GOOD-STATUSCODES                                      
105300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
105400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
105500     PERFORM IMS-STATUSCONTROL                                            
105600     .                                                                    
105700                                                                          
105800 IMS-INSERT-MSG SECTION.                                                  
105900                                                                          
106000     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
106100       MOVE '0' TO MFS-KDHUVOMR                                           
106200     END-IF                                                               
106300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
106400     MOVE SPACE TO GOOD-STATUSCODES                                       
106500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
106600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
106700     PERFORM IMS-STATUSCONTROL                                            
106800     .                                                                    
106900     EJECT                                                                
107000 IMS-GU-XXKH01 SECTION.                                                   
107100                                                                          
107200     STRING 'WLXXKH01(WDGXKEY  =' W-4447-X ')'                            
107300          DELIMITED BY SIZE INTO SSA1                                     
107400     MOVE '  GE' TO GOOD-STATUSCODES                                      
107500     CALL CBLTDLI USING GU XXKH-PCB DLI-IO-AREA SSA1                      
107600     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
107700     PERFORM IMS-STATUSCONTROL                                            
107800     .                                                                    
107900                                                                          
108000 IMS-GNP-XXKH11 SECTION.                                                  
108100                                                                          
108200     STRING 'WLXXKH11(WDGXKEY >=' W-4448-X                                
108300                    '&KDPRCGRP =' KDPRCGRP-WS                             
108400                    '&KDPRODKL =' KDPRODKL-WS ')'                         
108500          DELIMITED BY SIZE INTO SSA1                                     
108600     MOVE '  GE' TO GOOD-STATUSCODES                                      
108700     CALL CBLTDLI USING GNP XXKH-PCB DLI-IO-AREA SSA1                     
108800     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
108900     PERFORM IMS-STATUSCONTROL                                            
109000     .                                                                    
109100                                                                          
109200 IMS-GNP-XXKH11-NDC-US  SECTION.                                          
109300                                                                          
109400     STRING 'WLXXKH11(WDGXKEY >=' W-4448-X                                
109500                    '&KDPRCGRP =' KDPRCGRP-WS                             
109600                    '&KDPRODKL >' KDPRODKL-WS ')'                         
109700          DELIMITED BY SIZE INTO SSA1                                     
109800     MOVE '  GE' TO GOOD-STATUSCODES                                      
109900     CALL CBLTDLI USING GNP XXKH-PCB DLI-IO-AREA SSA1                     
110000     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
110100     PERFORM IMS-STATUSCONTROL                                            
110200     .                                                                    
110300                                                                          
110400 IMS-GU-ORQA-WDQ301    SECTION.                                           
110500                                                                          
110600     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                        
110700                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
110800          DELIMITED BY SIZE INTO SSA1                                     
110900     MOVE '  GE' TO GOOD-STATUSCODES                                      
111000     CALL CBLTDLI USING GU ORQA-PCB DLI-IO-AREA SSA1                      
111100     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
111200     PERFORM IMS-STATUSCONTROL                                            
111300     .                                                                    
111400                                                                          
111500 IMS-GN-ORQA-WDQ301 SECTION.                                              
111600                                                                          
111700     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                        
111800                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
111900          DELIMITED BY SIZE INTO SSA1                                     
112000     MOVE '  GBGE' TO GOOD-STATUSCODES                                    
112100     CALL CBLTDLI USING GN ORQA-PCB DLI-IO-AREA SSA1                      
112200     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
112300     PERFORM IMS-STATUSCONTROL                                            
112400     .                                                                    
112500     EJECT                                                                
112600 IMS-GU-WDQ3CSEQ-ORQC-OKVAL SECTION.                                      
112700                                                                          
112800     STRING 'WLORQA01(WDQ3CSEQ=>' W-WDQ3CSEQ-MIN                          
112900                    '&WDQ3CSEQ<=' W-WDQ3CSEQ-MAX                          
113000                    '&IDORDER >=' W-IDORDER-X ')'                         
113100          DELIMITED BY SIZE INTO SSA1                                     
113200     MOVE '  GBGE' TO GOOD-STATUSCODES                                    
113300     CALL CBLTDLI USING GU ORQC-PCB DLI-IO-AREA SSA1                      
113400     MOVE ORQC-STATUS-CODE TO STATUS-WS                                   
113500     PERFORM IMS-STATUSCONTROL                                            
113600     .                                                                    
113700                                                                          
113800 IMS-GU-WDQ3CSEQ-ORQC SECTION.                                            
113900                                                                          
114000     STRING 'WLORQA01(WDQ3CSEQ =' D-WDQ3CSEQ-X ')'                        
114100          DELIMITED BY SIZE INTO SSA1                                     
114200     MOVE '  GE'   TO GOOD-STATUSCODES                                    
114300     CALL CBLTDLI USING GU ORQC-PCB DLI-IO-AREA SSA1                      
114400     MOVE ORQC-STATUS-CODE TO STATUS-WS                                   
114500     PERFORM IMS-STATUSCONTROL                                            
114600     .                                                                    
114700                                                                          
114800 IMS-GN-2-WDQ3CSEQ-ORQC SECTION.                                          
114900                                                                          
115000     STRING 'WLORQA01(WDQ3CSEQ>=' W-WDQ3CSEQ-MIN                          
115100                    '&WDQ3CSEQ<=' W-WDQ3CSEQ-MAX                          
115200                    '&IDORDER NE' D-IDORDER-X ')'                         
115300          DELIMITED BY SIZE INTO SSA2                                     
115400     MOVE '  GBGE' TO GOOD-STATUSCODES                                    
115500     CALL CBLTDLI USING GN ORQC-PCB DLI-IO-AREA SSA2                      
115600     MOVE ORQC-STATUS-CODE TO STATUS-WS                                   
115700     PERFORM IMS-STATUSCONTROL                                            
115800     .                                                                    
115900     EJECT                                                                
116000 IMS-STATUSCONTROL SECTION.                                               
116100                                                                          
116200     SET STATUS-IX TO 1                                                   
116300     SEARCH GOOD-STATUS                                                   
116400       AT END CALL FELLOG                                                 
116500       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
116600     END-SEARCH                                                           
116700     .                                                                    
