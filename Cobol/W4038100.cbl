000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4038100.                                                
000400 AUTHOR.         LUC FEYS.                                                
000500 DATE-WRITTEN.   90/06/29.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNCTION.                                                            
001000*        SEE SPECIFICATIONS                                               
001100*          LÄSER DE PRCER SOM HÖR TILL EN AVD (PRCGRP), OCH VISAR         
001200*          DE ORDER SOM FINNS I PRC:ERNA I PRIORITETSORDNING.             
001300*          STATUS I VARJE PRC VISAS.                                      
001400*                                                                         
001500*        THE PROGRAM IS A  QUERY    MPP                                   
001600*        PROGRAM READS   WLXXKH (WDR1)                                    
001700*        PROGRAM READS   WLORQA (WDQ3)                                    
001800*                                                                         
001900*                                                                         
002000*    INDATA.                                                              
002100*        TRANSACTION: W4T381                                              
002200*        MID:         W4I38101                                            
002300*                                                                         
002400*   OUTDATA.                                                              
002500*        MOD:         W4O38101                                            
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100 WORKING-STORAGE SECTION.                                                 
003200                                                                          
003300*    -- CHECKED BY WY2000                                                 
003400 77  IDPGM                       PIC X(08)   VALUE 'W4038100'.            
003500                                                                          
003600 77  YES                         PIC X       VALUE 'J'.                   
003700 77  NOO                         PIC X       VALUE 'N'.                   
003800                                                                          
003900*    --- INDEX FOR BROWSE LINES                                           
004000 77  INDX                        PIC S9(4)  VALUE +0   COMP SYNC.         
004100 77  INDX2                       PIC S9(4)  VALUE +0   COMP SYNC.         
004200 77  INDX3                       PIC S9(4)  VALUE +0   COMP SYNC.         
004300 77  INDX4                       PIC S9(4)  VALUE +0   COMP SYNC.         
004400 77  MAX-INDX                    PIC S9(4)  VALUE +14  COMP SYNC.         
004500                                                                          
004600*      --- VALID IDDC CODES                                               
004700*                                                                         
004800*01    -COPY WWDC99                                                       
004900       EJECT                                                              
005800 77  W-HELP-IDORDER              PIC S9(7)  VALUE +0    COMP-3.           
005900 77  W-DARFS-NEXT                PIC  9(12) VALUE ZERO.                   
006000 77  W-DALSTORD-NEXT             PIC  9(12) VALUE ZERO.                   
006100                                                                          
006200*    --- WORKFIELDS FOR ACTUAL KEYVALUES OF SCREENS                       
006300                                                                          
006400 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006500     88  INDATA-OK                           VALUE 'J'.                   
006600     88  INDATA-WRONG                        VALUE 'N'.                   
006700                                                                          
006800 77  KEYS-SW                     PIC X       VALUE 'J'.                   
006900     88  KEYS-OK                             VALUE 'J'.                   
007000     88  KEYS-WRONG                          VALUE 'N'.                   
007100                                                                          
007200 77  ALLT-SW                     PIC X       VALUE 'J'.                   
007300     88  ALLT-OK                             VALUE 'J'.                   
007400                                                                          
007500 77  OLD-IDORDER-SW              PIC X       VALUE 'N'.                   
007600     88  OLD-IDORDER                         VALUE 'J'.                   
007700                                                                          
007800 77  PREV-LINE-SW                PIC X       VALUE 'N'.                   
007900     88  PREV-LINE-OK                        VALUE 'J'.                   
008000                                                                          
008100 77  KDODELSTA-R-SW              PIC X       VALUE 'N'.                   
008200                                                                          
008300 77  KDODELSTA-P-SW              PIC X       VALUE 'N'.                   
008400                                                                          
008500 77  T-TABLE-SW                  PIC X       VALUE 'N'.                   
008600     88  T-TABLE-OK                          VALUE 'J'.                   
008700     88  T-TABLE-NOT-OK                      VALUE 'N'.                   
008800                                                                          
008900 77  IDPRCBAS-SW                 PIC X       VALUE 'N'.                   
009000     88  IDPRCBAS-OK                         VALUE 'J'.                   
009100                                                                          
009200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
009300     88  OWN-MID                             VALUE '4381'.                
009400     88  GOOD-MID                            VALUE '4381' '4382'          
009500                                                   '4384'.                
009600     EJECT                                                                
009700*    --- SUBPROGRAM AND PARAMETER-AREAS                                   
009800 01  GENERAL-SUBPROGRAM.                                                  
009900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
010000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010200     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR '.            
010300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
010400*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
010500*01 -COPY WMSGINIT                                                        
010600     EJECT                                                                
010700*    --- PARAMETERS FOR  SUBPROGRAM WMEDKONV                              
010800*   -COPY WMEDAREA                                                        
010900     SKIP3                                                                
011000 01  MESSAGE-CODES.                                                       
011100     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
011200     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
011300     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
011400     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
011500     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
011600     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
011700     03  ERR-LAST-PAGE-SHOWED    PIC X(3)    VALUE '115'.                 
011800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
011900     EJECT                                                                
012000*    --- AREAS FOR MFS AND SCREENHANDLING                                 
012100*                                                                         
012200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
012300     SKIP3                                                                
012400*01  MID -COPY W4I38101                                                   
012500     EJECT                                                                
012600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
012700     SKIP3                                                                
012800*01  -COPY WMSGAREA                                                       
012900     EJECT                                                                
013000*    03  MOD -COPY W4O38101   -RED MSG-AREA.                              
013100     EJECT                                                                
013200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
013300     SKIP3                                                                
013400*01  -COPY WMFSAREA                                                       
013500     EJECT                                                                
013600*    --- WORK-AREAS FOR IMS-SECTIONS                                      
013700*                                                                         
013800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013900     SKIP3                                                                
014000 01  KEYS-TILL-DLI.                                                       
014100     03  W-4447-X.                                                        
014200         05  W-IDHTYP            PIC X(4)     VALUE '4447'.               
014300         05  W-4447-IDDC         PIC X(2).                                
014400         05  W-4447-LOW-VALUE    PIC X(24)    VALUE LOW-VALUE.            
014500     03  W-4448-X.                                                        
014600         05  W-4448-IDPRC        PIC X(4)    VALUE LOW-VALUE.             
014700         05  W-4448-LOW-VALUE    PIC X(1)    VALUE LOW-VALUE.             
014800     03  KDPRCGRP-WS             PIC X(5).                                
014900     03  KDPRODKL-WS             PIC X(1).                                
015000     03  IDPRC-WS                PIC X(4).                                
015100*                                                                         
015200*    DIRECT KEY                                                           
015300*                                                                         
015400     03  W-WDQ301KY-MIN-X.                                                
015500         05 W-Q301KY-IDORDER-MIN     PIC S9(7)  COMP-3.                   
015600         05 W-Q301KY-IDDC-MIN        PIC X(2).                            
015700         05 W-Q301KY-IDPRODNR-MIN    PIC X(4)   VALUE LOW-VALUE.          
015800         05 W-Q301KY-IDPLKLST-MIN    PIC X(2)   VALUE LOW-VALUE.          
015900*                                                                         
016000     03  W-WDQ301KY-MAX-X.                                                
016100         05 W-Q301KY-IDORDER-MAX     PIC S9(7)  COMP-3.                   
016200         05 W-Q301KY-IDDC-MAX        PIC X(2).                            
016300         05 W-Q301KY-IDPRODNR-MAX    PIC X(4)   VALUE HIGH-VALUE.         
016400         05 W-Q301KY-IDPLKLST-MAX    PIC X(2)   VALUE HIGH-VALUE.         
016500*                                                                         
016600     03  D-WDQ3CSEQ-X.                                                    
016700        05 D-IDDC-X.                                                      
016800            07 D-IDDC            PIC X(2).                                
016900        05 D-IDPRCBAS-X.                                                  
017000            07 D-IDPRCBAS        PIC X(3).                                
017100        05 D-IDPRCVAR-X.                                                  
017200            07 D-IDPRCVAR        PIC X(1).                                
017300        05 D-DARFS-X.                                                     
017400            07 D-DARFS           PIC  9(12).                              
017500        05 D-DALSTORD-X.                                                  
017600            07 D-DALSTORD        PIC  9(12).                              
017700        05 D-IDORDER-X.                                                   
017800            07 D-IDORDER         PIC S9(7)    COMP-3.                     
017900     03  XD-IDORDER-X.                                                    
018000         05 XD-IDORDER           PIC S9(7)    COMP-3.                     
018100     03  W-KDODELST-U            PIC X(1)     VALUE 'U'.                  
018200     03  W-KDODELST-P            PIC X(1)     VALUE 'P'.                  
018300*                                                                         
018400*    SECONDARY KEY                                                        
018500*                                                                         
018600     03  W-WDQ3CSEQ-MIN.                                                  
018700         05 W-IDDC-MIN           PIC  X(2).                               
018800         05 W-IDPRCBAS-MIN       PIC  X(3).                               
018900         05 W-IDPRCVAR-MIN       PIC  X(1).                               
019000         05 W-DARFS-MIN          PIC  9(12).                              
019100         05 W-DALSTORD-MIN       PIC  9(12).                              
019200         05 W-IDORDER-MIN        PIC  X(4)   VALUE LOW-VALUE.             
019300*                                                                         
019400     03  W-WDQ3CSEQ-MAX.                                                  
019500         05 W-IDDC-MAX           PIC  X(2).                               
019600         05 W-IDPRCBAS-MAX       PIC  X(3).                               
019700         05 W-IDPRCVAR-MAX       PIC  X(1).                               
019800         05 FILLER               PIC  X(28)  VALUE HIGH-VALUE.            
019900*                                                                         
020000     03  W-IDORDER-X.                                                     
020100         05 W-IDORDER            PIC S9(07)  VALUE ZERO  COMP-3.          
020200*                                                                         
020300     03  W-IDDC-X.                                                        
020400         05 W-IDDC               PIC  X(02).                              
020500****************************                                              
020600* ORDERPARTS PRIORITY TABLE                                               
020700****************************                                              
020800 01 FILLER     PIC X(16) VALUE 'ORDERPARTSPRTY'.                          
020900 01 OPARTS-TABLE.                                                         
021000     03 OPARTS-LINE OCCURS 10 INDEXED BY N.                               
021100*                                                                         
021200        05 WS-SORTFIELDS.                                                 
021300           07 WS-DARFS          PIC 9(12).                                
021400           07 WS-DALSTORD       PIC 9(12).                                
021500           07 WS-IDORDER        PIC 9(7).                                 
021600        05 WS-IDPRCBAS          PIC X(3).                                 
021700        05 WS-FULL-KEY.                                                   
021800           07 FILLER            PIC X(05).                                
021900           07 WS-IDPRCVAR       PIC X(01).                                
022000           07 FILLER            PIC X(22).                                
022100*                                                                         
022200*                                                                         
022300* SORT PARAMETERS FOR OPARTS-TABLE                                        
022400*                                                                         
022500*                                                                         
022600 01 FILLER PIC X(16) VALUE 'SORT PARAMETERS'.                             
022700 01 WS-TABLE.                                                             
022800     03 WS-LGT-RECORD           PIC S9(9)    VALUE +62 COMP SYNC.         
022900     03 WS-NUMBER-ELEM          PIC S9(9)    VALUE +10 COMP SYNC.         
023000     03 WS-LGT-SFIELDS          PIC S9(9)    VALUE +31 COMP SYNC.         
023100*********************************                                         
023200* IDORDERS TO BE DISPLAYED TABLE                                          
023300*********************************                                         
023400 01 FILLER     PIC X(16) VALUE 'IDORDERTABLE  '.                          
023500 01 IDORDER-TABLE.                                                        
023600     03 IDORDER-LINE OCCURS 14.                                           
023700*                                                                         
023800         05 T-IDORDER           PIC 9(7).                                 
023900         05 T-IDORDNR7          PIC 9(7).                                 
024000         05 T-IDDISTR           PIC 9(5).                                 
024100         05 T-IDKUNDNR          PIC 9(7).                                 
024200         05 T-IDPRODNR          PIC 9(7).                                 
024300         05 T-DARFS             PIC 9(12).                                
024400         05 T-DALSTORD          PIC 9(12).                                
024500         05 FILLER  OCCURS 10.                                            
024600           07 T-IDPRC.                                                    
024700            09 T-IDPRCBAS       PIC X(3).                                 
024800            09 T-IDPRCVAR       PIC X(1).                                 
024900           07 T-KDODELSTA       PIC X(1).                                 
025000           07 T-STARSPAC        PIC X(1).                                 
025100*                                                                         
025200*                                                                         
025300*    --- STATUS-CODE FROM IMS                                             
025400 01  STATUS-WS                   PIC XX.                                  
025500     88  SEGMENT-FOUND                       VALUE '  '.                  
025600     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
025700     88  SEGMENT-MISSING                     VALUE 'GE'.                  
025800     88  SEGMENT-END                         VALUE 'GB'.                  
025900     SKIP2                                                                
026000 01  GOOD-STATUSCODES.                                                    
026100     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
026200     SKIP3                                                                
026300 01  SSA1                        PIC X(140).                              
026400 01  SSA2                        PIC X(140).                              
026500     EJECT                                                                
026600*    --- IMS FUNCTIONCODES                                                
026700*01  -COPY W0003                                                          
026800     EJECT                                                                
026900*    ---  DLI INPUT-OUTPUT AREA                                           
027000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
027100     SKIP3                                                                
027200 01  DLI-IO-AREA.                                                         
027300     03  IO-AREA                 PIC X(3120) VALUE SPACE.                 
027400     SKIP3                                                                
027500     03  WLXXKH01 REDEFINES IO-AREA.                                      
027600*        05  -COPY WDGX4447   -PRE XXKH-                                  
027700     EJECT                                                                
027800     03  WLXXKH11 REDEFINES IO-AREA.                                      
027900*        05  -COPY WDGX4448   -PRE XXKH-                                  
028000     SKIP3                                                                
028100     03  WLORQA01 REDEFINES IO-AREA.                                      
028200*        05  -COPY WDQ301     -PRE ORQA-                                  
028300     EJECT                                                                
028400 LINKAGE SECTION.                                                         
028500                                                                          
028600*01  -COPY W0009      -PRE MSG-                                           
028700     EJECT                                                                
028800*01  -COPY W0008      -PRE USEA-                                          
028900     05  FILLER                  PIC X.                                   
029000     EJECT                                                                
029100*01  -COPY W0008      -PRE XXKH-                                          
029200     05  FILLER                  PIC X.                                   
029300     EJECT                                                                
029400*01  -COPY W0008      -PRE ORQC-                                          
029500     05  FILLER                  PIC X(28).                               
029600     EJECT                                                                
029700*01  -COPY W0008      -PRE ORQA-                                          
029800     05  FILLER                  PIC X.                                   
029900     EJECT                                                                
030000 PROCEDURE DIVISION  USING MSG-PCB  USEA-PCB                              
030100                           XXKH-PCB ORQC-PCB ORQA-PCB.                    
030200     ENTRY 'DLITCBL' USING MSG-PCB  USEA-PCB                              
030300                           XXKH-PCB ORQC-PCB ORQA-PCB.                    
030400                                                                          
030500     PERFORM IMS-GET-MSG                                                  
030600     IF SEGMENT-FOUND                                                     
030700       PERFORM A-INIT                                                     
030800       PERFORM B-CONTROL-KEYS                                             
030900       IF KEYS-OK                                                         
031000          PERFORM BA-GET-PRCBAS                                           
031100          IF ALLT-OK                                                      
031200             IF MFS-FIRST                                                 
031300               PERFORM C-FIRST-PAGE                                       
031400             ELSE                                                         
031500               IF MFS-NEXT                                                
031600                 PERFORM D-NEXT-PAGE                                      
031700               ELSE                                                       
031800                 PERFORM E-SAME-PAGE                                      
031900               END-IF                                                     
032000             END-IF                                                       
032100             IF ALLT-OK                                                   
032200               PERFORM F-READ-SHOW-INFO                                   
032300             END-IF                                                       
032400          END-IF                                                          
032500       END-IF                                                             
032600       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O38101 + 4                      
032700       PERFORM IMS-INSERT-MSG                                             
032800     END-IF                                                               
032900                                                                          
033000     MOVE ZERO TO RETURN-CODE                                             
033100     GOBACK                                                               
033200     .                                                                    
033300     EJECT                                                                
033400 A-INIT SECTION.                                                          
033500                                                                          
033600     IF MSG-DOUBLE-TRANSACTIONS                                           
033700       MOVE MSG-INDATA-MINUS-2-TRANSKODER   TO MID-W4I38101               
033800       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
033900       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
034000     ELSE                                                                 
034100       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I38101                  
034200       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
034300       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
034400     END-IF                                                               
034500                                                                          
034600     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
034700     MOVE MSG-IDPFK TO MFS-IDPFK                                          
034800     MOVE MFS-IDTRANS TO W-IDTRANS                                        
034900                                                                          
035000     MOVE LOW-VALUE TO MSG-AREA                                           
035100     MOVE 'W4O381N1' TO MFS-IDMOD                                         
035200     MOVE '4381' TO MOD-IDTRANS                                           
035300     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
035400                                                                          
035500     IF NOT OWN-MID                                                       
035600       MOVE SPACE TO MFS-KDTRTYP                                          
035700       MOVE '7' TO MFS-IDPFK                                              
035800     END-IF                                                               
035900                                                                          
036000     MOVE HIGH-VALUE TO W-WDQ3CSEQ-MAX                                    
036100     MOVE HIGH-VALUES TO OPARTS-TABLE                                     
036200     MOVE LOW-VALUE TO W-WDQ3CSEQ-MIN                                     
036300                                                                          
036400     MOVE +1                   TO INDX2                                   
036500     PERFORM UNTIL INDX2       >  +10                                     
036600         MOVE HIGH-VALUE       TO OPARTS-LINE(INDX2)                      
036700         MOVE SPACE            TO MOD-IDPRCVAR-NEXT(INDX2)                
036800                                  MOD-IDPRCVAR-ENTER(INDX2)               
036900         ADD +1                TO INDX2                                   
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
038300             MOVE SPACE        TO T-IDPRC     (INDX, INDX4)               
038400                                  T-KDODELSTA (INDX, INDX4)               
038500                                  T-STARSPAC  (INDX, INDX4)               
038600             ADD +1            TO INDX4                                   
038700         END-PERFORM                                                      
038800         ADD +1                TO INDX                                    
038900     END-PERFORM                                                          
039000     .                                                                    
039100     EJECT                                                                
039200 B-CONTROL-KEYS SECTION.                                                  
039300                                                                          
039400     MOVE ALL '+'           TO MSGI-WMSGINIT                              
039500     MOVE '001'             TO MSGI-KDCALL                                
039600     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
039700     MOVE '4381'            TO MSGI-IDTRANS                               
039800     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
039900     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
040000     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
040100                                                                          
040200     MOVE YES TO KEYS-SW                                                  
040300                                                                          
040400     IF MID-KDPRCGRP-IN = ALL '+'                                         
040500        MOVE MID-KDPRCGRP-OUT TO KDPRCGRP-WS                              
040600        INSPECT KDPRCGRP-WS REPLACING LEADING SPACE BY ZERO               
040700     ELSE                                                                 
040800        MOVE MID-KDPRCGRP-IN TO KDPRCGRP-WS                               
040900        MOVE '7'             TO MFS-IDPFK                                 
041000        MOVE SPACE           TO MFS-KDTRTYP                               
041100     END-IF                                                               
041200                                                                          
041300     IF KDPRCGRP-WS NOT NUMERIC                                           
041400        MOVE NOO TO KEYS-SW                                               
041500     ELSE                                                                 
041600        IF KDPRCGRP-WS NOT > ZERO                                         
041700           MOVE NOO TO KEYS-SW                                            
041800        END-IF                                                            
041900     END-IF                                                               
042000                                                                          
042100     IF MID-KDPRODKL-IN =     '+'                                         
042200        MOVE MID-KDPRODKL-OUT  TO KDPRODKL-WS                             
042300     ELSE                                                                 
042400        MOVE MID-KDPRODKL-IN TO KDPRODKL-WS                               
042500        MOVE '7'             TO MFS-IDPFK                                 
042600        MOVE SPACE           TO MFS-KDTRTYP                               
042700     END-IF                                                               
042800                                                                          
042810     MOVE MSGI-IDDC      TO WS-IDDC                                       
042820                                                                          
042900     IF KDPRODKL-WS = SPACE                                               
042910       IF CDC                                                             
043000        MOVE 'A' TO KDPRODKL-WS                                           
043010       ELSE                                                               
043020        MOVE 'B' TO KDPRODKL-WS                                           
043100       END-IF                                                             
043110     END-IF                                                               
043200                                                                          
043300     IF KDPRODKL-WS ALPHABETIC                                            
043400        CONTINUE                                                          
043500     ELSE                                                                 
043600        MOVE NOO TO KEYS-SW                                               
043700     END-IF                                                               
043800                                                                          
043900     IF MID-IDPRC-IN = ALL '+'                                            
044000        MOVE MID-IDPRC-OUT   TO IDPRC-WS                                  
044100     ELSE                                                                 
044200        MOVE MID-IDPRC-IN    TO IDPRC-WS                                  
044300        MOVE '7'             TO MFS-IDPFK                                 
044400        MOVE SPACE           TO MFS-KDTRTYP                               
044500     END-IF                                                               
044600                                                                          
044700     IF IDPRC-WS             = SPACE                                      
044800        MOVE LOW-VALUE       TO W-4448-IDPRC                              
044900     ELSE                                                                 
045000        MOVE IDPRC-WS        TO W-4448-IDPRC                              
045100     END-IF                                                               
045200                                                                          
045400                                                                          
045500     MOVE WS-IDDC          TO W-IDDC-MIN                                  
045600                              W-IDDC-MAX                                  
045700                              W-IDDC                                      
045800                              D-IDDC                                      
045900                              W-4447-IDDC                                 
046000                                                                          
046100     MOVE WS-IDDC       TO MOD-IDDC-OUT                                   
046200                                                                          
046300     IF GOOD-MID OR KEYS-OK                                               
046400       MOVE KDPRCGRP-WS TO MOD-KDPRCGRP-OUT                               
046410       IF CDC AND KDPRODKL-WS = 'A'                                       
046411         MOVE SPACE       TO MOD-KDPRODKL-OUT                             
046420       ELSE                                                               
046421         MOVE KDPRODKL-WS TO MOD-KDPRODKL-OUT                             
046430       END-IF                                                             
046600       MOVE IDPRC-WS    TO MOD-IDPRC-OUT                                  
046700       INSPECT MOD-IDPRC-OUT REPLACING LEADING ZERO BY SPACE              
046800     ELSE                                                                 
046900       MOVE MFS-RENSA-FAELT TO MOD-KDPRCGRP-OUT                           
047000                               MOD-KDPRODKL-OUT                           
047100                               MOD-IDPRC-OUT                              
047200     END-IF                                                               
047300                                                                          
047400     IF KEYS-WRONG                                                        
047500        MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                
047600        CALL WMEDKONV USING MED-WMEDAREA                                  
047700        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
047800        PERFORM MFS-ERASE-FIELD-IN                                        
047900        PERFORM MFS-ERASE-FIELD-OUT                                       
048000     END-IF                                                               
048100     .                                                                    
048200     EJECT                                                                
048300 BA-GET-PRCBAS SECTION.                                                   
048400                                                                          
048500     PERFORM IMS-GU-XXKH01                                                
048600     MOVE +1 TO INDX2                                                     
048700     MOVE SPACES  TO MOD-IDPRC-LINE  (1)                                  
048800     PERFORM IMS-GNP-XXKH11                                               
048900     PERFORM UNTIL INDX2 > 10                                             
049000       IF SEGMENT-FOUND                                                   
049100          IF XXKH-4448-IDPRC (1:3) = '998' AND                            
049200             CDC                                                          
049300              CONTINUE                                                    
049400*           SATS PRC SKALL EJ VISAS                                       
049500           ELSE                                                           
049600              MOVE XXKH-4448-IDPRC TO W-4448-IDPRC                        
049700                                         MOD-IDPRC-LINE (INDX2)           
049800              ADD +1 TO INDX2                                             
049900          END-IF                                                          
050000          PERFORM IMS-GNP-XXKH11                                          
050100       ELSE                                                               
050200          MOVE SPACES             TO MOD-IDPRC-LINE (INDX2)               
050300          ADD +1 TO INDX2                                                 
050400       END-IF                                                             
050500     END-PERFORM                                                          
050600     IF MOD-IDPRC-LINE (1)     = SPACES                                   
050700        MOVE NOO               TO ALLT-SW                                 
050800        MOVE '413'             TO MED-IDMFSFEL                            
050900        CALL WMEDKONV USING MED-WMEDAREA                                  
051000        MOVE MED-MFSFEL        TO MOD-TEMFSFEL                            
051100     END-IF                                                               
051200     .                                                                    
051300     EJECT                                                                
051400 C-FIRST-PAGE SECTION.                                                    
051500                                                                          
051600     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
051700     CALL WMEDKONV USING MED-WMEDAREA                                     
051800     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
051900                                                                          
052000     .                                                                    
052100     EJECT                                                                
052200 D-NEXT-PAGE SECTION.                                                     
052300                                                                          
052400     IF MID-IDORDER-NEXT = 'SLUT'                                         
052500        MOVE ERR-LAST-PAGE-SHOWED TO MED-IDMFSFEL                         
052600        CALL WMEDKONV USING MED-WMEDAREA                                  
052700        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
052800        MOVE 'SLUT'       TO MOD-IDORDER-NEXT                             
052900        MOVE NOO          TO ALLT-SW                                      
053000     ELSE                                                                 
053100        MOVE MID-TIRFS-NEXT TO W-DARFS-MIN                                
053110        IF MID-TIRFS-NEXT NOT = ZERO                                      
053120          IF MID-TIRFS-NEXT < 5000000000                                  
053130            MOVE 20           TO W-DARFS-MIN (1:2)                        
053140          ELSE                                                            
053150            IF MID-TIRFS-NEXT < 9999999999                                
053160              MOVE 19         TO W-DARFS-MIN (1:2)                        
053170            ELSE                                                          
053180              MOVE 999999999999 TO W-DARFS-MIN                            
053190            END-IF                                                        
053191          END-IF                                                          
053192        END-IF                                                            
053200        MOVE MID-TILST-O-NEXT TO W-DALSTORD-MIN                           
053210        IF MID-TILST-O-NEXT NOT = ZERO                                    
053220          IF MID-TILST-O-NEXT < 5000000000                                
053230            MOVE 20           TO W-DALSTORD-MIN (1:2)                     
053240          ELSE                                                            
053250            IF MID-TILST-O-NEXT < 9999999999                              
053270              MOVE 19         TO W-DALSTORD-MIN (1:2)                     
053280            ELSE                                                          
053290              MOVE 999999999999 TO W-DALSTORD-MIN                         
053291            END-IF                                                        
053292          END-IF                                                          
053293        END-IF                                                            
053300        MOVE MID-IDORDER-NEXT TO W-IDORDER                                
053400        MOVE YES TO ALLT-SW                                               
053500     END-IF                                                               
053600     .                                                                    
053700     EJECT                                                                
053800 E-SAME-PAGE SECTION.                                                     
053900                                                                          
054000       MOVE MID-TIRFS-ENTER TO W-DARFS-MIN                                
054010       IF MID-TIRFS-ENTER NOT = ZERO                                      
054020         IF MID-TIRFS-ENTER < 5000000000                                  
054030           MOVE 20           TO W-DARFS-MIN (1:2)                         
054040         ELSE                                                             
054050           IF MID-TIRFS-ENTER < 9999999999                                
054060             MOVE 19         TO W-DARFS-MIN (1:2)                         
054070           ELSE                                                           
054080             MOVE 999999999999 TO W-DARFS-MIN                             
054090           END-IF                                                         
054091         END-IF                                                           
054092       END-IF                                                             
054100                                                                          
054200       MOVE MID-TILST-O-ENTER TO W-DALSTORD-MIN                           
054210       IF MID-TILST-O-ENTER NOT = ZERO                                    
054220         IF MID-TILST-O-ENTER < 5000000000                                
054230           MOVE 20            TO W-DALSTORD-MIN (1:2)                     
054240         ELSE                                                             
054250           IF MID-TILST-O-ENTER < 9999999999                              
054251             MOVE 19          TO W-DALSTORD-MIN (1:2)                     
054260           ELSE                                                           
054270             MOVE 999999999999 TO W-DALSTORD-MIN                          
054280           END-IF                                                         
054290         END-IF                                                           
054291       END-IF                                                             
054300       MOVE MID-IDORDER-ENTER TO W-IDORDER                                
054400                                                                          
054500       MOVE YES TO ALLT-SW                                                
054600     .                                                                    
054700     EJECT                                                                
054800 F-READ-SHOW-INFO SECTION.                                                
054900                                                                          
055000     IF KEYS-OK                                                           
055100         PERFORM FA-SORT-IDPRCBAS                                         
055200         PERFORM FB-SORT-ORDERPARTS                                       
055300         PERFORM FC-MOVE-TAB-TO-MOD                                       
055400     ELSE                                                                 
055500         MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                               
055600         CALL WMEDKONV USING MED-WMEDAREA                                 
055700         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
055800         PERFORM MFS-ERASE-FIELD-IN                                       
055900         PERFORM MFS-ERASE-FIELD-OUT                                      
056000     END-IF                                                               
056100                                                                          
056200     IF WS-IDORDER (1)         = HIGH-VALUES                              
056300         MOVE '106' TO MED-IDMFSINF                                       
056400         CALL WMEDKONV USING MED-WMEDAREA                                 
056500         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
056600         MOVE 'SLUT'           TO MOD-IDORDER-NEXT                        
056700         MOVE SPACES           TO MOD-TIRFS-NEXT                          
056800         MOVE SPACES           TO MOD-TILST-O-NEXT                        
056900     END-IF                                                               
057000                                                                          
057100     PERFORM MFS-ERASE-FIELD-IN                                           
057200                                                                          
057300     .                                                                    
057400     EJECT                                                                
057500 FA-SORT-IDPRCBAS        SECTION.                                         
057600                                                                          
057700     MOVE +1 TO INDX2                                                     
057800     MOVE MOD-IDPRCBAS         IN MOD-IDPRC-LINE (INDX2)                  
057900                               TO     W-IDPRCBAS-MIN                      
058000                                      W-IDPRCBAS-MAX                      
058100     MOVE MOD-IDPRCVAR         IN MOD-IDPRC-LINE (INDX2)                  
058200                               TO     W-IDPRCVAR-MIN                      
058300                                      W-IDPRCVAR-MAX                      
058400     IF MFS-FIRST                                                         
058500         MOVE LOW-VALUE            TO W-IDORDER-X                         
058600      ELSE                                                                
058700         PERFORM S01-CHECK-IDPRCVAR                                       
058800     END-IF                                                               
058900     PERFORM IMS-GU-WDQ3CSEQ-ORQC-OKVAL                                   
059000     PERFORM UNTIL INDX2 = 10   OR                                        
059100                    MOD-IDPRC-LINE (INDX2) = SPACES                       
059200         IF SEGMENT-FOUND                                                 
059300            MOVE ORQA-ODEL-IDPRCBAS TO WS-IDPRCBAS (INDX2)                
059400            MOVE ORQA-ODEL-IDORDER   TO WS-IDORDER  (INDX2)               
059500            MOVE ORQA-ODEL-DARFS     TO WS-DARFS    (INDX2)               
059600            MOVE ORQA-ODEL-DALSTORD  TO WS-DALSTORD (INDX2)               
059700            MOVE ORQC-KEY-FB-AREA    TO WS-FULL-KEY (INDX2)               
059800            MOVE WS-IDPRCVAR(INDX2)  TO MOD-IDPRCVAR-ENTER(INDX2)         
059900         ELSE                                                             
060000            MOVE HIGH-VALUES         TO OPARTS-LINE (INDX2)               
060100         END-IF                                                           
060200         ADD +1 TO INDX2                                                  
060300         MOVE MOD-IDPRCBAS         IN MOD-IDPRC-LINE (INDX2)              
060400                                   TO W-IDPRCBAS-MIN                      
060500                                      W-IDPRCBAS-MAX                      
060600         MOVE MOD-IDPRCVAR         IN MOD-IDPRC-LINE (INDX2)              
060700                                   TO W-IDPRCVAR-MIN                      
060800                                      W-IDPRCVAR-MAX                      
060900         IF MFS-FIRST                                                     
061000             CONTINUE                                                     
061100          ELSE                                                            
061200             PERFORM S01-CHECK-IDPRCVAR                                   
061300         END-IF                                                           
061400         PERFORM IMS-GU-WDQ3CSEQ-ORQC-OKVAL                               
061500     END-PERFORM                                                          
061600                                                                          
061700     CALL WINTSOR USING   OPARTS-TABLE                                    
061800                          WS-LGT-RECORD                                   
061900                          WS-NUMBER-ELEM                                  
062000                          WS-SORTFIELDS (1)                               
062100                          WS-LGT-SFIELDS                                  
062200     .                                                                    
062300     EJECT                                                                
062400 FB-SORT-ORDERPARTS       SECTION.                                        
062500                                                                          
062600     MOVE ZERO                     TO INDX                                
062700     PERFORM UNTIL WS-IDORDER (1)  = HIGH-VALUES OR                       
062800                        INDX       > 13                                   
062900         MOVE WS-IDORDER (1)         TO W-HELP-IDORDER                    
063000         MOVE +1 TO INDX2                                                 
063100         PERFORM UNTIL WS-IDORDER (INDX2) = HIGH-VALUES OR                
063200                     INDX  > 13                         OR                
063300                     INDX2 > 10                         OR                
063400                     WS-IDORDER (INDX2) NOT = W-HELP-IDORDER              
063500                                                                          
063600              MOVE WS-FULL-KEY (INDX2) TO D-WDQ3CSEQ-X                    
063700              MOVE WS-IDORDER  (INDX2) TO XD-IDORDER                      
063800              MOVE D-IDDC              TO W-IDDC-MIN                      
063900                                          W-IDDC-MAX                      
064000              MOVE D-IDPRCBAS          TO W-IDPRCBAS-MIN                  
064100                                          W-IDPRCBAS-MAX                  
064200              MOVE D-IDPRCVAR          TO W-IDPRCVAR-MIN                  
064300                                          W-IDPRCVAR-MAX                  
064400              PERFORM FBA-CHECK-T-TABLE                                   
064500              IF T-TABLE-OK                                               
064600                  PERFORM FBB-SKAPA-MOD-TABELL                            
064700              END-IF                                                      
064800                                                                          
064900              PERFORM IMS-GU-WDQ3CSEQ-ORQC                                
065000              IF SEGMENT-FOUND                                            
065100                                                                          
065200                PERFORM IMS-GN-2-WDQ3CSEQ-ORQC                            
065300                IF SEGMENT-FOUND                                          
065400                   MOVE ORQA-ODEL-IDPRCBAS TO WS-IDPRCBAS (INDX2)         
065500                   MOVE ORQA-ODEL-IDORDER TO WS-IDORDER (INDX2)           
065600                   MOVE ORQA-ODEL-DARFS  TO WS-DARFS    (INDX2)           
065700                   MOVE ORQA-ODEL-DALSTORD TO WS-DALSTORD (INDX2)         
065800                   MOVE ORQC-KEY-FB-AREA TO WS-FULL-KEY (INDX2)           
065900                ELSE                                                      
066000                   MOVE HIGH-VALUES      TO OPARTS-LINE (INDX2)           
066100                END-IF                                                    
066200              ELSE                                                        
066300                 MOVE HIGH-VALUES        TO OPARTS-LINE (INDX2)           
066400              END-IF                                                      
066500           ADD +1 TO INDX2                                                
066600         END-PERFORM                                                      
066700                                                                          
066800         CALL WINTSOR USING OPARTS-TABLE                                  
066900                            WS-LGT-RECORD                                 
067000                            WS-NUMBER-ELEM                                
067100                            WS-SORTFIELDS (1)                             
067200                            WS-LGT-SFIELDS                                
067300     END-PERFORM                                                          
067400                                                                          
067500     IF SEGMENT-FOUND                                                     
067600         PERFORM FBC-SAVE-KEYS-NEXT                                       
067700      ELSE                                                                
067800         MOVE 9999999999           TO MOD-TIRFS-NEXT                      
067900                                      MOD-TILST-O-NEXT                    
068000         MOVE 9999999              TO MOD-IDORDER-NEXT                    
068100     END-IF                                                               
068200     .                                                                    
068300     EJECT                                                                
068400 FBA-CHECK-T-TABLE         SECTION.                                       
068500                                                                          
068600     MOVE YES                  TO T-TABLE-SW                              
068700     MOVE +1                   TO INDX3                                   
068800     PERFORM UNTIL INDX3       >  MAX-INDX OR                             
068900              T-IDORDER(INDX3) =  ZERO     OR                             
069000              T-TABLE-NOT-OK                                              
069100         IF WS-IDORDER(INDX2)  = T-IDORDER (INDX3)                        
069200             MOVE NOO          TO T-TABLE-SW                              
069300          ELSE                                                            
069400             ADD +1            TO INDX3                                   
069500         END-IF                                                           
069600     END-PERFORM                                                          
069700                                                                          
069800     .                                                                    
069900     EJECT                                                                
070000 FBB-SKAPA-MOD-TABELL      SECTION.                                       
070100                                                                          
070200     MOVE NOO                     TO OLD-IDORDER-SW                       
070300                                                                          
070400     MOVE HIGH-VALUE              TO W-WDQ301KY-MAX-X                     
070500     MOVE LOW-VALUE               TO W-WDQ301KY-MIN-X                     
070600     MOVE WS-IDORDER(INDX2)       TO W-Q301KY-IDORDER-MIN                 
070700                                     W-Q301KY-IDORDER-MAX                 
070800     MOVE WS-IDDC                 TO W-Q301KY-IDDC-MIN                    
070900                                     W-Q301KY-IDDC-MAX                    
071000                                                                          
071100     PERFORM IMS-GU-ORQA-WDQ301                                           
071200     PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-END                         
071300         PERFORM FBBA-CHECK-IDPRCBAS                                      
071400         IF IDPRCBAS-OK                                                   
071500             IF INDX              >  ZERO                                 
071600                 IF ORQA-ODEL-IDORDER = T-IDORDER (INDX)                  
071700                     IF T-KDODELSTA (INDX, INDX4) = SPACE                 
071800                         MOVE SPACE TO T-STARSPAC (INDX, INDX4)           
071900                         MOVE ORQA-ODEL-KDODELSTA                         
072000                                      TO T-KDODELSTA(INDX, INDX4)         
072100                      ELSE                                                
072200                         PERFORM FBBB-CHECK-STATUS                        
072300                     END-IF                                               
072400                     MOVE YES     TO OLD-IDORDER-SW                       
072500                 END-IF                                                   
072600             END-IF                                                       
072700                                                                          
072800             IF OLD-IDORDER                                               
072900                 MOVE ORQA-ODEL-IDPRC                                     
073000                              TO T-IDPRC    (INDX, INDX4)                 
073100              ELSE                                                        
073200                 IF INDX          >  ZERO                                 
073300                     PERFORM FBBC-CHECK-PREV-LINE                         
073400                     IF PREV-LINE-OK                                      
073500                         ADD +1   TO INDX                                 
073600                      ELSE                                                
073700                         PERFORM FBBD-INIT-T-KDODELSTA                    
073800                     END-IF                                               
073900                  ELSE                                                    
074000                     ADD +1       TO INDX                                 
074100                 END-IF                                                   
074200                 PERFORM FBBE-MOVE-IDPRCBAS                               
074300                 MOVE ORQA-ODEL-IDORDER  TO T-IDORDER     (INDX)          
074400                 MOVE ORQA-ODEL-IDORDNR7 TO T-IDORDNR7    (INDX)          
074500                 MOVE ORQA-ODEL-IDDISTR  TO T-IDDISTR     (INDX)          
074600                 MOVE ORQA-ODEL-IDPRODNR TO T-IDPRODNR    (INDX)          
074700                 MOVE ORQA-ODEL-IDKUNDNR TO T-IDKUNDNR    (INDX)          
074800                 MOVE ORQA-ODEL-DARFS    TO T-DARFS       (INDX)          
074900                 MOVE ORQA-ODEL-DALSTORD TO T-DALSTORD    (INDX)          
075000                 MOVE ORQA-ODEL-IDPRC                                     
075100                                    TO T-IDPRC    (INDX, INDX4)           
075200                 MOVE ORQA-ODEL-KDODELSTA                                 
075300                                    TO T-KDODELSTA (INDX, INDX4)          
075400                 MOVE SPACE         TO T-STARSPAC  (INDX, INDX4)          
075500             END-IF                                                       
075600         END-IF                                                           
075700         PERFORM IMS-GN-ORQA-WDQ301                                       
075800     END-PERFORM                                                          
075900                                                                          
076000     IF INDX > 0                                                          
076100        PERFORM FBBC-CHECK-PREV-LINE                                      
076200        IF NOT PREV-LINE-OK                                               
076300           PERFORM FBBD-INIT-T-KDODELSTA                                  
076400           MOVE 0 TO T-IDORDNR7 (INDX)                                    
076500        END-IF                                                            
076600     END-IF                                                               
076700     .                                                                    
076800     EJECT                                                                
076900 FBBA-CHECK-IDPRCBAS         SECTION.                                     
077000                                                                          
077100     MOVE YES                     TO IDPRCBAS-SW                          
077200     MOVE +1                      TO INDX4                                
077300     PERFORM UNTIL INDX4          > 10          OR                        
077400             ORQA-ODEL-IDPRC      =  MOD-IDPRC-LINE (INDX4)               
077500         ADD +1                   TO INDX4                                
077600     END-PERFORM                                                          
077700                                                                          
077800     IF INDX4                     = +11                                   
077900         MOVE NOO                 TO IDPRCBAS-SW                          
078000     END-IF                                                               
078100     .                                                                    
078200     EJECT                                                                
078300 FBBB-CHECK-STATUS           SECTION.                                     
078400                                                                          
078500     EVALUATE ORQA-ODEL-KDODELSTA                                         
078600     WHEN 'R'                                                             
078700        IF T-KDODELSTA(INDX, INDX4) =  SPACE                              
078800            MOVE 'R'                TO T-KDODELSTA(INDX, INDX4)           
078900            MOVE SPACE              TO T-STARSPAC (INDX, INDX4)           
079000         ELSE                                                             
079100            IF T-KDODELSTA(INDX, INDX4) = 'U' OR 'P'                      
079200                MOVE 'R'            TO T-KDODELSTA(INDX, INDX4)           
079300                MOVE '*'            TO T-STARSPAC (INDX, INDX4)           
079400            END-IF                                                        
079500        END-IF                                                            
079600                                                                          
079700     WHEN 'U'                                                             
079800        EVALUATE T-KDODELSTA(INDX, INDX4)                                 
079900          WHEN SPACE                                                      
080000             MOVE 'U'          TO T-KDODELSTA(INDX, INDX4)                
080100             MOVE SPACE        TO T-STARSPAC (INDX, INDX4)                
080200          WHEN 'R'                                                        
080300             MOVE 'R'          TO T-KDODELSTA(INDX, INDX4)                
080400             MOVE '*'          TO T-STARSPAC (INDX, INDX4)                
080500          WHEN 'P'                                                        
080600             MOVE 'U'          TO T-KDODELSTA(INDX, INDX4)                
080700             MOVE '*'          TO T-STARSPAC (INDX, INDX4)                
080800        END-EVALUATE                                                      
080900                                                                          
081000     WHEN 'P'                                                             
081100        EVALUATE T-KDODELSTA(INDX, INDX4)                                 
081200          WHEN  SPACE                                                     
081300             MOVE 'P'          TO T-KDODELSTA(INDX, INDX4)                
081400             MOVE SPACE        TO T-STARSPAC (INDX, INDX4)                
081500          WHEN 'R'                                                        
081600             MOVE 'R'          TO T-KDODELSTA(INDX, INDX4)                
081700             MOVE '*'          TO T-STARSPAC (INDX, INDX4)                
081800          WHEN 'U'                                                        
081900             MOVE 'U'          TO T-KDODELSTA(INDX, INDX4)                
082000             MOVE '*'          TO T-STARSPAC (INDX, INDX4)                
082100        END-EVALUATE                                                      
082200     END-EVALUATE                                                         
082300     .                                                                    
082400     EJECT                                                                
082500 FBBC-CHECK-PREV-LINE        SECTION.                                     
082600                                                                          
082700*    CHECK IF PREVIOUS LINE SHALL BE SHOWN. LINES WITH                    
082800*    ONLY STATUS R OR P SHALL NOT BE SHOWN                                
082900                                                                          
083000     MOVE NOO                     TO PREV-LINE-SW                         
083100                                     KDODELSTA-R-SW                       
083200                                     KDODELSTA-P-SW                       
083300     MOVE +1                      TO INDX3                                
083400     PERFORM UNTIL INDX3          > 10 OR PREV-LINE-OK                    
083500         IF ((T-KDODELSTA (INDX, INDX3) = 'R' OR 'P') AND                 
083600              T-STARSPAC  (INDX, INDX3) = SPACE)          OR              
083700              T-KDODELSTA (INDX, INDX3) = SPACE                           
083800              IF T-KDODELSTA (INDX, INDX3) = 'R'                          
083900                 MOVE YES TO KDODELSTA-R-SW                               
084000              END-IF                                                      
084100              IF T-KDODELSTA (INDX, INDX3) = 'P'                          
084200                 MOVE YES TO KDODELSTA-P-SW                               
084300              END-IF                                                      
084400              ADD +1              TO INDX3                                
084500          ELSE                                                            
084600              MOVE YES            TO PREV-LINE-SW                         
084700          END-IF                                                          
084800     END-PERFORM                                                          
084900                                                                          
085000     IF PREV-LINE-SW = NOO                                                
085100        IF KDODELSTA-R-SW = YES AND KDODELSTA-P-SW = YES                  
085200           MOVE YES            TO PREV-LINE-SW                            
085300        END-IF                                                            
085400     END-IF                                                               
085500     .                                                                    
085600     EJECT                                                                
085700 FBBD-INIT-T-KDODELSTA      SECTION.                                      
085800                                                                          
085900     MOVE +1                      TO INDX3                                
086000     PERFORM UNTIL INDX3          > 10                                    
086100         MOVE SPACE               TO T-KDODELSTA(INDX, INDX3)             
086200                                     T-STARSPAC (INDX, INDX3)             
086300         ADD +1                   TO INDX3                                
086400     END-PERFORM                                                          
086500     .                                                                    
086600     EJECT                                                                
086700 FBBE-MOVE-IDPRCBAS         SECTION.                                      
086800                                                                          
086900     MOVE +1                      TO INDX3                                
087000     PERFORM UNTIL INDX3          > 10 OR                                 
087100                   MOD-IDPRC-LINE (INDX3) = SPACE                         
087200         MOVE MOD-IDPRC-LINE  (INDX3) TO T-IDPRC   (INDX, INDX3)          
087300         ADD +1                   TO INDX3                                
087400     END-PERFORM                                                          
087500     .                                                                    
087600     EJECT                                                                
087700 FBC-SAVE-KEYS-NEXT      SECTION.                                         
087800                                                                          
087900     MOVE ORQA-ODEL-IDORDER    TO MOD-IDORDER-NEXT                        
088000     MOVE ORQA-ODEL-DARFS      TO W-DARFS-NEXT                            
088100     MOVE W-DARFS-NEXT (3:10)  TO MOD-TIRFS-NEXT                          
088200     MOVE ORQA-ODEL-DALSTORD   TO W-DALSTORD-NEXT                         
088300     MOVE W-DALSTORD-NEXT (3:10) TO MOD-TILST-O-NEXT                      
088400     MOVE +1                   TO INDX3                                   
088500     PERFORM UNTIL INDX3       >  +10                                     
088600         MOVE +1               TO INDX4                                   
088700         PERFORM UNTIL INDX4   >  +10                                     
088800             IF MOD-IDPRCBAS        IN MOD-IDPRC-LINE (INDX3)             
088900                                    = WS-IDPRCBAS(INDX4) AND              
089000                MOD-IDPRCVAR        IN MOD-IDPRC-LINE (INDX3)             
089100                                    = WS-IDPRCVAR(INDX4)                  
089200                 MOVE WS-IDPRCVAR(INDX4) TO                               
089300                                    MOD-IDPRCVAR-NEXT(INDX3)              
089400                 MOVE +11      TO INDX4                                   
089500              ELSE                                                        
089600                 ADD +1        TO INDX4                                   
089700             END-IF                                                       
089800         END-PERFORM                                                      
089900         ADD +1                TO INDX3                                   
090000     END-PERFORM                                                          
090100     MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                            
090200     CALL WMEDKONV USING MED-WMEDAREA                                     
090300     MOVE MED-MFSINF           TO MOD-TEMFSINF                            
090400     .                                                                    
090500     EJECT                                                                
090600 FC-MOVE-TAB-TO-MOD      SECTION.                                         
090700                                                                          
090800     MOVE +1 TO INDX                                                      
090900                                                                          
091000     PERFORM UNTIL INDX        > MAX-INDX                                 
091100                                                                          
091200         IF INDX                  =  1                                    
091300           PERFORM FCA-SAVE-KEYS-ENTER                                    
091400         END-IF                                                           
091500         IF T-IDORDNR7 (INDX)       NOT = ZERO                            
091600             MOVE T-IDORDNR7 (INDX) TO MOD-IDORDNR7 (INDX)                
091700             MOVE T-IDDISTR  (INDX) TO MOD-IDDISTR (INDX)                 
091800             MOVE T-IDPRODNR (INDX) TO MOD-IDPRODNR (INDX)                
091900             MOVE T-IDKUNDNR (INDX) TO MOD-IDKUNDNR (INDX)                
092000                                                                          
092100             MOVE +1 TO INDX2                                             
092200             PERFORM UNTIL INDX2    > 10                                  
092300                 MOVE T-KDODELSTA (INDX, INDX2) TO                        
092400                      MOD-KDODELSTA (INDX, INDX2)                         
092500                 MOVE T-STARSPAC  (INDX, INDX2) TO                        
092600                      MOD-STARSPAC (INDX, INDX2)                          
092700                 ADD +1             TO INDX2                              
092800             END-PERFORM                                                  
092900          ELSE                                                            
093000             MOVE MFS-RENSA-FAELT   TO MOD-IDORDNR7 (INDX)                
093100                                       MOD-IDDISTR (INDX)                 
093200                                       MOD-IDPRODNR (INDX)                
093300                                       MOD-IDKUNDNR (INDX)                
093400         END-IF                                                           
093500         ADD +1 TO INDX                                                   
093600     END-PERFORM                                                          
093700     .                                                                    
093800     EJECT                                                                
093900 FCA-SAVE-KEYS-ENTER SECTION.                                             
094000                                                                          
094100     MOVE T-IDORDER(INDX)      TO MOD-IDORDER-ENTER                       
094200     MOVE T-DARFS(INDX)        TO WS-DARFS (1)                            
094300     MOVE WS-DARFS (1) (3:10)  TO MOD-TIRFS-ENTER                         
094400     MOVE T-DALSTORD (INDX)    TO WS-DALSTORD (1)                         
094500     MOVE WS-DALSTORD (1) (3:10) TO MOD-TILST-O-ENTER                     
094600     .                                                                    
094700     EJECT                                                                
094800                                                                          
094900 S01-CHECK-IDPRCVAR  SECTION.                                             
095000                                                                          
095100     IF MFS-ENTER                                                         
095200         IF MID-IDPRCVAR-ENTER(INDX2)       = SPACE                       
095300             MOVE HIGH-VALUE                TO W-IDPRCVAR-MIN             
095400          ELSE                                                            
095500             MOVE MID-IDPRCVAR-ENTER(INDX2) TO W-IDPRCVAR-MIN             
095600         END-IF                                                           
095700      ELSE                                                                
095800         IF MID-IDPRCVAR-NEXT(INDX2)        = SPACE                       
095900             MOVE HIGH-VALUE                TO W-IDPRCVAR-MIN             
096000          ELSE                                                            
096100             MOVE MID-IDPRCVAR-NEXT(INDX2) TO W-IDPRCVAR-MIN              
096200         END-IF                                                           
096300     END-IF                                                               
096400     .                                                                    
096500     EJECT                                                                
096600                                                                          
096700 MFS-ERASE-FIELD-OUT SECTION.                                             
096800                                                                          
096900*    --- ALL OUTDATA-FIELD                                                
097000*    --- INCL SCROLLKEYS                                                  
097100     MOVE MFS-ERASE-FIELD TO MOD-KDPRCGRP-OUT                             
097200                             MOD-IDORDER-ENTER                            
097300                             MOD-IDORDER-NEXT                             
097400                             MOD-TIRFS-ENTER                              
097500                             MOD-TIRFS-NEXT                               
097600                             MOD-TILST-O-ENTER                            
097700                             MOD-TILST-O-NEXT                             
097800     .                                                                    
097900     SKIP2                                                                
098000     SKIP2                                                                
098100 MFS-ERASE-FIELD-IN SECTION.                                              
098200                                                                          
098300*    --- ALL INPUT-FIELDS                                                 
098400     MOVE MFS-ERASE-FIELD TO MOD-KDPRCGRP-IN                              
098500                             MOD-KDPRODKL-IN                              
098600                             MOD-IDPRC-IN                                 
098700                             MOD-IDDC-IN                                  
098800     .                                                                    
098900     EJECT                                                                
099000* --- IMS SECTIONS ---                                                    
099100                                                                          
099200 IMS-GET-MSG SECTION.                                                     
099300                                                                          
099400     MOVE '  QC' TO GOOD-STATUSCODES                                      
099500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
099600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
099700     PERFORM IMS-STATUSCONTROL                                            
099800     .                                                                    
099900                                                                          
100000 IMS-INSERT-MSG SECTION.                                                  
100100                                                                          
100200     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
100300       MOVE '0' TO MFS-KDHUVOMR                                           
100400     END-IF                                                               
100500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
100600     MOVE SPACE TO GOOD-STATUSCODES                                       
100700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
100800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
100900     PERFORM IMS-STATUSCONTROL                                            
101000     .                                                                    
101100     EJECT                                                                
101200 IMS-GU-XXKH01 SECTION.                                                   
101300                                                                          
101400     STRING 'WLXXKH01(WDGXKEY  =' W-4447-X ')'                            
101500          DELIMITED BY SIZE INTO SSA1                                     
101600     MOVE '  GE' TO GOOD-STATUSCODES                                      
101700     CALL CBLTDLI USING GU XXKH-PCB DLI-IO-AREA SSA1                      
101800     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
101900     PERFORM IMS-STATUSCONTROL                                            
102000     .                                                                    
102100                                                                          
102200 IMS-GNP-XXKH11 SECTION.                                                  
102300                                                                          
102400     STRING 'WLXXKH11(WDGXKEY >=' W-4448-X                                
102500                    '&KDPRCGRP =' KDPRCGRP-WS                             
102600                    '&KDPRODKL >' KDPRODKL-WS ')'                         
102700          DELIMITED BY SIZE INTO SSA1                                     
102800     MOVE '  GE' TO GOOD-STATUSCODES                                      
102900     CALL CBLTDLI USING GNP XXKH-PCB DLI-IO-AREA SSA1                     
103000     MOVE XXKH-STATUS-CODE TO STATUS-WS                                   
103100     PERFORM IMS-STATUSCONTROL                                            
103200     .                                                                    
103300     EJECT                                                                
103400 IMS-GU-ORQA-WDQ301    SECTION.                                           
103500                                                                          
103600     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                        
103700                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
103800          DELIMITED BY SIZE INTO SSA1                                     
103900     MOVE '  GE' TO GOOD-STATUSCODES                                      
104000     CALL CBLTDLI USING GU ORQA-PCB DLI-IO-AREA SSA1                      
104100     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
104200     PERFORM IMS-STATUSCONTROL                                            
104300     .                                                                    
104400                                                                          
104500 IMS-GN-ORQA-WDQ301 SECTION.                                              
104600                                                                          
104700     STRING 'WLORQA01(WDQ301KY>=' W-WDQ301KY-MIN-X                        
104800                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
104900          DELIMITED BY SIZE INTO SSA1                                     
105000     MOVE '  GBGE' TO GOOD-STATUSCODES                                    
105100     CALL CBLTDLI USING GN ORQA-PCB DLI-IO-AREA SSA1                      
105200     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
105300     PERFORM IMS-STATUSCONTROL                                            
105400     .                                                                    
105500     EJECT                                                                
105600 IMS-GU-WDQ3CSEQ-ORQC-OKVAL  SECTION.                                     
105700                                                                          
105800     STRING 'WLORQA01(WDQ3CSEQ=>' W-WDQ3CSEQ-MIN                          
105900                    '&WDQ3CSEQ<=' W-WDQ3CSEQ-MAX                          
106000                    '&IDORDER >=' W-IDORDER-X ')'                         
106100          DELIMITED BY SIZE INTO SSA1                                     
106200     MOVE '  GBGE' TO GOOD-STATUSCODES                                    
106300     CALL CBLTDLI USING GU ORQC-PCB DLI-IO-AREA SSA1                      
106400     MOVE ORQC-STATUS-CODE TO STATUS-WS                                   
106500     PERFORM IMS-STATUSCONTROL                                            
106600     .                                                                    
106700                                                                          
106800 IMS-GU-WDQ3CSEQ-ORQC SECTION.                                            
106900                                                                          
107000     STRING 'WLORQA01(WDQ3CSEQ =' D-WDQ3CSEQ-X ')'                        
107100          DELIMITED BY SIZE INTO SSA2                                     
107200     MOVE '  GE'   TO GOOD-STATUSCODES                                    
107300     CALL CBLTDLI USING GU ORQC-PCB DLI-IO-AREA SSA2                      
107400     MOVE ORQC-STATUS-CODE TO STATUS-WS                                   
107500     PERFORM IMS-STATUSCONTROL                                            
107600     .                                                                    
107700                                                                          
107800 IMS-GN-2-WDQ3CSEQ-ORQC SECTION.                                          
107900                                                                          
108000     STRING 'WLORQA01(WDQ3CSEQ>=' W-WDQ3CSEQ-MIN                          
108100                    '&WDQ3CSEQ<=' W-WDQ3CSEQ-MAX                          
108200                    '&IDORDER NE' XD-IDORDER-X ')'                        
108300          DELIMITED BY SIZE INTO SSA2                                     
108400     MOVE '  GBGE' TO GOOD-STATUSCODES                                    
108500     CALL CBLTDLI USING GN ORQC-PCB DLI-IO-AREA SSA2                      
108600     MOVE ORQC-STATUS-CODE TO STATUS-WS                                   
108700     PERFORM IMS-STATUSCONTROL                                            
108800     .                                                                    
108900     EJECT                                                                
109000 IMS-STATUSCONTROL SECTION.                                               
109100                                                                          
109200     SET STATUS-IX TO 1                                                   
109300     SEARCH GOOD-STATUS                                                   
109400       AT END CALL FELLOG                                                 
109500       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
109600     END-SEARCH                                                           
109700     .                                                                    
109800     EJECT                                                                
