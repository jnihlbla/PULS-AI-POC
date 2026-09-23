000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W9042700.                                                
000300 AUTHOR.         RICHARD.                                                 
000400 DATE-WRITTEN.   970413.                                                  
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PARTSINFO FROM VR/DSP                                            
000900*                                                                         
001000*        PROGRAMMET LÄSER      WLARTF (WDK8)                              
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSAKTION: W90427T                                             
001500*        MID:         W90427I1                                            
001600*                                                                         
001700*    UTDATA.                                                              
001800*        MOD:         W90427O1                                            
001900                                                                          
002000                                                                          
002100 ENVIRONMENT DIVISION.                                                    
002200 DATA DIVISION.                                                           
002300     EJECT                                                                
002400 WORKING-STORAGE SECTION.                                                 
002500*    -- CHECKED BY WY2000                                                 
002600                                                                          
002700 77  IDPGM                   PIC X(08)   VALUE 'W9042700'.                
002800 77  JA                      PIC X       VALUE 'J'.                       
002900 77  NEJ                     PIC X       VALUE 'N'.                       
003000 77  FELTEXT                 PIC X(80)   VALUE SPACE.                     
003100 77  WS-IDLANDX2             PIC X(2)    VALUE SPACE.                     
003200 77  RAD-IX                  PIC S9(9)   VALUE +0    COMP SYNC.           
003300 77  MAX-RAD-IX              PIC S9(9)   VALUE +13   COMP SYNC.           
003400                                                                          
003500 77  NYCKLAR-SW              PIC X       VALUE 'J'.                       
003600     88  NYCKLAR-OK                      VALUE 'J'.                       
003700     88  NYCKLAR-FEL                     VALUE 'N'.                       
003800                                                                          
003900 77  W-IDTRANS               PIC X(4)    VALUE SPACE.                     
004000     88  EGEN-MID                        VALUE '9427'.                    
004100     88  HELP-MID                        VALUE '0551'.                    
004200                                                                          
004300     SKIP3                                                                
004400 01  MESSAGE-CODES.                                                       
004500     03  INF-FIRST-PAGE      PIC X(3)    VALUE '006'.                     
004600     03  INF-MORE-INFO       PIC X(3)    VALUE '105'.                     
004700     03  ERR-WRONG-KEY       PIC X(3)    VALUE '401'.                     
004800     03  ERR-PART-MISSING    PIC X(3)    VALUE '017'.                     
004900                                                                          
005000                                                                          
005100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
005200 01  GENERELLA-SUBPROGRAM.                                                
005300     03  W005INIT            PIC X(8)    VALUE 'W005INIT'.                
005400     03  WMEDKONV            PIC X(8)    VALUE 'WMEDKONV'.                
005500     03  CBLTDLI             PIC X(8)    VALUE 'CBLTDLI '.                
005600     03  FELLOG              PIC X(8)    VALUE 'FELLOG  '.                
005700                                                                          
005800     EJECT                                                                
005900 01  FILLER                  PIC X(12)   VALUE 'RAD=79 BYTE'.             
006000 01  WS-RAD.                                                              
006100   03  WS-KUND-AIGRP.                                                     
006200     05  WS-IDKUNDNR         PIC Z(6).                                    
006300   03  FILLER                PIC X(2).                                    
006400   03  WS-KDSHELFL           PIC Z(2).                                    
006500   03  WS-KVBEST             PIC -(7).                                    
006600   03  WS-KVLS               PIC -(7).                                    
006700   03  WS-KVROS              PIC -(7).                                    
006800   03  WS-KVVRPROGN          PIC -(7).9(2)  BLANK WHEN ZERO.              
006900   03  WS-KVLEVART1          PIC -(7).                                    
007000   03  WS-KVLEVART2          PIC -(7).                                    
007100   03  WS-KVLEVART-INNEV     PIC -(8).                                    
007200   03  WS-KVLEVART-FOREG     PIC -(8).                                    
007300   03  WS-KVLEVART-F-FOREG   PIC -(8).                                    
007400                                                                          
007500     EJECT                                                                
007600*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007700*01 -COPY WMEDAREA                                                        
007800     EJECT                                                                
007900*    --- PARAMETRAR TILL SUBPROGRAM                                       
008000*01 -COPY WMSGINIT                                                        
008100     EJECT                                                                
008200*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008300                                                                          
008400 01  FILLER                  PIC X(16)   VALUE 'MID-AREA'.                
008500                                                                          
008600*01  MID -COPY W90427I1                                                   
008700     EJECT                                                                
008800 01  FILLER                  PIC X(16)  VALUE 'MSG/MOD-AREA'.             
008900                                                                          
009000*01  -COPY WMSGAREA                                                       
009100     EJECT                                                                
009200     03  MOD REDEFINES MSG-AREA.                                          
009300*      05  -COPY W90427O1                                                 
009400     EJECT                                                                
009500 01  FILLER                  PIC X(16)   VALUE 'MFS-AREA'.                
009600*01  -COPY WMFSAREA                                                       
009700     EJECT                                                                
009800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009900                                                                          
010000 01  FILLER                  PIC X(16)   VALUE 'IMS-WS'.                  
010100                                                                          
010200 01  SPAR-AREA.                                                           
010300   03  SPAR-IDTRANS          PIC X(4)    VALUE SPACE.                     
010400   03  SPAR-K801-ENTER-MIN   PIC X(13)   VALUE SPACE.                     
010500   03  SPAR-K801-ENTER-MAX   PIC X(13)   VALUE SPACE.                     
010600   03  SPAR-K801-NEXT-MIN    PIC X(13)   VALUE SPACE.                     
010700   03  SPAR-K801-NEXT-MAX    PIC X(13)   VALUE SPACE.                     
010800   03  SPAR-IDLANDX2         PIC X(2)    VALUE SPACE.                     
010900                                                                          
011000 01  NYCKLAR-TILL-DLI.                                                    
011100   03  W-IDARTNR-X.                                                       
011200     05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.               
011300   03  W-IDAIGRP-X.                                                       
011400     05  W-IDLANDX2          PIC X(2)    VALUE SPACE.                     
011500     05  W-KDAIKTYP          PIC X(1)    VALUE LOW-VALUE.                 
011600   03  W-IDSKYLT-X.                                                       
011700     05  W-IDSKYLT           PIC X(3)    VALUE 'GB '.                     
011800                                                                          
011900   03  W-WDK801KY-MIN.                                                    
012000     05  W-IDARTNR-MIN       PIC S9(9)   VALUE ZERO  COMP-3.              
012100     05  W-KDSEGKEY-MIN      PIC X       VALUE SPACE.                     
012200     05  W-IDAIGRP-MIN.                                                   
012300       07  W-IDLANDX2-MIN    PIC X(2)    VALUE LOW-VALUE.                 
012400       07  W-KDAIKTYP-MIN    PIC X(1)    VALUE LOW-VALUE.                 
012500     05  W-IDKUNDNR-MIN      PIC S9(7)   VALUE ZERO  COMP-3.              
012600                                                                          
012700   03  W-WDK801KY-MAX.                                                    
012800     05  W-IDARTNR-MAX       PIC S9(9)   VALUE ZERO  COMP-3.              
012900     05  W-KDSEGKEY-MAX      PIC X       VALUE SPACE.                     
013000     05  W-IDAIGRP-MAX.                                                   
013100       07  W-IDLANDX2-MAX    PIC X(2)    VALUE HIGH-VALUE.                
013200       07  W-KDAIKTYP-MAX    PIC X(1)    VALUE HIGH-VALUE.                
013300     05  W-IDKUNDNR-MAX      PIC S9(7)   VALUE +9999999 COMP-3.           
013400                                                                          
013500     EJECT                                                                
013600*    --- STATUS-KOD FRÅN IMS                                              
013700 01  STATUS-WS               PIC XX.                                      
013800     88  SEGMENT-FINNS                   VALUE '  '.                      
013900     88  SEGMENT-SAKNAS                  VALUE 'GE'.                      
014000                                                                          
014100                                                                          
014200 01  GODK-STATUSKODER.                                                    
014300   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
014400                                                                          
014500                                                                          
014600 01  SSA1                    PIC X(96).                                   
014700 01  SSA2                    PIC X(96).                                   
014800     EJECT                                                                
014900*    --- IMS FUNKTIONSKODER                                               
015000*01  -COPY W0003                                                          
015100     EJECT                                                                
015200 01  FILLER                  PIC X(16)   VALUE 'DLI-IO-ARTF01'.           
015300                                                                          
015400 01  DLI-IO-ARTF01.                                                       
015500*  03  -COPY WDK801  -PRE ARTF-                                           
015600                                                                          
015700     EJECT                                                                
016400 LINKAGE SECTION.                                                         
016500*01  -COPY W0009  -PRE MSG-                                               
016600                                                                          
016700*01  -COPY W0008  -PRE USEA-                                              
016800     05  FILLER              PIC X.                                       
016900                                                                          
017000     EJECT                                                                
017100*01  -COPY W0008  -PRE ARTF-                                              
017200     05  FILLER              PIC X.                                       
017300                                                                          
017800 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB ARTF-PCB.                     
017900 MAIN SECTION.                                                            
018000     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB ARTF-PCB.                     
018100                                                                          
018200     PERFORM IMS-GET-MSG                                                  
018300     IF SEGMENT-FINNS                                                     
018400       PERFORM A-INIT                                                     
018500       PERFORM B-KOLLA-NYCKLAR                                            
018600       IF NYCKLAR-OK                                                      
018700         IF MFS-FIRST                                                     
018800           PERFORM C-FOERSTA-SIDA                                         
018900         ELSE                                                             
019000           IF MFS-NEXT                                                    
019100             PERFORM D-NAESTA-SIDA                                        
019200           ELSE                                                           
019300             PERFORM E-SAMMA-SIDA                                         
019400           END-IF                                                         
019500         END-IF                                                           
019600         PERFORM F-LAES-VISA-INFO                                         
019700       END-IF                                                             
019800       COMPUTE MSG-KVLL = LENGTH OF MOD-W90427O1 + 4                      
019900       PERFORM IMS-INSERT-MSG                                             
020000     END-IF                                                               
020100                                                                          
020200     MOVE ZERO TO RETURN-CODE                                             
020300     GOBACK                                                               
020400     .                                                                    
020500     EJECT                                                                
020600 A-INIT SECTION.                                                          
020700                                                                          
020800     IF MSG-DUBBLA-TRANSKODER                                             
020900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W90427I1                 
021000       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
021100       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
021200     ELSE                                                                 
021300       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W90427I1                 
021400       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
021500       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
021600     END-IF                                                               
021700                                                                          
021800     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
021900     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
022000     MOVE MFS-IDTRANS TO W-IDTRANS                                        
022100                                                                          
022200     MOVE LOW-VALUE       TO MSG-AREA                                     
022300     MOVE 'W90427O1'      TO MFS-IDMOD                                    
022400     MOVE '9427'          TO MOD-IDTRANS                                  
022500     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
022600                                                                          
022700     IF EGEN-MID OR HELP-MID                                              
022800       CONTINUE                                                           
022900     ELSE                                                                 
023000       MOVE SPACE TO MFS-KDTRTYP                                          
023100       MOVE '7'   TO MFS-IDPFK                                            
023200     END-IF                                                               
023300                                                                          
023400     MOVE 'GB '   TO MED-IDSKYLT                                          
023500     .                                                                    
023600                                                                          
023700     EJECT                                                                
023800 B-KOLLA-NYCKLAR SECTION.                                                 
023900                                                                          
024000     MOVE JA                TO NYCKLAR-SW                                 
024100                                                                          
024200     MOVE ALL '+'           TO MSGI-WMSGINIT                              
024300     MOVE '001'             TO MSGI-KDCALL                                
024400     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
024500                               MSGI-IDLTERM-USER                          
024600     MOVE '9427'            TO MSGI-IDTRANS                               
024700                                                                          
024800     IF EGEN-MID                                                          
024900       MOVE MID-IDARTNR     TO MSGI-IDARTNR                               
025000     ELSE                                                                 
025100       IF MID-IDARTNR NUMERIC                                             
025200         MOVE MID-IDARTNR   TO MSGI-IDARTNR                               
025300       END-IF                                                             
025400     END-IF                                                               
025500                                                                          
025600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
025700     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
025800                                                                          
026000     IF MSGI-IDARTNR NOT NUMERIC                                          
026100       MOVE NEJ TO NYCKLAR-SW                                             
026200     END-IF                                                               
026300                                                                          
026500     IF EGEN-MID                                                          
026600       IF MID-IDLANDX2 = '++'                                             
026700         MOVE SPACE TO WS-IDLANDX2                                        
026800       ELSE                                                               
026900         MOVE MID-IDLANDX2 TO WS-IDLANDX2                                 
027000       END-IF                                                             
027100     ELSE                                                                 
027200       MOVE SPACE TO WS-IDLANDX2                                          
027300       MOVE '7'               TO MFS-IDPFK                                
027400       MOVE SPACE             TO MFS-KDTRTYP                              
027500     END-IF                                                               
027600                                                                          
028600     IF NYCKLAR-FEL                                                       
028700       MOVE ERR-WRONG-KEY     TO MED-IDMFSFEL                             
028800       CALL WMEDKONV USING MED-WMEDAREA                                   
028900       MOVE MED-MFSFEL        TO MOD-TEMFSFEL                             
029000     END-IF                                                               
029100                                                                          
029200     IF MID-IDARTNR = ALL '+'                                             
029300         AND MID-IDLANDX2 = ALL '+'                                       
029400       CONTINUE                                                           
029500     ELSE                                                                 
029600       MOVE '7' TO MFS-IDPFK                                              
029700     END-IF                                                               
029800     .                                                                    
029900                                                                          
030000     EJECT                                                                
030100 C-FOERSTA-SIDA SECTION.                                                  
030200                                                                          
030300     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
030400     CALL WMEDKONV USING MED-WMEDAREA                                     
030500     MOVE MED-MFSINF     TO MOD-TEMFSFEL                                  
030600                                                                          
030700     MOVE MSGI-IDARTNR  TO W-IDARTNR-MIN                                  
030800                           W-IDARTNR-MAX                                  
030900                           W-IDARTNR                                      
031000     MOVE SPACE         TO W-KDSEGKEY-MIN                                 
031100                           W-KDSEGKEY-MAX                                 
031200                                                                          
031300     IF WS-IDLANDX2 = SPACE                                               
031400       MOVE LOW-VALUE     TO W-IDAIGRP-MIN                                
031500       MOVE HIGH-VALUE    TO W-IDAIGRP-MAX                                
031600     ELSE                                                                 
031700       MOVE WS-IDLANDX2   TO W-IDLANDX2-MIN                               
031800                             W-IDLANDX2-MAX                               
031900       MOVE LOW-VALUE     TO W-KDAIKTYP-MIN                               
032000       MOVE HIGH-VALUE    TO W-KDAIKTYP-MAX                               
032100     END-IF                                                               
032200                                                                          
032300     MOVE WS-IDLANDX2   TO W-IDLANDX2                                     
032400     MOVE ZERO          TO W-IDKUNDNR-MIN                                 
032500     MOVE +9999999      TO W-IDKUNDNR-MAX                                 
032600     .                                                                    
032700                                                                          
032800     EJECT                                                                
032900 D-NAESTA-SIDA SECTION.                                                   
033000                                                                          
033100     IF SPAR-IDTRANS = '9427'                                             
033200       MOVE SPAR-K801-NEXT-MIN TO W-WDK801KY-MIN                          
033300       MOVE SPAR-K801-NEXT-MAX TO W-WDK801KY-MAX                          
033400       MOVE SPAR-IDLANDX2      TO W-IDLANDX2                              
033500       IF W-IDARTNR-MIN NUMERIC                                           
033600         MOVE W-IDARTNR-MIN    TO W-IDARTNR                               
033700       ELSE                                                               
033800         MOVE ZERO             TO W-IDARTNR                               
033900       END-IF                                                             
034000     END-IF                                                               
034100     .                                                                    
034200                                                                          
034300     SKIP3                                                                
034400 E-SAMMA-SIDA SECTION.                                                    
034500                                                                          
034600     IF SPAR-IDTRANS = '9427'                                             
034700       MOVE SPAR-K801-ENTER-MIN TO W-WDK801KY-MIN                         
034800       MOVE SPAR-K801-ENTER-MAX TO W-WDK801KY-MAX                         
034900       MOVE SPAR-IDLANDX2       TO W-IDLANDX2                             
035000       IF W-IDARTNR-MIN NUMERIC                                           
035100         MOVE W-IDARTNR-MIN    TO W-IDARTNR                               
035200       ELSE                                                               
035300         MOVE ZERO             TO W-IDARTNR                               
035400       END-IF                                                             
035500     END-IF                                                               
035600     .                                                                    
035700                                                                          
035800     EJECT                                                                
035900 F-LAES-VISA-INFO SECTION.                                                
036000                                                                          
036100     IF W-IDLANDX2 = SPACE                                                
036200       PERFORM IMS-GET-ARTF01-TOT                                         
036300     ELSE                                                                 
036400       PERFORM IMS-GET-ARTF01-LAND                                        
036500     END-IF                                                               
036600                                                                          
036700     IF SEGMENT-FINNS                                                     
037300       MOVE ARTF-ART-IDAIGRP  TO W-IDAIGRP-MIN                            
037400       MOVE ARTF-ART-IDKUNDNR TO W-IDKUNDNR-MIN                           
037500     ELSE                                                                 
037600       MOVE ERR-PART-MISSING TO MED-IDMFSFEL                              
037700       CALL WMEDKONV USING MED-WMEDAREA                                   
037800       MOVE MED-MFSFEL       TO MOD-TEMFSFEL                              
037900     END-IF                                                               
038000     MOVE W-WDK801KY-MIN    TO SPAR-K801-ENTER-MIN                        
038100     MOVE W-WDK801KY-MAX    TO SPAR-K801-ENTER-MAX                        
038200     MOVE W-IDLANDX2        TO SPAR-IDLANDX2                              
038300                                                                          
038400     MOVE +1 TO RAD-IX                                                    
038500     PERFORM UNTIL RAD-IX > MAX-RAD-IX                                    
038600       IF SEGMENT-FINNS                                                   
038700         PERFORM FA-REDIGERA-RAD                                          
038800         IF W-IDLANDX2 = SPACE                                            
038900           PERFORM IMS-GET-ARTF01-TOT                                     
039000         ELSE                                                             
039100           PERFORM IMS-GET-ARTF01-LAND                                    
039200         END-IF                                                           
039300       ELSE                                                               
039400         MOVE MFS-RENSA-FAELT TO MOD-RAD (RAD-IX)                         
039500       END-IF                                                             
039600       ADD +1 TO RAD-IX                                                   
039700     END-PERFORM                                                          
039800                                                                          
039900     IF SEGMENT-FINNS                                                     
040000       MOVE ARTF-ART-IDAIGRP  TO W-IDAIGRP-MIN                            
040100       MOVE ARTF-ART-IDKUNDNR TO W-IDKUNDNR-MIN                           
040200       MOVE W-WDK801KY-MIN TO SPAR-K801-NEXT-MIN                          
040300       MOVE W-WDK801KY-MAX TO SPAR-K801-NEXT-MAX                          
040400       MOVE INF-MORE-INFO TO MED-IDMFSINF                                 
040500       CALL WMEDKONV USING MED-WMEDAREA                                   
040600       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
040700     ELSE                                                                 
040800       MOVE W-WDK801KY-MIN TO SPAR-K801-NEXT-MIN                          
040900       MOVE W-WDK801KY-MAX TO SPAR-K801-NEXT-MAX                          
041000     END-IF                                                               
041200     MOVE '002'     TO MSGI-KDCALL                                        
041300     MOVE '9427'    TO SPAR-IDTRANS                                       
041400     MOVE SPAR-AREA TO MSGI-SPAR-AREA                                     
041500     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
042300     .                                                                    
042400     EJECT                                                                
042500                                                                          
042600 FA-REDIGERA-RAD  SECTION.                                                
042700     IF W-IDLANDX2 = SPACE                                                
042800       IF ARTF-ART-IDAIGRP = LOW-VALUE                                    
042900         MOVE 'TOTAL'               TO WS-KUND-AIGRP                      
043000       ELSE                                                               
043100         MOVE ARTF-ART-IDAIGRP      TO WS-KUND-AIGRP                      
043200       END-IF                                                             
043300     ELSE                                                                 
043400       IF ARTF-ART-KDAIKTYP = LOW-VALUE                                   
043500         MOVE ARTF-ART-IDAIGRP      TO WS-KUND-AIGRP                      
043600       ELSE                                                               
043700         MOVE ARTF-ART-IDKUNDNR     TO WS-IDKUNDNR                        
043800       END-IF                                                             
043900     END-IF                                                               
044000                                                                          
044100     MOVE ARTF-ART-KDSHELFL         TO WS-KDSHELFL                        
044200     MOVE ARTF-ART-KVBEST           TO WS-KVBEST                          
044300     MOVE ARTF-ART-KVLS             TO WS-KVLS                            
044400     MOVE ARTF-ART-KVROS            TO WS-KVROS                           
044500     MOVE ARTF-ART-KVVRPROGN        TO WS-KVVRPROGN                       
044600     MOVE ARTF-ART-KVLEVART(1)      TO WS-KVLEVART1                       
044700     MOVE ARTF-ART-KVLEVART(2)      TO WS-KVLEVART2                       
044800     MOVE ARTF-ART-KVLEVART-INNEV   TO WS-KVLEVART-INNEV                  
044900     MOVE ARTF-ART-KVLEVART-FOREG   TO WS-KVLEVART-FOREG                  
045000     MOVE ARTF-ART-KVLEVART-F-FOREG TO WS-KVLEVART-F-FOREG                
045100                                                                          
045200     MOVE WS-RAD                    TO MOD-RAD (RAD-IX)                   
045300     .                                                                    
045400                                                                          
045500     EJECT                                                                
045600 IMS-GET-MSG SECTION.                                                     
045700                                                                          
045800     MOVE '  QC' TO GODK-STATUSKODER                                      
045900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
046000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
046100     PERFORM IMS-STATUSKONTROLL                                           
046200     .                                                                    
046300                                                                          
046400                                                                          
046500 IMS-INSERT-MSG SECTION.                                                  
046600                                                                          
046700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
046800     MOVE SPACE TO GODK-STATUSKODER                                       
046900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
047000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
047100     PERFORM IMS-STATUSKONTROLL                                           
047200     .                                                                    
047300                                                                          
047400     EJECT                                                                
047500 IMS-GET-ARTF01-TOT    SECTION.                                           
047600                                                                          
047700     STRING 'WLARTF01(WDK801KY=>' W-WDK801KY-MIN                          
047800                    '&WDK801KY<=' W-WDK801KY-MAX                          
047900                    '&KDAIKTYP =' W-KDAIKTYP ')'                          
048000             DELIMITED BY SIZE INTO SSA1                                  
048100     MOVE '  GE' TO GODK-STATUSKODER                                      
048200                                                                          
048300     CALL CBLTDLI USING GN ARTF-PCB DLI-IO-ARTF01 SSA1                    
048400                                                                          
048500     MOVE ARTF-STATUS-CODE TO STATUS-WS                                   
048600     PERFORM IMS-STATUSKONTROLL                                           
048700     .                                                                    
048800                                                                          
048900                                                                          
049000                                                                          
049100 IMS-GET-ARTF01-LAND   SECTION.                                           
049200                                                                          
049300     STRING 'WLARTF01(WDK801KY=>' W-WDK801KY-MIN                          
049400                    '&WDK801KY<=' W-WDK801KY-MAX ')'                      
049500             DELIMITED BY SIZE INTO SSA1                                  
049600     MOVE '  GE' TO GODK-STATUSKODER                                      
049700                                                                          
049800     CALL CBLTDLI USING GN ARTF-PCB DLI-IO-ARTF01 SSA1                    
049900                                                                          
050000     MOVE ARTF-STATUS-CODE TO STATUS-WS                                   
050100     PERFORM IMS-STATUSKONTROLL                                           
050200     .                                                                    
050500                                                                          
050600     EJECT                                                                
052000 IMS-STATUSKONTROLL SECTION.                                              
052100                                                                          
052200     SET STATUS-IX TO 1                                                   
052300     SEARCH GODK-STATUS                                                   
052400       AT END                                                             
052500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
052600         DELIMITED BY SIZE INTO FELTEXT                                   
052700         CALL FELLOG                                                      
052800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
052900         CONTINUE                                                         
053000     END-SEARCH                                                           
053100     .                                                                    
