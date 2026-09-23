000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W9012100.                                                
000400 AUTHOR.         GUNNEL ERIKSSON                                          
000500 DATE-WRITTEN.   APRIL   1988.                                            
000600***  REMARKS   ************************************************           
000700*                                                             *           
000800*    FUNKTION.                                                *           
000900*                                                             *           
001000*        PROGRAMMET ÄR EN KÖ-BILD                             *           
001100*                                                             *           
001200*        BEREDAREN ANGER   PROJ(IDPROJ), MARKET(KDBASLM)      *           
001300*        OCH EVENTUELLT BPSR(KDBPSR).                         *           
001400*        HAN FÅR DÅ DE ARTIKLAR SOM LIGGER UNDER DE/DEN       *           
001500*        ANGIVNA NYCKELN.                                     *           
001600*        FÖR ATT KOMMA TILL NÄSTA BILD SÅ VÄLJER HAN UT EN    *           
001700*        ARTIKEL MED ETT 'S' OCH TRYCKER PFK9.                *           
001800*                                                             *           
001900*        BASER SOM BERÖRS:                                    *           
002000*                                                             *           
002100* --------       WLARTK, ARTK01      FÖR ATT MED HJÄLP AV     *           
002200* IDARTNR                IDPROJ   OCH KDBASLM HITTA IDARTNR.  *           
002300* KDBPSR                 IDARTNR OCH KDBPSR HÄMTAS TILL BILD. *           
002400* -------        WLARTG, FRÅN ARTG01 HÄMTAS KDPRODSL OCH      *           
002500* KDPRODSL                                         TISTOMREG. *           
002600* TISTOMREG              FRÅN ARTG11 HÄMTAS, OM SEGM. FINNS,  *           
002700* MARKNOT                MARKNOT. OM EJ SPACE FLYTTAS * UT.   *           
002800* -------        WLARTC, FRÅN ARTC01 HÄMTAS IDFKNGRP. *       *           
002900* IDFKNGRP                                                    *           
003000* -------        WLBENA, FRÅN BENA11 HÄMTAS TEXT-BEART,       *           
003100* TEXT-BEART             SEQ-NYCKEL WDD3B UTNYTTJAS.          *           
003200* -------        WLXXAP, FRÅN NYCKEL KOLLAS KDBASLM           *           
003300***************************************************************           
003400* ÄNDRING (AUG -91/C.E.)  I DA-FYLL-I-BILDEN SECTION.                     
003500* I DE FALL TISTOMREG FINNS PÅ ARTG11, LÄGGS DETTA DATUM UT               
003600* ISTÄLLET FÖR ROTENS TISTOMREG. KONTR-SIFF TILLAGD PÅ ARTIKLAR           
003700*                                                                         
003800***************************************************************           
003900*        TRANSAKTION: W9T121                                              
004000*        MID:         W9I12101                                            
004100*        MOD:         W9O12101                                            
004200*        FORMAT:      W0F12101                                            
004300                                                                          
004400 ENVIRONMENT DIVISION.                                                    
004500     SKIP3                                                                
004600 DATA DIVISION.                                                           
004700     EJECT                                                                
004800 WORKING-STORAGE SECTION.                                                 
004900                                                                          
005000*    -- CHECKED BY WY2000                                                 
005100 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W9012100'.            
005200 77  JA                          PIC X       VALUE 'J'.                   
005300 77  NEJ                         PIC X       VALUE 'N'.                   
005400 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
005500 77  RAD-INDX                    PIC S9(9)   VALUE +1   COMP SYNC.        
005600 77  IDARTNR-DISP                PIC  9(9)   VALUE  0.                    
005700 77  MAX-RAD                     PIC S9(2)   VALUE +13.                   
005800 77  MAX-MOD-LAENGD              PIC S9(4) VALUE +1107  COMP SYNC.        
005900                                                                          
006000*01  -COPY WWPRODSL                                                       
006100     SKIP2                                                                
006200 01  DYNAMISKA-SUBPGM.                                                    
006300     03  CBLTDLI              PIC X(8)      VALUE 'CBLTDLI'.              
006400     03  FELLOG               PIC X(8)      VALUE 'FELLOG '.              
006500     EJECT                                                                
006600 01  WS-IN-FAELT-OK              PIC X(1).                                
006700     88 IN-FAELT-OK                           VALUE 'J'.                  
006800     SKIP3                                                                
006900 01  NYCKLAR-TILL-DLI.                                                    
007000     03  W-WDD2D1KY-MIN.                                                  
007100       05  W-IDPROJ-MIN         PIC X(4)      VALUE SPACE.                
007200       05  W-KDBASLM-MIN        PIC X(6)      VALUE SPACE.                
007300       05  W-IDFKNGRP-MIN      PIC S9(5) COMP-3 VALUE ZERO.               
007400       05  W-IDARTNR-MIN       PIC S9(9) COMP-3 VALUE ZERO.               
007500                                                                          
007600     03  W-WDD2D1KY-MAX.                                                  
007700       05  W-IDPROJ-MAX     PIC X(4)      VALUE SPACE.                    
007800       05  W-KDBASLM-MAX    PIC X(6)      VALUE SPACE.                    
007900       05  FILLER           PIC S9(5) COMP-3 VALUE +99999.                
008000       05  FILLER           PIC S9(9) COMP-3 VALUE +999999999.            
008100                                                                          
008200     03    W-KDBPSR-MIN-X.                                                
008300       05  W-KDBPSR-MIN         PIC S9 VALUE +0   COMP-3.                 
008400                                                                          
008500     03    W-KDBPSR-MAX-X.                                                
008600       05  W-KDBPSR-MAX         PIC S9  VALUE +9  COMP-3.                 
008700                                                                          
008800     03  W-IDSKYLT-X.                                                     
008900        05  W-IDSKYLT         PIC X(3)      VALUE SPACE.                  
009000                                                                          
009100     03  W-IDARTNR-X.                                                     
009200        05  W-IDARTNR         PIC S9(9)     COMP-3.                       
009300                                                                          
009400     03  W-KDSEGKEY-X.                                                    
009500        05  W-KDSEGKEY        PIC X(1)      VALUE SPACE.                  
009600                                                                          
009700     03  W-KDBASLM-X.                                                     
009800        05  WX-KDBASLM        PIC X(6)      VALUE SPACE.                  
009900                                                                          
010000     03 W-1123-KEY-X.                                                     
010100       05 FILLER              PIC X(4)      VALUE '1123'.                 
010200       05 W-1123-KDPRODSL     PIC S9(3)     COMP-3 VALUE ZERO.            
010300       05 W-1123-IDPROJ       PIC X(4)      VALUE SPACE.                  
010400       05 FILLER              PIC X(20)     VALUE LOW-VALUE.              
010500                                                                          
010600     03 W-1126-KEY-X.                                                     
010700       05 W-1126-KDBASLM      PIC X(6)      VALUE SPACE.                  
010800       05 FILLER              PIC X(9)      VALUE LOW-VALUE.              
010900                                                                          
011000 01  ARBETS-FAELT-IN.                                                     
011100     03 WS-IDPROJ             PIC X(4)      VALUE SPACE.                  
011200     03 WS-KDBASLM            PIC X(6)      VALUE SPACE.                  
011300     03 WS-IDSKYLT            PIC X(3)      VALUE SPACE.                  
011400     03 WS-KDBPSR-MIN         PIC X(1)      VALUE SPACE.                  
011500     03 WS-KDBPSR-MAX         PIC X(1)      VALUE SPACE.                  
011600     03 WS-KDPRODSL           PIC X(2)      VALUE SPACE.                  
011700                                                                          
011800     EJECT                                                                
011900*01    MOD -COPY W9O12101    -PRE WS-                                     
012000     EJECT                                                                
012100*01        -COPY WWGODKID    -PRE GODK-                                   
012200     EJECT                                                                
012300 01  FEL-MEDDELANDE.                                                      
012400   03  FEL1.                                                              
012500     05 FILLER                   PIC X(40)                                
012600          VALUE 'NYCKLAR FEL'.                                            
012700     05 FILLER                   PIC X(40)                                
012800          VALUE 'WRONG KEYS'.                                             
012900   03  FILLER REDEFINES FEL1.                                             
013000     05  FEL-1                   PIC X(40)   OCCURS 2.                    
013100                                                                          
013200   03  FEL2.                                                              
013300     05 FILLER                   PIC X(40)                                
013400          VALUE ' ARTIKEL SAKNAS'.                                        
013500     05 FILLER                   PIC X(40)                                
013600          VALUE ' PARTNO MISSING'.                                        
013700   03  FILLER REDEFINES FEL2.                                             
013800     05  FEL-2                   PIC X(40)   OCCURS 2.                    
013900     SKIP3                                                                
014000 01    MEDDELANDE.                                                        
014100   03  MED1.                                                              
014200     05 FILLER                   PIC X(40)                                
014300          VALUE 'DETTA ÄR FÖRSTA SIDAN'.                                  
014400     05 FILLER                   PIC X(40)                                
014500          VALUE 'THIS IS THE FIRST PAGE'.                                 
014600   03  FILLER REDEFINES MED1.                                             
014700     05  MED-1                   PIC X(40)   OCCURS 2.                    
014800                                                                          
014900   03  MED2.                                                              
015000     05 FILLER                   PIC X(40)                                
015100          VALUE 'TRYCK PF8 FÖR FLERA RADER'.                              
015200     05 FILLER                   PIC X(40)                                
015300          VALUE 'PRESS PF8 FOR MORE LINES'.                               
015400   03  FILLER REDEFINES MED2.                                             
015500     05  MED-2                   PIC X(40)   OCCURS 2.                    
015600* - - - - - - - - - - - - - - - - - - - - - - - - - -                     
015700   03  WS-SP.                                                             
015800     05 FILLER     PIC X(3) VALUE 'S  '.                                  
015900     05 FILLER     PIC X(3) VALUE 'GB '.                                  
016000   03  FILLER REDEFINES WS-SP.                                            
016100     05  WS-SPRAK  PIC X(3) OCCURS 2.                                     
016200                                                                          
016300     EJECT                                                                
016400******************************************************************        
016500*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
016600******************************************************************        
016700     SKIP3                                                                
016800*01  MID -COPY W9I12201            -PRE  9122-                            
016900     EJECT                                                                
017000*01  MID -COPY W9I12101                                                   
017100     EJECT                                                                
017200*01  -COPY WMSGAREA                                                       
017300     EJECT                                                                
017400*03    MOD -COPY W9O12101           -RED MSG-AREA.                        
017500     EJECT                                                                
017600*01  -COPY WMFSAREA                                                       
017700     EJECT                                                                
017800******************************************************************        
017900*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
018000******************************************************************        
018100 01  IMS-WS.                                                              
018200   03  FILLER                    PIC X(16)   VALUE 'IMS-WS     '.         
018300     SKIP3                                                                
018400*                        **** STATUS-KOD FRÅN IMS                         
018500   03  STATUS-WS                 PIC XX.                                  
018600     88  SEGMENT-FINNS                       VALUE '  '.                  
018700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
018800     88  BASEN-SLUT                          VALUE 'GB'.                  
018900     SKIP3                                                                
019000   03  GODK-STATUSKODER.                                                  
019100     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019200     SKIP3                                                                
019300   03  SSA1                      PIC X(112)  VALUE SPACE.                 
019400   03  SSA2                      PIC X(96)   VALUE SPACE.                 
019500     EJECT                                                                
019600******************************************************************        
019700*                            IMS FUNKTIONSKODER                           
019800******************************************************************        
019900*01    -COPY W0003                                                        
020000     EJECT                                                                
020100*                            DLI INPUT-OUTPUT AREA                        
020200 01  DLI-IO-AREA.                                                         
020300   03  IO-AREA-1                 PIC X(600)  VALUE SPACE.                 
020400     SKIP3                                                                
020500*  03  ARTK01    -COPY WDD2D1      -PRE ARTK01-  -RED IO-AREA-1.          
020600     EJECT                                                                
020700*  03  ARTG01    -COPY WDD201      -PRE ARTG01-  -RED IO-AREA-1.          
020800     EJECT                                                                
020900*  03  ARTG11    -COPY WDD211      -PRE ARTG11-  -RED IO-AREA-1.          
021000     EJECT                                                                
021100*  03  BENA11    -COPY WDD311      -PRE BENA11-  -RED IO-AREA-1.          
021200     EJECT                                                                
021300*  03  WLXXAP12  -COPY WDGX1126    -PRE XXAP-    -RED IO-AREA-1.          
021400     EJECT                                                                
021500   03  IO-AREA-2                 PIC X(900) VALUE SPACE.                  
021600*  03  ARTC01    -COPY WDK601                    -RED IO-AREA-2.          
021700     EJECT                                                                
021800*  03  ARTC11    -COPY WDK611                    -RED IO-AREA-2.          
021900     EJECT                                                                
022000 LINKAGE SECTION.                                                         
022100*01  -COPY W0009     -PRE MSG-                                            
022200     EJECT                                                                
022300*01  -COPY W0008     -PRE ARTC-                                           
022400     05  FILLER                  PIC X(5).                                
022500     EJECT                                                                
022600*01  -COPY W0008     -PRE ARTK-                                           
022700     05  FILLER                  PIC X(13).                               
022800     EJECT                                                                
022900*01  -COPY W0008     -PRE ARTG-                                           
023000     05  FILLER                  PIC X(9).                                
023100     EJECT                                                                
023200*01  -COPY W0008     -PRE BENA-                                           
023300     05  FILLER                  PIC X(8).                                
023400     EJECT                                                                
023500*01  -COPY W0008     -PRE XXAP-                                           
023600     05  FILLER                  PIC X(8).                                
023700     EJECT                                                                
023800 PROCEDURE DIVISION  USING MSG-PCB ARTC-PCB ARTK-PCB                      
023900                                   ARTG-PCB BENA-PCB XXAP-PCB.            
024000     ENTRY 'DLITCBL' USING MSG-PCB ARTC-PCB ARTK-PCB                      
024100                                   ARTG-PCB BENA-PCB XXAP-PCB.            
024200 STYR SECTION.                                                            
024300     PERFORM IMS-GET-MSG                                                  
024400     IF SEGMENT-FINNS                                                     
024500       PERFORM A-INIT                                                     
024600       PERFORM B-FLYTTA-TILL-WS                                           
024700       PERFORM C-KOLLA-INDATA-FLYTTA-NYCKLAR                              
024800                                                                          
024900       IF IN-FAELT-OK                                                     
025000         PERFORM D-LAES-VISA-INFO                                         
025100       ELSE                                                               
025200         MOVE FEL-1(SPRAK-IX) TO MOD-TEMFSFEL                             
025300         PERFORM MFS-RENSA-MOD-RAD                                        
025400       END-IF                                                             
025500       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
025600       PERFORM IMS-INSERT-MSG                                             
025700     END-IF                                                               
025800     MOVE ZERO TO RETURN-CODE                                             
025900     GOBACK                                                               
026000     .                                                                    
026100     EJECT                                                                
026200 A-INIT SECTION.                                                          
026300     IF MSG-DUBBLA-TRANSKODER                                             
026400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W9I12101                 
026500       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
026600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
026700     ELSE                                                                 
026800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W9I12101                  
026900       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
027000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
027100     END-IF                                                               
027200                                                                          
027300     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
027400     MOVE MSG-IDPFK            TO MFS-IDPFK                               
027500                                                                          
027600     IF MFS-IDTRANS NOT = '9121'                                          
027700       IF MFS-IDTRANS = '9122'                                            
027800         MOVE MID-W9I12101   TO 9122-MID-W9I12201                         
027900         MOVE 9122-MID-KDBPSR-MIN TO MID-KDBPSR-1-UT                      
028000         MOVE 9122-MID-KDBPSR-MAX TO MID-KDBPSR-2-UT                      
028100         MOVE 9122-MID-IDARTNR-UT TO MID-IDARTNR-ENTER                    
028200         INSPECT MID-IDARTNR-ENTER                                        
028300                           REPLACING LEADING SPACE BY ZERO                
028400         MOVE 9122-MID-IDFKNGRP-MIN   TO MID-IDFKNGRP-ENTER               
028500         INSPECT MID-IDFKNGRP-ENTER                                       
028600                           REPLACING LEADING SPACE BY ZERO                
028700         MOVE '++'                 TO MID-KDPRODSL-IN                     
028800         MOVE 9122-MID-KDPRODSL-UT TO MID-KDPRODSL-UT                     
028900       ELSE                                                               
029000         IF MFS-IDTRANS NOT = '0551'                                      
029100           PERFORM MFS-RENSA-MID-IN                                       
029200           MOVE SPACE TO MFS-KDTRTYP                                      
029300           MOVE '7' TO MFS-IDPFK                                          
029400         END-IF                                                           
029500       END-IF                                                             
029600     END-IF                                                               
029700                                                                          
029800     IF ENGLISH-TEXT                                                      
029900       MOVE +2 TO SPRAK-IX                                                
030000     ELSE                                                                 
030100       MOVE +1 TO SPRAK-IX                                                
030200     END-IF                                                               
030300                                                                          
030400     MOVE LOW-VALUE TO MSG-AREA                                           
030500     MOVE 'W9O12101' TO MFS-IDMOD                                         
030600     MOVE '9121' TO MOD-IDTRANS                                           
030700     PERFORM MFS-RENSA-MOD-IN                                             
030800                                                                          
030900     .                                                                    
031000     EJECT                                                                
031100 B-FLYTTA-TILL-WS  SECTION.                                               
031200                                                                          
031300     IF MID-IDPROJ-IN = ALL '+'                                           
031400        MOVE MID-IDPROJ-UT TO WS-IDPROJ                                   
031500     ELSE                                                                 
031600        MOVE SPACE TO MFS-KDTRTYP                                         
031700        MOVE '7'             TO MFS-IDPFK                                 
031800        MOVE MID-IDPROJ-IN TO WS-IDPROJ                                   
031900     END-IF                                                               
032000                                                                          
032100     IF MID-KDBASLM-IN = ALL '+'                                          
032200        MOVE MID-KDBASLM-UT TO WS-KDBASLM                                 
032300     ELSE                                                                 
032400        MOVE SPACE TO MFS-KDTRTYP                                         
032500        MOVE '7'             TO MFS-IDPFK                                 
032600        MOVE MID-KDBASLM-IN  TO WS-KDBASLM                                
032700     END-IF                                                               
032800                                                                          
032900     IF MID-IDSKYLT-IN = ALL '+'                                          
033000        MOVE MID-IDSKYLT-UT TO WS-IDSKYLT                                 
033100     ELSE                                                                 
033200        MOVE SPACE TO MFS-KDTRTYP                                         
033300        MOVE '7'             TO MFS-IDPFK                                 
033400        MOVE MID-IDSKYLT-IN  TO WS-IDSKYLT                                
033500     END-IF                                                               
033600                                                                          
033700     EJECT                                                                
033800                                                                          
033900     IF MID-KDBPSR-1-IN = ALL '+'                                         
034000        MOVE MID-KDBPSR-1-UT TO WS-KDBPSR-MIN                             
034100     ELSE                                                                 
034200        MOVE SPACE TO MFS-KDTRTYP                                         
034300        MOVE '7'             TO MFS-IDPFK                                 
034400        MOVE MID-KDBPSR-1-IN  TO WS-KDBPSR-MIN                            
034500     END-IF                                                               
034600     INSPECT WS-KDBPSR-MIN REPLACING ALL SPACE BY ZERO                    
034700                                                                          
034800     IF MID-KDBPSR-2-IN = ALL '+'                                         
034900        MOVE MID-KDBPSR-2-UT TO WS-KDBPSR-MAX                             
035000     ELSE                                                                 
035100        MOVE SPACE TO MFS-KDTRTYP                                         
035200        MOVE '7'             TO MFS-IDPFK                                 
035300        MOVE MID-KDBPSR-2-IN  TO WS-KDBPSR-MAX                            
035400     END-IF                                                               
035500     INSPECT WS-KDBPSR-MAX REPLACING ALL SPACE BY ZERO                    
035600                                                                          
035700     IF MID-KDPRODSL-IN = ALL '+'                                         
035800        MOVE MID-KDPRODSL-UT  TO WS-KDPRODSL                              
035900     ELSE                                                                 
036000        MOVE MID-KDPRODSL-IN  TO WS-KDPRODSL                              
036100        MOVE SPACE            TO MFS-KDTRTYP                              
036200        MOVE '7'              TO MFS-IDPFK                                
036300     END-IF                                                               
036400     INSPECT WS-KDPRODSL REPLACING ALL SPACE BY ZERO                      
036500     .                                                                    
036600     EJECT                                                                
036700 C-KOLLA-INDATA-FLYTTA-NYCKLAR  SECTION.                                  
036800                                                                          
036900     MOVE JA TO WS-IN-FAELT-OK                                            
037000                                                                          
037100     IF WS-IDPROJ = SPACE                                                 
037200        MOVE NEJ TO WS-IN-FAELT-OK                                        
037300     END-IF                                                               
037400                                                                          
037500     IF WS-KDBASLM   NUMERIC                                              
037600        MOVE NEJ TO WS-IN-FAELT-OK                                        
037700     END-IF                                                               
037800                                                                          
037900     IF WS-KDBASLM = SPACE                                                
038000        MOVE NEJ TO WS-IN-FAELT-OK                                        
038100     END-IF                                                               
038200                                                                          
038300     IF WS-IDSKYLT = SPACE                                                
038400       MOVE WS-SPRAK(SPRAK-IX) TO WS-IDSKYLT                              
038500     ELSE                                                                 
038600       MOVE WS-IDSKYLT TO GODK-IDSKYLT                                    
038700       IF  GODK-IDSKYLT-VAERDEN                                           
038800         CONTINUE                                                         
038900       ELSE                                                               
039000         MOVE NEJ TO WS-IN-FAELT-OK                                       
039100       END-IF                                                             
039200     END-IF                                                               
039300                                                                          
039400     IF WS-KDBPSR-MIN NUMERIC AND WS-KDBPSR-MAX NUMERIC                   
039500                                                                          
039600       IF WS-KDBPSR-MIN = ZERO AND WS-KDBPSR-MAX = ZERO                   
039700         MOVE 9 TO WS-KDBPSR-MAX                                          
039800       ELSE                                                               
039900         IF WS-KDBPSR-MIN NOT = ZERO  AND WS-KDBPSR-MAX = ZERO            
040000           MOVE WS-KDBPSR-MIN  TO  WS-KDBPSR-MAX                          
040100         ELSE                                                             
040200           IF  WS-KDBPSR-MIN > WS-KDBPSR-MAX                              
040300             MOVE NEJ TO WS-IN-FAELT-OK                                   
040400           END-IF                                                         
040500         END-IF                                                           
040600       END-IF                                                             
040700     ELSE                                                                 
040800       MOVE NEJ TO WS-IN-FAELT-OK                                         
040900     END-IF                                                               
041000                                                                          
041100     IF WS-KDPRODSL NUMERIC                                               
041200       MOVE WS-KDPRODSL TO TEST-KDPRODSL                                  
041300       IF (NOT KDPRODSL-VOLVO-UTAN-EMB) AND (WS-KDPRODSL > ZERO)          
041400         MOVE NEJ TO WS-IN-FAELT-OK                                       
041500       END-IF                                                             
041600     ELSE                                                                 
041700       MOVE NEJ TO WS-IN-FAELT-OK                                         
041800     END-IF                                                               
041900                                                                          
042000     MOVE WS-IDPROJ       TO MOD-IDPROJ-UT                                
042100     MOVE WS-KDBASLM      TO MOD-KDBASLM-UT                               
042200     MOVE WS-IDSKYLT      TO MOD-IDSKYLT-UT                               
042300     MOVE WS-KDBPSR-MIN   TO MOD-KDBPSR-1-UT                              
042400     INSPECT MOD-KDBPSR-1-UT  REPLACING ALL ZERO BY SPACE                 
042500     MOVE WS-KDBPSR-MAX   TO MOD-KDBPSR-2-UT                              
042600     INSPECT MOD-KDBPSR-2-UT  REPLACING ALL ZERO BY SPACE                 
042700     MOVE WS-KDPRODSL     TO MOD-KDPRODSL-UT                              
042800     INSPECT MOD-KDPRODSL-UT REPLACING LEADING ZERO BY SPACE              
042900                                                                          
043000     IF IN-FAELT-OK                                                       
043100*      * KOLLA OM RIKTIGT INMATAT KDBASLM ELLER PROJ *                    
043200       MOVE WC-KDPRODSL-VCC-PARTS TO W-1123-KDPRODSL                      
043300       MOVE WS-IDPROJ  TO W-1123-IDPROJ                                   
043400       MOVE WS-KDBASLM TO W-1126-KDBASLM                                  
043500       PERFORM IMS-GET-PROJ-ROT                                           
043600       IF SEGMENT-FINNS                                                   
043700         PERFORM IMS-GNP-1126-UNIK                                        
043800         IF SEGMENT-FINNS                                                 
043900           CONTINUE                                                       
044000         ELSE                                                             
044100           MOVE NEJ TO WS-IN-FAELT-OK                                     
044200         END-IF                                                           
044300       ELSE                                                               
044400         MOVE NEJ TO WS-IN-FAELT-OK                                       
044500       END-IF                                                             
044600     END-IF                                                               
044700                                                                          
044800     IF IN-FAELT-OK                                                       
044900       MOVE WS-IDPROJ     TO W-IDPROJ-MIN    W-IDPROJ-MAX                 
045000       MOVE WS-KDBASLM    TO W-KDBASLM-MIN   W-KDBASLM-MAX                
045100                             WX-KDBASLM                                   
045200       MOVE WS-IDSKYLT    TO W-IDSKYLT                                    
045300       MOVE WS-KDBPSR-MIN TO W-KDBPSR-MIN                                 
045400       MOVE WS-KDBPSR-MAX TO W-KDBPSR-MAX                                 
045500                                                                          
045600       IF MFS-IDPFK = '7'                                                 
045700         MOVE ZERO TO W-IDARTNR                                           
045800                      W-IDARTNR-MIN                                       
045900                      W-IDFKNGRP-MIN                                      
046000         MOVE MED-1 (SPRAK-IX) TO MOD-TEMFSINF                            
046100       ELSE                                                               
046200         IF MFS-IDPFK = '8'                                               
046300           IF MID-IDARTNR-PF8 NUMERIC                                     
046400             MOVE MID-IDARTNR-PF8 TO W-IDARTNR                            
046500                                     W-IDARTNR-MIN                        
046600             IF MID-IDARTNR-PF8 = ZERO                                    
046700               MOVE '7'  TO MFS-IDPFK                                     
046800               MOVE MED-1 (SPRAK-IX) TO MOD-TEMFSINF                      
046900             END-IF                                                       
047000           ELSE                                                           
047100             MOVE ZERO TO W-IDARTNR W-IDARTNR-MIN                         
047200             MOVE '7'  TO MFS-IDPFK                                       
047300             MOVE MED-1 (SPRAK-IX) TO MOD-TEMFSINF                        
047400           END-IF                                                         
047500                                                                          
047600           IF MID-IDFKNGRP-PF8 NUMERIC                                    
047700             MOVE MID-IDFKNGRP-PF8 TO  W-IDFKNGRP-MIN                     
047800             IF MID-IDFKNGRP-PF8 = ZERO                                   
047900               MOVE '7'  TO MFS-IDPFK                                     
048000               MOVE MED-1 (SPRAK-IX) TO MOD-TEMFSINF                      
048100             END-IF                                                       
048200           ELSE                                                           
048300             MOVE ZERO TO  W-IDFKNGRP-MIN                                 
048400             MOVE '7'  TO MFS-IDPFK                                       
048500             MOVE MED-1 (SPRAK-IX) TO MOD-TEMFSINF                        
048600           END-IF                                                         
048700         ELSE                                                             
048800           IF MID-IDARTNR-ENTER NUMERIC                                   
048900             MOVE MID-IDARTNR-ENTER TO W-IDARTNR                          
049000                                       W-IDARTNR-MIN                      
049100           ELSE                                                           
049200             MOVE ZERO TO W-IDARTNR W-IDARTNR-MIN                         
049300           END-IF                                                         
049400                                                                          
049500           IF MID-IDFKNGRP-ENTER NUMERIC                                  
049600             MOVE MID-IDFKNGRP-ENTER TO W-IDFKNGRP-MIN                    
049700           ELSE                                                           
049800             MOVE ZERO TO W-IDFKNGRP-MIN                                  
049900           END-IF                                                         
050000         END-IF                                                           
050100       END-IF                                                             
050200     END-IF                                                               
050300     .                                                                    
050400     EJECT                                                                
050500 D-LAES-VISA-INFO SECTION.                                                
050600     SKIP2                                                                
050700     PERFORM IMS-GN-WLARTK01-KDBPSR                                       
050800                                                                          
050900     IF SEGMENT-FINNS                                                     
051000       MOVE ARTK01-SEQD-IDARTNR  TO MOD-IDARTNR-ENTER                     
051100       MOVE ARTK01-SEQD-IDFKNGRP TO MOD-IDFKNGRP-ENTER                    
051200     ELSE                                                                 
051300       MOVE ZERO TO MOD-IDARTNR-ENTER                                     
051400                    MOD-IDFKNGRP-ENTER                                    
051500       MOVE FEL-2(SPRAK-IX) TO MOD-TEMFSFEL                               
051600     END-IF                                                               
051700                                                                          
051800     PERFORM UNTIL RAD-INDX > MAX-RAD                                     
051900       IF SEGMENT-FINNS                                                   
052000         PERFORM DA-KOLLA-ARTIKEL-FYLL-I-BILD                             
052100         PERFORM IMS-GN-WLARTK01-KDBPSR                                   
052200       ELSE                                                               
052300         PERFORM MFS-RENSA-MOD-RAD                                        
052400       END-IF                                                             
052500       ADD +1 TO RAD-INDX                                                 
052600     END-PERFORM                                                          
052700                                                                          
052800     IF SEGMENT-FINNS                                                     
052900       MOVE ARTK01-SEQD-IDARTNR TO MOD-IDARTNR-PF8                        
053000       MOVE ARTK01-SEQD-IDFKNGRP TO MOD-IDFKNGRP-PF8                      
053100       MOVE MED-2 (SPRAK-IX) TO MOD-TEMFSINF                              
053200     ELSE                                                                 
053300       MOVE ZERO TO  MOD-IDARTNR-PF8                                      
053400                     MOD-IDFKNGRP-PF8                                     
053500     END-IF                                                               
053600     .                                                                    
053700     EJECT                                                                
053800 DA-KOLLA-ARTIKEL-FYLL-I-BILD SECTION.                                    
053900                                                                          
054000     MOVE ARTK01-SEQD-IDARTNR  TO W-IDARTNR                               
054100                                  W-IDARTNR-MIN                           
054200                                  WS-MOD-IDARTNR(RAD-INDX)                
054300     MOVE ARTK01-SEQD-IDFKNGRP TO W-IDFKNGRP-MIN                          
054400                                                                          
054500     PERFORM IMS-GU-WLARTC01                                              
054600                                                                          
054700     IF TEST-KDPRODSL = ZERO                                              
054800       PERFORM DAA-FYLL-I-BILDRAD                                         
054900     ELSE                                                                 
055000       IF TEST-KDPRODSL = ART-KDPRODSL                                    
055100         PERFORM DAA-FYLL-I-BILDRAD                                       
055200       ELSE                                                               
055300         SUBTRACT 1 FROM RAD-INDX                                         
055400       END-IF                                                             
055500     END-IF                                                               
055600     .                                                                    
055700     EJECT                                                                
055800 DAA-FYLL-I-BILDRAD    SECTION.                                           
055900                                                                          
056000     MOVE '-'                  TO WS-MOD-STRECK  (RAD-INDX)               
056100     MOVE ART-REKSIFFR         TO WS-MOD-REKSIFFR (RAD-INDX)              
056200     MOVE ART-KDPRODSL         TO WS-MOD-KDPRODSL (RAD-INDX)              
056300     MOVE ART-IDFKNGRP         TO WS-MOD-IDFKNGRP (RAD-INDX)              
056400     PERFORM IMS-GET-ARTC11                                               
056500     IF SEGMENT-FINNS                                                     
056600        MOVE CLAG-KDBPSR       TO WS-MOD-KDBPSR (RAD-INDX)                
056700     END-IF                                                               
056800                                                                          
056900     PERFORM IMS-GU-WLARTG01                                              
057000                                                                          
057100     MOVE ARTG01-ART-TISTOMREG TO WS-MOD-TISTOMREG(RAD-INDX)              
057200                                                                          
057300     PERFORM IMS-GNP-WLARTG11                                             
057400                                                                          
057500     IF ARTG11-ART-TISTOMREG > ZERO                                       
057600       MOVE ARTG11-ART-TISTOMREG TO WS-MOD-TISTOMREG(RAD-INDX)            
057700     END-IF                                                               
057800     IF ARTG11-ART-TEARTNOT-MARK NOT = SPACE                              
057900       MOVE '*' TO WS-MOD-FL-TEARTNOT-MARK (RAD-INDX)                     
058000     END-IF                                                               
058100     IF ARTG11-ART-FLBLMQ = JA                                            
058200        MOVE '+' TO WS-MOD-FLBLMQ (RAD-INDX)                              
058300     END-IF                                                               
058400                                                                          
058500     PERFORM IMS-GU-BENA                                                  
058600                                                                          
058700     MOVE BENA11-TEXT-BEART    TO WS-MOD-BEART (RAD-INDX)                 
058800     MOVE WS-MOD-RAD(RAD-INDX) TO MOD-RAD(RAD-INDX)                       
058900     .                                                                    
059000     EJECT                                                                
059100 MFS-RENSA-MID-IN SECTION.                                                
059200     MOVE MFS-RENSA-FAELT TO MID-IDPROJ-IN                                
059300                             MID-KDBASLM-IN                               
059400                             MID-IDSKYLT-IN                               
059500                             MID-KDBPSR-1-IN                              
059600                             MID-KDBPSR-2-IN                              
059700                             MID-KDPRODSL-IN                              
059800                             MID-IDFKNGRP-PF8                             
059900                             MID-IDFKNGRP-ENTER                           
060000     .                                                                    
060100     SKIP2                                                                
060200 MFS-RENSA-MOD-IN SECTION.                                                
060300     MOVE MFS-RENSA-FAELT TO MOD-IDPROJ-IN                                
060400                             MOD-KDBASLM-IN                               
060500                             MOD-IDSKYLT-IN                               
060600                             MOD-KDBPSR-1-IN                              
060700                             MOD-KDBPSR-2-IN                              
060800                             MOD-KDPRODSL-IN                              
060900                             MOD-IDFKNGRP-PF8                             
061000                             MOD-IDFKNGRP-ENTER                           
061100                             MOD-TEMFSFEL                                 
061200                             MOD-TEMFSINF                                 
061300     .                                                                    
061400     SKIP2                                                                
061500 MFS-RENSA-MOD-RAD SECTION.                                               
061600                                                                          
061700     PERFORM UNTIL RAD-INDX > 13                                          
061800        MOVE MFS-RENSA-FAELT TO MOD-KDSVAR-KVAR     (RAD-INDX)            
061900                                MOD-IDARTNR         (RAD-INDX)            
062000                                MOD-BEART           (RAD-INDX)            
062100                                MOD-IDFKNGRP        (RAD-INDX)            
062200                                MOD-TISTOMREG       (RAD-INDX)            
062300                                MOD-KDPRODSL        (RAD-INDX)            
062400                                MOD-KDBPSR          (RAD-INDX)            
062500                                MOD-FL-TEARTNOT-MARK(RAD-INDX)            
062600                                MOD-FLBLMQ          (RAD-INDX)            
062700        ADD +1 TO RAD-INDX                                                
062800     END-PERFORM                                                          
062900                                                                          
063000     .                                                                    
063100     EJECT                                                                
063200* IMS-SECTIONER                                                           
063300     SKIP3                                                                
063400                                                                          
063500                                                                          
063600 IMS-GET-MSG SECTION.                                                     
063700                                                                          
063800     MOVE '  QC' TO GODK-STATUSKODER                                      
063900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
064000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
064100     PERFORM IMS-STATUSKONTROLL                                           
064200     .                                                                    
064300 IMS-INSERT-MSG SECTION.                                                  
064400     IF ENGLISH-TEXT                                                      
064500        MOVE 'N' TO MFS-KDHUVOMR                                          
064600     END-IF                                                               
064700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
064800     MOVE SPACE  TO GODK-STATUSKODER                                      
064900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
065000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
065100     PERFORM IMS-STATUSKONTROLL                                           
065200     .                                                                    
065300     EJECT                                                                
065400 IMS-GN-WLARTK01-KDBPSR SECTION.                                          
065500     STRING 'WLARTK01(WDD2D1KY=>' W-WDD2D1KY-MIN                          
065600                    '&WDD2D1KY=<' W-WDD2D1KY-MAX                          
065700                    '&KDBPSR  =>' W-KDBPSR-MIN-X                          
065800                    '&KDBPSR  =<' W-KDBPSR-MAX-X ')'                      
065900                       DELIMITED BY SIZE INTO SSA1                        
066000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
066100     CALL CBLTDLI USING GN   ARTK-PCB IO-AREA-1 SSA1                      
066200     MOVE   ARTK-STATUS-CODE TO STATUS-WS                                 
066300     PERFORM IMS-STATUSKONTROLL                                           
066400     .                                                                    
066500     EJECT                                                                
066600 IMS-GU-WLARTG01      SECTION.                                            
066700     STRING 'WLARTG01(IDARTNR  =' W-IDARTNR-X ')'                         
066800            DELIMITED BY SIZE INTO SSA1                                   
066900     MOVE '  '   TO GODK-STATUSKODER                                      
067000     CALL CBLTDLI USING GU ARTG-PCB IO-AREA-1 SSA1                        
067100     MOVE   ARTG-STATUS-CODE TO STATUS-WS                                 
067200     PERFORM IMS-STATUSKONTROLL                                           
067300     .                                                                    
067400     SKIP1                                                                
067500 IMS-GNP-WLARTG11  SECTION.                                               
067600     STRING 'WLARTG11(KDBASLM  =' W-KDBASLM-X ')'                         
067700            DELIMITED BY SIZE INTO SSA1                                   
067800     MOVE '  '   TO GODK-STATUSKODER                                      
067900     CALL CBLTDLI USING GNP ARTG-PCB IO-AREA-1 SSA1                       
068000     MOVE   ARTG-STATUS-CODE TO STATUS-WS                                 
068100     PERFORM IMS-STATUSKONTROLL                                           
068200     .                                                                    
068300     EJECT                                                                
068400 IMS-GU-WLARTC01      SECTION.                                            
068500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
068600            DELIMITED BY SIZE INTO SSA1                                   
068700     MOVE '  GE'   TO GODK-STATUSKODER                                    
068800     CALL CBLTDLI USING GU ARTC-PCB IO-AREA-2 SSA1                        
068900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
069000     PERFORM IMS-STATUSKONTROLL                                           
069100     .                                                                    
069200     EJECT                                                                
069300 IMS-GET-ARTC11 SECTION.                                                  
069400     STRING 'WLARTC11(KDSEGKEY =1)'                                       
069500            DELIMITED BY SIZE INTO SSA1                                   
069600     MOVE '  GE'   TO GODK-STATUSKODER                                    
069700     CALL CBLTDLI USING GNP ARTC-PCB IO-AREA-2 SSA1                       
069800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
069900     PERFORM IMS-STATUSKONTROLL                                           
070000     .                                                                    
070100     EJECT                                                                
070200 IMS-GU-BENA            SECTION.                                          
070300     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
070400                          DELIMITED BY  SIZE INTO SSA1                    
070500     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
070600                          DELIMITED BY  SIZE INTO SSA2                    
070700     MOVE '  '     TO GODK-STATUSKODER                                    
070800     CALL CBLTDLI USING GU BENA-PCB IO-AREA-1 SSA1 SSA2                   
070900     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
071000     PERFORM IMS-STATUSKONTROLL                                           
071100     .                                                                    
071200     EJECT                                                                
071300 IMS-GET-PROJ-ROT  SECTION.                                               
071400                                                                          
071500     STRING 'WLXXAP01(WDGXKEY  =' W-1123-KEY-X ')'                        
071600            DELIMITED BY SIZE INTO SSA1                                   
071700     MOVE '  GE' TO GODK-STATUSKODER                                      
071800     CALL CBLTDLI USING GU XXAP-PCB IO-AREA-1 SSA1                        
071900     MOVE XXAP-STATUS-CODE TO STATUS-WS                                   
072000     PERFORM IMS-STATUSKONTROLL                                           
072100     .                                                                    
072200     EJECT                                                                
072300 IMS-GNP-1126-UNIK  SECTION.                                              
072400                                                                          
072500     STRING 'WLXXAP12(WDGXKEY  =' W-1126-KEY-X ')'                        
072600            DELIMITED BY SIZE INTO SSA1                                   
072700     MOVE '  GE' TO GODK-STATUSKODER                                      
072800     CALL CBLTDLI USING GNP XXAP-PCB IO-AREA-1 SSA1                       
072900     MOVE XXAP-STATUS-CODE TO STATUS-WS                                   
073000     PERFORM IMS-STATUSKONTROLL                                           
073100     .                                                                    
073200 IMS-STATUSKONTROLL SECTION.                                              
073300                                                                          
073400     SET STATUS-IX TO 1                                                   
073500     SEARCH GODK-STATUS AT END CALL FELLOG                                
073600       WHEN GODK-STATUS(STATUS-IX) = STATUS-WS CONTINUE                   
073700     END-SEARCH                                                           
073800     .                                                                    
