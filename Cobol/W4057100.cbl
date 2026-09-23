000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4057100.                                                
000400 AUTHOR.         LENNART LUNDGREN ADB-GRUPPEN.                            
000500 DATE-WRITTEN.   MAJ 1988.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION. DETTA PGM HANTERAR FRÅGEBILDEN 4571 OCH LÄSER              
001000*              RESTORDERREGISTRET FÖRST VIA SEKUNDÄRINDEXET               
001100*              WDA5D1 OCH SEDAN WDA501.                                   
001200*    INDATA.                                                              
001300*        TRANSAKTION: W4T571                                              
001400*        MID:         W4I57101                                            
001500*                                                                         
001600*    UTDATA.                                                              
001700*        MOD:         W4O57101                                            
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP3                                                                
002100 DATA DIVISION.                                                           
002200     EJECT                                                                
002300 WORKING-STORAGE SECTION.                                                 
002301*    -- CHECKED BY WY2000                                                 
002302     SKIP3                                                                
002303*    -- CHECKED BY WY2000                                                 
002310     SKIP3                                                                
002400 77  PROGRAM-NAMN              PIC X(8)     VALUE 'W4057100'.             
002500 77  JA                        PIC X(1)     VALUE 'J'.                    
002600 77  NEJ                       PIC X(1)     VALUE 'N'.                    
002700 77  W-NYSIDA                  PIC X(1)     VALUE '8'.                    
002800 77  INPUT-OK                  PIC X(1)     VALUE 'J'.                    
002900 77  W-NEWKEY                  PIC X(1)     VALUE ' '.                    
003000 77  SEGM-SKALL-SKRIVAS        PIC X(1)     VALUE 'N'.                    
003100 77  MINST-1-RAD-SKRIVEN       PIC X(1)     VALUE 'N'.                    
003200 77  IDDISTR-WS                PIC X(4)     VALUE SPACE.                  
003400 77  IDARTNR-WS                PIC X(9)     VALUE SPACE.                  
003500 77  KDORDKL-WS                PIC X(1)     VALUE SPACE.                  
003600 77  KDPRODSL-WS               PIC X(2)     VALUE SPACE.                  
003700 77  IDORDNR-WS                PIC X(5)     VALUE SPACE.                  
003800 77  KDTPOTYP-WS               PIC X(1)     VALUE SPACE.                  
003900 77  INDX                      PIC S9(9)    VALUE ZERO  COMP SYNC.        
004000 77  SPRAK-IX                  PIC S9(3)    VALUE ZERO  COMP SYNC.        
004100 77  MAX-LINE                  PIC S9(9)    VALUE +14   COMP SYNC.        
004200 77  W-IDDISTR                 PIC 9(4)     VALUE ZERO.                   
004300 77  W-IDKUNDNR                PIC 9(6)     VALUE ZERO.                   
004400 77  W-IDARTNR                 PIC 9(9)     VALUE ZERO.                   
004500 77  W-KDORDKL                 PIC 9(1)     VALUE ZERO.                   
004600 77  W-KDPRODSL                PIC 9(3)     VALUE ZERO.                   
004610 77  W-IDDC                    PIC X(2)     VALUE ZERO.                   
004700 77  W-IDORDNR                 PIC 9(5)     VALUE ZERO.                   
004800 77  W-KDTPOTYP                PIC 9(1)     VALUE ZERO.                   
004900 77  WS-IDTRANS                PIC X(4).                                  
005000     88  WS-GODKAEND-BILD          VALUE '4571' '4572' '4573'.            
005010     88  EGEN-MID                  VALUE '4571'.                          
005100                                                                          
005200 01  W-IDKUNDRF.                                                          
005300     03  W-IDKUNDRF-1-5        PIC 9(5).                                  
005400     03  FILLER                PIC X(5)      VALUE SPACE.                 
005500     EJECT                                                                
005600*                                                                         
005700 01  SUBPROGRAM.                                                          
005800     03  WSECURIT              PIC X(8)      VALUE 'WSECURIT'.            
005900     03  CBLTDLI               PIC X(8)      VALUE 'CBLTDLI '.            
006000     03  FELLOG                PIC X(8)      VALUE 'FELLOG  '.            
006010     03  W005INIT              PIC X(8)    VALUE 'W005INIT'.              
006020     EJECT                                                                
006030*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
006040*01 -COPY WMSGINIT                                                        
006100     EJECT                                                                
006200*                                                                         
006300 01  NYCKLAR-TILL-DLI.                                                    
006400*                                                                         
006500     03  W-SOK-KEY-MIN.                                                   
006600         05  W-IDARTNR-MIN-X.                                             
006700             07  W-IDARTNR-MIN    PIC S9(9)  COMP-3 VALUE ZERO.           
006800         05  W-IDKUNDRF-MIN.                                              
006900             07  W-IDORDNR-MIN    PIC 9(5)          VALUE ZERO.           
007000             07  FILLER           PIC X(5)          VALUE SPACE.          
007100         05  W-IDLOPNR-MIN-X.                                             
007200             07  W-IDLOPNR-MIN    PIC S9(3)  COMP-3 VALUE ZERO.           
007300         05  W-KDORDKL-MIN-X.                                             
007400             07  W-KDORDKL-MIN    PIC S9(1)  COMP-3 VALUE ZERO.           
007500         05  W-KDPRODSL-MIN-X.                                            
007600             07  W-KDPRODSL-MIN   PIC S9(3)  COMP-3 VALUE ZERO.           
007700         05  W-KDSTARAD-MIN       PIC  X(1)         VALUE SPACE.          
007800         05  W-KDTPOTYP-MIN-X.                                            
007900             07  W-KDTPOTYP-MIN   PIC S9(1)  COMP-3 VALUE ZERO.           
007910         05  W-IDDC-MIN           PIC  X(2)         VALUE SPACE.          
008000*                                                                         
008100     03  W-SOK-KEY-MAX.                                                   
008200         05  W-IDARTNR-MAX-X.                                             
008300             07  W-IDARTNR-MAX    PIC S9(9)  COMP-3  VALUE ZERO.          
008400         05  W-IDKUNDRF-MAX.                                              
008500             07  W-IDORDNR-MAX    PIC 9(5)           VALUE ZERO.          
008600             07  FILLER           PIC X(5)           VALUE SPACE.         
008700         05  W-IDLOPNR-MAX-X.                                             
008800             07  W-IDLOPNR-MAX    PIC S9(3)  COMP-3  VALUE ZERO.          
008900         05  W-KDORDKL-MAX-X.                                             
009000             07  W-KDORDKL-MAX    PIC S9(1)  COMP-3  VALUE ZERO.          
009100         05  W-KDPRODSL-MAX-X.                                            
009200             07  W-KDPRODSL-MAX   PIC S9(3)  COMP-3  VALUE ZERO.          
009300         05  W-KDSTARAD-MAX       PIC  X(1)          VALUE '2'.           
009400         05  W-KDTPOTYP-MAX-X.                                            
009500             07  W-KDTPOTYP-MAX   PIC S9(1)  COMP-3  VALUE ZERO.          
009510         05  W-IDDC-MAX           PIC  X(2)         VALUE SPACE.          
009600     EJECT                                                                
009700     03  W-WDA5D1KY-MIN.                                                  
009800         05  W-IDDISTR-N1-MIN     PIC S9(5) COMP-3   VALUE ZERO.          
009900         05  W-IDKUNDNR-N1-MIN    PIC S9(7) COMP-3   VALUE ZERO.          
010000         05  W-IDARTNR-N1-MIN     PIC S9(9) COMP-3   VALUE ZERO.          
010100         05  W-IDKUNDRF-N1-MIN.                                           
010200             07  W-IDORDNR-N1-MIN PIC 9(5)           VALUE ZERO.          
010300             07  FILLER           PIC X(5)           VALUE SPACE.         
010400         05  W-IDLOPNR-N1-MIN     PIC S9(3) COMP-3   VALUE ZERO.          
010500                                                                          
010600     03  W-WDA5D1KY-MAX.                                                  
010700         05  W-IDDISTR-N1-MAX     PIC S9(5) COMP-3   VALUE ZERO.          
010800         05  W-IDKUNDNR-N1-MAX    PIC S9(7) COMP-3   VALUE ZERO.          
010900         05  W-IDARTNR-N1-MAX     PIC S9(9) COMP-3   VALUE ZERO.          
011000         05  W-IDKUNDRF-N1-MAX.                                           
011100             07  W-IDORDNR-N1-MAX PIC 9(5)           VALUE ZERO.          
011200             07  FILLER           PIC X(5)           VALUE SPACE.         
011300         05  W-IDLOPNR-N1-MAX     PIC S9(3) COMP-3   VALUE ZERO.          
011400                                                                          
011500     03  W-WDA501KY.                                                      
011600         05  W-IDDISTR-N2         PIC S9(5) COMP-3   VALUE ZERO.          
011700         05  W-IDKUNDNR-N2        PIC S9(7) COMP-3   VALUE ZERO.          
011800         05  W-IDKUNDRF-N2.                                               
011900             07  W-IDORDNR-N2     PIC 9(5)           VALUE ZERO.          
012000             07  FILLER           PIC X(5)           VALUE SPACE.         
012100         05  W-IDARTNR-N2         PIC S9(9) COMP-3   VALUE ZERO.          
012200         05  W-IDLOPNR-N2         PIC S9(3) COMP-3   VALUE ZERO.          
012300                                                                          
012310     03  W-IDDC-B6-X.                                                     
012320         05 W-IDDC-B6                  PIC X(2).                          
012330                                                                          
012400     EJECT                                                                
012500*01    -COPY WWTEXT01                                                     
012600     EJECT                                                                
012700*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
012800                                                                          
012900 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
013000     SKIP3                                                                
013100*01    MID -COPY W4I57101.                                                
013200     EJECT                                                                
013300*01    -COPY WMSGAREA                                                     
013400     EJECT                                                                
013500*  03    MOD -COPY W4O57101  -RED MSG-AREA.                               
013600     EJECT                                                                
013700*01    -COPY WMFSAREA                                                     
013800     EJECT                                                                
013900*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014000                                                                          
014100 01    IMS-WS.                                                            
014200   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
014300     SKIP3                                                                
014400*                        **** STATUS-KOD FRÅN IMS                         
014500   03    STATUS-WS               PIC XX.                                  
014600     88    SEGMENT-FINNS                    VALUE '  '.                   
014700     88    SEGMENT-SAKNAS                   VALUE 'GE'.                   
014800     88    SEGMENT-SLUT                     VALUE 'GB'.                   
014900     SKIP3                                                                
015000   03    GODK-STATUSKODER.                                                
015100     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
015200     SKIP3                                                                
015300 01    SSA1                      PIC X(320).                              
015400     EJECT                                                                
015500*                            IMS FUNKTIONSKODER                           
015600*01    -COPY W0003                                                        
015700     EJECT                                                                
015800*                            DLI INPUT-OUTPUT AREA                        
015900 01    DLI-IO-AREA.                                                       
016000   03    IO-AREA                 PIC X(300)  VALUE SPACE.                 
016100     SKIP3                                                                
016200*  03    WDA501 -COPY WDA501   -RED IO-AREA.                              
016300     EJECT                                                                
016400*  03    WDA5D1 -COPY WDA5D1   -RED IO-AREA.                              
016500     EJECT                                                                
016600*  03    FILLER -COPY WSECAREA .                                          
016610                                                                          
016620 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
016630 01   DLI-IO-AREA-B601.                                                   
016640*     03  -COPY WDB601                                                    
016650                                                                          
016700     EJECT                                                                
016800 LINKAGE SECTION.                                                         
016900*01    -COPY W0009     -PRE MSG-                                          
017000     EJECT                                                                
017100*01    -COPY W0008     -PRE USEA-                                         
017200     05  FILLER                  PIC X.                                   
017300     EJECT                                                                
017310*01    -COPY W0008     -PRE ORDP-                                         
017320     05  FILLER                  PIC X.                                   
017330     EJECT                                                                
017400*01    -COPY W0008     -PRE ORDT-                                         
017500     05  FILLER                  PIC X.                                   
017600     EJECT                                                                
017610*01    -COPY W0008     -PRE WDB6-                                         
017620     05  FILLER                  PIC X.                                   
017630     EJECT                                                                
017700 PROCEDURE DIVISION USING MSG-PCB  USEA-PCB ORDP-PCB ORDT-PCB             
017710                          WDB6-PCB.                                       
017800     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB ORDP-PCB ORDT-PCB             
017810                          WDB6-PCB.                                       
017900     SKIP2                                                                
018000     PERFORM IMS-GET-MSG                                                  
018100     IF SEGMENT-FINNS                                                     
018200        PERFORM A-INIT                                                    
018400                                                                          
018500        IF SEC-KDSVAR = ' ' OR                                            
018600           SEC-KDSVAR = '1' OR                                            
018700           SEC-KDSVAR = '2' OR                                            
018800           SEC-KDSVAR = '3' OR                                            
018900           SEC-KDSVAR = '5' OR                                            
019000           SEC-KDSVAR = '6'                                               
019100           IF INPUT-OK = JA                                               
019200              EVALUATE TRUE                                               
019300              WHEN WS-IDTRANS NOT = '4571'                                
019400                  PERFORM D-FLYTTA-GAMMAL-KEY                             
019500                  PERFORM M-LAES                                          
019600              WHEN MFS-IDPFK = 7                                          
019700                  PERFORM D-FLYTTA-GAMMAL-KEY                             
019800                  PERFORM M-LAES                                          
019900              WHEN MFS-IDPFK = 8                                          
020000                 IF MID-IDDISTR-SPAR NOT = '0000'                         
020100                     PERFORM C-FLYTTA-SPARAD-KEY                          
020200                 ELSE                                                     
020300                     PERFORM E1-FLYTTA-KEY                                
020400                 END-IF                                                   
020500                 PERFORM N-LAES                                           
020600              WHEN OTHER                                                  
020700                 PERFORM E2-FLYTTA-KEY                                    
020800                 IF W-NEWKEY = JA                                         
020900                    PERFORM M-LAES                                        
021000                 ELSE                                                     
021100                    PERFORM N-LAES                                        
021200                 END-IF                                                   
021300              END-EVALUATE                                                
021400           ELSE                                                           
021500              MOVE TEXT-0401(SPRAK-IX) TO MOD-TEMFSFEL                    
021600           END-IF                                                         
021700        ELSE                                                              
021800           MOVE TEXT-0405(SPRAK-IX) TO MOD-TEMFSFEL                       
021900        END-IF                                                            
022100     END-IF                                                               
022200                                                                          
022300     COMPUTE MSG-KVLL = LENGTH OF MOD-W4O57101 + 4                        
022400     PERFORM IMS-INSERT-MSG                                               
022500     MOVE    ZERO TO RETURN-CODE                                          
022600                                                                          
022700     GOBACK.                                                              
022800     EJECT                                                                
022900 A-INIT SECTION.                                                          
023000                                                                          
023100     IF MSG-DUBBLA-TRANSKODER                                             
023200         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I57101               
023300         MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                
023400         MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR               
023500     ELSE                                                                 
023600         MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W4I57101                 
023700         MOVE MSG-IDTRANS-1               TO MFS-IDTRANS                  
023800         MOVE MSG-KDMFSFOR-1              TO MFS-KDMFSFOR                 
023900     END-IF                                                               
024000     MOVE MSG-IDPFK TO MFS-IDPFK                                          
024100     MOVE MFS-IDTRANS                     TO WS-IDTRANS                   
024200                                                                          
024300     MOVE LOW-VALUE TO MSG-AREA                                           
024400     MOVE 'W4O571N1' TO MFS-IDMOD                                         
024500     MOVE '4571' TO MOD-IDTRANS                                           
024600     MOVE ZERO TO MOD-SPARADE-NYCKLAR                                     
024700                  MOD-SPARADE-NYCKLAR-ENT                                 
024800                                                                          
025500     IF MID-IDDISTR-IN       NOT = ALL '+'                                
026000         MOVE SPACE TO MFS-IDPFK                                          
026100     END-IF                                                               
026200                                                                          
026210     MOVE ALL '+'           TO MSGI-WMSGINIT                              
026220     MOVE '001'             TO MSGI-KDCALL                                
026230     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
026231     MOVE '4571'            TO MSGI-IDTRANS                               
026232     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
026240     IF EGEN-MID                                                          
026250        MOVE MID-IDDISTR-IN  TO MSGI-IDDISTR                              
026260        MOVE MID-IDKUNDNR-IN TO MSGI-IDKUNDNR                             
026270        MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                              
026280     END-IF                                                               
026290     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
026291     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
026292         MOVE +1 TO SPRAK-IX                                              
026293     ELSE                                                                 
026294         MOVE +2 TO SPRAK-IX                                              
026295     END-IF                                                               
026296                                                                          
026300     PERFORM S30-SECURIT                                                  
026400     IF SEC-KDSVAR = ' ' OR                                               
026500        SEC-KDSVAR = '1' OR                                               
026600        SEC-KDSVAR = '2' OR                                               
026700        SEC-KDSVAR = '3' OR                                               
026800        SEC-KDSVAR = '5' OR                                               
026900        SEC-KDSVAR = '6'                                                  
027000                                                                          
027100        PERFORM AB-SPARA-INPUT                                            
027200                                                                          
027300        MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                            
027400                                MOD-IDKUNDNR-IN                           
027500                                MOD-IDARTNR-IN                            
027600                                MOD-KDORDKL-IN                            
027700                                MOD-KDPRODSL-IN                           
027800                                MOD-IDORDNR-IN                            
027900                                MOD-KDTPOTYP-IN                           
027910                                MOD-IDDC-IN                               
028000                                MOD-TEMFSFEL                              
028100                                MOD-TEMFSINF                              
028200        MOVE +1 TO INDX                                                   
028300        PERFORM UNTIL INDX > MAX-LINE                                     
028400           MOVE MFS-RENSA-FAELT TO MOD-RAD(INDX)                          
028500           ADD +1 TO INDX                                                 
028600        END-PERFORM                                                       
028700                                                                          
028800        IF MID-SPARADE-NYCKLAR        = ALL '0' AND                       
028900           MID-SPARADE-NYCKLAR-ENT = ALL '0'                              
029000          MOVE SPACE TO MFS-IDPFK                                         
029100        END-IF                                                            
029200                                                                          
029300        IF W-IDDISTR             = 0 AND                                  
029400           MID-IDDISTR-SPAR      = 0 AND                                  
029500           MID-IDDISTR-SPAR-E = 0                                         
029600            MOVE NEJ            TO INPUT-OK                               
029700        END-IF                                                            
029800     END-IF                                                               
031500     .                                                                    
031600     EJECT                                                                
031700 AB-SPARA-INPUT SECTION.                                                  
031800                                                                          
031900     MOVE JA TO INPUT-OK                                                  
031901                                                                          
032000                                                                          
032100     IF MID-IDKUNDNR-IN       NOT = ALL '+'                               
032600         MOVE SPACE TO MFS-IDPFK                                          
032700     END-IF                                                               
032800                                                                          
032900     IF MID-IDARTNR-IN = ALL '+'                                          
033000         MOVE MID-IDARTNR-UT TO IDARTNR-WS                                
033100         INSPECT IDARTNR-WS REPLACING LEADING SPACE BY ZERO               
033200     ELSE                                                                 
033300         MOVE MID-IDARTNR-IN TO IDARTNR-WS                                
033400         MOVE SPACE TO MFS-IDPFK                                          
033500     END-IF                                                               
033600                                                                          
033700     IF MID-KDORDKL-IN = ALL '+'                                          
033800         MOVE MID-KDORDKL-UT TO KDORDKL-WS                                
033900         INSPECT KDORDKL-WS REPLACING LEADING SPACE BY ZERO               
034000     ELSE                                                                 
034100         MOVE MID-KDORDKL-IN TO KDORDKL-WS                                
034200         MOVE SPACE TO MFS-IDPFK                                          
034300     END-IF                                                               
034400                                                                          
034500     IF MID-KDPRODSL-IN = ALL '+'                                         
034600         MOVE MID-KDPRODSL-UT TO KDPRODSL-WS                              
034700         INSPECT KDPRODSL-WS REPLACING LEADING SPACE BY ZERO              
034800     ELSE                                                                 
034900         MOVE MID-KDPRODSL-IN TO KDPRODSL-WS                              
035000         MOVE SPACE TO MFS-IDPFK                                          
035100     END-IF                                                               
035200                                                                          
035300     IF MID-IDORDNR-IN = ALL '+'                                          
035400         MOVE MID-IDORDNR-UT TO IDORDNR-WS                                
035500         INSPECT IDORDNR-WS REPLACING LEADING SPACE BY ZERO               
035600     ELSE                                                                 
035700         MOVE MID-IDORDNR-IN TO IDORDNR-WS                                
035800         MOVE SPACE TO MFS-IDPFK                                          
035900     END-IF                                                               
036000                                                                          
036100     IF MID-KDTPOTYP-IN = ALL '+'                                         
036200         MOVE MID-KDTPOTYP-UT TO KDTPOTYP-WS                              
036300         INSPECT KDTPOTYP-WS REPLACING LEADING SPACE BY ZERO              
036400     ELSE                                                                 
036500         MOVE MID-KDTPOTYP-IN TO KDTPOTYP-WS                              
036600         MOVE SPACE TO MFS-IDPFK                                          
036700     END-IF                                                               
036800                                                                          
036801     IF MID-IDDC-IN = ALL '+'                                             
036803         MOVE MID-IDDC-UT TO W-IDDC-B6                                    
036808     ELSE                                                                 
036812         MOVE MID-IDDC-IN TO W-IDDC-B6                                    
036814         MOVE SPACE TO MFS-IDPFK                                          
036815     END-IF                                                               
036816     PERFORM IMS-GU-WDB601                                                
036835                                                                          
036836     IF WS-GODKAEND-BILD                                                  
036837        CONTINUE                                                          
036838     ELSE                                                                 
036840        MOVE ZERO        TO IDARTNR-WS                                    
036850                            KDORDKL-WS                                    
036860                            KDPRODSL-WS                                   
036870                            IDORDNR-WS                                    
036880                            KDTPOTYP-WS                                   
036881                            DCS-IDDC                                      
036890     END-IF                                                               
036891                                                                          
036900     MOVE MSGI-IDDISTR     TO MOD-IDDISTR-UT                              
037000     INSPECT MOD-IDDISTR-UT  REPLACING LEADING ZERO BY SPACE              
037100                                                                          
037200     MOVE MSGI-IDKUNDNR     TO MOD-IDKUNDNR-UT                            
037300     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
037400                                                                          
037500     MOVE IDARTNR-WS   TO MOD-IDARTNR-UT                                  
037600     INSPECT MOD-IDARTNR-UT  REPLACING LEADING ZERO BY SPACE              
037700                                                                          
037800     MOVE KDORDKL-WS   TO MOD-KDORDKL-UT                                  
037900     INSPECT MOD-KDORDKL-UT  REPLACING LEADING ZERO BY SPACE              
038000                                                                          
038100     MOVE KDPRODSL-WS  TO MOD-KDPRODSL-UT                                 
038200     INSPECT MOD-KDPRODSL-UT REPLACING LEADING ZERO BY SPACE              
038300                                                                          
038400     MOVE IDORDNR-WS   TO MOD-IDORDNR-UT                                  
038500     INSPECT MOD-IDORDNR-UT  REPLACING LEADING ZERO BY SPACE              
038600                                                                          
038700     MOVE KDTPOTYP-WS  TO MOD-KDTPOTYP-UT                                 
038800     INSPECT MOD-KDTPOTYP-UT REPLACING LEADING ZERO BY SPACE              
038900                                                                          
038910     MOVE DCS-IDDC     TO MOD-IDDC-UT                                     
038912     IF DCS-KDDC = SPACE OR DCS-CDC-TR                                    
038920       MOVE MSGI-IDDC TO MOD-IDDC-UT                                      
038926                         W-IDDC-B6                                        
038927       PERFORM IMS-GU-WDB601                                              
038928     END-IF                                                               
038929     INSPECT MOD-IDDC-UT REPLACING LEADING ZERO BY SPACE                  
038930                                                                          
038940                                                                          
039000     IF MSGI-IDDISTR    NOT NUMERIC  OR                                   
039100        MSGI-IDKUNDNR   NOT NUMERIC  OR                                   
039200        IDARTNR-WS      NOT NUMERIC  OR                                   
039300        KDORDKL-WS      NOT NUMERIC  OR                                   
039400        KDPRODSL-WS     NOT NUMERIC  OR                                   
039500        IDORDNR-WS      NOT NUMERIC  OR                                   
039600        KDTPOTYP-WS     NOT NUMERIC  OR                                   
039610       (DCS-KDDC = SPACE OR DCS-DDC)                                      
039700       MOVE NEJ TO INPUT-OK                                               
039800     END-IF                                                               
039900                                                                          
040000     IF INPUT-OK = JA                                                     
040100        MOVE MSGI-IDDISTR  TO W-IDDISTR                                   
040200        MOVE MSGI-IDKUNDNR TO W-IDKUNDNR                                  
040300        MOVE IDARTNR-WS    TO W-IDARTNR                                   
040400        MOVE KDORDKL-WS    TO W-KDORDKL                                   
040500        MOVE KDPRODSL-WS   TO W-KDPRODSL                                  
040600        MOVE IDORDNR-WS    TO W-IDORDNR                                   
040700        MOVE KDTPOTYP-WS   TO W-KDTPOTYP                                  
040710        MOVE DCS-IDDC      TO W-IDDC                                      
040800                                                                          
040900        IF W-IDDISTR NOT > 0  OR                                          
041000           W-KDORDKL     > 5  OR                                          
041100           W-KDTPOTYP    > 7                                              
041200          MOVE NEJ TO INPUT-OK                                            
041300        END-IF                                                            
041400     END-IF                                                               
041500     .                                                                    
041600     EJECT                                                                
041700 C-FLYTTA-SPARAD-KEY         SECTION.                                     
041800                                                                          
041900     MOVE LOW-VALUE  TO W-WDA5D1KY-MIN                                    
042000                        W-SOK-KEY-MIN                                     
042100     MOVE HIGH-VALUE TO W-WDA5D1KY-MAX                                    
042200                        W-SOK-KEY-MAX                                     
042300     MOVE '2'        TO W-KDSTARAD-MAX                                    
042400                                                                          
042500     IF MID-IDDISTR-SPAR  > ZERO                                          
042600       MOVE MID-IDDISTR-SPAR       TO W-IDDISTR-N1-MIN                    
042700                                      W-IDDISTR-N1-MAX                    
042800     ELSE                                                                 
042900       MOVE W-IDDISTR              TO W-IDDISTR-N1-MIN                    
043000                                      W-IDDISTR-N1-MAX                    
043100     END-IF                                                               
043200                                                                          
043300     IF MID-IDKUNDNR-SPAR  > ZERO                                         
043400        MOVE MID-IDKUNDNR-SPAR  TO W-IDKUNDNR-N1-MIN                      
043500        IF W-IDKUNDNR      > ZERO                                         
043600           MOVE MID-IDKUNDNR-SPAR  TO W-IDKUNDNR-N1-MAX                   
043700        END-IF                                                            
043800     END-IF                                                               
043900                                                                          
044000     IF MID-IDARTNR-SPAR  > ZERO                                          
044100       MOVE MID-IDARTNR-SPAR    TO W-IDARTNR-MIN                          
044200                                   W-IDARTNR-N1-MIN                       
044300       IF W-IDARTNR       > ZERO                                          
044400          MOVE MID-IDARTNR-SPAR    TO W-IDARTNR-MAX                       
044500                                      W-IDARTNR-N1-MAX                    
044600       END-IF                                                             
044700     END-IF                                                               
044800                                                                          
044900     IF MID-IDORDNR-SPAR  > ZERO                                          
045000       MOVE MID-IDORDNR-SPAR    TO W-IDKUNDRF-1-5                         
045100       MOVE W-IDKUNDRF          TO W-IDKUNDRF-MIN                         
045200                                   W-IDKUNDRF-N1-MIN                      
045300       IF W-IDORDNR       > ZERO                                          
045400          MOVE W-IDKUNDRF          TO W-IDKUNDRF-MAX                      
045500                                      W-IDKUNDRF-N1-MAX                   
045600       END-IF                                                             
045700     END-IF                                                               
045800                                                                          
045900     IF MID-KDORDKL-SPAR   > ZERO                                         
046000       MOVE MID-KDORDKL-SPAR  TO W-KDORDKL-MIN                            
046100                                 W-KDORDKL-MAX                            
046200     END-IF                                                               
046300                                                                          
046400     IF MID-KDPRODSL-SPAR  > ZERO                                         
046500       MOVE MID-KDPRODSL-SPAR TO W-KDPRODSL-MIN                           
046600                                 W-KDPRODSL-MAX                           
046700     END-IF                                                               
046800                                                                          
046900     IF MID-KDTPOTYP-SPAR  > ZERO                                         
047000       MOVE MID-KDTPOTYP-SPAR TO W-KDTPOTYP-MIN                           
047100                                 W-KDTPOTYP-MAX                           
047200     END-IF                                                               
047300                                                                          
047400     IF MID-IDDC-SPAR  > ZERO                                             
047500       MOVE MID-IDDC-SPAR TO W-IDDC-MIN                                   
047600                             W-IDDC-MAX                                   
047700     END-IF                                                               
047800                                                                          
047900     IF MID-IDLOPNR-SPAR   > ZERO                                         
048000       MOVE MID-IDLOPNR-SPAR  TO W-IDLOPNR-N1-MIN                         
048100     END-IF                                                               
048200     .                                                                    
048300     EJECT                                                                
048400 D-FLYTTA-GAMMAL-KEY        SECTION.                                      
048500                                                                          
048600     MOVE LOW-VALUE           TO W-SOK-KEY-MIN                            
048700                                 W-WDA5D1KY-MIN                           
048800     MOVE HIGH-VALUE          TO W-SOK-KEY-MAX                            
048900                                 W-WDA5D1KY-MAX                           
049000     MOVE '2'                 TO W-KDSTARAD-MAX                           
049100                                                                          
049200     MOVE W-IDDISTR          TO W-IDDISTR-N1-MIN                          
049300                                W-IDDISTR-N1-MAX                          
049400                                                                          
049500     IF W-IDKUNDNR       > ZERO                                           
049600        MOVE W-IDKUNDNR       TO W-IDKUNDNR-N1-MIN                        
049700                                 W-IDKUNDNR-N1-MAX                        
049800     END-IF                                                               
049900                                                                          
050000     IF W-IDARTNR       > ZERO                                            
050100       MOVE W-IDARTNR            TO W-IDARTNR-MIN                         
050200                                    W-IDARTNR-MAX                         
050300        IF W-IDKUNDNR     > ZERO                                          
050400           MOVE W-IDARTNR        TO W-IDARTNR-N1-MIN                      
050500                                    W-IDARTNR-N1-MAX                      
050600        END-IF                                                            
050700     END-IF                                                               
050800                                                                          
050900     IF W-IDORDNR       > ZERO                                            
051000       MOVE W-IDORDNR             TO W-IDKUNDRF-1-5                       
051100       MOVE W-IDKUNDRF            TO W-IDKUNDRF-MIN                       
051200                                     W-IDKUNDRF-MAX                       
051300        IF W-IDKUNDNR     > ZERO AND                                      
051400           W-IDARTNR      > ZERO                                          
051500           MOVE W-IDKUNDRF         TO W-IDKUNDRF-N1-MIN                   
051600                                      W-IDKUNDRF-N1-MAX                   
051700        END-IF                                                            
051800     END-IF                                                               
051900     IF W-KDORDKL      > ZERO                                             
052000       MOVE W-KDORDKL       TO W-KDORDKL-MIN                              
052100                               W-KDORDKL-MAX                              
052200     END-IF                                                               
052300     IF W-KDPRODSL     > ZERO                                             
052400       MOVE W-KDPRODSL      TO W-KDPRODSL-MIN                             
052500                               W-KDPRODSL-MAX                             
052600     END-IF                                                               
052700     IF W-KDTPOTYP     > ZERO                                             
052800       MOVE W-KDTPOTYP      TO W-KDTPOTYP-MIN                             
052900                               W-KDTPOTYP-MAX                             
053000     END-IF                                                               
053010     IF W-IDDC         > ZERO                                             
053020       MOVE W-IDDC          TO W-IDDC-MIN                                 
053030                               W-IDDC-MAX                                 
053040     END-IF                                                               
053100     .                                                                    
053200     EJECT                                                                
053300 E1-FLYTTA-KEY     SECTION.                                               
053400                                                                          
053500     MOVE LOW-VALUE  TO W-WDA5D1KY-MIN                                    
053600                        W-SOK-KEY-MIN                                     
053700     MOVE HIGH-VALUE TO W-WDA5D1KY-MAX                                    
053800                        W-SOK-KEY-MAX                                     
053900     MOVE '2'        TO W-KDSTARAD-MAX                                    
054000     MOVE MID-IDDISTR-SPAR-E TO W-IDDISTR-N1-MIN                          
054100                                W-IDDISTR-N1-MAX                          
054200     IF W-IDKUNDNR         > ZERO                                         
054300        MOVE W-IDKUNDNR TO W-IDKUNDNR-N1-MAX                              
054400        IF W-IDARTNR        > ZERO                                        
054500           MOVE W-IDARTNR TO W-IDARTNR-N1-MAX                             
054600           IF W-IDORDNR        > ZERO                                     
054700                MOVE W-IDKUNDRF     TO W-IDKUNDRF-N1-MAX                  
054800           END-IF                                                         
054900        END-IF                                                            
055000     END-IF                                                               
055100                                                                          
055200     IF W-IDARTNR        > ZERO                                           
055300        MOVE W-IDARTNR   TO W-IDARTNR-MAX                                 
055400     END-IF                                                               
055500     IF W-IDORDNR        > ZERO                                           
055600        MOVE W-IDKUNDRF  TO W-IDKUNDRF-MAX                                
055700     END-IF                                                               
055800                                                                          
055900     IF MID-IDKUNDNR-SPAR-E > ZERO                                        
056000        MOVE MID-IDKUNDNR-SPAR-E TO W-IDKUNDNR-N1-MIN                     
056100        IF MID-IDARTNR-SPAR-E > ZERO                                      
056200           MOVE MID-IDARTNR-SPAR-E TO W-IDARTNR-N1-MIN                    
056300           IF MID-IDORDNR-SPAR-E > ZERO                                   
056400                MOVE MID-IDORDNR-SPAR-E TO W-IDKUNDRF-N1-MIN              
056500           END-IF                                                         
056600        END-IF                                                            
056700     END-IF                                                               
056800                                                                          
056900     IF MID-IDARTNR-SPAR-E > ZERO                                         
057000       MOVE MID-IDARTNR-SPAR-E   TO W-IDARTNR-MIN                         
057100         IF MID-IDARTNR-UT > ZERO                                         
057200           MOVE MID-IDARTNR-SPAR-E   TO W-IDARTNR-MAX                     
057300         END-IF                                                           
057400     END-IF                                                               
057500     IF MID-IDORDNR-SPAR-E > ZERO                                         
057600       MOVE MID-IDORDNR-SPAR-E    TO W-IDKUNDRF-MIN                       
057700         IF MID-IDORDNR-UT > ZERO                                         
057800           MOVE MID-IDORDNR-SPAR-E    TO W-IDKUNDRF-MAX                   
057900         END-IF                                                           
058000     END-IF                                                               
058100     IF MID-KDORDKL-SPAR-E  > ZERO                                        
058200        MOVE MID-KDORDKL-SPAR-E  TO W-KDORDKL-MIN                         
058300         IF MID-KDORDKL-UT > ZERO                                         
058400            MOVE MID-KDORDKL-SPAR-E  TO W-KDORDKL-MAX                     
058500         END-IF                                                           
058600     END-IF                                                               
058700     IF MID-KDPRODSL-SPAR-E > ZERO                                        
058800        MOVE MID-KDPRODSL-SPAR-E TO W-KDPRODSL-MIN                        
058900         IF MID-KDPRODSL-UT > ZERO                                        
059000            MOVE MID-KDPRODSL-SPAR-E TO W-KDPRODSL-MAX                    
059100         END-IF                                                           
059200     END-IF                                                               
059300     IF MID-KDTPOTYP-SPAR-E > ZERO                                        
059400        MOVE MID-KDTPOTYP-SPAR-E TO W-KDTPOTYP-MIN                        
059500         IF MID-KDTPOTYP-UT > ZERO                                        
059600            MOVE MID-KDTPOTYP-SPAR-E TO W-KDTPOTYP-MAX                    
059700         END-IF                                                           
059800     END-IF                                                               
059810     IF MID-IDDC-SPAR-E > ZERO                                            
059820        MOVE MID-IDDC-SPAR-E TO W-IDDC-MIN                                
059830         IF MID-IDDC-UT > ZERO                                            
059840            MOVE MID-IDDC-SPAR-E TO W-IDDC-MAX                            
059850         END-IF                                                           
059860     END-IF                                                               
059900     .                                                                    
060000     EJECT                                                                
060100 E2-FLYTTA-KEY SECTION.                                                   
060200                                                                          
060300     MOVE LOW-VALUE  TO W-WDA5D1KY-MIN                                    
060400                        W-SOK-KEY-MIN                                     
060500     MOVE HIGH-VALUE TO W-WDA5D1KY-MAX                                    
060600                        W-SOK-KEY-MAX                                     
060700     MOVE '2'        TO W-KDSTARAD-MAX                                    
060800                                                                          
060900****** KOMMANDE IF-SATS KOLLAR OM DET ÄR FÖRSTA GÅNGEN MAN                
061000****** TRYCKER ENTER, ISÅFALL LÄSER MAN FRÅN INMATNINGSRADEN              
061100****** OM INTE SÅ LÄSER MAN FRÅN SPARADE NYCKLAR RAD 20                   
061200                                                                          
061300     IF MID-IDDISTR-IN  NOT = ALL '+' OR                                  
061400        MID-IDKUNDNR-IN NOT = ALL '+' OR                                  
061500        MID-IDARTNR-IN  NOT = ALL '+' OR                                  
061600        MID-KDORDKL-IN  NOT = ALL '+' OR                                  
061700        MID-KDPRODSL-IN NOT = ALL '+' OR                                  
061800        MID-IDORDNR-IN  NOT = ALL '+' OR                                  
061900        MID-KDTPOTYP-IN NOT = ALL '+' OR                                  
061910        MID-IDDC-IN     NOT = ALL '+' OR                                  
062000        MID-IDDISTR-SPAR-E  = ALL '0'                                     
062100        MOVE JA              TO W-NEWKEY                                  
062200        MOVE W-IDDISTR       TO W-IDDISTR-N1-MIN                          
062300                                W-IDDISTR-N1-MAX                          
062400       IF MID-IDDISTR-IN NOT = ALL '+'                                    
062500         IF W-IDKUNDNR         > ZERO                                     
062600           MOVE W-IDKUNDNR   TO W-IDKUNDNR-N1-MIN                         
062700                                W-IDKUNDNR-N1-MAX                         
062800         END-IF                                                           
062900         IF W-IDARTNR          > ZERO                                     
063000            MOVE W-IDARTNR    TO W-IDARTNR-MIN                            
063100                                 W-IDARTNR-MAX                            
063200                                 W-IDARTNR-N1-MIN                         
063300                                 W-IDARTNR-N1-MAX                         
063400         END-IF                                                           
063500         IF W-IDORDNR          > ZERO                                     
063600            MOVE W-IDORDNR    TO W-IDKUNDRF-1-5                           
063700            MOVE W-IDKUNDRF   TO W-IDKUNDRF-MIN                           
063800                                 W-IDKUNDRF-MAX                           
063900                                 W-IDKUNDRF-N1-MIN                        
064000                                 W-IDKUNDRF-N1-MAX                        
064100         END-IF                                                           
064200         IF W-IDARTNR          > ZERO                                     
064300            MOVE W-IDARTNR    TO W-IDARTNR-MIN                            
064400                                 W-IDARTNR-MAX                            
064500                                 W-IDARTNR-N1-MIN                         
064600                                 W-IDARTNR-N1-MAX                         
064700         END-IF                                                           
064800         IF W-KDORDKL          > ZERO                                     
064900            MOVE W-KDORDKL    TO W-KDORDKL-MIN                            
065000                                 W-KDORDKL-MAX                            
065100         END-IF                                                           
065200         IF W-KDPRODSL         > ZERO                                     
065300            MOVE W-KDPRODSL   TO W-KDPRODSL-MIN                           
065400                                 W-KDPRODSL-MAX                           
065500         END-IF                                                           
065600         IF W-KDTPOTYP         > ZERO                                     
065700            MOVE W-KDTPOTYP   TO W-KDTPOTYP-MIN                           
065800                                 W-KDTPOTYP-MAX                           
065900         END-IF                                                           
065910         IF W-IDDC             > ZERO                                     
065920            MOVE W-IDDC       TO W-IDDC-MIN                               
065930                                 W-IDDC-MAX                               
065940         END-IF                                                           
066000       ELSE                                                               
066100        IF W-IDKUNDNR         > ZERO                                      
066200           MOVE W-IDKUNDNR   TO W-IDKUNDNR-N1-MIN                         
066300                                W-IDKUNDNR-N1-MAX                         
066400        END-IF                                                            
066500        IF W-IDARTNR          > ZERO                                      
066600           MOVE W-IDARTNR    TO W-IDARTNR-MIN                             
066700                                W-IDARTNR-MAX                             
066800                                W-IDARTNR-N1-MIN                          
066900                                W-IDARTNR-N1-MAX                          
067000        END-IF                                                            
067100        IF W-IDORDNR          > ZERO                                      
067200           MOVE W-IDORDNR    TO W-IDKUNDRF-1-5                            
067300           MOVE W-IDKUNDRF   TO W-IDKUNDRF-MIN                            
067400                                W-IDKUNDRF-MAX                            
067500                                W-IDKUNDRF-N1-MIN                         
067600                                W-IDKUNDRF-N1-MAX                         
067700        END-IF                                                            
067800        IF W-KDORDKL          > ZERO                                      
067900           MOVE W-KDORDKL    TO W-KDORDKL-MIN                             
068000                                W-KDORDKL-MAX                             
068100        END-IF                                                            
068200        IF W-KDPRODSL         > ZERO                                      
068300           MOVE W-KDPRODSL   TO W-KDPRODSL-MIN                            
068400                                W-KDPRODSL-MAX                            
068500        END-IF                                                            
068600        IF W-KDTPOTYP         > ZERO                                      
068700           MOVE W-KDTPOTYP   TO W-KDTPOTYP-MIN                            
068800                                W-KDTPOTYP-MAX                            
068900        END-IF                                                            
068910        IF W-IDDC             > ZERO                                      
068920           MOVE W-IDDC       TO W-IDDC-MIN                                
068930                                W-IDDC-MAX                                
068940        END-IF                                                            
069000       END-IF                                                             
069100     EJECT                                                                
069200*    VID ENTER UTAN NYA NYKLAR                                            
069300     ELSE                                                                 
069400                                                                          
069500         MOVE NEJ                TO W-NEWKEY                              
069600         MOVE MID-IDDISTR-SPAR-E TO W-IDDISTR-N1-MIN                      
069700                                    W-IDDISTR-N1-MAX                      
069800         IF W-IDKUNDNR         > ZERO                                     
069900            MOVE W-IDKUNDNR TO W-IDKUNDNR-N1-MAX                          
070000            IF W-IDARTNR        > ZERO                                    
070100               MOVE W-IDARTNR TO W-IDARTNR-N1-MAX                         
070200               IF W-IDORDNR        > ZERO                                 
070300                    MOVE W-IDKUNDRF     TO W-IDKUNDRF-N1-MAX              
070400               END-IF                                                     
070500            END-IF                                                        
070600         END-IF                                                           
070700                                                                          
070800         IF W-IDARTNR        > ZERO                                       
070900            MOVE W-IDARTNR   TO W-IDARTNR-MAX                             
071000         END-IF                                                           
071100         IF W-IDORDNR        > ZERO                                       
071200            MOVE W-IDKUNDRF  TO W-IDKUNDRF-MAX                            
071300         END-IF                                                           
071400                                                                          
071500         IF MID-IDKUNDNR-SPAR-E > ZERO                                    
071600            MOVE MID-IDKUNDNR-SPAR-E TO W-IDKUNDNR-N1-MIN                 
071700            IF MID-IDARTNR-SPAR-E > ZERO                                  
071800               MOVE MID-IDARTNR-SPAR-E TO W-IDARTNR-N1-MIN                
071900               IF MID-IDORDNR-SPAR-E > ZERO                               
072000                    MOVE MID-IDORDNR-SPAR-E TO W-IDKUNDRF-N1-MIN          
072100               END-IF                                                     
072200            END-IF                                                        
072300         END-IF                                                           
072400                                                                          
072500         IF MID-IDARTNR-SPAR-E > ZERO                                     
072600           MOVE MID-IDARTNR-SPAR-E   TO W-IDARTNR-MIN                     
072700             IF MID-IDARTNR-UT > ZERO                                     
072800               MOVE MID-IDARTNR-SPAR-E   TO W-IDARTNR-MAX                 
072900             END-IF                                                       
073000         END-IF                                                           
073100         IF MID-IDORDNR-SPAR-E > ZERO                                     
073200           MOVE MID-IDORDNR-SPAR-E    TO W-IDKUNDRF-MIN                   
073300             IF MID-IDORDNR-UT > ZERO                                     
073400               MOVE MID-IDORDNR-SPAR-E    TO W-IDKUNDRF-MAX               
073500             END-IF                                                       
073600         END-IF                                                           
073700         IF MID-KDORDKL-SPAR-E  > ZERO                                    
073800            MOVE MID-KDORDKL-SPAR-E  TO W-KDORDKL-MIN                     
073900             IF MID-KDORDKL-UT > ZERO                                     
074000                MOVE MID-KDORDKL-SPAR-E  TO W-KDORDKL-MAX                 
074100             END-IF                                                       
074200         END-IF                                                           
074300         IF MID-KDPRODSL-SPAR-E > ZERO                                    
074400            MOVE MID-KDPRODSL-SPAR-E TO W-KDPRODSL-MIN                    
074500             IF MID-KDPRODSL-UT > ZERO                                    
074600                MOVE MID-KDPRODSL-SPAR-E TO W-KDPRODSL-MAX                
074700             END-IF                                                       
074800         END-IF                                                           
074900         IF MID-KDTPOTYP-SPAR-E > ZERO                                    
075000            MOVE MID-KDTPOTYP-SPAR-E TO W-KDTPOTYP-MIN                    
075100             IF MID-KDTPOTYP-UT > ZERO                                    
075200                MOVE MID-KDTPOTYP-SPAR-E TO W-KDTPOTYP-MAX                
075300             END-IF                                                       
075400         END-IF                                                           
075410         IF MID-IDDC-SPAR-E > ZERO                                        
075420            MOVE MID-IDDC-SPAR-E TO W-IDDC-MIN                            
075430             IF MID-IDDC-UT > ZERO                                        
075440                MOVE MID-IDDC-SPAR-E TO W-IDDC-MAX                        
075450             END-IF                                                       
075460         END-IF                                                           
075500     END-IF.                                                              
075600     EJECT                                                                
075700 F-LAES-FLYTTA-TILL-MOD SECTION.                                          
075800                                                                          
075900     MOVE +1 TO INDX                                                      
076000                                                                          
076100     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
076200        SEGMENT-SLUT              OR                                      
076300        INDX > MAX-LINE                                                   
076400                                                                          
076500        IF SEGMENT-FINNS                                                  
076600           PERFORM FB-FLYTTA-TILL-KEY                                     
076700           PERFORM IMS-GU-RO                                              
076800              PERFORM FC-FLYTTA-TILL-MOD                                  
076900              MOVE JA TO MINST-1-RAD-SKRIVEN                              
077000              ADD +1 TO INDX                                              
077100        END-IF                                                            
077200        PERFORM IMS-GN-INDEX-RO                                           
077300     END-PERFORM                                                          
077400                                                                          
077500     IF SEGMENT-FINNS                                                     
077600        PERFORM FD-FLYTTA-TILL-SPAR-MOD                                   
077700     END-IF                                                               
077800     .                                                                    
077900     EJECT                                                                
078000 FB-FLYTTA-TILL-KEY     SECTION.                                          
078100     MOVE SEQD-IDDISTR    TO  W-IDDISTR-N2                                
078200     MOVE SEQD-IDKUNDNR   TO  W-IDKUNDNR-N2                               
078300     MOVE SEQD-IDKUNDRF   TO  W-IDKUNDRF-N2                               
078400     MOVE SEQD-IDARTNR    TO  W-IDARTNR-N2                                
078500     MOVE SEQD-IDLOPNR    TO  W-IDLOPNR-N2                                
078600     .                                                                    
078700     EJECT                                                                
078800 FC-FLYTTA-TILL-MOD SECTION.                                              
078900                                                                          
079000     MOVE RAD-IDKUNDNR   TO MOD-IDKUNDNR(INDX)                            
079100     MOVE RAD-IDARTNR    TO MOD-IDARTNR(INDX)                             
079200     MOVE RAD-KVART      TO MOD-KVART(INDX)                               
079300     MOVE RAD-KDORDKL    TO MOD-KDORDKL(INDX)                             
079400     MOVE RAD-KDPRODSL   TO MOD-KDPRODSL(INDX)                            
079500     MOVE RAD-DARODAT (3:6)  TO MOD-TIRODAT(INDX)                         
079510     IF RAD-DARODAT = 0                                                   
079520       INSPECT MOD-TIRODAT(INDX) REPLACING LEADING ZERO BY SPACE          
079530     END-IF                                                               
079600     MOVE RAD-TITPO      TO MOD-TITPO(INDX)                               
079610     IF RAD-TITPO = 0                                                     
079620       INSPECT MOD-TITPO (INDX) REPLACING LEADING ZERO BY SPACE           
079630     END-IF                                                               
079700     MOVE RAD-IDANSK     TO MOD-IDANSK(INDX)                              
079710     MOVE RAD-IDDC       TO MOD-IDDC(INDX)                                
079800                                                                          
079900     MOVE RAD-IDKUNDRF   TO W-IDKUNDRF                                    
080000     MOVE W-IDKUNDRF-1-5 TO MOD-IDORDNR(INDX)                             
080100                                                                          
080200     IF INDX = 1                                                          
080300       MOVE RAD-IDDISTR    TO MOD-IDDISTR-SPAR-E                          
080400       MOVE RAD-IDKUNDNR   TO MOD-IDKUNDNR-SPAR-E                         
080500       MOVE RAD-IDARTNR    TO MOD-IDARTNR-SPAR-E                          
080600       MOVE RAD-IDKUNDRF   TO W-IDKUNDRF                                  
080700       MOVE W-IDKUNDRF-1-5 TO MOD-IDORDNR-SPAR-E                          
080800       IF W-KDORDKL        > ZERO                                         
080900          MOVE W-KDORDKL   TO MOD-KDORDKL-SPAR-E                          
081000       ELSE                                                               
081100          MOVE ZERO        TO MOD-KDORDKL-SPAR-E                          
081200       END-IF                                                             
081300       IF W-KDPRODSL       > ZERO                                         
081400          MOVE W-KDPRODSL  TO MOD-KDPRODSL-SPAR-E                         
081500       ELSE                                                               
081600          MOVE ZERO        TO MOD-KDPRODSL-SPAR-E                         
081700       END-IF                                                             
081710       IF W-IDDC           > ZERO                                         
081720          MOVE W-IDDC      TO MOD-IDDC-SPAR-E                             
081730       ELSE                                                               
081740          MOVE ZERO        TO MOD-IDDC-SPAR-E                             
081750       END-IF                                                             
081800       IF W-KDTPOTYP       > ZERO                                         
081900          MOVE W-KDTPOTYP  TO MOD-KDTPOTYP-SPAR-E                         
082000       ELSE                                                               
082100          MOVE ZERO        TO MOD-KDTPOTYP-SPAR-E                         
082200       END-IF                                                             
082300     END-IF                                                               
082400     .                                                                    
082500     EJECT                                                                
082600 FD-FLYTTA-TILL-SPAR-MOD SECTION.                                         
082700     MOVE SEQD-IDDISTR  TO MOD-IDDISTR-SPAR                               
082800     MOVE SEQD-IDKUNDNR TO MOD-IDKUNDNR-SPAR                              
082900     MOVE SEQD-IDARTNR  TO MOD-IDARTNR-SPAR                               
083000     MOVE SEQD-IDLOPNR  TO MOD-IDLOPNR-SPAR                               
083100     MOVE SEQD-IDKUNDRF   TO W-IDKUNDRF                                   
083200     MOVE W-IDKUNDRF-1-5 TO MOD-IDORDNR-SPAR                              
083300     IF W-KDORDKL        > ZERO                                           
083400        MOVE W-KDORDKL   TO MOD-KDORDKL-SPAR                              
083500     ELSE                                                                 
083600        MOVE ZERO        TO MOD-KDORDKL-SPAR                              
083700     END-IF                                                               
083800     IF W-KDPRODSL       > ZERO                                           
083900        MOVE W-KDPRODSL  TO MOD-KDPRODSL-SPAR                             
084000     ELSE                                                                 
084100        MOVE ZERO        TO MOD-KDPRODSL-SPAR                             
084200     END-IF                                                               
084300     IF W-KDTPOTYP       > ZERO                                           
084400        MOVE W-KDTPOTYP  TO MOD-KDTPOTYP-SPAR                             
084500     ELSE                                                                 
084600        MOVE ZERO        TO MOD-KDTPOTYP-SPAR                             
084700     END-IF                                                               
084710     IF W-IDDC           > ZERO                                           
084720        MOVE W-IDDC      TO MOD-IDDC-SPAR                                 
084730     ELSE                                                                 
084740        MOVE ZERO        TO MOD-IDDC-SPAR                                 
084750     END-IF                                                               
084800     MOVE TEXT-0402(SPRAK-IX) TO MOD-TEMFSINF                             
084900     .                                                                    
085000     EJECT                                                                
085100 M-LAES            SECTION.                                               
085200                                                                          
085300     PERFORM IMS-GN-INDEX-RO                                              
085400     IF SEGMENT-FINNS                                                     
085500         PERFORM F-LAES-FLYTTA-TILL-MOD                                   
085600     ELSE                                                                 
085700         MOVE TEXT-0403(SPRAK-IX) TO MOD-TEMFSFEL                         
085800     END-IF                                                               
085900     .                                                                    
086000     EJECT                                                                
086100 N-LAES            SECTION.                                               
086200                                                                          
086300     PERFORM IMS-GN-INDEX-RO                                              
086400     IF SEGMENT-FINNS                                                     
086500         PERFORM FB-FLYTTA-TILL-KEY                                       
086600         PERFORM IMS-GU-RO                                                
086700         MOVE +1 TO INDX                                                  
086800                                                                          
086900         PERFORM UNTIL SEGMENT-SAKNAS OR                                  
087000            SEGMENT-SLUT OR                                               
087100            INDX > MAX-LINE                                               
087200            PERFORM FC-FLYTTA-TILL-MOD                                    
087300            ADD +1 TO INDX                                                
087400            IF INDX = +2                                                  
087500               PERFORM D-FLYTTA-GAMMAL-KEY                                
087600            END-IF                                                        
087700            PERFORM IMS-GN-INDEX-RO                                       
087800            IF SEGMENT-FINNS                                              
087900               PERFORM FB-FLYTTA-TILL-KEY                                 
088000               PERFORM IMS-GU-RO                                          
088100            END-IF                                                        
088200         END-PERFORM                                                      
088300                                                                          
088400         IF SEGMENT-FINNS                                                 
088500            PERFORM NA-FLYTTA-TILL-SPAR-MOD                               
088600         END-IF                                                           
088700                                                                          
088800         PERFORM UNTIL INDX > MAX-LINE                                    
088900            MOVE MFS-RENSA-FAELT TO                                       
089000                                    MOD-IDKUNDNR(INDX)                    
089100                                    MOD-IDARTNR(INDX)                     
089200                                    MOD-KVART(INDX)                       
089300                                    MOD-IDORDNR(INDX)                     
089400                                    MOD-KDORDKL(INDX)                     
089500                                    MOD-TITPO(INDX)                       
089510                                    MOD-IDDC(INDX)                        
089600            ADD +1 TO INDX                                                
089700         END-PERFORM                                                      
089800     ELSE                                                                 
089900         MOVE TEXT-0403(SPRAK-IX) TO MOD-TEMFSFEL                         
090000     END-IF                                                               
090100     .                                                                    
090200     EJECT                                                                
090300 NA-FLYTTA-TILL-SPAR-MOD SECTION.                                         
090400     MOVE RAD-IDDISTR  TO MOD-IDDISTR-SPAR                                
090500     MOVE RAD-IDKUNDNR TO MOD-IDKUNDNR-SPAR                               
090600     MOVE RAD-IDARTNR  TO MOD-IDARTNR-SPAR                                
090700     MOVE RAD-IDLOPNR  TO MOD-IDLOPNR-SPAR                                
090800     MOVE RAD-IDKUNDRF   TO W-IDKUNDRF                                    
090900     MOVE W-IDKUNDRF-1-5 TO MOD-IDORDNR-SPAR                              
091000     IF W-KDORDKL        > ZERO                                           
091100        MOVE W-KDORDKL   TO MOD-KDORDKL-SPAR                              
091200     ELSE                                                                 
091300        MOVE ZERO        TO MOD-KDORDKL-SPAR                              
091400     END-IF                                                               
091500     IF W-KDPRODSL       > ZERO                                           
091600        MOVE W-KDPRODSL  TO MOD-KDPRODSL-SPAR                             
091700     ELSE                                                                 
091800        MOVE ZERO        TO MOD-KDPRODSL-SPAR                             
091900     END-IF                                                               
092000     IF W-KDTPOTYP       > ZERO                                           
092100        MOVE W-KDTPOTYP  TO MOD-KDTPOTYP-SPAR                             
092200     ELSE                                                                 
092300        MOVE ZERO        TO MOD-KDTPOTYP-SPAR                             
092400     END-IF                                                               
092410     IF W-IDDC           > ZERO                                           
092420        MOVE W-IDDC      TO MOD-IDDC-SPAR                                 
092430     ELSE                                                                 
092440        MOVE ZERO        TO MOD-IDDC-SPAR                                 
092450     END-IF                                                               
092500     MOVE TEXT-0402(SPRAK-IX) TO MOD-TEMFSINF                             
092600     .                                                                    
092700     EJECT                                                                
092800 S30-SECURIT SECTION.                                                     
092900     MOVE MSG-SIGNON-USERID TO SEC-IDUSER                                 
093000     MOVE '4571'            TO SEC-IDTRANS                                
093100     MOVE MSGI-IDDISTR      TO SEC-IDKEY                                  
093200                                                                          
093300     CALL WSECURIT USING       SEC-IDUSER                                 
093400                               SEC-IDTRANS                                
093500                               SEC-IDKEY                                  
093600                               SEC-KDSVAR                                 
093700     .                                                                    
093800     EJECT                                                                
093900* IMS SEKTIONER                                                           
094000     SKIP3                                                                
094100 IMS-GET-MSG SECTION.                                                     
094200                                                                          
094300     MOVE '  QC' TO GODK-STATUSKODER                                      
094400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
094500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
094600     PERFORM IMS-STATUSKONTROLL                                           
094700     .                                                                    
094800     SKIP3                                                                
094900 IMS-INSERT-MSG SECTION.                                                  
095000                                                                          
095100     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
095200         MOVE '0' TO MFS-KDHUVOMR                                         
095300     END-IF                                                               
095400                                                                          
095500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
095600     MOVE SPACE TO GODK-STATUSKODER                                       
095700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
095800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
095900     PERFORM IMS-STATUSKONTROLL                                           
096000     .                                                                    
096100     EJECT                                                                
096200 IMS-GN-INDEX-RO SECTION.                                                 
096300                                                                          
096400     STRING 'WLORDT01(WDA5D1KY>=' W-WDA5D1KY-MIN                          
096500                    '&WDA5D1KY<=' W-WDA5D1KY-MAX                          
096600                    '&IDARTNR >=' W-IDARTNR-MIN-X                         
096700                    '&IDARTNR <=' W-IDARTNR-MAX-X                         
096800                    '&IDKUNDRF>=' W-IDKUNDRF-MIN                          
096900                    '&IDKUNDRF<=' W-IDKUNDRF-MAX                          
096910                    '&IDDC    >=' W-IDDC-MIN                              
096920                    '&IDDC    <=' W-IDDC-MAX                              
097000                    '&KDORDKL >=' W-KDORDKL-MIN-X                         
097100                    '&KDORDKL <=' W-KDORDKL-MAX-X                         
097200                    '&KDPRODSL>=' W-KDPRODSL-MIN-X                        
097300                    '&KDPRODSL<=' W-KDPRODSL-MAX-X                        
097400                    '&KDSTARAD<=' W-KDSTARAD-MAX                          
097500                    '&KDTPOTYP>=' W-KDTPOTYP-MIN-X                        
097600                    '&KDTPOTYP<=' W-KDTPOTYP-MAX-X ')'                    
097700            DELIMITED BY SIZE INTO SSA1                                   
097800     MOVE '  GBGE' TO GODK-STATUSKODER                                    
097900     CALL CBLTDLI USING GN ORDT-PCB DLI-IO-AREA SSA1                      
098000     MOVE ORDT-STATUS-CODE TO STATUS-WS                                   
098100     PERFORM IMS-STATUSKONTROLL                                           
098200     .                                                                    
098300     EJECT                                                                
098400 IMS-GU-RO    SECTION.                                                    
098500                                                                          
098600     STRING 'WLORDP01(WDA501KY =' W-WDA501KY ')'                          
098700            DELIMITED BY SIZE INTO SSA1                                   
098800     MOVE '  GE' TO GODK-STATUSKODER                                      
098900     CALL CBLTDLI USING GU ORDP-PCB DLI-IO-AREA SSA1                      
099000     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
099100     PERFORM IMS-STATUSKONTROLL                                           
099200     .                                                                    
099300     SKIP3                                                                
099310 IMS-GU-WDB601    SECTION.                                                
099320     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
099330          DELIMITED BY SIZE INTO SSA1                                     
099340     MOVE '  GE' TO GODK-STATUSKODER                                      
099350     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
099360     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
099370     PERFORM IMS-STATUSKONTROLL                                           
099380     IF SEGMENT-SAKNAS                                                    
099390         MOVE SPACE TO DCS-KDDC                                           
099391     END-IF                                                               
099392     .                                                                    
099400 IMS-STATUSKONTROLL SECTION.                                              
099500                                                                          
099600     SET STATUS-IX TO 1                                                   
099700     SEARCH GODK-STATUS AT END CALL FELLOG                                
099800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS NEXT SENTENCE             
099900     END-SEARCH                                                           
100000     .                                                                    
