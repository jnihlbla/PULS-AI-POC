000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2141200.                                                
000300 AUTHOR.         FRONTEC, GÖTEBORG.                                       
000400 DATE-WRITTEN.   960424.                                                  
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        LÄSER FILEN W21410 OCH UPPDATERAR WDK6 MED JA                    
001000*        OM BRIST I LAGRET HAR UPPSTÅTT ELLER MED NEJ OM                  
001100*        EN TIDIGARE LAGERBRIST ÅTERSTÄLLTS.                              
001200*                                                                         
001300*    ÄNDRING: 2001-11-19 (CONNY E)                                        
001400*        SKICKAR TRANS TILL 2191 VIA DISPATCHER (WMSGKOM)                 
001500*        NÄR DET FÖRELIGGER LARM FÖR UNDERSKRIDET SÄK.LAGER.              
001600*                                                                         
001700*                                                                         
001800*                                                                         
001900*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
002000*                                                                         
002100*        LÄGGER UPP LARM PÅ LARMBAS 2224 (WDR5) VIA 2191                  
002200*                                                                         
002300*    ABENDKODER:                                                          
002400*        U0016 -  . . . .                                                 
002500*        U1000 -  . . . .                                                 
002600*                                                                         
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     SKIP2                                                                
003100 INPUT-OUTPUT SECTION.                                                    
003200                                                                          
003300 FILE-CONTROL.                                                            
003400     SKIP2                                                                
003500*          --- UPPDATERINGSPOSTER FLLARM-BUF                              
003600     SELECT W21410                     ASSIGN TO W21412D1.                
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900     SKIP2                                                                
004000 FILE SECTION.                                                            
004100     SKIP3                                                                
004200 FD  W21410                                                               
004300     RECORDING       F                                                    
004400     BLOCK CONTAINS  0.                                                   
004500                                                                          
004600*01  -COPY W21410      -L.                                                
004700     EJECT                                                                
004800 WORKING-STORAGE SECTION.                                                 
004900                                                                          
005000                                                                          
005100*    -- CHECKED BY WY2000                                                 
005200 77  IDPGM                       PIC X(8)    VALUE 'W2141200'.            
005300 77  PROGRAM-NAMN                PIC X(6)    VALUE 'W21412'.              
005400 77  CURRENT-SECTION             PIC X(80)   VALUE SPACE.                 
005500 77  DBS-SECTION                 PIC X(80)   VALUE SPACE.                 
005600 77  JA                          PIC X       VALUE 'J'.                   
005700 77  NEJ                         PIC X       VALUE 'N'.                   
005800 77  WS-TIUPPDAT                 PIC S9(7)   COMP-3 VALUE ZERO.           
005900 77  WS-TIUPPTID                 PIC S9(9)   COMP-3 VALUE ZERO.           
006000 77  WS-IDANSK                   PIC 9(3)           VALUE ZERO.           
006100 77  MAX-CHKP                    PIC S9(5)   VALUE +99   COMP-3.          
006200 77  CHKP-ID                     PIC X(8)    VALUE 'W21412  '.            
006300     SKIP3                                                                
006400 77  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP  SYNC.        
006500 77  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
006600 77  CHKP-AREA-1-LENGTH          PIC S9(9)   VALUE +32 COMP  SYNC.        
006700 77  CHKP-AREA-1                 PIC X(32)   VALUE SPACE.                 
006800*                                                                         
006900*01  -COPY WWDCKONS                                                       
007000                                                                          
007100*                                                                         
007200*01  -COPY WWPRODSL                                                       
007300                                                                          
007400 77  W21410-EOF-SW               PIC X       VALUE 'N'.                   
007500     88  END-OF-W21410                       VALUE 'J'.                   
007600*                                                                         
007700 77  SW-WDK722-CHECK             PIC X       VALUE 'N'.                   
007800     88  WDK722-FINNS                        VALUE 'J'.                   
007900     88  WDK722-SAKNAS                       VALUE 'N'.                   
008000                                                                          
008100*    --- ARBETSAREA                                                       
008200 01  W-ARBETSAREA.                                                        
008300     03  W-CHKP                  PIC S9(5)   VALUE ZERO COMP-3.           
008400                                                                          
008500     EJECT                                                                
008600                                                                          
008700 01  DYNAMISKA-SUBPROGRAM.                                                
008800*                                                                         
008900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009300     03  W006KOM                 PIC X(8)    VALUE 'W006KOM'.             
009400     SKIP2                                                                
009500*    --- PARAMETRAR TILL ABEND                                            
009600                                                                          
009700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
009800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
009900     SKIP2                                                                
010000 01  FELTEXT.                                                             
010100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
010200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
010300     EJECT                                                                
010400*    --- PARAMETRAR TILL POSTSUM                                          
010500*                                                                         
010600*01  -COPY W0005   -PRE  POSTSUM-                                         
010700     EJECT                                                                
010800 01  W21410-AREA-START           PIC X(24)   VALUE                        
010900                                 'W21410-AREA-START  '.                   
011000     SKIP2                                                                
011100*01  AREA -COPY W21410     -PRE W21410-                                   
011200     EJECT                                                                
011300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011400*                                                                         
011500     EJECT                                                                
011600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011700     SKIP3                                                                
011800 01  NYCKLAR-TILL-DLI.                                                    
011900     03  W-IDARTNR-X.                                                     
012000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
012100     03  W-IDDC-X.                                                        
012200         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
012300     03  W-IDDC-B6-X.                                                     
012400         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
012500     03  W-KDSEGKEY-X.                                                    
012600         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
012700     03  W-IDSKYLT-X.                                                     
012800        05 W-IDSKYLT             PIC X(3)    VALUE 'S  '.                 
012900                                                                          
013000     03  W-WDGXKEY-2231-X.                                                
013100         05  W-IDHTYP-2231       PIC X(4)    VALUE '2231'.                
013200         05  FILLER              PIC X(26)   VALUE SPACE.                 
013300     03  W-WDGXKEY-2232-X.                                                
013400         05  W-IDANSK-X.                                                  
013500             07  W-IDANSK-2232   PIC S9(3)   COMP-3.                      
013600         05  FILLER              PIC X(3)    VALUE LOW-VALUE.             
013700     EJECT                                                                
013800     SKIP2                                                                
013900*    --- STATUS-KOD FRÅN IMS                                              
014000 01  STATUS-WS                   PIC XX.                                  
014100     88  SEGMENT-FINNS                       VALUE '  '.                  
014200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
014300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014400     88  IMS-EJ-OK                           VALUE 'XD'.                  
014500     SKIP2                                                                
014600 01  GODK-STATUSKODER.                                                    
014700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014800     SKIP3                                                                
014900 01  SSA1                        PIC X(64).                               
015000 01  SSA2                        PIC X(64).                               
015100 01  SSA3                        PIC X(64).                               
015200     EJECT                                                                
015300*    --- IMS FUNKTIONSKODER                                               
015400*01  -COPY W0003                                                          
015500     EJECT                                                                
015600*    ---  DLI INPUT-OUTPUT AREA                                           
015700                                                                          
015800 01  FILLER                    PIC X(16) VALUE 'DLI-IO-AREA-01'.          
015900     SKIP3                                                                
016000 01  DLI-IO-AREA-01.                                                      
016100     03  IO-AREA-01               PIC X(150) VALUE SPACE.                 
016200     03  WLARTC01 REDEFINES IO-AREA-01.                                   
016300*        05  -COPY WDK601                                                 
016400     SKIP3                                                                
016500                                                                          
016600 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA-11'.        
016700     SKIP3                                                                
016800 01  DLI-IO-AREA-11.                                                      
016900     03  IO-AREA-11               PIC X(900) VALUE SPACE.                 
017000     03  WLARTC11 REDEFINES IO-AREA-11.                                   
017100*        05  -COPY WDK611                                                 
017200                                                                          
017300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK722'.                      
017400 01  DLI-IO-WDK722.                                                       
017500*    03  -COPY WDK722                                                     
017600                                                                          
017700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
017800 01  DLI-IO-WDB601.                                                       
017900*    03  -COPY WDB601                                                     
018000*    --- DATA-AREA FÖR TRANSAKTION                                        
018100 01  FILLER                      PIC X(16) VALUE 'MSG-IO-AREA'.           
018200*01  -COPY WMSGAREA.                                                      
018300     EJECT                                                                
018400                                                                          
018500                                                                          
018600     05 FILLER REDEFINES MSG-MID-OUT.                                     
018700*       07  MID -COPY W2I19101   -PRE 2191-                               
018800     EJECT                                                                
018900                                                                          
019000                                                                          
019100*    --- KOMMUNIKATIONSAREA FÖR DISPATCHER                                
019200 01  FILLER                  PIC X(16) VALUE 'MSG-KOM-WMSGKOM'.           
019300*01  -COPY WMSGKOM                                                        
019400                                                                          
019500                                                                          
019600     EJECT                                                                
019700 LINKAGE SECTION.                                                         
019800*01  -COPY W0009  -PRE MSG-                                               
019900     EJECT                                                                
020000*01  -COPY W0009  -PRE ALT-                                               
020100     EJECT                                                                
020200*01  -COPY W0009  -PRE KOMA-                                              
020300     EJECT                                                                
020400*01  -COPY W0008  -PRE ARTC-                                              
020500     05  FILLER                  PIC X.                                   
020600*01  -COPY W0008  -PRE WDK7-                                              
020700     05  FILLER                  PIC X.                                   
020800*01  -COPY W0008  -PRE WDB6-                                              
020900     05  FILLER                  PIC X.                                   
021000     EJECT                                                                
021100 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB KOMA-PCB ARTC-PCB              
021200                           WDK7-PCB WDB6-PCB.                             
021300 MAIN SECTION.                                                            
021400     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB KOMA-PCB ARTC-PCB              
021500                           WDK7-PCB WDB6-PCB.                             
021600                                                                          
021700     PERFORM A-INIT                                                       
021800     PERFORM S01-LAES-W21410                                              
021900     PERFORM UNTIL END-OF-W21410                                          
022000        IF W21410-LARM-IDDC NOT = DCS-IDDC                                
022100           MOVE W21410-LARM-IDDC          TO W-IDDC-B6                    
022200           PERFORM IMS-GU-WDB601                                          
022300           IF SEGMENT-SAKNAS                                              
022400              MOVE SPACE                  TO DCS-IDDC                     
022500                                             DCS-KDDC                     
022600           END-IF                                                         
022700        END-IF                                                            
022800                                                                          
022900        MOVE ZERO                         TO WS-IDANSK                    
023000        MOVE NEJ                          TO SW-WDK722-CHECK              
023100*                                                                         
023200        MOVE W21410-LARM-IDARTNR          TO W-IDARTNR                    
023300        MOVE W21410-LARM-IDDC             TO W-IDDC                       
023400*                                                                         
023500        PERFORM IMS-GU-ARTC01                                             
023600        IF DCS-NDC-CN OR DCS-NDC-NA                                       
023700           PERFORM IMS-GHU-WDK722                                         
023800           IF SEGMENT-FINNS                                               
023900              MOVE W21410-LARM-FLLARM-BUF TO XLAG-FLLARM-BUF              
024000              MOVE XLAG-IDANSK            TO WS-IDANSK                    
024100              SET  WDK722-FINNS           TO TRUE                         
024200              PERFORM IMS-REPL-WDK722                                     
024300              ADD +1                      TO W-CHKP                       
024400           END-IF                                                         
024500        ELSE                                                              
024600           PERFORM IMS-GHNP-ARTC11                                        
024700           IF SEGMENT-FINNS                                               
024800              MOVE W21410-LARM-FLLARM-BUF TO CLAG-FLLARM-BUF              
024900              PERFORM IMS-REPL-ARTC11                                     
025000              ADD +1                      TO W-CHKP                       
025100           END-IF                                                         
025200        END-IF                                                            
025300                                                                          
025400        MOVE ART-KDPRODSL        TO TEST-KDPRODSL                         
025500        IF W21410-LARM-FLLARM-BUF = JA                                    
025600        AND KDPRODSL-VOLVO-BIMA                                           
025700          PERFORM C-SKAPA-KOMA-TRANS                                      
025800          PERFORM D-SKICKA-TRANS-TILL-2191                                
025900        ELSE                                                              
026000          CONTINUE                                                        
026100*          --- LARM 222 RENSAS I W2210400 & W2210300                      
026200        END-IF                                                            
026300                                                                          
026400        IF W-CHKP > MAX-CHKP                                              
026500          PERFORM E-TAG-CHECK-POINT                                       
026600        END-IF                                                            
026700        PERFORM S01-LAES-W21410                                           
026800     END-PERFORM                                                          
026900     PERFORM Z-FINIT                                                      
027000                                                                          
027100     MOVE ZERO TO RETURN-CODE                                             
027200     GOBACK                                                               
027300     .                                                                    
027400     EJECT                                                                
027500                                                                          
027600                                                                          
027700 A-INIT SECTION.                                                          
027800     MOVE ' A-INIT '                 TO CURRENT-SECTION                   
027900                                                                          
028000     OPEN INPUT  W21410                                                   
028100                                                                          
028200     PERFORM IMS-RESTART                                                  
028300     PERFORM AA-INITIERA-WMSGKOM-AREAN                                    
028400     .                                                                    
028500     EJECT                                                                
028600 AA-INITIERA-WMSGKOM-AREAN SECTION.                                       
028700     MOVE 'AA-INITIERA-WMSGKOM-AREAN' TO CURRENT-SECTION                  
028800                                                                          
028900     ACCEPT WS-TIUPPDAT FROM DATE                                         
029000     ACCEPT WS-TIUPPTID FROM TIME                                         
029100     MOVE SPACE                  TO MSG-KOM-WMSGKOM                       
029200     MOVE +54                    TO MSG-KOM-KVLL                          
029300     MOVE LOW-VALUE              TO MSG-KOM-KDZ1                          
029400     MOVE LOW-VALUE              TO MSG-KOM-KDZ2                          
029500     MOVE SPACE                  TO MSG-KOM-KDTRANS                       
029600     MOVE 'W2I19101'             TO MSG-KOM-IDCPYTXT                      
029700     MOVE 'ANSKLARM'             TO MSG-KOM-IDSNDNOD                      
029800     MOVE 'W2141200'             TO MSG-KOM-IDSNDJOB                      
029900     MOVE WS-TIUPPDAT            TO MSG-KOM-TIREGDAT                      
030000     MOVE WS-TIUPPTID            TO MSG-KOM-TIKLOCK                       
030100     MOVE SPACE                  TO MSG-KOM-IDMFSMED                      
030200     .                                                                    
030300     EJECT                                                                
030400                                                                          
030500                                                                          
030600 C-SKAPA-KOMA-TRANS SECTION.                                              
030700     MOVE 'C-SKAPA-KOMA-TRANS '   TO CURRENT-SECTION                      
030800                                                                          
030900     MOVE LENGTH OF 2191-MID-W2I19101                                     
031000                                  TO   MSG-KVLL                           
031100     ADD  +17                     TO   MSG-KVLL                           
031200     MOVE   LOW-VALUE             TO   MSG-KDZ1                           
031300                                       MSG-KDZ2                           
031400     MOVE   'W2T191X'             TO   MSG-KDTRANS-1                      
031500     MOVE   '2191'                TO   MSG-IDTRANS-1                      
031600     MOVE   '1'                   TO   MSG-KDMFSFOR-1                     
031700     SKIP2                                                                
031800*    --- FLYTTA MIDDEN                                                    
031900     MOVE W21410-LARM-IDARTNR     TO 2191-MID-IDARTNR                     
032000     IF  DCS-NDC-CN                                                       
032100     OR (DCS-NDC-NA AND DCS-USA)                                          
032200        IF WDK722-FINNS                                                   
032300           MOVE WS-IDANSK         TO 2191-MID-IDANSK                      
032400        ELSE                                                              
032500           IF DCS-NDC-CN                                                  
032600              MOVE '300'          TO 2191-MID-IDANSK                      
032700           ELSE                                                           
032800              MOVE '200'          TO 2191-MID-IDANSK                      
032900           END-IF                                                         
033000        END-IF                                                            
033100     ELSE                                                                 
033200        MOVE CLAG-IDANSK          TO 2191-MID-IDANSK                      
033300     END-IF                                                               
033400     MOVE '222'                   TO 2191-MID-KDLARM                      
033500     MOVE ZERO                    TO 2191-MID-KDCLAGER                    
033600                                     2191-MID-TISENBEK-DAG                
033700                                     2191-MID-TISENBEK-KL                 
033800                                     2191-MID-IDDISTR                     
033900                                     2191-MID-IDKUNDNR                    
034000     MOVE SPACE                   TO 2191-MID-IDKR                        
034100                                     2191-MID-IDKUNDRF                    
034200     MOVE 'J'                     TO 2191-MID-FLNYLARM                    
034300     MOVE W21410-LARM-IDDC        TO 2191-MID-IDDC                        
034400     MOVE W21410-LARM-IDLEVNR     TO 2191-MID-IDLEVNR                     
034500     .                                                                    
034600     EJECT                                                                
034700 D-SKICKA-TRANS-TILL-2191 SECTION.                                        
034800     MOVE 'D-SKICKA-TRANS-TILL-2191' TO CURRENT-SECTION                   
034900     CALL W006KOM USING MSG-PCB                                           
035000                        ALT-PCB                                           
035100                        KOMA-PCB                                          
035200                        MSG-KOM-WMSGKOM                                   
035300                        MSG-IO-AREA                                       
035400                                                                          
035500     MOVE 'W21412'      TO POSTSUM-FDNAMN                                 
035600     MOVE 'W2T191X'     TO POSTSUM-DDNAMN2                                
035700     MOVE 'TRAN'        TO POSTSUM-TRANSTYP                               
035800     CALL POSTSUM USING POSTSUM-PARM                                      
035900                                                                          
036000     ADD +1            TO W-CHKP                                          
036100     .                                                                    
036200     EJECT                                                                
036300                                                                          
036400 E-TAG-CHECK-POINT SECTION.                                               
036500     SKIP3                                                                
036600     PERFORM IMS-CHECK-POINT                                              
036700     MOVE ZERO               TO W-CHKP                                    
036800     .                                                                    
036900     EJECT                                                                
037000                                                                          
037100                                                                          
037200 Z-FINIT SECTION.                                                         
037300     CLOSE W21410                                                         
037400     SKIP2                                                                
037500     MOVE 'S' TO POSTSUM-OPKOD                                            
037600     CALL POSTSUM USING POSTSUM-PARM                                      
037700     .                                                                    
037800     EJECT                                                                
037900                                                                          
038000* --- INFIL ---                                                           
038100                                                                          
038200 S01-LAES-W21410  SECTION.                                                
038300     READ W21410 INTO W21410-AREA                                         
038400     AT END                                                               
038500        SET END-OF-W21410 TO TRUE                                         
038600                                                                          
038700     NOT AT END                                                           
038800        MOVE 'W21410'   TO POSTSUM-FDNAMN                                 
038900        MOVE 'W21410D1' TO POSTSUM-DDNAMN2                                
039000        CALL POSTSUM USING POSTSUM-PARM                                   
039100     END-READ                                                             
039200     .                                                                    
039300     EJECT                                                                
039400* --- IMS SEKTIONER ---                                                   
039500                                                                          
039600 IMS-RESTART SECTION.                                                     
039700     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
039800     MOVE '  ' TO  GODK-STATUSKODER                                       
039900     CALL CBLTDLI USING XRST MSG-PCB                                      
040000                             CHKP-MSG-IO-AREA-LENGTH                      
040100                             CHKP-MSG-IO-AREA                             
040200                             CHKP-AREA-1-LENGTH                           
040300                             CHKP-AREA-1                                  
040400                                                                          
040500     MOVE MSG-STATUS-CODE    TO STATUS-WS                                 
040600     PERFORM IMS-STATUSKONTROLL                                           
040700     .                                                                    
040800                                                                          
040900                                                                          
041000 IMS-CHECK-POINT SECTION.                                                 
041100     MOVE CHKP-ID TO CHKP-MSG-IO-AREA                                     
041200     MOVE '  XD' TO  GODK-STATUSKODER                                     
041300     CALL CBLTDLI USING CHKP MSG-PCB                                      
041400                             CHKP-MSG-IO-AREA-LENGTH                      
041500                             CHKP-MSG-IO-AREA                             
041600                             CHKP-AREA-1-LENGTH                           
041700                             CHKP-AREA-1                                  
041800                                                                          
041900     MOVE MSG-STATUS-CODE    TO STATUS-WS                                 
042000     PERFORM IMS-STATUSKONTROLL                                           
042100     IF IMS-EJ-OK                                                         
042200       DISPLAY 'IMS-KONTROLLREGION EJ TILLGÄNGLIG'                        
042300       CALL FELLOG                                                        
042400     END-IF                                                               
042500     .                                                                    
042600     EJECT                                                                
042700 IMS-GU-ARTC01 SECTION.                                                   
042800     MOVE 'IMS-GU-ARTC01     ' TO DBS-SECTION                             
042900                                                                          
043000     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X  ')'                        
043100          DELIMITED BY SIZE INTO SSA1                                     
043200     MOVE '    ' TO GODK-STATUSKODER                                      
043300     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-01 SSA1                   
043400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
043500     PERFORM IMS-STATUSKONTROLL                                           
043600     .                                                                    
043700                                                                          
043800                                                                          
043900 IMS-GHNP-ARTC11 SECTION.                                                 
044000     MOVE 'IMS-GHNP-ARTC11  ' TO DBS-SECTION                              
044100                                                                          
044200     MOVE 'WLARTC11 ' TO SSA1                                             
044300     MOVE '  GE' TO GODK-STATUSKODER                                      
044400     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA-11 SSA1                 
044500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
044600     PERFORM IMS-STATUSKONTROLL                                           
044700     .                                                                    
044800                                                                          
044900                                                                          
045000 IMS-REPL-ARTC11 SECTION.                                                 
045100     MOVE 'IMS-REPL-ARTC11  ' TO DBS-SECTION                              
045200                                                                          
045300     MOVE '  ' TO GODK-STATUSKODER                                        
045400     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA-11                      
045500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
045600     PERFORM IMS-STATUSKONTROLL                                           
045700     .                                                                    
045800     EJECT                                                                
045900 IMS-GHU-WDK722 SECTION.                                                  
046000     MOVE 'IMS-GHU-WDK722  ' TO DBS-SECTION                               
046100                                                                          
046200     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
046300          DELIMITED BY SIZE INTO SSA1                                     
046400     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
046500          DELIMITED BY SIZE INTO SSA2                                     
046600     STRING 'WDK722  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
046700          DELIMITED BY SIZE INTO SSA3                                     
046800     MOVE '  GE' TO GODK-STATUSKODER                                      
046900     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3         
047000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
047100     PERFORM IMS-STATUSKONTROLL                                           
047200     .                                                                    
047300     EJECT                                                                
047400 IMS-REPL-WDK722 SECTION.                                                 
047500     MOVE 'IMS-REPL-WDK722 ' TO DBS-SECTION                               
047600                                                                          
047700     MOVE '  ' TO GODK-STATUSKODER                                        
047800     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK722                       
047900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
048000     PERFORM IMS-STATUSKONTROLL                                           
048100     .                                                                    
048200     EJECT                                                                
048300                                                                          
048400                                                                          
048500 IMS-GU-WDB601 SECTION.                                                   
048600     MOVE 'IMS-GU-WDB601  ' TO DBS-SECTION                                
048700                                                                          
048800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X  ')'                        
048900          DELIMITED BY SIZE INTO SSA1                                     
049000     MOVE '  GE' TO GODK-STATUSKODER                                      
049100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
049200     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
049300     PERFORM IMS-STATUSKONTROLL                                           
049400     .                                                                    
049500                                                                          
049600                                                                          
049700 IMS-STATUSKONTROLL SECTION.                                              
049800                                                                          
049900     SET STATUS-IX TO 1                                                   
050000     SEARCH GODK-STATUS                                                   
050100       AT END                                                             
050200         CALL FELLOG                                                      
050300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
050400         CONTINUE                                                         
050500     END-SEARCH                                                           
050600     .                                                                    
