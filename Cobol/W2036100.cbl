000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2036100.                                                
000400*AUTHOR.         STEFAN ANDREASSON, FRONTEC.                              
000500*DATE-WRITTEN.   97/02/25.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAM SOM VISAR SALDOINFORMATION TOTALT, DVS FÖR               
001100*        BÅDE CDCR OCH NDC. CDC SAMT MAX FYRA                             
001200*        NDC VISAS.                                                       
001300*        PROGRAMMET ÄR EN KOPIA AV W2034400                               
001400*                                                                         
001500*        PROGRAMMET LÄSER      WDK6                                       
001600*        PROGRAMMET LÄSER      WDK7                                       
001700*        PROGRAMMET LÄSER      WDK9                                       
001800*        PROGRAMMET LÄSER      WDD3                                       
001900*        PROGRAMMET LÄSER      WDD7                                       
002000*        PROGRAMMET LÄSER      WDD7A                                      
002100*        PROGRAMMET LÄSER      WDB6                                       
002200*                                                                         
002300*    INDATA.                                                              
002400*        TRANSAKTION: W2T361                                              
002500*        MID:         W2I36101                                            
002600*                                                                         
002700*    UTDATA.                                                              
002800*        MOD:         W2O36101                                            
002900*                                                                         
003000*                                                                         
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600 WORKING-STORAGE SECTION.                                                 
003700                                                                          
003800*    -- CHECKED BY WY2000                                                 
003900 77  IDPGM                       PIC X(08)   VALUE 'W2036100'.            
004000                                                                          
004100*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004200 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004300                                                                          
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004600                                                                          
004700 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004800 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +576  COMP SYNC.        
004900                                                                          
005000*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005100 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
005200 77  WS-IDDC-1                   PIC X(2)    VALUE SPACE.                 
005300                                                                          
005400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005500     88  NYCKLAR-OK                          VALUE 'J'.                   
005600     88  NYCKLAR-FEL                         VALUE 'N'.                   
005700                                                                          
005800 77  REF-TO-CDC-SW               PIC X       VALUE 'N'.                   
005900     88  REF-TO-CDC                          VALUE 'J'.                   
006000     88  EJ-REF-TO-CDC                       VALUE 'N'.                   
006100                                                                          
006200 77  ALLT-SW                     PIC X       VALUE 'J'.                   
006300     88  ALLT-OK                             VALUE 'J'.                   
006400                                                                          
006500 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006600     88  EGEN-MID                            VALUE '2361'.                
006700     88  GODK-MID                            VALUE '2361' '2362'          
006800                                                   '2363' '2364'          
006900                                                   '2365' '2366'          
007000                                                   '2367' '2368'          
007100                                                   '2369'.                
007200     88  HELP-MID                            VALUE '0551'.                
007300 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007400 77  DAGENS-DATUM-SEKEL          PIC 9(8)    VALUE ZERO.                  
007500     EJECT                                                                
007600 01  FILLER                      PIC X(8)    VALUE 'ARB-FÄLT'.            
007700 01  ARBETSFAELT.                                                         
007800     03  DC-IX                   PIC  9(2)   VALUE ZERO.                  
007900     03  WS-KVOKS-TOT-CDC        PIC S9(7)   VALUE ZERO COMP-3.           
008000     03  WS-KVDISP-CLAG          PIC S9(7)   VALUE ZERO COMP-3.           
008100     03  WS-KVDISP-SLAG          PIC S9(7)   VALUE ZERO COMP-3.           
008200     03  WS-KVOKS-TOT-NDC        PIC S9(7)   VALUE ZERO COMP-3.           
008300     03  WS-KVROS                PIC S9(7)   VALUE ZERO.                  
008400     03  WS-FLREFERAL            PIC X       VALUE 'N'.                   
008500                                                                          
008600*      --- VALID IDDC CODES                                               
008700*                                                                         
008800*01    -COPY WWDC99                                                       
008900*01    -COPY WWDC99 -PRE BAS-                                             
009000*01    -COPY WWDCKONS                                                     
009100       EJECT                                                              
009200*                                                                         
009300*01  -COPY WWDCLAND                                                       
009400                                                                          
009500                                                                          
009600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
009700 01  GENERELLA-SUBPROGRAM.                                                
009800     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
009900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
010200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010300     EJECT                                                                
010400*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
010500*01 -COPY WMEDAREA                                                        
010600     EJECT                                                                
010700*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
010800*01 -COPY WMSGINIT                                                        
010900     EJECT                                                                
011000*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
011100*01  -COPY WDATAREA                                                       
011200     EJECT                                                                
011300     SKIP3                                                                
011400 01  MESSAGE-CODES.                                                       
011500     03  ARTIKEL-SAKNAS          PIC X(3)    VALUE '017'.                 
011600     03  ARTIKEL-SAKNAS-NDC      PIC X(3)    VALUE '305'.                 
011700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
011800                                                                          
011900     EJECT                                                                
012000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
012100*                                                                         
012200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
012300     SKIP3                                                                
012400*01  MID -COPY W2I36101                                                   
012500     EJECT                                                                
012600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
012700     SKIP3                                                                
012800*01  -COPY WMSGAREA                                                       
012900     EJECT                                                                
013000     03  MOD REDEFINES MSG-AREA.                                          
013100*      05  -COPY W2O36101                                                 
013200     EJECT                                                                
013300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
013400     SKIP3                                                                
013500*01  -COPY WMFSAREA                                                       
013600     EJECT                                                                
013700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013800*                                                                         
013900     EJECT                                                                
014000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014100     SKIP3                                                                
014200 01  NYCKLAR-TILL-DLI.                                                    
014300     03  W-IDARTNR-X.                                                     
014400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
014500     03  W-IDDC-X.                                                        
014600         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
014700     03  W-IDSKYLT-X.                                                     
014800         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
014900     03  W-IDLAND-X.                                                      
015000         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
015100     SKIP2                                                                
015200     03 W-WDD7A1KY-MIN.                                                   
015300       05 W-IDARTNR-MIN7          PIC S9(9)  COMP-3 VALUE ZERO.           
015400       05 FILLER                  PIC S9(9)  COMP-3 VALUE ZERO.           
015500       05 FILLER                  PIC S9(3)  COMP-3 VALUE ZERO.           
015600                                                                          
015700     03 W-WDD7A1KY-MAX.                                                   
015800       05 W-IDARTNR-MAX7   PIC S9(9)  COMP-3 VALUE ZERO.                  
015900       05 FILLER           PIC S9(9)  COMP-3 VALUE +999999999.            
016000       05 FILLER           PIC S9(3)  COMP-3 VALUE +999.                  
016100*    --- STATUS-KOD FRÅN IMS                                              
016200 01  STATUS-WS                   PIC XX.                                  
016300     88  SEGMENT-FINNS                       VALUE '  '.                  
016400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
016500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
016600     SKIP2                                                                
016700 01  GODK-STATUSKODER.                                                    
016800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016900     SKIP3                                                                
017000 01  SSA1                        PIC X(64).                               
017100 01  SSA2                        PIC X(64).                               
017200     EJECT                                                                
017300*    --- IMS FUNKTIONSKODER                                               
017400*01  -COPY W0003                                                          
017500     EJECT                                                                
017600*    ---  DLI INPUT-OUTPUT AREA                                           
017700                                                                          
017800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK601'.                      
017900 01  DLI-IO-WDK601.                                                       
018000*    03  -COPY WDK601                                                     
018100     EJECT                                                                
018200                                                                          
018300 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK611'.                      
018400 01  DLI-IO-WDK611.                                                       
018500*    03  -COPY WDK611                                                     
018600     EJECT                                                                
018700                                                                          
018800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK901'.                      
018900 01  DLI-IO-WDK901.                                                       
019000*    03  -COPY WDK901 -PRE WDK9-                                          
019100     EJECT                                                                
019200                                                                          
019300 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK701'.                      
019400 01  DLI-IO-WDK701.                                                       
019500*    03  -COPY WDK701                                                     
019600     EJECT                                                                
019700                                                                          
019800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK711'.                      
019900 01  DLI-IO-WDK711.                                                       
020000*    03  -COPY WDK711                                                     
020100     EJECT                                                                
020200                                                                          
020300 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK712'.                      
020400 01  DLI-IO-WDK712.                                                       
020500*    03  -COPY WDK712                                                     
020600     EJECT                                                                
020700                                                                          
020800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD311'.                      
020900 01  DLI-IO-WDD311.                                                       
021000*    03  -COPY WDD311                                                     
021100     EJECT                                                                
021200 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDD701'.            
021300     SKIP3                                                                
021400 01  DLI-IO-AREA-WDD701.                                                  
021500*  03  WDD701   -COPY WDD701  -PRE WDD701-                                
021600     EJECT                                                                
021700 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDD702'.            
021800     SKIP3                                                                
021900 01  DLI-IO-AREA-WDD702.                                                  
022000*  03  WDD702   -COPY WDD702  -PRE WDD702-                                
022100     EJECT                                                                
022200 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDD7A1'.            
022300     SKIP3                                                                
022400 01  DLI-IO-AREA-WDD7A1.                                                  
022500*  03  WDD7A1   -COPY WDD7A1  -PRE WDD7A1-                                
022600     EJECT                                                                
022700 01  FILLER                  PIC X(16)  VALUE 'DLI-IO-WDB601'.            
022800     SKIP3                                                                
022900 01  DLI-IO-AREA-WDB601.                                                  
023000*  03  WDB60101 -COPY WDB601  -PRE WDB601-                                
023100     EJECT                                                                
023200                                                                          
023300 LINKAGE SECTION.                                                         
023400                                                                          
023500*01  -COPY W0009   -PRE MSG-                                              
023600     EJECT                                                                
023700*01  -COPY W0008  -PRE USEA-                                              
023800     05  FILLER                  PIC X.                                   
023900     EJECT                                                                
024000*01  -COPY W0008  -PRE WDK6-                                              
024100     05  FILLER                  PIC X.                                   
024200     EJECT                                                                
024300*01  -COPY W0008  -PRE WDK7-                                              
024400     05  FILLER                  PIC X.                                   
024500     EJECT                                                                
024600*01  -COPY W0008  -PRE WDK9-                                              
024700     05  FILLER                  PIC X.                                   
024800     EJECT                                                                
024900*01  -COPY W0008  -PRE WDD3-                                              
025000     05  FILLER                  PIC X.                                   
025100     EJECT                                                                
025200*01  -COPY W0008  -PRE WDD7-                                              
025300     05  FILLER                  PIC X.                                   
025400     EJECT                                                                
025500*01  -COPY W0008  -PRE WDD7A-                                             
025600     05  FILLER                  PIC X.                                   
025700     EJECT                                                                
025800*01  -COPY W0008  -PRE WDB6-                                              
025900     05  FILLER                  PIC X.                                   
026000     EJECT                                                                
026100*01  -COPY W0008  -PRE WDK72-                                             
026200     05  FILLER                  PIC X.                                   
026300     EJECT                                                                
026400 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB                               
026500                                   WDK6-PCB WDK7-PCB                      
026600                           WDK9-PCB WDD3-PCB WDD7-PCB WDD7A-PCB           
026700                           WDB6-PCB WDK72-PCB.                            
026800     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
026900                                   WDK6-PCB WDK7-PCB                      
027000                           WDK9-PCB WDD3-PCB WDD7-PCB WDD7A-PCB           
027100                           WDB6-PCB WDK72-PCB.                            
027200                                                                          
027300     PERFORM IMS-GET-MSG                                                  
027400     IF SEGMENT-FINNS                                                     
027500       PERFORM A-INIT                                                     
027600       PERFORM B-KOLLA-NYCKLAR                                            
027700       IF NYCKLAR-OK                                                      
027800         PERFORM F-LAES-VISA-INFO                                         
027900       END-IF                                                             
028000       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
028100       PERFORM IMS-INSERT-MSG                                             
028200     END-IF                                                               
028300                                                                          
028400     MOVE ZERO TO RETURN-CODE                                             
028500     GOBACK                                                               
028600     .                                                                    
028700     EJECT                                                                
028800 A-INIT SECTION.                                                          
028900                                                                          
029000     IF MSG-DUBBLA-TRANSKODER                                             
029100       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I36101                 
029200       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
029300       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
029400     ELSE                                                                 
029500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I36101                  
029600       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
029700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
029800     END-IF                                                               
029900                                                                          
030000     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
030100     MOVE MSG-IDPFK TO MFS-IDPFK                                          
030200     MOVE MFS-IDTRANS TO W-IDTRANS                                        
030300                                                                          
030400     MOVE LOW-VALUE TO MSG-AREA                                           
030500     MOVE 'W2O361N1' TO MFS-IDMOD                                         
030600     MOVE '2361' TO MOD-IDTRANS                                           
030700     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
030800                                                                          
030900     COMPUTE MAX-MOD-LAENGD = LENGTH OF MOD-W2O36101 + 4                  
031000                                                                          
031100     IF EGEN-MID OR HELP-MID                                              
031200       CONTINUE                                                           
031300     ELSE                                                                 
031400       MOVE SPACE TO MFS-KDTRTYP                                          
031500       MOVE '7' TO MFS-IDPFK                                              
031600     END-IF                                                               
031700                                                                          
031800     MOVE +2                 TO SPRAK-IX                                  
031900     MOVE 'GB '              TO MED-IDSKYLT                               
032000                                W-IDSKYLT                                 
032100                                                                          
032200     ACCEPT DAGENS-DATUM FROM DATE                                        
032300                                                                          
032400     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
032500     MOVE DAGENS-DATUM       TO DAT-I-TIDATUM                             
032600                                                                          
032700     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
032800                     DAT-O-TIDATUM DAT-KDSVAR                             
032900                                                                          
033000     IF DAT-KDSVAR-OK                                                     
033100       MOVE DAGENS-DATUM      TO DAGENS-DATUM-SEKEL(3:6)                  
033200       MOVE DAT-TISEKEL       TO DAGENS-DATUM-SEKEL(1:2)                  
033300     ELSE                                                                 
033400       STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS                  
033500       DELIMITED BY SIZE INTO FELTEXT                                     
033600       CALL FELLOG                                                        
033700     END-IF                                                               
033800     .                                                                    
033900     EJECT                                                                
034000 B-KOLLA-NYCKLAR SECTION.                                                 
034100                                                                          
034200     MOVE ALL '+'           TO MSGI-WMSGINIT                              
034300     MOVE '001'             TO MSGI-KDCALL                                
034400     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
034500     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
034600     MOVE '2361'            TO MSGI-IDTRANS                               
034700     IF EGEN-MID                                                          
034800       MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                               
034900       MOVE MID-IDDC-IN     TO MSGI-IDDC-KEY                              
035000     ELSE                                                                 
035100       IF  MID-IDARTNR-IN NUMERIC                                         
035200       AND MID-IDARTNR-IN > ZERO                                          
035300         MOVE MID-IDARTNR-IN                                              
035400                            TO MSGI-IDARTNR                               
035500       END-IF                                                             
035600     END-IF                                                               
035700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
035800                                                                          
035900     MOVE JA TO NYCKLAR-SW                                                
036000                                                                          
036100*    -- KONTROLL AV IDARTNR                                               
036200     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
036300                                                                          
036400     IF MID-IDARTNR-IN NOT = ALL '+'                                      
036500       MOVE '7'         TO MFS-IDPFK                                      
036600       MOVE SPACE       TO MFS-KDTRTYP                                    
036700     END-IF                                                               
036800     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
036900     IF MSGI-IDARTNR NUMERIC                                              
037000       MOVE MSGI-IDARTNR TO WS-IDARTNR                                    
037100     ELSE                                                                 
037200       MOVE NEJ TO NYCKLAR-SW                                             
037300     END-IF                                                               
037400                                                                          
037500*    -- KONTROLL AV IDDC                                                  
037600     MOVE MFS-RENSA-FAELT    TO MOD-IDDC-IN                               
037700                                                                          
037800     IF MID-IDDC-IN NOT = ALL '+'                                         
037900       MOVE '7'              TO MFS-IDPFK                                 
038000       MOVE SPACE            TO MFS-KDTRTYP                               
038100     END-IF                                                               
038200                                                                          
038300     MOVE MSGI-IDDC-KEY    TO WS-IDDC-1                                   
038400                              W-IDDC                                      
038500     PERFORM IMS-GU-WDB601                                                
038600     IF SEGMENT-FINNS                                                     
038700        AND NOT WDB601-DCS-CDC-TR                                         
038800         CONTINUE                                                         
038900     ELSE                                                                 
039000         MOVE NEJ TO NYCKLAR-SW                                           
039100     END-IF                                                               
039200                                                                          
039300     IF NYCKLAR-FEL                                                       
039400       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
039500       CALL WMEDKONV USING MED-WMEDAREA                                   
039600       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
039700       PERFORM MFS-RENSA-FAELT-UT                                         
039800     ELSE                                                                 
039900       MOVE WS-IDARTNR     TO MOD-IDARTNR-UT                              
040000                              W-IDARTNR                                   
040100       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
040200       MOVE WS-IDDC-1      TO MOD-IDDC-UT                                 
040300     END-IF                                                               
040400     .                                                                    
040500     EJECT                                                                
040600 F-LAES-VISA-INFO SECTION.                                                
040700                                                                          
040800     MOVE MFS-RENSA-FAELT    TO MOD-REPL-BY                               
040900     PERFORM IMS-GU-WDD701                                                
041000     IF SEGMENT-FINNS                                                     
041100        PERFORM IMS-GNP-WDD702                                            
041200        IF SEGMENT-FINNS                                                  
041300           MOVE WDD702-IDARTNR-TILLK                                      
041400                             TO MOD-REPL-BY                               
041500           INSPECT MOD-REPL-BY REPLACING LEADING ZERO BY SPACE            
041600           PERFORM IMS-GNP-WDD702                                         
041700           IF SEGMENT-FINNS                                               
041800             MOVE 'VARIOUS'  TO MOD-REPL-BY                               
041900           END-IF                                                         
042000        END-IF                                                            
042100     END-IF                                                               
042200*                                                                         
042300     PERFORM IMS-GU-WDK711                                                
042400     IF SEGMENT-FINNS                                                     
042500        MOVE SLAG-IDDC-REF TO WS-IDDC                                     
042600     ELSE                                                                 
042700        MOVE SPACES        TO WS-IDDC                                     
042800     END-IF                                                               
042900                                                                          
043000     IF W-IDDC = '11'                                                     
043100       MOVE  JA TO REF-TO-CDC-SW                                          
043200       PERFORM FA-LAES-WDK6                                               
043300       IF ALLT-OK                                                         
043400         PERFORM FD-MOVE-MOD-REPL                                         
043500         PERFORM FE-LAES-WDK7-UNIK                                        
043600          IF NOT ALLT-OK                                                  
043700*           PERFORM MFS-RENSA-NDC                                         
043800            PERFORM MFS-RENSA-FAELT-CDC-KOL                               
043900            MOVE ARTIKEL-SAKNAS-NDC TO MED-IDMFSFEL                       
044000            CALL WMEDKONV USING MED-WMEDAREA                              
044100            MOVE MED-MFSFEL   TO MOD-TEMFSFEL                             
044200          END-IF                                                          
044300          PERFORM FC-LAES-WDD3                                            
044400       ELSE                                                               
044500          PERFORM MFS-RENSA-CDC                                           
044600*         PERFORM MFS-RENSA-NDC                                           
044700          PERFORM MFS-RENSA-FAELT-CDC-KOL                                 
044800          MOVE ERR-WRONG-KEY   TO MED-IDMFSFEL                            
044900*         MOVE ARTIKEL-SAKNAS  TO MED-IDMFSFEL                            
045000          CALL WMEDKONV USING MED-WMEDAREA                                
045100          MOVE MED-MFSFEL      TO MOD-TEMFSFEL                            
045200       END-IF                                                             
045300     ELSE                                                                 
045400       PERFORM FA-LAES-WDK6                                               
045500       IF ALLT-OK                                                         
045600*                                                                         
045700          PERFORM FD-MOVE-MOD-REPL                                        
045800          PERFORM FB-LAES-WDK7                                            
045900          IF NOT ALLT-OK                                                  
046000            PERFORM MFS-RENSA-NDC                                         
046100            MOVE ARTIKEL-SAKNAS-NDC TO MED-IDMFSFEL                       
046200            CALL WMEDKONV USING MED-WMEDAREA                              
046300            MOVE MED-MFSFEL   TO MOD-TEMFSFEL                             
046400          END-IF                                                          
046500          PERFORM FC-LAES-WDD3                                            
046600       ELSE                                                               
046700          PERFORM MFS-RENSA-CDC                                           
046800          PERFORM MFS-RENSA-NDC                                           
046900          MOVE ARTIKEL-SAKNAS  TO MED-IDMFSFEL                            
047000          CALL WMEDKONV USING MED-WMEDAREA                                
047100          MOVE MED-MFSFEL      TO MOD-TEMFSFEL                            
047200       END-IF                                                             
047300     END-IF                                                               
047400                                                                          
047500     .                                                                    
047600     EJECT                                                                
047700 FA-LAES-WDK6      SECTION.                                               
047800                                                                          
047900     PERFORM IMS-GU-WDK601                                                
048000     IF SEGMENT-FINNS                                                     
048100       MOVE ART-KDPRODSL    TO MOD-KDPRODSL                               
048200       PERFORM IMS-GU-WDK9                                                
048300       IF SEGMENT-FINNS                                                   
048400         PERFORM FAA-SUMMERA-OKS                                          
048500       END-IF                                                             
048600       PERFORM IMS-GNP-WDK611                                             
048700       IF SEGMENT-FINNS                                                   
048800         IF REF-TO-CDC                                                    
048900           IF CLAG-IDDC-REF = SPACE                                       
049000             MOVE NEJ       TO ALLT-SW                                    
049100           ELSE                                                           
049200             PERFORM FAC-FLYTTA-TILL-MOD                                  
049300           END-IF                                                         
049400         ELSE                                                             
049500           PERFORM FAB-FLYTTA-TILL-MOD                                    
049600         END-IF                                                           
049700       ELSE                                                               
049800         MOVE NEJ           TO ALLT-SW                                    
049900       END-IF                                                             
050000     ELSE                                                                 
050100       MOVE NEJ             TO ALLT-SW                                    
050200     END-IF                                                               
050300     .                                                                    
050400     EJECT                                                                
050500 FAA-SUMMERA-OKS SECTION.                                                 
050600                                                                          
050700     COMPUTE WS-KVOKS-TOT-CDC    = WDK9-ART-KVOKS-BULK +                  
050800                                   WDK9-ART-KVOKS-DAG +                   
050900                                   WDK9-ART-KVOKS-VOR                     
051000     .                                                                    
051100     EJECT                                                                
051200 FAB-FLYTTA-TILL-MOD SECTION.                                             
051300                                                                          
051400     MOVE CLAG-KVQPACK-1        TO MOD-KVQPACK-1                          
051500     MOVE CLAG-KDERS            TO MOD-KDERS                              
051600     IF CDC OR WS-IDDC = SPACES                                           
051700       COMPUTE WS-KVDISP-CLAG  = CLAG-KVLS    -                           
051800                                 WS-KVOKS-TOT-CDC -                       
051900                                 CLAG-KVRESS                              
052000                                                                          
052100                                                                          
052200       MOVE WC-CDC-SE           TO MOD-IDDC-CDC                           
052300       MOVE CLAG-KVLS           TO MOD-KVLS-CDC                           
052400       MOVE WS-KVDISP-CLAG      TO MOD-KVDISP-CDC                         
052500       MOVE CLAG-KVUTRS         TO MOD-KVUTRS-CDC                         
052600       MOVE WS-KVOKS-TOT-CDC    TO MOD-KVOKS-CDC                          
052700       MOVE CLAG-KVRESS         TO MOD-KVRESS-CDC                         
052800       MOVE CLAG-KVROS          TO MOD-KVROS-CDC                          
052900       MOVE CLAG-KVEFRS         TO MOD-KVEFRS-CDC                         
053000       MOVE CLAG-KVAKS-CDC      TO MOD-KVAKS-CDC                          
053100       MOVE CLAG-KVAKS-PAV      TO MOD-KVAKS-PAV-CDC                      
053200       MOVE CLAG-KDLEVSP        TO MOD-KDLEVSP-CDC                        
053300       MOVE CLAG-KDPSLLOC       TO MOD-KDPSLLOC                           
053400       MOVE CLAG-KVSPARR-KVAL   TO MOD-KVSPARR-KVAL-CDC                   
053500       MOVE CLAG-KVBEART        TO MOD-KVBEART-CDC                        
053600     END-IF                                                               
053700     .                                                                    
053800                                                                          
053900 FAC-FLYTTA-TILL-MOD SECTION.                                             
054000                                                                          
054100     MOVE CLAG-IDDC-REF         TO W-IDDC                                 
054200                                                                          
054300     MOVE '-'                   TO MOD-BLOCKCODE-NDC (1)                  
054400     MOVE '-'                   TO MOD-FREEZECODE-NDC (1)                 
054500     MOVE CLAG-KVQPACK-1        TO MOD-KVQPACK-1                          
054600     MOVE CLAG-KDERS            TO MOD-KDERS                              
054700     COMPUTE WS-KVDISP-CLAG    = CLAG-KVLS    -                           
054800                               WS-KVOKS-TOT-CDC -                         
054900                               CLAG-KVRESS                                
055000                                                                          
055100                                                                          
055200     MOVE WC-CDC-SE             TO MOD-IDDC-NDC(1)                        
055300     MOVE CLAG-KVLS             TO MOD-KVLS-NDC(1)                        
055400     MOVE WS-KVDISP-CLAG        TO MOD-KVDISP-NDC(1)                      
055500     MOVE CLAG-KVUTRS           TO MOD-KVUTRS-NDC(1)                      
055600     MOVE WS-KVOKS-TOT-CDC      TO MOD-KVOKS-NDC(1)                       
055700     MOVE CLAG-KVRESS           TO MOD-KVRESS-NDC(1)                      
055800     MOVE CLAG-KVROS            TO MOD-KVROS-NDC(1)                       
055900     MOVE CLAG-KVEFRS           TO MOD-KVEFRS-NDC(1)                      
056000     MOVE CLAG-KVAKS-CDC        TO MOD-KVAKS-NDC(1)                       
056100     MOVE CLAG-KVAKS-PAV        TO MOD-KVAKS-PAV-NDC(1)                   
056200     MOVE CLAG-KDLEVSP          TO MOD-KDLEVSP-NDC(1)                     
056300     MOVE CLAG-KDPSLLOC         TO MOD-KDPSLLOC                           
056400     MOVE CLAG-KVSPARR-KVAL     TO MOD-KVSPARR-KVAL-NDC(1)                
056500     MOVE CLAG-KVBEART          TO MOD-KVBEART(1)                         
056600     .                                                                    
056700     EJECT                                                                
056800 FB-LAES-WDK7  SECTION.                                                   
056900                                                                          
057000     PERFORM IMS-GU-WDK701                                                
057100     IF SEGMENT-SAKNAS                                                    
057200       MOVE ARTIKEL-SAKNAS-NDC TO MED-IDMFSFEL                            
057300       CALL WMEDKONV USING MED-WMEDAREA                                   
057400       MOVE MED-MFSFEL       TO MOD-TEMFSFEL                              
057500       MOVE NEJ             TO ALLT-SW                                    
057600     ELSE                                                                 
057700       MOVE +1              TO DC-IX                                      
057800       PERFORM IMS-GNP-WDK711                                             
057900       PERFORM UNTIL SEGMENT-SAKNAS                                       
058000         IF SLAG-IDDC = WS-IDDC                                           
058100            PERFORM FBA-CDCINFO-TILL-MOD                                  
058200         ELSE                                                             
058300           MOVE SLAG-IDDC TO BAS-WS-IDDC                                  
058400                                                                          
058500* * GENERAL CASE FOR INDIVIDUAL DCS. FOR EXAMPLE:                         
058600* * 1F, 52, 53, 81, 85, 86, 87, 91, 92                                    
058700           IF  (WS-IDDC-1   = BAS-WS-IDDC)                                
058800                                                                          
058900* * DC-GROUPS THAT ARE EXCEPTIONS TO THE GENERAL CASE:                    
059000* * 1A, 1B, 1C, 1D, 1E, 3J                                                
059100            OR ((WS-IDDC-1   = WC-LDC-SE-1A OR WC-LDC-SE-1B               
059200                            OR WC-LDC-SE-1C OR WC-LDC-SE-1D               
059300                            OR WC-LDC-SE-1E OR WC-LDC-NO-3J) AND          
059400                (BAS-WS-IDDC = WC-LDC-SE-1A OR WC-LDC-SE-1B               
059500                            OR WC-LDC-SE-1C OR WC-LDC-SE-1D               
059600                            OR WC-LDC-SE-1E OR WC-LDC-NO-3J))             
059700                                                                          
059800* *  1G, 1K                                                               
059900            OR ((WS-IDDC-1   = WC-LDC-SE-1G                               
060000                            OR WC-LDC-SE-1K ) AND                         
060100                (BAS-WS-IDDC = WC-LDC-SE-1G                               
060200                            OR WC-LDC-SE-1K ))                            
060300                                                                          
060400* * 2C, 2H, 3A, 3B                                                        
060500           OR  ((WS-IDDC-1   = WC-LDC-GB-2C OR WC-LDC-GB-2H               
060600                            OR WC-LDC-GB-3A OR WC-LDC-GB-3B) AND          
060700                (BAS-WS-IDDC = WC-LDC-GB-2C OR WC-LDC-GB-2H               
060800                            OR WC-LDC-GB-3A OR WC-LDC-GB-3B))             
060900                                                                          
061000* * 2J, 3E, 3K, 3T, 3S                                                    
061100           OR  ((WS-IDDC-1   = WC-LDC-DE-2J OR WC-LDC-DE-3E               
061200                            OR WC-LDC-DE-3K OR WC-LDC-DE-3T               
061300                            OR WC-LDC-PL-3S)                 AND          
061400                (BAS-WS-IDDC = WC-LDC-DE-2J OR WC-LDC-DE-3E               
061500                            OR WC-LDC-DE-3K OR WC-LDC-DE-3T               
061600                            OR WC-LDC-PL-3S))                             
061700                                                                          
061800* * 2I, 2L, 3C, 3G, 3M                                                    
061900           OR  ((WS-IDDC-1   = WC-LDC-DE-2I OR WC-LDC-DE-2L               
062000                            OR WC-LDC-DE-3C OR WC-LDC-DE-3G               
062100                            OR WC-LDC-DE-3M)                 AND          
062200                (BAS-WS-IDDC = WC-LDC-DE-2I OR WC-LDC-DE-2L               
062300                            OR WC-LDC-DE-3C OR WC-LDC-DE-3G               
062400                            OR WC-LDC-DE-3M))                             
062500                                                                          
062600* * 24, 26, 3O                                                            
062700           OR  ((WS-IDDC-1   = WC-SDC-ES    OR WC-SDC-AT                  
062800                            OR WC-LDC-FI-3O) AND                          
062900                (BAS-WS-IDDC = WC-SDC-ES    OR WC-SDC-AT                  
063000                            OR WC-LDC-FI-3O))                             
063100                                                                          
063200* * 2M, 21, 3L, 3N, 3P, 3R                                                
063300           OR  ((WS-IDDC-1   = WC-LDC-NL-2M OR WC-SDC-NL                  
063400                            OR WC-LDC-BE-3L OR WC-LDC-NL-3N               
063500                            OR WC-LDC-FR-3P OR WC-LDC-NL-3R) AND          
063600                (BAS-WS-IDDC = WC-LDC-NL-2M OR WC-SDC-NL                  
063700                            OR WC-LDC-BE-3L OR WC-LDC-NL-3N               
063800                            OR WC-LDC-FR-3P OR WC-LDC-NL-3R))             
063900                                                                          
064000* * 25, 3D, 3F, 3H                                                        
064100           OR  ((WS-IDDC-1   = WC-SDC-IT    OR WC-LDC-IT-3D               
064200                            OR WC-LDC-IT-3F OR WC-LDC-CH-3H) AND          
064300                (BAS-WS-IDDC = WC-SDC-IT    OR WC-LDC-IT-3D               
064400                            OR WC-LDC-IT-3F OR WC-LDC-CH-3H))             
064500                                                                          
064602* * 41, 43, 45, 46, 47, 51                                                
064700           OR  ((WS-IDDC-1   = WC-NDC-US-RU OR WC-NDC-US-LA               
064800                            OR WC-NDC-CA    OR WC-NDC-US-CH               
064902                            OR WC-NDC-US-JA OR WC-NDC-US-DA) AND          
065100                (BAS-WS-IDDC = WC-NDC-US-RU OR WC-NDC-US-LA               
066000                            OR WC-NDC-CA    OR WC-NDC-US-CH               
066102                            OR WC-NDC-US-JA OR WC-NDC-US-DA))             
066402* * 44                                                                    
066502           OR  ( WS-IDDC-1   = WC-NDC-US-SE AND                           
066802                 BAS-WS-IDDC = WC-NDC-US-SE)                              
066803                                                                          
066804* * 52, 53                                                                
066805           OR  ((WS-IDDC-1   = WC-NDC-BR    OR WC-NDC-MX   ) AND          
066808                (BAS-WS-IDDC = WC-NDC-BR    OR WC-NDC-MX   ))             
067302                                                                          
067402* * 71, 72, 73, 7A, 7B, 7C                                                
067502           OR  ((WS-IDDC-1   = WC-NDC-CN-71 OR WC-NDC-CN-72               
067602                            OR WC-NDC-CN-73 OR WC-NDC-CN-74               
067702                            OR WC-LDC-CN-7A OR WC-LDC-CN-7B) AND          
067802                (BAS-WS-IDDC = WC-NDC-CN-71 OR WC-NDC-CN-72               
067902                            OR WC-NDC-CN-73 OR WC-NDC-CN-74               
068002                            OR WC-LDC-CN-7A OR WC-LDC-CN-7B))             
068102                                                                          
068202* * 7D, 7E, 7F, 7G, 7H                                                    
068302           OR  ((WS-IDDC-1   = WC-LDC-CN-7C OR WC-LDC-CN-7D               
068402                            OR WC-LDC-CN-7E OR WC-LDC-CN-7F               
068502                            OR WC-LDC-CN-7G OR WC-LDC-CN-7H) AND          
068602                (BAS-WS-IDDC = WC-LDC-CN-7C OR WC-LDC-CN-7D               
068702                            OR WC-LDC-CN-7E OR WC-LDC-CN-7F               
068802                            OR WC-LDC-CN-7G OR WC-LDC-CN-7H))             
068902                                                                          
069002* * 6A, 61, 62, 67                                                        
069102           OR  ((WS-IDDC-1   = WC-NDC-JP-61 OR WC-NDC-JP-6A               
069202                            OR WC-NDC-AU    OR WC-NDC-IN) AND             
069302                (BAS-WS-IDDC = WC-NDC-JP-61 OR WC-NDC-JP-6A               
069402                            OR WC-NDC-AU    OR WC-NDC-IN))                
069502                                                                          
069602* * 63, 64, 65, 66                                                        
069702           OR  ((WS-IDDC-1   = WC-NDC-TH OR WC-NDC-TW                     
069802                            OR WC-NDC-KR OR WC-NDC-MY) AND                
069902                (BAS-WS-IDDC = WC-NDC-TH OR WC-NDC-TW                     
070002                            OR WC-NDC-KR OR WC-NDC-MY))                   
070003                                                                          
070004* * 93                                                                    
070005           OR  ((WS-IDDC-1   = WC-NDC-TH-93                ) AND          
070006                (BAS-WS-IDDC = WC-NDC-TH-93                ))             
070102                                                                          
070103* * SOUTH AFRICA DC 85                                                    
070104           OR  ((WS-IDDC-1   = WC-NDC-ZA                   ) AND          
070105                (BAS-WS-IDDC = WC-NDC-ZA                   ))             
070106                                                                          
070202              IF DC-IX           < 7                                      
070302                PERFORM FBB-NDCINFO-TILL-MOD                              
070402                ADD +1           TO DC-IX                                 
070502              END-IF                                                      
070602           END-IF                                                         
070702         END-IF                                                           
070802         PERFORM IMS-GNP-WDK711                                           
070902       END-PERFORM                                                        
071002       PERFORM UNTIL DC-IX > 6                                            
071102         PERFORM MFS-RENSA-FAELT-NDC-KOL                                  
071202         ADD +1             TO DC-IX                                      
071302       END-PERFORM                                                        
071402     END-IF                                                               
071502     .                                                                    
071602     EJECT                                                                
071702 FBA-CDCINFO-TILL-MOD SECTION.                                            
071802                                                                          
071902     MOVE ZERO              TO WS-KVDISP-SLAG                             
072002                               WS-KVOKS-TOT-NDC                           
072102                               WS-KVROS                                   
072202     COMPUTE WS-KVDISP-SLAG =                                             
072302             SLAG-KVLS - (SLAG-KVOKS-DAG + SLAG-KVOKS-BULK)               
072402                       - SLAG-KVRESS                                      
072502                                                                          
072602     COMPUTE WS-KVOKS-TOT-NDC =                                           
072702                        SLAG-KVOKS-DAG + SLAG-KVOKS-BULK                  
072802     COMPUTE WS-KVROS =                                                   
072902             SLAG-KVROS-BULK + SLAG-KVROS-DAG                             
073002                                                                          
073102     MOVE SLAG-IDDC           TO MOD-IDDC-CDC                             
073202     MOVE SLAG-KVLS           TO MOD-KVLS-CDC                             
073302     MOVE WS-KVDISP-SLAG      TO MOD-KVDISP-CDC                           
073402     MOVE SLAG-KVUTRS         TO MOD-KVUTRS-CDC                           
073502     MOVE WS-KVOKS-TOT-NDC    TO MOD-KVOKS-CDC                            
073602     MOVE SLAG-KVRESS         TO MOD-KVRESS-CDC                           
073702     MOVE WS-KVROS            TO MOD-KVROS-CDC                            
073802     MOVE SLAG-KVEFRS         TO MOD-KVEFRS-CDC                           
073902     MOVE SLAG-KVAKS-SDC      TO MOD-KVAKS-CDC                            
074002     MOVE SLAG-KVAKS-PAV      TO MOD-KVAKS-PAV-CDC                        
074102     MOVE SLAG-KVBEART        TO MOD-KVBEART-CDC                          
074202     MOVE SLAG-KDLEVSP        TO MOD-KDLEVSP-CDC                          
074302     MOVE SLAG-KVSPARR-KVAL   TO MOD-KVSPARR-KVAL-CDC                     
074402     .                                                                    
074502     EJECT                                                                
074602 FBB-NDCINFO-TILL-MOD SECTION.                                            
074702                                                                          
074802     MOVE ZERO              TO WS-KVDISP-SLAG                             
074902                               WS-KVOKS-TOT-NDC                           
075002                               WS-KVROS                                   
075102     COMPUTE WS-KVDISP-SLAG =                                             
075202             SLAG-KVLS - (SLAG-KVOKS-DAG + SLAG-KVOKS-BULK)               
075302                       - SLAG-KVRESS                                      
075402                                                                          
075502     COMPUTE WS-KVOKS-TOT-NDC =                                           
075602                        SLAG-KVOKS-DAG + SLAG-KVOKS-BULK                  
075702     COMPUTE WS-KVROS =                                                   
075802             SLAG-KVROS-BULK + SLAG-KVROS-DAG                             
075902                                                                          
076002     IF BAS-NDC                                                           
076102       PERFORM S08-SEARCH-IDLAND                                          
076202       PERFORM IMS-GU-WDK712                                              
076302     ELSE                                                                 
076402**GIVES SEGMENT-SAKNAS SO THAT PGM USES LOGIC FOR POPULATING              
076502**WS-FLREFERAL CORRECT                                                    
076602       MOVE 'GE'    TO STATUS-WS                                          
076702     END-IF                                                               
076802                                                                          
076902     IF SLAG-FLORDSP-EJRO = JA                                            
077002       MOVE 'D'              TO MOD-BLOCKCODE-NDC (DC-IX)                 
077102     ELSE                                                                 
077202       IF BAS-NDC-US OR BAS-NDC-CA                                        
077302          IF SEGMENT-FINNS                                                
077402             IF LART-DAPUBL > DAGENS-DATUM-SEKEL                          
077502                MOVE 'W'     TO MOD-BLOCKCODE-NDC (DC-IX)                 
077602             ELSE                                                         
077702                MOVE '-'     TO MOD-BLOCKCODE-NDC (DC-IX)                 
077802             END-IF                                                       
077902          ELSE                                                            
078002             MOVE '-'        TO MOD-BLOCKCODE-NDC (DC-IX)                 
078102          END-IF                                                          
078202       ELSE                                                               
078302         MOVE '-'            TO MOD-BLOCKCODE-NDC (DC-IX)                 
078402       END-IF                                                             
078502     END-IF                                                               
078602                                                                          
078702     IF SEGMENT-FINNS                                                     
078802       MOVE LART-FLREFERAL     TO WS-FLREFERAL                            
078902     ELSE                                                                 
079002       MOVE NEJ                TO WS-FLREFERAL                            
079102     END-IF                                                               
079202     IF SLAG-FLORDSP = JA                                                 
079302       IF WS-FLREFERAL = JA                                               
079402         MOVE 'T'            TO MOD-FREEZECODE-NDC (DC-IX)                
079502       ELSE                                                               
079602         MOVE 'F'            TO MOD-FREEZECODE-NDC (DC-IX)                
079702       END-IF                                                             
079802     ELSE                                                                 
079902       IF SLAG-FLSPBULK = JA                                              
080002         IF WS-FLREFERAL = JA                                             
080102           MOVE 'T'          TO MOD-FREEZECODE-NDC (DC-IX)                
080202         ELSE                                                             
080302           MOVE 'P'          TO MOD-FREEZECODE-NDC (DC-IX)                
080402         END-IF                                                           
080502       ELSE                                                               
080602         IF WS-FLREFERAL = JA                                             
080702           MOVE 'R'          TO MOD-FREEZECODE-NDC (DC-IX)                
080802         ELSE                                                             
080902           MOVE '-'          TO MOD-FREEZECODE-NDC (DC-IX)                
081002         END-IF                                                           
081102       END-IF                                                             
081202     END-IF                                                               
081302                                                                          
081402     MOVE SLAG-IDDC         TO MOD-IDDC-NDC(DC-IX)                        
081502     MOVE SLAG-KVLS         TO MOD-KVLS-NDC(DC-IX)                        
081602     MOVE WS-KVDISP-SLAG    TO MOD-KVDISP-NDC(DC-IX)                      
081702     MOVE SLAG-KVUTRS       TO MOD-KVUTRS-NDC(DC-IX)                      
081802     MOVE WS-KVOKS-TOT-NDC  TO MOD-KVOKS-NDC(DC-IX)                       
081902     MOVE SLAG-KVEFRS       TO MOD-KVEFRS-NDC(DC-IX)                      
082002     MOVE SLAG-KVRESS       TO MOD-KVRESS-NDC(DC-IX)                      
082102     MOVE WS-KVROS          TO MOD-KVROS-NDC(DC-IX)                       
082202     MOVE SLAG-KVAKS-SDC    TO MOD-KVAKS-NDC(DC-IX)                       
082302     MOVE SLAG-KVAKS-PAV    TO MOD-KVAKS-PAV-NDC(DC-IX)                   
082402     MOVE SLAG-KVBEART      TO MOD-KVBEART(DC-IX)                         
082502     MOVE SLAG-KDLEVSP      TO MOD-KDLEVSP-NDC(DC-IX)                     
082602     MOVE SLAG-KVSPARR-KVAL TO MOD-KVSPARR-KVAL-NDC(DC-IX)                
082702     .                                                                    
082802     SKIP2                                                                
082902 FC-LAES-WDD3 SECTION.                                                    
083002                                                                          
083102     PERFORM IMS-GU-WDD301-BSEQ                                           
083202     IF SEGMENT-FINNS                                                     
083302       IF NDC-NA                                                          
083402         MOVE 'USA'            TO W-IDSKYLT                               
083502       END-IF                                                             
083602       PERFORM IMS-GNP-WDD311                                             
083702       IF SEGMENT-FINNS                                                   
083802         MOVE TEXT-BEART TO MOD-BEART                                     
083902       ELSE                                                               
084002         MOVE MFS-RENSA-FAELT TO MOD-BEART                                
084102       END-IF                                                             
084202     ELSE                                                                 
084302       MOVE MFS-RENSA-FAELT   TO MOD-BEART                                
084402     END-IF                                                               
084502     .                                                                    
084602     EJECT                                                                
084702 FD-MOVE-MOD-REPL SECTION.                                                
084802     MOVE MFS-RENSA-FAELT                                                 
084902                       TO MOD-REPLACES                                    
085002     IF ART-FLERS = JA                                                    
085102        MOVE ART-IDARTNR                                                  
085202                       TO W-IDARTNR-MIN7                                  
085302                          W-IDARTNR-MAX7                                  
085402        PERFORM IMS-GU-WDD7A1-MINMAX                                      
085502        IF SEGMENT-FINNS                                                  
085602           IF WDD7A1-ERS-IDARTNR NOT = ZERO                               
085702              MOVE WDD7A1-ERS-IDARTNR                                     
085802                       TO MOD-REPLACES                                    
085902              INSPECT MOD-REPLACES REPLACING                              
086002                                    LEADING ZERO BY SPACE                 
086102              PERFORM IMS-GN-WDD7A1-MINMAX                                
086202              IF SEGMENT-FINNS                                            
086302              AND WDD7A1-ERS-IDARTNR NOT = ZERO                           
086402                MOVE 'VARIOUS'                                            
086502                       TO MOD-REPLACES                                    
086602              END-IF                                                      
086702           END-IF                                                         
086802        END-IF                                                            
086902     END-IF                                                               
087002     .                                                                    
087102     EJECT                                                                
087202 FE-LAES-WDK7-UNIK  SECTION.                                              
087302                                                                          
087402**DC11 REDAN FLYTTAT TILL W-IDDC I FAC- SECTION                           
087502                                                                          
087602     PERFORM IMS-GU-WDK711                                                
087702     IF SEGMENT-SAKNAS                                                    
087802       MOVE ARTIKEL-SAKNAS-NDC TO MED-IDMFSFEL                            
087902       CALL WMEDKONV USING MED-WMEDAREA                                   
088002       MOVE MED-MFSFEL       TO MOD-TEMFSFEL                              
088102       MOVE NEJ             TO ALLT-SW                                    
088202     ELSE                                                                 
088302       PERFORM FBA-CDCINFO-TILL-MOD                                       
088402     END-IF                                                               
088502                                                                          
088602     .                                                                    
088702     EJECT                                                                
088802                                                                          
088902 S08-SEARCH-IDLAND SECTION.                                               
089002                                                                          
089102     SEARCH ALL DC-LAND                                                   
089202       AT END                                                             
089302         MOVE SPACE          TO W-IDLAND                                  
089402       WHEN DCLAND-IDDC (DCLAND-IX) = SLAG-IDDC                           
089502         MOVE DCLAND-IDLANDX2(DCLAND-IX) TO W-IDLAND                      
089602     END-SEARCH                                                           
089702     .                                                                    
089802     EJECT                                                                
089902                                                                          
090002 MFS-RENSA-FAELT-UT SECTION.                                              
090102                                                                          
090202     PERFORM MFS-RENSA-CDC                                                
090302     PERFORM MFS-RENSA-NDC                                                
090402     .                                                                    
090502     SKIP2                                                                
090602 MFS-RENSA-CDC SECTION.                                                   
090702                                                                          
090802     MOVE MFS-RENSA-FAELT     TO                                          
090902                                 MOD-KDPRODSL                             
091002                                 MOD-KVQPACK-1                            
091102                                 MOD-KDERS                                
091202                                 MOD-IDDC-CDC                             
091302                                 MOD-KVLS-CDC                             
091402                                 MOD-KVDISP-CDC                           
091502                                 MOD-KVUTRS-CDC                           
091602                                 MOD-KVOKS-CDC                            
091702                                 MOD-KVRESS-CDC                           
091802                                 MOD-KVEFRS-CDC                           
091902                                 MOD-KVAKS-CDC                            
092002                                 MOD-KVAKS-PAV-CDC                        
092102                                 MOD-KDLEVSP-CDC                          
092202                                 MOD-KDPSLLOC                             
092302     .                                                                    
092402     EJECT                                                                
092502 MFS-RENSA-NDC    SECTION.                                                
092602                                                                          
092702     MOVE +1                  TO DC-IX                                    
092802     PERFORM UNTIL DC-IX      >  6                                        
092902       PERFORM MFS-RENSA-FAELT-NDC-KOL                                    
093002       ADD +1                 TO DC-IX                                    
093102     END-PERFORM                                                          
093202     .                                                                    
093302     SKIP2                                                                
093402 MFS-RENSA-FAELT-NDC-KOL SECTION.                                         
093502                                                                          
093602     MOVE MFS-RENSA-FAELT   TO MOD-IDDC-NDC(DC-IX)                        
093702                               MOD-KVLS-NDC(DC-IX)                        
093802                               MOD-KVDISP-NDC(DC-IX)                      
093902                               MOD-KVUTRS-NDC(DC-IX)                      
094002                               MOD-KVOKS-NDC(DC-IX)                       
094102                               MOD-KVEFRS-NDC(DC-IX)                      
094202                               MOD-KVAKS-NDC(DC-IX)                       
094302                               MOD-KVAKS-PAV-NDC(DC-IX)                   
094402                               MOD-KVBEART(DC-IX)                         
094502                               MOD-KDLEVSP-NDC(DC-IX)                     
094602                               MOD-FREEZECODE-NDC (DC-IX)                 
094702     .                                                                    
094802     EJECT                                                                
094902 MFS-RENSA-FAELT-CDC-KOL SECTION.                                         
095002                                                                          
095102     MOVE MFS-RENSA-FAELT   TO MOD-IDDC-NDC(1)                            
095202                               MOD-KVLS-NDC(1)                            
095302                               MOD-KVDISP-NDC(1)                          
095402                               MOD-KVUTRS-NDC(1)                          
095502                               MOD-KVOKS-NDC(1)                           
095602                               MOD-KVRESS-NDC(1)                          
095702                               MOD-KVROS-NDC(1)                           
095802                               MOD-KVEFRS-NDC(1)                          
095902                               MOD-KVAKS-NDC(1)                           
096002                               MOD-KVAKS-PAV-NDC(1)                       
096102                               MOD-KVBEART(1)                             
096202                               MOD-KDLEVSP-NDC(1)                         
096302                               MOD-FREEZECODE-NDC (1)                     
096402                               MOD-KVSPARR-KVAL-NDC(1)                    
096502     .                                                                    
096602     EJECT                                                                
096702* --- IMS SEKTIONER ---                                                   
096802     SKIP3                                                                
096902 IMS-GET-MSG SECTION.                                                     
097002                                                                          
097102     MOVE '  QC' TO GODK-STATUSKODER                                      
097202     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
097302     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
097402     PERFORM IMS-STATUSKONTROLL                                           
097502     .                                                                    
097602     SKIP3                                                                
097702 IMS-INSERT-MSG SECTION.                                                  
097802                                                                          
097902     IF ENGLISH-TEXT                                                      
098002       MOVE 'N' TO MFS-KDHUVOMR                                           
098102     END-IF                                                               
098202     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
098302     MOVE SPACE TO GODK-STATUSKODER                                       
098402     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
098502     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
098602     PERFORM IMS-STATUSKONTROLL                                           
098702     .                                                                    
098802     EJECT                                                                
098902 IMS-GU-WDK601   SECTION.                                                 
099002                                                                          
099102     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
099202          DELIMITED BY SIZE INTO SSA1                                     
099302     MOVE '  GE' TO GODK-STATUSKODER                                      
099402     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
099502     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
099602     PERFORM IMS-STATUSKONTROLL                                           
099702     .                                                                    
099802     EJECT                                                                
099902 IMS-GNP-WDK611     SECTION.                                              
100002     MOVE 'WDK611  ' TO SSA1                                              
100102     MOVE '  GE' TO GODK-STATUSKODER                                      
100202     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
100302     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
100402     PERFORM IMS-STATUSKONTROLL                                           
100502     .                                                                    
100602     EJECT                                                                
100702 IMS-GU-WDK9     SECTION.                                                 
100802     STRING 'WDK901  (IDARTNR  =' W-IDARTNR-X ')'                         
100902          DELIMITED BY SIZE INTO SSA1                                     
101002     MOVE '  GE' TO GODK-STATUSKODER                                      
101102     CALL CBLTDLI USING GU WDK9-PCB DLI-IO-WDK901 SSA1                    
101202     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
101302     PERFORM IMS-STATUSKONTROLL                                           
101402     .                                                                    
101502     EJECT                                                                
101602 IMS-GU-WDK701    SECTION.                                                
101702     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
101802          DELIMITED BY SIZE INTO SSA1                                     
101902     MOVE '  GE' TO GODK-STATUSKODER                                      
102002     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
102102     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
102202     PERFORM IMS-STATUSKONTROLL                                           
102302     .                                                                    
102402     EJECT                                                                
102502 IMS-GU-WDK711 SECTION.                                                   
102602     MOVE SPACES  TO SSA1                                                 
102702     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
102802          DELIMITED BY SIZE INTO SSA1                                     
102902     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
103002          DELIMITED BY SIZE INTO SSA2                                     
103102     MOVE '  GE' TO GODK-STATUSKODER                                      
103202     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
103302     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
103402     PERFORM IMS-STATUSKONTROLL                                           
103502     .                                                                    
103602     EJECT                                                                
103702 IMS-GNP-WDK711 SECTION.                                                  
103802     MOVE SPACES  TO SSA1                                                 
103902     STRING 'WDK711'                                                      
104002          DELIMITED BY SIZE INTO SSA1                                     
104102     MOVE '  GE' TO GODK-STATUSKODER                                      
104202     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
104302     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
104402     PERFORM IMS-STATUSKONTROLL                                           
104502     .                                                                    
104602     EJECT                                                                
104702 IMS-GU-WDK712 SECTION.                                                   
104802     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
104902          DELIMITED BY SIZE INTO SSA1                                     
105002     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
105102          DELIMITED BY SIZE INTO SSA2                                     
105202     MOVE '  GE' TO GODK-STATUSKODER                                      
105302     CALL CBLTDLI USING GU WDK72-PCB DLI-IO-WDK712 SSA1 SSA2              
105402     MOVE WDK72-STATUS-CODE TO STATUS-WS                                  
105502     PERFORM IMS-STATUSKONTROLL                                           
105602     .                                                                    
105702     EJECT                                                                
105802 IMS-GU-WDD301-BSEQ SECTION.                                              
105902     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
106002          DELIMITED BY SIZE INTO SSA1                                     
106102     MOVE '  GE' TO GODK-STATUSKODER                                      
106202     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1                    
106302     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
106402     PERFORM IMS-STATUSKONTROLL                                           
106502     .                                                                    
106602 IMS-GNP-WDD311 SECTION.                                                  
106702     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
106802          DELIMITED BY SIZE INTO SSA1                                     
106902     MOVE '  GE' TO GODK-STATUSKODER                                      
107002     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1                    
107102     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
107202     PERFORM IMS-STATUSKONTROLL                                           
107302     .                                                                    
107402     EJECT                                                                
107502 IMS-GU-WDD701      SECTION.                                              
107602     STRING 'WDD701  (IDARTNR  =' W-IDARTNR-X ')'                         
107702            DELIMITED BY SIZE INTO SSA1                                   
107802     MOVE '  GE' TO GODK-STATUSKODER                                      
107902     CALL CBLTDLI USING GU WDD7-PCB DLI-IO-AREA-WDD701 SSA1               
108002     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
108102     PERFORM IMS-STATUSKONTROLL                                           
108202     .                                                                    
108302     SKIP2                                                                
108402 IMS-GNP-WDD702      SECTION.                                             
108502     STRING 'WDD702  (FLTEXT   =N)'                                       
108602            DELIMITED BY SIZE INTO SSA1                                   
108702     MOVE '  GE' TO GODK-STATUSKODER                                      
108802     CALL CBLTDLI USING GNP WDD7-PCB DLI-IO-AREA-WDD702 SSA1              
108902     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
109002     PERFORM IMS-STATUSKONTROLL                                           
109102     .                                                                    
109202 IMS-GN-WDD7A1-MINMAX      SECTION.                                       
109302     STRING 'WDD7A1  (WDD7A1KY=>' W-WDD7A1KY-MIN                          
109402                    '&WDD7A1KY=<' W-WDD7A1KY-MAX ')'                      
109502            DELIMITED BY SIZE INTO SSA1                                   
109602     MOVE '  GEGB' TO GODK-STATUSKODER                                    
109702     CALL CBLTDLI USING GN WDD7A-PCB DLI-IO-AREA-WDD7A1 SSA1              
109802     MOVE WDD7A-STATUS-CODE TO STATUS-WS                                  
109902     PERFORM IMS-STATUSKONTROLL                                           
110002     .                                                                    
110102     EJECT                                                                
110202 IMS-GU-WDD7A1-MINMAX      SECTION.                                       
110302     STRING 'WDD7A1  (WDD7A1KY=>' W-WDD7A1KY-MIN                          
110402                    '&WDD7A1KY=<' W-WDD7A1KY-MAX ')'                      
110502            DELIMITED BY SIZE INTO SSA1                                   
110602     MOVE '  GE' TO GODK-STATUSKODER                                      
110702     CALL CBLTDLI USING GU WDD7A-PCB DLI-IO-AREA-WDD7A1 SSA1              
110802     MOVE WDD7A-STATUS-CODE TO STATUS-WS                                  
110902     PERFORM IMS-STATUSKONTROLL                                           
111002     .                                                                    
111102     EJECT                                                                
111202 IMS-GU-WDB601 SECTION.                                                   
111302     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
111402            DELIMITED BY SIZE INTO SSA1                                   
111502     MOVE '  GE' TO GODK-STATUSKODER                                      
111602     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-WDB601 SSA1               
111702     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
111802     PERFORM IMS-STATUSKONTROLL                                           
111902     .                                                                    
112002     EJECT                                                                
112102 IMS-STATUSKONTROLL SECTION.                                              
112202                                                                          
112302     SET STATUS-IX TO 1                                                   
112402     SEARCH GODK-STATUS                                                   
112502       AT END                                                             
112602         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
112702         DELIMITED BY SIZE INTO FELTEXT                                   
112802         CALL FELLOG                                                      
112902       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
113002         CONTINUE                                                         
113102     END-SEARCH                                                           
114002     .                                                                    
