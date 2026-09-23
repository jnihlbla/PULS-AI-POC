000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4037100.                                                
000400 AUTHOR.         ROGER OLSSON.                                            
000500 DATE-WRITTEN.   90/11/09.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET LÄSER OCH KOLLAR OM EN SATS ÄR BYGGBAR.               
001100*        SVARAR SKÄRMEN FRÅN PGM W4035400, SAMT STARTAR UPP               
001200*        UTSKRIFT OM ALLT OK.                                             
001300*                                                                         
001400*        PROGRAMMET LÄSER      WLSATG (WDJ2)                              
001400*                              WDD8                                       
001500*                                                                         
001600*        PROGRAMMET UPPDATERAR WLSATG (WDJ2)                              
001700*                              WLARTC (WDK6)                              
001800*                              WLLOGA (WDL9)                              
001900*    INDATA.                                                              
002000*        TRANSAKTION: W4T371X                                             
002100*        MID:         SAMMA MID SOM PGM W4035400(W4I35401)                
002200*                                                                         
002300*    UTDATA.                                                              
002400*        MOD:         W4O37101                                            
002500*        TRANSAKTION: W4T372X (UTSKRIFT AV ETIKETTER)                     
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100 WORKING-STORAGE SECTION.                                                 
003200                                                                          
003300*    -- CHECKED BY WY2000                                                 
003400 77  IDPGM                       PIC X(08)   VALUE 'W4037100'.            
003500                                                                          
003600*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003700 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003800                                                                          
003900 77  JA                          PIC X       VALUE 'J'.                   
004000 77  NEJ                         PIC X       VALUE 'N'.                   
004100                                                                          
004200 77  INDX                        PIC S9(9)  VALUE +0    COMP SYNC.        
004300 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004400 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +122  COMP SYNC.        
004500                                                                          
004600 77  WS-BRIST                    PIC S9(3)V9(2)         COMP-3.           
004700                                                                          
004800*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004900                                                                          
005000 77  WS-IDUSER                   PIC X(8).                                
005100                                                                          
005200 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005300     88  INDATA-OK                           VALUE 'J'.                   
005400     88  INDATA-FEL                          VALUE 'N'.                   
005500                                                                          
005600 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005700     88  NYCKLAR-OK                          VALUE 'J'.                   
005800     88  NYCKLAR-FEL                         VALUE 'N'.                   
005900                                                                          
006000 77  ALLT-SW                     PIC X       VALUE 'J'.                   
006100     88  ALLT-OK                             VALUE 'J'.                   
006200                                                                          
006300 77  BYGGBAR-SW                  PIC X       VALUE 'N'.                   
006400     88  SATS-BYGGBAR                        VALUE 'J'.                   
006500     88  SATS-EJ-BYGGBAR                     VALUE 'N'.                   
006600                                                                          
006700 77  CLAGER-SW                   PIC X       VALUE 'N'.                   
006800     88  CLAGER-SPAERR                       VALUE 'J'.                   
006900                                                                          
007000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007100     88  EGEN-MID                            VALUE '4354'.                
007200     88  GODK-MID                            VALUE '4354'.                
007300     EJECT                                                                
007400*      --- VALID IDDC CODES                                               
007500*                                                                         
007600*01    -COPY WWDCKONS                                                     
007700       EJECT                                                              
007800                                                                          
007900*    ---GENERELLA ARBETSFÄLT                                              
008000 01  DAGENS-DATUM                PIC 9(8).                                
008100 01  WS-TID                      PIC 9(9).                                
008200 01  WS-SPAR.                                                             
008300     03  WS-SPAR-IDDISTR         PIC S9(5)   COMP-3 VALUE ZERO.           
008400     03  WS-SPAR-IDKUNDNR        PIC S9(7)   COMP-3 VALUE ZERO.           
008500     03  WS-SPAR-IDORDNR5        PIC S9(5)   COMP-3 VALUE ZERO.           
008600     03  WS-SHUV-BEFT            PIC S9(3)   COMP-3 VALUE ZERO.           
008700                                                                          
008800 01  WS-RED-IDORDNR.                                                      
008900     03  WS-IDORDNR-4            PIC 9(4)           VALUE ZERO.           
009000     03  WS-IDORDNR-1            PIC 9(1)           VALUE ZERO.           
009100 01  WS-IDORDNR-5                PIC 9(5)           VALUE ZERO.           
009200                                                                          
009300 01 WS-TAG-FRAN-KVLS             PIC 9(7)           VALUE ZERO.           
009500                                                                          
009600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
009700 01  GENERELLA-SUBPROGRAM.                                                
009800     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
009900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010100     EJECT                                                                
010200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
010300*   -COPY WMEDAREA                                                        
010400     SKIP3                                                                
010500 01  MESSAGE-CODES.                                                       
010600     03  INF-PU-QUEUE            PIC X(3)    VALUE '037'.                 
010700     03  INF-ORDER-REPORTED      PIC X(3)    VALUE '703'.                 
010800     03  INF-ORDER-FINISH        PIC X(3)    VALUE '708'.                 
010900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
011000     EJECT                                                                
011100*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
011200*                                                                         
011300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
011400     SKIP3                                                                
011500 01  MID-W4I37101.                                                        
011600*    03   -COPY W4I35401                                                  
011700     EJECT                                                                
011800 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
011900     SKIP3                                                                
012000*01  -COPY WMSGAREA                                                       
012100     EJECT                                                                
012200     03  MOD REDEFINES MSG-AREA.                                          
012300*      05  -COPY W4O37101                                                 
012400     EJECT                                                                
012500 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-SW'.           
012600 01  P-TO-P-SW.                                                           
012700     03  PTOP-LL                 PIC S9(4)   VALUE 23 COMP SYNC.          
012800     03  PTOP-Z1                 PIC  X(1)   VALUE LOW-VALUE.             
012900     03  PTOP-Z2                 PIC  X(1)   VALUE LOW-VALUE.             
013000     03  PTOP-TRANSKOD           PIC  X(7)   VALUE 'W4T372X'.             
013100     03  FILLER                  PIC  X(1)   VALUE SPACE.                 
013200     03  PTOP-BILDNR             PIC  X(4)   VALUE '4371'.                
013300     03  PTOP-KDMFSFOR           PIC  X(1).                               
013400     03  PTOP-IDORDNSB           PIC  X(5).                               
013500     03  PTOP-IDORDNSS           PIC  X(1).                               
013600     EJECT                                                                
013700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
013800     SKIP3                                                                
013900*01  -COPY WMFSAREA                                                       
014000     EJECT                                                                
014100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014200*                                                                         
014300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014400     SKIP3                                                                
014500 01  NYCKLAR-TILL-DLI.                                                    
014600                                                                          
           03    W-IDARTNR-X.                                                   
             05    W-IDARTNR                    PIC S9(9) COMP-3.               
                                                                                
014700*----> DIREKTNYCKEL TILL SATSORDER.                                       
014800                                                                          
014900     03  W-WDJ2-IDORDNST-X.                                               
015000         05 W-WDJ2-IDORDNSB               PIC S9(5) COMP-3.               
015100         05 W-WDJ2-IDORDNSS               PIC S9(1) COMP-3.               
015200                                                                          
015300*----> ARTIKELREGISTER WDK6.                                              
015400                                                                          
015500     03  W-WDK6-IDARTNR-X.                                                
015600         05 W-WDK6-IDARTNR                PIC S9(9)      COMP-3.          
015700                                                                          
015800     03  W-WDK6-KDSEGKEY-X.                                               
015900         05 W-WDK6-KDSEGKEY               PIC X       VALUE '1'.          
016000                                                                          
016100*----> BENÄMNINGSREGISTER WDD3.                                           
016200                                                                          
016300     03  W-WDD3BSEQ-X.                                                    
016400         05 W-WDD3BSEQ-IDARTNR            PIC S9(9)      COMP-3.          
016500                                                                          
016600     03  W-WDD3-IDSKYLT-X                 PIC X(3).                       
016000                                                                          
016100*----> SALDOREGISTER BUFFERTLAGER WDD8.                                   
           03    W-WDD811KY-MIN-X.                                              
             05    W-IDDC-WDD8-MIN   PIC X(2)    VALUE '11'.                    
             05    W-ADBUFFOMR-MIN   PIC S9(3)   VALUE +1     COMP-3.           
             05    W-DABUFPAF-MIN    PIC  9(8)   VALUE ZERO.                    
             05    W-ADBUFFGANG-MIN  PIC S9(3)   VALUE ZERO   COMP-3.           
             05    W-ADBUFFPL-MIN    PIC S9(5)   VALUE ZERO   COMP-3.           
                                                                                
           03    W-WDD811KY-MAX-X.                                              
             05    W-IDDC-WDD8-MAX   PIC X(2)    VALUE '11'.                    
             05    W-ADBUFFOMR-MAX   PIC S9(3)   VALUE +1     COMP-3.           
             05    W-DABUFPAF-MAX    PIC  9(8)   VALUE  99999999.               
             05    W-ADBUFFGANG-MAX  PIC S9(3)   VALUE +99    COMP-3.           
             05    W-ADBUFFPL-MAX    PIC S9(5)   VALUE +99999 COMP-3.           
016700                                                                          
016800*    --- STATUS-KOD FRÅN IMS                                              
016900 01  STATUS-WS                   PIC XX.                                  
017000     88  SEGMENT-FINNS                       VALUE '  '.                  
017100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
017200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017300     88  END-OF-DATA                         VALUE 'GB'.                  
017400     SKIP2                                                                
017500 01  GODK-STATUSKODER.                                                    
017600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017700     SKIP3                                                                
017800 01  SSA1                        PIC X(96).                               
017900 01  SSA2                        PIC X(96).                               
018000 01  SSA3                        PIC X(96).                               
018100     EJECT                                                                
018200*    --- IMS FUNKTIONSKODER                                               
018300*01  -COPY W0003                                                          
018400     EJECT                                                                
018500*    ---  DLI INPUT-OUTPUT AREA                                           
018600 01  FILLER                      PIC X(16) VALUE 'WLLOGA01'.              
018700*01  WLLOGA01  -COPY WDL901                                               
018800     EJECT                                                                
018900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
019000     SKIP3                                                                
019100 01  DLI-IO-AREA1.                                                        
019200     03 IO-AREA1       PIC X(250).                                        
019300     03 WLSATG01 REDEFINES IO-AREA1.                                      
019400*    05  -COPY WDJ201                                                     
019500     EJECT                                                                
019600     03 WLSATG11 REDEFINES IO-AREA1.                                      
019700*    05  -COPY WDJ211                                                     
019800     EJECT                                                                
019900 01  DLI-IO-AREA2.                                                        
020000     03 WLSATG12.                                                         
020100*    05  -COPY WDJ212                                                     
020200     EJECT                                                                
020900 01  DLI-IO-AREA3.                                                        
020910     03 IO-AREA3       PIC X(900).                                        
020920     03 WLARTC11 REDEFINES IO-AREA3.                                      
020930*    05  -COPY WDK611                                                     
020940     EJECT                                                                
020950     03 WLBENA11 REDEFINES IO-AREA3.                                      
020960*    05  -COPY WDD311                                                     
       01  DLI-IO-AREA-811.                                                     
      *    03  -COPY WDD811                                                     
021000     EJECT                                                                
021100 LINKAGE SECTION.                                                         
021200                                                                          
021300*01  -COPY W0008  -PRE WLLOGA-                                            
021400     05  FILLER                  PIC X.                                   
021500                                                                          
021600*01  -COPY W0009      -PRE MSG-                                           
021700     EJECT                                                                
021800*01  -COPY W0009      -PRE ALT-                                           
021900     EJECT                                                                
022000*01  -COPY W0008      -PRE SATG1-                                         
022100     05  FILLER                  PIC X.                                   
022200     EJECT                                                                
022300*01  -COPY W0008      -PRE SATG2-                                         
022400     05  FILLER                  PIC X.                                   
022500     EJECT                                                                
022600*01  -COPY W0008      -PRE ARTC-                                          
022700     05  FILLER                  PIC X.                                   
022800     EJECT                                                                
022900*01  -COPY W0008      -PRE BENA-                                          
023000     05  FILLER                  PIC X.                                   
022800     EJECT                                                                
022900*01  -COPY W0008      -PRE WDD8-                                          
023000     05  FILLER                  PIC X.                                   
023100     EJECT                                                                
023200 PROCEDURE DIVISION  USING MSG-PCB                                        
023300                           ALT-PCB                                        
023400                           SATG1-PCB                                      
023500                           SATG2-PCB                                      
023600                           ARTC-PCB                                       
023700                           BENA-PCB                                       
023800                           WLLOGA-PCB                                     
023800                           WDD8-PCB.                                      
023900                                                                          
024000     ENTRY 'DLITCBL' USING MSG-PCB                                        
024100                           ALT-PCB                                        
024200                           SATG1-PCB                                      
024300                           SATG2-PCB                                      
024400                           ARTC-PCB                                       
024500                           BENA-PCB                                       
024600                           WLLOGA-PCB                                     
024600                           WDD8-PCB.                                      
024700                                                                          
024800     PERFORM IMS-GET-MSG                                                  
024900     IF SEGMENT-FINNS                                                     
025000        PERFORM A-INIT                                                    
025100        PERFORM B-KOLLA-NYCKLAR                                           
025200        IF NYCKLAR-OK                                                     
025300           PERFORM C-BEHANDLA-SATSORDER                                   
025400           IF ALLT-OK                                                     
025500              PERFORM D-STARTA-UTSKRIFT                                   
025600           END-IF                                                         
025700        END-IF                                                            
025800        MOVE MAX-MOD-LAENGD TO MSG-KVLL                                   
025900        PERFORM IMS-INSERT-MSG                                            
026000     END-IF                                                               
026100                                                                          
026200     MOVE ZERO TO RETURN-CODE                                             
026300     GOBACK                                                               
026400     .                                                                    
026500     EJECT                                                                
026600 A-INIT SECTION.                                                          
026700                                                                          
026800     IF MSG-DUBBLA-TRANSKODER                                             
026900        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I37101                
027000        MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                 
027100        MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                
027200     ELSE                                                                 
027300        MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W4I37101                
027400        MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                 
027500        MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                
027600     END-IF                                                               
027700                                                                          
027800     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
027900     MOVE MSG-IDPFK            TO MFS-IDPFK                               
028000     MOVE MFS-IDTRANS          TO W-IDTRANS                               
028100                                                                          
028200     MOVE LOW-VALUE       TO MSG-AREA                                     
028300     MOVE 'W4O35402'      TO MFS-IDMOD                                    
028400     MOVE '4354'          TO MOD-IDTRANS                                  
028500     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
028600                                                                          
028700     IF ENGLISH-TEXT                                                      
028800        MOVE +2    TO SPRAK-IX                                            
028900        MOVE 'GB ' TO MED-IDSKYLT                                         
029000     ELSE                                                                 
029100        MOVE +1    TO SPRAK-IX                                            
029200        MOVE 'S  ' TO MED-IDSKYLT                                         
029300     END-IF                                                               
029400                                                                          
029500     .                                                                    
029600     EJECT                                                                
029700 B-KOLLA-NYCKLAR SECTION.                                                 
029800                                                                          
029900     MOVE JA TO NYCKLAR-SW                                                
030000                                                                          
030100     IF MID-IDUSER-IN = ALL '+'                                           
030200        MOVE MID-IDUSER-UT TO WS-IDUSER                                   
030300     ELSE                                                                 
030400        MOVE MID-IDUSER-IN TO WS-IDUSER                                   
030500     END-IF                                                               
030600                                                                          
030700     INSPECT WS-IDUSER REPLACING LEADING SPACE BY ZERO                    
030800                                                                          
030900     IF (WS-IDUSER NOT NUMERIC)                                           
031000        OR                                                                
031100        (WS-IDUSER (1:3) NOT = ZERO)                                      
031200        MOVE ZERO TO WS-IDUSER                                            
031300     END-IF                                                               
031400                                                                          
031500     IF NOT GODK-MID                                                      
031600        MOVE NEJ TO NYCKLAR-SW                                            
031700     END-IF                                                               
031800                                                                          
031900     IF NYCKLAR-FEL                                                       
032000        MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                
032100        PERFORM S01-FEL-MEDDELANDE                                        
032200     END-IF                                                               
032300     .                                                                    
032400     EJECT                                                                
032500 C-BEHANDLA-SATSORDER SECTION.                                            
032600                                                                          
032700     IF MID-IDORDNSB-BYGGBAR NUMERIC                                      
032800        MOVE MID-IDORDNSB-BYGGBAR TO W-WDJ2-IDORDNSB                      
032900     ELSE                                                                 
033000        MOVE ZERO                 TO W-WDJ2-IDORDNSB                      
033100     END-IF                                                               
033200                                                                          
033300     IF MID-IDORDNSS-BYGGBAR NUMERIC                                      
033400        MOVE MID-IDORDNSS-BYGGBAR TO W-WDJ2-IDORDNSS                      
033500     ELSE                                                                 
033600        MOVE ZERO                 TO W-WDJ2-IDORDNSS                      
033700     END-IF                                                               
033800                                                                          
033900     PERFORM IMS-GHU-WDJ2-WLSATG01                                        
034000                                                                          
034100     IF SEGMENT-FINNS                                                     
034200        IF SHUV-KDSATSTA = 'U'                                            
034300           MOVE NEJ TO ALLT-SW                                            
034400        END-IF                                                            
034500        IF ALLT-OK                                                        
034600           MOVE SHUV-IDDISTR         TO WS-SPAR-IDDISTR                   
034700           MOVE SHUV-IDKUNDNR        TO WS-SPAR-IDKUNDNR                  
034800           MOVE SHUV-IDORDNSB        TO WS-SPAR-IDORDNR5                  
034900           MOVE SHUV-IDORDNSB        TO WS-IDORDNR-4                      
035000           MOVE SHUV-IDORDNSS        TO WS-IDORDNR-1                      
035100           MOVE SHUV-BEFT            TO WS-SHUV-BEFT                      
035200           PERFORM IMS-GNP-WDJ2-WLSATG11                                  
035300           PERFORM UNTIL SEGMENT-SAKNAS                                   
035400              IF (SRAD-REBEART = ZERO AND SRAD-KVSATRES = ZERO)           
035500                 CONTINUE                                                 
035600              ELSE                                                        
035700                 MOVE SRAD-IDARTNR  TO W-WDK6-IDARTNR                     
035800                 PERFORM IMS-GHU-WDK6-WLARTC11                            
035900                 PERFORM CA-KOLLA-BRIST                                   
036000                 PERFORM CB-AVBOKA-SALDO                                  
036100                 PERFORM CC-SKAPA-PRINTRAD                                
036200              END-IF                                                      
036300              PERFORM IMS-GNP-WDJ2-WLSATG11                               
036400           END-PERFORM                                                    
036500           PERFORM CD-UPPDAT-SATSHUVUD                                    
036600        END-IF                                                            
036700     ELSE                                                                 
036800        MOVE NEJ TO ALLT-SW                                               
036900     END-IF                                                               
037000                                                                          
037100     IF ALLT-OK                                                           
037200        MOVE 1 TO INDX                                                    
037300        PERFORM UNTIL INDX > 13                                           
037400           MOVE MFS-RENSA-FAELT TO MOD-KDCMD (INDX)                       
037500           ADD 1 TO INDX                                                  
037600        END-PERFORM                                                       
037700        MOVE INF-PU-QUEUE     TO MED-IDMFSINF                             
037800        PERFORM S02-INFO-MEDDELANDE                                       
037900     ELSE                                                                 
038000        MOVE INF-ORDER-FINISH TO MED-IDMFSINF                             
038100        PERFORM S02-INFO-MEDDELANDE                                       
038200     END-IF                                                               
038300     .                                                                    
038400     EJECT                                                                
038500 CA-KOLLA-BRIST SECTION.                                                  
038600                                                                          
038700     COMPUTE WS-BRIST =                                                   
038800         ((SRAD-KVSATRES / (CLAG-KVLS - CLAG-KVSPANT)) * 100)             
038900         ON SIZE ERROR MOVE 100 TO WS-BRIST                               
039000     END-COMPUTE                                                          
039100                                                                          
039200     IF WS-BRIST >= 90                                                    
039300        MOVE JA  TO URAD-FLSATBRI                                         
039400     ELSE                                                                 
039500        MOVE NEJ TO URAD-FLSATBRI                                         
039600     END-IF                                                               
039700     .                                                                    
039800     EJECT                                                                
039900 CB-AVBOKA-SALDO SECTION.                                                 
040000                                                                          
040300     SUBTRACT SRAD-KVSATRES FROM CLAG-KVLS                                
040400                                 CLAG-KVRESS                              
043200     ADD      SRAD-KVSATRES TO   CLAG-KVEFRS                              
043300                                                                          
043400     PERFORM IMS-REPL-WDK6-WLARTC11                                       
043500     PERFORM CBB-FLYTTA-SALDOLOGG-DATA                                    
043600     .                                                                    
043700     EJECT                                                                
043800 CBB-FLYTTA-SALDOLOGG-DATA SECTION.                                       
043900     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM                      
044000     ACCEPT WS-TID                   FROM TIME                            
044100     COMPUTE LOGG-DAREGDAT-9KOMPL = 99999999 - DAGENS-DATUM               
044200     COMPUTE LOGG-TIKLOCK-9KOMPL  = 999999999 - WS-TID                    
044300     MOVE 9                   TO LOGG-IDSEKVNR                            
044400     MOVE 'OUTB'              TO LOGG-IDHUVTYP                            
044500     MOVE 'KIT'               TO LOGG-IDSUBTYP                            
044600     MOVE 'W4037100'          TO LOGG-IDPGM                               
044700     MOVE '4354'              TO LOGG-IDTRANS                             
044800     MOVE WS-IDUSER           TO LOGG-IDUSER                              
044900     MOVE SPACE               TO LOGG-REF                                 
045000     MOVE WS-SPAR-IDDISTR     TO LOGG-IDDISTR                             
045100     MOVE WS-SPAR-IDKUNDNR    TO LOGG-IDKUNDNR                            
045200*    MOVE WS-SPAR-IDORDNR5    TO LOGG-IDORDNR5                            
045300     MOVE WS-RED-IDORDNR      TO WS-IDORDNR-5                             
045400     MOVE WS-IDORDNR-5        TO LOGG-IDORDNR5                            
045500     MOVE W-WDK6-IDARTNR      TO LOGG-IDARTNR                             
045600     MOVE WC-CDC-SE           TO LOGG-IDDC                                
045700*   ---SALDOFÖRÄNDRINGAR PÅ WDK611                                        
045800*   ---LOGGAS PÅ WDL9 OCH LOGGAR DESSUTOM                                 
045900*   ---SALDOÖKNING ELLER SALDOMINSKNING                                   
046100     MOVE CLAG-KVLS           TO LOGG-KVLS                                
046200     MOVE CLAG-KVEFRS         TO LOGG-KVEFRS                              
046300     MOVE '-'                 TO LOGG-IDTECKEN-KVLS                       
046400     MOVE '+'                 TO LOGG-IDTECKEN-KVEFRS                     
046500     MOVE SPACE               TO LOGG-IDTECKEN-KVAKS                      
046600     MOVE SPACE               TO LOGG-IDTECKEN-KVAKS-PAV                  
046700     MOVE SRAD-KVSATRES       TO LOGG-KVART-SALDO                         
046800     COMPUTE LOGG-KVAKS       =  CLAG-KVAKS-CDC                           
046900                              +  CLAG-KVAKS-T                             
047000     MOVE CLAG-KVAKS-PAV      TO LOGG-KVAKS-PAV                           
047100     MOVE '00000000'          TO LOGG-DAREGDAT-LADD                       
047200     PERFORM S03-ISRT-SALDOLOGG                                           
047300     .                                                                    
047400     EJECT                                                                
047500 CC-SKAPA-PRINTRAD SECTION.                                               
047600                                                                          
047700     MOVE '1'             TO URAD-KDSATLI                                 
047800     MOVE SRAD-IDARTNR    TO URAD-IDARTNR                                 
047900     MOVE SRAD-IDKONTO    TO URAD-IDKONTO                                 
048000     MOVE SRAD-IDANALYS   TO URAD-IDANALYS                                
048100     MOVE SRAD-IDKST      TO URAD-IDKST                                   
048200     MOVE SRAD-KDPRODSL   TO URAD-KDPRODSL                                
048300     MOVE SRAD-KDSATAND   TO URAD-KDSATAND                                
048400     MOVE SRAD-KDSATKMB   TO URAD-KDSATKMB                                
048500     MOVE SRAD-KDSORT     TO URAD-KDSORT                                  
048600     MOVE SRAD-PRARTSTD   TO URAD-PRARTSTD                                
048700     MOVE SRAD-REANTPSA   TO URAD-REANTPSA                                
048800     MOVE SRAD-REBEART    TO URAD-REBEART                                 
048900     MOVE SRAD-REKSIFFR   TO URAD-REKSIFFR                                
049000     MOVE SRAD-VKARTNTO   TO URAD-VKARTNTO                                
049100     MOVE SRAD-VLARTNTO   TO URAD-VLARTNTO                                
049300     IF WS-SHUV-BEFT = +015                                               
049400       MOVE CLAG-ADLAGOMR TO URAD-ADLAGOMR                                
049500       MOVE CLAG-ADGANG   TO URAD-ADGANG                                  
049600       MOVE CLAG-ADPLATS  TO URAD-ADPLATS                                 
049700     ELSE                                                                 
             MOVE SRAD-IDARTNR TO W-IDARTNR                                     
             PERFORM IMS-GU-WDD811                                              
             IF SEGMENT-FINNS                                                   
049800          IF SALDO-KVBUFF-OF > ZERO                                       
049900             MOVE SALDO-ADBUFFOMR     TO URAD-ADLAGOMR                    
050000             MOVE SALDO-ADBUFFGANG    TO URAD-ADGANG                      
050100             MOVE SALDO-ADBUFFPL      TO URAD-ADPLATS                     
050200          ELSE                                                            
049800             IF SALDO-KVBUFF-F >= SRAD-KVSATRES                           
049900                MOVE SALDO-ADBUFFOMR  TO URAD-ADLAGOMR                    
050000                MOVE SALDO-ADBUFFGANG TO URAD-ADGANG                      
050100                MOVE SALDO-ADBUFFPL   TO URAD-ADPLATS                     
                   ELSE                                                         
050300                MOVE CLAG-ADLAGOMR    TO URAD-ADLAGOMR                    
050400                MOVE CLAG-ADGANG      TO URAD-ADGANG                      
050500                MOVE CLAG-ADPLATS     TO URAD-ADPLATS                     
050600             END-IF                                                       
050600          END-IF                                                          
             ELSE                                                               
050300          MOVE CLAG-ADLAGOMR TO URAD-ADLAGOMR                             
050400          MOVE CLAG-ADGANG   TO URAD-ADGANG                               
050500          MOVE CLAG-ADPLATS  TO URAD-ADPLATS                              
050600       END-IF                                                             
050700     END-IF                                                               
050800                                                                          
050900     PERFORM CCB-HAMTA-BENAMNING                                          
051000                                                                          
051100     PERFORM IMS-ISRT-WDJ2-WLSATG12                                       
051200     .                                                                    
051300     EJECT                                                                
051400 CCB-HAMTA-BENAMNING SECTION.                                             
051500                                                                          
051600     MOVE SRAD-IDARTNR TO W-WDD3BSEQ-IDARTNR                              
051700                                                                          
051800     IF SRAD-KDCLAGER = 1                                                 
051900        MOVE 'S  ' TO W-WDD3-IDSKYLT-X                                    
052000     ELSE                                                                 
052100        MOVE 'GB ' TO W-WDD3-IDSKYLT-X                                    
052200     END-IF                                                               
052300                                                                          
052400     PERFORM IMS-GU-WDD3-WLBENA11                                         
052500     IF SEGMENT-FINNS                                                     
052600        MOVE TEXT-BEART       TO URAD-BEART                               
052700     ELSE                                                                 
052800        MOVE 'BENÄMN. SAKNAS' TO URAD-BEART                               
052900     END-IF                                                               
053000     .                                                                    
053100     EJECT                                                                
053200 CD-UPPDAT-SATSHUVUD SECTION.                                             
053300                                                                          
053400     PERFORM IMS-GHU-WDJ2-WLSATG01                                        
053500                                                                          
053600     IF SEGMENT-FINNS                                                     
053700        MOVE 'U'           TO SHUV-KDSATSTA                               
053800        MOVE '2'           TO SHUV-KDSATPLK                               
053900        IF SHUV-IDUSER = SPACE                                            
054000           MOVE WS-IDUSER  TO SHUV-IDUSER                                 
054100        END-IF                                                            
054200        PERFORM IMS-REPL-WDJ2-WLSATG01                                    
054300     END-IF                                                               
054400     .                                                                    
054500     EJECT                                                                
054600 D-STARTA-UTSKRIFT SECTION.                                               
054700                                                                          
054800     MOVE SHUV-KDCLAGER TO PTOP-KDMFSFOR                                  
054900     MOVE SHUV-IDORDNSB TO PTOP-IDORDNSB                                  
055000     MOVE SHUV-IDORDNSS TO PTOP-IDORDNSS                                  
055100                                                                          
055200     PERFORM IMS-INSERT-MSG-ALT                                           
055300     .                                                                    
055400     EJECT                                                                
055500 S01-FEL-MEDDELANDE SECTION.                                              
055600                                                                          
055700     CALL WMEDKONV USING MED-WMEDAREA                                     
055800     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
055900     .                                                                    
056000                                                                          
056100                                                                          
056200 S02-INFO-MEDDELANDE SECTION.                                             
056300                                                                          
056400     CALL WMEDKONV USING MED-WMEDAREA                                     
056500     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
056600     .                                                                    
056700     EJECT                                                                
056800 S03-ISRT-SALDOLOGG SECTION.                                              
056900     PERFORM IMS-ISRT-WDL901                                              
057000     IF SEGMENT-FINNS-REDAN                                               
057100       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
057200         SUBTRACT 1         FROM LOGG-IDSEKVNR                            
057300         PERFORM IMS-ISRT-WDL901                                          
057400       END-PERFORM                                                        
057500     END-IF                                                               
057600     .                                                                    
057700     EJECT                                                                
057800                                                                          
057900* --- IMS SEKTIONER ---                                                   
058000     SKIP3                                                                
058100 IMS-GET-MSG SECTION.                                                     
058200                                                                          
058300     MOVE '  QC' TO GODK-STATUSKODER                                      
058400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
058500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
058600     PERFORM IMS-STATUSKONTROLL                                           
058700     .                                                                    
058800     SKIP3                                                                
058900 IMS-INSERT-MSG SECTION.                                                  
059000                                                                          
059100     IF ENGLISH-TEXT                                                      
059200       MOVE 'N' TO MFS-KDHUVOMR                                           
059300     END-IF                                                               
059400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
059500     MOVE SPACE TO GODK-STATUSKODER                                       
059600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
059700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
059800     PERFORM IMS-STATUSKONTROLL                                           
059900     .                                                                    
060000     EJECT                                                                
060100 IMS-INSERT-MSG-ALT SECTION.                                              
060200                                                                          
060300     MOVE SPACE TO GODK-STATUSKODER                                       
060400     CALL CBLTDLI USING ISRT ALT-PCB P-TO-P-SW                            
060500     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
060600     PERFORM IMS-STATUSKONTROLL                                           
060700     .                                                                    
060800     EJECT                                                                
060900 IMS-GHU-WDJ2-WLSATG01 SECTION.                                           
061000                                                                          
061100     STRING 'WLSATG01(IDORDNST =' W-WDJ2-IDORDNST-X ')'                   
061200          DELIMITED BY SIZE INTO SSA1                                     
061300     MOVE '  GE' TO GODK-STATUSKODER                                      
061400     CALL CBLTDLI USING GHU SATG1-PCB DLI-IO-AREA1 SSA1                   
061500     MOVE SATG1-STATUS-CODE TO STATUS-WS                                  
061600     PERFORM IMS-STATUSKONTROLL                                           
061700     .                                                                    
061800                                                                          
061900                                                                          
062000 IMS-REPL-WDJ2-WLSATG01 SECTION.                                          
062100                                                                          
062200     MOVE '    ' TO GODK-STATUSKODER                                      
062300     CALL CBLTDLI USING REPL SATG1-PCB DLI-IO-AREA1                       
062400     MOVE SATG1-STATUS-CODE TO STATUS-WS                                  
062500     PERFORM IMS-STATUSKONTROLL                                           
062600     .                                                                    
062700     EJECT                                                                
062800 IMS-GNP-WDJ2-WLSATG11 SECTION.                                           
062900                                                                          
063000     STRING 'WLSATG01(IDORDNST =' W-WDJ2-IDORDNST-X ')'                   
063100          DELIMITED BY SIZE INTO SSA1                                     
063200     MOVE 'WLSATG11 ' TO SSA2                                             
063300     MOVE '  GE' TO GODK-STATUSKODER                                      
063400     CALL CBLTDLI USING GNP SATG1-PCB DLI-IO-AREA1 SSA1 SSA2              
063500     MOVE SATG1-STATUS-CODE TO STATUS-WS                                  
063600     PERFORM IMS-STATUSKONTROLL                                           
063700     .                                                                    
063800     EJECT                                                                
063900 IMS-ISRT-WDJ2-WLSATG12 SECTION.                                          
064000                                                                          
064100     STRING 'WLSATG01(IDORDNST =' W-WDJ2-IDORDNST-X ')'                   
064200          DELIMITED BY SIZE INTO SSA1                                     
064300     MOVE 'WLSATG12 ' TO SSA2                                             
064400     MOVE '    ' TO GODK-STATUSKODER                                      
064500     CALL CBLTDLI USING ISRT SATG2-PCB DLI-IO-AREA2 SSA1 SSA2             
064600     MOVE SATG2-STATUS-CODE TO STATUS-WS                                  
064700     PERFORM IMS-STATUSKONTROLL                                           
064800     .                                                                    
064900     EJECT                                                                
065000 IMS-GHU-WDK6-WLARTC11 SECTION.                                           
065100                                                                          
065200     STRING 'WLARTC01(IDARTNR  =' W-WDK6-IDARTNR-X ')'                    
065300          DELIMITED BY SIZE INTO SSA1                                     
065400     MOVE 'WLARTC11 ' TO SSA2                                             
065500     MOVE '    ' TO GODK-STATUSKODER                                      
065600     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA3 SSA1 SSA2               
065700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
065800     PERFORM IMS-STATUSKONTROLL                                           
065900     .                                                                    
066000                                                                          
066100                                                                          
066200 IMS-REPL-WDK6-WLARTC11 SECTION.                                          
066300                                                                          
066400     MOVE '    ' TO GODK-STATUSKODER                                      
066500     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA3                        
066600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
066700     PERFORM IMS-STATUSKONTROLL                                           
066800     .                                                                    
066900     EJECT                                                                
067000 IMS-GU-WDD3-WLBENA11 SECTION.                                            
067100                                                                          
067200     STRING 'WLBENA01(WDD3BSEQ =' W-WDD3BSEQ-X ')'                        
067300          DELIMITED BY SIZE INTO SSA1                                     
067400     STRING 'WLBENA11(IDSKYLT  =' W-WDD3-IDSKYLT-X ')'                    
067500          DELIMITED BY SIZE INTO SSA2                                     
067600     MOVE '  GE' TO GODK-STATUSKODER                                      
067700     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA3 SSA1 SSA2                
067800     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
067900     PERFORM IMS-STATUSKONTROLL                                           
068000     .                                                                    
068100     EJECT                                                                
068200 IMS-ISRT-WDL901 SECTION.                                                 
068300                                                                          
068400     MOVE 'WLLOGA01 ' TO SSA1                                             
068500     MOVE '  II' TO GODK-STATUSKODER                                      
068600     CALL CBLTDLI USING ISRT WLLOGA-PCB WLLOGA01 SSA1                     
068700     MOVE WLLOGA-STATUS-CODE TO STATUS-WS                                 
068800     PERFORM IMS-STATUSKONTROLL                                           
068900     .                                                                    
       IMS-GU-WDD811 SECTION.                                                   
           STRING 'WDD801  (IDARTNR  =' W-IDARTNR-X ')'                         
                   DELIMITED BY SIZE INTO SSA1                                  
           STRING 'WDD811  (WDD811KY>=' W-WDD811KY-MIN-X                        
                          '&WDD811KY<=' W-WDD811KY-MAX-X ')'                    
                   DELIMITED BY SIZE INTO SSA2                                  
           MOVE '  GE' TO GODK-STATUSKODER                                      
           CALL CBLTDLI USING GU WDD8-PCB DLI-IO-AREA-811 SSA1 SSA2             
           MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
           PERFORM IMS-STATUSKONTROLL                                           
           .                                                                    
069000     EJECT                                                                
069100 IMS-STATUSKONTROLL SECTION.                                              
069200                                                                          
069300     SET STATUS-IX TO 1                                                   
069400     SEARCH GODK-STATUS                                                   
069500       AT END                                                             
069600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
069700         DELIMITED BY SIZE INTO FELTEXT                                   
069800         CALL FELLOG                                                      
069900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
070000     END-SEARCH                                                           
070100     .                                                                    
