000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4041600.                                                
000300 AUTHOR.         OLGRENER LASSI.                                          
000400 DATE-WRITTEN.   04/12/23.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        VISAR FRÅN VILKA DC EN KUND FÅR SINA LEVERANSER.                 
000900*                                                                         
001000*        PROGRAMMET LÄSER      WDB2                                       
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSAKTION: W4T416                                              
001400*        MID:         W4I41601                                            
001500*                                                                         
001600*    UTDATA.                                                              
001700*        MOD:         W4O41601                                            
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100                                                                          
002200 DATA DIVISION.                                                           
002300     EJECT                                                                
002400 WORKING-STORAGE SECTION.                                                 
002500 77  IDPGM                       PIC X(08)   VALUE 'W4041600'.            
002600 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
002700 77  JA                          PIC X       VALUE 'J'.                   
002710 77  YES                         PIC X       VALUE 'Y'.                   
002800 77  NEJ                         PIC X       VALUE 'N'.                   
002900 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
003000 77  MAX-INDX                    PIC S9(4)  VALUE +13   COMP SYNC.        
003100                                                                          
003200 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
003300     88  NYCKLAR-OK                          VALUE 'J'.                   
003400     88  NYCKLAR-FEL                         VALUE 'N'.                   
003500                                                                          
003600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
003700     88  EGEN-MID                            VALUE '4416'.                
003800     88  GODK-MID                            VALUE '4411' '4412'          
003900                                                   '4413' '4416'.         
004000     88  HELP-MID                            VALUE '0551'.                
004100                                                                          
004200 77  SOEK-NYCKEL-SW              PIC X(3)    VALUE SPACE.                 
004300     88  SOEK-DAG                            VALUE 'D  '.                 
004400     88  SOEK-DAG-BULK                       VALUE 'DB '.                 
004500     88  SOEK-DAG-BULK-LDC                   VALUE 'DBL'.                 
004600     88  SOEK-DAG-LDC                        VALUE 'D L'.                 
004700     88  SOEK-BULK                           VALUE ' B '.                 
004800     88  SOEK-BULK-LDC                       VALUE ' BL'.                 
004900     88  SOEK-LDC                            VALUE '  L'.                 
005000     EJECT                                                                
005100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
005200 01  GENERELLA-SUBPROGRAM.                                                
005300     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
005400     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
005500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005700     EJECT                                                                
005800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
005900*01 -COPY WMEDAREA                                                        
006000     SKIP3                                                                
006100 01  MESSAGE-CODES.                                                       
006200     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
006300     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
006400     03  ERR-CUST-MISSING        PIC X(3)    VALUE '040'.                 
006500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
006600     EJECT                                                                
006700*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
006800*                                                                         
006900 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
007000     SKIP3                                                                
007100*01 -COPY WMSGINIT                                                        
007200     EJECT                                                                
007300*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
007400*                                                                         
007500 01  SPAR-AREA.                                                           
007600     03  SPAR-IDTRANS           PIC X(4)    VALUE '4416'.                 
007700     03  SPAR-IDKUNDNR-ENTER       PIC S9(7)   COMP-3.                    
007800     03  SPAR-IDKUNDNR-NEXT        PIC S9(7)   COMP-3.                    
007900     EJECT                                                                
008000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008100*                                                                         
008200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
008300     SKIP3                                                                
008400*01  MID -COPY W4I41601                                                   
008500     EJECT                                                                
008600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
008700     SKIP3                                                                
008800*01  -COPY WMSGAREA                                                       
008900     EJECT                                                                
009000     03  MOD REDEFINES MSG-AREA.                                          
009100*      05  -COPY W4O41601                                                 
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
009400     SKIP3                                                                
009500*01  -COPY WMFSAREA                                                       
009600     EJECT                                                                
009700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009800*                                                                         
009900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010000     SKIP3                                                                
010100 01  NYCKLAR-TILL-DLI.                                                    
010200*    --- VÄRDE PÅ BLÄDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN             
010300     03  W-IDDC-DAY-X.                                                    
010400         05  W-IDDC-DAY        PIC X(2).                                  
010500                                                                          
010600     03  W-IDDC-BULK-X.                                                   
010700         05  W-IDDC-BULK        PIC X(2).                                 
010800                                                                          
010900     03  W-FLLDCKND-X.                                                    
011000         05  W-FLLDCKND         PIC X(1).                                 
011100                                                                          
011200     03  W-IDGMT-MIN-X.                                                   
011300         05  W-IDDISTR-MIN       PIC S9(5)   VALUE ZERO COMP-3.           
011400         05  W-IDKUNDNR-MIN      PIC S9(7)   VALUE ZERO COMP-3.           
011500                                                                          
011600     03  W-IDGMT-MAX-X.                                                   
011700         05  W-IDDISTR-MAX       PIC S9(5)   VALUE ZERO COMP-3.           
011800         05  W-IDKUNDNR-MAX      PIC S9(7)   VALUE 9999999 COMP-3.        
011900     SKIP2                                                                
012000*    --- STATUS-KOD FRÅN IMS                                              
012100 01  STATUS-WS                   PIC XX.                                  
012200     88  SEGMENT-FINNS                       VALUE '  '.                  
012300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012500     SKIP2                                                                
012600 01  GODK-STATUSKODER.                                                    
012700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012800     SKIP3                                                                
012900 01  SSA1                        PIC X(128).                              
013000     EJECT                                                                
013100*    --- IMS FUNKTIONSKODER                                               
013200*01  -COPY W0003                                                          
013300     EJECT                                                                
013400*    ---  DLI INPUT-OUTPUT AREA                                           
013500                                                                          
013600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
013700 01  DLI-IO-WDB201.                                                       
013800*    03  -COPY WDB201                                                     
013900     EJECT                                                                
014000 LINKAGE SECTION.                                                         
014100*01  -COPY W0009   -PRE MSG-                                              
014200*01  -COPY W0008   -PRE WDP7-                                             
014300     05  FILLER                  PIC X.                                   
014400                                                                          
014500*01  -COPY W0008  -PRE WDB2-                                              
014600     05  FILLER                  PIC X.                                   
014700     EJECT                                                                
014800 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDB2-PCB.                     
014900 MAIN SECTION.                                                            
015000     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDB2-PCB.                     
015100                                                                          
015200     PERFORM IMS-GET-MSG                                                  
015300     IF SEGMENT-FINNS                                                     
015400       PERFORM A-INIT                                                     
015500       PERFORM B-KOLLA-NYCKLAR                                            
015600       IF NYCKLAR-OK                                                      
015700         IF MFS-FIRST                                                     
015800           PERFORM C-FOERSTA-SIDA                                         
015900         ELSE                                                             
016000           IF MFS-NEXT                                                    
016100             PERFORM D-NAESTA-SIDA                                        
016200           ELSE                                                           
016300             PERFORM E-SAMMA-SIDA                                         
016400           END-IF                                                         
016500         END-IF                                                           
016600         PERFORM F-LAES-VISA-INFO                                         
016700       END-IF                                                             
016800       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O41601 + 4                      
016900       PERFORM IMS-INSERT-MSG                                             
017000     END-IF                                                               
017100                                                                          
017200     MOVE ZERO TO RETURN-CODE                                             
017300     GOBACK                                                               
017400     .                                                                    
017500     EJECT                                                                
017600 A-INIT SECTION.                                                          
017700                                                                          
017800     IF MSG-DUBBLA-TRANSKODER                                             
017900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I41601                 
018000       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
018100       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
018200     ELSE                                                                 
018300       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I41601                  
018400       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
018500       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
018600     END-IF                                                               
018700                                                                          
018800     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
018900     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
019000     MOVE MFS-IDTRANS TO W-IDTRANS                                        
019100                                                                          
019200     MOVE LOW-VALUE TO MSG-AREA                                           
019300     MOVE 'W4O416N1' TO MFS-IDMOD                                         
019400     MOVE '4416' TO MOD-IDTRANS                                           
019500     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
019600                                                                          
019700     IF EGEN-MID OR HELP-MID                                              
019800       CONTINUE                                                           
019900     ELSE                                                                 
020000       MOVE SPACE TO MFS-KDTRTYP                                          
020100       MOVE '7' TO MFS-IDPFK                                              
020200     END-IF                                                               
020300     .                                                                    
020400     EJECT                                                                
020500 B-KOLLA-NYCKLAR SECTION.                                                 
020600                                                                          
020700     MOVE ALL '+'           TO MSGI-WMSGINIT                              
020800     MOVE '001'             TO MSGI-KDCALL                                
020900     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
021000     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
021100     MOVE '4416'            TO MSGI-IDTRANS                               
021200     IF EGEN-MID                                                          
021300       MOVE MID-IDDISTR-IN     TO MSGI-IDDISTR                            
021400       MOVE MID-IDDC-DAY-IN    TO MSGI-IDDC-DAY                           
021500       MOVE MID-IDDC-BULK-IN   TO MSGI-IDDC-BULK                          
021600       MOVE MID-FLLDCKND-IN    TO MSGI-FLLDCKND                           
021700     END-IF                                                               
021800     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
021900     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
022000                                                                          
022100*    - SPRÅK SOM SKA ANVÄNDAS AV WMEDKONV                                 
022200     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
022300                                                                          
022400     MOVE JA TO NYCKLAR-SW                                                
022500                                                                          
022600     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
022700                                                                          
022800     IF MID-IDDISTR-IN NOT = ALL '+'                                      
022900       MOVE '7'         TO MFS-IDPFK                                      
023000       MOVE SPACE       TO MFS-KDTRTYP                                    
023100     END-IF                                                               
023200     INSPECT MSGI-IDDISTR REPLACING LEADING SPACE BY ZERO                 
023300     IF MSGI-IDDISTR NUMERIC AND                                          
023400        MSGI-IDDISTR > ZERO                                               
023500       MOVE MSGI-IDDISTR TO W-IDDISTR-MIN                                 
023600                            W-IDDISTR-MAX                                 
023700     ELSE                                                                 
023800       MOVE NEJ TO NYCKLAR-SW                                             
023900     END-IF                                                               
024000                                                                          
024100     MOVE MFS-RENSA-FAELT TO MOD-IDDC-DAY-IN                              
024200                                                                          
024300     IF MID-IDDC-DAY-IN NOT = ALL '+'                                     
024400       MOVE '7'         TO MFS-IDPFK                                      
024500       MOVE SPACE       TO MFS-KDTRTYP                                    
024600     END-IF                                                               
024700     IF MSGI-IDDC-DAY > SPACE                                             
024800       MOVE MSGI-IDDC-DAY TO W-IDDC-DAY                                   
024900       MOVE 'D'           TO SOEK-NYCKEL-SW (1:1)                         
025000     ELSE                                                                 
025100       MOVE SPACE         TO SOEK-NYCKEL-SW (1:1)                         
025200     END-IF                                                               
025300                                                                          
025400     MOVE MFS-RENSA-FAELT TO MOD-IDDC-BULK-IN                             
025500                                                                          
025600     IF MID-IDDC-BULK-IN NOT = ALL '+'                                    
025700       MOVE '7'         TO MFS-IDPFK                                      
025800       MOVE SPACE       TO MFS-KDTRTYP                                    
025900     END-IF                                                               
026000     IF MSGI-IDDC-BULK > SPACE                                            
026100       MOVE MSGI-IDDC-BULK TO W-IDDC-BULK                                 
026200       MOVE 'B'           TO SOEK-NYCKEL-SW (2:1)                         
026300     ELSE                                                                 
026400       MOVE SPACE         TO SOEK-NYCKEL-SW (2:1)                         
026500     END-IF                                                               
026600                                                                          
026700     MOVE MFS-RENSA-FAELT TO MOD-FLLDCKND-IN                              
026800                                                                          
026900     IF MID-FLLDCKND-IN NOT = ALL '+'                                     
027000       MOVE '7'         TO MFS-IDPFK                                      
027100       MOVE SPACE       TO MFS-KDTRTYP                                    
027200     END-IF                                                               
027210                                                                          
027300     IF  MSGI-FLLDCKND > SPACE                                            
027400     AND MSGI-FLLDCKND = JA                                               
027410*    AND (MSGI-FLLDCKND = JA OR MSGI-FLLDCKND = YES)                      
027500       MOVE MSGI-FLLDCKND TO W-FLLDCKND                                   
027600       MOVE 'L'           TO SOEK-NYCKEL-SW (3:1)                         
027700     ELSE                                                                 
027710       IF MSGI-FLLDCKND > SPACE                                           
027720       AND MSGI-FLLDCKND = YES                                            
027820         MOVE JA          TO W-FLLDCKND                                   
027830         MOVE 'L'         TO SOEK-NYCKEL-SW (3:1)                         
027831       ELSE                                                               
027840         MOVE SPACE       TO SOEK-NYCKEL-SW (3:1)                         
027900       END-IF                                                             
027910     END-IF                                                               
028000                                                                          
028100     IF GODK-MID OR NYCKLAR-OK                                            
028200       MOVE MSGI-IDDISTR    TO MOD-IDDISTR-UT                             
028300       MOVE MSGI-IDDC-DAY   TO MOD-IDDC-DAY-UT                            
028400       MOVE MSGI-IDDC-BULK  TO MOD-IDDC-BULK-UT                           
028500       MOVE MSGI-FLLDCKND   TO MOD-FLLDCKND-UT                            
028600     ELSE                                                                 
028700       MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                             
028800                               MOD-IDDC-DAY-UT                            
028900                               MOD-IDDC-BULK-UT                           
029000                               MOD-FLLDCKND-UT                            
029100     END-IF                                                               
029200                                                                          
029300     IF NYCKLAR-FEL                                                       
029400       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
029500       CALL WMEDKONV USING MED-WMEDAREA                                   
029600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
029700       PERFORM MFS-RENSA-FAELT-UT                                         
029800     END-IF                                                               
029900     .                                                                    
030000     EJECT                                                                
030100 C-FOERSTA-SIDA SECTION.                                                  
030200                                                                          
030300     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
030400     CALL WMEDKONV USING MED-WMEDAREA                                     
030500     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
030600                                                                          
030700***  PERFORM MFS-RENSA-FAELT-IN                                           
030800     .                                                                    
030900     EJECT                                                                
031000 D-NAESTA-SIDA SECTION.                                                   
031100                                                                          
031200     IF SPAR-IDTRANS = '4416'                                             
031300       MOVE SPAR-IDKUNDNR-NEXT TO W-IDKUNDNR-MIN                          
031400*    ELSE                                                                 
031500***    PERFORM MFS-RENSA-FAELT-IN                                         
031600     END-IF                                                               
031700     .                                                                    
031800     EJECT                                                                
031900 E-SAMMA-SIDA SECTION.                                                    
032000                                                                          
032100     IF SPAR-IDTRANS = '4416' OR '0551'                                   
032200       MOVE SPAR-IDKUNDNR-ENTER  TO W-IDKUNDNR-MIN                        
032300*    ELSE                                                                 
032400***    PERFORM MFS-RENSA-FAELT-IN                                         
032500     END-IF                                                               
032600     .                                                                    
032700     EJECT                                                                
032800 F-LAES-VISA-INFO SECTION.                                                
032900                                                                          
033000     PERFORM FA-LAES-GRUNDDATA                                            
033100                                                                          
033200     IF SEGMENT-SAKNAS                                                    
033300                                                                          
033400        MOVE ERR-CUST-MISSING TO MED-IDMFSFEL                             
033500        CALL WMEDKONV USING MED-WMEDAREA                                  
033600        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
033700        PERFORM MFS-RENSA-FAELT-UT                                        
033800     ELSE                                                                 
033900                                                                          
034000       MOVE +1 TO INDX                                                    
034100       MOVE GMT-IDKUNDNR     TO SPAR-IDKUNDNR-ENTER                       
034200                                SPAR-IDKUNDNR-NEXT                        
034300                                                                          
034400       PERFORM UNTIL INDX > MAX-INDX                                      
034500         IF SEGMENT-FINNS AND GMT-TISTODAT > 0                            
034500           CONTINUE                                                       
034500         ELSE                                                             
034500          IF SEGMENT-FINNS                                                
034600            MOVE GMT-IDKUNDNR     TO MOD-IDKUNDNR (INDX)                  
034700            MOVE GMT-IDDC-BULK(1) TO MOD-IDDC-BULK (INDX)                 
034800            MOVE GMT-IDDC-DAY(1)  TO MOD-IDDC-DAY (INDX)                  
034900            MOVE GMT-IDDC-VOR(1)  TO MOD-IDDC-VOR (INDX)                  
035000            MOVE GMT-FLLDCKND     TO MOD-FLLDCKND (INDX)                  
035100          ELSE                                                            
035200            MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR (INDX)                   
035300                                    MOD-IDDC-BULK (INDX)                  
035400                                    MOD-IDDC-DAY (INDX)                   
035500                                    MOD-IDDC-VOR (INDX)                   
035600                                    MOD-FLLDCKND (INDX)                   
035700          END-IF                                                          
035800          ADD 1 TO INDX                                                   
035700         END-IF                                                           
035900                                                                          
036000         PERFORM FA-LAES-GRUNDDATA                                        
036100       END-PERFORM                                                        
036200                                                                          
036300       IF SEGMENT-FINNS                                                   
036400         MOVE GMT-IDKUNDNR     TO SPAR-IDKUNDNR-NEXT                      
036500         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
036600         CALL WMEDKONV USING MED-WMEDAREA                                 
036700         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
036800       END-IF                                                             
036900                                                                          
037000       MOVE '002'      TO MSGI-KDCALL                                     
037100       MOVE '4416'   TO SPAR-IDTRANS                                      
037200       MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                  
037300       CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                         
037400     END-IF                                                               
037500     .                                                                    
037600     EJECT                                                                
037700 FA-LAES-GRUNDDATA SECTION.                                               
037800                                                                          
037900     IF SOEK-NYCKEL-SW = SPACE                                            
038000       PERFORM IMS-GN-WDB201                                              
038100     ELSE                                                                 
038200       IF SOEK-DAG                                                        
038300         PERFORM IMS-GN-WDB201-D                                          
038400       END-IF                                                             
038500       IF SOEK-DAG-BULK                                                   
038600         PERFORM IMS-GN-WDB201-DB                                         
038700       END-IF                                                             
038800       IF SOEK-DAG-BULK-LDC                                               
038900         PERFORM IMS-GN-WDB201-DBL                                        
039000       END-IF                                                             
039100       IF SOEK-DAG-LDC                                                    
039200         PERFORM IMS-GN-WDB201-DL                                         
039300       END-IF                                                             
039400       IF SOEK-BULK                                                       
039500         PERFORM IMS-GN-WDB201-B                                          
039600       END-IF                                                             
039700       IF SOEK-BULK-LDC                                                   
039800         PERFORM IMS-GN-WDB201-BL                                         
039900       END-IF                                                             
040000       IF SOEK-LDC                                                        
040100         PERFORM IMS-GN-WDB201-L                                          
040200       END-IF                                                             
040300     END-IF                                                               
040400     .                                                                    
040500     EJECT                                                                
040600 MFS-RENSA-FAELT-UT SECTION.                                              
040700                                                                          
040800     MOVE +1 TO INDX                                                      
040900     PERFORM UNTIL INDX > MAX-INDX                                        
041000       MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR (INDX)                        
041100                               MOD-IDDC-BULK (INDX)                       
041200                               MOD-IDDC-DAY (INDX)                        
041300                               MOD-IDDC-VOR (INDX)                        
041400                               MOD-FLLDCKND (INDX)                        
041500       ADD +1 TO INDX                                                     
041600     END-PERFORM                                                          
041700     .                                                                    
041800     EJECT                                                                
041900* --- IMS SEKTIONER ---                                                   
042000     SKIP3                                                                
042100 IMS-GET-MSG SECTION.                                                     
042200                                                                          
042300     MOVE '  QC' TO GODK-STATUSKODER                                      
042400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
042500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
042600     PERFORM IMS-STATUSKONTROLL                                           
042700     .                                                                    
042800     SKIP3                                                                
042900 IMS-INSERT-MSG SECTION.                                                  
043000                                                                          
043100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
043200     MOVE SPACE TO GODK-STATUSKODER                                       
043300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
043400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
043500     PERFORM IMS-STATUSKONTROLL                                           
043600     .                                                                    
043700     EJECT                                                                
043800 IMS-GN-WDB201 SECTION.                                                   
043900                                                                          
044000     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
044100                    '&IDGMT   <=' W-IDGMT-MAX-X ')'                       
044200          DELIMITED BY SIZE INTO SSA1                                     
044300     MOVE '  GE' TO GODK-STATUSKODER                                      
044400     CALL CBLTDLI USING GN WDB2-PCB DLI-IO-WDB201 SSA1                    
044500     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
044600     PERFORM IMS-STATUSKONTROLL                                           
044700     .                                                                    
044800     SKIP2                                                                
044900 IMS-GN-WDB201-D SECTION.                                                 
045000                                                                          
045100     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
045200                    '&IDGMT   <=' W-IDGMT-MAX-X                           
045300                    '&IDDCDAY1 =' W-IDDC-DAY-X ')'                        
045400          DELIMITED BY SIZE INTO SSA1                                     
045500     MOVE '  GE' TO GODK-STATUSKODER                                      
045600     CALL CBLTDLI USING GN WDB2-PCB DLI-IO-WDB201 SSA1                    
045700     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
045800     PERFORM IMS-STATUSKONTROLL                                           
045900     .                                                                    
046000     SKIP2                                                                
046100 IMS-GN-WDB201-DB SECTION.                                                
046200                                                                          
046300     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
046400                    '&IDGMT   <=' W-IDGMT-MAX-X                           
046500                    '&IDDCDAY1 =' W-IDDC-DAY-X                            
046600                    '&IDDCBUL1 =' W-IDDC-BULK-X ')'                       
046700          DELIMITED BY SIZE INTO SSA1                                     
046800     MOVE '  GE' TO GODK-STATUSKODER                                      
046900     CALL CBLTDLI USING GN WDB2-PCB DLI-IO-WDB201 SSA1                    
047000     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
047100     PERFORM IMS-STATUSKONTROLL                                           
047200     .                                                                    
047300     EJECT                                                                
047400 IMS-GN-WDB201-DBL SECTION.                                               
047500                                                                          
047600     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
047700                    '&IDGMT   <=' W-IDGMT-MAX-X                           
047800                    '&IDDCDAY1 =' W-IDDC-DAY-X                            
047900                    '&IDDCBUL1 =' W-IDDC-BULK-X                           
048000                    '&FLLDCKND =' W-FLLDCKND-X ')'                        
048100          DELIMITED BY SIZE INTO SSA1                                     
048200     MOVE '  GE' TO GODK-STATUSKODER                                      
048300     CALL CBLTDLI USING GN WDB2-PCB DLI-IO-WDB201 SSA1                    
048400     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
048500     PERFORM IMS-STATUSKONTROLL                                           
048600     .                                                                    
048700     SKIP2                                                                
048800 IMS-GN-WDB201-DL SECTION.                                                
048900                                                                          
049000     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
049100                    '&IDGMT   <=' W-IDGMT-MAX-X                           
049200                    '&IDDCDAY1 =' W-IDDC-DAY-X                            
049300                    '&FLLDCKND =' W-FLLDCKND-X ')'                        
049400          DELIMITED BY SIZE INTO SSA1                                     
049500     MOVE '  GE' TO GODK-STATUSKODER                                      
049600     CALL CBLTDLI USING GN WDB2-PCB DLI-IO-WDB201 SSA1                    
049700     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
049800     PERFORM IMS-STATUSKONTROLL                                           
049900     .                                                                    
050000     EJECT                                                                
050100 IMS-GN-WDB201-B  SECTION.                                                
050200                                                                          
050300     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
050400                    '&IDGMT   <=' W-IDGMT-MAX-X                           
050500                    '&IDDCBUL1 =' W-IDDC-BULK-X ')'                       
050600          DELIMITED BY SIZE INTO SSA1                                     
050700     MOVE '  GE' TO GODK-STATUSKODER                                      
050800     CALL CBLTDLI USING GN WDB2-PCB DLI-IO-WDB201 SSA1                    
050900     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
051000     PERFORM IMS-STATUSKONTROLL                                           
051100     .                                                                    
051200     SKIP2                                                                
051300 IMS-GN-WDB201-BL SECTION.                                                
051400                                                                          
051500     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
051600                    '&IDGMT   <=' W-IDGMT-MAX-X                           
051700                    '&IDDCBUL1 =' W-IDDC-BULK-X                           
051800                    '&FLLDCKND =' W-FLLDCKND-X ')'                        
051900          DELIMITED BY SIZE INTO SSA1                                     
052000     MOVE '  GE' TO GODK-STATUSKODER                                      
052100     CALL CBLTDLI USING GN WDB2-PCB DLI-IO-WDB201 SSA1                    
052200     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
052300     PERFORM IMS-STATUSKONTROLL                                           
052400     .                                                                    
052500     SKIP2                                                                
052600 IMS-GN-WDB201-L SECTION.                                                 
052700                                                                          
052800     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
052900                    '&IDGMT   <=' W-IDGMT-MAX-X                           
053000                    '&FLLDCKND =' W-FLLDCKND-X ')'                        
053100          DELIMITED BY SIZE INTO SSA1                                     
053200     MOVE '  GE' TO GODK-STATUSKODER                                      
053300     CALL CBLTDLI USING GN WDB2-PCB DLI-IO-WDB201 SSA1                    
053400     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
053500     PERFORM IMS-STATUSKONTROLL                                           
053600     .                                                                    
053700     EJECT                                                                
053800 IMS-STATUSKONTROLL SECTION.                                              
053900                                                                          
054000     SET STATUS-IX TO 1                                                   
054100     SEARCH GODK-STATUS                                                   
054200       AT END                                                             
054300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
054400         DELIMITED BY SIZE INTO FELTEXT                                   
054500         CALL FELLOG                                                      
054600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
054700         CONTINUE                                                         
054800     END-SEARCH                                                           
054900     .                                                                    
