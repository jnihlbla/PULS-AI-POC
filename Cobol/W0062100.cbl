000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0062100.                                                
000300 AUTHOR.         RICHARD THÖRNGREN.                                       
000400 DATE-WRITTEN.   91/02/14.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        DISPATCH-SYSTEMET.                                               
000900*        VISAR, LÄGGER UPP, TAR BORT, FÖRÄNDRAR SAMT STARTAR              
001000*        PÅ MEDDELANDE-KOMMUNIKATIONS-DATABASEN                           
001100*        TEMPORÄRT LAGRADE MPP-TRANSAR.                                   
001200*                                                                         
001300*        PROGRAMMET UPPATERAR WLKOMA (WDP8)                               
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W0T621                                              
001700*                     W0T621X                                             
001800*        MID:         W0I62101                                            
001900*                                                                         
002000*    UTDATA.                                                              
002100*        MOD:         W0O62101                                            
002200*        TRANSAKTION: EFTER ÖNSKEMÅL                                      
002300                                                                          
002400                                                                          
002500 ENVIRONMENT DIVISION.                                                    
002600                                                                          
002700 DATA DIVISION.                                                           
002800     EJECT                                                                
002900 WORKING-STORAGE SECTION.                                                 
003000                                                                          
003100*    -- CHECKED BY WY2000                                                 
003200                                                                          
003300 77  IDPGM                       PIC X(08)   VALUE 'W0062100'.            
003400 77  W-COMPILED                  PIC X(16)   VALUE SPACE.                 
003500 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
003600 77  JA                          PIC X       VALUE 'J'.                   
003700 77  NEJ                         PIC X       VALUE 'N'.                   
003800 77  WS-DATE                     PIC 9(6).                                
003900 77  WS-TIME                     PIC 9(8).                                
004000 77  WS-IDRADNR-RED              PIC Z(3)9.                               
004100                                                                          
004200 77  UNIK-SW                     PIC X       VALUE 'N'.                   
004300                                                                          
004400 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004500     88  INDATA-OK                           VALUE 'J'.                   
004600     88  INDATA-FEL                          VALUE 'N'.                   
004700                                                                          
004800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004900     88  NYCKLAR-OK                          VALUE 'J'.                   
005000     88  NYCKLAR-FEL                         VALUE 'N'.                   
005100                                                                          
005200 77  ALLT-SW                     PIC X       VALUE 'J'.                   
005300     88  ALLT-OK                             VALUE 'J'.                   
005400                                                                          
005500 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005600     88  EGEN-MID                            VALUE '0621'.                
005700     88  GODK-MID                            VALUE '0621' '0622'.         
005800                                                                          
005900 01  W-VIMSID.                                                            
006000   03  W-IMSID                   PIC X(4)    VALUE SPACE.                 
006100   03  FILLER                    PIC X(4)    VALUE SPACE.                 
006200                                                                          
006300     EJECT                                                                
006400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006500 01  GENERELLA-SUBPROGRAM.                                                
006600   03  W005INIT                  PIC X(8)    VALUE 'W005INIT'.            
006700   03  WMEDKONV                  PIC X(8)    VALUE 'WMEDKONV'.            
006800   03  VIMSID                    PIC X(8)    VALUE 'VIMSID  '.            
006900   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
007000   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
007100     EJECT                                                                
007200*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
007300*   -COPY WMSGINIT                                                        
007400                                                                          
007500     EJECT                                                                
007600*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007700*   -COPY WMEDAREA                                                        
007800     SKIP3                                                                
007900 01  MESSAGE-CODES.                                                       
008000   03  INF-FIRST-PAGE            PIC X(3)    VALUE '006'.                 
008100   03  ERR-CORR-HILITE-FLDS      PIC X(3)    VALUE '001'.                 
008200   03  INF-PRESS-PF11            PIC X(3)    VALUE '003'.                 
008300   03  INF-UPDATE-DONE           PIC X(3)    VALUE '101'.                 
008400   03  ERR-WRONG-KEY             PIC X(3)    VALUE '401'.                 
008500   03  ERR-TRANS-MISS            PIC X(3)    VALUE '078'.                 
008600     EJECT                                                                
008700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008800*                                                                         
008900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009000                                                                          
009100*01  MID -COPY W0I62101                                                   
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)  VALUE 'MSG/ALT-AREA'.         
009400*01  -COPY WMSGKOM                                                        
009500     EJECT                                                                
009600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009700                                                                          
009800*01  -COPY WMSGAREA                                                       
009900     EJECT                                                                
010000   03  MOD REDEFINES MSG-AREA.                                            
010100*    05  -COPY W0O62101                                                   
010200     EJECT                                                                
010300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010400                                                                          
010500*01  -COPY WMFSAREA                                                       
010600     EJECT                                                                
010700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010800                                                                          
010900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011000                                                                          
011100 01  NYCKLAR-TILL-DLI.                                                    
011200   03  W-WDP801KY-X.                                                      
011300     05  W-IDSNDNOD              PIC X(8)     VALUE SPACE.                
011400     05  W-IDSNDJOB              PIC X(8)     VALUE SPACE.                
011500     05  W-TIREGDAT              PIC S9(7)    VALUE ZERO COMP-3.          
011600     05  W-TIKLOCK               PIC S9(9)    VALUE ZERO COMP-3.          
011700   03  W-IDRADNR-X.                                                       
011800     05  W-IDRADNR               PIC S9(5)    VALUE ZERO COMP-3.          
011900                                                                          
012000                                                                          
012100*    --- STATUS-KOD FRÅN IMS                                              
012200 01  STATUS-WS                   PIC XX.                                  
012300     88  SEGMENT-FINNS                       VALUE '  '.                  
012400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012600                                                                          
012700 01  GODK-STATUSKODER.                                                    
012800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012900                                                                          
013000 01  SSA1                        PIC X(64).                               
013100 01  SSA2                        PIC X(64).                               
013200     EJECT                                                                
013300*    --- IMS FUNKTIONSKODER                                               
013400*01  -COPY W0003                                                          
013500     EJECT                                                                
013600*    ---  DLI INPUT-OUTPUT AREA                                           
013700 01  FILLER                      PIC X(16)                                
013800                                     VALUE 'DLI-IO-KOMA01'.               
013900 01  DLI-IO-KOMA01.                                                       
014000*  03  -COPY WDP801     -PRE KOMA-                                        
014100     EJECT                                                                
014200 01  FILLER                      PIC X(16)                                
014300                                     VALUE 'DLI-IO-KOMA11'.               
014400 01  DLI-IO-KOMA11.                                                       
014500*  03  -COPY WDP811     -PRE KOMA-                                        
014600     EJECT                                                                
014700 LINKAGE SECTION.                                                         
014800*01  -COPY W0009      -PRE MSG-                                           
014900                                                                          
015000*01  -COPY W0009      -PRE ALT-                                           
015100     EJECT                                                                
015200*01  -COPY W0008      -PRE USEA-                                          
015300     05  FILLER                  PIC X.                                   
015400                                                                          
015500*01  -COPY W0008      -PRE KOMA-                                          
015600     05  FILLER                  PIC X.                                   
015700     EJECT                                                                
015800 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB KOMA-PCB.             
015900 MAIN SECTION.                                                            
016000     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB KOMA-PCB.             
016100                                                                          
016200     PERFORM IMS-GET-MSG                                                  
016300     IF SEGMENT-FINNS                                                     
016400       PERFORM A-INIT                                                     
016500       PERFORM B-KOLLA-NYCKLAR                                            
016600       IF NYCKLAR-OK                                                      
016700         IF MFS-UPDATE OR MFS-UPD-X                                       
016800           PERFORM G-KOLLA-INPUT                                          
016900           IF INDATA-OK                                                   
017000             PERFORM H-UPPDATERA                                          
017100           END-IF                                                         
017200         ELSE                                                             
017300           IF MFS-FIRST                                                   
017400             PERFORM C-FOERSTA-SIDA                                       
017500           ELSE                                                           
017600             IF MFS-NEXT                                                  
017700               PERFORM D-NAESTA-SIDA                                      
017800             ELSE                                                         
017900               PERFORM E-SAMMA-SIDA                                       
018000             END-IF                                                       
018100           END-IF                                                         
018200           IF ALLT-OK                                                     
018300             PERFORM F-LAES-VISA-INFO                                     
018400           END-IF                                                         
018500         END-IF                                                           
018600       END-IF                                                             
018700       COMPUTE MSG-KVLL = LENGTH OF MOD-W0O62101 + 4                      
018800       PERFORM IMS-INSERT-MSG                                             
018900     END-IF                                                               
019000                                                                          
019100     MOVE ZERO TO RETURN-CODE                                             
019200     GOBACK                                                               
019300     .                                                                    
019400     EJECT                                                                
019500 A-INIT SECTION.                                                          
019600     MOVE WHEN-COMPILED TO W-COMPILED                                     
019700                                                                          
019800     IF MSG-DUBBLA-TRANSKODER                                             
019900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W0I62101                 
020000       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
020100       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
020200     ELSE                                                                 
020300       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W0I62101                  
020400       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
020500       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
020600     END-IF                                                               
020700     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
020800     MOVE MSG-IDPFK TO MFS-IDPFK                                          
020900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
021000                                                                          
021100     MOVE LOW-VALUE TO MSG-AREA                                           
021200     MOVE 'W0O62101' TO MFS-IDMOD                                         
021300     MOVE '0621' TO MOD-IDTRANS                                           
021400     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
021500     MOVE SPACE TO MED-WMEDAREA                                           
021600                                                                          
021700     IF EGEN-MID                                                          
021800       IF      MID-IDSNDNOD   = ALL '+'                                   
021900           AND MID-IDSNDJOB   = ALL '+'                                   
022000           AND MID-TIREGDAT   = ALL '+'                                   
022100           AND MID-TIKLOCK    = ALL '+'                                   
022200         CONTINUE                                                         
022300       ELSE                                                               
022400         MOVE SPACE TO MFS-KDTRTYP                                        
022500         MOVE '7' TO MFS-IDPFK                                            
022600       END-IF                                                             
022700     ELSE                                                                 
022800       IF NOT MFS-UPD-X                                                   
022900         MOVE SPACE TO MFS-KDTRTYP                                        
023000         MOVE '7' TO MFS-IDPFK                                            
023100       END-IF                                                             
023200     END-IF                                                               
023300     .                                                                    
023400     EJECT                                                                
023500 B-KOLLA-NYCKLAR SECTION.                                                 
023600                                                                          
023700     MOVE ALL '+'           TO MSGI-WMSGINIT                              
023800     MOVE '001'             TO MSGI-KDCALL                                
023900     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
024000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
024100     MOVE '0621'            TO MSGI-IDTRANS                               
024200     IF GODK-MID                                                          
024300       MOVE MID-IDSNDNOD    TO MSGI-IDSNDNOD                              
024400       MOVE MID-IDSNDJOB    TO MSGI-IDSNDJOB                              
024500       MOVE MID-TIREGDAT    TO MSGI-TIREGDAT                              
024600       MOVE MID-TIKLOCK     TO MSGI-TIKLOCK                               
024700       IF MID-IDRADNR-IN = ALL '+'                                        
024800         MOVE MID-IDRADNR-UT TO MSGI-IDRADNR                              
024900       ELSE                                                               
025000         MOVE MID-IDRADNR-IN  TO MSGI-IDRADNR                             
025100       END-IF                                                             
025200     END-IF                                                               
025300     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
025400     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
025500                                                                          
025600     MOVE JA TO NYCKLAR-SW                                                
025700                                                                          
025800     MOVE MFS-RENSA-FAELT TO MOD-IDSNDNOD-IN                              
025900     MOVE MSGI-IDSNDNOD TO W-IDSNDNOD                                     
026000                                                                          
026100     MOVE MFS-RENSA-FAELT TO MOD-IDSNDJOB-IN                              
026200     MOVE MSGI-IDSNDJOB TO W-IDSNDJOB                                     
026300                                                                          
026400     MOVE MFS-RENSA-FAELT TO MOD-TIREGDAT-IN                              
026500     INSPECT MSGI-TIREGDAT REPLACING LEADING SPACE BY ZERO                
026600     IF MSGI-TIREGDAT NUMERIC AND MSGI-TIREGDAT > ZERO                    
026700       MOVE MSGI-TIREGDAT TO W-TIREGDAT                                   
026800     ELSE                                                                 
026900       MOVE NEJ TO NYCKLAR-SW                                             
027000     END-IF                                                               
027100                                                                          
027200     MOVE MFS-RENSA-FAELT TO MOD-TIKLOCK-IN                               
027300     INSPECT MSGI-TIKLOCK REPLACING LEADING SPACE BY ZERO                 
027400     IF MSGI-TIKLOCK NUMERIC AND MSGI-TIKLOCK > ZERO                      
027500       MOVE MSGI-TIKLOCK TO W-TIKLOCK                                     
027600     ELSE                                                                 
027700       MOVE NEJ TO NYCKLAR-SW                                             
027800     END-IF                                                               
027900                                                                          
028000     MOVE MFS-RENSA-FAELT TO MOD-IDRADNR-IN                               
028100     INSPECT MSGI-IDRADNR REPLACING LEADING SPACE BY ZERO                 
028200     IF MSGI-IDRADNR NUMERIC AND MSGI-IDRADNR > ZERO                      
028300       MOVE MSGI-IDRADNR TO W-IDRADNR                                     
028400     ELSE                                                                 
028500       MOVE +0 TO W-IDRADNR                                               
028600       MOVE '0000' TO MSGI-IDRADNR                                        
028700     END-IF                                                               
028800                                                                          
028900     IF GODK-MID OR NYCKLAR-OK                                            
029000       MOVE MSGI-IDSNDNOD TO MOD-IDSNDNOD-UT                              
029100       MOVE MSGI-IDSNDJOB TO MOD-IDSNDJOB-UT                              
029200       MOVE MSGI-TIREGDAT TO MOD-TIREGDAT-UT                              
029300       MOVE MSGI-TIKLOCK  TO MOD-TIKLOCK-UT                               
029400       MOVE MSGI-IDRADNR  TO MOD-IDRADNR-UT                               
029500       INSPECT MOD-IDRADNR-UT REPLACING LEADING ZERO BY SPACE             
029600     ELSE                                                                 
029700       MOVE MFS-RENSA-FAELT TO MOD-IDSNDNOD-UT                            
029800                               MOD-IDSNDJOB-UT                            
029900                               MOD-TIREGDAT-UT                            
030000                               MOD-TIKLOCK-UT                             
030100                               MOD-IDRADNR-UT                             
030200     END-IF                                                               
030300                                                                          
030400     IF NYCKLAR-FEL                                                       
030500       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
030600       CALL WMEDKONV USING MED-WMEDAREA                                   
030700       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
030800       PERFORM MFS-RENSA-FAELT-IN                                         
030900       PERFORM MFS-RENSA-FAELT-UT                                         
031000     END-IF                                                               
031100     .                                                                    
031200     EJECT                                                                
031300 C-FOERSTA-SIDA SECTION.                                                  
031400                                                                          
031500     MOVE NEJ TO UNIK-SW                                                  
031600     MOVE ZERO TO W-IDRADNR                                               
031700     MOVE INF-FIRST-PAGE TO MED-IDMFSFEL                                  
031800     CALL WMEDKONV USING MED-WMEDAREA                                     
031900     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
032000     MOVE JA TO ALLT-SW                                                   
032100     .                                                                    
032200                                                                          
032300                                                                          
032400                                                                          
032500 D-NAESTA-SIDA SECTION.                                                   
032600                                                                          
032700     MOVE NEJ TO UNIK-SW                                                  
032800     MOVE JA TO ALLT-SW                                                   
032900     IF MID-IDRADNR-IN = ALL '+'                                          
033000       ADD +1 TO W-IDRADNR                                                
033100     END-IF                                                               
033200     .                                                                    
033300                                                                          
033400                                                                          
033500                                                                          
033600 E-SAMMA-SIDA SECTION.                                                    
033700                                                                          
033800     IF MID-TRANSDATA = '++++++++++'                                      
033900       MOVE JA TO ALLT-SW                                                 
034000       MOVE JA TO UNIK-SW                                                 
034100     ELSE                                                                 
034200       MOVE NEJ TO ALLT-SW                                                
034300       MOVE INF-PRESS-PF11 TO MED-IDMFSINF                                
034400       CALL WMEDKONV USING MED-WMEDAREA                                   
034500       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
034600       PERFORM MFS-ROER-EJ-FAELT-IN                                       
034700       PERFORM MFS-ROER-EJ-FAELT-UT                                       
034800       PERFORM MFS-LAES-IN-IGEN                                           
034900     END-IF                                                               
035000     .                                                                    
035100     EJECT                                                                
035200 F-LAES-VISA-INFO SECTION.                                                
035300                                                                          
035400     PERFORM MFS-RENSA-FAELT-IN                                           
035500     PERFORM IMS-GET-KOMA-ROT                                             
035600                                                                          
035700     IF SEGMENT-FINNS                                                     
035800       MOVE KOMA-KOM-IDCPYTXT TO MOD-IDCPYTXT                             
035900       MOVE KOMA-KOM-IDLTERM  TO MOD-IDLTERM                              
036000       MOVE KOMA-KOM-IDMFSMED TO MOD-IDMFSMED                             
036100       MOVE KOMA-KOM-IDUSER   TO MOD-IDUSER                               
036200       MOVE KOMA-KOM-KDKOMSTA TO MOD-KDKOMSTA                             
036300       MOVE KOMA-KOM-KDKOMBEH TO MOD-KDKOMBEH                             
036400       IF UNIK-SW = JA                                                    
036500         PERFORM IMS-GET-KOMA-TRANS-UNIK                                  
036600       ELSE                                                               
036700         PERFORM IMS-GET-KOMA-TRANS                                       
036800       END-IF                                                             
036900       IF SEGMENT-FINNS                                                   
037000         MOVE KOMA-TRAN-IDRADNR TO WS-IDRADNR-RED                         
037100         MOVE WS-IDRADNR-RED TO MOD-IDRADNR-UT                            
037200         MOVE KOMA-TRAN-IDUSER TO MOD-IDUSER-TRAN                         
037300         MOVE KOMA-TRAN-TRANSDATA TO MOD-TRANSDATA                        
037400       ELSE                                                               
037500         MOVE ERR-TRANS-MISS TO MED-IDMFSFEL                              
037600         CALL WMEDKONV USING MED-WMEDAREA                                 
037700         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
037800         MOVE W-IDRADNR TO WS-IDRADNR-RED                                 
037900         MOVE WS-IDRADNR-RED TO MOD-IDRADNR-UT                            
038000         MOVE MFS-RENSA-FAELT TO MOD-IDUSER-TRAN                          
038100                                 MOD-TRANSDATA                            
038200       END-IF                                                             
038300     ELSE                                                                 
038400       MOVE ERR-TRANS-MISS TO MED-IDMFSFEL                                
038500       CALL WMEDKONV USING MED-WMEDAREA                                   
038600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
038700       PERFORM MFS-RENSA-FAELT-UT                                         
038800     END-IF                                                               
038900     .                                                                    
039000     EJECT                                                                
039100 G-KOLLA-INPUT SECTION.                                                   
039200                                                                          
039300     MOVE JA  TO INDATA-SW                                                
039400     IF MID-KDCMDVAL = 'I-R' OR 'INS' OR 'REP'                            
039500                    OR 'D-R' OR 'DEL' OR 'STA'                            
039600       IF MID-KDCMDVAL = 'I-R' OR 'INS' OR 'REP'                          
039700         IF MID-TRANSDATA = '++++++++++'                                  
039800           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMDVAL-ATTR                   
039900           MOVE NEJ TO INDATA-SW                                          
040000         ELSE                                                             
040100           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMDVAL-ATTR                 
040200           MOVE MFS-ALFA-FAELT-RAETT TO MOD-TRANSDATA-ATTR                
040300         END-IF                                                           
040400       ELSE                                                               
040500         IF MID-TRANSDATA = '++++++++++'                                  
040600           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMDVAL-ATTR                 
040700         ELSE                                                             
040800           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMDVAL-ATTR                   
040900           MOVE MFS-ALFA-FAELT-FEL TO MOD-TRANSDATA-ATTR                  
041000           MOVE NEJ TO INDATA-SW                                          
041100         END-IF                                                           
041200       END-IF                                                             
041300     ELSE                                                                 
041400       MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMDVAL-ATTR                       
041500       MOVE NEJ TO INDATA-SW                                              
041600     END-IF                                                               
041700                                                                          
041800     IF INDATA-FEL                                                        
041900       MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                          
042000       CALL WMEDKONV USING MED-WMEDAREA                                   
042100       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
042200       PERFORM MFS-ROER-EJ-FAELT-IN                                       
042300       PERFORM MFS-ROER-EJ-FAELT-UT                                       
042400     END-IF                                                               
042500     .                                                                    
042600     EJECT                                                                
042700 H-UPPDATERA SECTION.                                                     
042800                                                                          
042900     EVALUATE MID-KDCMDVAL                                                
043000       WHEN 'I-R' PERFORM HA-INSERT-ROT                                   
043100                  PERFORM HB-INSERT                                       
043200       WHEN 'INS' PERFORM HB-INSERT                                       
043300       WHEN 'REP' PERFORM HC-REPLACE                                      
043400       WHEN 'D-R' PERFORM HD-DELETE-ROT                                   
043500       WHEN 'DEL' PERFORM HE-DELETE                                       
043600       WHEN 'STA' PERFORM HF-STARTA                                       
043700     END-EVALUATE                                                         
043800                                                                          
043900     IF INDATA-FEL                                                        
044000       MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                          
044100       CALL WMEDKONV USING MED-WMEDAREA                                   
044200       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
044300       MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMDVAL-ATTR                       
044400       PERFORM MFS-ROER-EJ-FAELT-IN                                       
044500       PERFORM MFS-ROER-EJ-FAELT-UT                                       
044600     ELSE                                                                 
044700       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
044800       CALL WMEDKONV USING MED-WMEDAREA                                   
044900       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
045000       PERFORM MFS-FORM-ATTR                                              
045100       PERFORM MFS-ROER-EJ-FAELT-UT                                       
045200       MOVE MFS-RENSA-FAELT TO MOD-KDCMDVAL                               
045300       MOVE MFS-ROER-EJ-FAELT TO MOD-TRANSDATA                            
045400     END-IF                                                               
045500     .                                                                    
045600     EJECT                                                                
045700 HA-INSERT-ROT SECTION.                                                   
045800                                                                          
045900     CALL VIMSID USING W-IMSID                                            
046000     MOVE W-VIMSID TO KOMA-KOM-IDSNDNOD                                   
046100                      MOD-IDSNDNOD-UT                                     
046200                      W-IDSNDNOD                                          
046300     MOVE 'W0062100' TO KOMA-KOM-IDSNDJOB                                 
046400                        MOD-IDSNDJOB-UT                                   
046500                        W-IDSNDJOB                                        
046600     ACCEPT WS-DATE FROM DATE                                             
046700     MOVE WS-DATE TO KOMA-KOM-TIREGDAT                                    
046800                     MOD-TIREGDAT-UT                                      
046900                     W-TIREGDAT                                           
047000     ACCEPT WS-TIME FROM TIME                                             
047100     MOVE WS-TIME TO KOMA-KOM-TIKLOCK                                     
047200                     MOD-TIKLOCK-UT                                       
047300                     W-TIKLOCK                                            
047400     MOVE 'SCREEN ' TO KOMA-KOM-IDCPYTXT                                  
047500                       MOD-IDCPYTXT                                       
047600     MOVE MSG-LTERM-NAME TO KOMA-KOM-IDLTERM                              
047700                            MOD-IDLTERM                                   
047800     MOVE MSG-SIGNON-USERID TO KOMA-KOM-IDUSER                            
047900                               MOD-IDUSER                                 
048000     MOVE '121' TO KOMA-KOM-IDMFSMED                                      
048100                   MOD-IDMFSMED                                           
048200     MOVE ' ' TO KOMA-KOM-KDKOMSTA                                        
048300                 MOD-KDKOMSTA                                             
048400     MOVE ' ' TO KOMA-KOM-KDKOMBEH                                        
048500                 MOD-KDKOMBEH                                             
048600     PERFORM IMS-ISRT-KOMA-ROT                                            
048700     IF SEGMENT-FINNS-REDAN                                               
048800       MOVE NEJ TO INDATA-SW                                              
048900     END-IF                                                               
049000     .                                                                    
049100     EJECT                                                                
049200 HB-INSERT SECTION.                                                       
049300                                                                          
049400     MOVE W-IDRADNR TO KOMA-TRAN-IDRADNR                                  
049500     MOVE MSG-SIGNON-USERID TO KOMA-TRAN-IDUSER                           
049600     MOVE +1204 TO KOMA-TRAN-KVLL                                         
049700     MOVE LOW-VALUE TO KOMA-TRAN-KDZ1                                     
049800                       KOMA-TRAN-KDZ2                                     
049900     MOVE MID-TRANSDATA TO KOMA-TRAN-TRANSDATA                            
050000     PERFORM IMS-ISRT-KOMA-TRANS                                          
050100     IF SEGMENT-FINNS-REDAN OR SEGMENT-SAKNAS                             
050200       MOVE NEJ TO INDATA-SW                                              
050300     END-IF                                                               
050400     .                                                                    
050500     EJECT                                                                
050600 HC-REPLACE SECTION.                                                      
050700                                                                          
050800     PERFORM IMS-GET-KOMA-ROT                                             
050900     IF SEGMENT-FINNS                                                     
051000       PERFORM IMS-GET-KOMA-TRANS-UNIK                                    
051100       IF SEGMENT-FINNS                                                   
051200         MOVE +1204 TO KOMA-TRAN-KVLL                                     
051300         MOVE MSG-SIGNON-USERID TO KOMA-TRAN-IDUSER                       
051400         MOVE MID-TRANSDATA TO KOMA-TRAN-TRANSDATA                        
051500         PERFORM IMS-REPL-KOMA-TRANS                                      
051600       ELSE                                                               
051700         MOVE NEJ TO INDATA-SW                                            
051800       END-IF                                                             
051900     ELSE                                                                 
052000       MOVE NEJ TO INDATA-SW                                              
052100     END-IF                                                               
052200     .                                                                    
052300     EJECT                                                                
052400 HD-DELETE-ROT SECTION.                                                   
052500                                                                          
052600     PERFORM IMS-GET-KOMA-ROT                                             
052700     IF SEGMENT-FINNS                                                     
052800       PERFORM IMS-DLET-KOMA-ROT                                          
052900     ELSE                                                                 
053000       MOVE NEJ TO INDATA-SW                                              
053100     END-IF                                                               
053200     .                                                                    
053300     EJECT                                                                
053400 HE-DELETE SECTION.                                                       
053500                                                                          
053600     PERFORM IMS-GET-KOMA-ROT                                             
053700     IF SEGMENT-FINNS                                                     
053800       PERFORM IMS-GET-KOMA-TRANS-UNIK                                    
053900       IF SEGMENT-FINNS                                                   
054000         PERFORM IMS-DLET-KOMA-TRANS                                      
054100       ELSE                                                               
054200         MOVE NEJ TO INDATA-SW                                            
054300       END-IF                                                             
054400     ELSE                                                                 
054500       MOVE NEJ TO INDATA-SW                                              
054600     END-IF                                                               
054700     .                                                                    
054800     EJECT                                                                
054900 HF-STARTA SECTION.                                                       
055000                                                                          
055100     PERFORM IMS-GET-KOMA-ROT                                             
055200     IF SEGMENT-FINNS                                                     
055300       MOVE +0 TO W-IDRADNR                                               
055400       PERFORM IMS-GET-KOMA-TRANS                                         
055500       IF SEGMENT-FINNS                                                   
055600         PERFORM IMS-GET-KOMA-ROT                                         
055700         MOVE 'K' TO KOMA-KOM-KDKOMSTA                                    
055800         MOVE ' ' TO KOMA-KOM-KDKOMBEH                                    
055900         PERFORM IMS-REPL-KOMA-ROT                                        
056000         MOVE +54 TO MSG-KOM-KVLL                                         
056100         MOVE LOW-VALUE TO MSG-KOM-KDZ1                                   
056200                           MSG-KOM-KDZ2                                   
056300         MOVE 'W0T693X ' TO MSG-KOM-KDTRANS                               
056400         MOVE KOMA-KOM-IDCPYTXT TO MSG-KOM-IDCPYTXT                       
056500         MOVE KOMA-KOM-IDSNDNOD TO MSG-KOM-IDSNDNOD                       
056600         MOVE KOMA-KOM-IDSNDJOB TO MSG-KOM-IDSNDJOB                       
056700         MOVE KOMA-KOM-TIREGDAT TO MSG-KOM-TIREGDAT                       
056800         MOVE KOMA-KOM-TIKLOCK TO MSG-KOM-TIKLOCK                         
056900         MOVE SPACE TO MSG-KOM-IDMFSMED                                   
057000                       MSG-KOM-KDSVAR                                     
057100         PERFORM IMS-INSERT-KOM-MSG                                       
057200       ELSE                                                               
057300         MOVE NEJ TO INDATA-SW                                            
057400       END-IF                                                             
057500     ELSE                                                                 
057600       MOVE NEJ TO INDATA-SW                                              
057700     END-IF                                                               
057800     .                                                                    
057900     EJECT                                                                
058000 MFS-RENSA-FAELT-UT SECTION.                                              
058100                                                                          
058200*    --- ALLA UTDATA-FÄLT                                                 
058300     MOVE MFS-RENSA-FAELT TO MOD-IDCPYTXT                                 
058400                             MOD-IDLTERM                                  
058500                             MOD-IDUSER                                   
058600                             MOD-IDMFSMED                                 
058700                             MOD-KDKOMSTA                                 
058800                             MOD-KDKOMBEH                                 
058900                             MOD-IDUSER-TRAN                              
059000     .                                                                    
059100                                                                          
059200 MFS-RENSA-FAELT-IN SECTION.                                              
059300                                                                          
059400*    --- ALLA INDATA-FÄLT                                                 
059500     MOVE MFS-RENSA-FAELT TO MOD-KDCMDVAL                                 
059600                             MOD-TRANSDATA                                
059700     .                                                                    
059800                                                                          
059900 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
060000                                                                          
060100*    --- ALLA UTDATA-FÄLT                                                 
060200     MOVE MFS-ROER-EJ-FAELT TO MOD-IDCPYTXT                               
060300                               MOD-IDLTERM                                
060400                               MOD-IDUSER                                 
060500                               MOD-IDMFSMED                               
060600                               MOD-KDKOMSTA                               
060700                               MOD-KDKOMBEH                               
060800                               MOD-IDUSER-TRAN                            
060900     .                                                                    
061000                                                                          
061100 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
061200                                                                          
061300*    --- ALLA INDATA-FÄLT                                                 
061400     MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMDVAL                               
061500                               MOD-TRANSDATA                              
061600     .                                                                    
061700     EJECT                                                                
061800 MFS-FORM-ATTR SECTION.                                                   
061900                                                                          
062000*    --- ALLA INDATA-FÄLT                                                 
062100     MOVE MFS-FORMATETS-ATTR TO MOD-KDCMDVAL-ATTR                         
062200                                MOD-TRANSDATA-ATTR                        
062300     .                                                                    
062400                                                                          
062500 MFS-LAES-IN-IGEN SECTION.                                                
062600                                                                          
062700*    --- ALLA INDATA-FÄLT                                                 
062800     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMDVAL-ATTR                      
062900                                   MOD-TRANSDATA-ATTR                     
063000     .                                                                    
063100     EJECT                                                                
063200* --- IMS SEKTIONER ---                                                   
063300                                                                          
063400 IMS-GET-MSG SECTION.                                                     
063500                                                                          
063600     MOVE '  QC' TO GODK-STATUSKODER                                      
063700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
063800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
063900     PERFORM IMS-STATUSKONTROLL                                           
064000     .                                                                    
064100                                                                          
064200                                                                          
064300 IMS-INSERT-MSG SECTION.                                                  
064400                                                                          
064500     IF MSGI-IDLAND-SPR = 'GB'                                            
064600       MOVE '0' TO MFS-KDHUVOMR                                           
064700     END-IF                                                               
064800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
064900     MOVE SPACE TO GODK-STATUSKODER                                       
065000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
065100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
065200     PERFORM IMS-STATUSKONTROLL                                           
065300     .                                                                    
065400                                                                          
065500                                                                          
065600 IMS-INSERT-KOM-MSG SECTION.                                              
065700                                                                          
065800     MOVE SPACE TO GODK-STATUSKODER                                       
065900     CALL CBLTDLI USING ISRT ALT-PCB MSG-KOM-WMSGKOM                      
066000     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
066100     PERFORM IMS-STATUSKONTROLL                                           
066200     .                                                                    
066300     EJECT                                                                
066400 IMS-GET-KOMA-ROT SECTION.                                                
066500     STRING 'WLKOMA01(WDP801KY =' W-WDP801KY-X ')'                        
066600          DELIMITED BY SIZE INTO SSA1                                     
066700     MOVE '  GE' TO GODK-STATUSKODER                                      
066800     CALL CBLTDLI USING GHU KOMA-PCB DLI-IO-KOMA01 SSA1                   
066900     MOVE KOMA-STATUS-CODE TO STATUS-WS                                   
067000     PERFORM IMS-STATUSKONTROLL                                           
067100     .                                                                    
067200                                                                          
067300                                                                          
067400 IMS-ISRT-KOMA-ROT SECTION.                                               
067500                                                                          
067600     MOVE 'WLKOMA01 ' TO SSA1                                             
067700     MOVE '  II' TO GODK-STATUSKODER                                      
067800     CALL CBLTDLI USING ISRT KOMA-PCB DLI-IO-KOMA01 SSA1                  
067900     MOVE KOMA-STATUS-CODE TO STATUS-WS                                   
068000     PERFORM IMS-STATUSKONTROLL                                           
068100     .                                                                    
068200     EJECT                                                                
068300 IMS-GET-KOMA-TRANS SECTION.                                              
068400     STRING 'WLKOMA11(IDRADNR =>' W-IDRADNR-X ')'                         
068500          DELIMITED BY SIZE INTO SSA1                                     
068600     MOVE '  GE' TO GODK-STATUSKODER                                      
068700     CALL CBLTDLI USING GNP KOMA-PCB DLI-IO-KOMA11 SSA1                   
068800     MOVE KOMA-STATUS-CODE TO STATUS-WS                                   
068900     PERFORM IMS-STATUSKONTROLL                                           
069000     .                                                                    
069100                                                                          
069200                                                                          
069300 IMS-GET-KOMA-TRANS-UNIK SECTION.                                         
069400     STRING 'WLKOMA11(IDRADNR  =' W-IDRADNR-X ')'                         
069500          DELIMITED BY SIZE INTO SSA1                                     
069600     MOVE '  GE' TO GODK-STATUSKODER                                      
069700     CALL CBLTDLI USING GHNP KOMA-PCB DLI-IO-KOMA11 SSA1                  
069800     MOVE KOMA-STATUS-CODE TO STATUS-WS                                   
069900     PERFORM IMS-STATUSKONTROLL                                           
070000     .                                                                    
070100                                                                          
070200                                                                          
070300 IMS-ISRT-KOMA-TRANS SECTION.                                             
070400                                                                          
070500     STRING 'WLKOMA01(WDP801KY =' W-WDP801KY-X ')'                        
070600          DELIMITED BY SIZE INTO SSA1                                     
070700     MOVE 'WLKOMA11 ' TO SSA2                                             
070800     MOVE '  GEII' TO GODK-STATUSKODER                                    
070900     CALL CBLTDLI USING ISRT KOMA-PCB DLI-IO-KOMA11 SSA1 SSA2             
071000     MOVE KOMA-STATUS-CODE TO STATUS-WS                                   
071100     PERFORM IMS-STATUSKONTROLL                                           
071200     .                                                                    
071300     EJECT                                                                
071400 IMS-REPL-KOMA-ROT SECTION.                                               
071500                                                                          
071600     MOVE '  ' TO GODK-STATUSKODER                                        
071700     CALL CBLTDLI USING REPL KOMA-PCB DLI-IO-KOMA01                       
071800     MOVE KOMA-STATUS-CODE TO STATUS-WS                                   
071900     PERFORM IMS-STATUSKONTROLL                                           
072000     .                                                                    
072100                                                                          
072200                                                                          
072300 IMS-DLET-KOMA-ROT SECTION.                                               
072400                                                                          
072500     MOVE '  ' TO GODK-STATUSKODER                                        
072600     CALL CBLTDLI USING DLET KOMA-PCB DLI-IO-KOMA01                       
072700     MOVE KOMA-STATUS-CODE TO STATUS-WS                                   
072800     PERFORM IMS-STATUSKONTROLL                                           
072900     .                                                                    
073000                                                                          
073100                                                                          
073200 IMS-REPL-KOMA-TRANS SECTION.                                             
073300                                                                          
073400     MOVE '  ' TO GODK-STATUSKODER                                        
073500     CALL CBLTDLI USING REPL KOMA-PCB DLI-IO-KOMA11                       
073600     MOVE KOMA-STATUS-CODE TO STATUS-WS                                   
073700     PERFORM IMS-STATUSKONTROLL                                           
073800     .                                                                    
073900                                                                          
074000                                                                          
074100 IMS-DLET-KOMA-TRANS SECTION.                                             
074200                                                                          
074300     MOVE '  ' TO GODK-STATUSKODER                                        
074400     CALL CBLTDLI USING DLET KOMA-PCB DLI-IO-KOMA11                       
074500     MOVE KOMA-STATUS-CODE TO STATUS-WS                                   
074600     PERFORM IMS-STATUSKONTROLL                                           
074700     .                                                                    
074800     EJECT                                                                
074900 IMS-STATUSKONTROLL SECTION.                                              
075000                                                                          
075100     SET STATUS-IX TO 1                                                   
075200     SEARCH GODK-STATUS                                                   
075300       AT END                                                             
075400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
075500         DELIMITED BY SIZE INTO FELTEXT                                   
075600         CALL FELLOG                                                      
075700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
075800         CONTINUE                                                         
075900     END-SEARCH                                                           
076000     .                                                                    
