000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2013400.                                                
000400 AUTHOR.         CAMELIA OLGRENER.                                        
000500 DATE-WRITTEN.   SEPT. 90.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*                                                                         
001100*        MÖJLIGHETER: PROGRAMMET VISAR OCH UPPDATERAR VISS                
001200*                     ARTIKELINFO.                                        
001300*                                                                         
001400*                     PROGRAMMET LÄSER : WDK6 (ARTREG)                    
001500*                                        WDD3 (BENÄMNINGSREG)             
001600*                                                                         
001700*                     PROGRAMMET UPPDATERAR : WDK6 (ARTREG)               
001800*                                             WDR5 - HTYP 2227            
001900*                                                         2228            
002000*                                                                         
002100*    INDATA.                                                              
002200*        TRANSAKTION: W2T134  W2T134U                                     
002300*        MID:         W2I13401                                            
002400*                                                                         
002500*    UTDATA.                                                              
002600*        MOD:         W2O13401                                            
002700*                                                                         
002800*   ÄNDRINGAR:                                                            
002900*        03-05-15. TILLAGT FUNKTION FÖR ATT BEGRÄNSA INFORMATION          
003000*                  FÖR USER VARS SEC-IDLEVNR PÅ USER-BASEN                
003100*                  INTE ÄR LIKA MED HUVUDLEVERANTÖREN.                    
003200*                  ( SEC-IDLEVNR = SPACE, FÅR SE ALLT )    /C.E.          
003300*                                                                         
003400                                                                          
003500     SKIP3                                                                
003600 ENVIRONMENT DIVISION.                                                    
003700 DATA DIVISION.                                                           
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000*    -COPY WY2000W2                                                       
004100     SKIP3                                                                
004200 77  IDPGM                   PIC X(8)    VALUE 'W4036600'.                
004300 77  JA                      PIC X       VALUE 'J'.                       
004310 77  YES                     PIC X       VALUE 'Y'.                       
004400 77  NEJ                     PIC X       VALUE 'N'.                       
004500                                                                          
004600 77  IDARTNR-WS              PIC X(9)    VALUE SPACE.                     
004700 77  WS-REST                 PIC 9(3)    VALUE ZERO.                      
004800                                                                          
004900 77  SPRAK-IX                PIC S9(9)   VALUE +0   COMP SYNC.            
005000 77  MAX-MOD-LAENGD          PIC S9(4)   VALUE +250 COMP SYNC.            
005100                                                                          
005200 77  INDATA-SW               PIC X       VALUE 'J'.                       
005300   88  INDATA-OK                         VALUE 'J'.                       
005400   88  INDATA-FEL                        VALUE 'N'.                       
005500                                                                          
005600 77  ALLT-SW                 PIC X       VALUE 'J'.                       
005700   88  ALLT-OK                           VALUE 'J'.                       
005800   88  ALLT-FEL                          VALUE 'N'.                       
005900                                                                          
006000 77  NYCKLAR-SW              PIC X       VALUE 'J'.                       
006100   88  NYCKLAR-OK                        VALUE 'J'.                       
006200   88  NYCKLAR-FEL                       VALUE 'N'.                       
006300                                                                          
006400 77  UPPDAT-SW               PIC X       VALUE 'N'.                       
006500   88  UPPDATERING-OK                    VALUE 'J'.                       
006600   88  UPPDATERING-FEL                   VALUE 'N'.                       
006700                                                                          
006800 77  NUMERISK-SW             PIC X       VALUE 'J'.                       
006900   88  NUMERISK-OK                       VALUE 'J'.                       
007000   88  NUMERISK-FEL                      VALUE 'N'.                       
007100                                                                          
007200 77  W-IDTRANS               PIC X(4)    VALUE SPACE.                     
007300   88  EGEN-TRANS                        VALUE '2134'.                    
007400   88  GODK-TRANS                        VALUE '2131' '2132'              
007500                                               '2133' '2135'              
007600                                               '2136' '2137'              
007700                                               '2138' '2139'.             
007800******* DIVERSE SPARFÄLT *******                                          
007900                                                                          
008000 01  WS-FLMANOSK             PIC X       VALUE SPACE.                     
008100 01  WS-KVPB-TPO-C1.                                                      
008200   03  WS-KVPB-C1-HELTAL     PIC 9(6)    VALUE ZERO.                      
008300   03  WS-KVPB-C1-PUNKT      PIC X       VALUE SPACE.                     
008400   03  WS-KVPB-C1-DECIMAL    PIC 9       VALUE ZERO.                      
008500                                                                          
008600 01  WS-KVFRYSTI             PIC S9(3)   VALUE ZERO COMP-3.               
008700 01  WS-KDUART               PIC X       VALUE SPACE.                     
008800 01  WS-IDLEVNR              PIC X(5)    VALUE SPACE.                     
008900 01  WS-IDLEVNR-8            PIC X(8)    VALUE SPACE.                     
009000 01  WS-KDHF                 PIC S9      VALUE ZERO COMP-3.               
009100                                                                          
009200 01  WS-TIFINLV              PIC 9(5).                                    
009300 01  FILLER REDEFINES WS-TIFINLV.                                         
009400   03  WS-TIFINLV-AR         PIC 9(2).                                    
009500   03  WS-TIFINLV-VECKA      PIC 9(2).                                    
009600   03  WS-TIFINLV-DAG        PIC 9.                                       
009700                                                                          
009800 01  WS-PERIOD               PIC 9(5)    VALUE ZERO.                      
009900 01  FILLER REDEFINES WS-PERIOD.                                          
010000   03  WS-AAVV               PIC 9(4).                                    
010100   03  FILLER REDEFINES WS-AAVV.                                          
010200     05 WS-AR                PIC 9(2).                                    
010300     05 WS-VECKA             PIC 9(2).                                    
010400   03  WS-DAG                PIC 9.                                       
010500                                                                          
010600 01  SPAR-KVPB-C1-RED.                                                    
010700   03  SPAR-KVPB-C1-HELTAL   PIC 9(6)    VALUE ZERO.                      
010800   03  SPAR-KVPB-C1-DECIMAL  PIC 9       VALUE ZERO.                      
010900 01  SPAR-KVPB-TPO-C1-R REDEFINES SPAR-KVPB-C1-RED.                       
011000   03  SPAR-KVPB-TPO-C1      PIC 9(6)V9(1).                               
011100                                                                          
011200 01  SPAR-FLTPO1             PIC X          VALUE SPACE.                  
011300 01  SPAR-KVFRYSTI           PIC S9(3)      VALUE ZERO COMP-3.            
011400 01  SPAR-KDUART             PIC X          VALUE SPACE.                  
011500 01  SPAR-FLRADREF           PIC X          VALUE SPACE.                  
011600 01  SPAR-KDOPPLAN           PIC X          VALUE SPACE.                  
011700 01  SPAR-FLMANOSK           PIC X          VALUE SPACE.                  
011710 01  SPAR-FLAUTREL           PIC X          VALUE SPACE.                  
011800 01  SPAR-PRORDSK            PIC S9(5)V9(2) VALUE ZERO.                   
011900     EJECT                                                                
012000 01  GENERELLA-SUBPROGRAM.                                                
012100   03  WMEDKONV              PIC X(8)    VALUE 'WMEDKONV'.                
012200   03  WDATKONV              PIC X(8)    VALUE 'WDATKONV'.                
012300   03  WDECEDIT              PIC X(8)    VALUE 'WDECEDIT'.                
012400   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
012500   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
012600   03  W005INIT              PIC X(8)    VALUE 'W005INIT'.                
012700   03  W009VADD              PIC X(8)    VALUE 'W009VADD'.                
012800     EJECT                                                                
012900*   -COPY WMEDAREA                                                        
013000     SKIP3                                                                
013100 01  MESSAGE-CODES.                                                       
013200   03  ERR-CORR-HILITE-FLDS  PIC X(3)    VALUE '001'.                     
013300   03  INF-PRESS-PF11        PIC X(3)    VALUE '003'.                     
013400   03  ERR-PF11-AND-NO-DATA  PIC X(3)    VALUE '011'.                     
013500   03  ERR-ART-MISSING       PIC X(3)    VALUE '017'.                     
013600   03  ERR-NOT-NUM-KEY       PIC X(3)    VALUE '401'.                     
013700   03  INF-UPDATE-DONE       PIC X(3)    VALUE '101'.                     
013800   03  ERR-NOT-AUTHORIZED    PIC X(3)    VALUE '405'.                     
013900   03  INF-REFILL-PART       PIC X(3)    VALUE '434'.                     
014000     EJECT                                                                
014100 01      FELMEDDELANDEN.                                                  
014200   03    FEL-1           PIC X(40)                                        
014300                VALUE 'ARTIKELN ERSATT                         '.         
014400     EJECT                                                                
014500*   -COPY WDATAREA                                                        
014600     EJECT                                                                
014700*                    ****  PARAMETRAR TILL W005INIT                       
014800*   -COPY WMSGINIT                                                        
014900     EJECT                                                                
015000 01 FILLER               PIC X(24) VALUE 'DEC-EDIT AREA'.                 
015100*   -COPY WDECAREA                                                        
015200     EJECT                                                                
015300 01 FILLER               PIC X(24) VALUE 'W009VADD AREA'.                 
015400                                                                          
015500 01 VADD-AAVV            PIC S9(5) COMP-3.                                
015600 01 VADD-ANTAL           PIC S9(3) COMP-3.                                
015700     EJECT                                                                
015800******************************************************************        
015900*                                                                         
016000*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
016100*                                                                         
016200 01  FILLER                  PIC X(16)   VALUE 'MFS-WS'.                  
016300     SKIP3                                                                
016400*01  MID -COPY W2I13401                                                   
016500     EJECT                                                                
016600*01  -COPY WMSGAREA                                                       
016700     EJECT                                                                
016800*  03  MOD -COPY W2O13401 -RED MSG-AREA.                                  
016900     EJECT                                                                
017000*01  -COPY WMFSAREA                                                       
017100     EJECT                                                                
017200******************************************************************        
017300*                                                                         
017400*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017500*                                                                         
017600 01  IMS-WS.                                                              
017700   03  FILLER                PIC X(16)   VALUE 'IMS-WS     '.             
017800     SKIP3                                                                
017900*                        **** STATUS-KOD FRÅN IMS                         
018000   03  STATUS-WS             PIC XX.                                      
018100     88  SEGMENT-FINNS                   VALUE '  '.                      
018200     88  SEGMENT-FINNS-REDAN             VALUE 'II'.                      
018300     88  SEGMENT-SAKNAS                  VALUE 'GE'.                      
018400     88  SEGMENT-HOGRE                   VALUE 'GA'.                      
018500     SKIP3                                                                
018600   03  GODK-STATUSKODER.                                                  
018700     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018800     SKIP3                                                                
018900 01  NYCKLAR-TILL-DLI.                                                    
019000   03  W-WDK601-IDARTNR-X.                                                
019100     05  W-IDARTNR           PIC S9(9)   VALUE ZERO  COMP-3.              
019200                                                                          
019300   03  W-WDK611-KDSEGKEY-X.                                               
019400     05  W-KDSEGKEY-WDK611   PIC X       VALUE '1'.                       
019500                                                                          
019600   03  W-WDGXKEY-2227-X.                                                  
019700     05  W-IDHTYP-2227       PIC X(4)    VALUE '2227'.                    
019800     05  FILLER              PIC X(26)   VALUE LOW-VALUE.                 
019900                                                                          
020000   03  W-WDGXKEY-2228-X.                                                  
020100     05  W-IDARTNR-2228      PIC S9(9)   VALUE ZERO  COMP-3.              
020200     05  FILLER              PIC X(4)    VALUE LOW-VALUE.                 
020300                                                                          
020400*----SEKUNDÄR INDEX ARTBENÄMNING.                                         
020500                                                                          
020600   03  W-WDD3BSEQ-X.                                                      
020700     05  W-IDARTNR-WDD3BSEQ  PIC S9(9)   VALUE ZERO  COMP-3.              
020800                                                                          
020900   03  W-WDD3-IDSKYLT-X.                                                  
021000     05  W-IDSKYLT-WDD3      PIC X(3)    VALUE SPACE.                     
021100                                                                          
021200     SKIP2                                                                
021300 01    SSA1                  PIC X(64).                                   
021400 01    SSA2                  PIC X(64).                                   
021500 01    SSA3                  PIC X(64).                                   
021600     EJECT                                                                
021700*                            IMS FUNKTIONSKODER                           
021800*01    -COPY W0003                                                        
021900     EJECT                                                                
022000*                            DLI INPUT-OUTPUT AREA                        
022100 01  DLI-IO-AREA-01.                                                      
022200   03  IO-AREA-01            PIC X(150)  VALUE SPACE.                     
022300                                                                          
022400     SKIP2                                                                
022500*  03  WLARTC01  -COPY WDK601    -RED IO-AREA-01.                         
022600     EJECT                                                                
022700 01  DLI-IO-AREA-11.                                                      
022800   03  IO-AREA-11            PIC X(900)  VALUE SPACE.                     
022900                                                                          
023000     SKIP2                                                                
023100*  03  WLARTC11  -COPY WDK611    -RED IO-AREA-11.                         
023200     EJECT                                                                
023300 01  DLI-IO-AREA.                                                         
023400   03  IO-AREA               PIC X(150)  VALUE SPACE.                     
023500                                                                          
023600     SKIP2                                                                
023700*  03  WLBENA11  -COPY WDD311    -PRE BENA-    -RED IO-AREA.              
023800     EJECT                                                                
023900*  03  WLXXBW01  -COPY WDGX2228  -PRE XXBW-    -RED IO-AREA.              
024000     EJECT                                                                
024100                                                                          
024200 LINKAGE SECTION.                                                         
024300*01  -COPY W0009     -PRE MSG-                                            
024400     EJECT                                                                
024500*01  -COPY W0008     -PRE USEA-                                           
024600     05  FILLER              PIC X.                                       
024700     EJECT                                                                
024800*01  -COPY W0008     -PRE ARTC-                                           
024900     05  FILLER              PIC X.                                       
025000     EJECT                                                                
025100*01  -COPY W0008     -PRE BENA-                                           
025200     05  FILLER              PIC X.                                       
025300     EJECT                                                                
025400*01  -COPY W0008     -PRE XXBW-                                           
025500     05  FILLER              PIC X.                                       
025600                                                                          
025700 EJECT                                                                    
025800 PROCEDURE DIVISION USING MSG-PCB  USEA-PCB                               
025900                                   ARTC-PCB  BENA-PCB                     
026000                                   XXBW-PCB.                              
026100     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
026200                                   ARTC-PCB  BENA-PCB                     
026300                                   XXBW-PCB.                              
026400                                                                          
026500     PERFORM IMS-GET-MSG                                                  
026600     IF SEGMENT-FINNS                                                     
026700       PERFORM A-INIT                                                     
026800       PERFORM B-KOLLA-NYCKLAR                                            
026900       IF NYCKLAR-OK                                                      
027000         PERFORM IMS-GU-WDK601-ARTIKEL                                    
027100         IF SEGMENT-FINNS                                                 
027200            MOVE ART-IDLEVNR TO WS-IDLEVNR-8                              
027300*           --- KOLLA OM BEHÖRIG USER                                     
027400            IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                     
027500            OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE               
027600*              --- BEHÖRIG USER!                                          
027700               IF ART-KDERS-UTG = 0                                       
027800                  PERFORM C-LAES-SPARA-BENAEMNING                         
027900                  IF MFS-UPDATE                                           
028000                    PERFORM D-LAES-SPARA-DATA                             
028100                    PERFORM E-UPPDATERA                                   
028200                  ELSE                                                    
028300                    PERFORM F-BEHANDLA-ENTER                              
028400                  END-IF                                                  
028500                                                                          
028600                  PERFORM G-LAES-VISA-BILD                                
028700                ELSE                                                      
028800                  MOVE FEL-1 TO MOD-TEMFSFEL                              
028900                  PERFORM MFS-RENSA-FAELT-IN                              
029000                  PERFORM MFS-RENSA-FAELT-UT                              
029100               END-IF                                                     
029200            ELSE                                                          
029300*              --- OBEHÖRIG !                                             
029400               MOVE ERR-NOT-AUTHORIZED TO MED-IDMFSFEL                    
029500               CALL WMEDKONV USING MED-WMEDAREA                           
029600               MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                          
029700               PERFORM MFS-RENSA-FAELT-IN                                 
029800               PERFORM MFS-RENSA-FAELT-UT                                 
029900            END-IF                                                        
030000         ELSE                                                             
030100           MOVE ERR-ART-MISSING TO MED-IDMFSFEL                           
030200           CALL WMEDKONV USING MED-WMEDAREA                               
030300           MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                              
030400           PERFORM MFS-RENSA-FAELT-IN                                     
030500           PERFORM MFS-RENSA-FAELT-UT                                     
030600         END-IF                                                           
030700       END-IF                                                             
030800       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
030900       PERFORM IMS-INSERT-MSG                                             
031000     END-IF                                                               
031100                                                                          
031200     MOVE ZERO TO RETURN-CODE                                             
031300     GOBACK                                                               
031400     .                                                                    
031500     EJECT                                                                
031600 A-INIT SECTION.                                                          
031700                                                                          
031800     IF MSG-DUBBLA-TRANSKODER                                             
031900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I13401                 
032000       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
032100       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
032200     ELSE                                                                 
032300       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I13401                  
032400       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
032500       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
032600     END-IF                                                               
032700                                                                          
032800     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
032900     MOVE MSG-IDPFK TO MFS-IDPFK                                          
033000     MOVE MFS-IDTRANS TO W-IDTRANS                                        
033100                                                                          
033200     MOVE LOW-VALUE TO MSG-AREA                                           
033300     MOVE 'W2O13401' TO MFS-IDMOD                                         
033400     MOVE '2134' TO MOD-IDTRANS                                           
033500     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
033600                                                                          
033700     IF NOT EGEN-TRANS                                                    
033800       MOVE SPACE TO MFS-KDTRTYP                                          
033900       MOVE ' '   TO MFS-IDPFK                                            
034000       PERFORM MFS-RENSA-FAELT-IN                                         
034100     END-IF                                                               
034200                                                                          
034300     IF ENGLISH-TEXT                                                      
034400       MOVE +2    TO SPRAK-IX                                             
034500       MOVE 'GB ' TO MED-IDSKYLT                                          
034600     ELSE                                                                 
034700       MOVE 'S  ' TO MED-IDSKYLT                                          
034800       MOVE +1    TO SPRAK-IX                                             
034900     END-IF                                                               
035000     .                                                                    
035100     EJECT                                                                
035200 B-KOLLA-NYCKLAR SECTION.                                                 
035300                                                                          
035400     MOVE JA TO NYCKLAR-SW                                                
035500     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
035600                                                                          
035700     MOVE ALL '+' TO MSGI-WMSGINIT                                        
035800     MOVE '001'             TO MSGI-KDCALL                                
035900     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
036000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
036100     MOVE '2134'            TO MSGI-IDTRANS                               
036200     IF MFS-IDTRANS = '2134'                                              
036300     OR (MID-IDARTNR-IN NUMERIC                                           
036400     AND MID-IDARTNR-IN > ZERO)                                           
036500         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
036600     END-IF                                                               
036700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
036800     MOVE MSGI-IDARTNR TO IDARTNR-WS                                      
036900     INSPECT IDARTNR-WS REPLACING ALL SPACE BY ZERO                       
037000                                                                          
037100     IF MID-IDARTNR-IN = ALL '+'                                          
037200       CONTINUE                                                           
037300     ELSE                                                                 
037400       MOVE ' '            TO MFS-IDPFK                                   
037500       MOVE SPACE          TO MFS-KDTRTYP                                 
037600       PERFORM MFS-RENSA-FAELT-IN                                         
037700     END-IF                                                               
037800                                                                          
037900     IF IDARTNR-WS NUMERIC                                                
038000       MOVE IDARTNR-WS TO W-IDARTNR                                       
038100     ELSE                                                                 
038200       MOVE NEJ TO NYCKLAR-SW                                             
038300     END-IF                                                               
038400                                                                          
038500     IF GODK-TRANS OR NYCKLAR-OK                                          
038600       MOVE IDARTNR-WS TO MOD-IDARTNR-UT                                  
038700       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
038800     ELSE                                                                 
038900       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
039000     END-IF                                                               
039100                                                                          
039200     IF NYCKLAR-FEL                                                       
039300       MOVE ERR-NOT-NUM-KEY TO MED-IDMFSFEL                               
039400       CALL WMEDKONV USING MED-WMEDAREA                                   
039500       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
039600       PERFORM MFS-RENSA-FAELT-IN                                         
039700       PERFORM MFS-RENSA-FAELT-UT                                         
039800     END-IF                                                               
039900     .                                                                    
040000     EJECT                                                                
040100 C-LAES-SPARA-BENAEMNING SECTION.                                         
040200                                                                          
040300     MOVE ART-TIFINLV    TO WS-TIFINLV                                    
040400     MOVE ART-IDLEVNR    TO WS-IDLEVNR                                    
040500                                                                          
040600     MOVE ART-IDARTNR    TO W-IDARTNR-WDD3BSEQ                            
040700     MOVE 'S  '          TO W-IDSKYLT-WDD3                                
040800     PERFORM IMS-GU-BENA11-BENAEMNING                                     
040900     MOVE BENA-TEXT-BEART TO MOD-BEART                                    
041000     .                                                                    
041100     EJECT                                                                
041200 D-LAES-SPARA-DATA SECTION.                                               
041300                                                                          
041400     PERFORM IMS-GHU-WDK611                                               
041500     IF SEGMENT-FINNS                                                     
041600       MOVE CLAG-KDHF TO WS-KDHF                                          
041700       MOVE CLAG-FLMANOSK TO WS-FLMANOSK                                  
041800                                                                          
041900        IF (MID-KDUART-UPP NOT = ALL '+') OR                              
042000           (MID-KVFRYSTI-UPP   = ALL '+')                                 
042100                                                                          
042200          MOVE CLAG-KDUART TO  WS-KDUART                                  
042300                                                                          
042400          IF MID-KVFRYSTI-UPP = ALL '+'                                   
042500            MOVE CLAG-KVFRYSTI TO WS-KVFRYSTI                             
042600          END-IF                                                          
042700        END-IF                                                            
042800     END-IF                                                               
042900     .                                                                    
043000     EJECT                                                                
043100 E-UPPDATERA SECTION.                                                     
043200                                                                          
043300     IF MID-INPUT NOT = ALL '+'                                           
043400       PERFORM EA-KOLLA-INPUT                                             
043500                                                                          
043600       IF INDATA-OK AND UPPDATERING-OK                                    
043700                                                                          
043800         PERFORM EB-AENDRA-I-DB                                           
043900       END-IF                                                             
044000                                                                          
044100     ELSE                                                                 
044200       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
044300       CALL WMEDKONV USING MED-WMEDAREA                                   
044400       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
044500       MOVE NEJ        TO UPPDAT-SW                                       
044600     END-IF                                                               
044700                                                                          
044800     .                                                                    
044900     EJECT                                                                
045000 EA-KOLLA-INPUT SECTION.                                                  
045100                                                                          
045200     MOVE JA TO INDATA-SW                                                 
045300                                                                          
045400     IF MID-KVFRYSTI-UPP NOT = ALL '+'                                    
045500       IF MID-KVFRYSTI-UPP NUMERIC                                        
045600         MOVE MFS-NUM-FAELT-RAETT TO MOD-KVFRYSTI-UPP-ATTR                
045700         MOVE MID-KVFRYSTI-UPP    TO SPAR-KVFRYSTI WS-KVFRYSTI            
045800       ELSE                                                               
045900         MOVE MFS-NUM-FAELT-FEL   TO MOD-KVFRYSTI-UPP-ATTR                
046000         MOVE NEJ                 TO INDATA-SW                            
046100       END-IF                                                             
046200     END-IF                                                               
046300                                                                          
046400     IF MID-KVPB-TPO-C1-UPP NOT = ALL '+'                                 
046500       MOVE MID-KVPB-TPO-C1-UPP TO WS-KVPB-TPO-C1                         
046600       PERFORM EAA-KOLLA-OM-NUMERISK                                      
046700       PERFORM EAB-KOLLA-OM-PERIOD-OK                                     
046800       IF ALLT-OK                                                         
046900         MOVE MFS-NUM-FAELT-RAETT    TO MOD-KVPB-TPO-C1-UPP-ATTR          
047000         MOVE WS-KVPB-C1-HELTAL      TO SPAR-KVPB-C1-HELTAL               
047100         MOVE WS-KVPB-C1-DECIMAL     TO SPAR-KVPB-C1-DECIMAL              
047200       ELSE                                                               
047300         MOVE MFS-NUM-FAELT-FEL     TO MOD-KVPB-TPO-C1-UPP-ATTR           
047400         MOVE NEJ                   TO INDATA-SW                          
047500       END-IF                                                             
047600     END-IF                                                               
047700                                                                          
047800     IF MID-FLTPO1-UPP NOT = ALL '+'                                      
047900       IF MID-FLTPO1-UPP = 'J' OR 'N'                                     
048000         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLTPO1-UPP-ATTR                 
048100         MOVE MID-FLTPO1-UPP       TO SPAR-FLTPO1                         
048200       ELSE                                                               
048300         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLTPO1-UPP-ATTR                 
048400         MOVE NEJ                  TO INDATA-SW                           
048500       END-IF                                                             
048600     END-IF                                                               
048700                                                                          
048800     IF MID-KDUART-UPP NOT = ALL '+'                                      
048900       IF MID-KDUART-UPP = SPACE AND WS-KDUART = 'L'                      
049000         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDUART-UPP-ATTR                 
049100         MOVE MID-KDUART-UPP       TO SPAR-KDUART                         
049200       ELSE                                                               
049300         IF MID-KDUART-UPP = 'L' AND WS-KDUART = SPACE                    
049400           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDUART-UPP-ATTR               
049500           MOVE MID-KDUART-UPP       TO SPAR-KDUART                       
049600         ELSE                                                             
049700           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDUART-UPP-ATTR                 
049800           MOVE NEJ                TO INDATA-SW                           
049900         END-IF                                                           
050000       END-IF                                                             
050100     END-IF                                                               
050200                                                                          
050300     IF MID-FLRADREF-UPP NOT = ALL '+'                                    
050400       IF MID-FLRADREF-UPP = 'J' OR 'N'                                   
050500         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLRADREF-UPP-ATTR               
050600         MOVE MID-FLRADREF-UPP     TO SPAR-FLRADREF                       
050700       ELSE                                                               
050800         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLRADREF-UPP-ATTR               
050900         MOVE NEJ                  TO INDATA-SW                           
051000       END-IF                                                             
051100     END-IF                                                               
051200                                                                          
051300     IF MID-KDOPPLAN-UPP NOT = ALL '+'                                    
051400       IF MID-KDOPPLAN-UPP = 'J' OR 'N' OR 'X'                            
051500         IF  (MID-KDOPPLAN-UPP = 'J' OR 'X')                              
051600         AND (WS-IDLEVNR = '1002' OR WS-KDHF > 0)                         
051700           MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDOPPLAN-UPP-ATTR             
051800           MOVE NEJ                  TO INDATA-SW                         
051900         ELSE                                                             
052000           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDOPPLAN-UPP-ATTR             
052100           MOVE MID-KDOPPLAN-UPP     TO SPAR-KDOPPLAN                     
052200         END-IF                                                           
052300       ELSE                                                               
052400         MOVE MFS-ALFA-FAELT-FEL     TO MOD-KDOPPLAN-UPP-ATTR             
052500         MOVE NEJ                    TO INDATA-SW                         
052600       END-IF                                                             
052700     END-IF                                                               
052800                                                                          
052900     IF MID-FLMANOSK-UPP NOT = ALL '+'                                    
053000       IF MID-FLMANOSK-UPP NOT = 'J' AND 'N'                              
053100         MOVE MFS-ALFA-FAELT-FEL     TO MOD-FLMANOSK-UPP-ATTR             
053200         MOVE NEJ                    TO INDATA-SW                         
053300       ELSE                                                               
053400        MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLMANOSK-UPP-ATTR                
053500        MOVE MID-FLMANOSK-UPP     TO SPAR-FLMANOSK                        
053600       END-IF                                                             
053700     END-IF                                                               
053800                                                                          
053900     IF MID-PRORDSK-UPP NOT = ALL '+'                                     
054000        MOVE MID-PRORDSK-UPP TO DEC-IDFRIDATA                             
054100        MOVE 5               TO DEC-KVHELTAL                              
054200        MOVE 2               TO DEC-KVDECIMAL                             
054300        MOVE 'P'             TO DEC-KDSVAR                                
054400        CALL WDECEDIT USING DEC-WDECAREA                                  
054500        IF DEC-KDSVAR-OK                                                  
054600           IF WS-FLMANOSK = JA                                            
054700              MOVE DEC-IDEDITDATA TO SPAR-PRORDSK                         
054800              MOVE MFS-NUM-FAELT-RAETT TO MOD-PRORDSK-UPP-ATTR            
054900           ELSE                                                           
055000              MOVE MFS-NUM-FAELT-FEL   TO MOD-PRORDSK-UPP-ATTR            
055100              MOVE NEJ                 TO INDATA-SW                       
055200           END-IF                                                         
055300        ELSE                                                              
055400           MOVE MFS-NUM-FAELT-FEL   TO MOD-PRORDSK-UPP-ATTR               
055500           MOVE NEJ                 TO INDATA-SW                          
055600        END-IF                                                            
055700     END-IF                                                               
055800                                                                          
055801     IF MID-FLAUTREL-UPP NOT = ALL '+'                                    
055802       IF MID-FLAUTREL-UPP = JA OR YES OR NEJ                             
055804         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLAUTREL-UPP-ATTR               
055806         MOVE MID-FLAUTREL-UPP     TO SPAR-FLAUTREL                       
055807       ELSE                                                               
055808         MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLAUTREL-UPP-ATTR               
055809         MOVE NEJ                  TO INDATA-SW                           
055810       END-IF                                                             
055820     END-IF                                                               
055891                                                                          
055900     IF INDATA-FEL                                                        
056000       PERFORM EAC-VISA-INPUTFEL                                          
056100       MOVE NEJ TO UPPDAT-SW                                              
056200     ELSE                                                                 
056300       MOVE JA TO UPPDAT-SW                                               
056400     END-IF                                                               
056500     .                                                                    
056600     EJECT                                                                
056700 EAA-KOLLA-OM-NUMERISK SECTION.                                           
056800                                                                          
056900     IF WS-KVPB-C1-HELTAL    NUMERIC AND                                  
057000        WS-KVPB-C1-DECIMAL   NUMERIC AND                                  
057100        WS-KVPB-C1-PUNKT     = '.'                                        
057200       IF WS-KVPB-C1-HELTAL  > ZERO OR                                    
057300          WS-KVPB-C1-DECIMAL > ZERO                                       
057400                                                                          
057500         MOVE JA TO NUMERISK-SW                                           
057600       ELSE                                                               
057700         MOVE NEJ TO NUMERISK-SW                                          
057800       END-IF                                                             
057900     ELSE                                                                 
058000         MOVE NEJ TO NUMERISK-SW                                          
058100     END-IF                                                               
058200     .                                                                    
058300     EJECT                                                                
058400  EAB-KOLLA-OM-PERIOD-OK SECTION.                                         
058500                                                                          
058600     IF NUMERISK-OK                                                       
058700       PERFORM EABA-BERAEKNA-PERIOD                                       
058800                                                                          
058900       MOVE 'IDAG' TO DAT-KDDATFORM                                       
059000       CALL WDATKONV USING DAT-KDDATFORM,                                 
059100                           DAT-I-TIDATUM,                                 
059200                           DAT-O-TIDATUM,                                 
059300                           DAT-KDSVAR                                     
059400       MOVE DAT-TIAAVVD   TO TMP1-YYWWD                                   
059500       MOVE WS-PERIOD     TO TMP2-YYWWD                                   
059600       PERFORM WY2000P2                                                   
059700       IF TMP1-YYWWD <= TMP2-YYWWD                                        
059800         MOVE JA TO ALLT-SW                                               
059900       ELSE                                                               
060000         MOVE NEJ TO ALLT-SW                                              
060100       END-IF                                                             
060200     ELSE                                                                 
060300       MOVE NEJ TO ALLT-SW                                                
060400     END-IF                                                               
060500     .                                                                    
060600     EJECT                                                                
060700 EABA-BERAEKNA-PERIOD SECTION.                                            
060800                                                                          
060900*    BERÄKNA TIFINLV (ÅÅVVD) MINUS KVFRYSTI (VECKOR):                     
061000*    LÄGG RESULTATET I WS-PERIOD (ÅÅVVD)                                  
061100                                                                          
061200     MOVE WS-TIFINLV  TO WS-PERIOD                                        
061300     MOVE WS-AAVV     TO VADD-AAVV                                        
061400                                                                          
061500     COMPUTE WS-KVFRYSTI = WS-KVFRYSTI + 1                                
061600     COMPUTE VADD-ANTAL = -1 * WS-KVFRYSTI                                
061700                                                                          
061800     CALL W009VADD USING VADD-AAVV VADD-ANTAL                             
061900                                                                          
062000     MOVE VADD-AAVV TO WS-AAVV                                            
062100     .                                                                    
062200     EJECT                                                                
062300  EAC-VISA-INPUTFEL SECTION.                                              
062400                                                                          
062500     MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                            
062600     CALL WMEDKONV USING MED-WMEDAREA                                     
062700     MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                    
062800     PERFORM MFS-ROER-EJ-FAELT-IN                                         
062900     .                                                                    
063000     EJECT                                                                
063100 EB-AENDRA-I-DB SECTION.                                                  
063200                                                                          
063300     IF ((MID-FLMANOSK-UPP NOT = ALL '+' ) OR                             
063400        (MID-PRORDSK-UPP NOT = ALL '+' ))                                 
063500        PERFORM IMS-GHU-WDK611                                            
063600        IF MID-FLMANOSK-UPP NOT = ALL '+'                                 
063700           MOVE SPAR-FLMANOSK TO CLAG-FLMANOSK                            
063800           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLMANOSK-ATTR                
063900        END-IF                                                            
064000        IF MID-PRORDSK-UPP NOT = ALL '+'                                  
064100           MOVE SPAR-PRORDSK TO CLAG-PRORDSK                              
064200           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-PRORDSK-ATTR                 
064300        END-IF                                                            
064400        PERFORM IMS-REPL-WDK6                                             
064500     END-IF                                                               
064600     IF MID-KVPB-TPO-C1-UPP NOT = ALL '+'                                 
064700       PERFORM ECA-LAEGG-UPP-WDR5-2227                                    
064800                                                                          
064900       PERFORM IMS-GHU-WDK611                                             
065000                                                                          
065100       MOVE SPAR-KVPB-TPO-C1       TO CLAG-KVPB-TPO                       
065200       MOVE 'J'                    TO CLAG-FLMANPB                        
065300       PERFORM IMS-REPL-WDK6                                              
065400                                                                          
065500       MOVE MFS-ADD-LYS-UPP-FAELT  TO MOD-KVPB-TPO-C1-ATTR                
065600     END-IF                                                               
065700                                                                          
065800     IF (MID-FLTPO1-UPP   NOT = ALL '+') OR                               
065900        (MID-KVFRYSTI-UPP NOT = ALL '+') OR                               
066000        (MID-KDUART-UPP   NOT = ALL '+') OR                               
066100        (MID-FLRADREF-UPP NOT = ALL '+') OR                               
066200        (MID-KDOPPLAN-UPP NOT = ALL '+') OR                               
066210        (MID-FLAUTREL-UPP NOT = ALL '+')                                  
066300                                                                          
066400       PERFORM IMS-GHU-WDK611                                             
066500                                                                          
066600       IF MID-FLTPO1-UPP NOT = ALL '+'                                    
066700         MOVE SPAR-FLTPO1           TO CLAG-FLTPO1                        
066800         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLTPO1-ATTR                    
066900       END-IF                                                             
067000                                                                          
067100       IF MID-KVFRYSTI-UPP NOT = ALL '+'                                  
067200         MOVE SPAR-KVFRYSTI          TO CLAG-KVFRYSTI                     
067300         MOVE MFS-ADD-LYS-UPP-FAELT  TO MOD-KVFRYSTI-ATTR                 
067400       END-IF                                                             
067500                                                                          
067600       IF MID-KDUART-UPP NOT = ALL '+'                                    
067700         MOVE SPAR-KDUART            TO CLAG-KDUART                       
067800         MOVE MFS-ADD-LYS-UPP-FAELT  TO MOD-KDUART-ATTR                   
067900       END-IF                                                             
068000                                                                          
068100       IF MID-FLRADREF-UPP NOT = ALL '+'                                  
068200         MOVE SPAR-FLRADREF          TO CLAG-FLRADREF                     
068300         MOVE MFS-ADD-LYS-UPP-FAELT  TO MOD-FLRADREF-ATTR                 
068400       END-IF                                                             
068500                                                                          
068600       IF MID-KDOPPLAN-UPP NOT = ALL '+'                                  
068700         MOVE SPAR-KDOPPLAN          TO CLAG-KDOPPLAN                     
068800         MOVE 'J'                    TO CLAG-FLMANOPP                     
068900         MOVE MFS-ADD-LYS-UPP-FAELT  TO MOD-KDOPPLAN-ATTR                 
069000       END-IF                                                             
069100                                                                          
069110       IF MID-FLAUTREL-UPP NOT = ALL '+'                                  
069111         IF SPAR-FLAUTREL = YES                                           
069112            MOVE JA                  TO CLAG-FLAUTREL                     
069113         ELSE                                                             
069120            MOVE SPAR-FLAUTREL       TO CLAG-FLAUTREL                     
069130         END-IF                                                           
069140         MOVE MFS-ADD-LYS-UPP-FAELT  TO MOD-FLAUTREL-ATTR                 
069150       END-IF                                                             
069160                                                                          
069200       PERFORM IMS-REPL-WDK6                                              
069300     END-IF                                                               
069400     .                                                                    
069500     EJECT                                                                
069600 ECA-LAEGG-UPP-WDR5-2227 SECTION.                                         
069700                                                                          
069800       MOVE W-IDARTNR   TO XXBW-2228-IDARTNR                              
069900       MOVE LOW-VALUE   TO XXBW-2228-LOW-VALUE                            
070000       MOVE SPACE       TO XXBW-2228-FILLER                               
070100       PERFORM IMS-ISRT-XXBW-2227                                         
070200       .                                                                  
070300       EJECT                                                              
070400 F-BEHANDLA-ENTER SECTION.                                                
070500                                                                          
070600     IF MID-IDARTNR-IN = ALL '+'                                          
070700       IF MID-INPUT NOT = ALL '+' AND EGEN-TRANS                          
070800                                                                          
070900         MOVE INF-PRESS-PF11 TO MED-IDMFSFEL                              
071000         CALL WMEDKONV USING MED-WMEDAREA                                 
071100         MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                
071200                                                                          
071300         PERFORM MFS-ROER-EJ-FAELT-IN                                     
071400         PERFORM MFS-LAES-IN-IGEN                                         
071500                                                                          
071600       ELSE                                                               
071700         PERFORM MFS-RENSA-FAELT-IN                                       
071800       END-IF                                                             
071900     END-IF                                                               
072000                                                                          
072100     IF MID-IDARTNR-IN NOT = ALL '+'                                      
072200       PERFORM MFS-RENSA-FAELT-IN                                         
072300     END-IF                                                               
072400     .                                                                    
072500     EJECT                                                                
072600 G-LAES-VISA-BILD SECTION.                                                
072700                                                                          
072800     PERFORM IMS-GHU-WDK611                                               
072900     MOVE CLAG-FLMANOSK         TO MOD-FLMANOSK                           
073000     MOVE CLAG-PRORDSK          TO MOD-PRORDSK                            
073100     MOVE CLAG-KVPB-TPO         TO MOD-KVPB-TPO-C1                        
073200                                                                          
073300     MOVE MFS-RENSA-FAELT       TO MOD-KVPB-TPO-C2                        
073400                                                                          
073500                                                                          
073600     MOVE CLAG-FLTPO1           TO MOD-FLTPO1                             
073700     MOVE CLAG-KVFRYSTI         TO MOD-KVFRYSTI                           
073800     MOVE CLAG-KDUART           TO MOD-KDUART                             
073900     MOVE CLAG-FLRADREF         TO MOD-FLRADREF                           
074000     MOVE CLAG-KDOPPLAN         TO MOD-KDOPPLAN                           
074001     IF CLAG-FLAUTREL = JA AND ENGLISH-TEXT                               
074003        MOVE YES                TO MOD-FLAUTREL                           
074007     ELSE                                                                 
074008        MOVE CLAG-FLAUTREL      TO MOD-FLAUTREL                           
074009     END-IF                                                               
074100     MOVE MFS-RENSA-FAELT       TO MOD-RECLPROC-C1                        
074200                                   MOD-RECLPROC-C2                        
074300                                                                          
074310     IF CLAG-IDDC-REF NOT = SPACE                                         
074311       IF INDATA-OK                                                       
074320         MOVE INF-REFILL-PART   TO MED-IDMFSFEL                           
074330         CALL WMEDKONV       USING MED-WMEDAREA                           
074340         MOVE MED-MFSFEL        TO MOD-TEMFSFEL                           
074341       END-IF                                                             
074350       PERFORM S010-CLOSE-ARTC12-FAELT                                    
074360     END-IF                                                               
074370                                                                          
074400     IF UPPDATERING-OK                                                    
074500       MOVE INF-UPDATE-DONE     TO MED-IDMFSINF                           
074600       CALL WMEDKONV         USING MED-WMEDAREA                           
074700       MOVE MED-TEMFSINF        TO MOD-TEMFSINF                           
074800       PERFORM MFS-FORM-ATTR                                              
074900       PERFORM MFS-RENSA-FAELT-IN                                         
075000     END-IF                                                               
075100     .                                                                    
075200     EJECT                                                                
075210 S010-CLOSE-ARTC12-FAELT SECTION.                                         
075220                                                                          
075230     MOVE MFS-CLOSE-FIELD             TO MOD-KDOPPLAN-UPP-ATTR            
075240                                         MOD-FLMANOSK-UPP-ATTR            
075250                                         MOD-PRORDSK-UPP-ATTR             
075280     .                                                                    
075290     EJECT                                                                
075300 MFS-RENSA-FAELT-UT SECTION.                                              
075400                                                                          
075500     MOVE MFS-RENSA-FAELT TO MOD-BEART                                    
075600                             MOD-KVPB-TPO-C1                              
075700                             MOD-KVPB-TPO-C2                              
075800                             MOD-FLTPO1                                   
075900                             MOD-KVFRYSTI                                 
076000                             MOD-KDUART                                   
076100                             MOD-FLRADREF                                 
076200                             MOD-RECLPROC-C1                              
076300                             MOD-RECLPROC-C2                              
076400                             MOD-KDOPPLAN                                 
076500                             MOD-FLMANOSK                                 
076600                             MOD-FLAUTREL                                 
076610                             MOD-PRORDSK                                  
076700     .                                                                    
076800     SKIP2                                                                
076900 MFS-RENSA-FAELT-IN SECTION.                                              
077000                                                                          
077100     MOVE MFS-RENSA-FAELT TO MOD-KVPB-TPO-C1-UPP                          
077200                             MOD-FLTPO1-UPP                               
077300                             MOD-KVFRYSTI-UPP                             
077400                             MOD-KDUART-UPP                               
077500                             MOD-FLRADREF-UPP                             
077600                             MOD-KDOPPLAN-UPP                             
077700                             MOD-FLMANOSK-UPP                             
077710                             MOD-FLAUTREL-UPP                             
077800                             MOD-PRORDSK-UPP                              
077900     .                                                                    
078000     EJECT                                                                
078100 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
078200     MOVE MFS-ROER-EJ-FAELT TO MOD-KVPB-TPO-C1-UPP                        
078300                               MOD-FLTPO1-UPP                             
078400                               MOD-KVFRYSTI-UPP                           
078500                               MOD-KDUART-UPP                             
078600                               MOD-FLRADREF-UPP                           
078700                               MOD-KDOPPLAN-UPP                           
078800                               MOD-FLMANOSK-UPP                           
078810                               MOD-FLAUTREL-UPP                           
078900                               MOD-PRORDSK-UPP                            
079000     .                                                                    
079100 MFS-LAES-IN-IGEN SECTION.                                                
079200     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVPB-TPO-C1-UPP-ATTR               
079300                                   MOD-FLTPO1-UPP-ATTR                    
079400                                   MOD-KVFRYSTI-UPP-ATTR                  
079500                                   MOD-KDUART-UPP-ATTR                    
079600                                   MOD-FLRADREF-UPP-ATTR                  
079700                                   MOD-KDOPPLAN-UPP-ATTR                  
079800                                   MOD-FLMANOSK-UPP-ATTR                  
079810                                   MOD-FLAUTREL-UPP-ATTR                  
079900                                   MOD-PRORDSK-UPP-ATTR                   
080000     .                                                                    
080100     EJECT                                                                
080200 MFS-FORM-ATTR SECTION.                                                   
080300                                                                          
080400     MOVE MFS-FORMATETS-ATTR TO MOD-KVPB-TPO-C1-UPP-ATTR                  
080500                                MOD-FLTPO1-UPP-ATTR                       
080600                                MOD-KVFRYSTI-UPP-ATTR                     
080700                                MOD-KDUART-UPP-ATTR                       
080800                                MOD-FLRADREF-UPP-ATTR                     
080900                                MOD-KDOPPLAN-UPP-ATTR                     
081000                                MOD-FLMANOSK-UPP-ATTR                     
081010                                MOD-FLAUTREL-UPP-ATTR                     
081100                                MOD-PRORDSK-UPP-ATTR                      
081200     .                                                                    
081300     EJECT                                                                
081400* IMS SEKTIONER                                                           
081500     SKIP3                                                                
081600 IMS-GET-MSG SECTION.                                                     
081700                                                                          
081800     MOVE '  QC' TO GODK-STATUSKODER                                      
081900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
082000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
082100     PERFORM IMS-STATUSKONTROLL                                           
082200     .                                                                    
082300     SKIP3                                                                
082400 IMS-INSERT-MSG SECTION.                                                  
082500                                                                          
082600     IF ENGLISH-TEXT                                                      
082700       MOVE 'N' TO MFS-KDHUVOMR                                           
082800     END-IF                                                               
082900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
083000     MOVE SPACE TO GODK-STATUSKODER                                       
083100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
083200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
083300     PERFORM IMS-STATUSKONTROLL                                           
083400     .                                                                    
083500     EJECT                                                                
083600 IMS-GU-WDK601-ARTIKEL SECTION.                                           
083700                                                                          
083800     STRING 'WLARTC01(IDARTNR  =' W-WDK601-IDARTNR-X ')'                  
083900          DELIMITED BY SIZE INTO SSA1                                     
084000     MOVE '  GE' TO GODK-STATUSKODER                                      
084100     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-01 SSA1                   
084200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
084300     PERFORM IMS-STATUSKONTROLL                                           
084400     .                                                                    
084500     SKIP3                                                                
084600 IMS-GU-BENA11-BENAEMNING SECTION.                                        
084700                                                                          
084800     STRING 'WLBENA01(WDD3BSEQ =' W-WDD3BSEQ-X ')'                        
084900          DELIMITED BY SIZE INTO SSA1                                     
085000     STRING 'WLBENA11(IDSKYLT  =' W-WDD3-IDSKYLT-X ')'                    
085100          DELIMITED BY SIZE INTO SSA2                                     
085200     MOVE '  GE' TO GODK-STATUSKODER                                      
085300     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1 SSA2                 
085400     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
085500     PERFORM IMS-STATUSKONTROLL                                           
085600     .                                                                    
085700     EJECT                                                                
085800 IMS-GHU-WDK611 SECTION.                                                  
085900                                                                          
086000     STRING 'WLARTC01(IDARTNR  =' W-WDK601-IDARTNR-X ')'                  
086100          DELIMITED BY SIZE INTO SSA1                                     
086200     STRING 'WLARTC11(KDSEGKEY =' W-WDK611-KDSEGKEY-X ')'                 
086300          DELIMITED BY SIZE INTO SSA2                                     
086400     MOVE '  GE' TO GODK-STATUSKODER                                      
086500     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA-11 SSA1 SSA2             
086600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
086700     PERFORM IMS-STATUSKONTROLL                                           
086800     .                                                                    
086900     EJECT                                                                
087000 IMS-REPL-WDK6 SECTION.                                                   
087100                                                                          
087200     MOVE '  ' TO GODK-STATUSKODER                                        
087300     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA-11                      
087400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
087500     PERFORM IMS-STATUSKONTROLL                                           
087600     .                                                                    
087700     EJECT                                                                
087800 IMS-ISRT-XXBW-2227 SECTION.                                              
087900                                                                          
088000     STRING 'WLXXBW01(WDGXKEY  =' W-WDGXKEY-2227-X ')'                    
088100          DELIMITED BY SIZE INTO SSA1                                     
088200     MOVE 'WLXXBW11 ' TO SSA2                                             
088300     MOVE '  II' TO GODK-STATUSKODER                                      
088400     CALL CBLTDLI USING ISRT XXBW-PCB DLI-IO-AREA SSA1 SSA2               
088500     MOVE XXBW-STATUS-CODE TO STATUS-WS                                   
088600     PERFORM IMS-STATUSKONTROLL                                           
088700     .                                                                    
088800     SKIP3                                                                
088900 IMS-STATUSKONTROLL SECTION.                                              
089000                                                                          
089100     SET STATUS-IX TO 1                                                   
089200     SEARCH GODK-STATUS                                                   
089300       AT END CALL FELLOG                                                 
089400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
089500     END-SEARCH                                                           
089600     .                                                                    
089700     EJECT                                                                
089800*    -COPY WY2000P2                                                       
