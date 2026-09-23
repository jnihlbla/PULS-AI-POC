000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5016600.                                                
000300 AUTHOR.         NIHLBLAD JOHAN.                                          
000400 DATE-WRITTEN.   05/12/15.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PGM FÖR JUSTERING AV SALDON                                      
000900*                                                                         
001000*        PROGRAMMET UPPDATERAR WDK6                                       
001100*        PROGRAMMET UPPDATERAR WDK7                                       
001200*        PROGRAMMET LÄSER      WDD3                                       
001300*        PROGRAMMET UPPDATERAR WDK9                                       
001400*        PROGRAMMET UPPDATERAR WDL9                                       
001500*        PROGRAMMET LÄSER      WDB6                                       
001600*        PROGRAMMET UPPDATERAR WDP9                                       
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSAKTION: W5T166                                              
002000*        MID:         W5I16601                                            
002100*                                                                         
002200*    UTDATA.                                                              
002300*        MOD:         W5O16601                                            
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700                                                                          
002800 DATA DIVISION.                                                           
002900     EJECT                                                                
003000 WORKING-STORAGE SECTION.                                                 
003100 77  IDPGM                       PIC X(08)   VALUE 'W5016600'.            
003200                                                                          
003300*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003400 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003500                                                                          
003600 77  JA                          PIC X       VALUE 'J'.                   
003700 77  NEJ                         PIC X       VALUE 'N'.                   
003800                                                                          
003900*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004000 77  IX                          PIC S9(4)  VALUE +0    COMP SYNC.        
004100 77  W-DAGENS-DATUM              PIC 9(8).                                
004200 77  TRANS-TID                   PIC 9(9).                                
004300*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004400                                                                          
004500 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004600     88  INDATA-OK                           VALUE 'J'.                   
004700     88  INDATA-FEL                          VALUE 'N'.                   
004800                                                                          
004900 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005000     88  NYCKLAR-OK                          VALUE 'J'.                   
005100     88  NYCKLAR-FEL                         VALUE 'N'.                   
005200                                                                          
005300 77  JUST-SW                   PIC X       VALUE 'N'.                     
005400     88  JUST-OK                           VALUE 'J'.                     
005500     88  JUST-FEL                          VALUE 'N'.                     
005600                                                                          
005700 01  ARBETS-AREA.                                                         
005800     03 WS-KVLS                  PIC -(6)9  VALUE ZERO.                   
005900     03 WS-KVAKS                 PIC -(6)9  VALUE ZERO.                   
006000     03 WS-KVEFRS                PIC -(6)9  VALUE ZERO.                   
006100     03 WS-KVAKS-PAV             PIC -(6)9  VALUE ZERO.                   
006200     03 WS-OQB                   PIC -(6)9  VALUE ZERO.                   
006300     03 WS-KVBEART               PIC -(6)9  VALUE ZERO.                   
006400     03 WS-KVRESS                PIC -(6)9  VALUE ZERO.                   
006500     03 WS-BO                    PIC -(6)9  VALUE ZERO.                   
006600     03 WS-KVOKS-VOR             PIC -(6)9  VALUE ZERO.                   
006700     03 WS-KVOKS-DAG             PIC -(6)9  VALUE ZERO.                   
006800     03 WS-KVOKS-BULK            PIC -(6)9  VALUE ZERO.                   
006900     03 WS-KVROS-DAG             PIC -(6)9  VALUE ZERO.                   
007000     03 WS-KVROS-BULK            PIC -(6)9  VALUE ZERO.                   
007100     03 WS-KVROS                 PIC -(6)9  VALUE ZERO.                   
007200     03 WS-AVAILABLE             PIC -(6)9  VALUE ZERO.                   
007300     03 WS-COMM                  PIC X(17)  VALUE SPACE.                  
007400     03 WS-KVAKS-IN              PIC 9(7)   VALUE ZERO.                   
007500     03 WS-KVEFRS-IN             PIC 9(7)   VALUE ZERO.                   
007600     03 WS-KVAKS-PAV-IN          PIC 9(7)   VALUE ZERO.                   
007700     03 WS-OQB-IN                PIC 9(7)   VALUE ZERO.                   
007800     03 WS-KVBEART-IN            PIC 9(7)   VALUE ZERO.                   
007900     03 WS-KVRESS-IN             PIC 9(7)   VALUE ZERO.                   
008000     03 WS-BO-IN                 PIC 9(7)   VALUE ZERO.                   
008100                                                                          
008200 01  WS-DATUM-LOPNR            PIC 9(9).                                  
008300 01  XX-DATUM-LOPNR REDEFINES WS-DATUM-LOPNR.                             
008400     03 WS-DATUM               PIC 9(8).                                  
008500     03 WS-IDLOPNR             PIC 9(1).                                  
008600                                                                          
008700 01  WS-LOGGRAD.                                                          
008800     03 WS-TYP                 PIC X(3)  VALUE SPACE.                     
008900     03 FILLER                 PIC X(6)  VALUE ' WITH '.                  
009000     03 WS-TECKEN              PIC X(1)  VALUE SPACE.                     
009100     03 WS-ANTAL               PIC Z(7)  VALUE SPACE.                     
009200                                                                          
009300 01  W-DATE-LOPNR              PIC 9(9)  VALUE ZERO.                      
009400                                                                          
009500 01  MEDDELANDEN.                                                         
009600     03 MED-1-AREA.                                                       
009700        05 FILLER                PIC X(49)                                
009800           VALUE 'ONLY ONE ROW CAN BE UPDATED EACH TIME       '.          
009900     03 FILLER REDEFINES MED-1-AREA.                                      
010000        05 MED-1                 PIC X(49).                               
010100                                                                          
010200     03 MED-2-AREA.                                                       
010300        05 FILLER                PIC X(49)                                
010400           VALUE 'UPDATE NOT ALLOWED FOR SDC                  '.          
010500     03 FILLER REDEFINES MED-2-AREA.                                      
010600        05 MED-2                 PIC X(49).                               
010700                                                                          
011400     03 MED-4-AREA.                                                       
011500        05 FILLER                PIC X(49)                                
011600           VALUE 'V/D/B FIELD IS NOT VALID FOR CDC            '.          
011700     03 FILLER REDEFINES MED-4-AREA.                                      
011800        05 MED-4                 PIC X(49).                               
011900                                                                          
012000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
012100     88  EGEN-MID                            VALUE '5166'.                
012200     88  GODK-MID                            VALUE '5161' '5162'          
012300                                                   '5163' '5164'          
012400                                                   '5165' '5166'          
012500                                                   '5167' '5168'          
012600                                                   '5169'.                
012700     88  HELP-MID                            VALUE '0551'.                
012800     EJECT                                                                
012900*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
013000 01  GENERELLA-SUBPROGRAM.                                                
013100     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
013200     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
013300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013500     EJECT                                                                
013600*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
013700*01 -COPY WMEDAREA                                                        
013800     SKIP3                                                                
013900 01  MESSAGE-CODES.                                                       
014000     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
014100     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
014200     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
014300     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
014400     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
014500     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
014600     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
014700     03  PART-MISSING            PIC X(3)    VALUE '017'.                 
014800     03  PART-SUPERSEDED         PIC X(3)    VALUE '220'.                 
014900     EJECT                                                                
015000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
015100*                                                                         
015200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
015300     SKIP3                                                                
015400*01 -COPY WMSGINIT                                                        
015500     EJECT                                                                
015600*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
015700*                                                                         
015800 01  SPAR-AREA.                                                           
015900     03  SPAR-IDTRANS           PIC X(4)    VALUE '5166'.                 
016000     EJECT                                                                
016100*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
016200*                                                                         
016300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
016400     SKIP3                                                                
016500*01  MID -COPY W5I16601                                                   
016600     EJECT                                                                
016700 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
016800     SKIP3                                                                
016900*01  -COPY WMSGAREA                                                       
017000     EJECT                                                                
017100     03  MOD REDEFINES MSG-AREA.                                          
017200*      05  -COPY W5O16601                                                 
017300     EJECT                                                                
017400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
017500     SKIP3                                                                
017600*01  -COPY WMFSAREA                                                       
017700     EJECT                                                                
017800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017900*                                                                         
018000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
018100     SKIP3                                                                
018200 01  NYCKLAR-TILL-DLI.                                                    
018300*    --- VÄRDE PÅ BLÄDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN             
018400     03  W-IDARTNR-X.                                                     
018500         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
018600     03  W-KDSEGKEY-X.                                                    
018700         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
018800     03  W-IDDC-B6-X.                                                     
018900         05  W-IDDC-B6           PIC X(2)   VALUE SPACE.                  
019000     03  W-IDDC-X.                                                        
019100         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
019200     03  W-IDSKYLT-X.                                                     
019300         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
019400     03  W-WDP901KY-MIN-X.                                                
019500         05  W-IDARTNR-WDP9-MIN      PIC S9(9)  VALUE ZERO COMP-3.        
019600         05  W-IDDC-WDP9-MIN         PIC X(2)   VALUE SPACE.              
019700         05  W-TISEGKEY-9KOMPL-MIN   PIC S9(9)  VALUE ZERO COMP-3.        
019800     03  W-WDP901KY-MAX-X.                                                
019900         05  W-IDARTNR-WDP9-MAX      PIC S9(9)  VALUE ZERO COMP-3.        
020000         05  W-IDDC-WDP9-MAX         PIC X(2)   VALUE SPACE.              
020100         05  W-TISEGKEY-9KOMPL-MAX   PIC S9(9)                            
020200                                         VALUE 999999999 COMP-3.          
020300     SKIP2                                                                
020400*    --- STATUS-KOD FRÅN IMS                                              
020500 01  STATUS-WS                   PIC XX.                                  
020600     88  SEGMENT-FINNS                       VALUE '  '.                  
020700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
020800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
020900     SKIP2                                                                
021000 01  GODK-STATUSKODER.                                                    
021100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
021200     SKIP3                                                                
021300 01  SSA1                        PIC X(64).                               
021400 01  SSA2                        PIC X(64).                               
021500     EJECT                                                                
021600*    --- IMS FUNKTIONSKODER                                               
021700*01  -COPY W0003                                                          
021800     EJECT                                                                
021900*    ---  DLI INPUT-OUTPUT AREA                                           
022000                                                                          
022100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
022200 01  DLI-IO-WDK601.                                                       
022300*    03  -COPY WDK601                                                     
022400     EJECT                                                                
022500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
022600 01  DLI-IO-WDK611.                                                       
022700*    03  -COPY WDK611                                                     
022800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
022900 01  DLI-IO-WDK701.                                                       
023000*    03  -COPY WDK701                                                     
023100     EJECT                                                                
023200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
023300 01  DLI-IO-WDK711.                                                       
023400*    03  -COPY WDK711                                                     
023500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD301'.                      
023600 01  DLI-IO-WDD301.                                                       
023700*    03  -COPY WDD301                                                     
023800     EJECT                                                                
023900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
024000 01  DLI-IO-WDD311.                                                       
024100*    03  -COPY WDD311                                                     
024200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK901'.                      
024300 01  DLI-IO-WDK901.                                                       
024400*    03  -COPY WDK901                                                     
024500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL901'.                      
024600 01  DLI-IO-WDL901.                                                       
024700*    03  -COPY WDL901                                                     
024800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
024900 01  DLI-IO-WDB601.                                                       
025000*    03  -COPY WDB601                                                     
025100     EJECT                                                                
025200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP901'.                      
025300 01  DLI-IO-WDP901.                                                       
025400*    03  -COPY WDP901                                                     
025500     EJECT                                                                
025600 LINKAGE SECTION.                                                         
025700*01  -COPY W0009   -PRE MSG-                                              
025800*01  -COPY W0008   -PRE WDP7-                                             
025900     05  FILLER                  PIC X.                                   
026000                                                                          
026100*01  -COPY W0008  -PRE WDK6-                                              
026200     05  FILLER                  PIC X.                                   
026300                                                                          
026400*01  -COPY W0008  -PRE WDK7-                                              
026500     05  FILLER                  PIC X.                                   
026600                                                                          
026700*01  -COPY W0008  -PRE WDD3-                                              
026800     05  FILLER                  PIC X.                                   
026900                                                                          
027000*01  -COPY W0008  -PRE WDK9-                                              
027100     05  FILLER                  PIC X.                                   
027200                                                                          
027300*01  -COPY W0008  -PRE WDL9-                                              
027400     05  FILLER                  PIC X.                                   
027500                                                                          
027600*01  -COPY W0008  -PRE WDB6-                                              
027700     05  FILLER                  PIC X.                                   
027800                                                                          
027900*01  -COPY W0008  -PRE WDP9-                                              
028000     05  FILLER                  PIC X.                                   
028100     EJECT                                                                
028200 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDK6-PCB WDK7-PCB             
028300     WDD3-PCB WDK9-PCB WDL9-PCB WDB6-PCB WDP9-PCB.                        
028400 MAIN SECTION.                                                            
028500     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDK6-PCB WDK7-PCB             
028600     WDD3-PCB WDK9-PCB WDL9-PCB WDB6-PCB WDP9-PCB.                        
028700                                                                          
028800     PERFORM IMS-GET-MSG                                                  
028900     IF SEGMENT-FINNS                                                     
029000       PERFORM A-INIT                                                     
029100       PERFORM B-KOLLA-NYCKLAR                                            
029200       IF NYCKLAR-OK                                                      
029300         IF MFS-UPDATE                                                    
029400           PERFORM G-KOLLA-INPUT                                          
029500           IF INDATA-OK                                                   
029600             PERFORM H-UPPDATERA                                          
029700           END-IF                                                         
029800         ELSE                                                             
029900           IF MFS-FIRST                                                   
030000             PERFORM C-FOERSTA-SIDA                                       
030100           ELSE                                                           
030200             PERFORM E-SAMMA-SIDA                                         
030300           END-IF                                                         
030400         END-IF                                                           
030500         PERFORM F-LAES-VISA-INFO                                         
030600       END-IF                                                             
030700       COMPUTE MSG-KVLL = LENGTH OF MOD-W5O16601 + 4                      
030800       PERFORM IMS-INSERT-MSG                                             
030900     END-IF                                                               
031000                                                                          
031100     MOVE ZERO TO RETURN-CODE                                             
031200     GOBACK                                                               
031300     .                                                                    
031400     EJECT                                                                
031500 A-INIT SECTION.                                                          
031600                                                                          
031700     IF MSG-DUBBLA-TRANSKODER                                             
031800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I16601                 
031900       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
032000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
032100     ELSE                                                                 
032200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I16601                  
032300       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
032400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
032500     END-IF                                                               
032600                                                                          
032700     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
032800     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
032900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
033000                                                                          
033100     MOVE LOW-VALUE TO MSG-AREA                                           
033200     MOVE 'W5O166N1' TO MFS-IDMOD                                         
033300     MOVE '5166' TO MOD-IDTRANS                                           
033400     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
033500     MOVE FUNCTION CURRENT-DATE(1:8) TO W-DAGENS-DATUM                    
033600                                        WS-DATUM                          
033700     MOVE +0  TO WS-IDLOPNR                                               
033800     IF EGEN-MID OR HELP-MID                                              
033900       CONTINUE                                                           
034000     ELSE                                                                 
034100       MOVE SPACE TO MFS-KDTRTYP                                          
034200       MOVE '7' TO MFS-IDPFK                                              
034300     END-IF                                                               
034400     .                                                                    
034500     EJECT                                                                
034600 B-KOLLA-NYCKLAR SECTION.                                                 
034700                                                                          
034800     MOVE ALL '+'           TO MSGI-WMSGINIT                              
034900     MOVE '001'             TO MSGI-KDCALL                                
035000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
035100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
035200     MOVE '5166'            TO MSGI-IDTRANS                               
035300     IF EGEN-MID                                                          
035400       MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                               
035500       MOVE MID-IDDC-IN     TO MSGI-IDDC-KEY                              
035600     ELSE                                                                 
035700       IF  MID-IDARTNR-IN NUMERIC                                         
035800       AND MID-IDARTNR-IN > ZERO                                          
035900         MOVE MID-IDARTNR-IN                                              
036000                            TO MSGI-IDARTNR                               
036100       END-IF                                                             
036200     END-IF                                                               
036300     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
036400     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
036500                                                                          
036600*    - SPRÅK SOM SKA ANVÄNDAS AV WMEDKONV                                 
036700     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
036800                                                                          
036900     MOVE JA TO NYCKLAR-SW                                                
037000                                                                          
037100                                                                          
037200*    -- KONTROLL AV IDARTNR                                               
037300     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
037400                                                                          
037500     IF MID-IDARTNR-IN NOT = ALL '+'                                      
037600       MOVE '7'         TO MFS-IDPFK                                      
037700       MOVE SPACE       TO MFS-KDTRTYP                                    
037800     END-IF                                                               
037900     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
038000     IF MSGI-IDARTNR NUMERIC                                              
038100       MOVE MSGI-IDARTNR TO W-IDARTNR                                     
038200                            W-IDARTNR-WDP9-MIN                            
038300                            W-IDARTNR-WDP9-MAX                            
038400     ELSE                                                                 
038500       MOVE NEJ TO NYCKLAR-SW                                             
038600     END-IF                                                               
038700                                                                          
038800*    -- KONTROLL AV IDDC                                                  
038900     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
039000                                                                          
039100     IF MID-IDDC-IN NOT = ALL '+'                                         
039200       MOVE '7'           TO MFS-IDPFK                                    
039300       MOVE SPACE         TO MFS-KDTRTYP                                  
039400     END-IF                                                               
039500                                                                          
039600     MOVE MSGI-IDDC-KEY   TO W-IDDC-B6                                    
039700                                                                          
039800     PERFORM IMS-GU-WDB601                                                
039900                                                                          
040000     IF SEGMENT-FINNS                                                     
040100     AND (DCS-CDC                                                         
040200     OR   DCS-CDC-TR                                                      
040300     OR   DCS-SDC                                                         
040400     OR   DCS-NDC)                                                        
040600       MOVE W-IDDC-B6        TO W-IDDC                                    
040700                                MOD-IDDC-UT                               
040800                                W-IDDC-WDP9-MIN                           
040900                                W-IDDC-WDP9-MAX                           
041000     ELSE                                                                 
041100       MOVE NEJ              TO NYCKLAR-SW                                
041200       MOVE MSGI-IDDC-KEY    TO MOD-IDDC-UT                               
041300     END-IF                                                               
041400                                                                          
041500       MOVE MSGI-IDARTNR        TO MOD-IDARTNR-UT                         
041600       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
041700                                                                          
041800     IF NYCKLAR-FEL                                                       
041900       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
042000       CALL WMEDKONV USING MED-WMEDAREA                                   
042100       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
042200       PERFORM MFS-RENSA-FAELT-IN                                         
042300       PERFORM MFS-RENSA-FAELT-UT                                         
042400     END-IF                                                               
042500     .                                                                    
042600     EJECT                                                                
042700 C-FOERSTA-SIDA SECTION.                                                  
042800                                                                          
042900     PERFORM MFS-RENSA-FAELT-IN                                           
043000     .                                                                    
043100     EJECT                                                                
043200 E-SAMMA-SIDA SECTION.                                                    
043300                                                                          
043400     IF SPAR-IDTRANS = '5166' OR '0551'                                   
043500       IF MID-INPUT = ALL '+'                                             
043600       AND MID-JUST = ALL ' '                                             
043700         PERFORM MFS-RENSA-FAELT-IN                                       
043800       ELSE                                                               
043900         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
044000         CALL WMEDKONV USING MED-WMEDAREA                                 
044100         MOVE MED-MFSINF TO MOD-TEMFSFEL                                  
044200         PERFORM EA-MID-INDATA-TILL-MOD                                   
044300       END-IF                                                             
044400     ELSE                                                                 
044500       PERFORM MFS-RENSA-FAELT-IN                                         
044600     END-IF                                                               
044700     .                                                                    
044800     EJECT                                                                
044900 EA-MID-INDATA-TILL-MOD SECTION.                                          
045000                                                                          
045100     IF MID-JUST1 = '+' OR '-'                                            
045200         MOVE MID-JUST1 TO MOD-JUST1                                      
045300         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-JUST1-ATTR                    
045400     ELSE                                                                 
045500         MOVE MFS-RENSA-FAELT TO MOD-JUST1                                
045600     END-IF                                                               
045700                                                                          
045800     IF MID-JUST2 = '+' OR '-'                                            
045900         MOVE MID-JUST2 TO MOD-JUST2                                      
046000         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-JUST2-ATTR                    
046100     ELSE                                                                 
046200         MOVE MFS-RENSA-FAELT TO MOD-JUST2                                
046300     END-IF                                                               
046400                                                                          
046500     IF MID-JUST3 = '+' OR '-'                                            
046600         MOVE MID-JUST3 TO MOD-JUST3                                      
046700         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-JUST3-ATTR                    
046800     ELSE                                                                 
046900         MOVE MFS-RENSA-FAELT TO MOD-JUST3                                
047000     END-IF                                                               
047100                                                                          
047200     IF MID-JUST4 = '+' OR '-'                                            
047300         MOVE MID-JUST4 TO MOD-JUST4                                      
047400         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-JUST4-ATTR                    
047500     ELSE                                                                 
047600         MOVE MFS-RENSA-FAELT TO MOD-JUST4                                
047700     END-IF                                                               
047800                                                                          
047900     IF MID-JUST5 = '+' OR '-'                                            
048000         MOVE MID-JUST5 TO MOD-JUST5                                      
048100         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-JUST5-ATTR                    
048200     ELSE                                                                 
048300         MOVE MFS-RENSA-FAELT TO MOD-JUST5                                
048400     END-IF                                                               
048500                                                                          
048600     IF MID-JUST6 = '+' OR '-'                                            
048700         MOVE MID-JUST6 TO MOD-JUST6                                      
048800         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-JUST6-ATTR                    
048900     ELSE                                                                 
049000         MOVE MFS-RENSA-FAELT TO MOD-JUST6                                
049100     END-IF                                                               
049200                                                                          
049300     IF MID-JUST7 = '+' OR '-'                                            
049400         MOVE MID-JUST7 TO MOD-JUST7                                      
049500         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-JUST7-ATTR                    
049600     ELSE                                                                 
049700         MOVE MFS-RENSA-FAELT TO MOD-JUST7                                
049800     END-IF                                                               
049900                                                                          
050000     IF MID-KVAKS-IN NOT = ALL '+'                                        
050100         MOVE MID-KVAKS-IN TO MOD-KVAKS-IN                                
050200         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KVAKS-IN-ATTR                 
050300     ELSE                                                                 
050400         MOVE MFS-RENSA-FAELT TO MOD-KVAKS-IN                             
050500     END-IF                                                               
050600                                                                          
050700     IF MID-KVEFRS-IN NOT = ALL '+'                                       
050800         MOVE MID-KVEFRS-IN TO MOD-KVEFRS-IN                              
050900         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KVEFRS-IN-ATTR                
051000     ELSE                                                                 
051100         MOVE MFS-RENSA-FAELT TO MOD-KVEFRS-IN                            
051200     END-IF                                                               
051300                                                                          
051400     IF MID-KVAKS-PAV-IN NOT = ALL '+'                                    
051500         MOVE MID-KVAKS-PAV-IN TO MOD-KVAKS-PAV-IN                        
051600         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KVAKS-PAV-IN-ATTR             
051700     ELSE                                                                 
051800         MOVE MFS-RENSA-FAELT TO MOD-KVAKS-PAV-IN                         
051900     END-IF                                                               
052000                                                                          
052100     IF MID-OQB-IN NOT = ALL '+'                                          
052200         MOVE MID-OQB-IN TO MOD-OQB-IN                                    
052300         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-OQB-IN-ATTR                   
052400     ELSE                                                                 
052500         MOVE MFS-RENSA-FAELT TO MOD-OQB-IN                               
052600     END-IF                                                               
052700                                                                          
052800     IF MID-VDB4 NOT = ALL '+'                                            
052900         MOVE MID-VDB4 TO MOD-VDB4                                        
053000         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-VDB4-ATTR                     
053100     ELSE                                                                 
053200         MOVE MFS-RENSA-FAELT TO MOD-VDB4                                 
053300     END-IF                                                               
053400                                                                          
053500     IF MID-KVBEART-IN NOT = ALL '+'                                      
053600         MOVE MID-KVBEART-IN TO MOD-KVBEART-IN                            
053700         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KVBEART-IN-ATTR               
053800     ELSE                                                                 
053900         MOVE MFS-RENSA-FAELT TO MOD-KVBEART-IN                           
054000     END-IF                                                               
054100                                                                          
054200     IF MID-KVRESS-IN NOT = ALL '+'                                       
054300         MOVE MID-KVRESS-IN TO MOD-KVRESS-IN                              
054400         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-KVRESS-IN-ATTR                
054500     ELSE                                                                 
054600         MOVE MFS-RENSA-FAELT TO MOD-KVRESS-IN                            
054700     END-IF                                                               
054800                                                                          
054900     IF MID-BO-IN NOT = ALL '+'                                           
055000         MOVE MID-BO-IN TO MOD-BO-IN                                      
055100         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-BO-IN-ATTR                    
055200     ELSE                                                                 
055300         MOVE MFS-RENSA-FAELT TO MOD-BO-IN                                
055400     END-IF                                                               
055500                                                                          
055600     IF MID-VDB7 NOT = ALL '+'                                            
055700         MOVE MID-VDB7 TO MOD-VDB7                                        
055800         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-VDB7-ATTR                     
055900     ELSE                                                                 
056000         MOVE MFS-RENSA-FAELT TO MOD-VDB7                                 
056100     END-IF                                                               
056200                                                                          
056300     IF MID-COMM NOT = ALL '+'                                            
056400         MOVE MID-COMM TO MOD-COMM                                        
056500         MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-COMM-ATTR                     
056600     ELSE                                                                 
056700         MOVE MFS-RENSA-FAELT TO MOD-COMM                                 
056800     END-IF                                                               
056900                                                                          
057000     .                                                                    
057100     EJECT                                                                
057200 F-LAES-VISA-INFO SECTION.                                                
057300                                                                          
057400     PERFORM FA-LAES-GRUNDDATA                                            
057500                                                                          
057600     IF SEGMENT-SAKNAS                                                    
057700        MOVE PART-MISSING TO MED-IDMFSFEL                                 
057800        MOVE 'GB '         TO MED-IDSKYLT                                 
057900        CALL WMEDKONV USING MED-WMEDAREA                                  
058000        MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                 
058100        PERFORM MFS-RENSA-FAELT-UT                                        
058200     ELSE                                                                 
058300        IF ART-KDERS-UTG > +0                                             
058400           MOVE PART-SUPERSEDED TO MED-IDMFSFEL                           
058500           MOVE 'GB '         TO MED-IDSKYLT                              
058600           CALL WMEDKONV USING MED-WMEDAREA                               
058700           MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                              
058800           PERFORM MFS-RENSA-FAELT-UT                                     
058900        ELSE                                                              
059000           PERFORM FB-VISA                                                
059100        END-IF                                                            
059200     END-IF                                                               
059300                                                                          
059400     .                                                                    
059500     EJECT                                                                
059600 FA-LAES-GRUNDDATA SECTION.                                               
059700                                                                          
059800     PERFORM IMS-GU-WDK601                                                
059900     IF SEGMENT-FINNS                                                     
060000       PERFORM IMS-GNP-WDK611                                             
060100                                                                          
060200       IF SEGMENT-FINNS                                                   
060300         MOVE CLAG-PRARTSTD  TO MOD-PRARTSTD                              
060400       END-IF                                                             
060500     END-IF                                                               
060600     .                                                                    
060700     EJECT                                                                
060800 FB-VISA SECTION.                                                         
060900                                                                          
061000     IF DCS-CDC                                                           
061100     OR DCS-CDC-TR                                                        
061200        PERFORM FBA-VISA-CDC                                              
061300     ELSE                                                                 
061400        PERFORM FBB-VISA-ANDRA                                            
061500     END-IF                                                               
061600     PERFORM FBC-VISA-LOGG                                                
061700     .                                                                    
061800     EJECT                                                                
061900                                                                          
062000 FBA-VISA-CDC SECTION.                                                    
062100                                                                          
062200     MOVE CLAG-KVLS                 TO MOD-KVLS                           
062300     MOVE CLAG-KVAKS-CDC            TO MOD-KVAKS                          
062400     MOVE CLAG-KVEFRS               TO MOD-KVEFRS                         
062500     MOVE CLAG-KVAKS-PAV            TO MOD-KVAKS-PAV                      
062600     MOVE CLAG-KVRESS               TO MOD-KVRESS                         
062700     MOVE CLAG-KVROS                TO MOD-BO                             
062710     MOVE CLAG-KVBEART              TO MOD-KVBEART                        
062800     PERFORM IMS-GU-WDK901                                                
062900     IF SEGMENT-FINNS                                                     
063000       COMPUTE WS-AVAILABLE = CLAG-KVLS                                   
063100                            - CLAG-KVRESS                                 
063200                            - ART-KVOKS-BULK                              
063300                            - ART-KVOKS-DAG                               
063400                            - ART-KVOKS-VOR                               
063500                                                                          
063600       COMPUTE WS-OQB       = ART-KVOKS-BULK                              
063700                            + ART-KVOKS-DAG                               
063800                            + ART-KVOKS-VOR                               
063900     ELSE                                                                 
064000       COMPUTE WS-AVAILABLE = CLAG-KVLS                                   
064100                            - CLAG-KVRESS                                 
064200                                                                          
064300       MOVE ZERO TO WS-OQB                                                
064400     END-IF                                                               
064500     MOVE WS-AVAILABLE     TO MOD-AVAILABLE                               
064600     MOVE WS-OQB           TO MOD-OQB                                     
064700     MOVE 'GB ' TO W-IDSKYLT                                              
064800     PERFORM IMS-GU-WDD311-BSEQ                                           
064900     IF SEGMENT-FINNS                                                     
065000         MOVE TEXT-BEART TO MOD-BEART                                     
065100       ELSE                                                               
065200         MOVE SPACE        TO MOD-BEART                                   
065300     END-IF                                                               
065400                                                                          
065500     .                                                                    
065600     EJECT                                                                
065700                                                                          
065800 FBB-VISA-ANDRA SECTION.                                                  
065900                                                                          
066000     PERFORM IMS-GU-WDK711                                                
066100     IF SEGMENT-FINNS                                                     
066200       MOVE SLAG-KVLS               TO MOD-KVLS                           
066300       MOVE SLAG-KVAKS-SDC          TO MOD-KVAKS                          
066400       MOVE SLAG-KVEFRS             TO MOD-KVEFRS                         
066500       MOVE SLAG-KVAKS-PAV          TO MOD-KVAKS-PAV                      
066600       MOVE SLAG-KVBEART            TO MOD-KVBEART                        
066700       MOVE SLAG-KVRESS             TO MOD-KVRESS                         
066800         COMPUTE WS-AVAILABLE = SLAG-KVLS                                 
066900                              - SLAG-KVRESS                               
067000                              - SLAG-KVOKS-BULK                           
067100                              - SLAG-KVOKS-DAG                            
067200                                                                          
067300         COMPUTE WS-OQB     = SLAG-KVOKS-BULK                             
067400                              + SLAG-KVOKS-DAG                            
067500         COMPUTE WS-BO      = SLAG-KVROS-BULK                             
067600                            + SLAG-KVROS-DAG                              
067700                                                                          
067800       MOVE WS-AVAILABLE   TO MOD-AVAILABLE                               
067900       MOVE WS-OQB         TO MOD-OQB                                     
068000       MOVE WS-BO          TO MOD-BO                                      
068100       PERFORM IMS-GU-WDD301-BSEQ                                         
068200       IF SEGMENT-FINNS                                                   
068300         MOVE 'GB ' TO W-IDSKYLT                                          
068400         PERFORM IMS-GNP-WDD311-BSEQ                                      
068500         IF SEGMENT-FINNS                                                 
068600           MOVE TEXT-BEART TO MOD-BEART                                   
068700         ELSE                                                             
068800           MOVE SPACE      TO MOD-BEART                                   
068900         END-IF                                                           
069000       END-IF                                                             
069100     ELSE                                                                 
069200       MOVE PART-MISSING TO MED-IDMFSFEL                                  
069300       MOVE 'GB '          TO MED-IDSKYLT                                 
069400       CALL WMEDKONV USING MED-WMEDAREA                                   
069500       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
069600       PERFORM MFS-RENSA-FAELT-UT                                         
069700     END-IF                                                               
069800                                                                          
069900     .                                                                    
070000     EJECT                                                                
070100 FBC-VISA-LOGG SECTION.                                                   
070200                                                                          
070300     MOVE +1    TO IX                                                     
070400     PERFORM IMS-GU-WDP901                                                
070500     PERFORM UNTIL SEGMENT-SAKNAS OR IX > 14                              
070600       COMPUTE W-DATE-LOPNR = 999999999 - LOGA-TISEGKEY-9KOMPL            
070700       MOVE W-DATE-LOPNR(3:6)   TO MOD-DATE(IX)                           
070800       MOVE LOGA-IDSTYP         TO MOD-TYPE(IX)                           
070900       COMPUTE MOD-QTY(IX)      = LOGA-KVANTAL-E                          
071000                                - LOGA-KVANTAL-F                          
071100       MOVE LOGA-IDUSER         TO MOD-IDUSER(IX)                         
071200       ADD +1   TO IX                                                     
071300       PERFORM IMS-GN-WDP901                                              
071400     END-PERFORM                                                          
071500     .                                                                    
071600     EJECT                                                                
071700                                                                          
071800 G-KOLLA-INPUT SECTION.                                                   
071900                                                                          
072000     MOVE JA  TO INDATA-SW                                                
072100     IF (MID-INPUT = ALL '+') AND (MID-JUST = ALL ' ')                    
072200       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
072300       CALL WMEDKONV USING MED-WMEDAREA                                   
072400       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
072500       PERFORM MFS-ROER-EJ-FAELT-IN                                       
072600       PERFORM MFS-ROER-EJ-FAELT-UT                                       
072700       MOVE NEJ TO INDATA-SW                                              
072800     ELSE                                                                 
072900                                                                          
073000       IF MID-JUST NOT = ALL ' '                                          
073100         IF NOT DCS-DDC                                                   
073200           PERFORM GA-KOLLA-JUST                                          
073300         ELSE                                                             
073400           MOVE NEJ TO INDATA-SW                                          
073500         END-IF                                                           
073600       ELSE                                                               
073700         MOVE NEJ TO INDATA-SW                                            
073800       END-IF                                                             
073900                                                                          
074000     END-IF                                                               
074100     IF INDATA-FEL                                                        
074200       MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                          
074300       CALL WMEDKONV USING MED-WMEDAREA                                   
074400       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
074500       PERFORM MFS-ROER-EJ-FAELT-UT                                       
074600       PERFORM MFS-ROER-EJ-FAELT-IN                                       
074700       PERFORM MFS-LAES-IN-IGEN                                           
074800     END-IF                                                               
074900     .                                                                    
075000     EJECT                                                                
075100 GA-KOLLA-JUST SECTION.                                                   
075200                                                                          
075300**************JUSTERA AK SALDO******************************              
075400     IF MID-JUST1 = '+' OR '-'                                            
075500       IF MID-JUST2 = ' '                                                 
075600       AND MID-JUST3 = ' '                                                
075700       AND MID-JUST4 = ' '                                                
075800       AND MID-JUST5 = ' '                                                
075900       AND MID-JUST6 = ' '                                                
076000       AND MID-JUST7 = ' '                                                
076100         IF MID-KVAKS-IN NOT = ALL '+'                                    
076200           IF MID-KVAKS-IN NUMERIC                                        
076300             MOVE MFS-ALFA-FAELT-RAETT TO MOD-KVAKS-IN-ATTR               
076400             MOVE MID-KVAKS-IN         TO WS-KVAKS-IN                     
076500           ELSE                                                           
076600             MOVE MFS-ALFA-FAELT-FEL  TO MOD-KVAKS-IN-ATTR                
076700             MOVE NEJ TO INDATA-SW                                        
076800           END-IF                                                         
076900         ELSE                                                             
077000           MOVE MFS-ALFA-FAELT-FEL    TO MOD-KVAKS-IN-ATTR                
077100           MOVE NEJ TO INDATA-SW                                          
077200         END-IF                                                           
077300       ELSE                                                               
077400         MOVE MED-1        TO MOD-TEMFSINF                                
077500         MOVE NEJ TO INDATA-SW                                            
077600       END-IF                                                             
077700     END-IF                                                               
077800**************JUSTERA EFR SALDO******************************             
077900     IF MID-JUST2 = '+' OR '-'                                            
078000       IF MID-JUST1 = ' '                                                 
078100       AND MID-JUST3 = ' '                                                
078200       AND MID-JUST4 = ' '                                                
078300       AND MID-JUST5 = ' '                                                
078400       AND MID-JUST6 = ' '                                                
078500       AND MID-JUST7 = ' '                                                
078600         IF MID-KVEFRS-IN NOT = ALL '+'                                   
078700           IF MID-KVEFRS-IN NUMERIC                                       
078800             MOVE MFS-ALFA-FAELT-RAETT TO MOD-KVEFRS-IN-ATTR              
078900             MOVE MID-KVEFRS-IN       TO WS-KVEFRS-IN                     
079000           ELSE                                                           
079100             MOVE MFS-ALFA-FAELT-FEL  TO MOD-KVEFRS-IN-ATTR               
079200             MOVE NEJ TO INDATA-SW                                        
079300           END-IF                                                         
079400         ELSE                                                             
079500           MOVE MFS-ALFA-FAELT-FEL    TO MOD-KVEFRS-IN-ATTR               
079600           MOVE NEJ TO INDATA-SW                                          
079700         END-IF                                                           
079800       ELSE                                                               
079900         MOVE MED-1        TO MOD-TEMFSINF                                
080000         MOVE NEJ TO INDATA-SW                                            
080100       END-IF                                                             
080200     END-IF                                                               
080300**************JUSTERA GIT SALDO******************************             
080400     IF MID-JUST3 = '+' OR '-'                                            
080500       IF MID-JUST1 = ' '                                                 
080600       AND MID-JUST2 = ' '                                                
080700       AND MID-JUST4 = ' '                                                
080800       AND MID-JUST5 = ' '                                                
080900       AND MID-JUST6 = ' '                                                
081000       AND MID-JUST7 = ' '                                                
081100         IF MID-KVAKS-PAV-IN NOT = ALL '+'                                
081200           IF MID-KVAKS-PAV-IN NUMERIC                                    
081300             MOVE MFS-ALFA-FAELT-RAETT TO MOD-KVAKS-PAV-IN-ATTR           
081400             MOVE MID-KVAKS-PAV-IN    TO WS-KVAKS-PAV-IN                  
081500           ELSE                                                           
081600             MOVE MFS-ALFA-FAELT-FEL  TO MOD-KVAKS-PAV-IN-ATTR            
081700             MOVE NEJ TO INDATA-SW                                        
081800           END-IF                                                         
081900         ELSE                                                             
082000           MOVE MFS-ALFA-FAELT-FEL    TO MOD-KVAKS-PAV-IN-ATTR            
082100           MOVE NEJ TO INDATA-SW                                          
082200         END-IF                                                           
082300       ELSE                                                               
082400         MOVE MED-1        TO MOD-TEMFSINF                                
082500         MOVE NEJ TO INDATA-SW                                            
082600       END-IF                                                             
082700     END-IF                                                               
082800**************JUSTERA OQB SALDO******************************             
082900     IF MID-JUST4 = '+' OR '-'                                            
083000       IF MID-JUST1 = ' '                                                 
083100       AND MID-JUST2 = ' '                                                
083200       AND MID-JUST3 = ' '                                                
083300       AND MID-JUST5 = ' '                                                
083400       AND MID-JUST6 = ' '                                                
083500       AND MID-JUST7 = ' '                                                
083600         IF MID-OQB-IN NOT = ALL '+'                                      
083700           IF MID-OQB-IN NUMERIC                                          
083800             MOVE MFS-ALFA-FAELT-RAETT TO MOD-OQB-IN-ATTR                 
083900             MOVE MID-OQB-IN          TO WS-OQB-IN                        
084000             IF MID-VDB4 NOT = ALL '+'                                    
084100               IF MID-VDB4 = 'V' OR 'D' OR 'B'                            
084200                 IF (MID-VDB4 = 'V' AND DCS-CDC)                          
084300                 OR (MID-VDB4 = 'D' OR 'B')                               
084400                   MOVE MFS-ALFA-FAELT-RAETT TO MOD-VDB4-ATTR             
084500                 ELSE                                                     
084600                   MOVE MFS-ALFA-FAELT-FEL TO MOD-VDB4-ATTR               
084700                   MOVE NEJ TO INDATA-SW                                  
084800                 END-IF                                                   
084900               ELSE                                                       
085000                 MOVE MFS-ALFA-FAELT-FEL TO MOD-VDB4-ATTR                 
085100                 MOVE NEJ TO INDATA-SW                                    
085200               END-IF                                                     
085300             ELSE                                                         
085400               MOVE MFS-ALFA-FAELT-FEL   TO MOD-VDB4-ATTR                 
085500               MOVE NEJ TO INDATA-SW                                      
085600             END-IF                                                       
085700           ELSE                                                           
085800             MOVE MFS-ALFA-FAELT-FEL  TO MOD-OQB-IN-ATTR                  
085900             MOVE NEJ TO INDATA-SW                                        
086000           END-IF                                                         
086100         ELSE                                                             
086200           MOVE MFS-ALFA-FAELT-FEL    TO MOD-OQB-IN-ATTR                  
086300           MOVE NEJ TO INDATA-SW                                          
086400         END-IF                                                           
086500       ELSE                                                               
086600         MOVE MED-1        TO MOD-TEMFSINF                                
086700         MOVE NEJ TO INDATA-SW                                            
086800       END-IF                                                             
086900     END-IF                                                               
087000**************JUSTERA REFILL ORDERED SALDO*******************             
087100     IF MID-JUST5 = '+' OR '-'                                            
087300       IF MID-JUST1 = ' '                                                 
087400       AND MID-JUST2 = ' '                                                
087500       AND MID-JUST3 = ' '                                                
087600       AND MID-JUST4 = ' '                                                
087700       AND MID-JUST6 = ' '                                                
087800       AND MID-JUST7 = ' '                                                
087900         IF MID-KVBEART-IN NOT = ALL '+'                                  
088000           IF MID-KVBEART-IN NUMERIC                                      
088100             MOVE MFS-ALFA-FAELT-RAETT TO MOD-KVBEART-IN-ATTR             
088200             MOVE MID-KVBEART-IN        TO WS-KVBEART-IN                  
088300           ELSE                                                           
088400             MOVE MFS-ALFA-FAELT-FEL TO MOD-KVBEART-IN-ATTR               
088500             MOVE NEJ TO INDATA-SW                                        
088600             MOVE 'A' TO MOD-TEMFSINF                                     
088700           END-IF                                                         
088800         ELSE                                                             
088900           MOVE MFS-ALFA-FAELT-FEL    TO MOD-KVBEART-IN-ATTR              
089000           MOVE NEJ TO INDATA-SW                                          
089100         END-IF                                                           
089200       ELSE                                                               
089300         MOVE MED-1        TO MOD-TEMFSINF                                
089400         MOVE NEJ TO INDATA-SW                                            
089500       END-IF                                                             
090100     END-IF                                                               
090200**************JUSTERA RESERVED SALDO*******************                   
090300     IF MID-JUST6 = '+' OR '-'                                            
090400       IF NOT DCS-SDC                                                     
090500         IF MID-JUST1 = ' '                                               
090600         AND MID-JUST2 = ' '                                              
090700         AND MID-JUST3 = ' '                                              
090800         AND MID-JUST4 = ' '                                              
090900         AND MID-JUST5 = ' '                                              
091000         AND MID-JUST7 = ' '                                              
091100           IF MID-KVRESS-IN NOT = ALL '+'                                 
091200             IF MID-KVRESS-IN NUMERIC                                     
091300               MOVE MFS-ALFA-FAELT-RAETT TO MOD-KVRESS-IN-ATTR            
091400               MOVE MID-KVRESS-IN       TO WS-KVRESS-IN                   
091500             ELSE                                                         
091600               MOVE MFS-ALFA-FAELT-FEL TO MOD-KVRESS-IN-ATTR              
091700               MOVE NEJ TO INDATA-SW                                      
091800             END-IF                                                       
091900           ELSE                                                           
092000             MOVE MFS-ALFA-FAELT-FEL  TO MOD-KVRESS-IN-ATTR               
092100             MOVE NEJ TO INDATA-SW                                        
092200           END-IF                                                         
092300         ELSE                                                             
092400           MOVE MED-1      TO MOD-TEMFSINF                                
092500           MOVE NEJ TO INDATA-SW                                          
092600         END-IF                                                           
092700       ELSE                                                               
092800         MOVE MED-2                    TO MOD-TEMFSINF                    
092900         MOVE MFS-ALFA-FAELT-FEL       TO MOD-JUST6-ATTR                  
093000         MOVE NEJ TO INDATA-SW                                            
093100       END-IF                                                             
093200     END-IF                                                               
093300**************JUSTERA BO SALDO*******************                         
093400     IF MID-JUST7 = '+' OR '-'                                            
093500       IF NOT DCS-SDC                                                     
093600         IF MID-JUST1 = ' '                                               
093700         AND MID-JUST2 = ' '                                              
093800         AND MID-JUST3 = ' '                                              
093900         AND MID-JUST4 = ' '                                              
094000         AND MID-JUST5 = ' '                                              
094100         AND MID-JUST6 = ' '                                              
094200           IF MID-BO-IN NOT = ALL '+'                                     
094300             IF MID-BO-IN NUMERIC                                         
094400               MOVE MFS-ALFA-FAELT-RAETT TO MOD-BO-IN-ATTR                
094500               MOVE MID-BO-IN           TO WS-BO-IN                       
094600               IF DCS-CDC                                                 
094700                 IF MID-VDB7 NOT = ALL '+'                                
094800                   MOVE MED-4               TO MOD-TEMFSINF               
094900                   MOVE MFS-ALFA-FAELT-FEL TO MOD-VDB7-ATTR               
095000                   MOVE NEJ TO INDATA-SW                                  
095100                 ELSE                                                     
095200                   MOVE MFS-ALFA-FAELT-RAETT TO MOD-VDB7-ATTR             
095300                 END-IF                                                   
095400               ELSE                                                       
095500                 IF DCS-NDC                                               
095600                   IF MID-VDB7 NOT = ALL '+'                              
095700                     IF MID-VDB7 = 'D' OR 'B'                             
095800                       MOVE MFS-ALFA-FAELT-RAETT TO MOD-VDB7-ATTR         
095900                     ELSE                                                 
096000                       MOVE MFS-ALFA-FAELT-FEL TO MOD-VDB7-ATTR           
096100                       MOVE NEJ TO INDATA-SW                              
096200                     END-IF                                               
096300                   ELSE                                                   
096400                     MOVE MFS-ALFA-FAELT-FEL TO MOD-VDB7-ATTR             
096500                     MOVE NEJ TO INDATA-SW                                
096600                   END-IF                                                 
096700                 ELSE                                                     
096800                   MOVE MFS-ALFA-FAELT-FEL TO MOD-VDB7-ATTR               
096900                   MOVE NEJ TO INDATA-SW                                  
097000                 END-IF                                                   
097100               END-IF                                                     
097200             ELSE                                                         
097300               MOVE MFS-ALFA-FAELT-FEL TO MOD-BO-IN-ATTR                  
097400               MOVE NEJ TO INDATA-SW                                      
097500             END-IF                                                       
097600           ELSE                                                           
097700             MOVE MFS-ALFA-FAELT-FEL  TO MOD-BO-IN-ATTR                   
097800             MOVE NEJ TO INDATA-SW                                        
097900           END-IF                                                         
098000         ELSE                                                             
098100           MOVE MED-1      TO MOD-TEMFSINF                                
098200           MOVE NEJ TO INDATA-SW                                          
098300         END-IF                                                           
098400       ELSE                                                               
098500         MOVE MED-2                    TO MOD-TEMFSINF                    
098600         MOVE MFS-ALFA-FAELT-FEL      TO MOD-BO-IN-ATTR                   
098700         MOVE NEJ TO INDATA-SW                                            
098800       END-IF                                                             
098900     END-IF                                                               
099000                                                                          
099100     IF MID-COMM       NOT = ALL '+'                                      
099200        MOVE MID-COMM                  TO MOD-COMM                        
099300     ELSE                                                                 
099400        MOVE MFS-RENSA-FAELT           TO MOD-COMM                        
099500     END-IF                                                               
099600                                                                          
099700     IF MID-JUST1 = '+' OR '-' OR ' '                                     
099800       CONTINUE                                                           
099900     ELSE                                                                 
100000       MOVE MFS-ADD-HILIGHT-FIELD   TO MOD-JUST1-ATTR                     
100100       MOVE NEJ TO INDATA-SW                                              
100200     END-IF                                                               
100300     IF MID-JUST2 = '+' OR '-' OR ' '                                     
100400       CONTINUE                                                           
100500     ELSE                                                                 
100600       MOVE MFS-ADD-HILIGHT-FIELD   TO MOD-JUST2-ATTR                     
100700       MOVE NEJ TO INDATA-SW                                              
100800     END-IF                                                               
100900     IF MID-JUST3 = '+' OR '-' OR ' '                                     
101000       CONTINUE                                                           
101100     ELSE                                                                 
101200       MOVE MFS-ADD-HILIGHT-FIELD   TO MOD-JUST3-ATTR                     
101300       MOVE NEJ TO INDATA-SW                                              
101400     END-IF                                                               
101500     IF MID-JUST4 = '+' OR '-' OR ' '                                     
101600       CONTINUE                                                           
101700     ELSE                                                                 
101800       MOVE MFS-ADD-HILIGHT-FIELD   TO MOD-JUST4-ATTR                     
101900       MOVE NEJ TO INDATA-SW                                              
102000     END-IF                                                               
102100     IF MID-JUST5 = '+' OR '-' OR ' '                                     
102200       CONTINUE                                                           
102300     ELSE                                                                 
102400       MOVE MFS-ADD-HILIGHT-FIELD   TO MOD-JUST5-ATTR                     
102500       MOVE NEJ TO INDATA-SW                                              
102600     END-IF                                                               
102700     IF MID-JUST6 = '+' OR '-' OR ' '                                     
102800       CONTINUE                                                           
102900     ELSE                                                                 
103000       MOVE MFS-ADD-HILIGHT-FIELD   TO MOD-JUST6-ATTR                     
103100       MOVE NEJ TO INDATA-SW                                              
103200     END-IF                                                               
103300     IF MID-JUST7 = '+' OR '-' OR ' '                                     
103400       CONTINUE                                                           
103500     ELSE                                                                 
103600       MOVE MFS-ADD-HILIGHT-FIELD   TO MOD-JUST7-ATTR                     
103700       MOVE NEJ TO INDATA-SW                                              
103800     END-IF                                                               
103900     .                                                                    
104000     EJECT                                                                
104100 H-UPPDATERA SECTION.                                                     
104200                                                                          
104300     IF DCS-CDC                                                           
104400       PERFORM HA-UPPDATERA-CDC                                           
104500     ELSE                                                                 
104600       PERFORM HB-UPPDATERA-OVRIGA                                        
104700     END-IF                                                               
104800     .                                                                    
104900     EJECT                                                                
105000 HA-UPPDATERA-CDC SECTION.                                                
105100                                                                          
105200     PERFORM IMS-GHU-WDK611                                               
105300     IF SEGMENT-FINNS                                                     
105400****************AK SALDO******************************                    
105500       IF MID-JUST1  NOT = ALL ' '                                        
105600         IF MID-JUST1 = '+'                                               
105700           COMPUTE WS-KVAKS = CLAG-KVAKS-CDC + WS-KVAKS-IN                
105800           COMPUTE WS-KVLS  = CLAG-KVLS  - WS-KVAKS-IN                    
105900           MOVE '+'               TO LOGG-IDTECKEN-KVAKS                  
106000           MOVE '-'               TO LOGG-IDTECKEN-KVLS                   
106100         ELSE                                                             
106200           COMPUTE WS-KVAKS = CLAG-KVAKS-CDC - WS-KVAKS-IN                
106300           COMPUTE WS-KVLS  = CLAG-KVLS  + WS-KVAKS-IN                    
106400           MOVE '-'               TO LOGG-IDTECKEN-KVAKS                  
106500           MOVE '+'               TO LOGG-IDTECKEN-KVLS                   
106600         END-IF                                                           
106700         MOVE SPACE               TO LOGG-IDTECKEN-KVAKS-PAV              
106800         MOVE SPACE               TO LOGG-IDTECKEN-KVEFRS                 
106900         MOVE CLAG-KVAKS-CDC      TO LOGA-KVANTAL-F                       
107000         MOVE WS-KVAKS-IN        TO LOGG-KVART-SALDO                      
107100                                     LOGA-KVANTAL                         
107200         MOVE WS-KVLS             TO LOGG-KVLS                            
107300                                     CLAG-KVLS                            
107400         MOVE WS-KVAKS            TO LOGG-KVAKS                           
107500                                     CLAG-KVAKS-CDC                       
107600                                     LOGA-KVANTAL-E                       
107700         MOVE CLAG-KVAKS-PAV      TO LOGG-KVAKS-PAV                       
107800         MOVE CLAG-KVEFRS         TO LOGG-KVEFRS                          
107900         MOVE 'AK'                TO LOGA-IDSTYP                          
108000       END-IF                                                             
108100****************EFR SALDO*****************************                    
108200       IF MID-JUST2  NOT = ALL ' '                                        
108300         IF MID-JUST2 = '+'                                               
108400           COMPUTE WS-KVEFRS = CLAG-KVEFRS + WS-KVEFRS-IN                 
108500           COMPUTE WS-KVLS   = CLAG-KVLS  - WS-KVEFRS-IN                  
108600           MOVE '+'               TO LOGG-IDTECKEN-KVEFRS                 
108700           MOVE '-'               TO LOGG-IDTECKEN-KVLS                   
108800         ELSE                                                             
108900           MOVE '-'               TO LOGG-IDTECKEN-KVEFRS                 
109000           MOVE '+'               TO LOGG-IDTECKEN-KVLS                   
109100           COMPUTE WS-KVEFRS = CLAG-KVEFRS - WS-KVEFRS-IN                 
109200           COMPUTE WS-KVLS   = CLAG-KVLS  + WS-KVEFRS-IN                  
109300         END-IF                                                           
109400         MOVE SPACE               TO LOGG-IDTECKEN-KVAKS-PAV              
109500         MOVE SPACE               TO LOGG-IDTECKEN-KVAKS                  
109600         MOVE WS-KVEFRS-IN       TO LOGG-KVART-SALDO                      
109700                                     LOGA-KVANTAL                         
109800         MOVE CLAG-KVEFRS         TO LOGA-KVANTAL-F                       
109900         MOVE WS-KVLS             TO LOGG-KVLS                            
110000                                     CLAG-KVLS                            
110100         MOVE WS-KVEFRS           TO LOGG-KVEFRS                          
110200                                     CLAG-KVEFRS                          
110300                                     LOGA-KVANTAL-E                       
110400         MOVE CLAG-KVAKS-PAV      TO LOGG-KVAKS-PAV                       
110500         MOVE CLAG-KVAKS-CDC      TO LOGG-KVAKS                           
110600         MOVE 'EFR'               TO LOGA-IDSTYP                          
110700       END-IF                                                             
110800****************GIT SALDO*****************************                    
110900       IF MID-JUST3  NOT = ALL ' '                                        
111000         IF MID-JUST3 = '+'                                               
111100           COMPUTE WS-KVAKS-PAV                                           
111200                   = CLAG-KVAKS-PAV + WS-KVAKS-PAV-IN                     
111300           COMPUTE WS-KVAKS  = CLAG-KVAKS-CDC - WS-KVAKS-PAV-IN           
111400           MOVE '+'               TO LOGG-IDTECKEN-KVAKS-PAV              
111500           MOVE '-'               TO LOGG-IDTECKEN-KVAKS                  
111600         ELSE                                                             
111700           COMPUTE WS-KVAKS-PAV                                           
111800                   = CLAG-KVAKS-PAV - WS-KVAKS-PAV-IN                     
111900           COMPUTE WS-KVAKS  = CLAG-KVAKS-CDC + WS-KVAKS-PAV-IN           
112000           MOVE '-'               TO LOGG-IDTECKEN-KVAKS-PAV              
112100           MOVE '+'               TO LOGG-IDTECKEN-KVAKS                  
112200         END-IF                                                           
112300         MOVE SPACE               TO LOGG-IDTECKEN-KVEFRS                 
112400         MOVE SPACE               TO LOGG-IDTECKEN-KVLS                   
112500         MOVE WS-KVAKS-PAV-IN    TO LOGG-KVART-SALDO                      
112600                                     LOGA-KVANTAL                         
112700         MOVE CLAG-KVAKS-PAV      TO LOGA-KVANTAL-F                       
112800         MOVE WS-KVAKS-PAV        TO LOGG-KVAKS-PAV                       
112900                                     CLAG-KVAKS-PAV                       
113000                                     LOGA-KVANTAL-E                       
113100         MOVE WS-KVAKS            TO LOGG-KVAKS                           
113200                                     CLAG-KVAKS-CDC                       
113300         MOVE CLAG-KVEFRS         TO LOGG-KVEFRS                          
113400         MOVE CLAG-KVLS           TO LOGG-KVLS                            
113500         MOVE 'GIT'               TO LOGA-IDSTYP                          
113600       END-IF                                                             
113700****************OQB SALDO*****************************                    
113800       IF MID-JUST4  NOT = ALL ' '                                        
113900         PERFORM IMS-GHU-WDK901                                           
114000         IF SEGMENT-FINNS                                                 
114100           IF MID-VDB4 = 'V'                                              
114200             MOVE ART-KVOKS-VOR  TO LOGA-KVANTAL-F                        
114300             IF MID-JUST4 = '+'                                           
114400               COMPUTE WS-KVOKS-VOR                                       
114500                       = ART-KVOKS-VOR + WS-OQB-IN                        
114600               MOVE WS-KVOKS-VOR     TO ART-KVOKS-VOR                     
114700               MOVE '+'              TO WS-TECKEN                         
114800             ELSE                                                         
114900               COMPUTE WS-KVOKS-VOR                                       
115000                       = ART-KVOKS-VOR - WS-OQB-IN                        
115100               MOVE WS-KVOKS-VOR     TO ART-KVOKS-VOR                     
115200               MOVE '-'              TO WS-TECKEN                         
115300             END-IF                                                       
115400             MOVE WS-KVOKS-VOR       TO LOGA-KVANTAL-E                    
115500             MOVE 'OQV'              TO WS-TYP                            
115600                                        LOGA-IDSTYP                       
115700             MOVE WS-OQB-IN         TO WS-ANTAL                           
115800                                        LOGA-KVANTAL                      
115900           END-IF                                                         
116000           IF MID-VDB4 = 'D'                                              
116100             MOVE ART-KVOKS-DAG  TO LOGA-KVANTAL-F                        
116200             IF MID-JUST4 = '+'                                           
116300               COMPUTE WS-KVOKS-DAG                                       
116400                       = ART-KVOKS-DAG + WS-OQB-IN                        
116500               MOVE WS-KVOKS-DAG     TO ART-KVOKS-DAG                     
116600               MOVE '+'              TO WS-TECKEN                         
116700             ELSE                                                         
116800               COMPUTE WS-KVOKS-DAG                                       
116900                       = ART-KVOKS-DAG - WS-OQB-IN                        
117000               MOVE WS-KVOKS-DAG     TO ART-KVOKS-DAG                     
117100               MOVE '-'              TO WS-TECKEN                         
117200             END-IF                                                       
117300             MOVE WS-KVOKS-DAG       TO LOGA-KVANTAL-E                    
117400             MOVE 'OQD'              TO WS-TYP                            
117500                                        LOGA-IDSTYP                       
117600             MOVE WS-OQB-IN         TO WS-ANTAL                           
117700                                        LOGA-KVANTAL                      
117800           END-IF                                                         
117900           IF MID-VDB4 = 'B'                                              
118000             MOVE ART-KVOKS-BULK TO LOGA-KVANTAL-F                        
118100             IF MID-JUST4 = '+'                                           
118200               COMPUTE WS-KVOKS-BULK                                      
118300                       = ART-KVOKS-BULK + WS-OQB-IN                       
118400               MOVE WS-KVOKS-BULK    TO ART-KVOKS-BULK                    
118500               MOVE '+'              TO WS-TECKEN                         
118600             ELSE                                                         
118700               COMPUTE WS-KVOKS-BULK                                      
118800                       = ART-KVOKS-BULK - WS-OQB-IN                       
118900               MOVE WS-KVOKS-BULK    TO ART-KVOKS-BULK                    
119000               MOVE '-'              TO WS-TECKEN                         
119100             END-IF                                                       
119200             MOVE WS-KVOKS-BULK      TO LOGA-KVANTAL-E                    
119300             MOVE 'OQB'              TO WS-TYP                            
119400                                        LOGA-IDSTYP                       
119500             MOVE WS-OQB-IN         TO WS-ANTAL                           
119600                                        LOGA-KVANTAL                      
119700           END-IF                                                         
119701**** THIS CODE IS TO CORRECT THE PRODUCTION ERROR                         
119710*          IF MID-JUST4  NOT = ALL ' '                                    
119720*             PERFORM IMS-REPL-WDK901                                     
119730*          END-IF                                                         
119800         END-IF                                                           
119900         MOVE ZERO                TO LOGG-KVART-SALDO                     
120000         MOVE SPACE               TO LOGG-IDTECKEN-KVAKS-PAV              
120100         MOVE SPACE               TO LOGG-IDTECKEN-KVEFRS                 
120200         MOVE SPACE               TO LOGG-IDTECKEN-KVAKS                  
120300         MOVE SPACE               TO LOGG-IDTECKEN-KVLS                   
120400         MOVE CLAG-KVLS           TO LOGG-KVLS                            
120500         MOVE CLAG-KVAKS-CDC      TO LOGG-KVAKS                           
120600         MOVE CLAG-KVAKS-PAV      TO LOGG-KVAKS-PAV                       
120700         MOVE CLAG-KVEFRS         TO LOGG-KVEFRS                          
120800       END-IF                                                             
120810****************REFILL ORDER SALDO********************                    
120820       IF MID-JUST5  NOT = ALL ' '                                        
120830         MOVE CLAG-KVBEART        TO LOGA-KVANTAL-F                       
120840         IF MID-JUST5 = '+'                                               
120850           COMPUTE WS-KVBEART                                             
120860                   = CLAG-KVBEART   + WS-KVBEART-IN                       
120870           MOVE '+'               TO WS-TECKEN                            
120880         ELSE                                                             
120890           COMPUTE WS-KVBEART                                             
120891                   = CLAG-KVBEART   - WS-KVBEART-IN                       
120892           MOVE '-'               TO WS-TECKEN                            
120893         END-IF                                                           
120894         MOVE 'REF'               TO WS-TYP                               
120895                                     LOGA-IDSTYP                          
120896         MOVE WS-KVBEART-IN      TO WS-ANTAL                              
120897                                     LOGA-KVANTAL                         
120898         MOVE WS-KVBEART          TO CLAG-KVBEART                         
120899                                     LOGA-KVANTAL-E                       
120900         MOVE ZERO                TO LOGG-KVART-SALDO                     
120901         MOVE SPACE               TO LOGG-IDTECKEN-KVAKS-PAV              
120902         MOVE SPACE               TO LOGG-IDTECKEN-KVEFRS                 
120903         MOVE SPACE               TO LOGG-IDTECKEN-KVAKS                  
120904         MOVE SPACE               TO LOGG-IDTECKEN-KVLS                   
120909         MOVE CLAG-KVLS           TO LOGG-KVLS                            
120910         MOVE CLAG-KVAKS-CDC      TO LOGG-KVAKS                           
120911         MOVE CLAG-KVAKS-PAV      TO LOGG-KVAKS-PAV                       
120912         MOVE CLAG-KVEFRS         TO LOGG-KVEFRS                          
120913       END-IF                                                             
120920****************RESERVED SALDO************************                    
121000       IF MID-JUST6  NOT = ALL ' '                                        
121100         MOVE CLAG-KVRESS         TO LOGA-KVANTAL-F                       
121200         IF MID-JUST6 = '+'                                               
121300           COMPUTE WS-KVRESS                                              
121400                   = CLAG-KVRESS    + WS-KVRESS-IN                        
121500           MOVE '+'               TO WS-TECKEN                            
121600         ELSE                                                             
121700           COMPUTE WS-KVRESS                                              
121800                   = CLAG-KVRESS    - WS-KVRESS-IN                        
121900           MOVE '-'               TO WS-TECKEN                            
122000         END-IF                                                           
122100         MOVE 'RES'               TO WS-TYP                               
122200                                     LOGA-IDSTYP                          
122300         MOVE WS-KVRESS-IN       TO WS-ANTAL                              
122400         MOVE WS-KVRESS           TO CLAG-KVRESS                          
122500                                     LOGA-KVANTAL-E                       
122600         MOVE ZERO                TO LOGG-KVART-SALDO                     
122700         MOVE SPACE               TO LOGG-IDTECKEN-KVAKS-PAV              
122800         MOVE SPACE               TO LOGG-IDTECKEN-KVEFRS                 
122900         MOVE SPACE               TO LOGG-IDTECKEN-KVAKS                  
123000         MOVE SPACE               TO LOGG-IDTECKEN-KVLS                   
123100         MOVE CLAG-KVLS           TO LOGG-KVLS                            
123200         MOVE CLAG-KVAKS-CDC      TO LOGG-KVAKS                           
123300         MOVE CLAG-KVAKS-PAV      TO LOGG-KVAKS-PAV                       
123400         MOVE CLAG-KVEFRS         TO LOGG-KVEFRS                          
123500       END-IF                                                             
123600****************BO SALDO******************************                    
123700       IF MID-JUST7  NOT = ALL ' '                                        
123800         MOVE CLAG-KVROS          TO LOGA-KVANTAL-F                       
123900         IF MID-JUST7 = '+'                                               
124000           COMPUTE WS-KVROS                                               
124100                   = CLAG-KVROS     + WS-BO-IN                            
124200           MOVE '+'               TO WS-TECKEN                            
124300         ELSE                                                             
124400           COMPUTE WS-KVROS                                               
124500                   = CLAG-KVROS     - WS-BO-IN                            
124600           MOVE '-'               TO WS-TECKEN                            
124700         END-IF                                                           
124800         MOVE 'BO '               TO WS-TYP                               
124900                                     LOGA-IDSTYP                          
125000         MOVE WS-BO-IN           TO WS-ANTAL                              
125100                                     LOGA-KVANTAL                         
125200         MOVE WS-KVROS            TO CLAG-KVROS                           
125300                                     LOGA-KVANTAL-E                       
125400         MOVE ZERO                TO LOGG-KVART-SALDO                     
125500         MOVE SPACE               TO LOGG-IDTECKEN-KVAKS-PAV              
125600         MOVE SPACE               TO LOGG-IDTECKEN-KVEFRS                 
125700         MOVE SPACE               TO LOGG-IDTECKEN-KVAKS                  
125800         MOVE SPACE               TO LOGG-IDTECKEN-KVLS                   
125900         MOVE CLAG-KVLS           TO LOGG-KVLS                            
126000         MOVE CLAG-KVAKS-CDC      TO LOGG-KVAKS                           
126100         MOVE CLAG-KVAKS-PAV      TO LOGG-KVAKS-PAV                       
126200         MOVE CLAG-KVEFRS         TO LOGG-KVEFRS                          
126300       END-IF                                                             
126400       PERFORM IMS-REPL-WDK611                                            
126410***** THIS CODE HAS TO BE REMOVED TO FIX PRODUCTION PROBLEM               
126500       IF MID-JUST4  NOT = ALL ' '                                        
126600         PERFORM IMS-REPL-WDK901                                          
126700       END-IF                                                             
126710**** END                                                                  
126800       IF MID-COMM NOT = ALL '+'                                          
126900         MOVE MID-COMM                TO WS-COMM                          
127000       ELSE                                                               
127100         MOVE SPACE                   TO WS-COMM                          
127200       END-IF                                                             
127300       PERFORM S01-FLYTTA-LOGG-WDL9                                       
127400       PERFORM S02-FLYTTA-LOGG-WDP9-CDC                                   
127500                                                                          
127600                                                                          
127700       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
127800       CALL WMEDKONV USING MED-WMEDAREA                                   
127900       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
128000       PERFORM MFS-FORM-ATTR                                              
128100       PERFORM MFS-RENSA-FAELT-IN                                         
128200     END-IF                                                               
128300     .                                                                    
128400     EJECT                                                                
128500                                                                          
128600 HB-UPPDATERA-OVRIGA SECTION.                                             
128700                                                                          
128800     PERFORM IMS-GHU-WDK711                                               
128900     IF SEGMENT-FINNS                                                     
129000****************AK SALDO******************************                    
129100       IF MID-JUST1  NOT = ALL ' '                                        
129200         IF MID-JUST1 = '+'                                               
129300           COMPUTE WS-KVAKS = SLAG-KVAKS-SDC + WS-KVAKS-IN                
129400           COMPUTE WS-KVLS  = SLAG-KVLS  - WS-KVAKS-IN                    
129500           MOVE '+'               TO LOGG-IDTECKEN-KVAKS                  
129600           MOVE '-'               TO LOGG-IDTECKEN-KVLS                   
129700         ELSE                                                             
129800           COMPUTE WS-KVAKS = SLAG-KVAKS-SDC - WS-KVAKS-IN                
129900           COMPUTE WS-KVLS  = SLAG-KVLS  + WS-KVAKS-IN                    
130000           MOVE '-'               TO LOGG-IDTECKEN-KVAKS                  
130100           MOVE '+'               TO LOGG-IDTECKEN-KVLS                   
130200         END-IF                                                           
130300         MOVE SPACE               TO LOGG-IDTECKEN-KVAKS-PAV              
130400         MOVE SPACE               TO LOGG-IDTECKEN-KVEFRS                 
130500         MOVE SLAG-KVAKS-SDC      TO LOGA-KVANTAL-F                       
130600         MOVE WS-KVAKS-IN        TO LOGG-KVART-SALDO                      
130700                                     LOGA-KVANTAL                         
130800         MOVE WS-KVLS             TO LOGG-KVLS                            
130900                                     SLAG-KVLS                            
131000         MOVE WS-KVAKS            TO LOGG-KVAKS                           
131100                                     SLAG-KVAKS-SDC                       
131200                                     LOGA-KVANTAL-E                       
131300         MOVE SLAG-KVAKS-PAV      TO LOGG-KVAKS-PAV                       
131400         MOVE SLAG-KVEFRS         TO LOGG-KVEFRS                          
131500         MOVE 'AK'                TO LOGA-IDSTYP                          
131600       END-IF                                                             
131700****************EFR SALDO*****************************                    
131800       IF MID-JUST2  NOT = ALL ' '                                        
131900         IF MID-JUST2 = '+'                                               
132000           COMPUTE WS-KVEFRS = SLAG-KVEFRS + WS-KVEFRS-IN                 
132100           COMPUTE WS-KVLS   = SLAG-KVLS  - WS-KVEFRS-IN                  
132200           MOVE '+'               TO LOGG-IDTECKEN-KVEFRS                 
132300           MOVE '-'               TO LOGG-IDTECKEN-KVLS                   
132400         ELSE                                                             
132500           COMPUTE WS-KVEFRS = SLAG-KVEFRS - WS-KVEFRS-IN                 
132600           COMPUTE WS-KVLS   = SLAG-KVLS  + WS-KVEFRS-IN                  
132700           MOVE '-'               TO LOGG-IDTECKEN-KVEFRS                 
132800           MOVE '+'               TO LOGG-IDTECKEN-KVLS                   
132900         END-IF                                                           
133000         MOVE SPACE               TO LOGG-IDTECKEN-KVAKS-PAV              
133100         MOVE SPACE               TO LOGG-IDTECKEN-KVAKS                  
133200         MOVE WS-KVEFRS-IN       TO LOGG-KVART-SALDO                      
133300                                     LOGA-KVANTAL                         
133400         MOVE SLAG-KVEFRS         TO LOGA-KVANTAL-F                       
133500         MOVE WS-KVLS             TO LOGG-KVLS                            
133600                                     SLAG-KVLS                            
133700         MOVE WS-KVEFRS           TO LOGG-KVEFRS                          
133800                                     SLAG-KVEFRS                          
133900                                     LOGA-KVANTAL-E                       
134000         MOVE SLAG-KVAKS-PAV      TO LOGG-KVAKS-PAV                       
134100         MOVE SLAG-KVAKS-SDC      TO LOGG-KVAKS                           
134200         MOVE 'EFR'               TO LOGA-IDSTYP                          
134300       END-IF                                                             
134400****************GIT SALDO*****************************                    
134500       IF MID-JUST3  NOT = ALL ' '                                        
134600         IF MID-JUST3 = '+'                                               
134700           COMPUTE WS-KVAKS-PAV                                           
134800                   = SLAG-KVAKS-PAV + WS-KVAKS-PAV-IN                     
134900           COMPUTE WS-KVAKS  = SLAG-KVAKS-SDC - WS-KVAKS-PAV-IN           
135000           MOVE '+'               TO LOGG-IDTECKEN-KVAKS-PAV              
135100           MOVE '-'               TO LOGG-IDTECKEN-KVAKS                  
135200         ELSE                                                             
135300           COMPUTE WS-KVAKS-PAV                                           
135400                   = SLAG-KVAKS-PAV - WS-KVAKS-PAV-IN                     
135500           COMPUTE WS-KVAKS  = SLAG-KVAKS-SDC + WS-KVAKS-PAV-IN           
135600           MOVE '-'               TO LOGG-IDTECKEN-KVAKS-PAV              
135700           MOVE '+'               TO LOGG-IDTECKEN-KVAKS                  
135800         END-IF                                                           
135900         MOVE SPACE               TO LOGG-IDTECKEN-KVEFRS                 
136000         MOVE SPACE               TO LOGG-IDTECKEN-KVLS                   
136100         MOVE WS-KVAKS-PAV-IN    TO LOGG-KVART-SALDO                      
136200                                     LOGA-KVANTAL                         
136300         MOVE SLAG-KVAKS-PAV      TO LOGA-KVANTAL-F                       
136400         MOVE WS-KVAKS-PAV        TO LOGG-KVAKS-PAV                       
136500                                     SLAG-KVAKS-PAV                       
136600                                     LOGA-KVANTAL-E                       
136700         MOVE WS-KVAKS            TO LOGG-KVAKS                           
136800                                     SLAG-KVAKS-SDC                       
136900         MOVE SLAG-KVEFRS         TO LOGG-KVEFRS                          
137000         MOVE SLAG-KVLS           TO LOGG-KVLS                            
137100         MOVE 'GIT'               TO LOGA-IDSTYP                          
137200       END-IF                                                             
137300****************OQB SALDO*****************************                    
137400       IF MID-JUST4  NOT = ALL ' '                                        
137500         IF MID-VDB4 = 'D'                                                
137600           MOVE SLAG-KVOKS-DAG  TO LOGA-KVANTAL-F                         
137700           IF MID-JUST4 = '+'                                             
137800             COMPUTE WS-KVOKS-DAG                                         
137900                     = SLAG-KVOKS-DAG + WS-OQB-IN                         
138000             MOVE WS-KVOKS-DAG       TO SLAG-KVOKS-DAG                    
138100             MOVE '+'             TO WS-TECKEN                            
138200           ELSE                                                           
138300             COMPUTE WS-KVOKS-DAG                                         
138400                     = SLAG-KVOKS-DAG - WS-OQB-IN                         
138500             MOVE WS-KVOKS-DAG       TO SLAG-KVOKS-DAG                    
138600             MOVE '-'             TO WS-TECKEN                            
138700           END-IF                                                         
138800           MOVE WS-KVOKS-DAG         TO LOGA-KVANTAL-E                    
138900           MOVE 'OQD'                TO WS-TYP                            
139000                                        LOGA-IDSTYP                       
139100           MOVE WS-OQB-IN           TO WS-ANTAL                           
139200                                        LOGA-KVANTAL                      
139300         END-IF                                                           
139400         IF MID-VDB4 = 'B'                                                
139500           MOVE SLAG-KVOKS-BULK TO LOGA-KVANTAL-F                         
139600           IF MID-JUST4 = '+'                                             
139700             COMPUTE WS-KVOKS-BULK                                        
139800                     = SLAG-KVOKS-BULK + WS-OQB-IN                        
139900             MOVE WS-KVOKS-BULK      TO SLAG-KVOKS-BULK                   
140000             MOVE '+'             TO WS-TECKEN                            
140100           ELSE                                                           
140200             COMPUTE WS-KVOKS-BULK                                        
140300                     = SLAG-KVOKS-BULK - WS-OQB-IN                        
140400             MOVE WS-KVOKS-BULK      TO SLAG-KVOKS-BULK                   
140500             MOVE '-'             TO WS-TECKEN                            
140600           END-IF                                                         
140700           MOVE WS-KVOKS-BULK        TO LOGA-KVANTAL-E                    
140800           MOVE 'OQB'                TO WS-TYP                            
140900                                        LOGA-IDSTYP                       
141000           MOVE WS-OQB-IN           TO WS-ANTAL                           
141100                                        LOGA-KVANTAL                      
141200         END-IF                                                           
141300         MOVE ZERO                TO LOGG-KVART-SALDO                     
141400         MOVE SPACE               TO LOGG-IDTECKEN-KVAKS-PAV              
141500         MOVE SPACE               TO LOGG-IDTECKEN-KVEFRS                 
141600         MOVE SPACE               TO LOGG-IDTECKEN-KVAKS                  
141700         MOVE SPACE               TO LOGG-IDTECKEN-KVLS                   
141800         MOVE SLAG-KVLS           TO LOGG-KVLS                            
141900         MOVE SLAG-KVAKS-SDC      TO LOGG-KVAKS                           
142000         MOVE SLAG-KVAKS-PAV      TO LOGG-KVAKS-PAV                       
142100         MOVE SLAG-KVEFRS         TO LOGG-KVEFRS                          
142200       END-IF                                                             
142300****************REFILL ORDER SALDO********************                    
142400       IF MID-JUST5  NOT = ALL ' '                                        
142500         MOVE SLAG-KVBEART        TO LOGA-KVANTAL-F                       
142600         IF MID-JUST5 = '+'                                               
142700           COMPUTE WS-KVBEART                                             
142800                   = SLAG-KVBEART   + WS-KVBEART-IN                       
142900           MOVE '+'               TO WS-TECKEN                            
143000         ELSE                                                             
143100           COMPUTE WS-KVBEART                                             
143200                   = SLAG-KVBEART   - WS-KVBEART-IN                       
143300           MOVE '-'               TO WS-TECKEN                            
143400         END-IF                                                           
143500         MOVE 'REF'               TO WS-TYP                               
143600                                     LOGA-IDSTYP                          
143700         MOVE WS-KVBEART-IN      TO WS-ANTAL                              
143800                                     LOGA-KVANTAL                         
143900         MOVE WS-KVBEART          TO SLAG-KVBEART                         
144000                                     LOGA-KVANTAL-E                       
144100         MOVE ZERO                TO LOGG-KVART-SALDO                     
144200         MOVE SPACE               TO LOGG-IDTECKEN-KVAKS-PAV              
144300         MOVE SPACE               TO LOGG-IDTECKEN-KVEFRS                 
144400         MOVE SPACE               TO LOGG-IDTECKEN-KVAKS                  
144500         MOVE SPACE               TO LOGG-IDTECKEN-KVLS                   
144600         MOVE SLAG-KVLS           TO LOGG-KVLS                            
144700         MOVE SLAG-KVAKS-SDC      TO LOGG-KVAKS                           
144800         MOVE SLAG-KVAKS-PAV      TO LOGG-KVAKS-PAV                       
144900         MOVE SLAG-KVEFRS         TO LOGG-KVEFRS                          
145000       END-IF                                                             
145100****************RESERVED SALDO************************                    
145200       IF MID-JUST6  NOT = ALL ' '                                        
145300         MOVE SLAG-KVRESS         TO LOGA-KVANTAL-F                       
145400         IF MID-JUST6 = '+'                                               
145500           COMPUTE WS-KVRESS                                              
145600                   = SLAG-KVRESS    + WS-KVRESS-IN                        
145700           MOVE '+'               TO WS-TECKEN                            
145800         ELSE                                                             
145900           COMPUTE WS-KVRESS                                              
146000                   = SLAG-KVRESS    - WS-KVRESS-IN                        
146100           MOVE '-'               TO WS-TECKEN                            
146200         END-IF                                                           
146300         MOVE 'RES'               TO WS-TYP                               
146400                                     LOGA-IDSTYP                          
146500         MOVE WS-KVRESS-IN       TO WS-ANTAL                              
146600                                     LOGA-KVANTAL                         
146700         MOVE WS-KVRESS           TO SLAG-KVRESS                          
146800                                     LOGA-KVANTAL-E                       
146900         MOVE ZERO                TO LOGG-KVART-SALDO                     
147000         MOVE SPACE               TO LOGG-IDTECKEN-KVAKS-PAV              
147100         MOVE SPACE               TO LOGG-IDTECKEN-KVEFRS                 
147200         MOVE SPACE               TO LOGG-IDTECKEN-KVAKS                  
147300         MOVE SPACE               TO LOGG-IDTECKEN-KVLS                   
147400         MOVE SLAG-KVLS           TO LOGG-KVLS                            
147500         MOVE SLAG-KVAKS-SDC      TO LOGG-KVAKS                           
147600         MOVE SLAG-KVAKS-PAV      TO LOGG-KVAKS-PAV                       
147700         MOVE SLAG-KVEFRS         TO LOGG-KVEFRS                          
147800       END-IF                                                             
147900****************BO SALDO******************************                    
148000       IF MID-JUST7  NOT = ALL ' '                                        
148100         IF MID-VDB7 = 'D'                                                
148200           MOVE SLAG-KVROS-DAG       TO LOGA-KVANTAL-F                    
148300           IF MID-JUST7 = '+'                                             
148400             COMPUTE WS-KVROS-DAG                                         
148500                     = SLAG-KVROS-DAG + WS-BO-IN                          
148600             MOVE WS-KVROS-DAG       TO SLAG-KVROS-DAG                    
148700                                        LOGA-KVANTAL-E                    
148800             MOVE '+'             TO WS-TECKEN                            
148900           ELSE                                                           
149000             COMPUTE WS-KVROS-DAG                                         
149100                     = SLAG-KVROS-DAG - WS-BO-IN                          
149200             MOVE WS-KVROS-DAG       TO SLAG-KVROS-DAG                    
149300                                        LOGA-KVANTAL-E                    
149400             MOVE '-'             TO WS-TECKEN                            
149500           END-IF                                                         
149600           MOVE 'BOD'             TO WS-TYP                               
149700                                     LOGA-IDSTYP                          
149800           MOVE WS-BO-IN         TO WS-ANTAL                              
149900                                     LOGA-KVANTAL                         
150000         END-IF                                                           
150100         IF MID-VDB7 = 'B'                                                
150200           MOVE SLAG-KVROS-BULK      TO LOGA-KVANTAL-F                    
150300           IF MID-JUST7 = '+'                                             
150400             COMPUTE WS-KVROS-BULK                                        
150500                     = SLAG-KVROS-BULK + WS-BO-IN                         
150600             MOVE WS-KVROS-BULK      TO SLAG-KVROS-BULK                   
150700                                        LOGA-KVANTAL-E                    
150800             MOVE '+'             TO WS-TECKEN                            
150900           ELSE                                                           
151000             COMPUTE WS-KVROS-BULK                                        
151100                     = SLAG-KVROS-BULK - WS-BO-IN                         
151200             MOVE WS-KVROS-BULK      TO SLAG-KVROS-BULK                   
151300                                        LOGA-KVANTAL-E                    
151400             MOVE '-'             TO WS-TECKEN                            
151500           END-IF                                                         
151600           MOVE 'BOB'             TO WS-TYP                               
151700                                     LOGA-IDSTYP                          
151800           MOVE WS-BO-IN         TO WS-ANTAL                              
151900                                     LOGA-KVANTAL                         
152000         END-IF                                                           
152100         MOVE ZERO                TO LOGG-KVART-SALDO                     
152200         MOVE SPACE               TO LOGG-IDTECKEN-KVAKS-PAV              
152300         MOVE SPACE               TO LOGG-IDTECKEN-KVEFRS                 
152400         MOVE SPACE               TO LOGG-IDTECKEN-KVAKS                  
152500         MOVE SPACE               TO LOGG-IDTECKEN-KVLS                   
152600         MOVE SLAG-KVLS           TO LOGG-KVLS                            
152700         MOVE SLAG-KVAKS-SDC      TO LOGG-KVAKS                           
152800         MOVE SLAG-KVAKS-PAV      TO LOGG-KVAKS-PAV                       
152900         MOVE SLAG-KVEFRS         TO LOGG-KVEFRS                          
153000       END-IF                                                             
153100       PERFORM IMS-REPL-WDK711                                            
153200       IF MID-COMM NOT = ALL '+'                                          
153300         MOVE MID-COMM                TO WS-COMM                          
153400       ELSE                                                               
153500         MOVE SPACE                   TO WS-COMM                          
153600       END-IF                                                             
153700       PERFORM S01-FLYTTA-LOGG-WDL9                                       
153800       PERFORM S03-FLYTTA-LOGG-WDP9-NOT-CDC                               
153900                                                                          
154000       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
154100       CALL WMEDKONV USING MED-WMEDAREA                                   
154200       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
154300       PERFORM MFS-FORM-ATTR                                              
154400       PERFORM MFS-RENSA-FAELT-IN                                         
154500     END-IF                                                               
154600     .                                                                    
154700     EJECT                                                                
154800 S01-FLYTTA-LOGG-WDL9 SECTION.                                            
154900                                                                          
155000* LÄGGER UPP SALDOLOGG I WDL9                                             
155100     MOVE W-IDARTNR             TO LOGG-IDARTNR                           
155200     COMPUTE LOGG-DAREGDAT-9KOMPL = 99999999 - W-DAGENS-DATUM             
155300     ACCEPT TRANS-TID FROM TIME                                           
155400     COMPUTE LOGG-TIKLOCK-9KOMPL  = 999999999 - TRANS-TID                 
155500     MOVE 9                       TO LOGG-IDSEKVNR                        
155600     MOVE W-IDDC                  TO LOGG-IDDC                            
155700     MOVE 'MISC'                  TO LOGG-IDHUVTYP                        
155800     MOVE 'ADJ'                   TO LOGG-IDSUBTYP                        
155900     MOVE IDPGM                   TO LOGG-IDPGM                           
156000     MOVE '5166'                  TO LOGG-IDTRANS                         
156100     MOVE MSG-SIGNON-USERID       TO LOGG-IDUSER                          
156200     MOVE SPACE                   TO LOGG-REF                             
156300     MOVE WS-COMM                 TO LOGG-UREF1                           
156400     IF MID-JUST1    NOT = ALL ' '                                        
156500     OR MID-JUST2    NOT = ALL ' '                                        
156600     OR MID-JUST3    NOT = ALL ' '                                        
156700       MOVE SPACE                 TO LOGG-UREF2                           
156800     ELSE                                                                 
156900       MOVE WS-LOGGRAD            TO LOGG-UREF2                           
157000     END-IF                                                               
157100     MOVE ZERO                    TO LOGG-DAREGDAT-LADD                   
157200     PERFORM IMS-ISRT-WDL901                                              
157300     IF SEGMENT-FINNS-REDAN                                               
157400       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
157500          ADD -1 TO LOGG-IDSEKVNR                                         
157600          PERFORM IMS-ISRT-WDL901                                         
157700       END-PERFORM                                                        
157800     END-IF                                                               
157900                                                                          
158000     .                                                                    
158100     EJECT                                                                
158200                                                                          
158300 S02-FLYTTA-LOGG-WDP9-CDC SECTION.                                        
158400                                                                          
158500     IF MID-JUST4    NOT = ALL ' '                                        
158600       MOVE 'WDK901'              TO LOGA-IDSEGM                          
158700     ELSE                                                                 
158800       MOVE 'WDK611'              TO LOGA-IDSEGM                          
158900     END-IF                                                               
159000     MOVE W-IDARTNR               TO LOGA-IDARTNR                         
159100     MOVE '11'                    TO LOGA-IDDC                            
159200     MOVE IDPGM                   TO LOGA-IDPGM                           
159300     MOVE '5166'                  TO LOGA-IDTRANS                         
159400     MOVE MSGI-IDUSER             TO LOGA-IDUSER                          
159500     COMPUTE LOGA-TISEGKEY-9KOMPL =                                       
159600             999999999 - WS-DATUM-LOPNR                                   
159700     PERFORM IMS-ISRT-WDP901                                              
159800     IF SEGMENT-FINNS-REDAN                                               
159900       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
160000         ADD +1 TO WS-IDLOPNR                                             
160100         COMPUTE LOGA-TISEGKEY-9KOMPL =                                   
160200             999999999 - WS-DATUM-LOPNR                                   
160300         PERFORM IMS-ISRT-WDP901                                          
160400       END-PERFORM                                                        
160500     END-IF                                                               
160600     .                                                                    
160700     EJECT                                                                
160800 S03-FLYTTA-LOGG-WDP9-NOT-CDC SECTION.                                    
160900                                                                          
161000     MOVE 'WDK711'                TO LOGA-IDSEGM                          
161100     MOVE W-IDARTNR               TO LOGA-IDARTNR                         
161200     MOVE W-IDDC                  TO LOGA-IDDC                            
161300     MOVE IDPGM                   TO LOGA-IDPGM                           
161400     MOVE '5166'                  TO LOGA-IDTRANS                         
161500     MOVE MSGI-IDUSER             TO LOGA-IDUSER                          
161600     COMPUTE LOGA-TISEGKEY-9KOMPL =                                       
161700             999999999 - WS-DATUM-LOPNR                                   
161800     PERFORM IMS-ISRT-WDP901                                              
161900     IF SEGMENT-FINNS-REDAN                                               
162000       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
162100         ADD +1 TO WS-IDLOPNR                                             
162200         COMPUTE LOGA-TISEGKEY-9KOMPL =                                   
162300             999999999 - WS-DATUM-LOPNR                                   
162400         PERFORM IMS-ISRT-WDP901                                          
162500       END-PERFORM                                                        
162600     END-IF                                                               
162700     .                                                                    
162800     EJECT                                                                
162900 MFS-RENSA-FAELT-UT SECTION.                                              
163000                                                                          
163100*    --- ALLA UTDATA-FÄLT                                                 
163200*    --- INKL. BLÄDDRINGSNYCKLAR                                          
163300     MOVE MFS-RENSA-FAELT TO MOD-BEART                                    
163400                             MOD-PRARTSTD                                 
163500                             MOD-KVLS                                     
163600                             MOD-AVAILABLE                                
163700                             MOD-KVAKS                                    
163800                             MOD-KVEFRS                                   
163900                             MOD-KVAKS-PAV                                
164000                             MOD-OQB                                      
164100                             MOD-KVBEART                                  
164200                             MOD-KVRESS                                   
164300                             MOD-BO                                       
164400                             MOD-COMM                                     
164500                             MOD-KVAKS-IN                                 
164600                             MOD-KVEFRS-IN                                
164700                             MOD-KVAKS-PAV-IN                             
164800                             MOD-OQB-IN                                   
164900                             MOD-KVBEART-IN                               
165000                             MOD-KVRESS-IN                                
165100                             MOD-BO-IN                                    
165200                             MOD-JUST1                                    
165300                             MOD-JUST2                                    
165400                             MOD-JUST3                                    
165500                             MOD-JUST4                                    
165600                             MOD-JUST5                                    
165700                             MOD-JUST6                                    
165800                             MOD-JUST7                                    
165900                             MOD-VDB4                                     
166000                             MOD-VDB7                                     
166100     MOVE +1   TO IX                                                      
166200     PERFORM UNTIL IX > 14                                                
166300       MOVE MFS-RENSA-FAELT TO MOD-DATE(IX)                               
166400                               MOD-QTY(IX)                                
166500                               MOD-TYPE(IX)                               
166600                               MOD-IDUSER(IX)                             
166700       ADD +1 TO IX                                                       
166800     END-PERFORM                                                          
166900     .                                                                    
167000     SKIP3                                                                
167100 MFS-RENSA-FAELT-IN SECTION.                                              
167200                                                                          
167300*    --- ALLA INDATA-FÄLT                                                 
167400     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
167500                             MOD-IDDC-IN                                  
167600                             MOD-KVAKS-IN                                 
167700                             MOD-KVEFRS-IN                                
167800                             MOD-KVAKS-PAV-IN                             
167900                             MOD-OQB-IN                                   
168000                             MOD-KVBEART-IN                               
168100                             MOD-KVRESS-IN                                
168200                             MOD-BO-IN                                    
168300                             MOD-JUST1                                    
168400                             MOD-JUST2                                    
168500                             MOD-JUST3                                    
168600                             MOD-JUST4                                    
168700                             MOD-JUST5                                    
168800                             MOD-JUST6                                    
168900                             MOD-JUST7                                    
169000                             MOD-VDB4                                     
169100                             MOD-VDB7                                     
169200                             MOD-COMM                                     
169300     .                                                                    
169400     EJECT                                                                
169500 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
169600                                                                          
169700*    --- ALLA UTDATA-FÄLT                                                 
169800*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
169900     MOVE MFS-ROER-EJ-FAELT TO MOD-BEART                                  
170000                               MOD-PRARTSTD                               
170100                               MOD-KVLS                                   
170200                               MOD-AVAILABLE                              
170300                               MOD-KVAKS                                  
170400                               MOD-KVEFRS                                 
170500                               MOD-KVAKS-PAV                              
170600                               MOD-OQB                                    
170700                               MOD-KVBEART                                
170800                               MOD-KVRESS                                 
170900                               MOD-BO                                     
171000                               MOD-COMM                                   
171100                               MOD-KVAKS-IN                               
171200                               MOD-KVEFRS-IN                              
171300                               MOD-KVAKS-PAV-IN                           
171400                               MOD-OQB-IN                                 
171500                               MOD-KVBEART-IN                             
171600                               MOD-KVRESS-IN                              
171700                               MOD-BO-IN                                  
171800                               MOD-JUST1                                  
171900                               MOD-JUST2                                  
172000                               MOD-JUST3                                  
172100                               MOD-JUST4                                  
172200                               MOD-JUST5                                  
172300                               MOD-JUST6                                  
172400                               MOD-JUST7                                  
172500                               MOD-VDB4                                   
172600                               MOD-VDB7                                   
172700     MOVE +1   TO IX                                                      
172800     PERFORM UNTIL IX > 14                                                
172900       MOVE MFS-ROER-EJ-FAELT TO MOD-DATE(IX)                             
173000                                 MOD-QTY(IX)                              
173100                                 MOD-TYPE(IX)                             
173200                                 MOD-IDUSER(IX)                           
173300       ADD +1 TO IX                                                       
173400     END-PERFORM                                                          
173500     .                                                                    
173600     SKIP2                                                                
173700 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
173800                                                                          
173900*    --- ALLA INDATA-FÄLT                                                 
174000     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-IN                             
174100                               MOD-IDDC-IN                                
174200                               MOD-KVAKS-IN                               
174300                               MOD-KVEFRS-IN                              
174400                               MOD-KVAKS-PAV-IN                           
174500                               MOD-OQB-IN                                 
174600                               MOD-KVBEART-IN                             
174700                               MOD-KVRESS-IN                              
174800                               MOD-BO-IN                                  
174900                               MOD-JUST1                                  
175000                               MOD-JUST2                                  
175100                               MOD-JUST3                                  
175200                               MOD-JUST4                                  
175300                               MOD-JUST5                                  
175400                               MOD-JUST6                                  
175500                               MOD-JUST7                                  
175600                               MOD-VDB4                                   
175700                               MOD-VDB7                                   
175800                               MOD-COMM                                   
175900     .                                                                    
176000     EJECT                                                                
176100 MFS-FORM-ATTR SECTION.                                                   
176200                                                                          
176300*    --- ALLA INDATA-FÄLT                                                 
176400     MOVE MFS-FORMATETS-ATTR TO MOD-JUST1-ATTR                            
176500                                MOD-JUST2-ATTR                            
176600                                MOD-JUST3-ATTR                            
176700                                MOD-JUST4-ATTR                            
176800                                MOD-JUST5-ATTR                            
176900                                MOD-JUST6-ATTR                            
177000                                MOD-JUST7-ATTR                            
177100                                MOD-KVAKS-IN-ATTR                         
177200                                MOD-KVEFRS-IN-ATTR                        
177300                                MOD-KVAKS-PAV-IN-ATTR                     
177400                                MOD-OQB-IN-ATTR                           
177500                                MOD-KVBEART-IN-ATTR                       
177600                                MOD-KVRESS-IN-ATTR                        
177700                                MOD-BO-IN-ATTR                            
177800                                MOD-VDB4-ATTR                             
177900                                MOD-VDB7-ATTR                             
178000                                MOD-COMM-ATTR                             
178100     .                                                                    
178200     SKIP2                                                                
178300 MFS-LAES-IN-IGEN SECTION.                                                
178400                                                                          
178500*    --- ALLA INDATA-FÄLT                                                 
178600*    MOVE MFS-ADD-LAES-IN-FAELT TO MOD-JUST2-ATTR                         
178700*                                  MOD-JUST2-ATTR                         
178800*                                  MOD-JUST3-ATTR                         
178900*                                  MOD-JUST4-ATTR                         
179000*                                  MOD-JUST5-ATTR                         
179100*                                  MOD-JUST6-ATTR                         
179200*                                  MOD-JUST7-ATTR                         
179300     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVAKS-IN-ATTR                      
179400                                   MOD-KVEFRS-IN-ATTR                     
179500                                   MOD-KVAKS-PAV-IN-ATTR                  
179600                                   MOD-OQB-IN-ATTR                        
179700                                   MOD-KVBEART-IN-ATTR                    
179800                                   MOD-KVRESS-IN-ATTR                     
179900                                   MOD-BO-IN-ATTR                         
180000                                   MOD-VDB4-ATTR                          
180100                                   MOD-VDB7-ATTR                          
180200                                   MOD-COMM-ATTR                          
180300     .                                                                    
180400     SKIP2                                                                
180500* --- IMS SEKTIONER ---                                                   
180600     SKIP3                                                                
180700 IMS-GET-MSG SECTION.                                                     
180800                                                                          
180900     MOVE '  QC' TO GODK-STATUSKODER                                      
181000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
181100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
181200     PERFORM IMS-STATUSKONTROLL                                           
181300     .                                                                    
181400     SKIP3                                                                
181500 IMS-INSERT-MSG SECTION.                                                  
181600                                                                          
181700     MOVE 'N' TO MFS-KDHUVOMR                                             
181800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
181900     MOVE SPACE TO GODK-STATUSKODER                                       
182000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
182100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
182200     PERFORM IMS-STATUSKONTROLL                                           
182300     .                                                                    
182400     EJECT                                                                
182500 IMS-GU-WDK601 SECTION.                                                   
182600                                                                          
182700     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
182800          DELIMITED BY SIZE INTO SSA1                                     
182900     MOVE '  GE' TO GODK-STATUSKODER                                      
183000     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
183100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
183200     PERFORM IMS-STATUSKONTROLL                                           
183300     .                                                                    
183400     EJECT                                                                
183500 IMS-GNP-WDK611 SECTION.                                                  
183600                                                                          
183700     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
183800          DELIMITED BY SIZE INTO SSA1                                     
183900     MOVE '  GE' TO GODK-STATUSKODER                                      
184000     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
184100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
184200     PERFORM IMS-STATUSKONTROLL                                           
184300     .                                                                    
184400     SKIP3                                                                
184500 IMS-GHU-WDK611 SECTION.                                                  
184600                                                                          
184700     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
184800          DELIMITED BY SIZE INTO SSA1                                     
184900     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
185000          DELIMITED BY SIZE INTO SSA2                                     
185100     MOVE '  GE' TO GODK-STATUSKODER                                      
185200     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
185300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
185400     PERFORM IMS-STATUSKONTROLL                                           
185500     .                                                                    
185600     SKIP3                                                                
185700 IMS-REPL-WDK611 SECTION.                                                 
185800                                                                          
185900     MOVE '  ' TO GODK-STATUSKODER                                        
186000     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
186100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
186200     PERFORM IMS-STATUSKONTROLL                                           
186300     .                                                                    
186400     EJECT                                                                
186500 IMS-GU-WDK711 SECTION.                                                   
186600                                                                          
186700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
186800          DELIMITED BY SIZE INTO SSA1                                     
186900     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
187000          DELIMITED BY SIZE INTO SSA2                                     
187100     MOVE '  GE' TO GODK-STATUSKODER                                      
187200     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
187300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
187400     PERFORM IMS-STATUSKONTROLL                                           
187500     .                                                                    
187600     SKIP3                                                                
187700 IMS-GHU-WDK711 SECTION.                                                  
187800                                                                          
187900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
188000          DELIMITED BY SIZE INTO SSA1                                     
188100     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
188200          DELIMITED BY SIZE INTO SSA2                                     
188300     MOVE '  GE' TO GODK-STATUSKODER                                      
188400     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
188500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
188600     PERFORM IMS-STATUSKONTROLL                                           
188700     .                                                                    
188800     SKIP3                                                                
188900 IMS-REPL-WDK711 SECTION.                                                 
189000                                                                          
189100     MOVE '  ' TO GODK-STATUSKODER                                        
189200     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
189300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
189400     PERFORM IMS-STATUSKONTROLL                                           
189500     .                                                                    
189600     EJECT                                                                
189700 IMS-GU-WDD301-BSEQ   SECTION.                                            
189800                                                                          
189900     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
190000            DELIMITED BY SIZE INTO SSA1                                   
190100     MOVE '  GE' TO GODK-STATUSKODER                                      
190200     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD301 SSA1                    
190300     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
190400     PERFORM IMS-STATUSKONTROLL                                           
190500     .                                                                    
190600     SKIP3                                                                
190700 IMS-GU-WDD311-BSEQ   SECTION.                                            
190800                                                                          
190900     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
191000            DELIMITED BY SIZE INTO SSA1                                   
191100     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
191200            DELIMITED BY SIZE INTO SSA2                                   
191300     MOVE '  GE' TO GODK-STATUSKODER                                      
191400     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
191500     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
191600     PERFORM IMS-STATUSKONTROLL                                           
191700     .                                                                    
191800     SKIP3                                                                
191900 IMS-GNP-WDD311-BSEQ   SECTION.                                           
192000                                                                          
192100     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
192200            DELIMITED BY SIZE INTO SSA1                                   
192300     MOVE '  GE' TO GODK-STATUSKODER                                      
192400     CALL CBLTDLI USING GNP WDD3-PCB DLI-IO-WDD311 SSA1                   
192500     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
192600     PERFORM IMS-STATUSKONTROLL                                           
192700     .                                                                    
192800     EJECT                                                                
192900 IMS-GU-WDK901 SECTION.                                                   
193000                                                                          
193100     STRING 'WDK901  (IDARTNR  =' W-IDARTNR-X ')'                         
193200          DELIMITED BY SIZE INTO SSA1                                     
193300     MOVE '  GE' TO GODK-STATUSKODER                                      
193400     CALL CBLTDLI USING GU WDK9-PCB DLI-IO-WDK901 SSA1                    
193500     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
193600     PERFORM IMS-STATUSKONTROLL                                           
193700     .                                                                    
193800     SKIP3                                                                
193900 IMS-GHU-WDK901 SECTION.                                                  
194000                                                                          
194100     STRING 'WDK901  (IDARTNR  =' W-IDARTNR-X ')'                         
194200          DELIMITED BY SIZE INTO SSA1                                     
194300     MOVE '  GE' TO GODK-STATUSKODER                                      
194400     CALL CBLTDLI USING GHU WDK9-PCB DLI-IO-WDK901 SSA1                   
194500     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
194600     PERFORM IMS-STATUSKONTROLL                                           
194700     .                                                                    
194800     SKIP3                                                                
194900 IMS-REPL-WDK901 SECTION.                                                 
195000                                                                          
195100     MOVE '  II' TO GODK-STATUSKODER                                      
195200     CALL CBLTDLI USING REPL WDK9-PCB DLI-IO-WDK901                       
195300     MOVE WDK9-STATUS-CODE TO STATUS-WS                                   
195400     PERFORM IMS-STATUSKONTROLL                                           
195500     .                                                                    
195600     EJECT                                                                
195700 IMS-ISRT-WDL901 SECTION.                                                 
195800                                                                          
195900     MOVE 'WDL901 ' TO SSA1                                               
196000     MOVE '  II' TO GODK-STATUSKODER                                      
196100     CALL CBLTDLI USING ISRT WDL9-PCB DLI-IO-WDL901 SSA1                  
196200     MOVE WDL9-STATUS-CODE TO STATUS-WS                                   
196300     PERFORM IMS-STATUSKONTROLL                                           
196400     .                                                                    
196500     SKIP3                                                                
196600 IMS-GU-WDB601    SECTION.                                                
196700     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
196800          DELIMITED BY SIZE INTO SSA1                                     
196900     MOVE '  GE' TO GODK-STATUSKODER                                      
197000     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601  SSA1                   
197100     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
197200     PERFORM IMS-STATUSKONTROLL                                           
197300     .                                                                    
197400     EJECT                                                                
197500 IMS-GU-WDP901 SECTION.                                                   
197600                                                                          
197700     STRING 'WDP901  (WDP901KY>=' W-WDP901KY-MIN-X                        
197800                    '&WDP901KY<=' W-WDP901KY-MAX-X ')'                    
197900          DELIMITED BY SIZE INTO SSA1                                     
198000     MOVE '  GE' TO GODK-STATUSKODER                                      
198100     CALL CBLTDLI USING GU WDP9-PCB DLI-IO-WDP901 SSA1                    
198200     MOVE WDP9-STATUS-CODE TO STATUS-WS                                   
198300     PERFORM IMS-STATUSKONTROLL                                           
198400     .                                                                    
198500     SKIP3                                                                
198600 IMS-GN-WDP901 SECTION.                                                   
198700                                                                          
198800     STRING 'WDP901  (WDP901KY>=' W-WDP901KY-MIN-X                        
198900                    '&WDP901KY<=' W-WDP901KY-MAX-X ')'                    
199000          DELIMITED BY SIZE INTO SSA1                                     
199100     MOVE '  GE' TO GODK-STATUSKODER                                      
199200     CALL CBLTDLI USING GN WDP9-PCB DLI-IO-WDP901 SSA1                    
199300     MOVE WDP9-STATUS-CODE TO STATUS-WS                                   
199400     PERFORM IMS-STATUSKONTROLL                                           
199500     .                                                                    
199600     SKIP3                                                                
199700 IMS-ISRT-WDP901 SECTION.                                                 
199800                                                                          
199900     MOVE 'WDP901 ' TO SSA1                                               
200000     MOVE '  II' TO GODK-STATUSKODER                                      
200100     CALL CBLTDLI USING ISRT WDP9-PCB DLI-IO-WDP901 SSA1                  
200200     MOVE WDP9-STATUS-CODE TO STATUS-WS                                   
200300     PERFORM IMS-STATUSKONTROLL                                           
200400     .                                                                    
200500     SKIP3                                                                
200600 IMS-STATUSKONTROLL SECTION.                                              
200700                                                                          
200800     SET STATUS-IX TO 1                                                   
200900     SEARCH GODK-STATUS                                                   
201000       AT END                                                             
201100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
201200         DELIMITED BY SIZE INTO FELTEXT                                   
201300         CALL FELLOG                                                      
201400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
201500         CONTINUE                                                         
201600     END-SEARCH                                                           
201700     .                                                                    
