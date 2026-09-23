000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3030500.                                                
000300 AUTHOR.         GAVIN SMITH.                                             
000400 DATE-WRITTEN.   02/02/27.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*    MPP PRICEQUERY TO VIPS AT SALES COMPANY, SYNCHRONOUS.                
000900*                                                                         
001000*                                                                         
001100*    INDATA.                                                              
001200*        TRANSAKTION: W3T305                                              
001300*        MID:         W30305I1                                            
001400*                                                                         
001500*    UTDATA.                                                              
001600*        MOD:         W30305O1                                            
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000                                                                          
002100 DATA DIVISION.                                                           
002200     EJECT                                                                
002300 WORKING-STORAGE SECTION.                                                 
002400                                                                          
002500 77  IDPGM                       PIC X(08)   VALUE 'W3030500'.            
002600                                                                          
002700*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
002800 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
002900                                                                          
003000 77  JA                          PIC X       VALUE 'J'.                   
003100 77  NEJ                         PIC X       VALUE 'N'.                   
003200 77  FLSLUTA-LAS                 PIC X       VALUE 'N'.                   
003300 77  INDX                        PIC S9(9)   VALUE +0   COMP SYNC.        
003400                                                                          
003500*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
003600                                                                          
003700                                                                          
003800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
003900     88  NYCKLAR-OK                          VALUE 'J'.                   
004000     88  NYCKLAR-FEL                         VALUE 'N'.                   
004100                                                                          
004200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
004300     88  EGEN-MID                            VALUE '3305'.                
004400     88  GODK-MID                            VALUE '3301' '3302'          
004500                                                   '3303' '3304'          
004600                                                   '3305' '3306'          
004700                                                   '3307' '3308'          
004800                                                   '3309'.                
004900     88  HELP-MID                            VALUE '0551'.                
005000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005100     EJECT                                                                
005200 01  SPAR-PRARTBEL-PR    PIC S9(7)V9(5)      VALUE ZERO COMP-3.           
005300 01  SPAR-PRDIRLON       PIC S9(4)V9(3)      VALUE ZERO COMP-3.           
005400 01  SPAR-PRDMTRL        PIC S9(6)V9(3)      VALUE ZERO COMP-3.           
005500 01  SPAR-PROVRPAL       PIC S9(4)V9(3)      VALUE ZERO COMP-3.           
005600 01  WS-RETULF           PIC S9(3)V9(4)      VALUE ZERO COMP-3.           
005700 01  WS-PRARTBES-PR      PIC S9(7)V9(5)      VALUE ZERO COMP-3.           
005800 01  WS-PRARTBES-PR-LOC  PIC S9(7)V9(5)      VALUE ZERO COMP-3.           
005900 01  WS-PRARTSJK         PIC S9(7)V9(5)      VALUE ZERO COMP-3.           
006000 01  WS-PRARTSJK-LOC     PIC S9(7)V9(5)      VALUE ZERO COMP-3.           
006100 01  W-PRARTSJK-LOC      PIC S9(7)V9(2)      VALUE ZERO COMP-3.           
006200 01  WS-PRARTSJK-MONLOC  PIC S9(7)V9(2)      VALUE ZERO COMP-3.           
006300 01  WS-IDDC             PIC X(2)            VALUE SPACE.                 
006400 01  WS-REBVSPPR         PIC S9(3)V9(1)      VALUE ZERO COMP-3.           
006500 01  WS-PRARTNTO-LOC     PIC S9(7)V9(5)      VALUE ZERO COMP-3.           
006600 01  WS-KDVALISO         PIC X(3)            VALUE SPACE.                 
006700 01  W-DATE-AAMM         PIC 9(4)            VALUE ZERO.                  
006800 01  WS-KDVALISO-HUV     PIC X(3)            VALUE 'SEK'.                 
006900 01  W-WDK621-KDVALISO   PIC X(3)            VALUE SPACE.                 
007000 01  W-WDB2-KDKUNDKAT    PIC 9(2)            VALUE ZERO.                  
007100     SKIP2                                                                
007200                                                                          
007300     EJECT                                                                
007400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007500 01  GENERELLA-SUBPROGRAM.                                                
007600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008000     03  WZ01CALL                PIC X(8)    VALUE 'WZ01CALL'.            
008100     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
008200     EJECT                                                                
008300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
008400*01 -COPY WMEDAREA                                                        
008500     SKIP3                                                                
008600*    --- PARAMETRAR TILL SUBPROGRAM W510CURR                              
008700*01 -COPY W510CURR                                                        
008800     SKIP3                                                                
008900 01  MESSAGE-CODES.                                                       
009000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
009100     03  PART-MISSING            PIC X(3)    VALUE '017'.                 
009200     03  PART-SUPERSEDED         PIC X(3)    VALUE '018'.                 
009300     EJECT                                                                
009400*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
009500*                                                                         
009600 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
009700     SKIP3                                                                
009800*01 -COPY WMSGINIT                                                        
009900     EJECT                                                                
010000*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
010100*                                                                         
010200 01  SPAR-AREA.                                                           
010300     03  SPAR-IDTRANS           PIC X(4)    VALUE '3305'.                 
010400     EJECT                                                                
010500*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
010600*                                                                         
010700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
010800     SKIP3                                                                
010900*01  MID -COPY W30305I1                                                   
011000     EJECT                                                                
011100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
011200     SKIP3                                                                
011300*01  -COPY WMSGAREA                                                       
011400     EJECT                                                                
011500     03  MOD REDEFINES MSG-AREA.                                          
011600*      05  -COPY W30305O1                                                 
011700     EJECT                                                                
011800 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
011900     SKIP3                                                                
012000*01  -COPY WMFSAREA                                                       
012100     EJECT                                                                
012200*   -COPY  WWPRODSL                                                       
012300     EJECT                                                                
012400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012500*                                                                         
012600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012700     SKIP3                                                                
012800 01  NYCKLAR-TILL-DLI.                                                    
012900     03  W-IDHTYP-X.                                                      
013000         05  FILLER              PIC X(4)    VALUE '3101'.                
013100         05  W-IDDISTR           PIC 9(4)    VALUE ZERO.                  
013200         05  FILLER              PIC X(22)   VALUE LOW-VALUE.             
013300     SKIP2                                                                
013400* TILL WDK6.                                                              
013500     03  W-WDK6-IDARTNR-X.                                                
013600         05  W-IDARTNR            PIC S9(9)    VALUE ZERO COMP-3.         
013700     03  W-KDSEGKEY-X.                                                    
013800         05  W-KDSEGKEY           PIC X(1)     VALUE '1'.                 
013900* TILL WDK7.                                                              
014000     03  W-WDK701-IDARTNR-X.                                              
014100         05  W-WDK701-IDARTNR     PIC S9(9)    VALUE ZERO COMP-3.         
014200     03  W-WDK711-IDDC-X.                                                 
014300         05  W-WDK711-IDDC        PIC X(2)     VALUE SPACE.               
014400* TILL WDF1.                                                              
014500     03  W-WDF101-IDLEVNR-X.                                              
014600         05    W-WDF101-IDLEVNR   PIC X(5)     VALUE SPACE.               
014700     03  W-WDF102-IDLAND-X.                                               
014800         05    W-WDF102-IDLAND    PIC X(2)     VALUE SPACE.               
014900* TO WDB2                                                                 
015000     03  W-IDGMT-X.                                                       
015100         05  W-WDB2-IDDISTR      PIC S9(5)   VALUE ZERO COMP-3.           
015200         05  W-WDB2-IDKUNDNR     PIC S9(7)   VALUE ZERO COMP-3.           
015300                                                                          
015400*    --- STATUS-KOD FRÅN IMS                                              
015500 01  STATUS-WS                   PIC XX.                                  
015600     88  SEGMENT-FINNS                       VALUE '  '.                  
015700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
015800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
015900     SKIP2                                                                
016000 01  GODK-STATUSKODER.                                                    
016100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016200     SKIP3                                                                
016300 01  SSA1                        PIC X(64).                               
016400 01  SSA2                        PIC X(64).                               
016500     EJECT                                                                
016600*    --- IMS FUNKTIONSKODER                                               
016700*01  -COPY W0003                                                          
016800     EJECT                                                                
016900*    ---  DLI INPUT-OUTPUT AREA                                           
017000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3101'.                    
017100 01  DLI-IO-WDGX3101.                                                     
017200*    03  -COPY WDGX3101                                                   
017300     EJECT                                                                
017400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3102'.                    
017500 01  DLI-IO-WDGX3102.                                                     
017600*    03  -COPY WDGX3102                                                   
017700     EJECT                                                                
017800 01  FILLER                   PIC X(16) VALUE 'DLI-IO-WDK601'.            
017900 01  DLI-IO-WDK601.                                                       
018000*    03  -COPY WDK601                                                     
018100     EJECT                                                                
018200 01  FILLER                   PIC X(16) VALUE 'DLI-IO-WDK611'.            
018300 01  DLI-IO-WDK611.                                                       
018400*    03  -COPY WDK611                                                     
018500     EJECT                                                                
018600 01  FILLER                   PIC X(16) VALUE 'DLI-IO-WDK621'.            
018700 01  DLI-IO-WDK621.                                                       
018800*    03  -COPY WDK621                                                     
018900     EJECT                                                                
019000 01  FILLER                   PIC X(16) VALUE 'DLI-IO-WDF101'.            
019100 01  DLI-IO-WDF101.                                                       
019200*    03   -COPY WDF101                                                    
019300     EJECT                                                                
019400 01  FILLER                   PIC X(16) VALUE 'DLI-IO-WDF102'.            
019500 01  DLI-IO-WDF102.                                                       
019600    03   -COPY WDF102                                                     
019700     EJECT                                                                
019800 01  FILLER                   PIC X(16) VALUE 'DLI-IO-WDK701'.            
019900 01  DLI-IO-WDK701.                                                       
020000*    03   -COPY WDK701                                                    
020100     EJECT                                                                
020200 01  FILLER                   PIC X(16) VALUE 'DLI-IO-WDK711'.            
020300 01  DLI-IO-WDK711.                                                       
020400*    03   -COPY WDK711                                                    
020500     EJECT                                                                
020600 01  FILLER               PIC X(16)   VALUE 'WDB201-AREA'.                
020700 01  DLI-IO-WDB201.                                                       
020800*    03  -COPY WDB201                                                     
020900     EJECT                                                                
021000*    ---  WZ01 INPUT-OUTPUT AREA                                          
021100     EJECT                                                                
021200 01  FILLER                PIC X(16)   VALUE 'CALL-CONTROL'.              
021300     SKIP3                                                                
021400 01  -COPY WZ01CALL                                                       
021500     EJECT                                                                
021600 01  FILLER                PIC X(16)   VALUE 'CALL-REQU-AREA'.            
021700     SKIP3                                                                
021800 01  CALL-REQU-AREA.                                                      
021900*    03  -COPY WZ01REQU                                                   
022000*    03  -COPY W30305O2                                                   
022100     EJECT                                                                
022200 01  FILLER                PIC X(16)   VALUE 'CALL-RESP-AREA'.            
022300     SKIP3                                                                
022400 01  CALL-RESP-AREA.                                                      
022500*    03  -COPY WZ01RESP                                                   
022600*    03  -COPY W30305I2                                                   
022700     EJECT                                                                
022800 LINKAGE SECTION.                                                         
022900*01  -COPY W0009   -PRE MSG-                                              
023000*01  -COPY W0008   -PRE WDP7-                                             
023100     05  FILLER                  PIC X.                                   
023200*01  -COPY W0008   -PRE WDR4-                                             
023300     05  FILLER                  PIC X.                                   
023400*01  -COPY W0008   -PRE WDK6-                                             
023500     05  FILLER                  PIC X.                                   
023600*01  -COPY W0008   -PRE WDK7-                                             
023700     05  FILLER                  PIC X.                                   
023800*01  -COPY W0008   -PRE WDF1-                                             
023900     05  FILLER                  PIC X.                                   
024000*01  -COPY W0008   -PRE 9305-                                             
024100     05  FILLER                  PIC X.                                   
024200*01  -COPY W0008   -PRE WDB2-                                             
024300   05 FILLER                     PIC X.                                   
024400     EJECT                                                                
024500 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDR4-PCB WDK6-PCB             
024600                                   WDK7-PCB WDF1-PCB 9305-PCB             
024700                                   WDB2-PCB.                              
024800 MAIN SECTION.                                                            
024900     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDR4-PCB WDK6-PCB             
025000                                   WDK7-PCB WDF1-PCB 9305-PCB             
025100                                   WDB2-PCB.                              
025200                                                                          
025300     PERFORM IMS-GET-MSG                                                  
025400     IF SEGMENT-FINNS                                                     
025500       PERFORM A-INIT                                                     
025600       PERFORM B-KOLLA-NYCKLAR                                            
025700       IF NYCKLAR-OK                                                      
025800         PERFORM F-HAEMTA-VISA-INFO                                       
025900       END-IF                                                             
026000       COMPUTE MSG-KVLL = LENGTH OF MOD-W30305O1 + 4                      
026100       PERFORM IMS-INSERT-MSG                                             
026200     END-IF                                                               
026300                                                                          
026400     MOVE ZERO TO RETURN-CODE                                             
026500     GOBACK                                                               
026600     .                                                                    
026700     EJECT                                                                
026800 A-INIT SECTION.                                                          
026900                                                                          
027000     MOVE ZERO TO W-WDB2-KDKUNDKAT                                        
027100     IF MSG-DUBBLA-TRANSKODER                                             
027200       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W30305I1                 
027300       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
027400       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
027500     ELSE                                                                 
027600       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W30305I1                  
027700       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
027800       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
027900     END-IF                                                               
028000                                                                          
028100     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
028200     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
028300     MOVE MFS-IDTRANS TO W-IDTRANS                                        
028400                                                                          
028500     MOVE LOW-VALUE TO MSG-AREA                                           
028600     MOVE 'W3O305N1' TO MFS-IDMOD                                         
028700     MOVE '3305' TO MOD-IDTRANS                                           
028800     MOVE 'GB' TO MED-IDSKYLT                                             
028900     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
029000                                                                          
029100     MOVE FUNCTION CURRENT-DATE(3:6) TO  DAGENS-DATUM                     
029200     MOVE FUNCTION CURRENT-DATE(3:2) TO  W-DATE-AAMM(1:2)                 
029300     MOVE FUNCTION CURRENT-DATE(5:2) TO  W-DATE-AAMM(3:2)                 
029400                                                                          
029500     MOVE W-DATE-AAMM                TO CURR-TIAAMM                       
029600     MOVE WS-KDVALISO-HUV            TO CURR-KDVALISO-HUV                 
029700     MOVE 'M'                        TO CURR-KDVALTYP                     
029800                                                                          
029900     IF EGEN-MID OR HELP-MID                                              
030000       CONTINUE                                                           
030100     ELSE                                                                 
030200       MOVE SPACE TO MFS-KDTRTYP                                          
030300       MOVE '7' TO MFS-IDPFK                                              
030400     END-IF                                                               
030500     .                                                                    
030600     EJECT                                                                
030700 B-KOLLA-NYCKLAR SECTION.                                                 
030800                                                                          
030900     MOVE ALL '+'           TO MSGI-WMSGINIT                              
031000     MOVE '001'             TO MSGI-KDCALL                                
031100     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
031200     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
031300     MOVE '3305'            TO MSGI-IDTRANS                               
031400     IF EGEN-MID                                                          
031500       IF MID-IDARTNR-IN NOT = ALL '+'                                    
031600         MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                             
031700       END-IF                                                             
031800       IF MID-IDDISTR-IN NOT = ALL '+'                                    
031900         MOVE MID-IDDISTR-IN TO MSGI-IDDISTR                              
032000       END-IF                                                             
032100       IF MID-IDKUNDNR-IN NOT = ALL '+'                                   
032200         MOVE MID-IDKUNDNR-IN TO MSGI-IDKUNDNR                            
032300       END-IF                                                             
032400       IF MID-KDORDKL-IN NOT = ALL '+'                                    
032500         MOVE MID-KDORDKL-IN TO MSGI-KDORDKL                              
032600       END-IF                                                             
032700       IF MID-KVBEART-IN NOT = ALL '+'                                    
032800         MOVE MID-KVBEART-IN TO MSGI-KVBEART                              
032900       END-IF                                                             
033000     END-IF                                                               
033100                                                                          
033200     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
033300     MOVE MSGI-SPAR-AREA      TO SPAR-AREA                                
033400                                                                          
033500     MOVE JA                  TO                                          
033600                                 NYCKLAR-SW                               
033700                                                                          
033800*    -- KONTROLL AV IDARTNR                                               
033900     MOVE MFS-RENSA-FAELT     TO MOD-IDARTNR-IN                           
034000                                                                          
034100     IF MSGI-IDARTNR NOT = ALL '+'                                        
034200       INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO               
034300       IF MSGI-IDARTNR NUMERIC                                            
034400         MOVE MSGI-IDARTNR TO MOD-IDARTNR                                 
034500                              MOD-IDARTNR-UT                              
034600       ELSE                                                               
034700         MOVE NEJ             TO NYCKLAR-SW                               
034800       END-IF                                                             
034900     ELSE                                                                 
035000       MOVE NEJ               TO NYCKLAR-SW                               
035100     END-IF                                                               
035200*    -- KONTROLL AV IDDISTR                                               
035300     MOVE MFS-RENSA-FAELT     TO MOD-IDDISTR-IN                           
035400                                                                          
035500     IF MSGI-IDDISTR NOT = ALL '+'                                        
035600       INSPECT MSGI-IDDISTR REPLACING LEADING SPACE BY ZERO               
035700       IF MSGI-IDDISTR NUMERIC                                            
035800         MOVE MSGI-IDDISTR TO MOD-IDDISTR-UT                              
035900                              MOD-IDDISTR                                 
036000                              W-IDDISTR                                   
036100                              W-WDB2-IDDISTR                              
036200       ELSE                                                               
036300         MOVE NEJ             TO NYCKLAR-SW                               
036400                                                                          
036500       END-IF                                                             
036600     ELSE                                                                 
036700       MOVE NEJ               TO NYCKLAR-SW                               
036800     END-IF                                                               
036900* KONTROLL ATT DET ÄR EN AKTIV DDI VIPS DISTRIKT.                         
037000     IF NYCKLAR-OK                                                        
037100       PERFORM IMS-GN-WDGX3102                                            
037200       IF SEGMENT-SAKNAS                                                  
037300         MOVE NEJ   TO NYCKLAR-SW                                         
037400       END-IF                                                             
037500     END-IF                                                               
037600*    -- KONTROLL AV IDKUNDNR                                              
037700     MOVE MFS-RENSA-FAELT     TO MOD-IDKUNDNR-IN                          
037800                                                                          
037900     IF MSGI-IDKUNDNR NOT = ALL '+'                                       
038000       INSPECT MSGI-IDKUNDNR REPLACING LEADING SPACE BY ZERO              
038100       IF MSGI-IDKUNDNR NUMERIC                                           
038200         MOVE MSGI-IDKUNDNR TO MOD-IDKUNDNR-UT                            
038300                               MOD-IDKUNDNR                               
038400                               W-WDB2-IDKUNDNR                            
038500       ELSE                                                               
038600         MOVE NEJ             TO NYCKLAR-SW                               
038700                                                                          
038800       END-IF                                                             
038900     ELSE                                                                 
039000       MOVE NEJ               TO NYCKLAR-SW                               
039100     END-IF                                                               
039200*    -- KONTROLL AV KDORDKL                                               
039300     IF MSGI-KDORDKL NOT = ALL '+'                                        
039400       IF MSGI-KDORDKL NUMERIC                                            
039500        MOVE MFS-RENSA-FAELT     TO MOD-KDORDKL-IN                        
039600       END-IF                                                             
039700       IF MSGI-KDORDKL     = '0' OR '1' OR '2' OR '3' OR '4'              
039800           MOVE MSGI-KDORDKL    TO MOD-KDORDKL-UT                         
039900                                   MOD-KDORDKL                            
040000       ELSE                                                               
040100         MOVE '3'               TO MOD-KDORDKL-UT                         
040200                                 MOD-KDORDKL                              
040300       END-IF                                                             
040400     END-IF                                                               
040500*    -- KONTROLL AV KVBEART-IN                                            
040600     IF MSGI-KVBEART   NOT = ALL '+'                                      
040700       INSPECT MSGI-KVBEART   REPLACING LEADING SPACE BY ZERO             
040800       IF MSGI-KVBEART   NUMERIC                                          
040900        MOVE MFS-RENSA-FAELT     TO MOD-KVBEART-IN                        
041000       END-IF                                                             
041100                                                                          
041200       IF MSGI-KVBEART   NUMERIC                                          
041300         IF MSGI-KVBEART   > 0                                            
041400           MOVE MSGI-KVBEART    TO MOD-KVBEART-UT                         
041500                                 MOD-KVBEART                              
041600          ELSE                                                            
041700            MOVE 1               TO MOD-KVBEART-UT                        
041800                                 MOD-KVBEART                              
041900          END-IF                                                          
042000         ELSE                                                             
042100          MOVE 1                 TO MOD-KVBEART-UT                        
042200                                 MOD-KVBEART                              
042300         END-IF                                                           
042400       END-IF                                                             
042500*                                                                         
042600     IF NYCKLAR-FEL                                                       
042700       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
042800       CALL WMEDKONV USING MED-WMEDAREA                                   
042900       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
043000       PERFORM MFS-RENSA-FAELT-IN                                         
043100       PERFORM MFS-RENSA-FAELT-UT                                         
043200     END-IF                                                               
043300     .                                                                    
043400     EJECT                                                                
043500 F-HAEMTA-VISA-INFO SECTION.                                              
043600                                                                          
043700     MOVE MSGI-IDARTNR            TO  MOD-IDARTNR                         
043800     MOVE MSGI-IDDISTR            TO  MOD-IDDISTR                         
043900     MOVE MSGI-IDKUNDNR           TO  MOD-IDKUNDNR                        
044000                                                                          
044100     PERFORM S05-CALL-SCPRICING                                           
044200     IF CALL-KDRC > 0                                                     
044300       MOVE 'NO CONTACT WITH VIPS' TO MOD-TEMFSFEL                        
044400       PERFORM MFS-RENSA-FAELT-UT                                         
044500     ELSE                                                                 
044600       MOVE    MID-BEART-VIPS    TO MOD-BEART-VIPS                        
044700       MOVE    MID-KDVALISO      TO MOD-KDVALISO                          
044800       MOVE    MID-PRARTBTO-LOC  TO MOD-PRARTBTO-LOC                      
044900       MOVE    MID-PRARTNTO-LOC  TO MOD-PRARTNTO-LOC                      
045000       MOVE    MID-KDRAB         TO MOD-KDRAB                             
045100                                                                          
045200       MOVE    MID-KDVALISO      TO WS-KDVALISO                           
045300       MOVE    MID-PRARTNTO-LOC  TO WS-PRARTNTO-LOC                       
045400                                                                          
045500       PERFORM FA-LAS-ARTIKELREGISTER                                     
045600     END-IF                                                               
045700                                                                          
045800*  ---  TO CLEAN UP ANY REMAINING CONNECTIONS                             
045900     MOVE SPACES                 TO CALL-ADDISPABS                        
046000                                                                          
046100     CALL WZ01CALL USING CALL-CONTROL-AREA                                
046200                                                                          
046300     .                                                                    
046400     EJECT                                                                
046500 FA-LAS-ARTIKELREGISTER SECTION.                                          
046600                                                                          
046700     MOVE MSGI-IDARTNR            TO W-IDARTNR                            
046800     MOVE ZERO                    TO WS-PRARTBES-PR                       
046900                                     WS-PRARTBES-PR-LOC                   
047000                                     WS-PRARTSJK                          
047100                                     WS-PRARTSJK-LOC                      
047200                                     W-PRARTSJK-LOC                       
047300                                     WS-PRARTSJK-MONLOC                   
047400                                                                          
047500     PERFORM IMS-GU-WDK601                                                
047600                                                                          
047700     IF SEGMENT-FINNS                                                     
047800                                                                          
047900       MOVE ART-IDLEVNR           TO  W-WDF101-IDLEVNR                    
048000       MOVE ART-KDPRODSL          TO TEST-KDPRODSL                        
048100                                                                          
048200       IF ART-KDERS-UTG = 0                                               
048300                                                                          
048400         PERFORM IMS-GNP-WDK611                                           
048500                                                                          
048600         MOVE CLAG-PRDIRLON        TO SPAR-PRDIRLON                       
048700         MOVE CLAG-PRDMTRL         TO SPAR-PRDMTRL                        
048800         MOVE CLAG-PROVRPAL        TO SPAR-PROVRPAL                       
048900                                                                          
049000         MOVE NEJ TO FLSLUTA-LAS                                          
049100                                                                          
049200         PERFORM UNTIL SEGMENT-SAKNAS OR FLSLUTA-LAS = JA                 
049300                                                                          
049400           PERFORM IMS-GNP-WDK621                                         
049500           IF SEGMENT-FINNS AND PRL-KDSTATUS-PR = 1 AND                   
049600                PRL-SUINLEV-PR > 0                                        
049700             MOVE PRL-PRARTBEL-PR    TO SPAR-PRARTBEL-PR                  
049800                                        WS-PRARTNTO-LOC                   
049900             MOVE PRL-IDLEVNR        TO W-WDF101-IDLEVNR                  
050000             MOVE PRL-KDVALISO       TO W-WDK621-KDVALISO                 
050100                                        WS-KDVALISO                       
050200             PERFORM FAA-BEHANDLA-LEVERANTOR-KURS                         
050300             IF PRL-FLHUVLEV = JA                                         
050400               PERFORM FAB-BERAKNA-PRARTBES-HUVLEV                        
050500             ELSE                                                         
050600               PERFORM FAC-BERAKNA-PRARTBES-EJ-HUVLEV                     
050700             END-IF                                                       
050800             PERFORM FAD-BERAKNA-PRARTSJK                                 
050900             PERFORM FAE-BERAKNA-PRARTSJK-LOC                             
051000             PERFORM FAF-BERAKNA-BRUTTOVINST                              
051100             MOVE JA TO FLSLUTA-LAS                                       
051200           END-IF                                                         
051300         END-PERFORM                                                      
051400         IF W-PRARTSJK-LOC > 0                                            
051500           MOVE W-PRARTSJK-LOC       TO MOD-PRARTSJK-MONLOC               
051600         ELSE                                                             
051700           MOVE +0                   TO MOD-PRARTSJK-MONLOC               
051800         END-IF                                                           
051900       ELSE                                                               
052000         MOVE PART-SUPERSEDED TO MED-IDMFSFEL                             
052100         CALL WMEDKONV USING MED-WMEDAREA                                 
052200         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
052300         PERFORM MFS-RENSA-FAELT-IN                                       
052400         PERFORM MFS-RENSA-FAELT-UT                                       
052500       END-IF                                                             
052600     ELSE                                                                 
052700       MOVE PART-MISSING TO MED-IDMFSFEL                                  
052800       CALL WMEDKONV USING MED-WMEDAREA                                   
052900       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
053000       PERFORM MFS-RENSA-FAELT-IN                                         
053100       PERFORM MFS-RENSA-FAELT-UT                                         
053200     END-IF                                                               
053300     .                                                                    
053400     EJECT                                                                
053500 FAA-BEHANDLA-LEVERANTOR-KURS SECTION.                                    
053600                                                                          
053700     PERFORM IMS-GU-WDF101                                                
053800     IF SEGMENT-FINNS                                                     
053900       MOVE 'SE' TO W-WDF102-IDLAND                                       
054000       PERFORM IMS-GNP-WDF102                                             
054100       IF SEGMENT-FINNS                                                   
054200         IF TULL-TITULF < DAGENS-DATUM                                    
054300           MOVE TULL-RETULF-1   TO WS-RETULF                              
054400         ELSE                                                             
054500           MOVE TULL-RETULF-2   TO WS-RETULF                              
054600         END-IF                                                           
054700**** HÄMTA 9305-PRKURS                                                    
054800         MOVE PRL-KDVALISO      TO CURR-KDVALISO-ROW                      
054900         CALL W510CURR USING CURR-W510CURR 9305-PCB                       
055000         IF CURR-KDSVAR = ' '                                             
055100           CONTINUE                                                       
055200         ELSE                                                             
055300           MOVE 1               TO CURR-PRKURS-NEW                        
055400         END-IF                                                           
055500       END-IF                                                             
055600     END-IF                                                               
055700     .                                                                    
055800     EJECT                                                                
055900 FAB-BERAKNA-PRARTBES-HUVLEV SECTION.                                     
056000                                                                          
056100     COMPUTE WS-PRARTBES-PR  =                                            
056200      (SPAR-PRARTBEL-PR * WS-RETULF * CURR-PRKURS-NEW)                    
056300     .                                                                    
056400     EJECT                                                                
056500 FAC-BERAKNA-PRARTBES-EJ-HUVLEV SECTION.                                  
056600                                                                          
056700     COMPUTE WS-PRARTBES-PR  =                                            
056800      (SPAR-PRARTBEL-PR * CURR-PRKURS-NEW)                                
056900     .                                                                    
057000     EJECT                                                                
057100 FAD-BERAKNA-PRARTSJK SECTION.                                            
057200                                                                          
057300     COMPUTE WS-PRARTSJK  = WS-PRARTBES-PR +                              
057400             SPAR-PRDIRLON + SPAR-PRDMTRL + SPAR-PROVRPAL                 
057500     .                                                                    
057600     EJECT                                                                
057700 FAE-BERAKNA-PRARTSJK-LOC SECTION.                                        
057800                                                                          
057900     MOVE WS-KDVALISO        TO CURR-KDVALISO-ROW                         
058000                                                                          
058100     CALL W510CURR USING CURR-W510CURR 9305-PCB                           
058200     IF CURR-KDSVAR = ' '                                                 
058300       CONTINUE                                                           
058400     ELSE                                                                 
058500       MOVE 1                TO CURR-PRKURS-NEW                           
058600     END-IF                                                               
058700     COMPUTE WS-PRARTSJK-LOC  = WS-PRARTSJK / CURR-PRKURS-NEW             
058800     COMPUTE W-PRARTSJK-LOC ROUNDED  = WS-PRARTSJK-LOC                    
058900                                                                          
059000     .                                                                    
059100     EJECT                                                                
059200 FAF-BERAKNA-BRUTTOVINST SECTION.                                         
059300                                                                          
059400     IF WS-PRARTNTO-LOC > 0                                               
059500       COMPUTE WS-REBVSPPR ROUNDED = (WS-PRARTNTO-LOC -                   
059600                 WS-PRARTSJK-LOC) / WS-PRARTNTO-LOC * 100                 
059700                                                                          
059800       MOVE WS-REBVSPPR        TO MOD-REBVSPPR                            
059900     ELSE                                                                 
060000       MOVE ZERO              TO MOD-REBVSPPR                             
060100     END-IF                                                               
060200                                                                          
060300     .                                                                    
060400     EJECT                                                                
060500* --- KOMMUNIKATIONS SEKTIONER -----                                      
060600 S05-CALL-SCPRICING SECTION.                                              
060700                                                                          
060800     MOVE 3102-ADDISPABS-SYNC        TO CALL-ADDISPABS                    
060900     MOVE LENGTH OF CALL-REQU-AREA   TO CALL-KVDLEN-IN                    
061000     MOVE LENGTH OF CALL-RESP-AREA   TO CALL-KVDLEN-OUT                   
061100                                                                          
061200     CALL WZ01CALL USING CALL-CONTROL-AREA                                
061300                         CALL-KVDLEN-IN  CALL-REQU-AREA                   
061400                         CALL-KVDLEN-OUT CALL-RESP-AREA                   
061500     .                                                                    
061600     EJECT                                                                
061700 MFS-RENSA-FAELT-UT SECTION.                                              
061800                                                                          
061900*    --- ALLA UTDATA-FÄLT                                                 
062000     MOVE MFS-RENSA-FAELT TO MOD-BEART-VIPS                               
062100                             MOD-KDVALISO                                 
062200                             MOD-PRARTBTO-LOC                             
062300                             MOD-PRARTNTO-LOC                             
062400                             MOD-KDRAB                                    
062500                             MOD-PRARTSJK-MONLOC                          
062600                             MOD-REBVSPPR                                 
062700                             MOD-TEMFSINF                                 
062800     .                                                                    
062900     SKIP3                                                                
063000 MFS-RENSA-FAELT-IN SECTION.                                              
063100                                                                          
063200*    --- ALLA INDATA-FÄLT                                                 
063300     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
063400                             MOD-IDDISTR-IN                               
063500                             MOD-IDKUNDNR-IN                              
063600                             MOD-KDORDKL-IN                               
063700                             MOD-KVBEART-IN                               
063800     .                                                                    
063900     EJECT                                                                
064000     SKIP3                                                                
064100 IMS-GET-MSG SECTION.                                                     
064200                                                                          
064300     MOVE '  QC' TO GODK-STATUSKODER                                      
064400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
064500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
064600     PERFORM IMS-STATUSKONTROLL                                           
064700     .                                                                    
064800     SKIP3                                                                
064900 IMS-INSERT-MSG SECTION.                                                  
065000                                                                          
065100     IF MSGI-IDLAND-SPR = 'SE'                                            
065200       MOVE 'N' TO MFS-KDHUVOMR                                           
065300     END-IF                                                               
065400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
065500     MOVE SPACE TO GODK-STATUSKODER                                       
065600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
065700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
065800     PERFORM IMS-STATUSKONTROLL                                           
065900     .                                                                    
066000     EJECT                                                                
066100 IMS-GN-WDGX3102  SECTION.                                                
066200                                                                          
066300     STRING 'WDR401  (WDGXKEY  =' W-IDHTYP-X ')'                          
066400          DELIMITED BY SIZE INTO SSA1                                     
066500     MOVE 'WDGX3102 ' TO         SSA2                                     
066600     MOVE '  GE' TO GODK-STATUSKODER                                      
066700     CALL CBLTDLI USING GN  WDR4-PCB DLI-IO-WDGX3102                      
066800                                          SSA1 SSA2                       
066900     MOVE WDR4-STATUS-CODE TO STATUS-WS                                   
067000     PERFORM IMS-STATUSKONTROLL                                           
067100     .                                                                    
067200     SKIP3                                                                
067300 IMS-GU-WDK601 SECTION.                                                   
067400                                                                          
067500     STRING 'WDK601  (IDARTNR  =' W-WDK6-IDARTNR-X ')'                    
067600             DELIMITED BY SIZE INTO SSA1                                  
067700     MOVE '  GE' TO GODK-STATUSKODER                                      
067800     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
067900     MOVE WDK6-STATUS-CODE  TO STATUS-WS                                  
068000     PERFORM IMS-STATUSKONTROLL                                           
068100     .                                                                    
068200     EJECT                                                                
068300 IMS-GNP-WDK611  SECTION.                                                 
068400                                                                          
068500     STRING 'WDK611  (KDSEGKEY =1)'                                       
068600             DELIMITED BY SIZE INTO SSA1                                  
068700     MOVE '  GE' TO GODK-STATUSKODER                                      
068800     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
068900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
069000     PERFORM IMS-STATUSKONTROLL                                           
069100     .                                                                    
069200     SKIP2                                                                
069300 IMS-GNP-WDK621  SECTION.                                                 
069400     MOVE 'WDK621   ' TO SSA1                                             
069500     MOVE '  GE' TO GODK-STATUSKODER                                      
069600     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK621 SSA1                   
069700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
069800     PERFORM IMS-STATUSKONTROLL                                           
069900     .                                                                    
070000     SKIP2                                                                
070100 IMS-GU-WDF101 SECTION.                                                   
070200                                                                          
070300     STRING 'WDF101  (IDLEVNR  =' W-WDF101-IDLEVNR-X ')'                  
070400     DELIMITED BY SIZE INTO SSA1                                          
070500     MOVE '  GE' TO GODK-STATUSKODER                                      
070600     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF101 SSA1                    
070700     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
070800     PERFORM IMS-STATUSKONTROLL                                           
070900     .                                                                    
071000     SKIP3                                                                
071100 IMS-GNP-WDF102 SECTION.                                                  
071200                                                                          
071300     STRING 'WDF102  (IDLAND   =' W-WDF102-IDLAND-X ')'                   
071400     DELIMITED BY SIZE INTO SSA1                                          
071500     MOVE '  GE' TO GODK-STATUSKODER                                      
071600     CALL CBLTDLI USING GNP WDF1-PCB DLI-IO-WDF102 SSA1                   
071700     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
071800     PERFORM IMS-STATUSKONTROLL                                           
071900     .                                                                    
072000     SKIP2                                                                
072100 IMS-GU-WDB201 SECTION.                                                   
072200                                                                          
072300     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
072400          DELIMITED BY SIZE INTO SSA1                                     
072500     MOVE '  GE'              TO GODK-STATUSKODER                         
072600     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
072700     MOVE WDB2-STATUS-CODE    TO STATUS-WS                                
072800     PERFORM IMS-STATUSKONTROLL                                           
072900     .                                                                    
073000 IMS-STATUSKONTROLL SECTION.                                              
073100                                                                          
073200     SET STATUS-IX TO 1                                                   
073300     SEARCH GODK-STATUS                                                   
073400       AT END                                                             
073500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
073600         DELIMITED BY SIZE INTO FELTEXT                                   
073700         CALL FELLOG                                                      
073800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
073900         CONTINUE                                                         
074000     END-SEARCH                                                           
074100     .                                                                    
