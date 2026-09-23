000100**********************************************************                
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W5020100.                                                
000400 AUTHOR.         GUN LÖFGREN.                                             
000500 DATE-WRITTEN.   NOVEMBER 1996.                                           
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        PRICE AND VALUE ADJUSTMENT                                       
001000*                                                                         
001100*    INDATA.                                                              
001200*        TRANSAKTION: W5T201                                              
001300*        MID:         W5I20101                                            
001400*                                                                         
001500*    UTDATA.                                                              
001600*        MOD:         W5O20101                                            
001700                                                                          
001800     EJECT                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP3                                                                
002100 DATA DIVISION.                                                           
002200 WORKING-STORAGE SECTION.                                                 
002300                                                                          
002400*    -- CHECKED BY WY2000                                                 
002500                                                                          
002600 77  IDPGM                       PIC X(08)   VALUE 'W5020100'.            
002700                                                                          
002800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
002900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003000                                                                          
003100 77  JA                          PIC X       VALUE 'J'.                   
003200 77  NEJ                         PIC X       VALUE 'N'.                   
003300 77  FLFEL-FAELT                 PIC X       VALUE 'N'.                   
003400                                                                          
003500*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
003600                                                                          
003700                                                                          
003800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
003900     88  NYCKLAR-OK                          VALUE 'J'.                   
004000     88  NYCKLAR-FEL                         VALUE 'N'.                   
004100                                                                          
004200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
004300     88  EGEN-MID                            VALUE '5201'.                
004400     88  GODK-MID                            VALUE '5201'.                
004500                                                                          
004600 77  W-UPPDAT                    PIC X(2)    VALUE SPACE.                 
004700     88  W-PR-UPPDAT                         VALUE 'PR'.                  
004800     88  W-SU-UPPDAT                         VALUE 'SU'.                  
004900     88  W-PM-UPPDAT                         VALUE 'PM'.                  
004910 77  W-UPPDATE-TOTAL             PIC X(2)    VALUE 'JA'.                  
005000     EJECT                                                                
005100 01  DIVERSE.                                                             
005200     03  DAGENS-DAT.                                                      
005300      05 DAGENS-SEKEL            PIC 9(2).                                
005400      05 DAGENS-DATUM            PIC 9(6).                                
005500      05 FILLER REDEFINES DAGENS-DATUM.                                   
005600       07 DAGENS-AAR             PIC 9(2).                                
005700       07 FILLER                 PIC 9(4).                                
005800     03  LOKAL-DAT.                                                       
005900      05 LOKAL-SEKEL             PIC 9(2).                                
006000      05 LOKAL-DATUM             PIC 9(6).                                
006100      05 FILLER REDEFINES LOKAL-DATUM.                                    
006200       07 LOKAL-AAR              PIC 9(2).                                
006300       07 FILLER                 PIC 9(4).                                
006400     03  DAGENS-TID              PIC 9(8)    VALUE ZERO.                  
006500     03  WS-IDUSER.                                                       
006600      05 WS-IDU1                 PIC X(5)    VALUE 'WIDDC'.               
006700      05 WS-IDLAGER              PIC X(2)    VALUE SPACE.                 
006800     03  SPAR-IDUSER             PIC X(8)    VALUE SPACE.                 
006900     03  WS-KVDISP               PIC S9(7)   VALUE ZERO.                  
007000     03  WS-SULAGVDE             PIC S9(9)V9(2)                           
007100                                             VALUE ZERO.                  
007200     03  WS-KDAVCOST             PIC X(2)    VALUE SPACE.                 
007300     03  WS-IDARTNR              PIC X(9)    VALUE SPACE.                 
007400     03  WS-SEKTION              PIC X(40)   VALUE SPACE.                 
007500     03  WS-IDSEKVNR             PIC S9(3) COMP-3 VALUE ZERO.             
007600     03  WS-BYTES                PIC X       VALUE 'N'.                   
007700     03  WS-PRAVCOST             PIC 9(7)V9(2) VALUE ZERO.                
007800     03  WS-PRAVCOST-OLD         PIC 9(7)V9(2) VALUE ZERO.                
007900     03  WS-PRMATRL              PIC 9(7)V9(2) VALUE ZERO.                
008000     03  WS-PRMATRL-OLD          PIC 9(7)V9(2) VALUE ZERO.                
008100     03  WS-KVAKS-PAV            PIC S9(7)   VALUE ZERO.                  
008200     03  WS-KVAKS-SDC            PIC S9(7)   VALUE ZERO.                  
008300     03  WS-KVLS                 PIC S9(7)   VALUE ZERO.                  
008400     03  WS-KVEFRS               PIC S9(7)   VALUE ZERO.                  
008500     03  WS-KDRC                 PIC 9(5)   VALUE ZERO.                   
008600     03  WS-DAGENS-DAT-FMT.                                               
008700         05 WS-DAGENS-DAT-CCYY   PIC 9(4)    VALUE ZERO.                  
008800         05 FILLER               PIC X       VALUE '/'.                   
008900         05 WS-DAGENS-DAT-MM     PIC 9(2)    VALUE ZERO.                  
009000         05 FILLER               PIC X       VALUE '/'.                   
009100         05 WS-DAGENS-DAT-DD     PIC 9(2)    VALUE ZERO.                  
009200     03  WS-DAGENS-TID-FMT.                                               
009300         05 WS-DAGENS-TID-HH     PIC 9(2)    VALUE ZERO.                  
009400         05 FILLER               PIC X       VALUE ':'.                   
009500         05 WS-DAGENS-TID-MM     PIC 9(2)    VALUE ZERO.                  
009600         05 FILLER               PIC X       VALUE ':'.                   
009700         05 WS-DAGENS-TID-SS     PIC 9(2)    VALUE ZERO.                  
009800                                                                          
009900 01  DAP-LINE-AREA.                                                       
010000     03  DAP-LINE1-AREA.                                                  
010100         05 DAP-LINE-TEXT-1      PIC X(11).                               
010200         05 DAP-LINE-IDARTNR-1   PIC X(9).                                
010300*                                 PART NO                                 
010400     03  DAP-LINE2-AREA.                                                  
010500         05 DAP-LINE-TEXT-2      PIC X(16).                               
010600         05 DAP-LINE-IDLANDX2-2  PIC X(2).                                
010700*                                 COUNTRY CODE                            
010800     03  DAP-LINE3-AREA.                                                  
010900         05 DAP-LINE-TEXT-3      PIC X(26).                               
011000         05 DAP-LINE-PRMATRL-OLD PIC Z(6)9.9(2).                          
011100*                                 CURRENT MATERIAL PRICE                  
011200     03  DAP-LINE4-AREA.                                                  
011300         05 DAP-LINE-TEXT-4      PIC X(27).                               
011400         05 DAP-LINE-PRMATRL-NEW PIC Z(6)9.9(2).                          
011500*                                 MODIFIED MATERIAL PRICE                 
011600     03  DAP-LINE5-AREA.                                                  
011700         05 DAP-LINE-TEXT-5      PIC X(11).                               
011800         05 DAP-LINE-USER        PIC X(8).                                
011900*                                 USER                                    
012000     03  DAP-LINE6-AREA.                                                  
012100         05 DAP-LINE-TEXT-6      PIC X(8).                                
012200         05 DAP-LINE-DATE        PIC X(10).                               
012300*                                 DATE                                    
012400     03  DAP-LINE7-AREA.                                                  
012500         05 DAP-LINE-TEXT-7      PIC X(8).                                
012600         05 DAP-LINE-SWEDE-TIME  PIC X(8).                                
012700*                                 SWEDISH TIME                            
012800                                                                          
012900 01  WS-ERROR-TEXT.                                                       
013000     03  WS-ERR-PF23-AND-NO-DATA PIC X(44)   VALUE                        
013100                                    'PF23 AND NO INPUT'.                  
013200     03  WS-ERR-UPDATE-KEY       PIC X(44)   VALUE                        
013300                                 'PRESS PF11 OR PF23 TO UPDATE'.          
013400                                                                          
013410     03  WS-ERR-SAME-AVG-COST    PIC X(44)   VALUE                        
013420                                'SAME AS CURRENT AVG COST PRICE'.         
013500*01  FILLER -COPY WWBYT20                                                 
013600                                                                          
013700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
013800 01  GENERELLA-SUBPROGRAM.                                                
013900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
014000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
014100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014300     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
014400     03  W009CIA                 PIC X(8)    VALUE 'W009CIA '.            
014500     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
014600     EJECT                                                                
014700*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
014800*01 -COPY WMEDAREA                                                        
014900     EJECT                                                                
015000 01  MESSAGE-CODES.                                                       
015100     03  ERR-DATA                PIC X(3)    VALUE '001'.                 
015200     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
015300     03  INF-PRESS-PF23          PIC X(3)    VALUE '206'.                 
015400     03  ERR-INFO-MISSING        PIC X(3)    VALUE '005'.                 
015500     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
015600     03  ERR-UPDATE-NOT-ALLOWED  PIC X(3)    VALUE '007'.                 
015700     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
015800     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
015900     03  ERR-NO-UPDATE-DONE      PIC X(3)    VALUE '034'.                 
016000     03  ERR-TO-MANY-FIELDS      PIC X(3)    VALUE '238'.                 
016100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
016200     EJECT                                                                
016300*    --- AREAS FOR COMMUNICATION                                          
016400*                                                                         
016500 01  FILLER                      PIC X(16)  VALUE 'SEND-CONTROL'.         
016600*01  -COPY WZ01SEND                                                       
016700     EJECT                                                                
016800 01  HDR-AREA.                                                            
016900*    03  -COPY WZ01REQU -PRE ERROR-                                       
017000*    03  -COPY WZ04HDR                                                    
017100     EJECT                                                                
017200*    --- VALID IDDC CODES                                                 
017300*                                                                         
017400 01  FILLER                      PIC X(16)   VALUE 'IDDC CODES'.          
017500*01  -COPY WWDCKONS                                                       
017600     EJECT                                                                
017700*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
017800*                                                                         
017900 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
018000     SKIP3                                                                
018100*01 -COPY WMSGINIT                                                        
018200*    --- PARAMETRAR TILL SUBPROGRAM W009CIA                               
018300     SKIP3                                                                
018400 01  FILLER                      PIC X(16)   VALUE 'W009CIA '.            
018500*01 -COPY W009CIA                                                         
018600     EJECT                                                                
018700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
018800*                                                                         
018900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
019000     SKIP3                                                                
019100*01  MID -COPY W5I20101                                                   
019200     EJECT                                                                
019300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
019400     SKIP3                                                                
019500*01  -COPY WMSGAREA                                                       
019600     EJECT                                                                
019700     03  MOD REDEFINES MSG-AREA.                                          
019800*      05  -COPY W5O20101                                                 
019900     EJECT                                                                
020000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
020100     SKIP3                                                                
020200*01  -COPY WMFSAREA                                                       
020300     EJECT                                                                
020400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
020500*                                                                         
020600*                                                                         
020700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
020800     SKIP3                                                                
020900 01  NYCKLAR-TILL-DLI.                                                    
021000     03  W-IDARTNR-X.                                                     
021100         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
021200     03  W-IDDC-X.                                                        
021300         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
021400     03  W-KDSEGKEY-X.                                                    
021500         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
021600     03  W-IDARTNR-WDD3-X.                                                
021700         05  W-IDARTNR-WDD3      PIC S9(9)   VALUE ZERO COMP-3.           
021800     03  W-IDSKYLT-X.                                                     
021900         05  W-IDSKYLT           PIC X(3)    VALUE 'GB '.                 
022000     03  W-IDDC-B6-X.                                                     
022100         05 W-IDDC-B6            PIC X(2).                                
022200     03  W-IDLANDX2-B6-X.                                                 
022300         05 W-IDLANDX2-B6        PIC X(2).                                
022400*                                                                         
022500     EJECT                                                                
022600*    --- MEDDELANDEN                                                      
022700 01  FILLER                      PIC X(8)    VALUE 'WDECAREA'.            
022800*                                                                         
022900*01  -COPY  WDECAREA.                                                     
023000     EJECT                                                                
023100                                                                          
023200 01  FILLER                      PIC X(8)    VALUE 'A16-AREA'.            
023300*                                                                         
023400*01  -COPY  W510A16  -PRE  A16-.                                          
023500     EJECT                                                                
023600                                                                          
023700 01  FILLER                      PIC X(8)    VALUE 'EKH-AREA'.            
023800*01  -COPY  W510EKHA.                                                     
023900     EJECT                                                                
024000                                                                          
024100*    --- STATUS-KOD FRÅN IMS                                              
024200 01  STATUS-WS                   PIC XX.                                  
024300     88  SEGMENT-FINNS                       VALUE '  '.                  
024400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
024500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
024600     SKIP2                                                                
024700 01  GODK-STATUSKODER.                                                    
024800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
024900     SKIP3                                                                
025000 01  SSA1                        PIC X(75).                               
025100 01  SSA2                        PIC X(75).                               
025200     EJECT                                                                
025300*    --- IMS FUNKTIONSKODER                                               
025400*01  -COPY W0003                                                          
025500     EJECT                                                                
025600*    ---  DLI INPUT-OUTPUT AREA                                           
025700 01  FILLER                      PIC X(16) VALUE 'WDK601 AREA'.           
025800 01  DLI-IO-WDK601.                                                       
025900*    03  -COPY WDK601                                                     
026000     EJECT                                                                
026100 01  FILLER                      PIC X(16) VALUE 'WDK611 AREA'.           
026200 01  DLI-IO-WDK611.                                                       
026300*    03  -COPY WDK611                                                     
026400     EJECT                                                                
026500 01  FILLER                      PIC X(16) VALUE 'WDD311 AREA'.           
026600 01  DLI-IO-WDD311.                                                       
026700*    03  -COPY WDD311                                                     
026800     EJECT                                                                
026900 01  FILLER                      PIC X(16) VALUE 'WDK701 AREA'.           
027000 01  DLI-IO-WDK701.                                                       
027100*    03  -COPY WDK701                                                     
027200     EJECT                                                                
027300 01  FILLER                      PIC X(16) VALUE 'WDK711 AREA'.           
027400 01  DLI-IO-WDK711.                                                       
027500*    03  -COPY WDK711                                                     
027600     EJECT                                                                
027700 01  FILLER                      PIC X(16) VALUE 'WDK712 AREA'.           
027800 01  DLI-IO-WDK712.                                                       
027900*    03  -COPY WDK712                                                     
028000     EJECT                                                                
028100 01  FILLER                      PIC X(16) VALUE 'WDR801 AREA'.           
028200 01  DLI-IO-WDR801.                                                       
028300*    03  -COPY WDR801                                                     
028400                                                                          
028500 01  FILLER                      PIC X(16) VALUE 'WDB601 AREA'.           
028600 01   DLI-IO-AREA-B601.                                                   
028700*     03  -COPY WDB601                                                    
028800                                                                          
028900     EJECT                                                                
029000 LINKAGE SECTION.                                                         
029100                                                                          
029200*01  -COPY W0009  -PRE MSG-                                               
029300     05  FILLER                  PIC X.                                   
029400     EJECT                                                                
029500*01  -COPY W0009  -PRE DISTRDOC-                                          
029600     05  FILLER                  PIC X.                                   
029700     EJECT                                                                
029800*01  -COPY W0008  -PRE WDP7-                                              
029900     05  FILLER                  PIC X.                                   
030000     EJECT                                                                
030100*01  -COPY W0008  -PRE WDK6-                                              
030200     05  FILLER                  PIC X.                                   
030300     EJECT                                                                
030400*01  -COPY W0008  -PRE WDD3-                                              
030500     05  FILLER                  PIC X.                                   
030600     EJECT                                                                
030700*01  -COPY W0008  -PRE WDK7-                                              
030800     05  FILLER                  PIC X.                                   
030900     EJECT                                                                
031000*01  -COPY W0008  -PRE WDR8-                                              
031100     05  FILLER                  PIC X.                                   
031200     EJECT                                                                
031300*01  -COPY W0008  -PRE WDB6-                                              
031400     05  FILLER                  PIC X.                                   
031500     EJECT                                                                
031600 PROCEDURE DIVISION  USING MSG-PCB DISTRDOC-PCB WDP7-PCB WDK6-PCB         
031700                           WDD3-PCB WDK7-PCB WDR8-PCB WDB6-PCB.           
031800 MAIN SECTION.                                                            
031900     ENTRY 'DLITCBL' USING MSG-PCB DISTRDOC-PCB WDP7-PCB WDK6-PCB         
032000                           WDD3-PCB WDK7-PCB WDR8-PCB WDB6-PCB.           
032100                                                                          
032200     PERFORM IMS-GET-MSG                                                  
032300     IF SEGMENT-FINNS                                                     
032400       PERFORM A-INIT                                                     
032500       PERFORM B-KOLLA-NYCKLAR                                            
032600       IF NYCKLAR-OK                                                      
032700         IF MFS-UPDATE OR MFS-UPD-V                                       
032800           PERFORM C-KONTROLLERA-INDATA                                   
032900           IF FLFEL-FAELT = NEJ                                           
033000             PERFORM G-UPPDATERA-DATA                                     
033600             PERFORM F-LAES-VISA-INFO                                     
033700           END-IF                                                         
033800         ELSE                                                             
033900           PERFORM D-KOLLA-INFAELT                                        
034000           IF FLFEL-FAELT = NEJ                                           
034100             PERFORM F-LAES-VISA-INFO                                     
034200           END-IF                                                         
034300         END-IF                                                           
034400       END-IF                                                             
034500       COMPUTE MSG-KVLL = LENGTH OF MOD-W5O20101 + 4                      
034600       PERFORM IMS-INSERT-MSG                                             
034700     END-IF                                                               
034800                                                                          
034900     MOVE ZERO TO RETURN-CODE                                             
035000     GOBACK                                                               
035100     .                                                                    
035200     EJECT                                                                
035300 A-INIT SECTION.                                                          
035400                                                                          
035500     MOVE 'A-INIT'         TO WS-SEKTION                                  
035600                                                                          
035700     ACCEPT DAGENS-DATUM  FROM DATE                                       
035800     ACCEPT DAGENS-TID    FROM TIME                                       
035900                                                                          
036000     IF DAGENS-AAR < 50                                                   
036100       MOVE 20             TO DAGENS-SEKEL                                
036200     ELSE                                                                 
036300       MOVE 19             TO DAGENS-SEKEL                                
036400     END-IF                                                               
036500                                                                          
036600     MOVE DAGENS-DAT(1:4)  TO WS-DAGENS-DAT-CCYY                          
036700     MOVE DAGENS-DAT(5:2)  TO WS-DAGENS-DAT-MM                            
036800     MOVE DAGENS-DAT(7:2)  TO WS-DAGENS-DAT-DD                            
036900     MOVE DAGENS-TID(1:2)  TO WS-DAGENS-TID-HH                            
037000     MOVE DAGENS-TID(3:2)  TO WS-DAGENS-TID-MM                            
037100     MOVE DAGENS-TID(5:2)  TO WS-DAGENS-TID-SS                            
037200                                                                          
037300     IF MSG-DUBBLA-TRANSKODER                                             
037400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I20101                 
037500       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
037600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
037700     ELSE                                                                 
037800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I20101                  
037900       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
038000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
038100     END-IF                                                               
038200                                                                          
038300     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
038400     IF MSG-IDPFK = 'U'                                                   
038500       MOVE 'U'       TO MFS-KDTRTYP                                      
038600       MOVE SPACE     TO MFS-IDPFK                                        
038700     ELSE                                                                 
038800       IF MSG-IDPFK = 'V'                                                 
038900         MOVE 'V'     TO MFS-KDTRTYP                                      
039000         MOVE SPACE   TO MFS-IDPFK                                        
039100       ELSE                                                               
039200         MOVE MSG-IDPFK   TO MFS-IDPFK                                    
039300       END-IF                                                             
039400     END-IF                                                               
039500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
039600                                                                          
039700     MOVE LOW-VALUE TO MSG-AREA                                           
039800     MOVE 'W5O201N1' TO MFS-IDMOD                                         
039900     MOVE '5201' TO MOD-IDTRANS                                           
040000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
040100                                                                          
040200     IF EGEN-MID                                                          
040300       CONTINUE                                                           
040400     ELSE                                                                 
040500       MOVE SPACE TO MFS-KDTRTYP                                          
040600       MOVE '7' TO MFS-IDPFK                                              
040700     END-IF                                                               
040800                                                                          
040900     MOVE 'GB '      TO W-IDSKYLT                                         
041000                                                                          
041100     .                                                                    
041200     EJECT                                                                
041300 B-KOLLA-NYCKLAR SECTION.                                                 
041400                                                                          
041500     MOVE 'B-KOLLA-NYCKLAR' TO WS-SEKTION                                 
041600                                                                          
041700     MOVE ALL '+'           TO MSGI-WMSGINIT                              
041800     MOVE '001'             TO MSGI-KDCALL                                
041900     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
042000     MOVE MSGI-IDUSER       TO SPAR-IDUSER                                
042100     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
042200     MOVE '5201'            TO MSGI-IDTRANS                               
042300     IF EGEN-MID                                                          
042400       MOVE MID-IDARTNR-IN     TO MSGI-IDARTNR                            
042500     ELSE                                                                 
042600       IF MID-IDARTNR-IN NUMERIC                                          
042700         MOVE MID-IDARTNR-IN   TO MSGI-IDARTNR                            
042800       END-IF                                                             
042900       MOVE MID-IDDC-IN        TO W-IDDC-B6                               
043000       PERFORM IMS-GU-WDB601                                              
043100       IF DCS-KDDC = SPACE OR DCS-DDC                                     
043200         CONTINUE                                                         
043300       ELSE                                                               
043400         MOVE MID-IDDC-IN      TO MID-IDDC-SPAR                           
043500       END-IF                                                             
043600     END-IF                                                               
043700     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
043800                                                                          
043900     MOVE MID-IDDC-SPAR     TO WS-IDLAGER                                 
044000     MOVE MSGI-IDARTNR      TO WS-IDARTNR                                 
044100     INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO                   
044200     MOVE JA TO NYCKLAR-SW                                                
044300                                                                          
044400*    -- KONTROLL AV IDARTNR                                               
044500     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
044600                                                                          
044700     IF MID-IDARTNR-IN NOT = ALL '+'                                      
044800       MOVE '7'         TO MFS-IDPFK                                      
044900       MOVE SPACE       TO MFS-KDTRTYP                                    
045000     END-IF                                                               
045100                                                                          
045200     IF WS-IDARTNR NUMERIC                                                
045300       MOVE WS-IDARTNR TO W-IDARTNR                                       
045400                          W-IDARTNR-WDD3                                  
045500     ELSE                                                                 
045600       MOVE NEJ TO NYCKLAR-SW                                             
045700     END-IF                                                               
045800                                                                          
045900****   KONTROLL AV IDDC                                                   
046000                                                                          
046100     MOVE MFS-RENSA-FAELT     TO MOD-IDDC-UT                              
046200                                                                          
046300     IF MID-IDDC-IN = '++'                                                
046400       MOVE MID-IDDC-SPAR     TO W-IDDC-B6                                
046500     ELSE                                                                 
046600       MOVE MID-IDDC-IN       TO W-IDDC-B6                                
046700     END-IF                                                               
046800     PERFORM IMS-GU-WDB601                                                
046900                                                                          
047000     MOVE DCS-IDLANDX2        TO W-IDLANDX2-B6                            
047100                                                                          
047200     IF  MSGI-IDDC = DCS-IDDC                                             
047210     AND (DCS-LAND-NON-VCC-OWNED                                          
047220     OR   DCS-USA                                                         
047230     OR   DCS-CANADA)                                                     
048100       MOVE DCS-IDDC          TO W-IDDC                                   
048200     ELSE                                                                 
048300       MOVE NEJ               TO NYCKLAR-SW                               
048400     END-IF                                                               
048500                                                                          
048600     IF GODK-MID OR NYCKLAR-OK                                            
048700       MOVE WS-IDARTNR        TO MOD-IDARTNR-UT                           
048800       INSPECT MOD-IDARTNR-UT REPLACING                                   
048900                              LEADING ZERO BY SPACE                       
049000       MOVE DCS-IDDC          TO MOD-IDDC-UT                              
049100     ELSE                                                                 
049200       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
049300     END-IF                                                               
049400                                                                          
049500     IF NYCKLAR-FEL                                                       
049600       MOVE 'GB'          TO MED-IDSKYLT                                  
049700       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
049800       CALL WMEDKONV USING MED-WMEDAREA                                   
049900       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
050000       PERFORM MFS-RENSA-FAELT-IN                                         
050100     END-IF                                                               
050200     .                                                                    
050300     EJECT                                                                
050400                                                                          
050500  C-KONTROLLERA-INDATA  SECTION.                                          
050600                                                                          
050700     MOVE 'C-KONTROLLERA-INDATA' TO WS-SEKTION                            
050800                                                                          
050900     IF MID-PRMATRL-IN  = ALL ' '                                         
051000       MOVE ALL '+'             TO MID-PRMATRL-IN                         
051100     END-IF                                                               
051200     IF MID-PRAVCOST  = ALL ' '                                           
051300       MOVE ALL '+'             TO MID-PRAVCOST                           
051400     END-IF                                                               
051500     IF MID-SULAGVDE  = ALL ' '                                           
051600       MOVE ALL '+'             TO MID-SULAGVDE                           
051700     END-IF                                                               
051800     IF MID-KDAVCOST  = ALL ' '                                           
051900       MOVE ALL '+'             TO MID-KDAVCOST                           
052000     END-IF                                                               
052100                                                                          
052200     IF (MFS-UPDATE)                                                      
052300       IF MID-PRMATRL-IN  NOT = ALL '+'                                   
052400         MOVE 'GB'           TO MED-IDSKYLT                               
052500         MOVE ERR-UPDATE-NOT-ALLOWED TO MED-IDMFSFEL                      
052600         CALL WMEDKONV USING MED-WMEDAREA                                 
052700         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
052800         MOVE JA             TO FLFEL-FAELT                               
052900         PERFORM MFS-ROER-EJ-FAELT-UT                                     
053000         PERFORM MFS-ROER-EJ-FAELT-IN                                     
053100         MOVE MFS-NUM-FAELT-RAETT                                         
053200                             TO MOD-PRAVCOST-IN-ATTR                      
053300                                MOD-SULAGVDE-IN-ATTR                      
053400         IF MID-KDAVCOST = '10' OR '20' OR 'E1' OR 'K1'                   
053500           MOVE MFS-ALFA-FAELT-RAETT                                      
053600                             TO MOD-KDAVCOST-IN-ATTR                      
053700         ELSE                                                             
053800           MOVE MFS-ALFA-FAELT-FEL                                        
053900                             TO MOD-KDAVCOST-IN-ATTR                      
054000         END-IF                                                           
054100         MOVE MFS-ALFA-FAELT-RAETT                                        
054200                             TO MOD-IDFS-IN-ATTR                          
054300         MOVE MFS-NUM-FAELT-FEL TO MOD-PRMATRL-IN-ATTR                    
054400       END-IF                                                             
054500     END-IF                                                               
054600     IF FLFEL-FAELT = NEJ                                                 
054700       IF (MFS-UPD-V)                                                     
054800         IF (MID-PRAVCOST NOT = ALL '+' OR                                
054900             MID-SULAGVDE NOT = ALL '+')                                  
055000           MOVE 'GB'           TO MED-IDSKYLT                             
055100           MOVE ERR-UPDATE-NOT-ALLOWED TO MED-IDMFSFEL                    
055200           CALL WMEDKONV USING MED-WMEDAREA                               
055300           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
055400           MOVE JA             TO FLFEL-FAELT                             
055500           PERFORM MFS-ROER-EJ-FAELT-UT                                   
055600           PERFORM MFS-ROER-EJ-FAELT-IN                                   
055700           IF MID-KDAVCOST = '30'                                         
055800             MOVE MFS-ALFA-FAELT-RAETT                                    
055900                               TO MOD-KDAVCOST-IN-ATTR                    
056000           ELSE                                                           
056100             MOVE MFS-ALFA-FAELT-FEL                                      
056200                               TO MOD-KDAVCOST-IN-ATTR                    
056300           END-IF                                                         
056400           MOVE MFS-ALFA-FAELT-RAETT                                      
056500                               TO MOD-IDFS-IN-ATTR                        
056600           MOVE MFS-NUM-FAELT-RAETT                                       
056700                               TO MOD-PRMATRL-IN-ATTR                     
056800                                  MOD-PRAVCOST-IN-ATTR                    
056900                                  MOD-SULAGVDE-IN-ATTR                    
057000           IF (MID-PRAVCOST NOT = ALL '+' )                               
057100             MOVE MFS-NUM-FAELT-FEL                                       
057200                               TO MOD-PRAVCOST-IN-ATTR                    
057300           END-IF                                                         
057400           IF (MID-SULAGVDE NOT = ALL '+')                                
057500             MOVE MFS-NUM-FAELT-FEL                                       
057600                               TO MOD-SULAGVDE-IN-ATTR                    
057700           END-IF                                                         
057800         END-IF                                                           
057900       END-IF                                                             
058000     END-IF                                                               
058100     IF FLFEL-FAELT = NEJ                                                 
058200       IF (MFS-UPDATE)                                                    
058300         IF (MID-PRAVCOST = ALL '+'  AND                                  
058400             MID-SULAGVDE = ALL '+')                                      
058500           MOVE 'GB'           TO MED-IDSKYLT                             
058600           MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                      
058700           CALL WMEDKONV USING MED-WMEDAREA                               
058800           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
058900           MOVE JA             TO FLFEL-FAELT                             
059000           PERFORM MFS-ROER-EJ-FAELT-UT                                   
059100           PERFORM MFS-ROER-EJ-FAELT-IN                                   
059200           IF MID-KDAVCOST = '10' OR '20' OR 'E1' OR 'K1'                 
059300             MOVE MFS-ALFA-FAELT-RAETT                                    
059400                               TO MOD-KDAVCOST-IN-ATTR                    
059500           ELSE                                                           
059600             MOVE MFS-ALFA-FAELT-FEL                                      
059700                               TO MOD-KDAVCOST-IN-ATTR                    
059800           END-IF                                                         
059900           MOVE MFS-ALFA-FAELT-RAETT                                      
060000                               TO MOD-IDFS-IN-ATTR                        
060100           MOVE MFS-NUM-FAELT-RAETT TO MOD-PRMATRL-IN-ATTR                
060200         ELSE                                                             
060300           IF (MID-PRAVCOST NOT = ALL '+'  AND                            
060400               MID-SULAGVDE NOT = ALL '+')                                
060500             MOVE 'GB'         TO MED-IDSKYLT                             
060600             MOVE ERR-TO-MANY-FIELDS TO MED-IDMFSFEL                      
060700             CALL WMEDKONV USING MED-WMEDAREA                             
060800             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
060900             PERFORM MFS-ROER-EJ-FAELT-UT                                 
061000             PERFORM MFS-ROER-EJ-FAELT-IN                                 
061100             MOVE JA           TO FLFEL-FAELT                             
061200             MOVE MFS-NUM-FAELT-FEL                                       
061300                               TO MOD-PRAVCOST-IN-ATTR                    
061400                                  MOD-SULAGVDE-IN-ATTR                    
061500             MOVE MFS-NUM-FAELT-RAETT                                     
061600                               TO MOD-PRMATRL-IN-ATTR                     
061700             IF MID-KDAVCOST = '10' OR '20' OR 'E1' OR 'K1'               
061800               MOVE MFS-ALFA-FAELT-RAETT                                  
061900                               TO MOD-KDAVCOST-IN-ATTR                    
062000             ELSE                                                         
062100               MOVE MFS-ALFA-FAELT-FEL                                    
062200                               TO MOD-KDAVCOST-IN-ATTR                    
062300             END-IF                                                       
062400             MOVE MFS-ALFA-FAELT-RAETT                                    
062500                               TO MOD-KDAVCOST-IN-ATTR                    
062600                                  MOD-IDFS-IN-ATTR                        
062700           ELSE                                                           
062800             IF (MID-SULAGVDE = ALL '+'  AND                              
062900                 MID-PRAVCOST NOT = ALL '+')                              
063000               IF MID-PRAVCOST NOT = ALL '+'                              
063100                 MOVE MID-PRAVCOST TO DEC-IDFRIDATA                       
063200                 MOVE +7           TO DEC-KVHELTAL                        
063300                 MOVE +2           TO DEC-KVDECIMAL                       
063400                 CALL WDECEDIT USING DEC-WDECAREA                         
063500                 IF DEC-KDSVAR-FEL                                        
063600                   MOVE MFS-NUM-FAELT-FEL                                 
063700                                   TO MOD-PRAVCOST-IN-ATTR                
063800                   MOVE 'GB'       TO MED-IDSKYLT                         
063900                   MOVE ERR-DATA   TO MED-IDMFSFEL                        
064000                   CALL WMEDKONV USING MED-WMEDAREA                       
064100                   MOVE MED-MFSFEL TO MOD-TEMFSFEL                        
064200                   PERFORM MFS-ROER-EJ-FAELT-UT                           
064300                   PERFORM MFS-ROER-EJ-FAELT-IN                           
064400                   MOVE MFS-NUM-FAELT-FEL                                 
064500                                   TO MOD-PRAVCOST-IN-ATTR                
064600                   MOVE MFS-NUM-FAELT-RAETT                               
064700                                   TO MOD-PRMATRL-IN-ATTR                 
064800                                      MOD-SULAGVDE-IN-ATTR                
064900                   MOVE MFS-ALFA-FAELT-RAETT                              
065000                                   TO MOD-IDFS-IN-ATTR                    
065100                   MOVE JA         TO FLFEL-FAELT                         
065200                 ELSE                                                     
065300                   MOVE DEC-IDEDITDATA                                    
065400                                   TO WS-PRAVCOST                         
065500                   MOVE MFS-NUM-FAELT-RAETT                               
065600                                   TO MOD-PRAVCOST-IN-ATTR                
065700                   MOVE 'PR'       TO W-UPPDAT                            
065800                 END-IF                                                   
065900               END-IF                                                     
066000               IF MID-KDAVCOST NOT = '++'                                 
066100                 IF ((MID-KDAVCOST = '10') AND                            
066200                     (MID-PRAVCOST NOT = ALL '+'))                        
066300                   MOVE MFS-ALFA-FAELT-RAETT                              
066400                                      TO MOD-KDAVCOST-IN-ATTR             
066500                   MOVE MID-KDAVCOST  TO WS-KDAVCOST                      
066600                 ELSE                                                     
066700                   MOVE MFS-ALFA-FAELT-FEL                                
066800                                       TO MOD-KDAVCOST-IN-ATTR            
066900                   MOVE 'GB'           TO MED-IDSKYLT                     
067000                   MOVE ERR-DATA       TO MED-IDMFSFEL                    
067100                   CALL WMEDKONV USING MED-WMEDAREA                       
067200                   MOVE MED-MFSFEL     TO MOD-TEMFSFEL                    
067300                   MOVE JA             TO FLFEL-FAELT                     
067400                   PERFORM MFS-ROER-EJ-FAELT-UT                           
067500                   PERFORM MFS-ROER-EJ-FAELT-IN                           
067600                   MOVE MFS-NUM-FAELT-RAETT                               
067700                                       TO MOD-SULAGVDE-IN-ATTR            
067800                                          MOD-PRMATRL-IN-ATTR             
067900                   MOVE MFS-ALFA-FAELT-RAETT                              
068000                                       TO MOD-IDFS-IN-ATTR                
068100                 END-IF                                                   
068200               ELSE                                                       
068300                 MOVE MFS-ALFA-FAELT-FEL                                  
068400                                       TO MOD-KDAVCOST-IN-ATTR            
068500                 MOVE 'GB'             TO MED-IDSKYLT                     
068600                 MOVE ERR-DATA         TO MED-IDMFSFEL                    
068700                 CALL WMEDKONV USING MED-WMEDAREA                         
068800                 MOVE MED-MFSFEL       TO MOD-TEMFSFEL                    
068900                 MOVE JA               TO FLFEL-FAELT                     
069000                 PERFORM MFS-ROER-EJ-FAELT-UT                             
069100                 PERFORM MFS-ROER-EJ-FAELT-IN                             
069200                 MOVE MFS-NUM-FAELT-RAETT                                 
069300                                       TO MOD-PRAVCOST-IN-ATTR            
069400                                          MOD-SULAGVDE-IN-ATTR            
069500                                          MOD-PRMATRL-IN-ATTR             
069600                 MOVE MFS-ALFA-FAELT-RAETT                                
069700                                       TO MOD-IDFS-IN-ATTR                
069800               END-IF                                                     
069900             ELSE                                                         
070000               IF (MID-PRAVCOST = ALL '+'  AND                            
070100                   MID-SULAGVDE NOT = ALL '+')                            
070200                 IF MID-SULAGVDE NOT = ALL '+'                            
070300                   MOVE MID-SULAGVDE TO DEC-IDFRIDATA                     
070400                   MOVE +8           TO DEC-KVHELTAL                      
070500                   MOVE +2           TO DEC-KVDECIMAL                     
070600                   CALL WDECEDIT USING DEC-WDECAREA                       
070700                   IF DEC-KDSVAR-FEL                                      
070800                     MOVE MFS-NUM-FAELT-FEL                               
070900                                     TO MOD-SULAGVDE-IN-ATTR              
071000                     MOVE 'GB'       TO MED-IDSKYLT                       
071100                     MOVE ERR-DATA   TO MED-IDMFSFEL                      
071200                     CALL WMEDKONV USING MED-WMEDAREA                     
071300                     MOVE MED-MFSFEL TO MOD-TEMFSFEL                      
071400                     MOVE JA         TO FLFEL-FAELT                       
071500                     PERFORM MFS-ROER-EJ-FAELT-UT                         
071600                     PERFORM MFS-ROER-EJ-FAELT-IN                         
071700                     MOVE MFS-NUM-FAELT-FEL                               
071800                                     TO MOD-SULAGVDE-IN-ATTR              
071900                     MOVE MFS-NUM-FAELT-RAETT                             
072000                                     TO MOD-PRAVCOST-IN-ATTR              
072100                                        MOD-PRMATRL-IN-ATTR               
072200                     MOVE MFS-ALFA-FAELT-RAETT                            
072300                                     TO MOD-IDFS-IN-ATTR                  
072400                                        MOD-KDAVCOST-IN-ATTR              
072500                   ELSE                                                   
072600                     MOVE DEC-IDEDITDATA                                  
072700                                     TO WS-SULAGVDE                       
072800                     MOVE MFS-NUM-FAELT-RAETT                             
072900                                     TO MOD-SULAGVDE-IN-ATTR              
073000                     MOVE 'SU'       TO W-UPPDAT                          
073100                   END-IF                                                 
073200                 END-IF                                                   
073300                 IF MID-KDAVCOST NOT = '++'                               
073400                   IF ((MID-KDAVCOST = '20' OR 'E1' OR 'K1') AND          
073500                       (MID-SULAGVDE NOT = ALL '+'))                      
073600                     MOVE MFS-ALFA-FAELT-RAETT                            
073700                                         TO MOD-KDAVCOST-IN-ATTR          
073800                     MOVE MID-KDAVCOST TO WS-KDAVCOST                     
073900                   ELSE                                                   
074000                     MOVE MFS-ALFA-FAELT-FEL                              
074100                                         TO MOD-KDAVCOST-IN-ATTR          
074200                     MOVE 'GB'         TO MED-IDSKYLT                     
074300                     MOVE ERR-DATA     TO MED-IDMFSFEL                    
074400                     CALL WMEDKONV USING MED-WMEDAREA                     
074500                     MOVE MED-MFSFEL   TO MOD-TEMFSFEL                    
074600                     MOVE JA           TO FLFEL-FAELT                     
074700                     PERFORM MFS-ROER-EJ-FAELT-UT                         
074800                     PERFORM MFS-ROER-EJ-FAELT-IN                         
074900                     MOVE MFS-NUM-FAELT-RAETT                             
075000                                       TO MOD-PRAVCOST-IN-ATTR            
075100                                          MOD-PRMATRL-IN-ATTR             
075200                     MOVE MFS-ALFA-FAELT-RAETT                            
075300                                       TO MOD-IDFS-IN-ATTR                
075400                   END-IF                                                 
075500                 ELSE                                                     
075600                   MOVE MFS-ALFA-FAELT-FEL                                
075700                                     TO MOD-KDAVCOST-IN-ATTR              
075800                   MOVE 'GB'         TO MED-IDSKYLT                       
075900                   MOVE ERR-DATA     TO MED-IDMFSFEL                      
076000                   CALL WMEDKONV USING MED-WMEDAREA                       
076100                   MOVE MED-MFSFEL   TO MOD-TEMFSFEL                      
076200                   MOVE JA           TO FLFEL-FAELT                       
076300                   PERFORM MFS-ROER-EJ-FAELT-UT                           
076400                   PERFORM MFS-ROER-EJ-FAELT-IN                           
076500                   MOVE MFS-NUM-FAELT-RAETT                               
076600                                     TO MOD-PRAVCOST-IN-ATTR              
076700                                        MOD-SULAGVDE-IN-ATTR              
076800                                        MOD-PRMATRL-IN-ATTR               
076900                   MOVE MFS-ALFA-FAELT-RAETT                              
077000                                     TO MOD-IDFS-IN-ATTR                  
077100                 END-IF                                                   
077200               END-IF                                                     
077300             END-IF                                                       
077400           END-IF                                                         
077500         END-IF                                                           
077600       END-IF                                                             
077700     END-IF                                                               
077800     IF FLFEL-FAELT = NEJ                                                 
077900       IF (MFS-UPD-V)                                                     
078000         IF (MID-PRMATRL-IN = ALL '+')                                    
078100           MOVE 'GB'           TO MED-IDSKYLT                             
078200           MOVE WS-ERR-PF23-AND-NO-DATA TO MOD-TEMFSFEL                   
078300           MOVE JA             TO FLFEL-FAELT                             
078400           PERFORM MFS-ROER-EJ-FAELT-UT                                   
078500           PERFORM MFS-ROER-EJ-FAELT-IN                                   
078600           MOVE MFS-NUM-FAELT-FEL TO MOD-PRMATRL-IN-ATTR                  
078700           MOVE MFS-NUM-FAELT-RAETT                                       
078800                               TO MOD-PRAVCOST-IN-ATTR                    
078900                                  MOD-SULAGVDE-IN-ATTR                    
079000           MOVE MFS-ALFA-FAELT-RAETT                                      
079100                               TO MOD-IDFS-IN-ATTR                        
079200           IF MID-KDAVCOST = '30'                                         
079300             MOVE MFS-ALFA-FAELT-RAETT                                    
079400                               TO MOD-KDAVCOST-IN-ATTR                    
079500           ELSE                                                           
079600             MOVE MFS-ALFA-FAELT-FEL                                      
079700                               TO MOD-KDAVCOST-IN-ATTR                    
079800           END-IF                                                         
079900         ELSE                                                             
080000           IF (MID-PRMATRL-IN NOT = ALL '+')                              
080100             MOVE MID-PRMATRL-IN TO DEC-IDFRIDATA                         
080200             MOVE +7             TO DEC-KVHELTAL                          
080300             MOVE +2             TO DEC-KVDECIMAL                         
080400             CALL WDECEDIT USING DEC-WDECAREA                             
080500             IF DEC-KDSVAR-FEL                                            
080600               MOVE MFS-NUM-FAELT-FEL                                     
080700                                 TO MOD-PRMATRL-IN-ATTR                   
080800               MOVE 'GB'       TO MED-IDSKYLT                             
080900               MOVE ERR-DATA   TO MED-IDMFSFEL                            
081000               CALL WMEDKONV USING MED-WMEDAREA                           
081100               MOVE MED-MFSFEL TO MOD-TEMFSFEL                            
081200               PERFORM MFS-ROER-EJ-FAELT-UT                               
081300               PERFORM MFS-ROER-EJ-FAELT-IN                               
081400               MOVE MFS-NUM-FAELT-RAETT                                   
081500                                   TO MOD-PRAVCOST-IN-ATTR                
081600                                      MOD-SULAGVDE-IN-ATTR                
081700               MOVE MFS-ALFA-FAELT-RAETT                                  
081800                                   TO MOD-IDFS-IN-ATTR                    
081900               IF MID-KDAVCOST = '30'                                     
082000                 MOVE MFS-ALFA-FAELT-RAETT                                
082100                                   TO MOD-KDAVCOST-IN-ATTR                
082200               ELSE                                                       
082300                 MOVE MFS-ALFA-FAELT-FEL                                  
082400                                   TO MOD-KDAVCOST-IN-ATTR                
082500               END-IF                                                     
082600               MOVE JA         TO FLFEL-FAELT                             
082700             ELSE                                                         
082800               MOVE DEC-IDEDITDATA                                        
082900                               TO WS-PRMATRL                              
083000               MOVE MFS-NUM-FAELT-RAETT                                   
083100                               TO MOD-PRMATRL-IN-ATTR                     
083200               MOVE 'PM'       TO W-UPPDAT                                
083300             END-IF                                                       
083400           END-IF                                                         
083500           IF MID-KDAVCOST NOT = '++'                                     
083600             IF MID-KDAVCOST = '30'                                       
083700               IF (MID-PRMATRL-IN NOT = ALL '+')                          
083800                 MOVE MFS-ALFA-FAELT-RAETT                                
083900                               TO MOD-KDAVCOST-IN-ATTR                    
084000                 MOVE MID-KDAVCOST                                        
084100                               TO WS-KDAVCOST                             
084200               ELSE                                                       
084300                 MOVE MFS-ALFA-FAELT-FEL                                  
084400                                 TO MOD-KDAVCOST-IN-ATTR                  
084500                 MOVE 'GB'       TO MED-IDSKYLT                           
084600                 MOVE ERR-DATA   TO MED-IDMFSFEL                          
084700                 CALL WMEDKONV USING MED-WMEDAREA                         
084800                 MOVE MED-MFSFEL TO MOD-TEMFSFEL                          
084900                 MOVE JA         TO FLFEL-FAELT                           
085000                 PERFORM MFS-ROER-EJ-FAELT-UT                             
085100                 PERFORM MFS-ROER-EJ-FAELT-IN                             
085200                 MOVE MFS-NUM-FAELT-RAETT                                 
085300                                     TO MOD-PRAVCOST-IN-ATTR              
085400                                        MOD-SULAGVDE-IN-ATTR              
085500                 MOVE MFS-ALFA-FAELT-RAETT                                
085600                                     TO MOD-IDFS-IN-ATTR                  
085700               END-IF                                                     
085800             ELSE                                                         
085900               MOVE MFS-ALFA-FAELT-FEL                                    
086000                                 TO MOD-KDAVCOST-IN-ATTR                  
086100               MOVE 'GB'         TO MED-IDSKYLT                           
086200               MOVE ERR-DATA     TO MED-IDMFSFEL                          
086300               CALL WMEDKONV USING MED-WMEDAREA                           
086400               MOVE MED-MFSFEL   TO MOD-TEMFSFEL                          
086500               MOVE JA         TO FLFEL-FAELT                             
086600               PERFORM MFS-ROER-EJ-FAELT-UT                               
086700               PERFORM MFS-ROER-EJ-FAELT-IN                               
086800               MOVE MFS-NUM-FAELT-RAETT                                   
086900                                   TO MOD-PRAVCOST-IN-ATTR                
087000                                      MOD-SULAGVDE-IN-ATTR                
087100               MOVE MFS-ALFA-FAELT-RAETT                                  
087200                                   TO MOD-IDFS-IN-ATTR                    
087300             END-IF                                                       
087400           ELSE                                                           
087500             MOVE MFS-ALFA-FAELT-FEL                                      
087600                                    TO MOD-KDAVCOST-IN-ATTR               
087700             MOVE 'GB'              TO MED-IDSKYLT                        
087800             MOVE ERR-INFO-MISSING  TO MED-IDMFSFEL                       
087900             CALL WMEDKONV USING MED-WMEDAREA                             
088000             MOVE MED-MFSFEL        TO MOD-TEMFSFEL                       
088100             MOVE JA                TO FLFEL-FAELT                        
088200             PERFORM MFS-ROER-EJ-FAELT-UT                                 
088300             PERFORM MFS-ROER-EJ-FAELT-IN                                 
088400             MOVE MFS-NUM-FAELT-RAETT                                     
088500                                 TO MOD-PRAVCOST-IN-ATTR                  
088600                                    MOD-SULAGVDE-IN-ATTR                  
088700             MOVE MFS-ALFA-FAELT-RAETT                                    
088800                                 TO MOD-IDFS-IN-ATTR                      
088900           END-IF                                                         
089000         END-IF                                                           
089100       END-IF                                                             
089200     END-IF                                                               
089300     .                                                                    
089400     EJECT                                                                
089500                                                                          
089600 D-KOLLA-INFAELT  SECTION.                                                
089700                                                                          
089800     MOVE 'D-KOLLA-INFAELT '  TO WS-SEKTION                               
089900                                                                          
090000     IF MID-PRMATRL-IN  = ALL ' '                                         
090100       MOVE ALL '+'             TO MID-PRMATRL-IN                         
090200     END-IF                                                               
090300     IF MID-PRAVCOST  = ALL ' '                                           
090400       MOVE ALL '+'             TO MID-PRAVCOST                           
090500     END-IF                                                               
090600     IF MID-SULAGVDE  = ALL ' '                                           
090700       MOVE ALL '+'             TO MID-SULAGVDE                           
090800     END-IF                                                               
090900     IF MID-KDAVCOST  = ALL ' '                                           
091000       MOVE ALL '+'             TO MID-KDAVCOST                           
091100     END-IF                                                               
091200                                                                          
091300     IF (MID-PRAVCOST = ALL '+')                                          
091400      AND (MID-SULAGVDE = ALL '+')                                        
091500      AND (MID-KDAVCOST NOT = ALL '+')                                    
091600      AND (MID-IDFS = ALL '+')                                            
091700      AND (MID-PRMATRL-IN NOT = ALL '+')                                  
091800         MOVE 'GB'              TO MED-IDSKYLT                            
091900         MOVE INF-PRESS-PF23    TO MED-IDMFSFEL                           
092000         CALL WMEDKONV USING MED-WMEDAREA                                 
092100         MOVE MED-MFSFEL        TO MOD-TEMFSFEL                           
092200         MOVE JA                TO FLFEL-FAELT                            
092300         PERFORM MFS-LAES-IN-IGEN                                         
092400         PERFORM MFS-ROER-EJ-FAELT-IN                                     
092500         PERFORM MFS-ROER-EJ-FAELT-UT                                     
092600         MOVE MFS-NUM-FAELT-RAETT                                         
092700                              TO MOD-PRAVCOST-IN-ATTR                     
092800                                 MOD-PRMATRL-IN-ATTR                      
092900                                 MOD-SULAGVDE-IN-ATTR                     
093000         MOVE MFS-ALFA-FAELT-RAETT                                        
093100                              TO MOD-IDFS-IN-ATTR                         
093200                                 MOD-KDAVCOST-IN-ATTR                     
093300     END-IF                                                               
093400     IF (MID-PRAVCOST NOT = ALL '+')                                      
093500      AND (MID-SULAGVDE = ALL '+')                                        
093600      AND (MID-KDAVCOST NOT = ALL '+')                                    
093700      AND (MID-PRMATRL-IN = ALL '+')                                      
093800         MOVE 'GB'              TO MED-IDSKYLT                            
093900         MOVE INF-PRESS-PF11    TO MED-IDMFSFEL                           
094000         CALL WMEDKONV USING MED-WMEDAREA                                 
094100         MOVE MED-MFSFEL        TO MOD-TEMFSFEL                           
094200         MOVE JA                TO FLFEL-FAELT                            
094300         PERFORM MFS-LAES-IN-IGEN                                         
094400         PERFORM MFS-ROER-EJ-FAELT-IN                                     
094500         PERFORM MFS-ROER-EJ-FAELT-UT                                     
094600         MOVE MFS-NUM-FAELT-RAETT                                         
094700                                TO MOD-PRAVCOST-IN-ATTR                   
094800                                   MOD-PRMATRL-IN-ATTR                    
094900                                   MOD-SULAGVDE-IN-ATTR                   
095000         MOVE MFS-ALFA-FAELT-RAETT                                        
095100                                TO MOD-IDFS-IN-ATTR                       
095200                                   MOD-KDAVCOST-IN-ATTR                   
095300     END-IF                                                               
095400     IF (MID-PRAVCOST = ALL '+')                                          
095500      AND (MID-SULAGVDE NOT = ALL '+')                                    
095600      AND (MID-KDAVCOST NOT = ALL '+')                                    
095700      AND (MID-PRMATRL-IN = ALL '+')                                      
095800         MOVE 'GB'              TO MED-IDSKYLT                            
095900         MOVE INF-PRESS-PF11    TO MED-IDMFSFEL                           
096000         CALL WMEDKONV USING MED-WMEDAREA                                 
096100         MOVE MED-MFSFEL        TO MOD-TEMFSFEL                           
096200         MOVE JA                TO FLFEL-FAELT                            
096300         PERFORM MFS-LAES-IN-IGEN                                         
096400         PERFORM MFS-ROER-EJ-FAELT-IN                                     
096500         PERFORM MFS-ROER-EJ-FAELT-UT                                     
096600         MOVE MFS-NUM-FAELT-RAETT                                         
096700                                TO MOD-PRAVCOST-IN-ATTR                   
096800                                   MOD-PRMATRL-IN-ATTR                    
096900                                   MOD-SULAGVDE-IN-ATTR                   
097000         MOVE MFS-ALFA-FAELT-RAETT                                        
097100                                TO MOD-IDFS-IN-ATTR                       
097200                                   MOD-KDAVCOST-IN-ATTR                   
097300     END-IF                                                               
097400     IF (MID-PRAVCOST NOT = ALL '+')                                      
097500      AND (MID-SULAGVDE NOT = ALL '+')                                    
097600      AND (MID-KDAVCOST NOT = ALL '+')                                    
097700      AND (MID-IDFS NOT = ALL '+')                                        
097800      AND (MID-PRMATRL-IN NOT = ALL '+')                                  
097900         MOVE 'GB'              TO MED-IDSKYLT                            
098000         MOVE WS-ERR-UPDATE-KEY TO MOD-TEMFSFEL                           
098100         MOVE JA                TO FLFEL-FAELT                            
098200         PERFORM MFS-LAES-IN-IGEN                                         
098300         PERFORM MFS-ROER-EJ-FAELT-IN                                     
098400         PERFORM MFS-ROER-EJ-FAELT-UT                                     
098500         MOVE MFS-NUM-FAELT-RAETT                                         
098600                                TO MOD-PRAVCOST-IN-ATTR                   
098700                                   MOD-PRMATRL-IN-ATTR                    
098800                                   MOD-SULAGVDE-IN-ATTR                   
098900         MOVE MFS-ALFA-FAELT-RAETT                                        
099000                                TO MOD-IDFS-IN-ATTR                       
099100                                   MOD-KDAVCOST-IN-ATTR                   
099200     END-IF                                                               
099300                                                                          
099400     .                                                                    
099500     EJECT                                                                
099600                                                                          
099700 F-LAES-VISA-INFO SECTION.                                                
099800                                                                          
099900     MOVE 'F-LAES-VISA-INFO'  TO WS-SEKTION                               
100000                                                                          
100100     PERFORM IMS-GU-WDK601                                                
100200                                                                          
100300     IF SEGMENT-SAKNAS                                                    
100400       MOVE 'GB'                 TO MED-IDSKYLT                           
100500       MOVE ERR-PART-MISSING     TO MED-IDMFSMED                          
100600       CALL WMEDKONV USING MED-WMEDAREA                                   
100700       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
100800       PERFORM MFS-RENSA-FAELT-UT                                         
100900     ELSE                                                                 
101000       MOVE ART-KDPRODSL         TO MOD-KDPRODSL                          
101100       PERFORM IMS-GNP-WDK611                                             
101200       IF SEGMENT-SAKNAS                                                  
101300          MOVE 'GB'              TO MED-IDSKYLT                           
101400          MOVE ERR-PART-MISSING  TO MED-IDMFSFEL                          
101500          CALL WMEDKONV USING MED-WMEDAREA                                
101600          MOVE MED-MFSFEL        TO MOD-TEMFSFEL                          
101700          PERFORM MFS-RENSA-FAELT-UT                                      
101800       ELSE                                                               
101900         MOVE CLAG-KDPSLLOC      TO MOD-KDPSLLOC                          
102000         PERFORM IMS-GU-WDD3                                              
102100         IF SEGMENT-FINNS                                                 
102200           MOVE TEXT-BEART       TO MOD-BEART                             
102300         ELSE                                                             
102400           MOVE SPACE            TO MOD-BEART                             
102500         END-IF                                                           
102600         PERFORM IMS-GU-WDK701                                            
102700         IF SEGMENT-SAKNAS                                                
102800           MOVE 'GB'             TO MED-IDSKYLT                           
102900           MOVE ERR-PART-MISSING TO MED-IDMFSFEL                          
103000           CALL WMEDKONV USING MED-WMEDAREA                               
103100           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
103200           PERFORM MFS-RENSA-FAELT-UT                                     
103300         ELSE                                                             
103400           PERFORM IMS-GNP-WDK711                                         
103500           IF SEGMENT-SAKNAS                                              
103600             MOVE 'GB'             TO MED-IDSKYLT                         
103700             MOVE ERR-PART-MISSING TO MED-IDMFSFEL                        
103800             CALL WMEDKONV USING MED-WMEDAREA                             
103900             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
104000             PERFORM MFS-RENSA-FAELT-UT                                   
104100           ELSE                                                           
104200             MOVE SLAG-KVAKS-PAV    TO WS-KVAKS-PAV                       
104300             MOVE SLAG-KVAKS-SDC    TO WS-KVAKS-SDC                       
104400             MOVE SLAG-KVLS         TO WS-KVLS                            
104500             MOVE SLAG-KVLS         TO WS-KVDISP                          
104600             MOVE SLAG-KVEFRS       TO WS-KVEFRS                          
104700             ADD  SLAG-KVEFRS       TO WS-KVDISP                          
104800             MOVE SLAG-TIAVCOST     TO MOD-TIAVCOST                       
104900             MOVE SLAG-PRAVCOST     TO WS-PRAVCOST                        
105000             PERFORM IMS-GU-WDK712                                        
105100             IF SEGMENT-SAKNAS                                            
105200               MOVE ZERO            TO WS-PRMATRL                         
105300             ELSE                                                         
105400               MOVE LART-PRMATRL    TO WS-PRMATRL                         
105500             END-IF                                                       
105600             PERFORM FBD-CHECK-IF-BYTES                                   
105700             IF WS-BYTES = 'Y'                                            
105800               PERFORM IMS-GU-WDK701                                      
105900               IF SEGMENT-FINNS                                           
106000                 PERFORM IMS-GNP-WDK711                                   
106100                 IF SEGMENT-FINNS                                         
106200                   ADD SLAG-KVAKS-PAV TO WS-KVAKS-PAV                     
106300                                  GIVING MOD-KVAKS-PAV                    
106400                   ADD SLAG-KVAKS-SDC TO WS-KVAKS-SDC                     
106500                                  GIVING MOD-KVAKS-SDC                    
106600                   ADD SLAG-KVLS   TO WS-KVLS                             
106700                   MOVE WS-KVLS    TO MOD-KVLS                            
106800                   MOVE WS-KVLS    TO WS-KVDISP                           
106900                   ADD SLAG-KVEFRS TO WS-KVDISP                           
107000                   ADD SLAG-KVEFRS TO WS-KVEFRS                           
107100                                  GIVING MOD-KVEFRS                       
107200                 ELSE                                                     
107300                   MOVE 'N' TO WS-BYTES                                   
107400                 END-IF                                                   
107500               ELSE                                                       
107600                  MOVE 'N' TO WS-BYTES                                    
107700               END-IF                                                     
107800             END-IF                                                       
107900             IF WS-BYTES = 'N'                                            
108000               MOVE WS-KVAKS-PAV TO MOD-KVAKS-PAV                         
108100               MOVE WS-KVAKS-SDC TO MOD-KVAKS-SDC                         
108200               MOVE WS-KVLS      TO MOD-KVLS                              
108300               MOVE WS-KVLS      TO WS-KVDISP                             
108400               MOVE WS-KVEFRS    TO MOD-KVEFRS                            
108500               ADD  WS-KVEFRS    TO WS-KVDISP                             
108600             END-IF                                                       
108700             MULTIPLY WS-KVDISP BY WS-PRAVCOST                            
108800                                   GIVING WS-SULAGVDE                     
108900             MOVE WS-SULAGVDE     TO MOD-SULAGVDE                         
109000             MOVE WS-PRAVCOST     TO MOD-PRAVCOST                         
109100             MOVE WS-PRMATRL      TO MOD-PRMATRL                          
109200             PERFORM MFS-OEPPNA-FAELT-INDATA                              
109300             PERFORM MFS-RENSA-FAELT-IN                                   
109400           END-IF                                                         
109500         END-IF                                                           
109600       END-IF                                                             
109700     END-IF                                                               
109800     .                                                                    
109900     EJECT                                                                
110000 FBD-CHECK-IF-BYTES SECTION.                                              
110100     MOVE W-IDARTNR TO BYT20-IDARTNR                                      
110200       IF BYT20-BYTES                                                     
110300             SUBTRACT +6000 FROM   W-IDARTNR                              
110400             MOVE 'Y'  TO   WS-BYTES                                      
110500       ELSE                                                               
110600         IF BYT20-RADIO                                                   
110700             SUBTRACT +1000 FROM   W-IDARTNR                              
110800             MOVE 'Y'  TO   WS-BYTES                                      
110900         ELSE                                                             
111000             MOVE 'N'  TO   WS-BYTES                                      
111100         END-IF                                                           
111200       END-IF                                                             
111300                                                                          
111400     .                                                                    
111500     EJECT                                                                
111600                                                                          
111700                                                                          
111800 G-UPPDATERA-DATA  SECTION.                                               
111900                                                                          
112000     MOVE 'G-UPPDATERA-DATA' TO WS-SEKTION                                
112100                                                                          
112200     PERFORM IMS-GU-WDK701                                                
112300                                                                          
112400     IF SEGMENT-SAKNAS                                                    
112500       MOVE 'GB'               TO MED-IDSKYLT                             
112600       MOVE ERR-PART-MISSING   TO MED-IDMFSFEL                            
112700       CALL WMEDKONV USING MED-WMEDAREA                                   
112800       MOVE MED-MFSFEL         TO MOD-TEMFSFEL                            
112900       MOVE JA                 TO FLFEL-FAELT                             
113000     ELSE                                                                 
113100       IF (MFS-UPDATE)                                                    
113200         PERFORM IMS-GHNP-WDK711                                          
113300         IF SEGMENT-SAKNAS                                                
113400           MOVE 'GB'             TO MED-IDSKYLT                           
113500           MOVE ERR-PART-MISSING TO MED-IDMFSFEL                          
113600           CALL WMEDKONV USING MED-WMEDAREA                               
113700           MOVE MED-MFSFEL       TO MOD-TEMFSFEL                          
113800           MOVE JA               TO FLFEL-FAELT                           
113900         ELSE                                                             
114000           PERFORM GB-KONVERTERA-DATUM                                    
114100           PERFORM GA-UPPDATERA                                           
114200         END-IF                                                           
114300       ELSE                                                               
114400         IF (MFS-UPD-V)                                                   
114500           PERFORM IMS-GHU-WDK712                                         
114600           IF SEGMENT-SAKNAS                                              
114700             MOVE 'GB'           TO MED-IDSKYLT                           
114800             MOVE ERR-PART-MISSING TO MED-IDMFSFEL                        
114900             CALL WMEDKONV USING MED-WMEDAREA                             
115000             MOVE MED-MFSFEL     TO MOD-TEMFSFEL                          
115100             MOVE JA             TO FLFEL-FAELT                           
115200           ELSE                                                           
115300             PERFORM GC-UPPDATERA-PRMATRL                                 
115400           END-IF                                                         
115500         END-IF                                                           
115600       END-IF                                                             
115700     END-IF                                                               
115800                                                                          
115900     .                                                                    
116000     EJECT                                                                
116100 GA-UPPDATERA   SECTION.                                                  
116200                                                                          
116300     MOVE 'GA-UPPDATERA  ' TO WS-SEKTION                                  
116400                                                                          
116500     MOVE SPACE            TO A16-W510A16                                 
116600                                                                          
116700     MOVE 'A16'            TO A16-IDPTYP                                  
116800     MOVE 'M20'            TO A16-KDEKOHT                                 
116900                                                                          
117000     MOVE DCS-IDFTG        TO A16-IDFTG                                   
118400                                                                          
118500     MOVE DCS-IDDC         TO A16-IDDC-SEND                               
118600                              A16-IDDC-REC                                
118700     MOVE W-IDARTNR        TO A16-IDARTNR                                 
118800     PERFORM IMS-GU-WDK601                                                
118900                                                                          
119000     IF SEGMENT-FINNS                                                     
119100       MOVE ART-KDPRODSL   TO A16-KDPRODSL                                
119200       PERFORM IMS-GNP-WDK611                                             
119300       IF SEGMENT-FINNS                                                   
119400         MOVE CLAG-KDPSLLOC TO A16-KDPSLLOC                               
119500       ELSE                                                               
119600         MOVE ERR-PART-MISSING TO MED-IDMFSFEL                            
119700         CALL WMEDKONV USING MED-WMEDAREA                                 
119800         MOVE MED-MFSFEL       TO MOD-TEMFSFEL                            
119900         MOVE JA               TO FLFEL-FAELT                             
120000       END-IF                                                             
120100     ELSE                                                                 
120200       MOVE ERR-PART-MISSING TO MED-IDMFSFEL                              
120300       CALL WMEDKONV USING MED-WMEDAREA                                   
120400       MOVE MED-MFSFEL       TO MOD-TEMFSFEL                              
120500       MOVE JA               TO FLFEL-FAELT                               
120600     END-IF                                                               
120700                                                                          
120800     MOVE LOKAL-DAT        TO A16-DAJUSTDA                                
120900     MOVE LOKAL-DATUM      TO SLAG-TIAVCOST                               
121000     MOVE SLAG-KVLS        TO WS-KVDISP                                   
121100     ADD  SLAG-KVEFRS      TO WS-KVDISP                                   
121200     MOVE WS-KVDISP        TO A16-KVLS                                    
121300     MOVE SPAR-IDUSER      TO A16-IDUSER                                  
121400     MOVE SLAG-PRAVCOST    TO A16-PRAVCOST-OLD                            
121500                              WS-PRAVCOST-OLD                             
121600     MOVE WS-KDAVCOST      TO A16-KDAVCOST                                
121700                                                                          
121710     IF WS-PRAVCOST   =  SLAG-PRAVCOST                                    
121720       MOVE NEJ                    TO W-UPPDATE-TOTAL                     
121730       MOVE ERR-NO-UPDATE-DONE     TO MED-IDMFSINF                        
121740       CALL WMEDKONV            USING MED-WMEDAREA                        
121750       MOVE MED-MFSINF             TO MOD-TEMFSINF                        
121751       MOVE 'GB'                   TO MED-IDSKYLT                         
121752       MOVE WS-ERR-SAME-AVG-COST   TO MOD-TEMFSFEL                        
121753       MOVE JA                     TO FLFEL-FAELT                         
121754       PERFORM MFS-LAES-IN-IGEN                                           
121755       PERFORM MFS-ROER-EJ-FAELT-IN                                       
121756       PERFORM MFS-ROER-EJ-FAELT-UT                                       
121757       MOVE MFS-NUM-FAELT-RAETT                                           
121758                                   TO MOD-PRAVCOST-IN-ATTR                
121759                                      MOD-PRMATRL-IN-ATTR                 
121760                                      MOD-SULAGVDE-IN-ATTR                
121761       MOVE MFS-ALFA-FAELT-RAETT                                          
121762                                   TO MOD-IDFS-IN-ATTR                    
121763                                      MOD-KDAVCOST-IN-ATTR                
121764                                                                          
121765     ELSE                                                                 
121766       MOVE JA                     TO W-UPPDATE-TOTAL                     
121767     END-IF                                                               
121770     IF W-UPPDATE-TOTAL  = JA                                             
121800       IF W-PR-UPPDAT                                                     
121900         MOVE WS-PRAVCOST  TO SLAG-PRAVCOST                               
122000         PERFORM IMS-REPL-WDK711                                          
122100         IF SEGMENT-SAKNAS                                                
122200           MOVE ERR-NO-UPDATE-DONE   TO MED-IDMFSINF                      
122300           CALL WMEDKONV          USING MED-WMEDAREA                      
122400           MOVE MED-MFSINF           TO MOD-TEMFSINF                      
122500         ELSE                                                             
122600           MOVE INF-UPDATE-DONE      TO MED-IDMFSINF                      
122700           CALL WMEDKONV          USING MED-WMEDAREA                      
122800           MOVE MED-MFSINF           TO MOD-TEMFSINF                      
122900         END-IF                                                           
123000         MOVE WS-PRAVCOST  TO A16-PRAVCOST                                
123100         MOVE ZERO         TO A16-SUAVCOST                                
123200       ELSE                                                               
123300         IF W-SU-UPPDAT                                                   
123400           MOVE WS-SULAGVDE  TO A16-SUAVCOST                              
123500           COMPUTE WS-PRAVCOST ROUNDED = ((WS-KVDISP *                    
123600                                 SLAG-PRAVCOST) +                         
123700                                 WS-SULAGVDE) / WS-KVDISP                 
123800            ON SIZE ERROR                                                 
123900              MOVE ZERO      TO WS-PRAVCOST                               
124000           END-COMPUTE                                                    
124100           MOVE WS-PRAVCOST  TO SLAG-PRAVCOST                             
124200           PERFORM IMS-REPL-WDK711                                        
124300           IF SEGMENT-SAKNAS                                              
124400             MOVE ERR-NO-UPDATE-DONE   TO MED-IDMFSINF                    
124500             CALL WMEDKONV          USING MED-WMEDAREA                    
124600             MOVE MED-MFSINF           TO MOD-TEMFSINF                    
124700           ELSE                                                           
124800             MOVE INF-UPDATE-DONE      TO MED-IDMFSINF                    
124900             CALL WMEDKONV          USING MED-WMEDAREA                    
125000             MOVE MED-MFSINF           TO MOD-TEMFSINF                    
125100           END-IF                                                         
125200           MOVE WS-PRAVCOST  TO A16-PRAVCOST                              
125300         END-IF                                                           
125400       END-IF                                                             
125410     END-IF                                                               
125500                                                                          
125600     IF MID-IDFS NOT = ALL '+'                                            
125700       MOVE MID-IDFS       TO A16-IDFS                                    
125800     END-IF                                                               
125900                                                                          
125910     IF W-UPPDATE-TOTAL  = JA                                             
125920       IF DCS-LAND-NON-VCC-OWNED                                          
125930         IF MFS-UPDATE                                                    
125940           PERFORM H-UPPDATERA-WDR8                                       
125950         END-IF                                                           
125960       ELSE                                                               
126300         IF MFS-UPDATE                                                    
126400           PERFORM GAA-SKAPA-WDR8                                         
126500         END-IF                                                           
126600       END-IF                                                             
126610     END-IF                                                               
126700     .                                                                    
126800     EJECT                                                                
126900 GAA-SKAPA-WDR8  SECTION.                                                 
127000                                                                          
127100     MOVE 'GA-SKAPA-WDR8   ' TO WS-SEKTION                                
127200                                                                          
127300     MOVE SPACE                TO FIL-WDR801                              
127400                                                                          
127500     MOVE 'W5020100'           TO FIL-IDPGM                               
127600     MOVE DAGENS-DATUM         TO FIL-TIREGDAT                            
127700     MOVE DAGENS-TID           TO FIL-TIKLOCK                             
127800     ADD +1                    TO WS-IDSEKVNR                             
127900     MOVE WS-IDSEKVNR          TO FIL-IDSEKVNR                            
128000     MOVE 'W510'               TO FIL-CT-IDSYSTEM                         
128100     MOVE 'A16'                TO FIL-CT-IDPTYP                           
128200     MOVE A16-W510A16          TO FIL-WDR801-DATA                         
128300                                                                          
128400       PERFORM IMS-ISRT-WDR801                                            
128500                                                                          
128600       IF SEGMENT-FINNS-REDAN                                             
128700         ADD +1                  TO WS-IDSEKVNR                           
128800         MOVE WS-IDSEKVNR        TO FIL-IDSEKVNR                          
128900         PERFORM IMS-ISRT-WDR801                                          
128910       END-IF                                                             
129100                                                                          
129200     .                                                                    
129300     EJECT                                                                
129400 GB-KONVERTERA-DATUM  SECTION.                                            
129500                                                                          
129600     MOVE 'GB-KONVERTERA-DATUM' TO WS-SEKTION                             
129700                                                                          
129800     MOVE ALL '+'              TO MSGI-WMSGINIT                           
129900     MOVE '001'                TO MSGI-KDCALL                             
130000     MOVE WS-IDUSER            TO MSGI-IDUSER                             
130100                                                                          
130200     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
130300                                                                          
130400     MOVE MSGI-TILOKDAT        TO LOKAL-DATUM                             
130500                                                                          
130600     IF LOKAL-AAR < 50                                                    
130700       MOVE 20                 TO LOKAL-SEKEL                             
130800     ELSE                                                                 
130900       MOVE 19                 TO LOKAL-SEKEL                             
131000     END-IF                                                               
131100                                                                          
131200     .                                                                    
131300     EJECT                                                                
131400 GC-UPPDATERA-PRMATRL   SECTION.                                          
131500                                                                          
131600     MOVE 'GC-UPPDATERA-PRMATRL' TO WS-SEKTION                            
131700                                                                          
131800     IF W-PM-UPPDAT                                                       
131900       MOVE LART-PRMATRL TO WS-PRMATRL-OLD                                
132000       MOVE WS-PRMATRL   TO LART-PRMATRL                                  
132100       PERFORM IMS-REPL-WDK712                                            
132200       IF SEGMENT-SAKNAS                                                  
132300         MOVE ERR-NO-UPDATE-DONE     TO MED-IDMFSINF                      
132400         CALL WMEDKONV            USING MED-WMEDAREA                      
132500         MOVE MED-MFSINF             TO MOD-TEMFSINF                      
132600       ELSE                                                               
132700         IF WS-PRMATRL-OLD = WS-PRMATRL                                   
132800           CONTINUE                                                       
132900         ELSE                                                             
133000           PERFORM S-SEND-TO-DAP                                          
133100         END-IF                                                           
133200         MOVE INF-UPDATE-DONE        TO MED-IDMFSINF                      
133300         CALL WMEDKONV            USING MED-WMEDAREA                      
133400         MOVE MED-MFSINF             TO MOD-TEMFSINF                      
133500       END-IF                                                             
133600     END-IF                                                               
133700     .                                                                    
133800     EJECT                                                                
133900 H-UPPDATERA-WDR8 SECTION.                                                
134000                                                                          
134100     MOVE 'H-UPPDATERA   ' TO WS-SEKTION                                  
134200                                                                          
134300     MOVE SPACE            TO EKH-W510EKHA                                
134400                                                                          
134500     MOVE 'W5020100'           TO FIL-IDPGM                               
134600     MOVE DAGENS-DATUM         TO FIL-TIREGDAT                            
134700     MOVE DAGENS-TID           TO FIL-TIKLOCK                             
134800     ADD +1                    TO WS-IDSEKVNR                             
134900     MOVE WS-IDSEKVNR          TO FIL-IDSEKVNR                            
135000                                                                          
135100     MOVE W-IDARTNR        TO EKH-IDARTNR                                 
135200     MOVE '401'            TO EKH-KDEKHHT                                 
135300     MOVE '420'            TO EKH-KDEKSHT                                 
135400     MOVE 'DET'            TO EKH-KDEKNIVA                                
135500     MOVE DCS-IDDC         TO EKH-IDDC-SEND                               
135600                              EKH-IDDC-REC                                
135700     MOVE +0               TO EKH-IDDISTR                                 
135800     MOVE +0               TO EKH-IDKUNDNR                                
135900                                                                          
136000     MOVE 'VO'             TO CIA-IDARTPRE-IN                             
136100     MOVE W-IDARTNR        TO CIA-IDARTBET-IN                             
136200     CALL W009CIA USING       CIA-W009CIA                                 
136300     MOVE CIA-IDARTBET-UT  TO EKH-IDVERGL                                 
136400                                                                          
136500     MOVE DAGENS-DAT       TO EKH-DAVERDAT                                
136600     MOVE ART-KDPRODSL     TO EKH-KDPRODSL                                
136700     MOVE ZERO             TO EKH-KDPSLLOC                                
136800     MOVE SPACE            TO EKH-FLLSBOK                                 
136900     MOVE 1.00             TO EKH-PRKURS                                  
137000     MOVE ZERO             TO EKH-PRARTNTO                                
137100     MOVE ZERO             TO EKH-PRARTSJK                                
137200     MOVE ZERO             TO EKH-PRHEMTAG                                
137300     COMPUTE EKH-PRARTSTD =                                               
137400             SLAG-PRAVCOST - WS-PRAVCOST-OLD                              
137500     END-COMPUTE                                                          
137600     MOVE ZERO             TO EKH-PRLANDCO                                
137700     MOVE ZERO             TO EKH-PRINK                                   
137800     MOVE ZERO             TO EKH-PRDIRLON                                
137900     MOVE ZERO             TO EKH-PRDMTRL                                 
138000     MOVE ZERO             TO EKH-PROVRPAL                                
138100     MOVE ZERO             TO EKH-SUBEL                                   
138200     MOVE WS-KVDISP        TO EKH-KVANTAL                                 
138300     MOVE '5201'           TO EKH-IDTRANS                                 
138400     MOVE ZERO                   TO EKH-BEVAT                             
138500                                    EKH-IDANALYS                          
138600                                    EKH-IDKONTO                           
138700                                    EKH-KDANMORS                          
138800                                    EKH-KDFRAKT                           
138900                                    EKH-SUVAT                             
139000     MOVE ZERO                   TO EKH-DAAVIDAT                          
139100                                    EKH-IDAVINR                           
139200                                    EKH-KDAVVTYP                          
139300                                    EKH-KDRT                              
139400                                    EKH-KVANTMOT                          
139500                                    EKH-KVAVIS                            
139600     MOVE ART-KDSORT             TO EKH-KDSORT                            
139700     MOVE SPACE                  TO EKH-IDLEVNR                           
139800                                    EKH-IDKST                             
139900     MOVE SPACE                  TO EKH-FLDCET                            
140000     MOVE SPACE                  TO EKH-IDKUNDRF                          
140100     MOVE SPACE                  TO EKH-IDFAKT-EXP                        
140200     MOVE DCS-KDVALISO           TO EKH-KDVALISO                          
140300     MOVE DCS-KDTRADP            TO EKH-KDTRADP                           
140400     IF DCS-NDC-CN                                                        
140500       MOVE 'W570'               TO FIL-IDCPYTXT(1:4)                     
140600     ELSE                                                                 
140700       IF DCS-INDIA                                                       
140800         MOVE 'W515'             TO FIL-IDCPYTXT(1:4)                     
140900       ELSE                                                               
141000         MOVE DCS-KDTRADP        TO FIL-IDCPYTXT(1:4)                     
141100       END-IF                                                             
141200     END-IF                                                               
141300     MOVE 'EKHA'                 TO FIL-IDCPYTXT(5:4)                     
141400                                                                          
141500     MOVE EKH-W510EKHA           TO FIL-WDR801-DATA                       
141600                                                                          
141700     IF EKH-KVANTAL = ZERO                                                
141800       CONTINUE                                                           
141900     ELSE                                                                 
141920      IF  W-UPPDATE-TOTAL  = JA                                           
142000       PERFORM IMS-ISRT-WDR801                                            
142100       IF SEGMENT-FINNS-REDAN                                             
142200         ADD +1                  TO WS-IDSEKVNR                           
142300         MOVE WS-IDSEKVNR        TO FIL-IDSEKVNR                          
142400         PERFORM IMS-ISRT-WDR801                                          
142500       END-IF                                                             
142510      END-IF                                                              
142600     END-IF                                                               
142700     .                                                                    
142800     EJECT                                                                
142900 S-SEND-TO-DAP SECTION.                                                   
143000*****************************************************************         
143100*UPDATE  PARAMETERS*****                                                  
143200****************************************************                      
143300     MOVE 001                        TO ERROR-REQU-IDMSGVER               
143400     MOVE 'R'                        TO ERROR-REQU-KDPGMACT               
143500     MOVE IDPGM                      TO ERROR-REQU-IDUSER                 
143600                                                                          
143700     MOVE 'W50201-001'               TO HDR-IDOUTTYPE                     
143800     MOVE SPACE                      TO HDR-IDOUTREC                      
143900                                                                          
144000     MOVE W-IDLANDX2-B6-X            TO HDR-IDOUTREC                      
144100     MOVE 'W50201'                   TO HDR-IDLIST                        
144200                                                                          
144300     MOVE ' PART NO : '              TO DAP-LINE-TEXT-1                   
144400     MOVE WS-IDARTNR                 TO DAP-LINE-IDARTNR-1                
144500     MOVE ' COUNTRY CODE : '         TO DAP-LINE-TEXT-2                   
144600     MOVE W-IDLANDX2-B6-X            TO DAP-LINE-IDLANDX2-2               
144700     MOVE ' CURRENT MATERIAL PRICE : '                                    
144800                                     TO DAP-LINE-TEXT-3                   
144900     MOVE WS-PRMATRL-OLD             TO DAP-LINE-PRMATRL-OLD              
145000     MOVE ' MODIFIED MATERIAL PRICE : '                                   
145100                                     TO DAP-LINE-TEXT-4                   
145200     MOVE WS-PRMATRL                 TO DAP-LINE-PRMATRL-NEW              
145300     MOVE ' USER ID : '              TO DAP-LINE-TEXT-5                   
145400     MOVE SPAR-IDUSER                TO DAP-LINE-USER                     
145500     MOVE ' DATE : '                 TO DAP-LINE-TEXT-6                   
145600     MOVE WS-DAGENS-DAT-FMT          TO DAP-LINE-DATE                     
145700     MOVE ' TIME : '                 TO DAP-LINE-TEXT-7                   
145800     MOVE WS-DAGENS-TID-FMT          TO DAP-LINE-SWEDE-TIME               
145900                                                                          
146000                                                                          
146100*******SENDING MATERIAL PRICE CHANGE TO DAP                               
146200     PERFORM S90-SEND-OPEN                                                
146300     PERFORM S90-PUT-HEADER                                               
146400     PERFORM S90-PUT-LINE                                                 
146500     PERFORM S90-SEND-CLOSE                                               
146600     .                                                                    
146700 S90-SEND-OPEN SECTION.                                                   
146800     MOVE 'CARPARTS.DAP.DISTRDOC'         TO SEND-ADDISPABS               
146900     MOVE 'OPEN'                          TO SEND-KDFUNC                  
147000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
147100                         SEND-OPEN-AREA                                   
147200     IF SEND-KDRC > ZERO                                                  
147300       MOVE SEND-KDRC                     TO WS-KDRC                      
147400       STRING 'WZ01SEND OPEN ERROR RC=' WS-KDRC                           
147500       DELIMITED BY SIZE INTO FELTEXT                                     
147600       CALL FELLOG                                                        
147700     END-IF                                                               
147800     .                                                                    
147900     EJECT                                                                
148000                                                                          
148100                                                                          
148200 S90-PUT-HEADER SECTION.                                                  
148300     MOVE 'PUT'                           TO SEND-KDFUNC                  
148400     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
148500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
148600                         SEND-KVDLEN                                      
148700                         HDR-AREA                                         
148800     IF SEND-KDRC > ZERO                                                  
148900       MOVE SEND-KDRC                     TO WS-KDRC                      
149000       STRING 'WZ01SEND PUT ERROR RC=' WS-KDRC                            
149100       DELIMITED BY SIZE INTO FELTEXT                                     
149200       CALL FELLOG                                                        
149300     END-IF                                                               
149400     .                                                                    
149500     EJECT                                                                
149600                                                                          
149700 S90-PUT-LINE SECTION.                                                    
149800     MOVE 'PUT'                           TO SEND-KDFUNC                  
149900     MOVE LENGTH OF DAP-LINE1-AREA        TO SEND-KVDLEN                  
150000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
150100                         SEND-KVDLEN                                      
150200                         DAP-LINE1-AREA                                   
150300     IF SEND-KDRC > ZERO                                                  
150400       MOVE SEND-KDRC                     TO WS-KDRC                      
150500       STRING 'WZ01SEND PUT ERROR RC=' WS-KDRC                            
150600       DELIMITED BY SIZE INTO FELTEXT                                     
150700       CALL FELLOG                                                        
150800     END-IF                                                               
150900     MOVE LENGTH OF DAP-LINE2-AREA        TO SEND-KVDLEN                  
151000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
151100                         SEND-KVDLEN                                      
151200                         DAP-LINE2-AREA                                   
151300     IF SEND-KDRC > ZERO                                                  
151400       MOVE SEND-KDRC                     TO WS-KDRC                      
151500       STRING 'WZ01SEND PUT ERROR RC=' WS-KDRC                            
151600       DELIMITED BY SIZE INTO FELTEXT                                     
151700       CALL FELLOG                                                        
151800     END-IF                                                               
151900     MOVE LENGTH OF DAP-LINE3-AREA        TO SEND-KVDLEN                  
152000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
152100                         SEND-KVDLEN                                      
152200                         DAP-LINE3-AREA                                   
152300     IF SEND-KDRC > ZERO                                                  
152400       MOVE SEND-KDRC                     TO WS-KDRC                      
152500       STRING 'WZ01SEND PUT ERROR RC=' WS-KDRC                            
152600       DELIMITED BY SIZE INTO FELTEXT                                     
152700       CALL FELLOG                                                        
152800     END-IF                                                               
152900     MOVE LENGTH OF DAP-LINE4-AREA        TO SEND-KVDLEN                  
153000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
153100                         SEND-KVDLEN                                      
153200                         DAP-LINE4-AREA                                   
153300     IF SEND-KDRC > ZERO                                                  
153400       MOVE SEND-KDRC                     TO WS-KDRC                      
153500       STRING 'WZ01SEND PUT ERROR RC=' WS-KDRC                            
153600       DELIMITED BY SIZE INTO FELTEXT                                     
153700       CALL FELLOG                                                        
153800     END-IF                                                               
153900     MOVE LENGTH OF DAP-LINE5-AREA        TO SEND-KVDLEN                  
154000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
154100                         SEND-KVDLEN                                      
154200                         DAP-LINE5-AREA                                   
154300     IF SEND-KDRC > ZERO                                                  
154400       MOVE SEND-KDRC                     TO WS-KDRC                      
154500       STRING 'WZ01SEND PUT ERROR RC=' WS-KDRC                            
154600       DELIMITED BY SIZE INTO FELTEXT                                     
154700       CALL FELLOG                                                        
154800     END-IF                                                               
154900     MOVE LENGTH OF DAP-LINE6-AREA        TO SEND-KVDLEN                  
155000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
155100                         SEND-KVDLEN                                      
155200                         DAP-LINE6-AREA                                   
155300     IF SEND-KDRC > ZERO                                                  
155400       MOVE SEND-KDRC                     TO WS-KDRC                      
155500       STRING 'WZ01SEND PUT ERROR RC=' WS-KDRC                            
155600       DELIMITED BY SIZE INTO FELTEXT                                     
155700       CALL FELLOG                                                        
155800     END-IF                                                               
155900     MOVE LENGTH OF DAP-LINE7-AREA        TO SEND-KVDLEN                  
156000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
156100                         SEND-KVDLEN                                      
156200                         DAP-LINE7-AREA                                   
156300     IF SEND-KDRC > ZERO                                                  
156400       MOVE SEND-KDRC                     TO WS-KDRC                      
156500       STRING 'WZ01SEND PUT ERROR RC=' WS-KDRC                            
156600       DELIMITED BY SIZE INTO FELTEXT                                     
156700       CALL FELLOG                                                        
156800     END-IF                                                               
156900     .                                                                    
157000     EJECT                                                                
157100                                                                          
157200 S90-SEND-CLOSE SECTION.                                                  
157300     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
157400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
157500     .                                                                    
157600     EJECT                                                                
157700                                                                          
157800 MFS-RENSA-FAELT-UT SECTION.                                              
157900                                                                          
158000*    --- ALLA UTDATA-FÄLT                                                 
158100     MOVE MFS-RENSA-FAELT TO MOD-BEART                                    
158200                             MOD-KDPSLLOC                                 
158300                             MOD-KDPRODSL                                 
158400                             MOD-KVAKS-PAV                                
158500                             MOD-KVAKS-SDC                                
158600                             MOD-KVLS                                     
158700                             MOD-KVEFRS                                   
158800                             MOD-SULAGVDE                                 
158900                             MOD-PRAVCOST                                 
159000                             MOD-TIAVCOST                                 
159100                             MOD-PRMATRL                                  
159200     .                                                                    
159300     EJECT                                                                
159400 MFS-RENSA-FAELT-IN SECTION.                                              
159500*    --- ALLA INDATA-FÄLT                                                 
159600                                                                          
159700     MOVE MFS-RENSA-FAELT   TO MOD-PRAVCOST-IN                            
159800                               MOD-SULAGVDE-IN                            
159900                               MOD-KDAVCOST-IN                            
160000                               MOD-IDFS-IN                                
160100                               MOD-PRMATRL-IN                             
160200     .                                                                    
160300     EJECT                                                                
160400 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
160500                                                                          
160600*    --- ALLA UTDATA-FÄLT                                                 
160700     MOVE MFS-ROER-EJ-FAELT TO MOD-BEART                                  
160800                               MOD-KDPSLLOC                               
160900                               MOD-KDPRODSL                               
161000                               MOD-KVAKS-PAV                              
161100                               MOD-KVAKS-SDC                              
161200                               MOD-KVLS                                   
161300                               MOD-KVEFRS                                 
161400                               MOD-SULAGVDE                               
161500                               MOD-PRAVCOST                               
161600                               MOD-TIAVCOST                               
161700                               MOD-PRMATRL                                
161800                               MOD-PRAVCOST-IN                            
161900                               MOD-SULAGVDE-IN                            
162000                               MOD-KDAVCOST-IN                            
162100                               MOD-IDFS-IN                                
162200                               MOD-PRMATRL-IN                             
162300     .                                                                    
162400     EJECT                                                                
162500 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
162600                                                                          
162700*    --- ALLA INDATA-FÄLT                                                 
162800     MOVE MFS-ROER-EJ-FAELT TO  MOD-PRAVCOST-IN                           
162900                                MOD-SULAGVDE-IN                           
163000                                MOD-KDAVCOST-IN                           
163100                                MOD-IDFS-IN                               
163200                                MOD-PRMATRL-IN                            
163300     .                                                                    
163400     SKIP2                                                                
163500 MFS-LAES-IN-IGEN SECTION.                                                
163600                                                                          
163700*    --- ALLA INDATA-FÄLT                                                 
163800     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-PRAVCOST-IN-ATTR                   
163900                                   MOD-SULAGVDE-IN-ATTR                   
164000                                   MOD-KDAVCOST-IN-ATTR                   
164100                                   MOD-IDFS-IN-ATTR                       
164200                                   MOD-PRMATRL-IN-ATTR                    
164300     .                                                                    
164400     SKIP2                                                                
164500 MFS-OEPPNA-FAELT-INDATA  SECTION.                                        
164600                                                                          
164700*    --- ALLA INDATA-FÄLT                                                 
164800     MOVE MFS-OEPPNA-NUM-FAELT  TO  MOD-PRAVCOST-IN-ATTR                  
164900                                    MOD-SULAGVDE-IN-ATTR                  
165000                                    MOD-PRMATRL-IN-ATTR                   
165100     MOVE MFS-OEPPNA-ALFA-FAELT TO  MOD-KDAVCOST-IN-ATTR                  
165200     MOVE MFS-OEPPNA-ALFA-FAELT TO  MOD-IDFS-IN-ATTR                      
165300     .                                                                    
165400     EJECT                                                                
165500* --- IMS SEKTIONER ---                                                   
165600     SKIP2                                                                
165700 IMS-GET-MSG SECTION.                                                     
165800                                                                          
165900     MOVE '  QC' TO GODK-STATUSKODER                                      
166000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
166100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
166200     PERFORM IMS-STATUSKONTROLL                                           
166300     .                                                                    
166400     SKIP3                                                                
166500 IMS-INSERT-MSG SECTION.                                                  
166600                                                                          
166700     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
166800        MOVE '0' TO MFS-KDHUVOMR                                          
166900     END-IF                                                               
167000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
167100     MOVE SPACE TO GODK-STATUSKODER                                       
167200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
167300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
167400     PERFORM IMS-STATUSKONTROLL                                           
167500     .                                                                    
167600     EJECT                                                                
167700 IMS-GU-WDK601 SECTION.                                                   
167800                                                                          
167900     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
168000          DELIMITED BY SIZE INTO SSA1                                     
168100     MOVE '  GE' TO GODK-STATUSKODER                                      
168200     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
168300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
168400     PERFORM IMS-STATUSKONTROLL                                           
168500     .                                                                    
168600     SKIP3                                                                
168700 IMS-GU-WDK701 SECTION.                                                   
168800                                                                          
168900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
169000          DELIMITED BY SIZE INTO SSA1                                     
169100     MOVE '  GE' TO GODK-STATUSKODER                                      
169200     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
169300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
169400     PERFORM IMS-STATUSKONTROLL                                           
169500     .                                                                    
169600     SKIP3                                                                
169700 IMS-GNP-WDK611 SECTION.                                                  
169800                                                                          
169900     MOVE 'WDK611   '      TO SSA1                                        
170000     MOVE '  GE' TO GODK-STATUSKODER                                      
170100     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
170200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
170300     PERFORM IMS-STATUSKONTROLL                                           
170400     .                                                                    
170500     EJECT                                                                
170600 IMS-GNP-WDK711 SECTION.                                                  
170700                                                                          
170800     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
170900     DELIMITED BY SIZE INTO SSA1                                          
171000     MOVE '  GE' TO GODK-STATUSKODER                                      
171100     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
171200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
171300     PERFORM IMS-STATUSKONTROLL                                           
171400     .                                                                    
171500     EJECT                                                                
171600 IMS-GU-WDK712 SECTION.                                                   
171700                                                                          
171800     MOVE SPACE               TO SSA1 SSA2                                
171900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
172000          DELIMITED BY SIZE INTO SSA1                                     
172100     STRING 'WDK712  (IDLAND   =' W-IDLANDX2-B6-X ')'                     
172200          DELIMITED BY SIZE INTO SSA2                                     
172300     MOVE '  GE' TO GODK-STATUSKODER                                      
172400     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2               
172500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
172600     PERFORM IMS-STATUSKONTROLL                                           
172700     .                                                                    
172800     EJECT                                                                
172900 IMS-GHNP-WDK711 SECTION.                                                 
173000                                                                          
173100     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
173200     DELIMITED BY SIZE INTO SSA1                                          
173300     MOVE '  GE' TO GODK-STATUSKODER                                      
173400     CALL CBLTDLI USING GHNP WDK7-PCB DLI-IO-WDK711 SSA1                  
173500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
173600     PERFORM IMS-STATUSKONTROLL                                           
173700     .                                                                    
173800     EJECT                                                                
173900 IMS-GHU-WDK712 SECTION.                                                  
174000                                                                          
174100     MOVE SPACE               TO SSA1 SSA2                                
174200     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
174300          DELIMITED BY SIZE INTO SSA1                                     
174400     STRING 'WDK712  (IDLAND   =' W-IDLANDX2-B6-X ')'                     
174500          DELIMITED BY SIZE INTO SSA2                                     
174600     MOVE '  GE' TO GODK-STATUSKODER                                      
174700     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2              
174800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
174900     PERFORM IMS-STATUSKONTROLL                                           
175000     .                                                                    
175100     EJECT                                                                
175200 IMS-REPL-WDK711 SECTION.                                                 
175300                                                                          
175400     MOVE '    ' TO GODK-STATUSKODER                                      
175500     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
175600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
175700     PERFORM IMS-STATUSKONTROLL                                           
175800     .                                                                    
175900     EJECT                                                                
176000 IMS-REPL-WDK712 SECTION.                                                 
176100                                                                          
176200     MOVE '    ' TO GODK-STATUSKODER                                      
176300     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK712                       
176400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
176500     PERFORM IMS-STATUSKONTROLL                                           
176600     .                                                                    
176700     EJECT                                                                
176800 IMS-GU-WDD3 SECTION.                                                     
176900                                                                          
177000     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
177100          DELIMITED BY SIZE INTO SSA1                                     
177200     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
177300          DELIMITED BY SIZE INTO SSA2                                     
177400     MOVE '  GE' TO GODK-STATUSKODER                                      
177500     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
177600     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
177700     PERFORM IMS-STATUSKONTROLL                                           
177800     .                                                                    
177900     EJECT                                                                
178000 IMS-ISRT-WDR801  SECTION.                                                
178100                                                                          
178200     MOVE 'WDR801   ' TO SSA1                                             
178300     MOVE '  II' TO GODK-STATUSKODER                                      
178400     CALL CBLTDLI USING ISRT WDR8-PCB DLI-IO-WDR801 SSA1                  
178500     MOVE WDR8-STATUS-CODE TO STATUS-WS                                   
178600     PERFORM IMS-STATUSKONTROLL                                           
178700     .                                                                    
178800     EJECT                                                                
178900 IMS-GU-WDB601    SECTION.                                                
179000     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
179100          DELIMITED BY SIZE INTO SSA1                                     
179200     MOVE '  GE' TO GODK-STATUSKODER                                      
179300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
179400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
179500     PERFORM IMS-STATUSKONTROLL                                           
179600     IF SEGMENT-SAKNAS                                                    
179700         MOVE SPACE TO DCS-KDDC                                           
179800     END-IF                                                               
179900     .                                                                    
180000 IMS-STATUSKONTROLL SECTION.                                              
180100                                                                          
180200     SET STATUS-IX TO 1                                                   
180300     SEARCH GODK-STATUS                                                   
180400       AT END                                                             
180500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
180600         DELIMITED BY SIZE INTO FELTEXT                                   
180700         CALL FELLOG                                                      
180800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
180900         CONTINUE                                                         
181000     END-SEARCH                                                           
181100     .                                                                    
