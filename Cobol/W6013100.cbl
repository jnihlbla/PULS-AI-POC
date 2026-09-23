000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6013100.                                                
000400* UTHOR.         KATARINA KYMMER.                                         
000500*DATE-WRITTEN.   92/05/11.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000810*                                                                         
000900*    FUNKTION:                                                            
001000*    MPP SOM ANVÄNDS FÖR ATT TA REDA PÅ VAD SOM FINNS PÅ EN VAGN          
001100*    SAMT ATT TÖMMA VAGNEN TILL EN VISS PLACERING.                        
001200*                                                                         
001300*        PROGRAMMET LÄSER      W6PLAA (W6G1)                              
001400*        PROGRAMMET LÄSER      W6INLA (W6D1)                              
001500*        PROGRAMMET LÄSER      WDK6                                       
001600*        PROGRAMMET LÄSER      WDK7                                       
001700*        PROGRAMMET LÄSER      WDB6                                       
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSAKTION: W6T131                                              
002100*        MID:         W6I13101                                            
002200*                                                                         
002300*    UTDATA.                                                              
002400*        MOD:         W6O13101                                            
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000 WORKING-STORAGE SECTION.                                                 
003100                                                                          
003200*    -- CHECKED BY WY2000                                                 
003300 77  IDPGM                       PIC X(08)   VALUE 'W6013100'.            
003400                                                                          
003500*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003600 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003700                                                                          
003800 77  JA                          PIC X       VALUE 'J'.                   
003900 77  NEJ                         PIC X       VALUE 'N'.                   
004000                                                                          
004100*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004200 77  IX                          PIC S9(4)  VALUE +0    COMP SYNC.        
004300 77  TRANS-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004400 77  MAX-KVPOST                  PIC S9(9)  VALUE +24   COMP SYNC.        
004500 77  KVAVIS-IX                   PIC S9(4)  VALUE +0    COMP SYNC.        
004600 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004700 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004800 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +930 COMP SYNC.         
004900                                                                          
005000 77  P-TO-P-PREFIX-LNG           PIC S9(4)  VALUE +17   COMP SYNC.        
005100 77  MID-6191-FASTDEL-LNG        PIC S9(4)  VALUE +17   COMP SYNC.        
005200 77  MID-6191-UPPF-POST-LNG      PIC S9(4)  VALUE +64   COMP SYNC.        
005300                                                                          
005400*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005500                                                                          
005600                                                                          
005700 77  LAES-NEXT                   PIC X       VALUE 'N'.                   
005800 77  LAES-ENTER                  PIC X       VALUE 'N'.                   
005900 77  RAD-BRYTN                   PIC X       VALUE 'J'.                   
006000 77  SKICKAT-TRANS               PIC X       VALUE 'N'.                   
006100 77  VAGN-KOLL                   PIC X       VALUE 'N'.                   
006200 77  UPPD-OK                     PIC X       VALUE 'N'.                   
006300 77  NYA-NYCKLAR                 PIC X       VALUE 'N'.                   
006400 77  WS-IDINLVGN                 PIC X(3)    VALUE SPACE.                 
006500 77  WS-IDLOPNRM-NEXT            PIC 9(9).                                
006600 77  WS-IDRADNR-NEXT             PIC 9(4).                                
006700 77  WS-IDLOPNRM-ENTER           PIC 9(9).                                
006800 77  WS-IDRADNR-ENTER            PIC 9(4).                                
006900 77  WW-IDLOPNRM-ENTER           PIC 9(9).                                
007000 77  WW-IDRADNR-ENTER            PIC 9(4).                                
007100 77  WS-ADINLOMR                 PIC X(4).                                
007200 77  WS-ADINLOMR-NXT             PIC X(4).                                
007300 77  WS-KDINLQ                   PIC X.                                   
007400 77  WS-BEFT-FOM                 PIC X(2).                                
007500 77  WS-BEFT-TOM                 PIC X(2).                                
007600 77  WS-FLINLFB                  PIC X.                                   
007700 77  WS-IDLEVNR-KOLLI            PIC X(5).                                
007800 77  WS-IDOKOLLI                 PIC X(9).                                
007900 77  WS-IDLOPNRM                 PIC S9(9)   VALUE ZERO COMP-3.           
008000 77  WS-PRIM                     PIC S9(6).                               
008100 77  W-KVAVIS                    PIC 9(6).                                
008200 77  W-KVAVIS-KIT                PIC S9(6).                               
008300 77  WS-KVROS                    PIC S9(6).                               
008400                                                                          
008500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
008600     88  NYCKLAR-OK                          VALUE 'J'.                   
008700     88  NYCKLAR-FEL                         VALUE 'N'.                   
008800                                                                          
008900 77  ALLT-SW                     PIC X       VALUE 'J'.                   
009000     88  ALLT-OK                             VALUE 'J'.                   
009100                                                                          
009200 77  PRIM-CONTROL-SW             PIC X       VALUE 'N'.                   
009300     88  PRIM-CONTROL-YES                    VALUE 'J'.                   
009400     88  PRIM-CONTROL-NO                     VALUE 'N'.                   
009500                                                                          
009600 77  INDATA-SW                   PIC X       VALUE 'J'.                   
009700     88  INDATA-OK                           VALUE 'J'.                   
009800     88  INDATA-FEL                          VALUE 'N'.                   
009900                                                                          
010000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
010100     88  EGEN-MID                            VALUE '6131'.                
010200     88  GODK-MID                            VALUE '6131' '6132'          
010300                                                   '6133' '6134'          
010400                                                   '6135' '6136'          
010500                                                   '6137' '6138'          
010600                                                   '6139'.                
010700     88  HELP-MID                            VALUE '0551'.                
010800     EJECT                                                                
010900*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
011000 01  GENERELLA-SUBPROGRAM.                                                
011100     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
011200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011400     03  W611ADR                 PIC X(8)    VALUE 'W611ADR'.             
011500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
011600     EJECT                                                                
011700*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
011800*01 -COPY WMEDAREA                                                        
011900     SKIP3                                                                
012000 01  MESSAGE-CODES.                                                       
012100     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
012200     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
012300     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
012400     03  INF-TRYCK-PF11          PIC X(3)    VALUE '003'.                 
012500     03  INF-TRYCK-PF4           PIC X(3)    VALUE '081'.                 
012600     03  INF-UDAT-UTFORD         PIC X(3)    VALUE '101'.                 
012700     03  INF-MED-SAKNAS          PIC X(3)    VALUE '999'.                 
012800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
012900     03  ERR-OTILL-UPPD          PIC X(3)    VALUE '007'.                 
013000     03  ERR-FINNS-EJ            PIC X(3)    VALUE '010'.                 
013100     03  ERR-PF11-INGENDATA      PIC X(3)    VALUE '011'.                 
013200     03  ERR-KVAL-FEL            PIC X(3)    VALUE '189'.                 
013300     03  ERR-FEL-NYCKEL          PIC X(3)    VALUE '401'.                 
013400     03  ERR-KORR-UPPL           PIC X(3)    VALUE '001'.                 
013500     03  ERR-MED-SAKNAS          PIC X(3)    VALUE '999'.                 
013600     EJECT                                                                
013700 01  SPAR-AREA.                                                           
013800     03 SPAR-ADINLOMR            PIC X(4).                                
013900     03 SPAR-ADINLOMR-NXT        PIC X(4).                                
014000     03 SPAR-KDINLSTA            PIC X(3).                                
014100     03 SPAR-KVINLART            PIC S9(7).                               
014200     EJECT                                                                
014300*01  -COPY W611ADR                                                        
014400     EJECT                                                                
014500*01  -COPY WMSGINIT                                                       
014600     EJECT                                                                
014700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
014800*                                                                         
014900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
015000     SKIP3                                                                
015100*01  MID -COPY W6I13101                                                   
015200     EJECT                                                                
015300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
015400     SKIP3                                                                
015500*01  -COPY WMSGAREA                                                       
015600     EJECT                                                                
015700     03  MOD REDEFINES MSG-AREA.                                          
015800*      05  -COPY W6O13101                                                 
015900     EJECT                                                                
016000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
016100     SKIP3                                                                
016200*01  -COPY WMSGSNUF   -PRE  P-TO-P-                                       
016300     EJECT                                                                
016400*01  -COPY W6I19101   -PRE 6191-                                          
016500     EJECT                                                                
016600*01  -COPY WMFSAREA                                                       
016700     EJECT                                                                
016800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
016900*                                                                         
017000     EJECT                                                                
017100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017200     SKIP3                                                                
017300 01  NYCKLAR-TILL-DLI.                                                    
017400     03  W-W6GXKEY-6005-X.                                                
017500         05  W-6005-IDHTYP       PIC X(4)    VALUE '6005'.                
017600         05  W-6005-IDDC         PIC X(2)    VALUE SPACE.                 
017700         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
017800     03  W-W6GXKEY-6006-X.                                                
017900         05  W-6006-ADINLOMR     PIC X(4)    VALUE SPACE.                 
018000         05  FILLER              PIC X       VALUE LOW-VALUE.             
018100     03  W-W6D1FSEQ-X.                                                    
018200         05  W-IDINLVGN          PIC 9(3).                                
018300     03  W-IDINLVGN-X.                                                    
018400         05  W-VAGN              PIC X(3)    VALUE SPACE.                 
018500     03  W-IDDC-X.                                                        
018600         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
018700     03  W-IDARTNR-X.                                                     
018800         05  W-IDARTNR           PIC S9(9)   COMP-3 VALUE ZERO.           
018900     03  W-KDSEGKEY-X.                                                    
019000         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
019100     03  W-IDLOPNRM-X.                                                    
019200         05  W-IDLOPNRM          PIC S9(9)   COMP-3 VALUE ZERO.           
019300                                                                          
019400     03  W-IDDC-B6-X.                                                     
019500         05 W-IDDC-B6            PIC X(2).                                
019600                                                                          
019700     SKIP2                                                                
019800*    --- STATUS-KOD FRÅN IMS                                              
019900 01  STATUS-WS                   PIC XX.                                  
020000     88  SEGMENT-FINNS                       VALUE '  '.                  
020100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
020200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
020300     88  SEGMENT-SLUT                        VALUE 'GB'.                  
020400     SKIP2                                                                
020500 01  GODK-STATUSKODER.                                                    
020600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
020700     SKIP3                                                                
020800 01  SSA1                        PIC X(64).                               
020900 01  SSA2                        PIC X(64).                               
021000     EJECT                                                                
021100*    --- IMS FUNKTIONSKODER                                               
021200*01  -COPY W0003                                                          
021300     EJECT                                                                
021400*    ---  DLI INPUT-OUTPUT AREA                                           
021500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
021600     SKIP3                                                                
021700 01  DLI-IO-AREA.                                                         
021800     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
021900     SKIP3                                                                
022000     03  W6PLAA01 REDEFINES IO-AREA.                                      
022100*        05  -COPY W6GX01                                                 
022200     SKIP3                                                                
022300     03  W6PLAA11 REDEFINES IO-AREA.                                      
022400*        05  -COPY W6GX6006                                               
022500     SKIP3                                                                
022600 01  DLI-IO-AREA-INL01.                                                   
022700     03  W6INLA01.                                                        
022800*        05  -COPY W6D101                                                 
022900     EJECT                                                                
023000 01  DLI-IO-AREA-INLF11.                                                  
023100     03  W6INLF11.                                                        
023200*        05  -COPY W6D111                                                 
023300     EJECT                                                                
023400 01  DLI-IO-AREA-INL21.                                                   
023500     03  W6INLA21.                                                        
023600*        05  -COPY W6D121                                                 
023700     EJECT                                                                
023800 01  DLI-IO-AREA-WDK6.                                                    
023900     03  WDK611.                                                          
024000*        05  -COPY WDK611                                                 
024100     EJECT                                                                
024200 01  DLI-IO-AREA-WDK7.                                                    
024300     03  WDK711.                                                          
024400*        05  -COPY WDK711                                                 
024500     EJECT                                                                
024600 01  DLI-IO-AREA-UPFA01.                                                  
024700     03  W6UPFA01.                                                        
024800*        05  -COPY W6L101                                                 
024900     SKIP3                                                                
025000 01  DLI-IO-AREA-UPFA11.                                                  
025100     03  W6UPFA11.                                                        
025200*        05  -COPY W6L111                                                 
025300     SKIP3                                                                
025400 01  DLI-IO-AREA-UPFA12.                                                  
025500     03  W6UPFA12.                                                        
025600*        05  -COPY W6L112                                                 
025700     SKIP3                                                                
025800                                                                          
025900 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
026000 01   DLI-IO-AREA-B601.                                                   
026100*     03  -COPY WDB601                                                    
026200                                                                          
026300 LINKAGE SECTION.                                                         
026400                                                                          
026500*01  -COPY W0009   -PRE MSG-                                              
026600     EJECT                                                                
026700*01  -COPY W0009  -PRE 6191-                                              
026800     EJECT                                                                
026900*01  -COPY W0008  -PRE USEA-                                              
027000     05  FILLER                  PIC X.                                   
027100     EJECT                                                                
027200*01  -COPY W0008  -PRE PLAA-                                              
027300     05  FILLER                  PIC X.                                   
027400     EJECT                                                                
027500*01  -COPY W0008  -PRE INLG-                                              
027600     05  FILLER                  PIC X.                                   
027700     EJECT                                                                
027800*01  -COPY W0008  -PRE WDK6-                                              
027900     05  FILLER                  PIC X.                                   
028000     EJECT                                                                
028100*01  -COPY W0008  -PRE WDK7-                                              
028200     05  FILLER                  PIC X.                                   
028300     EJECT                                                                
028400*01  -COPY W0008  -PRE ADR-INLA-                                          
028500     05  FILLER                  PIC X.                                   
028600     EJECT                                                                
028700*01  -COPY W0008  -PRE ADR-INLC-                                          
028800     05  FILLER                  PIC X.                                   
028900     EJECT                                                                
029000*01  -COPY W0008  -PRE ADR-PLAA-                                          
029100     05  FILLER                  PIC X.                                   
029200     EJECT                                                                
029300*01  -COPY W0008  -PRE ADR-WDK6-                                          
029400     05  FILLER                  PIC X.                                   
029500     EJECT                                                                
029600*01  -COPY W0008  -PRE ADR-STYR-HANA-                                     
029700     05  FILLER                  PIC X.                                   
029800     EJECT                                                                
029900*01  -COPY W0008  -PRE ADR-STYR-PLAA-                                     
030000     05  FILLER                  PIC X.                                   
030100     EJECT                                                                
030200*01  -COPY W0008  -PRE UPFA-                                              
030300     05  FILLER                  PIC X.                                   
030400     EJECT                                                                
030500*01  -COPY W0008  -PRE WDB6-                                              
030600     05  FILLER                  PIC X.                                   
030700     EJECT                                                                
030800 PROCEDURE DIVISION  USING MSG-PCB                                        
030900                           6191-PCB                                       
031000                           USEA-PCB                                       
031100                           PLAA-PCB                                       
031200                           INLG-PCB                                       
031300                           WDK6-PCB                                       
031400                           WDK7-PCB                                       
031500                           ADR-INLA-PCB                                   
031600                           ADR-INLC-PCB                                   
031700                           ADR-PLAA-PCB                                   
031800                           ADR-WDK6-PCB                                   
031900                           ADR-STYR-HANA-PCB                              
032000                           ADR-STYR-PLAA-PCB                              
032100                           UPFA-PCB                                       
032200                           WDB6-PCB.                                      
032300     ENTRY 'DLITCBL' USING MSG-PCB                                        
032400                           6191-PCB                                       
032500                           USEA-PCB                                       
032600                           PLAA-PCB                                       
032700                           INLG-PCB                                       
032800                           WDK6-PCB                                       
032900                           WDK7-PCB                                       
033000                           ADR-INLA-PCB                                   
033100                           ADR-PLAA-PCB                                   
033200                           ADR-WDK6-PCB                                   
033300                           ADR-STYR-HANA-PCB                              
033400                           ADR-STYR-PLAA-PCB                              
033500                           UPFA-PCB                                       
033600                           WDB6-PCB.                                      
033700                                                                          
033800     PERFORM IMS-GET-MSG                                                  
033900     IF SEGMENT-FINNS                                                     
034000       PERFORM A-INIT                                                     
034100         PERFORM B-KOLLA-NYCKLAR                                          
034200         IF NYCKLAR-OK                                                    
034300          IF MFS-UPDATE                                                   
034400           IF NYA-NYCKLAR = JA                                            
034500             PERFORM C-LAES-VISA-INFO                                     
034600           ELSE                                                           
034700             MOVE WS-IDINLVGN TO W-IDINLVGN                               
034800             PERFORM IMS-GET-W6INLA-F                                     
034900             IF SEGMENT-SAKNAS                                            
035000               MOVE ERR-FINNS-EJ TO MED-IDMFSFEL                          
035100               CALL WMEDKONV USING MED-WMEDAREA                           
035200               MOVE MED-MFSFEL TO MOD-TEMFSFEL                            
035300             ELSE                                                         
035400               PERFORM D-UPPDATERA                                        
035500               PERFORM C-LAES-VISA-INFO                                   
035600             END-IF                                                       
035700           END-IF                                                         
035800          ELSE                                                            
035900            IF NYA-NYCKLAR = JA OR MFS-FIRST                              
036000              PERFORM C-LAES-VISA-INFO                                    
036100            ELSE                                                          
036200              IF MID-ADINLOMR-TOMN = ALL '+'  OR                          
036300                 MID-ADINLOMR-TOMN = SPACE                                
036400                PERFORM C-LAES-VISA-INFO                                  
036500                PERFORM F-MID-INDATA-TILL-MOD                             
036600              ELSE                                                        
036700                PERFORM E-TRYCK-PF11                                      
036800                IF HELP-MID                                               
036900                  PERFORM C-LAES-VISA-INFO                                
037000                  PERFORM F-MID-INDATA-TILL-MOD                           
037100                END-IF                                                    
037200              END-IF                                                      
037300            END-IF                                                        
037400          END-IF                                                          
037500         END-IF                                                           
037600         MOVE MAX-MOD-LAENGD TO MSG-KVLL                                  
037700         PERFORM IMS-INSERT-MSG                                           
037800     END-IF                                                               
037900                                                                          
038000     MOVE ZERO TO RETURN-CODE                                             
038100     GOBACK                                                               
038200     .                                                                    
038300     EJECT                                                                
038400                                                                          
038500 A-INIT SECTION.                                                          
038600     MOVE JA TO INDATA-SW                                                 
038700     MOVE JA TO NYCKLAR-SW                                                
038800     MOVE NEJ TO NYA-NYCKLAR                                              
038900     IF MSG-DUBBLA-TRANSKODER                                             
039000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I13101                 
039100       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
039200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
039300     ELSE                                                                 
039400       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I13101                  
039500       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
039600       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
039700     END-IF                                                               
039800                                                                          
039900     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
040000     MOVE MSG-IDPFK TO MFS-IDPFK                                          
040100     MOVE MFS-IDTRANS TO W-IDTRANS                                        
040200                                                                          
040300     MOVE LOW-VALUE TO MSG-AREA                                           
040400     MOVE 'W6O131N1' TO MFS-IDMOD                                         
040500     MOVE '6131' TO MOD-IDTRANS                                           
040600     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
040700                                                                          
040800     IF NOT EGEN-MID AND NOT HELP-MID                                     
040900       MOVE SPACE TO MFS-KDTRTYP                                          
041000       MOVE '7' TO MFS-IDPFK                                              
041100     END-IF                                                               
041200                                                                          
041300     PERFORM AA-INIT-NYCKLAR                                              
041400                                                                          
041500     IF MSGI-IDLAND-SPR = 'GB'                                            
041600       MOVE +2 TO SPRAK-IX                                                
041700       MOVE 'GB ' TO MED-IDSKYLT                                          
041800     ELSE                                                                 
041900       MOVE +1 TO SPRAK-IX                                                
042000       MOVE 'S  ' TO MED-IDSKYLT                                          
042100     END-IF                                                               
042200     .                                                                    
042300     EJECT                                                                
042400                                                                          
042500 AA-INIT-NYCKLAR SECTION.                                                 
042600     MOVE ALL '+'         TO MSGI-WMSGINIT                                
042700     MOVE '001'           TO MSGI-KDCALL                                  
042800     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
042900     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
043000     MOVE '6131'                 TO MSGI-IDTRANS                          
043100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
043200     .                                                                    
043300     EJECT                                                                
043400                                                                          
043500 B-KOLLA-NYCKLAR SECTION.                                                 
043600     MOVE MFS-RENSA-FAELT TO MOD-IDINLVGN-IN                              
043700     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
043800                                                                          
043900     IF MID-IDINLVGN-IN = ALL '+'                                         
044000       MOVE MID-IDINLVGN-UT TO WS-IDINLVGN                                
044100       INSPECT WS-IDINLVGN REPLACING LEADING SPACE BY ZERO                
044200     ELSE                                                                 
044300       MOVE MID-IDINLVGN-IN TO WS-IDINLVGN                                
044400       MOVE '7'             TO MFS-IDPFK                                  
044500       MOVE SPACE           TO MFS-KDTRTYP                                
044600       MOVE JA TO NYA-NYCKLAR                                             
044700     END-IF                                                               
044800                                                                          
044900                                                                          
045000     IF WS-IDINLVGN NUMERIC AND WS-IDINLVGN > ZERO                        
045100        MOVE WS-IDINLVGN TO W-IDINLVGN                                    
045200     ELSE                                                                 
045300        MOVE NEJ TO NYCKLAR-SW                                            
045400     END-IF                                                               
045500                                                                          
045600     IF MID-IDDC-IN     = ALL '+'                                         
045700       MOVE MSGI-IDDC       TO W-IDDC-B6                                  
045800     ELSE                                                                 
045900       MOVE MID-IDDC-IN     TO W-IDDC-B6                                  
046000       MOVE '7'             TO MFS-IDPFK                                  
046100       MOVE SPACE           TO MFS-KDTRTYP                                
046200       MOVE JA TO NYA-NYCKLAR                                             
046300     END-IF                                                               
046400     PERFORM IMS-GU-WDB601                                                
046500                                                                          
046600                                                                          
046700     IF DCS-KDDC = SPACE OR DCS-DDC                                       
046800        MOVE NEJ TO NYCKLAR-SW                                            
046900     ELSE                                                                 
047000        MOVE DCS-IDDC    TO W-IDDC                                        
047100                            W-6005-IDDC                                   
047200     END-IF                                                               
047300                                                                          
047400                                                                          
047500     IF GODK-MID OR HELP-MID                                              
047600       PERFORM BA-KOLLA-OEVRIGA-NYCKLAR                                   
047700     ELSE                                                                 
047800       MOVE MFS-RENSA-FAELT         TO MOD-ADINLOMR-PRT                   
047900                                       MOD-ADINLOMR-UT                    
048000                                       MOD-ADINLOMR-NXT-UT                
048100                                       MOD-KDINLQ-UT                      
048200                                       MOD-BEFT-FOM-UT                    
048300                                       MOD-BEFT-TOM-UT                    
048400                                       MOD-FLINLFB-UT                     
048500                                       MOD-IDLEVNR-KOLLI-UT               
048600                                       MOD-IDOKOLLI-UT                    
048700                                       MOD-IDLOPNRM-UT                    
048800     END-IF                                                               
048900                                                                          
049000     IF GODK-MID OR NYCKLAR-OK                                            
049100        MOVE WS-IDINLVGN TO MOD-IDINLVGN-UT                               
049200        INSPECT MOD-IDINLVGN-UT REPLACING LEADING ZERO BY SPACE           
049300        MOVE DCS-IDDC    TO MOD-IDDC-UT                                   
049400        INSPECT MOD-IDDC-UT REPLACING LEADING ZERO BY SPACE               
049500     ELSE                                                                 
049600        MOVE MFS-RENSA-FAELT TO MOD-IDDC-UT                               
049700     END-IF                                                               
049800                                                                          
049900                                                                          
050000     IF NYCKLAR-FEL                                                       
050100       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
050200       CALL WMEDKONV USING MED-WMEDAREA                                   
050300       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
050400       PERFORM  MFS-RENSA-RADFAELT                                        
050500     END-IF                                                               
050600     .                                                                    
050700     EJECT                                                                
050800                                                                          
050900 BA-KOLLA-OEVRIGA-NYCKLAR SECTION.                                        
051000     IF MID-ADINLOMR-PRT         = ALL '+'                                
051100       CONTINUE                                                           
051200     ELSE                                                                 
051300       MOVE MID-ADINLOMR-PRT    TO MOD-ADINLOMR-PRT                       
051400     END-IF                                                               
051500                                                                          
051600     MOVE MFS-RENSA-FAELT        TO  MOD-ADINLOMR-IN                      
051700     IF MID-ADINLOMR-IN          = ALL '+'                                
051800       MOVE MID-ADINLOMR-UT      TO WS-ADINLOMR                           
051900     ELSE                                                                 
052000       MOVE MID-ADINLOMR-IN      TO WS-ADINLOMR                           
052100     END-IF                                                               
052200     MOVE WS-ADINLOMR            TO MOD-ADINLOMR-UT                       
052300                                                                          
052400     MOVE MFS-RENSA-FAELT        TO  MOD-ADINLOMR-NXT-IN                  
052500     IF MID-ADINLOMR-NXT-IN      = ALL '+'                                
052600       MOVE MID-ADINLOMR-NXT-UT TO WS-ADINLOMR-NXT                        
052700     ELSE                                                                 
052800       MOVE MID-ADINLOMR-NXT-IN  TO WS-ADINLOMR-NXT                       
052900     END-IF                                                               
053000     MOVE WS-ADINLOMR-NXT        TO MOD-ADINLOMR-NXT-UT                   
053100                                                                          
053200     MOVE MFS-RENSA-FAELT        TO  MOD-KDINLQ-IN                        
053300     IF MID-KDINLQ-IN      = ALL '+'                                      
053400       MOVE MID-KDINLQ-UT TO WS-KDINLQ                                    
053500     ELSE                                                                 
053600       MOVE MID-KDINLQ-IN  TO WS-KDINLQ                                   
053700     END-IF                                                               
053800     MOVE WS-KDINLQ        TO MOD-KDINLQ-UT                               
053900                                                                          
054000     MOVE MFS-RENSA-FAELT        TO  MOD-BEFT-FOM-IN                      
054100     IF MID-BEFT-FOM-IN      = ALL '+'                                    
054200       MOVE MID-BEFT-FOM-UT TO WS-BEFT-FOM                                
054300     ELSE                                                                 
054400       MOVE MID-BEFT-FOM-IN  TO WS-BEFT-FOM                               
054500     END-IF                                                               
054600     MOVE WS-BEFT-FOM        TO MOD-BEFT-FOM-UT                           
054700                                                                          
054800     MOVE MFS-RENSA-FAELT        TO  MOD-BEFT-TOM-IN                      
054900     IF MID-BEFT-TOM-IN      = ALL '+'                                    
055000       MOVE MID-BEFT-TOM-UT TO WS-BEFT-TOM                                
055100     ELSE                                                                 
055200       MOVE MID-BEFT-TOM-IN  TO WS-BEFT-TOM                               
055300     END-IF                                                               
055400     MOVE WS-BEFT-TOM        TO MOD-BEFT-TOM-UT                           
055500                                                                          
055600     MOVE MFS-RENSA-FAELT        TO  MOD-FLINLFB-IN                       
055700     IF MID-FLINLFB-IN   = ALL '+'                                        
055800       MOVE MID-FLINLFB-UT TO WS-FLINLFB                                  
055900     ELSE                                                                 
056000       MOVE MID-FLINLFB-IN TO WS-FLINLFB                                  
056100     END-IF                                                               
056200     MOVE WS-FLINLFB     TO MOD-FLINLFB-UT                                
056300                                                                          
056400     MOVE MFS-RENSA-FAELT        TO  MOD-IDLEVNR-KOLLI-IN                 
056500     IF MID-IDLEVNR-KOLLI-IN   = ALL '+'                                  
056600       MOVE MID-IDLEVNR-KOLLI-UT TO WS-IDLEVNR-KOLLI                      
056700     ELSE                                                                 
056800       MOVE MID-IDLEVNR-KOLLI-IN TO WS-IDLEVNR-KOLLI                      
056900     END-IF                                                               
057000     MOVE WS-IDLEVNR-KOLLI TO MOD-IDLEVNR-KOLLI-UT                        
057100                                                                          
057200     MOVE MFS-RENSA-FAELT        TO  MOD-IDOKOLLI-IN                      
057300     IF MID-IDOKOLLI-IN   = ALL '+'                                       
057400       MOVE MID-IDOKOLLI-UT TO WS-IDOKOLLI                                
057500     ELSE                                                                 
057600       MOVE MID-IDOKOLLI-IN TO WS-IDOKOLLI                                
057700     END-IF                                                               
057800     MOVE WS-IDOKOLLI TO MOD-IDOKOLLI-UT                                  
057900                                                                          
058000     MOVE MFS-RENSA-FAELT        TO  MOD-IDLOPNRM-IN                      
058100     IF MID-IDLOPNRM-IN   = ALL '+'                                       
058200       MOVE MID-IDLOPNRM-UT TO MOD-IDLOPNRM-UT                            
058300     ELSE                                                                 
058400       MOVE MID-IDLOPNRM-IN TO MOD-IDLOPNRM-UT                            
058500     END-IF                                                               
058600     .                                                                    
058700     EJECT                                                                
058800                                                                          
058900 C-LAES-VISA-INFO SECTION.                                                
059000     PERFORM CA-TRANSAR                                                   
059100     IF INDATA-OK                                                         
059200       PERFORM CB-BEHANDLA                                                
059300     END-IF                                                               
059400     .                                                                    
059500     EJECT                                                                
059600                                                                          
059700 CA-TRANSAR SECTION.                                                      
059800     EVALUATE TRUE                                                        
059900       WHEN NYA-NYCKLAR = JA                                              
060000         MOVE WS-IDINLVGN TO W-IDINLVGN                                   
060100                             W-VAGN                                       
060200         PERFORM CAA-LAES                                                 
060300                                                                          
060400       WHEN MFS-FIRST AND NYA-NYCKLAR = NEJ                               
060500           MOVE WS-IDINLVGN TO W-IDINLVGN                                 
060600                               W-VAGN                                     
060700         PERFORM CAA-LAES                                                 
060800                                                                          
060900       WHEN MFS-NEXT AND NYA-NYCKLAR = NEJ                                
061000         MOVE WS-IDINLVGN TO W-IDINLVGN                                   
061100                             W-VAGN                                       
061200         IF MID-IDLOPNRM-NEXT = ALL '+'  OR SPACE                         
061300           MOVE MID-IDLOPNRM-ENTER TO WS-IDLOPNRM-ENTER                   
061400           MOVE MID-IDRADNR-ENTER TO WS-IDRADNR-ENTER                     
061500           PERFORM CAC-LAES-ENTER                                         
061600         ELSE                                                             
061700           MOVE MID-IDLOPNRM-NEXT TO WS-IDLOPNRM-NEXT                     
061800           MOVE MID-IDRADNR-NEXT  TO WS-IDRADNR-NEXT                      
061900           PERFORM CAB-LAES-PF8                                           
062000         END-IF                                                           
062100                                                                          
062200       WHEN MFS-ENTER AND NYA-NYCKLAR = NEJ                               
062300         MOVE WS-IDINLVGN TO W-IDINLVGN                                   
062400                             W-VAGN                                       
062500         MOVE MID-IDLOPNRM-ENTER TO  WS-IDLOPNRM-ENTER                    
062600         MOVE MID-IDRADNR-ENTER  TO  WS-IDRADNR-ENTER                     
062700         PERFORM CAC-LAES-ENTER                                           
062800                                                                          
062900       WHEN MFS-UPDATE                                                    
063000         MOVE WS-IDINLVGN TO W-IDINLVGN                                   
063100                             W-VAGN                                       
063200         PERFORM CAD-LAES-ENTER                                           
063300      END-EVALUATE                                                        
063400     .                                                                    
063500     EJECT                                                                
063600                                                                          
063700 CAA-LAES SECTION.                                                        
063800     PERFORM IMS-GET-W6INLA-F                                             
063900     IF SEGMENT-SAKNAS                                                    
064000       MOVE ERR-FINNS-EJ TO MED-IDMFSFEL                                  
064100       CALL WMEDKONV USING MED-WMEDAREA                                   
064200       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
064300       MOVE NEJ TO INDATA-SW                                              
064400     END-IF                                                               
064500     .                                                                    
064600     EJECT                                                                
064700                                                                          
064800 CAB-LAES-PF8 SECTION.                                                    
064900     PERFORM IMS-GET-W6INLA-F                                             
065000     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
065100                   SEGMENT-SLUT   OR                                      
065200                   WS-IDLOPNRM-NEXT = ART-IDLOPNRM                        
065300      PERFORM IMS-GN-W6INLA-F                                             
065400     END-PERFORM                                                          
065500     MOVE JA TO LAES-NEXT                                                 
065600     IF SEGMENT-FINNS                                                     
065700        CONTINUE                                                          
065710     ELSE                                                                 
065800        MOVE ERR-FINNS-EJ TO MED-IDMFSFEL                                 
065900        CALL WMEDKONV USING MED-WMEDAREA                                  
066000        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
066100        MOVE NEJ TO INDATA-SW                                             
066200     END-IF                                                               
066300                                                                          
066400     .                                                                    
066500     EJECT                                                                
066600                                                                          
066700 CAC-LAES-ENTER SECTION.                                                  
066800     PERFORM IMS-GET-W6INLA-F                                             
066900     IF SEGMENT-SAKNAS                                                    
067000       MOVE ERR-FINNS-EJ TO MED-IDMFSFEL                                  
067100       CALL WMEDKONV USING MED-WMEDAREA                                   
067200       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
067300       MOVE NEJ TO INDATA-SW                                              
067400     ELSE                                                                 
067500        PERFORM UNTIL  WS-IDLOPNRM-ENTER = ART-IDLOPNRM OR                
067600                       SEGMENT-SAKNAS OR                                  
067700                       SEGMENT-SLUT                                       
067800          PERFORM IMS-GN-W6INLA-F                                         
067900        END-PERFORM                                                       
068000        MOVE JA TO LAES-ENTER                                             
068100     END-IF                                                               
068200                                                                          
068300     .                                                                    
068400     EJECT                                                                
068500                                                                          
068600 CAD-LAES-ENTER SECTION.                                                  
068700     PERFORM IMS-GET-W6INLA-F                                             
068800     IF SEGMENT-SAKNAS                                                    
068900        MOVE NEJ TO INDATA-SW                                             
069000     ELSE                                                                 
069100        PERFORM UNTIL  WS-IDLOPNRM-ENTER = ART-IDLOPNRM OR                
069200                       SEGMENT-SAKNAS OR                                  
069300                       SEGMENT-SLUT                                       
069400          PERFORM IMS-GN-W6INLA-F                                         
069500        END-PERFORM                                                       
069600        MOVE JA TO LAES-ENTER                                             
069700     END-IF                                                               
069800                                                                          
069900     .                                                                    
070000     EJECT                                                                
070100                                                                          
070200 CB-BEHANDLA SECTION.                                                     
070300     MOVE 1 TO IX                                                         
070400     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
070500                   SEGMENT-SLUT OR                                        
070600                   IX > 12                                                
070700       IF ART-IDLOPNRM  NOT = WS-IDLOPNRM                                 
070800         MOVE IX TO KVAVIS-IX                                             
070900         PERFORM CBA-W6INLA11-RADEN                                       
071000         PERFORM CBB-W6INLA21-RADEN                                       
071100       END-IF                                                             
071200       PERFORM IMS-GN-W6INLA-F                                            
071300     END-PERFORM                                                          
071400                                                                          
071500     IF SEGMENT-FINNS                                                     
071600       PERFORM CBC-FLER-RADER                                             
071700     ELSE                                                                 
071800       MOVE WW-IDLOPNRM-ENTER TO MOD-IDLOPNRM-ENTER                       
071900       MOVE WW-IDRADNR-ENTER  TO MOD-IDRADNR-ENTER                        
072000       PERFORM UNTIL IX > 12                                              
072100       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR(IX)                            
072200                               MOD-KVINLART(IX)                           
072300                               MOD-KVAVIS-PRIO(IX)                        
072400                               MOD-BEFT(IX)                               
072500                               MOD-FLKVAANT-TOT(IX)                       
072600                               MOD-KVKVAPRIM(IX)                          
072700                               MOD-KVROS(IX)                              
072800                               MOD-KVAVIS-KIT(IX)                         
072900                               MOD-ADTRDEST-KIT(IX)                       
073000                               MOD-KDLAGEMB(IX)                           
073100                               MOD-ADINLOMR-NXT1(IX)                      
073200                               MOD-ADINLOMR-NXT2(IX)                      
073300                               MOD-ADINLOMR-NXT3(IX)                      
073400       ADD 1 TO IX                                                        
073500       END-PERFORM                                                        
073600        IF MFS-IDPFK = '8' AND NYA-NYCKLAR = NEJ                          
073700          MOVE INF-LAST-PAGE TO MED-IDMFSINF                              
073800          CALL WMEDKONV USING MED-WMEDAREA                                
073900          MOVE MED-MFSINF TO MOD-TEMFSINF                                 
074000        END-IF                                                            
074100     END-IF                                                               
074200     MOVE MFS-ROER-EJ-FAELT TO MOD-ADINLOMR-TOMN                          
074300     .                                                                    
074400     EJECT                                                                
074500                                                                          
074600 CBA-W6INLA11-RADEN SECTION.                                              
074700     IF IX = 1                                                            
074800       MOVE ART-IDLOPNRM TO WW-IDLOPNRM-ENTER                             
074900     END-IF                                                               
075000     MOVE ART-IDARTNR  TO MOD-IDARTNR(IX)                                 
075100     IF ART-FLKVAFEL = JA OR ART-FLKVAKAR = JA                            
075200       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDARTNR-ATTR(IX)                 
075300       IF MED-IDMFSFEL NOT = '605'                                        
075400         MOVE ERR-KVAL-FEL  TO MED-IDMFSFEL                               
075500         CALL WMEDKONV USING MED-WMEDAREA                                 
075600         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
075700       END-IF                                                             
075800     END-IF                                                               
075900     MOVE ART-BEFT         TO MOD-BEFT(IX)                                
076000     MOVE ART-KDLAGEMB     TO MOD-KDLAGEMB(IX)                            
076100     MOVE ART-ADTRDEST-KIT TO MOD-ADTRDEST-KIT(IX)                        
076200     MOVE ART-IDLOPNRM     TO WS-IDLOPNRM                                 
076300                                                                          
076400     MOVE ART-IDLOPNRM     TO ADR-IDLOPNRM                                
076500                                                                          
076600     CALL W611ADR USING ADR-W611ADR ADR-INLA-PCB ADR-INLC-PCB             
076700                        ADR-PLAA-PCB ADR-WDK6-PCB                         
076800                                     ADR-STYR-HANA-PCB                    
076900                                     ADR-STYR-PLAA-PCB                    
077000                                                                          
077100     MOVE ADR-ADINLOMR-NXT1 TO MOD-ADINLOMR-NXT1(IX)                      
077200     MOVE ADR-ADINLOMR-NXT2 TO MOD-ADINLOMR-NXT2(IX)                      
077300     MOVE ADR-ADINLOMR-NXT3 TO MOD-ADINLOMR-NXT3(IX)                      
077400                                                                          
077500     MOVE ART-KVAVIS-PRIO   TO MOD-KVAVIS-PRIO(IX)                        
077600                                                                          
077700     IF ART-KDKVAANT > ZERO                                               
077800        MOVE 'J' TO MOD-FLKVAANT-TOT(IX)                                  
077900     ELSE                                                                 
078000        MOVE 'N' TO MOD-FLKVAANT-TOT(IX)                                  
078100     END-IF                                                               
078200                                                                          
078300     COMPUTE WS-PRIM = ART-KVKVAPRIM-BER - ART-KVKVAPRIM-VER              
078400     IF WS-PRIM < ZERO                                                    
078500        MOVE ZERO TO MOD-KVKVAPRIM(IX)                                    
078600     ELSE                                                                 
078700        MOVE WS-PRIM TO MOD-KVKVAPRIM(IX)                                 
078800     END-IF                                                               
078900                                                                          
079000     MOVE ART-IDARTNR TO W-IDARTNR                                        
079100     MOVE ZERO        TO WS-KVROS                                         
079200     IF DCS-CDC                                                           
079300       PERFORM IMS-GU-WDK611                                              
079400       IF SEGMENT-FINNS                                                   
079500         MOVE CLAG-KVROS TO WS-KVROS                                      
079600       END-IF                                                             
079700     ELSE                                                                 
079800       PERFORM IMS-GU-WDK711                                              
079900       IF SEGMENT-FINNS                                                   
080000         COMPUTE WS-KVROS = SLAG-KVROS-DAG + SLAG-KVROS-BULK              
080100       END-IF                                                             
080200     END-IF                                                               
080300                                                                          
080400     MOVE WS-KVROS TO MOD-KVROS(IX)                                       
080500     .                                                                    
080600     EJECT                                                                
080700                                                                          
080800 CBB-W6INLA21-RADEN SECTION.                                              
080900       IF LAES-NEXT = JA                                                  
081000         PERFORM IMS-GNP-W6INLA21                                         
081100         PERFORM UNTIL WS-IDRADNR-NEXT = RAD-IDRADNR OR                   
081200                       SEGMENT-SAKNAS OR                                  
081300                       SEGMENT-SLUT                                       
081400           PERFORM IMS-GNP-W6INLA21                                       
081500         END-PERFORM                                                      
081600         MOVE NEJ TO LAES-NEXT                                            
081700         MOVE RAD-KVINLART TO MOD-KVINLART(IX)                            
081800         IF IX = 1                                                        
081900           MOVE RAD-IDRADNR  TO WW-IDRADNR-ENTER                          
082000         END-IF                                                           
082100         ADD +1 TO IX                                                     
082200         MOVE +0 TO W-KVAVIS                                              
082300         IF RAD-FLSATS = 'J'                                              
082400           ADD RAD-KVINLART TO W-KVAVIS                                   
082500         END-IF                                                           
082600       ELSE                                                               
082700         IF LAES-ENTER = JA                                               
082800           PERFORM IMS-GNP-W6INLA21                                       
082900           PERFORM UNTIL WS-IDRADNR-ENTER = RAD-IDRADNR OR                
083000                                        SEGMENT-SAKNAS OR                 
083100                                        SEGMENT-SLUT                      
083200            PERFORM IMS-GNP-W6INLA21                                      
083300           END-PERFORM                                                    
083400           MOVE NEJ TO LAES-ENTER                                         
083500           MOVE RAD-KVINLART TO MOD-KVINLART(IX)                          
083600           IF IX = 1                                                      
083700             MOVE RAD-IDRADNR  TO WW-IDRADNR-ENTER                        
083800           END-IF                                                         
083900             ADD +1 TO IX                                                 
084000             MOVE +0 TO W-KVAVIS                                          
084100           IF RAD-FLSATS = 'J'                                            
084200            ADD RAD-KVINLART TO W-KVAVIS                                  
084300           END-IF                                                         
084400         ELSE                                                             
084500           PERFORM IMS-GNP-W6INLA21                                       
084600           MOVE RAD-KVINLART TO MOD-KVINLART(IX)                          
084700           IF IX = 1                                                      
084800             MOVE RAD-IDRADNR TO WW-IDRADNR-ENTER                         
084900           END-IF                                                         
085000           ADD +1 TO IX                                                   
085100           MOVE +0 TO W-KVAVIS                                            
085200           IF RAD-FLSATS = 'J'                                            
085300             ADD RAD-KVINLART TO W-KVAVIS                                 
085400           END-IF                                                         
085500         END-IF                                                           
085600       END-IF                                                             
085700                                                                          
085800       PERFORM IMS-GNP-W6INLA21                                           
085900       PERFORM UNTIL SEGMENT-SAKNAS OR                                    
086000                      SEGMENT-SLUT   OR                                   
086100                      IX > 12                                             
086200          MOVE RAD-KVINLART TO MOD-KVINLART(IX)                           
086300          IF IX = 1                                                       
086400            MOVE RAD-IDRADNR TO WW-IDRADNR-ENTER                          
086500          END-IF                                                          
086600          ADD +1 TO IX                                                    
086700          IF RAD-FLSATS = 'J'                                             
086800            ADD RAD-KVINLART TO W-KVAVIS                                  
086900          END-IF                                                          
087000          PERFORM IMS-GNP-W6INLA21                                        
087100       END-PERFORM                                                        
087200       IF STATUS-WS = 'GE'                                                
087300          MOVE NEJ TO RAD-BRYTN                                           
087400       ELSE                                                               
087500          MOVE JA TO RAD-BRYTN                                            
087600       END-IF                                                             
087700                                                                          
087800     COMPUTE W-KVAVIS-KIT = ART-KVAVIS-KIT - W-KVAVIS                     
087900                                                                          
088000     IF W-KVAVIS-KIT < ZERO                                               
088100       MOVE ZERO TO MOD-KVAVIS-KIT(KVAVIS-IX)                             
088200     ELSE                                                                 
088300       MOVE W-KVAVIS-KIT TO MOD-KVAVIS-KIT(KVAVIS-IX)                     
088400     END-IF                                                               
088500                                                                          
088600     .                                                                    
088700     EJECT                                                                
088800                                                                          
088900 CBC-FLER-RADER SECTION.                                                  
089000     IF RAD-BRYTN = NEJ                                                   
089100       PERFORM UNTIL ART-IDLOPNRM NOT = WS-IDLOPNRM OR                    
089200       SEGMENT-SLUT OR SEGMENT-SAKNAS                                     
089300         PERFORM IMS-GN-W6INLA-F                                          
089400       END-PERFORM                                                        
089500       IF SEGMENT-FINNS                                                   
089600         PERFORM IMS-GNP-W6INLA21                                         
089700         MOVE ART-IDLOPNRM      TO MOD-IDLOPNRM-NEXT                      
089800         MOVE RAD-IDRADNR       TO MOD-IDRADNR-NEXT                       
089900         MOVE WW-IDLOPNRM-ENTER TO MOD-IDLOPNRM-ENTER                     
090000         MOVE WW-IDRADNR-ENTER  TO MOD-IDRADNR-ENTER                      
090100       ELSE                                                               
090200         MOVE WW-IDLOPNRM-ENTER TO MOD-IDLOPNRM-ENTER                     
090300         MOVE WW-IDRADNR-ENTER  TO MOD-IDRADNR-ENTER                      
090400         PERFORM UNTIL IX > 12                                            
090500         MOVE MFS-RENSA-FAELT TO MOD-IDARTNR(IX)                          
090600                                 MOD-KVINLART(IX)                         
090700                                 MOD-KVAVIS-PRIO(IX)                      
090800                                 MOD-BEFT(IX)                             
090900                                 MOD-FLKVAANT-TOT(IX)                     
091000                                 MOD-KVKVAPRIM(IX)                        
091100                                 MOD-KVROS(IX)                            
091200                                 MOD-KVAVIS-KIT(IX)                       
091300                                 MOD-ADTRDEST-KIT(IX)                     
091400                                 MOD-KDLAGEMB(IX)                         
091500                                 MOD-ADINLOMR-NXT1(IX)                    
091600                                 MOD-ADINLOMR-NXT2(IX)                    
091700                                 MOD-ADINLOMR-NXT3(IX)                    
091800         ADD 1 TO IX                                                      
091900         END-PERFORM                                                      
092000          IF MFS-IDPFK = '8' AND NYA-NYCKLAR = NEJ                        
092100             MOVE INF-LAST-PAGE TO MED-IDMFSINF                           
092200             CALL WMEDKONV USING MED-WMEDAREA                             
092300             MOVE MED-MFSINF TO MOD-TEMFSINF                              
092400          END-IF                                                          
092500       END-IF                                                             
092600     ELSE                                                                 
092700        MOVE ART-IDLOPNRM       TO MOD-IDLOPNRM-NEXT                      
092800        MOVE RAD-IDRADNR        TO MOD-IDRADNR-NEXT                       
092900         MOVE WW-IDLOPNRM-ENTER TO MOD-IDLOPNRM-ENTER                     
093000         MOVE WW-IDRADNR-ENTER  TO MOD-IDRADNR-ENTER                      
093100     END-IF                                                               
093200     MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                            
093300     CALL WMEDKONV USING MED-WMEDAREA                                     
093400     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
093500     .                                                                    
093600     EJECT                                                                
093700                                                                          
093800 D-UPPDATERA SECTION.                                                     
093900     MOVE +1 TO TRANS-IX                                                  
094000     MOVE NEJ TO SKICKAT-TRANS                                            
094100     IF MID-ADINLOMR-TOMN NOT = ALL '+'                                   
094200                                                                          
094300       MOVE MID-ADINLOMR-TOMN TO W-6006-ADINLOMR                          
094400       PERFORM IMS-GET-PLAA11                                             
094500                                                                          
094600       IF SEGMENT-SAKNAS                                                  
094700          MOVE MFS-ALFA-FAELT-FEL TO MOD-ADINLOMR-TOMN-ATTR               
094800       ELSE                                                               
094900          MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADINLOMR-TOMN-ATTR             
095000          MOVE JA TO UPPD-OK                                              
095100       END-IF                                                             
095200                                                                          
095300       IF UPPD-OK = JA                                                    
095400          MOVE WS-IDINLVGN TO W-IDINLVGN                                  
095500                              W-VAGN                                      
095600          PERFORM IMS-GET-W6INLA-F                                        
095700          IF SEGMENT-FINNS                                                
095800            PERFORM IMS-GHNP-W6INLA21                                     
095900          END-IF                                                          
096000          PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR                 
096100                        INDATA-FEL                                        
096200            IF TRANS-IX < MAX-KVPOST                                      
096300               PERFORM S01-SPARA-FAELT                                    
096400               PERFORM S50-PRIM-CONTROL                                   
096500               IF INDATA-OK                                               
096600                 MOVE ZERO TO RAD-IDINLVGN                                
096700                 MOVE MID-ADINLOMR-TOMN TO RAD-ADINLOMR                   
096800                 MOVE SPACE TO RAD-ADINLOMR-NXT                           
096900                 PERFORM IMS-REPL-W6INLA21                                
097000                 PERFORM S03-SKAPA-TRANS-6191                             
097100               END-IF                                                     
097200               PERFORM IMS-GN-W6INLA-F                                    
097300               IF SEGMENT-FINNS                                           
097400                 PERFORM IMS-GHNP-W6INLA21                                
097500               END-IF                                                     
097600            ELSE                                                          
097700               PERFORM S04-SKICKA-TRANS-6191                              
097800               MOVE ZERO TO TRANS-IX                                      
097900               MOVE JA TO SKICKAT-TRANS                                   
098000            END-IF                                                        
098100            ADD +1 TO TRANS-IX                                            
098200          END-PERFORM                                                     
098300          IF INDATA-OK                                                    
098410            IF SKICKAT-TRANS = NEJ                                        
098420              PERFORM S04-SKICKA-TRANS-6191                               
098430            END-IF                                                        
098440            MOVE INF-UDAT-UTFORD TO MED-IDMFSINF                          
098500          END-IF                                                          
098600          CALL WMEDKONV USING MED-WMEDAREA                                
098700          MOVE MED-MFSINF TO MOD-TEMFSINF                                 
098800          PERFORM MFS-RENSA-RADFAELT                                      
098900          MOVE MFS-RENSA-FAELT TO MOD-IDINLVGN-IN                         
099000                                  MOD-ADINLOMR-TOMN                       
099400       ELSE                                                               
099500          PERFORM MFS-ROER-EJ-FAELT-UT                                    
099600          MOVE ERR-KORR-UPPL TO MED-IDMFSFEL                              
099700          CALL WMEDKONV USING MED-WMEDAREA                                
099800          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
099900       END-IF                                                             
100000     ELSE                                                                 
100100         MOVE ERR-PF11-INGENDATA TO MED-IDMFSFEL                          
100200         CALL WMEDKONV USING MED-WMEDAREA                                 
100300         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
100400         MOVE INF-TRYCK-PF4 TO MED-IDMFSINF                               
100500         CALL WMEDKONV USING MED-WMEDAREA                                 
100600         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
100700         PERFORM C-LAES-VISA-INFO                                         
100800         PERFORM MFS-ROER-EJ-FAELT-UT                                     
100900     END-IF                                                               
101000     MOVE JA TO INDATA-SW                                                 
101100     .                                                                    
101200     EJECT                                                                
101300                                                                          
101400 E-TRYCK-PF11  SECTION.                                                   
101500     MOVE INF-TRYCK-PF11 TO MED-IDMFSFEL                                  
101600     CALL WMEDKONV USING MED-WMEDAREA                                     
101700     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
101800     PERFORM MFS-ROER-EJ-FAELT-UT                                         
101900     .                                                                    
102000     EJECT                                                                
102100                                                                          
102200 F-MID-INDATA-TILL-MOD SECTION.                                           
102300     IF MID-ADINLOMR-TOMN NOT = ALL '+'                                   
102400       MOVE MID-ADINLOMR-TOMN TO MOD-ADINLOMR-TOMN                        
102500     ELSE                                                                 
102600       MOVE MFS-RENSA-FAELT TO MOD-ADINLOMR-TOMN                          
102700     END-IF                                                               
102800                                                                          
102900     IF MID-ADINLOMR-PRT NOT = ALL '+'                                    
103000       MOVE MID-ADINLOMR-PRT  TO MOD-ADINLOMR-PRT                         
103100     ELSE                                                                 
103200       MOVE MFS-RENSA-FAELT TO MOD-ADINLOMR-PRT                           
103300     END-IF                                                               
103400     .                                                                    
103500     EJECT                                                                
103600                                                                          
103700 S01-SPARA-FAELT SECTION.                                                 
103800     MOVE RAD-ADINLOMR     TO SPAR-ADINLOMR                               
103900     MOVE RAD-ADINLOMR-NXT TO SPAR-ADINLOMR-NXT                           
104000     MOVE RAD-KDINLSTA     TO SPAR-KDINLSTA                               
104100     MOVE RAD-KVINLART     TO SPAR-KVINLART                               
104200     .                                                                    
104300     EJECT                                                                
104400                                                                          
104500 S03-SKAPA-TRANS-6191 SECTION.                                            
104600     MOVE IDPGM         TO 6191-MID-IDPGM                                 
104700     MOVE DCS-IDDC      TO 6191-MID-IDDC                                  
104800     MOVE ART-IDLOPNRM  TO 6191-MID-IDLOPNRM(TRANS-IX)                    
104900     MOVE RAD-IDRADNR   TO 6191-MID-IDRADNR(TRANS-IX)                     
105000     MOVE RAD-KDINLPRIO TO 6191-MID-KDINLPRIO(TRANS-IX)                   
105100     MOVE ART-PRARTSTD  TO 6191-MID-PRARTSTD(TRANS-IX)                    
105200     MOVE +0            TO 6191-MID-KVKOLLI (TRANS-IX)                    
105300     MOVE 'N'           TO 6191-MID-FLINLI  (TRANS-IX)                    
105400     MOVE SPAR-ADINLOMR TO 6191-MID-ADINLOMR-OLD(TRANS-IX)                
105500     MOVE SPAR-ADINLOMR-NXT TO                                            
105600                            6191-MID-ADINLOMR-NXT-OLD(TRANS-IX)           
105700     MOVE SPAR-KDINLSTA TO  6191-MID-KDINLSTA-OLD(TRANS-IX)               
105800     MOVE SPAR-KVINLART TO  6191-MID-KVINLART-OLD(TRANS-IX)               
105900     MOVE RAD-ADINLOMR  TO  6191-MID-ADINLOMR-NEW(TRANS-IX)               
106000     MOVE RAD-ADINLOMR-NXT TO                                             
106100                           6191-MID-ADINLOMR-NXT-NEW(TRANS-IX)            
106200     MOVE RAD-KDINLSTA  TO 6191-MID-KDINLSTA-NEW(TRANS-IX)                
106300     MOVE RAD-KVINLART  TO 6191-MID-KVINLART-NEW(TRANS-IX)                
106400     .                                                                    
106500     EJECT                                                                
106600                                                                          
106700 S04-SKICKA-TRANS-6191 SECTION.                                           
106800     IF SKICKAT-TRANS = NEJ                                               
106900       COMPUTE 6191-MID-KVPOST = TRANS-IX - 1                             
107000     ELSE                                                                 
107100       MOVE TRANS-IX         TO 6191-MID-KVPOST                           
107200     END-IF                                                               
107300     MOVE 'W6T191X  '  TO P-TO-P-MSG-KDTRANS                              
107400     MOVE '6131'       TO P-TO-P-MSG-IDTRANS                              
107500     MOVE MFS-KDMFSFOR TO P-TO-P-MSG-KDMFSFOR                             
107600     COMPUTE P-TO-P-MSG-KVLL = P-TO-P-PREFIX-LNG +                        
107700                               MID-6191-FASTDEL-LNG  +                    
107800                        (6191-MID-KVPOST * MID-6191-UPPF-POST-LNG)        
107900                                                                          
108000     MOVE 6191-MID-W6I19101  TO P-TO-P-MSG-INDATA                         
108100     PERFORM IMS-ISRT-ALT-MSG-6191                                        
108200     .                                                                    
108300     EJECT                                                                
108400                                                                          
108500 S50-PRIM-CONTROL SECTION.                                                
108600* THIS IS A CONTROL TO CHECK THE OLD PLACE IF FLAG FLKNTRGK = YES         
108700* IF THE FLAG IS YES WE HAVE TO DO A CHECK ON THE OLD PLACE BEFORE        
108800* WE CHECK THE NEW PLACE.                                                 
108900     MOVE RAD-ADINLOMR         TO W-6006-ADINLOMR                         
109000     PERFORM IMS-GET-PLAA11                                               
109100     IF SEGMENT-FINNS                                                     
109200       IF 6006-FLKNTRGK = JA                                              
109300         MOVE MID-ADINLOMR-TOMN TO W-6006-ADINLOMR                        
109400         PERFORM IMS-GET-PLAA11                                           
109500         IF (6006-FLKNTRGK = JA )                                         
109600         OR 6006-KDINLOMR = 'LPL'                                         
109700           MOVE 'N' TO PRIM-CONTROL-SW                                    
109800         ELSE                                                             
109900           MOVE 'J' TO PRIM-CONTROL-SW                                    
110000         END-IF                                                           
110100       ELSE                                                               
110200         MOVE 'N' TO PRIM-CONTROL-SW                                      
110300       END-IF                                                             
110400     ELSE                                                                 
110500       MOVE 'N' TO PRIM-CONTROL-SW                                        
110600     END-IF                                                               
110700     IF PRIM-CONTROL-YES                                                  
110800       PERFORM S51-CHECK-OLD-PLACE                                        
110900     END-IF                                                               
111000     .                                                                    
111100     EJECT                                                                
111200                                                                          
111300 S51-CHECK-OLD-PLACE     SECTION.                                         
111400     MOVE ART-IDLOPNRM  TO W-IDLOPNRM                                     
111500     PERFORM IMS-GU-UPFA01                                                
111600     IF SEGMENT-FINNS                                                     
111700       IF UPPF-KVKVAPRIM > ZERO                                           
111800         IF UPPF-KDKVASTA-PRI = '2' OR '3'                                
111900           CONTINUE                                                       
112000         ELSE                                                             
112100           MOVE '605' TO MED-IDMFSFEL                                     
112200           MOVE NEJ TO INDATA-SW                                          
112300         END-IF                                                           
112400       END-IF                                                             
112500       IF UPPF-KVKVASEK > ZERO                                            
112600         IF UPPF-KDKVASTA-PRI = '2' OR '3'                                
112700           CONTINUE                                                       
112800         ELSE                                                             
112900           MOVE '605' TO MED-IDMFSFEL                                     
113000           MOVE NEJ TO INDATA-SW                                          
113100         END-IF                                                           
113200       END-IF                                                             
113300     END-IF                                                               
113400                                                                          
113500     IF INDATA-OK                                                         
113600       PERFORM IMS-GU-UPFA01                                              
113700       IF SEGMENT-FINNS                                                   
113800         PERFORM IMS-GNP-UPFA11                                           
113900         PERFORM UNTIL SEGMENT-SAKNAS                                     
114000           IF RAPP-KDKVASTA-PRI = '2' OR '3'                              
114100             CONTINUE                                                     
114200           ELSE                                                           
114300             MOVE '605' TO MED-IDMFSFEL                                   
114400             MOVE NEJ TO INDATA-SW                                        
114500           END-IF                                                         
114600           PERFORM IMS-GNP-UPFA11                                         
114700         END-PERFORM                                                      
114800       END-IF                                                             
114900     END-IF                                                               
115000                                                                          
115100     IF INDATA-OK                                                         
115200       PERFORM IMS-GU-UPFA01                                              
115300       IF SEGMENT-FINNS                                                   
115400         PERFORM IMS-GNP-UPFA12                                           
115500         PERFORM UNTIL SEGMENT-SAKNAS                                     
115600           IF SPEC-KDKVASTA-PRI = '2' OR '3'                              
115700             CONTINUE                                                     
115800           ELSE                                                           
115900             MOVE '605' TO MED-IDMFSFEL                                   
116000             MOVE NEJ TO INDATA-SW                                        
116100           END-IF                                                         
116200           PERFORM IMS-GNP-UPFA12                                         
116300         END-PERFORM                                                      
116400       END-IF                                                             
116500     END-IF                                                               
116600                                                                          
116700     IF MED-IDMFSFEL = '605'                                              
116800        CALL WMEDKONV USING MED-WMEDAREA                                  
116900        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
117000     END-IF                                                               
117100     .                                                                    
117200     EJECT                                                                
117300                                                                          
117400 MFS-RENSA-RADFAELT SECTION.                                              
117500       MOVE 1 TO INDX                                                     
117600       PERFORM UNTIL INDX > 12                                            
117700        MOVE MFS-RENSA-FAELT TO MOD-IDARTNR(INDX)                         
117800                                MOD-KVINLART(INDX)                        
117900                                MOD-KVAVIS-PRIO(INDX)                     
118000                                MOD-BEFT(INDX)                            
118100                                MOD-FLKVAANT-TOT(INDX)                    
118200                                MOD-KVKVAPRIM(INDX)                       
118300                                MOD-KVROS(INDX)                           
118400                                MOD-KVAVIS-KIT(INDX)                      
118500                                MOD-ADTRDEST-KIT(INDX)                    
118600                                MOD-KDLAGEMB(INDX)                        
118700                                MOD-ADINLOMR-NXT1(INDX)                   
118800                                MOD-ADINLOMR-NXT2(INDX)                   
118900                                MOD-ADINLOMR-NXT3(INDX)                   
119000        ADD 1 TO INDX                                                     
119100       END-PERFORM                                                        
119200     .                                                                    
119300     EJECT                                                                
119400                                                                          
119500 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
119600     MOVE MFS-ROER-EJ-FAELT TO MOD-IDLOPNRM-NEXT                          
119700                               MOD-IDRADNR-NEXT                           
119800                               MOD-IDLOPNRM-ENTER                         
119900                               MOD-IDRADNR-ENTER                          
120000                               MOD-ADINLOMR-TOMN                          
120100     MOVE +1 TO INDX                                                      
120200     PERFORM UNTIL INDX > 12                                              
120300       MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR(INDX)                        
120400                                 MOD-KVINLART(INDX)                       
120500                                 MOD-KVAVIS-PRIO(INDX)                    
120600                                 MOD-BEFT(INDX)                           
120700                                 MOD-FLKVAANT-TOT(INDX)                   
120800                                 MOD-KVKVAPRIM(INDX)                      
120900                                 MOD-KVROS(INDX)                          
121000                                 MOD-KVAVIS-KIT(INDX)                     
121100                                 MOD-ADTRDEST-KIT(INDX)                   
121200                                 MOD-KDLAGEMB(INDX)                       
121300                                 MOD-ADINLOMR-NXT1(INDX)                  
121400                                 MOD-ADINLOMR-NXT2(INDX)                  
121500                                 MOD-ADINLOMR-NXT3(INDX)                  
121600       ADD +1 TO INDX                                                     
121700     END-PERFORM                                                          
121800     SKIP2                                                                
121900     .                                                                    
122000     EJECT                                                                
122100* --- IMS SEKTIONER ---                                                   
122200     SKIP3                                                                
122300                                                                          
122400 IMS-GET-MSG SECTION.                                                     
122500     MOVE '  QC' TO GODK-STATUSKODER                                      
122600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
122700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
122800     PERFORM IMS-STATUSKONTROLL                                           
122900     .                                                                    
123000     SKIP3                                                                
123100                                                                          
123200 IMS-INSERT-MSG SECTION.                                                  
123300     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
123400       MOVE '0' TO MFS-KDHUVOMR                                           
123500     END-IF                                                               
123600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
123700     MOVE SPACE TO GODK-STATUSKODER                                       
123800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
123900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
124000     PERFORM IMS-STATUSKONTROLL                                           
124100     .                                                                    
124200     EJECT                                                                
124300                                                                          
124400 IMS-ISRT-ALT-MSG-6191 SECTION.                                           
124500     MOVE SPACE TO GODK-STATUSKODER                                       
124600     CALL CBLTDLI USING ISRT 6191-PCB P-TO-P-MSG-IO-AREA-SNUF             
124700     MOVE 6191-STATUS-CODE TO STATUS-WS                                   
124800     PERFORM IMS-STATUSKONTROLL                                           
124900     .                                                                    
125000     EJECT                                                                
125100                                                                          
125200 IMS-GET-W6INLA-F SECTION.                                                
125300     STRING 'W6INLA11(W6D1FSEQ =' W-W6D1FSEQ-X                            
125400                    '&IDDC     =' W-IDDC-X ')'                            
125500             DELIMITED BY SIZE INTO SSA1                                  
125600     MOVE '  GE'          TO GODK-STATUSKODER                             
125700     CALL CBLTDLI USING GU INLG-PCB DLI-IO-AREA-INLF11 SSA1               
125800     MOVE INLG-STATUS-CODE TO STATUS-WS                                   
125900     PERFORM IMS-STATUSKONTROLL                                           
126000     .                                                                    
126100     EJECT                                                                
126200                                                                          
126300 IMS-GNP-W6INLA21 SECTION.                                                
126400     STRING 'W6INLA21(IDINLVGN =' W-IDINLVGN-X ')'                        
126500             DELIMITED BY SIZE INTO SSA1                                  
126600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
126700     CALL CBLTDLI USING GNP INLG-PCB DLI-IO-AREA-INL21 SSA1               
126800     MOVE INLG-STATUS-CODE TO STATUS-WS                                   
126900     PERFORM IMS-STATUSKONTROLL                                           
127000     .                                                                    
127100     EJECT                                                                
127200                                                                          
127300 IMS-GHNP-W6INLA21 SECTION.                                               
127400     STRING 'W6INLA21(IDINLVGN =' W-IDINLVGN-X ')'                        
127500             DELIMITED BY SIZE INTO SSA1                                  
127600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
127700     CALL CBLTDLI USING GHNP INLG-PCB DLI-IO-AREA-INL21 SSA1              
127800     MOVE INLG-STATUS-CODE TO STATUS-WS                                   
127900     PERFORM IMS-STATUSKONTROLL                                           
128000     .                                                                    
128100     EJECT                                                                
128200                                                                          
128300 IMS-GN-W6INLA-F SECTION.                                                 
128400     STRING 'W6INLA11(W6D1FSEQ =' W-W6D1FSEQ-X                            
128500                    '&IDDC     =' W-IDDC-X ')'                            
128600             DELIMITED BY SIZE INTO SSA1                                  
128700     MOVE '  GEGB' TO GODK-STATUSKODER                                    
128800     CALL CBLTDLI USING GN INLG-PCB DLI-IO-AREA-INLF11 SSA1               
128900     MOVE INLG-STATUS-CODE TO STATUS-WS                                   
129000     PERFORM IMS-STATUSKONTROLL                                           
129100     .                                                                    
129200     EJECT                                                                
129300                                                                          
129400 IMS-GET-PLAA11 SECTION.                                                  
129500     STRING 'W6PLAA01(W6GXKEY  =' W-W6GXKEY-6005-X ')'                    
129600          DELIMITED BY SIZE INTO SSA1                                     
129700     STRING 'W6PLAA11(W6GXKEY  =' W-W6GXKEY-6006-X ')'                    
129800          DELIMITED BY SIZE INTO SSA2                                     
129900     MOVE '  GE' TO GODK-STATUSKODER                                      
130000     CALL CBLTDLI USING GU PLAA-PCB DLI-IO-AREA SSA1 SSA2                 
130100     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
130200     PERFORM IMS-STATUSKONTROLL                                           
130300     .                                                                    
130400     EJECT                                                                
130500                                                                          
130600 IMS-REPL-W6INLA21 SECTION.                                               
130700     MOVE '  ' TO GODK-STATUSKODER                                        
130800     CALL CBLTDLI USING REPL INLG-PCB DLI-IO-AREA-INL21                   
130900     MOVE INLG-STATUS-CODE TO STATUS-WS                                   
131000     PERFORM IMS-STATUSKONTROLL                                           
131100     .                                                                    
131200     EJECT                                                                
131300                                                                          
131400 IMS-GU-WDK611 SECTION.                                                   
131500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
131600          DELIMITED BY SIZE INTO SSA1                                     
131700     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY ')'                          
131800          DELIMITED BY SIZE INTO SSA2                                     
131900     MOVE '  GE' TO GODK-STATUSKODER                                      
132000     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-WDK6 SSA1 SSA2            
132100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
132200     PERFORM IMS-STATUSKONTROLL                                           
132300     .                                                                    
132400     SKIP2                                                                
132500                                                                          
132600 IMS-GU-WDK711 SECTION.                                                   
132700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
132800          DELIMITED BY SIZE INTO SSA1                                     
132900     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
133000          DELIMITED BY SIZE INTO SSA2                                     
133100     MOVE '  GE' TO GODK-STATUSKODER                                      
133200     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK7 SSA1 SSA2            
133300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
133400     PERFORM IMS-STATUSKONTROLL                                           
133500     .                                                                    
133600     EJECT                                                                
133700                                                                          
133800 IMS-GU-UPFA01 SECTION.                                                   
133900     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
134000          DELIMITED BY SIZE INTO SSA1                                     
134100     MOVE '  GE' TO GODK-STATUSKODER                                      
134200     CALL CBLTDLI USING GU UPFA-PCB DLI-IO-AREA-UPFA01 SSA1               
134300     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
134400     PERFORM IMS-STATUSKONTROLL                                           
134500     .                                                                    
134600     SKIP3                                                                
134700                                                                          
134800 IMS-GNP-UPFA11 SECTION.                                                  
134900     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
135000          DELIMITED BY SIZE INTO SSA1                                     
135100     MOVE 'W6UPFA11 ' TO SSA2                                             
135200     MOVE '  GE' TO GODK-STATUSKODER                                      
135300     CALL CBLTDLI USING GNP UPFA-PCB DLI-IO-AREA-UPFA11 SSA1 SSA2         
135400     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
135500     PERFORM IMS-STATUSKONTROLL                                           
135600     .                                                                    
135700     SKIP3                                                                
135800                                                                          
135900 IMS-GNP-UPFA12 SECTION.                                                  
136000     STRING 'W6UPFA01(IDLOPNRM =' W-IDLOPNRM-X ')'                        
136100          DELIMITED BY SIZE INTO SSA1                                     
136200     MOVE 'W6UPFA12 ' TO SSA2                                             
136300     MOVE '  GE' TO GODK-STATUSKODER                                      
136400     CALL CBLTDLI USING GNP UPFA-PCB DLI-IO-AREA-UPFA12 SSA1 SSA2         
136500     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
136600     PERFORM IMS-STATUSKONTROLL                                           
136700     .                                                                    
136800     SKIP3                                                                
136900 IMS-GU-WDB601    SECTION.                                                
137000     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
137100          DELIMITED BY SIZE INTO SSA1                                     
137200     MOVE '  GE' TO GODK-STATUSKODER                                      
137300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
137400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
137500     PERFORM IMS-STATUSKONTROLL                                           
137600     IF SEGMENT-SAKNAS                                                    
137700         MOVE SPACE TO DCS-KDDC                                           
137800     END-IF                                                               
137900     .                                                                    
138000 IMS-STATUSKONTROLL SECTION.                                              
138100     SET STATUS-IX TO 1                                                   
138200     SEARCH GODK-STATUS                                                   
138300       AT END                                                             
138400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
138500         DELIMITED BY SIZE INTO FELTEXT                                   
138600         CALL FELLOG                                                      
138700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
138800         CONTINUE                                                         
138900     END-SEARCH                                                           
139000     .                                                                    
