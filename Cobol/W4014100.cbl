000100 ID DIVISION.                                                             
000201 PROGRAM-ID.     W4014100.                                                
000301 AUTHOR.         GÖRAN KJELLSON.                                          
000401 DATE-WRITTEN.   JAN 2020.                                                
000501 DATE-COMPILED.                                                           
000601                                                                          
000701*    FUNKTION:                                                            
000801*        UNDERHÅLL AV WHITE LIST FÖR MIC, DVS ARTIKLAR FÖR                
000901*        VILKA VI INTE SKALL SKICKA ORDERRADEN TILL MIC.                  
001001*                                                                         
001701*                                                                         
001801*        PROGRAMMET UPPDATERAR WDR5 (4253/4254)                           
001901*        PROGRAMMET      LÄSER WDK6                                       
002001*                                                                         
002101*    INDATA.                                                              
002201*        TRANSAKTION: W4T141 W4T141U                                      
002301*        MID:         W4I14101                                            
002401*                                                                         
002501*    UTDATA.                                                              
002601*        MOD:         W4O14101                                            
002701                                                                          
002801                                                                          
002901 ENVIRONMENT DIVISION.                                                    
003001 DATA DIVISION.                                                           
003101 WORKING-STORAGE SECTION.                                                 
003201                                                                          
003301 77  IDPGM                       PIC X(08)   VALUE 'W4014100'.            
003401 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003501                                                                          
003601 77  JA                          PIC X       VALUE 'J'.                   
003701 77  NEJ                         PIC X       VALUE 'N'.                   
003801 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
003901 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
004001                                                                          
004101*    --- GENERELLA ARBETSFÄLT                                             
004201 01  CURRENT-DATE                PIC 9(6).                                
004301 01  CURRENT-TIME-NUM            PIC 9(8).                                
004401 01  FILLER REDEFINES CURRENT-TIME-NUM.                                   
004501     03  CURRENT-HH              PIC 9(2).                                
004502     03  CURRENT-MM              PIC 9(2).                                
004601     03  FILLER                  PIC 9(4).                                
004701                                                                          
004901 01  WS-TID                      PIC 9(4)    VALUE ZERO.                  
004902 01  FILLER REDEFINES WS-TID.                                             
005001     03   WS-HH                  PIC 9(2).                                
005201     03   WS-MM                  PIC 9(2).                                
005202                                                                          
005203 01  WS-TID-RED.                                                          
005204     03   WS-HH-RED              PIC 9(2)    VALUE ZERO.                  
005205     03   FILLER                 PIC X(1)    VALUE ':'.                   
005206     03   WS-MM-RED              PIC 9(2)    VALUE ZERO.                  
005207                                                                          
005301 01  WS-IDARTNR                  PIC 9(9).                                
005501                                                                          
005502 01  W-LATEST-UPD                PIC 9(6)    VALUE ZERO.                  
005503 01  W-ACTIVE-PARTS              PIC 9(6)    VALUE ZERO.                  
005504 01  W-TOTAL-PARTS               PIC 9(6)    VALUE ZERO.                  
005505                                                                          
005506 01  W-CMD                       PIC X      VALUE SPACE.                  
005507     88 W-OK-CMD                            VALUE ' ' 'S' 'A' 'D'.        
005508     88 W-STOP                              VALUE ' ' 'S'.                
005509     88 W-ACTIVATE                          VALUE ' ' 'A'.                
005510     88 W-DELETE                            VALUE ' ' 'D'.                
005520                                                                          
005601*    --- INDEX FÖR BLÄDDRINGSRADER                                        
005701 77  MFS-INDX                    PIC S9(4)  VALUE +0    COMP SYNC.        
005801 77  INDX                        PIC 9(4)  VALUE 0.                       
005901 77  INDX-MAX                    PIC 9(4)  VALUE 14.                      
006001*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
006101                                                                          
006201 77  ALLT-SW                     PIC X       VALUE 'J'.                   
006301     88  ALLT-OK                             VALUE 'J'.                   
006401                                                                          
006402 77  NYARTIKEL-SW               PIC X       VALUE 'N'.                    
006403     88  NYARTIKEL-JA                        VALUE 'J'.                   
006404                                                                          
006501 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006601     88  INDATA-OK                           VALUE 'J'.                   
006701     88  INDATA-FEL                          VALUE 'N'.                   
006801                                                                          
007001 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007101     88  NYCKLAR-OK                          VALUE 'J'.                   
007201     88  NYCKLAR-FEL                         VALUE 'N'.                   
010401                                                                          
010501                                                                          
010600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
010701     88  EGEN-MID                            VALUE '4141'.                
010801     88  GODK-MID                            VALUE '4141'.                
011300     88  HELP-MID                            VALUE '0551'.                
011400                                                                          
011501                                                                          
011600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
011700 01  GENERELLA-SUBPROGRAM.                                                
011800     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
012000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
012100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012300                                                                          
012400*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
012500*01 -COPY WMEDAREA                                                        
012600                                                                          
013000 01  MESSAGE-CODES.                                                       
013100     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
013200     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
013300     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
013400     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
013500     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
013600     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
013700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
013900     03  PART-EXISTS             PIC X(3)    VALUE '278'.                 
014000     03  PART-MISSING            PIC X(3)    VALUE '279'.                 
014100     03  PART-MISSING-WDK6       PIC X(3)    VALUE '017'.                 
014200                                                                          
014301                                                                          
014400*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
014500                                                                          
014600 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
014700                                                                          
014800*01 -COPY WMSGINIT                                                        
014900                                                                          
015001                                                                          
015100*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
015200                                                                          
015300 01  SPAR-AREA.                                                           
015401     03  SPAR-IDTRANS            PIC X(4)    VALUE '4141'.                
015500     03  SPAR-IDARTNR-ENTER      PIC 9(9)    VALUE ZERO.                  
015600     03  SPAR-IDARTNR-NEXT       PIC 9(9)    VALUE ZERO.                  
016700                                                                          
016800*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
016900                                                                          
017000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
017100                                                                          
017201*01  MID -COPY W4I14101                                                   
017300                                                                          
017401                                                                          
017500 01  FILLER                      PIC X(16)   VALUE 'MSG/MOD-AREA'.        
017600                                                                          
017700*01  -COPY WMSGAREA                                                       
017800                                                                          
017901                                                                          
018000     03  MOD REDEFINES MSG-AREA.                                          
018101*      05  -COPY W4O14101                                                 
018200                                                                          
018301                                                                          
018400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
018500                                                                          
018600*01  -COPY WMFSAREA                                                       
018700                                                                          
018801                                                                          
018900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
019000*                                                                         
019100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
019200                                                                          
019300 01  FILLER              PIC X(16)  VALUE 'MSG-KOM-AREA'.                 
019400*      --- GENERELL IO-KOMMUNIKATIONSAREA FÖR DISPATCHER                  
019500*01  -COPY WMSGKOM                                                        
019600                                                                          
019700 01  NYCKLAR-TILL-DLI.                                                    
019710     03  W-WDGXKEY-X.                                                     
019720         05  W-IDHTYP            PIC X(4)    VALUE '4253'.                
019730         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
019900                                                                          
020300     03  W-IDARTNR-X.                                                     
020400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
020500                                                                          
023601                                                                          
023700                                                                          
023800*    --- STATUS-KOD FRÅN IMS                                              
023900 01  STATUS-WS                   PIC XX.                                  
024000     88  SEGMENT-FINNS                       VALUE '  '.                  
024200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
024300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
024400                                                                          
024500 01  GODK-STATUSKODER.                                                    
024600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
024700                                                                          
024800 01  ALL-SSA.                                                             
024901     03 SSA1                     PIC X(64).                               
025000     03 SSA2                     PIC X(64).                               
025100                                                                          
025201                                                                          
025300*    --- IMS FUNKTIONSKODER                                               
025400*01  -COPY W0003                                                          
025500                                                                          
025601                                                                          
025700*    ---  DLI INPUT-OUTPUT AREA                                           
025800                                                                          
025900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
026000 01  DLI-IO-WDK601.                                                       
026100*    03  -COPY WDK601 -PRE WDK6                                           
026200                                                                          
026301                                                                          
026302 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX01'.                      
026303 01  DLI-IO-WDGX01.                                                       
026304*    03  -COPY WDGX01                                                     
026305                                                                          
026306 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4254'.                    
026307 01  DLI-IO-WDGX4254.                                                     
026308*    03  -COPY WDGX4254                                                   
026309                                                                          
026310                                                                          
028901                                                                          
029001                                                                          
029101                                                                          
029200 LINKAGE SECTION.                                                         
029300*01  -COPY W0009  -PRE MSG-                                               
029400                                                                          
029700*01  -COPY W0008  -PRE USEA-                                              
029800     05  FILLER                  PIC X.                                   
029900                                                                          
030000*01  -COPY W0008  -PRE WDR5-                                              
030100     05  FILLER                  PIC X.                                   
030200                                                                          
030300*01  -COPY W0008  -PRE WDK6-                                              
030400     05  FILLER                  PIC X.                                   
030500                                                                          
030800                                                                          
030901                                                                          
031001 PROCEDURE DIVISION  USING MSG-PCB  USEA-PCB                              
031101                           WDR5-PCB WDK6-PCB.                             
031200 MAIN SECTION.                                                            
031301     ENTRY 'DLITCBL' USING MSG-PCB  USEA-PCB                              
031401                           WDR5-PCB WDK6-PCB.                             
031500                                                                          
031600     PERFORM IMS-GET-MSG                                                  
031700     IF SEGMENT-FINNS                                                     
031900        PERFORM A-INIT                                                    
032000        PERFORM B-KOLLA-NYCKLAR                                           
032100        IF NYCKLAR-OK                                                     
032201           IF MFS-UPDATE                                                  
032300              PERFORM G-KOLLA-INPUT                                       
032400              IF INDATA-OK                                                
032500                 PERFORM H-UPPDATERA                                      
032600              END-IF                                                      
032700           ELSE                                                           
032710              PERFORM I-KOLLA-ATT-EJ-UPDATE                               
032720              IF ALLT-OK                                                  
032800                 IF MFS-FIRST                                             
032900                    PERFORM C-FOERSTA-SIDA                                
033000                 ELSE                                                     
033100                    IF MFS-NEXT                                           
033200                       PERFORM D-NAESTA-SIDA                              
033300                    ELSE                                                  
033400                       PERFORM E-SAMMA-SIDA                               
033500                    END-IF                                                
033600                 END-IF                                                   
033610              END-IF                                                      
033700           END-IF                                                         
033800           IF ALLT-OK                                                     
033900              PERFORM F-LAES-VISA-INFO                                    
034001           ELSE                                                           
034201              PERFORM MFS-ROER-EJ-FAELT-IN                                
034202              PERFORM MFS-ROER-EJ-FAELT-UT                                
034400           END-IF                                                         
034500        END-IF                                                            
034901                                                                          
035001        COMPUTE MSG-KVLL = LENGTH OF MOD-W4O14101 + 4                     
035101        PERFORM IMS-INSERT-MSG                                            
035300     END-IF                                                               
035400                                                                          
035500     MOVE ZERO TO RETURN-CODE                                             
035600     GOBACK                                                               
035700     .                                                                    
035800                                                                          
035901                                                                          
036000 A-INIT SECTION.                                                          
036101     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
036200                                                                          
036401     ACCEPT CURRENT-DATE     FROM DATE                                    
036501     ACCEPT CURRENT-TIME-NUM FROM TIME                                    
036600                                                                          
036700     IF MSG-DUBBLA-TRANSKODER                                             
036801       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I14101                 
036900       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
037000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
037100     ELSE                                                                 
037201       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I14101                  
037300       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
037400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
037500     END-IF                                                               
037600                                                                          
037700     MOVE MSG-KDTRTYP      TO MFS-KDTRTYP                                 
037800     MOVE MSG-IDPFK        TO MFS-IDPFK                                   
037900     MOVE MFS-IDTRANS      TO W-IDTRANS                                   
038000                                                                          
038100     MOVE LOW-VALUE        TO MSG-AREA                                    
038201     MOVE 'W4O141N1'       TO MFS-IDMOD                                   
038301     MOVE '4141'           TO MOD-IDTRANS                                 
038401                                                                          
038500     MOVE MFS-RENSA-FAELT  TO MOD-TEMFSFEL MOD-TEMFSINF                   
038600                                                                          
038700     IF EGEN-MID OR HELP-MID                                              
038901       IF MID-IDARTNR-IN  NOT = ALL '+'                                   
039001          MOVE SPACE       TO MFS-KDTRTYP                                 
039101          MOVE '7'         TO MFS-IDPFK                                   
039201       ELSE                                                               
039300          CONTINUE                                                        
039401       END-IF                                                             
039500     ELSE                                                                 
039600       MOVE SPACE          TO MFS-KDTRTYP                                 
039700       MOVE '7'            TO MFS-IDPFK                                   
039800     END-IF                                                               
039900     .                                                                    
040000                                                                          
040101                                                                          
040200 B-KOLLA-NYCKLAR SECTION.                                                 
040301     MOVE 'B-KOLLA-NYCKLAR ' TO CURRENT-SECTION                           
040400                                                                          
040500     MOVE ALL '+'            TO MSGI-WMSGINIT                             
040600     MOVE '001'              TO MSGI-KDCALL                               
040700     MOVE MSG-LTERM-NAME     TO MSGI-IDLTERM-USER                         
040800     MOVE MSG-SIGNON-USERID  TO MSGI-IDUSER                               
040901     MOVE '4141'             TO MSGI-IDTRANS                              
041000                                                                          
041100     IF EGEN-MID                                                          
041300       IF MID-IDARTNR-IN NOT = ALL '+'                                    
041401         INSPECT MID-IDARTNR-IN REPLACING LEADING SPACE BY ZERO           
041501         IF MID-IDARTNR-IN NUMERIC                                        
041600            MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                           
041700                                   SPAR-IDARTNR-ENTER                     
041701         ELSE                                                             
041801            MOVE ALL '+'        TO MID-IDARTNR-IN                         
041901         END-IF                                                           
042000       ELSE                                                               
042100         MOVE MID-IDARTNR-UT TO MID-IDARTNR-IN                            
042200                                MSGI-IDARTNR                              
042300       END-IF                                                             
042401     ELSE                                                                 
042501       MOVE ALL '+'          TO MID-IDARTNR-IN                            
042600     END-IF                                                               
042700                                                                          
042800     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
042900     MOVE MSGI-SPAR-AREA     TO SPAR-AREA                                 
043000                                                                          
043100     IF MSGI-IDLAND-SPR = 'GB'                                            
043200       MOVE 'GB'             TO MED-IDSKYLT                               
043300     ELSE                                                                 
043400       MOVE 'S'              TO MED-IDSKYLT                               
043500     END-IF                                                               
043600                                                                          
043700     MOVE JA                 TO ALLT-SW                                   
043800     MOVE JA                 TO NYCKLAR-SW                                
043900                                                                          
044000     MOVE SPACE              TO MED-IDMFSFEL                              
044100                                MED-IDMFSINF                              
044200                                MSG-KOM-IDMFSMED                          
044300                                                                          
044400                                                                          
044600                                                                          
044700     IF MID-IDARTNR-IN NOT = ALL '+'                                      
044800        INSPECT MID-IDARTNR-IN REPLACING LEADING SPACE BY ZERO            
044900        IF MID-IDARTNR-IN NUMERIC                                         
045000           MOVE MID-IDARTNR-IN TO WS-IDARTNR                              
045100           IF MID-IDARTNR-IN NOT = ZERO                                   
045200              MOVE WS-IDARTNR     TO W-IDARTNR                            
045300           END-IF                                                         
045400        ELSE                                                              
045500           MOVE NEJ            TO NYCKLAR-SW                              
045510                                  ALLT-SW                                 
045600        END-IF                                                            
045700        MOVE MID-IDARTNR-IN    TO MOD-IDARTNR-UT                          
045800        INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE            
046000     ELSE                                                                 
046100        MOVE MFS-RENSA-FAELT   TO MOD-IDARTNR-UT                          
046200     END-IF                                                               
046300                                                                          
047000     IF NYCKLAR-FEL                                                       
047100        MOVE ERR-WRONG-KEY    TO MED-IDMFSFEL                             
047200                                 MSG-KOM-IDMFSMED                         
047300        CALL WMEDKONV USING MED-WMEDAREA                                  
047400        MOVE MED-MFSFEL       TO MOD-TEMFSFEL                             
047500        PERFORM MFS-RENSA-FAELT-IN                                        
047600        PERFORM MFS-RENSA-FAELT-UT                                        
047700     END-IF                                                               
047800     .                                                                    
047900                                                                          
048001                                                                          
048100 C-FOERSTA-SIDA SECTION.                                                  
048201     MOVE 'C-FOERSTA-SIDA  ' TO CURRENT-SECTION                           
048300                                                                          
048340                                                                          
048400     MOVE INF-FIRST-PAGE TO MED-IDMFSFEL                                  
048500                            MSG-KOM-IDMFSMED                              
048600     CALL WMEDKONV USING MED-WMEDAREA                                     
048700     MOVE MED-MFSFEL     TO MOD-TEMFSFEL                                  
048800                                                                          
048900     PERFORM MFS-RENSA-FAELT-IN                                           
049000     .                                                                    
049100                                                                          
049201                                                                          
049300 D-NAESTA-SIDA SECTION.                                                   
049401     MOVE 'D-NAESTA-SIDA   ' TO CURRENT-SECTION                           
049501                                                                          
049601     IF SPAR-IDTRANS = '4141'                                             
049700        MOVE SPAR-IDARTNR-NEXT      TO W-IDARTNR                          
050300     ELSE                                                                 
050400        PERFORM MFS-RENSA-FAELT-IN                                        
050500     END-IF                                                               
050600     .                                                                    
050700                                                                          
050801                                                                          
050900 E-SAMMA-SIDA SECTION.                                                    
051001     MOVE 'E-SAMMA-SIDA    ' TO CURRENT-SECTION                           
051101                                                                          
051201     IF SPAR-IDTRANS = '4141'                                             
051202        IF SPAR-IDARTNR-ENTER NUMERIC                                     
051301           MOVE SPAR-IDARTNR-ENTER  TO W-IDARTNR                          
051302        ELSE                                                              
051303           MOVE ZERO                TO W-IDARTNR                          
051304        END-IF                                                            
051901     END-IF                                                               
052001     MOVE JA  TO INDATA-SW                                                
052201                                                                          
062001     .                                                                    
062101                                                                          
062201                                                                          
080101 F-LAES-VISA-INFO SECTION.                                                
080201     MOVE 'F-LAES-VISA-INFO' TO CURRENT-SECTION                           
080301                                                                          
080402     PERFORM IMS-GU-WDR501                                                
080501                                                                          
080602     PERFORM IMS-GNP-WDGX4254                                             
080603     MOVE +1 TO INDX                                                      
080702     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
080703                   INDX > INDX-MAX                                        
080902        PERFORM FB-VISA-ARTIKEL                                           
087002                                                                          
087003        ADD +1 TO INDX                                                    
087102        PERFORM IMS-GNP-WDGX4254                                          
087402     END-PERFORM                                                          
087403                                                                          
087404     IF INDX NOT > INDX-MAX                                               
087406        PERFORM FA-RENSA-BILD                                             
087407     ELSE                                                                 
087408        MOVE 4254-IDARTNR         TO SPAR-IDARTNR-NEXT                    
087409        MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                         
087410                                     MSG-KOM-IDMFSMED                     
087411        CALL WMEDKONV USING MED-WMEDAREA                                  
087412        MOVE MED-TEMFSINF         TO MOD-TEMFSINF                         
087413     END-IF                                                               
087416                                                                          
087418     PERFORM FC-VISA-AKTIV-O-TOTAL                                        
087426                                                                          
087501                                                                          
087701     MOVE '002'                TO MSGI-KDCALL                             
087802     MOVE '4141'               TO SPAR-IDTRANS                            
087803                                                                          
087804     MOVE W-LATEST-UPD         TO MOD-TIAAMMDD-LAST                       
087806     MOVE W-ACTIVE-PARTS       TO MOD-KVANTAL-ACTIVE                      
087808     MOVE W-TOTAL-PARTS        TO MOD-KVANTAL-TOTAL                       
087901     MOVE SPAR-AREA            TO MSGI-SPAR-AREA                          
087902                                                                          
088101     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
088201     .                                                                    
088301                                                                          
088401                                                                          
088501 FA-RENSA-BILD SECTION.                                                   
088601     MOVE 'FA-RENSA-BILD   ' TO CURRENT-SECTION                           
088701                                                                          
088702*    MOVE +1 TO INDX                                                      
093001     PERFORM UNTIL INDX > INDX-MAX                                        
093101        MOVE MFS-RENSA-FAELT  TO MOD-IDARTNR      (INDX)                  
093102        MOVE MFS-RENSA-FAELT  TO MOD-IDUSER-ASTA  (INDX)                  
093103        MOVE MFS-RENSA-FAELT  TO MOD-TISTADAT     (INDX)                  
093104        MOVE MFS-RENSA-FAELT  TO MOD-TISTAMIN     (INDX)                  
093105        MOVE MFS-RENSA-FAELT  TO MOD-IDUSER-ASTO  (INDX)                  
093106        MOVE MFS-RENSA-FAELT  TO MOD-TISTODAT     (INDX)                  
093107        MOVE MFS-RENSA-FAELT  TO MOD-TISTOMIN     (INDX)                  
093301        MOVE MFS-STAENG-FAELT TO MOD-KDCMD-ATTR   (INDX)                  
093401        ADD +1 TO INDX                                                    
093501     END-PERFORM                                                          
094601     .                                                                    
094701                                                                          
094801                                                                          
094802 FB-VISA-ARTIKEL SECTION.                                                 
094803     MOVE 'FB-VISA-ARTIKEL ' TO CURRENT-SECTION                           
094804                                                                          
094805     MOVE 4254-IDARTNR       TO MOD-IDARTNR     (INDX)                    
094806     MOVE 4254-IDUSER-ASTA   TO MOD-IDUSER-ASTA (INDX)                    
094807     MOVE 4254-TISTADAT      TO MOD-TISTADAT    (INDX)                    
094808     MOVE 4254-TISTAMIN      TO WS-TID                                    
094809     MOVE WS-HH              TO WS-HH-RED                                 
094810     MOVE WS-MM              TO WS-MM-RED                                 
094811     MOVE WS-TID-RED         TO MOD-TISTAMIN    (INDX)                    
094812     IF 4254-TISTODAT > ZERO                                              
094813       MOVE 4254-IDUSER-ASTO TO MOD-IDUSER-ASTO (INDX)                    
094814       MOVE 4254-TISTODAT    TO MOD-TISTODAT    (INDX)                    
094820       MOVE 4254-TISTOMIN    TO WS-TID                                    
094830       MOVE WS-HH            TO WS-HH-RED                                 
094840       MOVE WS-MM            TO WS-MM-RED                                 
094850       MOVE WS-TID-RED       TO MOD-TISTOMIN    (INDX)                    
094851     ELSE                                                                 
094852       MOVE MFS-RENSA-FAELT  TO MOD-IDUSER-ASTO (INDX)                    
094853       MOVE MFS-RENSA-FAELT  TO MOD-TISTODAT    (INDX)                    
094854       MOVE MFS-RENSA-FAELT  TO MOD-TISTOMIN    (INDX)                    
094860     END-IF                                                               
095100     .                                                                    
095200                                                                          
095300                                                                          
095400 FC-VISA-AKTIV-O-TOTAL SECTION.                                           
095500     MOVE 'FC-VISA-AKT-O-TO' TO CURRENT-SECTION                           
095600                                                                          
095610     PERFORM IMS-GU-WDR501                                                
095611     MOVE ZERO             TO W-LATEST-UPD                                
095612                              W-ACTIVE-PARTS                              
095613                              W-TOTAL-PARTS                               
095620                                                                          
095621     MOVE ZERO TO W-IDARTNR                                               
095630     PERFORM IMS-GNP-WDGX4254                                             
095631     PERFORM UNTIL SEGMENT-SAKNAS                                         
096300                                                                          
096400        IF 4254-TISTADAT > W-LATEST-UPD                                   
096500           MOVE 4254-TISTADAT     TO W-LATEST-UPD                         
096600        END-IF                                                            
096610        IF 4254-TISTODAT > W-LATEST-UPD                                   
096620           MOVE 4254-TISTODAT     TO W-LATEST-UPD                         
096630        END-IF                                                            
096700                                                                          
096900        ADD +1                    TO W-TOTAL-PARTS                        
097000        IF 4254-TISTODAT = ZERO                                           
097100           ADD +1                 TO W-ACTIVE-PARTS                       
097200        END-IF                                                            
097210        PERFORM IMS-GNP-WDGX4254                                          
097220     END-PERFORM                                                          
097300     .                                                                    
097400                                                                          
097500                                                                          
131801 G-KOLLA-INPUT SECTION.                                                   
131901     MOVE 'G-KOLLA-INPUT   ' TO CURRENT-SECTION                           
132001                                                                          
132101     MOVE JA  TO INDATA-SW                                                
132201     MOVE JA  TO ALLT-SW                                                  
133501                                                                          
133502     PERFORM GA-KOLLA-INDATA-FINNS                                        
133503     IF INDATA-OK                                                         
133504     IF MID-IDARTNR-NEW NOT = ALL '+'                                     
133505        INSPECT MID-IDARTNR-NEW REPLACING LEADING SPACE BY ZERO           
133506        IF MID-IDARTNR-NEW NUMERIC                                        
133601           MOVE MID-IDARTNR-NEW    TO W-IDARTNR                           
133602           PERFORM IMS-GU-WDK601                                          
133603           IF SEGMENT-SAKNAS                                              
133604              MOVE MFS-NUM-FAELT-FEL  TO MOD-IDARTNR-NEW-ATTR             
133605              MOVE PART-MISSING-WDK6  TO MED-IDMFSINF                     
133606                                         MSG-KOM-IDMFSMED                 
133607              CALL WMEDKONV USING MED-WMEDAREA                            
133608              MOVE MED-MFSINF         TO MOD-TEMFSINF                     
133609              MOVE NEJ                TO INDATA-SW                        
133610                                         ALLT-SW                          
133611           ELSE                                                           
133612              PERFORM IMS-GHU-WDGX4254                                    
133613              IF SEGMENT-FINNS                                            
133614                MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                 
133615                MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDARTNR-NEW-ATTR         
133616                MOVE NEJ                  TO ALLT-SW                      
133617                MOVE PART-EXISTS          TO MED-IDMFSINF                 
133618                                           MSG-KOM-IDMFSMED               
133619                CALL WMEDKONV USING MED-WMEDAREA                          
133620                MOVE MED-MFSINF       TO MOD-TEMFSINF                     
133621                MOVE NEJ              TO INDATA-SW                        
133622                                         ALLT-SW                          
133623              ELSE                                                        
133624                MOVE MFS-NUM-FAELT-RAETT TO MOD-IDARTNR-NEW-ATTR          
133625              END-IF                                                      
133626           END-IF                                                         
133627        ELSE                                                              
133628           MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSINF                     
133629                                         MSG-KOM-IDMFSMED                 
133630           CALL WMEDKONV USING MED-WMEDAREA                               
133631           MOVE MED-MFSINF            TO MOD-TEMFSINF                     
133632           MOVE NEJ                   TO INDATA-SW                        
133633                                         ALLT-SW                          
133634        END-IF                                                            
133640     END-IF                                                               
134201                                                                          
134202     MOVE 1 TO INDX                                                       
134203     PERFORM UNTIL INDX > INDX-MAX                                        
134204        IF MID-KDCMD (INDX) NOT = '+'                                     
134205           MOVE MID-KDCMD (INDX)      TO W-CMD                            
134210           IF NOT W-OK-CMD                                                
134211            MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSINF                     
134212                                          MSG-KOM-IDMFSMED                
134213            MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMD-ATTR (INDX)            
134214            CALL WMEDKONV USING MED-WMEDAREA                              
134215            MOVE MED-MFSINF           TO MOD-TEMFSINF                     
134216            MOVE NEJ                  TO INDATA-SW                        
134217                                         ALLT-SW                          
134218           ELSE                                                           
134219            MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-ATTR (INDX)            
134220           END-IF                                                         
134221        END-IF                                                            
134222        ADD 1 TO INDX                                                     
134230     END-PERFORM                                                          
134240     END-IF                                                               
141201     .                                                                    
141301                                                                          
192101 GA-KOLLA-INDATA-FINNS SECTION.                                           
192201     MOVE 'GA-KOLLA-INDATA '   TO CURRENT-SECTION                         
192301                                                                          
192302     MOVE NEJ TO INDATA-SW                                                
192303     IF MID-IDARTNR-NEW NOT = ALL '+'                                     
192304        MOVE JA TO INDATA-SW                                              
192305     END-IF                                                               
192306     IF NOT INDATA-OK                                                     
192307        MOVE +1 TO INDX                                                   
192308        PERFORM UNTIL INDX > INDX-MAX OR                                  
192309                      INDATA-OK                                           
192310           IF MID-KDCMD (INDX) = '+' OR SPACE                             
192311              CONTINUE                                                    
192312           ELSE                                                           
192313              MOVE JA TO INDATA-SW                                        
192314           END-IF                                                         
192315           ADD +1 TO INDX                                                 
192316        END-PERFORM                                                       
192317     END-IF                                                               
192318                                                                          
192319     IF NOT INDATA-OK                                                     
192320       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
192321       CALL WMEDKONV USING MED-WMEDAREA                                   
192322       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
192323       MOVE NEJ TO ALLT-SW                                                
192324     END-IF                                                               
192325     .                                                                    
192326                                                                          
192327 H-UPPDATERA SECTION.                                                     
192328     MOVE 'H-UPPDATERA     '   TO CURRENT-SECTION                         
192329                                                                          
192330     IF MID-IDARTNR-NEW NOT = ALL '+'                                     
192331        SET NYARTIKEL-JA       TO TRUE                                    
192334        PERFORM HA-NY-ARTIKEL                                             
192338     END-IF                                                               
192339                                                                          
192340     MOVE 1 TO INDX                                                       
192341     PERFORM UNTIL INDX > INDX-MAX                                        
192342        IF MID-KDCMD (INDX) NOT = '+'                                     
192343           MOVE MID-KDCMD (INDX)      TO W-CMD                            
192344           PERFORM HB-UPPDATERA-ARTIKEL                                   
192345        END-IF                                                            
192346        ADD 1 TO INDX                                                     
192350     END-PERFORM                                                          
193301                                                                          
193302     IF NYARTIKEL-JA                                                      
193303       MOVE MFS-OPEN-NUM-FIELD TO MOD-IDARTNR-NEW-ATTR                    
193304     END-IF                                                               
193306                                                                          
193501     MOVE INF-UPDATE-DONE   TO MED-IDMFSINF                               
193601                                  MSG-KOM-IDMFSMED                        
193701     CALL WMEDKONV USING MED-WMEDAREA                                     
193801     MOVE MED-MFSINF        TO MOD-TEMFSINF                               
193901     PERFORM MFS-RENSA-FAELT-IN                                           
194101                                                                          
194201     PERFORM HI-SPARA-ENTER-NYCKLAR                                       
194301     .                                                                    
194401                                                                          
194501                                                                          
194502 HA-NY-ARTIKEL SECTION.                                                   
194503     MOVE 'HA-NY-ARTIKEL   '   TO CURRENT-SECTION                         
194504                                                                          
194506     MOVE MID-IDARTNR-NEW        TO WS-IDARTNR                            
194507     MOVE WS-IDARTNR             TO W-IDARTNR                             
194508                                    4254-IDARTNR                          
194509     MOVE MSGI-IDUSER     TO 4254-IDUSER-ASTA                             
194510     MOVE CURRENT-DATE    TO 4254-TISTADAT                                
194511     MOVE CURRENT-HH      TO WS-HH                                        
194512     MOVE CURRENT-MM      TO WS-MM                                        
194513     MOVE WS-TID          TO 4254-TISTAMIN                                
194514     MOVE SPACE           TO 4254-IDUSER-ASTO                             
194515     MOVE ZERO            TO 4254-TISTODAT                                
194516     MOVE ZERO            TO 4254-TISTOMIN                                
194517     PERFORM IMS-ISRT-WDGX4254                                            
194560     .                                                                    
194570                                                                          
194580                                                                          
194601 HB-UPPDATERA-ARTIKEL SECTION.                                            
194701     MOVE 'HB-UPD-ARTIKEL '   TO CURRENT-SECTION                          
194801                                                                          
194901*    MOVE 1 TO INDX                                                       
195001*    PERFORM UNTIL INDX > INDX-MAX                                        
195002        MOVE MID-IDARTNR (INDX) TO W-IDARTNR                              
195003        PERFORM IMS-GHU-WDGX4254                                          
195101        IF W-STOP                                                         
195102           MOVE MSGI-IDUSER     TO 4254-IDUSER-ASTO                       
195104           MOVE CURRENT-DATE    TO 4254-TISTODAT                          
195106           MOVE CURRENT-HH      TO WS-HH                                  
195107           MOVE CURRENT-MM      TO WS-MM                                  
195108           MOVE WS-TID          TO 4254-TISTOMIN                          
195109           PERFORM IMS-REPL-WDGX4254                                      
195201        ELSE                                                              
195301           IF W-ACTIVATE                                                  
195302              MOVE MSGI-IDUSER  TO 4254-IDUSER-ASTA                       
195303              MOVE CURRENT-DATE TO 4254-TISTADAT                          
195305              MOVE CURRENT-HH   TO WS-HH                                  
195306              MOVE CURRENT-MM   TO WS-MM                                  
195307              MOVE WS-TID       TO 4254-TISTAMIN                          
195308              IF 4254-TISTODAT > ZERO                                     
195309               MOVE MSGI-IDUSER TO 4254-IDUSER-ASTO                       
195310               MOVE ZERO        TO 4254-TISTODAT                          
195311              MOVE ZERO         TO 4254-TISTOMIN                          
195312              END-IF                                                      
195313              PERFORM IMS-REPL-WDGX4254                                   
195402           ELSE                                                           
195502              IF W-DELETE                                                 
195506                 PERFORM IMS-DLET-WDGX4254                                
195702              END-IF                                                      
195703           END-IF                                                         
195801        END-IF                                                            
195901*       ADD 1 TO INDX                                                     
196001*    END-PERFORM                                                          
196101     .                                                                    
196201                                                                          
196301                                                                          
217901 HI-SPARA-ENTER-NYCKLAR SECTION.                                          
218001     MOVE 'HI-SPARA-ENTER  '   TO CURRENT-SECTION                         
218101                                                                          
218102     IF MID-IDARTNR-NEW = ALL '+'                                         
218103        IF SPAR-IDARTNR-ENTER NUMERIC                                     
218201           MOVE SPAR-IDARTNR-ENTER TO W-IDARTNR                           
218202        ELSE                                                              
218203           MOVE ZERO                TO W-IDARTNR                          
218204        END-IF                                                            
218205     END-IF                                                               
218801                                                                          
218901     MOVE SPACE       TO MFS-KDTRTYP                                      
219001     MOVE '7'         TO MFS-IDPFK                                        
219101     .                                                                    
219201                                                                          
219301                                                                          
219302 I-KOLLA-ATT-EJ-UPDATE  SECTION.                                          
219303     MOVE 'I-KOLLA-EJ-UPF  '   TO CURRENT-SECTION                         
219304                                                                          
219305     IF MID-IDARTNR-NEW = ALL '+' OR SPACE                                
219306        CONTINUE                                                          
219307     ELSE                                                                 
219308        MOVE NEJ               TO ALLT-SW                                 
219309     END-IF                                                               
219310                                                                          
219311     MOVE +1 TO INDX                                                      
219312     PERFORM UNTIL INDX > INDX-MAX OR                                     
219313             NOT ALLT-OK                                                  
219314        IF MID-KDCMD (INDX) = '+' OR SPACE                                
219315           CONTINUE                                                       
219316        ELSE                                                              
219317           MOVE NEJ TO ALLT-SW                                            
219318        END-IF                                                            
219319        ADD +1 TO INDX                                                    
219320     END-PERFORM                                                          
219321                                                                          
219322     IF NOT ALLT-OK                                                       
219323        MOVE INF-PRESS-PF11    TO MED-IDMFSINF                            
219324        CALL WMEDKONV USING MED-WMEDAREA                                  
219325        MOVE MED-MFSINF        TO MOD-TEMFSINF                            
219326     END-IF                                                               
219327     .                                                                    
219328                                                                          
219330                                                                          
219401 MFS-RENSA-FAELT-UT SECTION.                                              
219501                                                                          
219601*    --- ALLA UTDATA-FÄLT                                                 
219701*    --- INKL. BLÄDDRINGSNYCKLAR                                          
219902     MOVE MFS-RENSA-FAELT TO MOD-TIAAMMDD-LAST                            
220102                             MOD-KVANTAL-ACTIVE                           
220302                             MOD-KVANTAL-TOTAL                            
220402                                                                          
221501     MOVE +1 TO MFS-INDX                                                  
221601     PERFORM UNTIL MFS-INDX > INDX-MAX                                    
221701       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
221801       ADD +1 TO MFS-INDX                                                 
221901     END-PERFORM                                                          
222001     .                                                                    
222101                                                                          
222201 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
222301                                                                          
222401*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
222502     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR        (MFS-INDX)                
222802                             MOD-IDUSER-ASTA    (MFS-INDX)                
223002                             MOD-TISTADAT       (MFS-INDX)                
223302                             MOD-TISTAMIN       (MFS-INDX)                
223402                             MOD-IDUSER-ASTO    (MFS-INDX)                
223602                             MOD-TISTODAT       (MFS-INDX)                
223802                             MOD-TISTOMIN       (MFS-INDX)                
223901     .                                                                    
224001                                                                          
224101 MFS-RENSA-FAELT-IN SECTION.                                              
224201                                                                          
224301*    --- ALLA INDATA-FÄLT                                                 
224403     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
224404                             MOD-IDARTNR-NEW                              
225701     MOVE +1 TO MFS-INDX                                                  
225801     PERFORM UNTIL MFS-INDX > INDX-MAX                                    
225901       PERFORM MFS-RENSA-RAD-FAELT-IN                                     
226001       ADD +1 TO MFS-INDX                                                 
226101     END-PERFORM                                                          
226201     .                                                                    
226301 MFS-RENSA-RAD-FAELT-IN  SECTION.                                         
226401                                                                          
226503     MOVE MFS-RENSA-FAELT   TO MOD-IDARTNR        (MFS-INDX)              
226703                               MOD-KDCMD          (MFS-INDX)              
226803                               MOD-IDUSER-ASTA    (MFS-INDX)              
226903                               MOD-TISTADAT       (MFS-INDX)              
227003                               MOD-TISTAMIN       (MFS-INDX)              
227103                               MOD-IDUSER-ASTO    (MFS-INDX)              
227203                               MOD-TISTODAT       (MFS-INDX)              
227303                               MOD-TISTOMIN       (MFS-INDX)              
227403     .                                                                    
227503                                                                          
227603                                                                          
227703 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
227803                                                                          
227903*    --- ALLA UTDATA-FÄLT                                                 
228003*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
228103     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-NEW                            
229603                                                                          
229703     MOVE +1 TO MFS-INDX                                                  
229803     PERFORM UNTIL MFS-INDX > INDX-MAX                                    
229903       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
230003       ADD +1 TO MFS-INDX                                                 
230103     END-PERFORM                                                          
230203     .                                                                    
230303                                                                          
230403 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
230503                                                                          
230603*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
230703     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR      (MFS-INDX)                
231703     .                                                                    
231803                                                                          
231903 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
232003                                                                          
232103*    --- ALLA INDATA-FÄLT                                                 
232203     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-IN                             
232303                                                                          
232403     MOVE +1 TO MFS-INDX                                                  
232503     PERFORM UNTIL MFS-INDX > INDX-MAX                                    
232603     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR        (MFS-INDX)              
232703                               MOD-KDCMD          (MFS-INDX)              
232803                               MOD-IDUSER-ASTA    (MFS-INDX)              
232903                               MOD-TISTADAT       (MFS-INDX)              
233003                               MOD-TISTAMIN       (MFS-INDX)              
233103                               MOD-IDUSER-ASTO    (MFS-INDX)              
233203                               MOD-TISTODAT       (MFS-INDX)              
233303                               MOD-TISTOMIN       (MFS-INDX)              
234403       ADD +1 TO MFS-INDX                                                 
234503     END-PERFORM                                                          
234603     .                                                                    
234703                                                                          
234803                                                                          
241303 MFS-FORM-ATTR SECTION.                                                   
241403                                                                          
241503*    --- ALLA INDATA-FÄLT                                                 
241603     MOVE MFS-FORMATETS-ATTR TO MOD-IDARTNR-NEW-ATTR                      
242703     .                                                                    
242803                                                                          
242903 MFS-LAES-IN-IGEN-E SECTION.                                              
243003                                                                          
243103*    --- ALLA INDATA-FÄLT                                                 
243203     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDARTNR-NEW-ATTR                   
244303     .                                                                    
244403                                                                          
244503                                                                          
244603* --- IMS SEKTIONER ---                                                   
244703                                                                          
244803 IMS-GET-MSG SECTION.                                                     
244903                                                                          
245003     MOVE '  QC' TO GODK-STATUSKODER                                      
245103     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
245203     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
245303     PERFORM IMS-STATUSKONTROLL                                           
245403     .                                                                    
245503                                                                          
245603                                                                          
245703 IMS-INSERT-MSG SECTION.                                                  
245803                                                                          
245903     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
246003     MOVE SPACE TO GODK-STATUSKODER                                       
246103     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
246203     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
246303     PERFORM IMS-STATUSKONTROLL                                           
246403     .                                                                    
246503                                                                          
246504 IMS-GU-WDR501   SECTION.                                                 
246505     MOVE 'IMS-GU-WDR501   '   TO CURRENT-IMS-SECTION                     
246506                                                                          
246507     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-X ')'                         
246508          DELIMITED BY SIZE INTO SSA1                                     
246509     MOVE '    '              TO GODK-STATUSKODER                         
246510     CALL CBLTDLI USING GU WDR5-PCB DLI-IO-WDGX01 SSA1                    
246511     MOVE WDR5-STATUS-CODE    TO STATUS-WS                                
246520     PERFORM IMS-STATUSKONTROLL                                           
246530     .                                                                    
246540                                                                          
246550 IMS-GNP-WDGX4254 SECTION.                                                
246551     MOVE 'IMS-GNP-WDGX4254'   TO CURRENT-IMS-SECTION                     
246560                                                                          
246570     STRING 'WDGX4254(IDARTNR =>' W-IDARTNR-X ')'                         
246580          DELIMITED BY SIZE INTO SSA1                                     
246590     MOVE '  GE'              TO GODK-STATUSKODER                         
246600     CALL CBLTDLI USING GNP WDR5-PCB DLI-IO-WDGX4254 SSA1                 
246700     MOVE WDR5-STATUS-CODE    TO STATUS-WS                                
246800     PERFORM IMS-STATUSKONTROLL                                           
246900     .                                                                    
247000                                                                          
247010 IMS-GHU-WDGX4254 SECTION.                                                
247011     MOVE 'IMS-GHU-WDGX4254'   TO CURRENT-IMS-SECTION                     
247020                                                                          
247021     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-X ')'                         
247022          DELIMITED BY SIZE INTO SSA1                                     
247030     STRING 'WDGX4254(IDARTNR = ' W-IDARTNR-X ')'                         
247040          DELIMITED BY SIZE INTO SSA2                                     
247050     MOVE '  GE'              TO GODK-STATUSKODER                         
247060     CALL CBLTDLI USING GHU WDR5-PCB DLI-IO-WDGX4254 SSA1 SSA2            
247070     MOVE WDR5-STATUS-CODE    TO STATUS-WS                                
247080     PERFORM IMS-STATUSKONTROLL                                           
247090     .                                                                    
247091                                                                          
247092 IMS-ISRT-WDGX4254 SECTION.                                               
247093     MOVE 'IMS-ISRTWDGX4254'   TO CURRENT-IMS-SECTION                     
247094                                                                          
247095     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-X ')'                         
247096          DELIMITED BY SIZE INTO SSA1                                     
247097     MOVE 'WDGX4254 '         TO SSA2                                     
247098     MOVE '    '              TO GODK-STATUSKODER                         
247099     CALL CBLTDLI USING ISRT WDR5-PCB DLI-IO-WDGX4254 SSA1 SSA2           
247100     MOVE WDR5-STATUS-CODE    TO STATUS-WS                                
247101     PERFORM IMS-STATUSKONTROLL                                           
247102     .                                                                    
247103                                                                          
247110 IMS-REPL-WDGX4254 SECTION.                                               
247120     MOVE 'IMS-REPLWDGX4254'   TO CURRENT-IMS-SECTION                     
247200                                                                          
247300     MOVE '  '                TO GODK-STATUSKODER                         
247400     CALL CBLTDLI USING REPL WDR5-PCB DLI-IO-WDGX4254                     
247500     MOVE WDR5-STATUS-CODE    TO STATUS-WS                                
247501     PERFORM IMS-STATUSKONTROLL                                           
247502     .                                                                    
247503                                                                          
247504 IMS-DLET-WDGX4254 SECTION.                                               
247505     MOVE 'IMS-DLETWDGX4254'   TO CURRENT-IMS-SECTION                     
247506                                                                          
247507     MOVE '  '                TO GODK-STATUSKODER                         
247508     CALL CBLTDLI USING DLET WDR5-PCB DLI-IO-WDGX4254                     
247509     MOVE WDR5-STATUS-CODE    TO STATUS-WS                                
247510     PERFORM IMS-STATUSKONTROLL                                           
247511     .                                                                    
247512                                                                          
247513                                                                          
247520 IMS-GU-WDK601 SECTION.                                                   
247603     MOVE 'IMS-GU-WDK601   '   TO CURRENT-IMS-SECTION                     
247703                                                                          
247803     MOVE SPACE                TO ALL-SSA                                 
247903                                                                          
248003     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
248103          DELIMITED BY SIZE  INTO SSA1                                    
248203     MOVE '  GE'               TO GODK-STATUSKODER                        
248303     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
248403     MOVE WDK6-STATUS-CODE     TO STATUS-WS                               
248503     PERFORM IMS-STATUSKONTROLL                                           
248603     .                                                                    
248703                                                                          
249003                                                                          
296601                                                                          
296701                                                                          
296801 IMS-STATUSKONTROLL SECTION.                                              
296901                                                                          
297001     SET STATUS-IX TO 1                                                   
297101     SEARCH GODK-STATUS                                                   
297200       AT END                                                             
297300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
297400         DELIMITED BY SIZE INTO FELTEXT                                   
297500         CALL FELLOG                                                      
297600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
297700         CONTINUE                                                         
297800     END-SEARCH                                                           
297900     .                                                                    
