000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6020410.                                                
000400*AUTHOR.         ELAINE CURTSSON / RAHUL REDDY.                           
000500*DATE-WRITTEN.   AUGUSTI 91 / APRIL 2012                                  
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        KONTROLLRAPPORT PACKNING                                         
001100*        HANDLES THE BUSINESS LOGIC FOR THE SCREEN 6204                   
001200*        CALLED FROM W6020400 (MFS) AND W6W20400 (WEB).                   
001300*                                                                         
001400*        PROGRAMMET UPPATERAR W6KVAE (W6H7)                               
001500*        PROGRAMMET LÄSER     WLARTC (WDK6)                               
001600*        PROGRAMMET LÄSER     WLBENA (WDD3)                               
001700*        PROGRAMMET LÄSER     WLLEVA (WDF1)                               
001800*        PROGRAMMET LÄSER     WLEMBB (WDK5)                               
001900*        PROGRAMMET LÄSER     WDP3                                        
002000*        PROGRAMMET ANROPAR W602KRUP SOM AUTOMAT-                         
002100*        FAKTURERAR KR.                                                   
002200*                                                                         
002300*    INDATA.                                                              
002400*       REQU:         W60204I1                                            
002500*                                                                         
002600*    UTDATA.                                                              
002700*       RESP:         W60204O1                                            
002800                                                                          
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300 WORKING-STORAGE SECTION.                                                 
003400                                                                          
003500*    -- CHECKED BY WY2000                                                 
003600 77  IDPGM                       PIC X(08)   VALUE 'W6020410'.            
003700                                                                          
003800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004000                                                                          
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300 01  ALL-SPACE.                                                           
004400     03 FILLER                   PIC X(50)   VALUE SPACE.                 
004500 01  ALL-PLUS.                                                            
004600     03 FILLER                   PIC X(50)   VALUE ALL '+'.               
004700 01  ALL-UTF8-SPACE.                                                      
004800     03 FILLER                   PIC X(50)   VALUE ALL X'20'.             
004900 01  ALL-UTF8-PLUS.                                                       
005000     03 FILLER                   PIC X(50)   VALUE ALL X'2B'.             
005100                                                                          
005200*    --- INDEX FÖR BLÄDDRINGSRADER                                        
005300 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
005400                                                                          
005500*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005600 77  WS-IDKR                     PIC X(5)    VALUE SPACE.                 
005700 77  WS-KDPERSON                 PIC X(3)    VALUE SPACE.                 
005800 77  WS-VKKOLLIB                 PIC S9(6)V9 VALUE 0 COMP-3.              
005900 77  WS-KVKRPACK                 PIC 9(2)V9  VALUE 0.                     
006000 77  WS-BENAEMN                  PIC X(25)   VALUE SPACE.                 
006100 77  WS-DAREGDAT                 PIC 9(8)    VALUE ZERO.                  
006200                                                                          
006300 77  DATUM                       PIC 9(6)    VALUE ZERO.                  
006400                                                                          
006500 77  NAGOT-ANDRAT-SW             PIC X       VALUE 'N'.                   
006600     88  NAGOT-ANDRAT                        VALUE 'J'.                   
006700                                                                          
006800 77  TRYCK-PF11-SW               PIC X       VALUE 'N'.                   
006900     88  TRYCK-PF11                          VALUE 'J'.                   
007000                                                                          
007100 77  INDATA-SW                   PIC X       VALUE 'J'.                   
007200     88  INDATA-OK                           VALUE 'J'.                   
007300     88  INDATA-FEL                          VALUE 'N'.                   
007400                                                                          
007500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007600     88  NYCKLAR-OK                          VALUE 'J'.                   
007700     88  NYCKLAR-FEL                         VALUE 'N'.                   
007800                                                                          
007900 77  ALLT-SW                     PIC X       VALUE 'J'.                   
008000     88  ALLT-OK                             VALUE 'J'.                   
008100                                                                          
008200 77  WDF11-SAKNAS-SW             PIC X       VALUE 'N'.                   
008300     88  WDF11-FEL                           VALUE 'J'.                   
008400                                                                          
008500 77  KOLLI-FINNS-SW              PIC X       VALUE 'J'.                   
008600     88  KOLLI-FINNS                         VALUE 'J'.                   
008700     88  KOLLI-SAKNAS                        VALUE 'N'.                   
008800                                                                          
008900     EJECT                                                                
009000 01  TEST-IDLEVNR                PIC X(5).                                
009100*01  FILLER -COPY WWLEVHF -RED TEST-IDLEVNR.                              
009200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
009300 01  GENERELLA-SUBPROGRAM.                                                
009400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009600     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
009700     03  W602KRUP                PIC X(8)    VALUE 'W602KRUP'.            
009800     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
009900     EJECT                                                                
010000*01  -COPY WDECAREA                                                       
010100     EJECT                                                                
010200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010300     SKIP3                                                                
010400*01  -COPY WMFSAREA                                                       
010500     EJECT                                                                
010600*    ---  COPYTEXT FÖR TRANS TILL W602KRUP                                
010700*01  -COPY W602KRUP                                                       
010800     EJECT                                                                
010900 01  MESSAGE-CODES.                                                       
011000     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '020'.                 
011100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
011200     03  INF-PF11-AND-NO-DATA    PIC X(3)    VALUE '014'.                 
011300     03  INF-ARTIKEL-SAKNAS      PIC X(3)    VALUE '025'.                 
011400     03  INF-PRESS-PF11          PIC X(3)    VALUE '013'.                 
011500     03  INF-UPDATE-DONE         PIC X(3)    VALUE '001'.                 
011600     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '011'.                 
011700     03  INF-UPPDAT-OTILLATET    PIC X(3)    VALUE '007'.                 
011800     03  INF-INGET-ANDRAT        PIC X(3)    VALUE '004'.                 
011900     03  INF-FARLIGT-GODS        PIC X(3)    VALUE '362'.                 
012000     03  ERR-VERNR-OVERSKRIDEN   PIC X(3)    VALUE '361'.                 
012100     03  VALUTAKOD-SAKNAS        PIC X(3)    VALUE '025'.                 
012200     EJECT                                                                
012210 01  FILLER                      PIC X(16)   VALUE 'DC CODES   '.         
012220     SKIP3                                                                
012230*   -COPY WWDC99                                                          
012260                                                                          
012300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012400*                                                                         
012500     EJECT                                                                
012600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012700     SKIP3                                                                
012800 01  FILLER                  PIC X(16)  VALUE 'WTRAUTF8-AREA   '.         
012900*01  -COPY WTRAUTF8                                                       
013000                                                                          
013100 01  WS-IDSKYLT-SE               PIC X(3) VALUE 'S  '.                    
013200 01  WS-IDSKYLT-GB               PIC X(3) VALUE 'GB '.                    
013300 01  WS-IDSKYLT-CN               PIC X(3) VALUE 'RCN'.                    
013400 01  WS-CP-UNICODE               PIC X(4)  VALUE 'UTF8'.                  
013500 01  WS-CP-EBCDIC                PIC X(3)  VALUE '278'.                   
013600                                                                          
013700 01  NYCKLAR-TILL-DLI.                                                    
013800     03  W-IDKR-X.                                                        
013900         05  W-IDKR              PIC 9(5)    VALUE ZERO.                  
014000     03  W-IDKOLLI-X.                                                     
014100         05  W-IDKOLLI           PIC S9(5)   VALUE ZERO COMP-3.           
014200     03  W-IDARTNR-X.                                                     
014300         05  W-IDARTNR           PIC S9(9)   COMP-3 VALUE ZERO.           
014400     03  W-KDSEGKEY-X.                                                    
014500         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
014600     03  W-IDSKYLT-X.                                                     
014700         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
014800     03  W-IDLEVNR-X.                                                     
014900         05  W-IDLEVNR           PIC  X(5)   VALUE SPACE.                 
015000     03  W-IDLAND-X.                                                      
015100         05  W-IDLAND            PIC  X(2)   VALUE SPACE.                 
015200     03  W-IDLEVG-X.                                                      
015300         05  W-IDLEVG            PIC S9(5)   COMP-3 VALUE ZERO.           
015400     03  W-KDKOLLI-X.                                                     
015500         05  W-KDKOLLI           PIC X(8)    VALUE SPACE.                 
015600     03  W-KDARBTYP-X.                                                    
015700         05  W-KDARBTYP          PIC X(8)    VALUE 'QUAL    '.            
015800     03  W-IDPERSON-X.                                                    
015900         05  W-IDPERSON          PIC S9(3)   VALUE +0 COMP-3.             
           03  W-IDDC-B6-X.                                                     
               05 W-IDDC-B6            PIC X(2).                                
016000     SKIP2                                                                
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
       01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDB601'.        
       01  DLI-IO-WDB601.                                                       
      *    03  -COPY WDB601                                                     
           EJECT                                                                
017700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA1'.        
017800     SKIP3                                                                
017900 01  DLI-IO-AREA1.                                                        
018000     03  IO-AREA1                PIC X(520)  VALUE SPACE.                 
018100     03  W6KVAE01 REDEFINES IO-AREA1.                                     
018200*        05  -COPY W6H701                                                 
018300     SKIP3                                                                
018400     03  W6KVAE11 REDEFINES IO-AREA1.                                     
018500*        05  -COPY W6H711                                                 
018600     EJECT                                                                
018700 01  DLI-IO-W6H712.                                                       
018800     03  IO-W6H712               PIC X(520)  VALUE SPACE.                 
018900     03  W6H712 REDEFINES IO-W6H712.                                      
019000*        05  -COPY W6H712                                                 
019100     SKIP3                                                                
019200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
019300     SKIP3                                                                
019400 01  DLI-IO-AREA2.                                                        
019500     03  IO-AREA2                PIC X(150)  VALUE SPACE.                 
019600     03  WLBENA11 REDEFINES IO-AREA2.                                     
019700*        05  -COPY WDD311                                                 
019800     EJECT                                                                
019900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA3'.        
020000     SKIP3                                                                
020100 01  DLI-IO-AREA3.                                                        
020200     03  IO-AREA3                PIC X(300)  VALUE SPACE.                 
020300     03  WLLEVA01 REDEFINES IO-AREA3.                                     
020400*        05  -COPY WDF101                                                 
020500     SKIP3                                                                
020600     03 WLLEVA11 REDEFINES IO-AREA3.                                      
020700*        05 -COPY WDF102                                                  
020800     SKIP3                                                                
020900     03  WLLEVA14 REDEFINES IO-AREA3.                                     
021000*        05  -COPY WDF106                                                 
021100     EJECT                                                                
021200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA4'.        
021300     SKIP3                                                                
021400 01  DLI-IO-AREA4.                                                        
021500     03  IO-AREA4                PIC X(150)  VALUE SPACE.                 
021600     SKIP3                                                                
021700     03  WLEMBB01 REDEFINES IO-AREA4.                                     
021800*        05  -COPY WDK501                                                 
021900     EJECT                                                                
022000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-P311'.         
022100     SKIP3                                                                
022200 01  DLI-IO-P311.                                                         
022300*    03  -COPY WDP311                                                     
022400     EJECT                                                                
022500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA6'.        
022600     SKIP3                                                                
022700 01  DLI-IO-AREA6.                                                        
022800     03  IO-AREA6                PIC X(900)  VALUE SPACE.                 
022900     03  WLARTC11 REDEFINES IO-AREA6.                                     
023000*        05  -COPY WDK611                                                 
023100     EJECT                                                                
023200 LINKAGE SECTION.                                                         
023300                                                                          
023400 01  REQU-AREA.                                                           
023500*    03 -COPY WZ01REQU                                                    
023600*    03 -COPY W60204I1                                                    
023700     EJECT                                                                
023800 01  RESP-AREA.                                                           
023900*    03 -COPY WZ01RESP                                                    
024000*    03 -COPY W60204O1                                                    
024100     EJECT                                                                
024200 01  MAX-KVRADER                 PIC S9(4) COMP.                          
024300*01  -COPY W0009   -PRE MSG-                                              
024400     EJECT                                                                
024500*01  -COPY W0008  -PRE KVAE-                                              
024600     05  FILLER                  PIC X.                                   
024700     EJECT                                                                
024800*01  -COPY W0008  -PRE BENA-                                              
024900     05  FILLER                  PIC X.                                   
025000     EJECT                                                                
025100*01  -COPY W0008  -PRE LEVA-                                              
025200     05  FILLER                  PIC X.                                   
025300     EJECT                                                                
025400*01  -COPY W0008  -PRE EMBB-                                              
025500     05  FILLER                  PIC X.                                   
025600     EJECT                                                                
025700*01  -COPY W0008  -PRE WDP3-                                              
025800     05  FILLER                  PIC X.                                   
025900     EJECT                                                                
026000*01  -COPY W0008  -PRE ARTC-                                              
026100     05  FILLER                  PIC X.                                   
026200     EJECT                                                                
027740*01  -COPY W0008  -PRE WDB6-                                              
027750     05  FILLER                  PIC X.                                   
027760     EJECT                                                                
026300*01  -COPY W0008  -PRE W6H7-                                              
026400     05  FILLER                  PIC X.                                   
026500     EJECT                                                                
026600*01  -COPY W0008  -PRE WDG2-                                              
026700     05  FILLER                  PIC X.                                   
026800     EJECT                                                                
026900*01  -COPY W0008  -PRE WDK6-                                              
027000     05  FILLER                  PIC X.                                   
027100     EJECT                                                                
027200*01  -COPY W0008  -PRE LOPB-                                              
027300     05  FILLER                  PIC X.                                   
027400     EJECT                                                                
027500*01  -COPY W0008  -PRE FILC-                                              
027600     05  FILLER                  PIC X.                                   
027700     EJECT                                                                
027710*01  -COPY W0008  -PRE WDK7-                                              
027720     05  FILLER                  PIC X.                                   
027730     EJECT                                                                
       01  KRUP-WDB6-PCB               PIC X.                                   
           EJECT                                                                
027800 PROCEDURE DIVISION  USING                                                
027900     REQU-AREA RESP-AREA MAX-KVRADER                                      
028000     MSG-PCB  KVAE-PCB BENA-PCB                                           
028100     LEVA-PCB EMBB-PCB WDP3-PCB                                           
028200     ARTC-PCB WDB6-PCB                                                    
028200     W6H7-PCB WDG2-PCB                                                    
028300     WDK6-PCB LOPB-PCB FILC-PCB                                           
028310     WDK7-PCB KRUP-WDB6-PCB.                                              
028400                                                                          
028500     PERFORM A-INIT                                                       
028600     PERFORM B-KOLLA-NYCKLAR                                              
028700     IF NYCKLAR-OK                                                        
028800       IF REQU-UPDATE                                                     
028900         PERFORM G-KOLLA-INPUT                                            
029000         IF INDATA-OK                                                     
029100           PERFORM H-UPPDATERA                                            
029200         END-IF                                                           
029300       ELSE                                                               
029400         IF REQU-FIRST                                                    
029500           PERFORM C-FOERSTA-SIDA                                         
029600         ELSE                                                             
029700           IF REQU-NEXT                                                   
029800             PERFORM D-NAESTA-SIDA                                        
029900           ELSE                                                           
030000             PERFORM E-SAMMA-SIDA                                         
030100           END-IF                                                         
030200         END-IF                                                           
030300       END-IF                                                             
030400       IF ALLT-OK                                                         
030500         PERFORM F-LAES-VISA-INFO                                         
030600       END-IF                                                             
030700     END-IF                                                               
030800                                                                          
030900     GOBACK                                                               
031000     .                                                                    
031100     EJECT                                                                
031200 A-INIT SECTION.                                                          
031300     MOVE ALL '+'                TO RESP-W60204O1                         
031400                                                                          
031500     PERFORM MFS-FORM-ATTR                                                
031600                                                                          
031700     MOVE 001                    TO RESP-IDMSGVER                         
031800     MOVE SPACE                  TO RESP-IDMSG-ERROR                      
031900                                    RESP-IDMSG-INFO                       
032000                                    RESP-IDELMT-ERROR                     
032100                                                                          
032200     MOVE REQU-KVRADER           TO RESP-KVRADER                          
032300     ACCEPT DATUM              FROM DATE                                  
032400     .                                                                    
032500     EJECT                                                                
032600 B-KOLLA-NYCKLAR SECTION.                                                 
032700     MOVE JA                     TO NYCKLAR-SW                            
032800                                                                          
032900     IF REQU-IDSPRAK = 'SV'                                               
033000       MOVE 'S  '                TO W-IDSKYLT                             
033100     ELSE                                                                 
033200       MOVE 'GB '                TO W-IDSKYLT                             
033300     END-IF                                                               
033400     IF REQU-IDKR-KEY NUMERIC AND                                         
033500        REQU-IDKR-KEY > ZERO                                              
033600       MOVE REQU-IDKR-KEY        TO W-IDKR                                
033700                                    WS-IDKR                               
033800     ELSE                                                                 
033900       MOVE NEJ                  TO NYCKLAR-SW                            
034000       MOVE 'IDKR'               TO RESP-IDELMT-ERROR                     
034100     END-IF                                                               
034200                                                                          
034300     IF REQU-IDKOLLINR-KEY NUMERIC                                        
034400       IF REQU-IDKOLLINR-KEY > ZERO                                       
034500         MOVE REQU-IDKOLLINR-KEY TO W-IDKOLLI                             
034600       END-IF                                                             
034700     ELSE                                                                 
034800       MOVE NEJ                  TO NYCKLAR-SW                            
034900       MOVE 'IDKOLLI'            TO RESP-IDELMT-ERROR                     
035000     END-IF                                                               
035100                                                                          
035200     IF NYCKLAR-FEL                                                       
035300       MOVE ERR-WRONG-KEY        TO RESP-IDMSG-ERROR                      
035400       MOVE ZERO                 TO RESP-KVRADER                          
035500       PERFORM MFS-RENSA-FAELT-IN                                         
035600       PERFORM MFS-RENSA-FAELT-UT                                         
           ELSE                                                                 
             MOVE REQU-IDDC-KEY TO W-IDDC-B6                                    
             PERFORM IMS-GU-WDB601                                              
035700     END-IF                                                               
035800     .                                                                    
035900     EJECT                                                                
036000 C-FOERSTA-SIDA SECTION.                                                  
036100     IF REQU-IDKOLLINR-KEY = ALL '+' OR SPACE                             
036200       MOVE ZERO                 TO W-IDKOLLI                             
036300     END-IF                                                               
036400     MOVE JA                     TO ALLT-SW                               
036500     .                                                                    
036600     EJECT                                                                
036700 D-NAESTA-SIDA SECTION.                                                   
036800     MOVE REQU-IDKOLLI-START     TO W-IDKOLLI                             
036900     MOVE JA                     TO ALLT-SW                               
037000     .                                                                    
037100     EJECT                                                                
037200 E-SAMMA-SIDA SECTION.                                                    
037300     MOVE REQU-IDKOLLI-START     TO W-IDKOLLI                             
037400     IF REQU-W60204I1-001-GRP = ALL '+'                                   
037500       PERFORM MFS-RENSA-FAELT-IN                                         
037600     ELSE                                                                 
037700       MOVE JA                   TO TRYCK-PF11-SW                         
037800       MOVE INF-PRESS-PF11       TO RESP-IDMSG-INFO                       
038000       PERFORM EA-MID-INDATA-TILL-MOD                                     
038100       PERFORM IMS-GU-W6KVAE01                                            
038200       IF KR-KDKRSTA = '2' OR '3'                                         
038300         CONTINUE                                                         
038400       ELSE                                                               
038500         MOVE INF-UPPDAT-OTILLATET                                        
038600                                 TO RESP-IDMSG-INFO                       
038700         PERFORM MFS-SPAERRA-FAELT                                        
038800       END-IF                                                             
038900     END-IF                                                               
039000     .                                                                    
039100     EJECT                                                                
039200 EA-MID-INDATA-TILL-MOD SECTION.                                          
039300     IF REQU-KDKOLLI-UPD NOT = ALL '+'                                    
039400       MOVE REQU-KDKOLLI-UPD     TO RESP-KDKOLLI-UPD                      
039500       MOVE MFS-ADD-LAES-IN-FAELT                                         
039600                                 TO RESP-KDKOLLI-UPD-ATTR                 
039700     ELSE                                                                 
039800       MOVE ALL-SPACE            TO RESP-KDKOLLI-UPD                      
039900     END-IF                                                               
040000                                                                          
040100     IF REQU-IDKOLLI-UPD NOT = ALL '+'                                    
040200       MOVE REQU-IDKOLLI-UPD     TO RESP-IDKOLLI-UPD                      
040300       MOVE MFS-ADD-LAES-IN-FAELT                                         
040400                                 TO RESP-IDKOLLI-UPD-ATTR                 
040500     ELSE                                                                 
040600       MOVE ALL-SPACE            TO RESP-IDKOLLI-UPD                      
040700     END-IF                                                               
040800                                                                          
040900     IF REQU-VKKOLLIB-UPD NOT = ALL '+'                                   
041000       MOVE REQU-VKKOLLIB-UPD    TO DEC-IDFRIDATA                         
041100       MOVE 5                    TO DEC-KVHELTAL                          
041200       MOVE 1                    TO DEC-KVDECIMAL                         
041300       CALL WDECEDIT          USING DEC-WDECAREA                          
041400       IF DEC-KDSVAR-OK                                                   
041500         MOVE DEC-IDEDITDATA     TO RESP-VKKOLLIB-UPD                     
041600         MOVE MFS-ADD-LAES-IN-FAELT                                       
041700                                 TO RESP-VKKOLLIB-UPD-ATTR                
041800       ELSE                                                               
041900         MOVE ALL-SPACE          TO RESP-VKKOLLIB-UPD                     
042000       END-IF                                                             
042100     ELSE                                                                 
042200       MOVE ALL-SPACE            TO RESP-VKKOLLIB-UPD                     
042300     END-IF                                                               
042400                                                                          
042500     IF REQU-DIKOLLIL-UPD NOT = ALL '+' AND                               
042600        REQU-DIKOLLIL-UPD NUMERIC       AND                               
042700        REQU-DIKOLLIL-UPD > ZERO                                          
042800       MOVE REQU-DIKOLLIL-UPD    TO RESP-DIKOLLIL-UPD                     
042900       MOVE MFS-ADD-LAES-IN-FAELT                                         
043000                                 TO RESP-DIKOLLIL-UPD-ATTR                
043100     ELSE                                                                 
043200       MOVE ALL-SPACE            TO RESP-DIKOLLIL-UPD                     
043300     END-IF                                                               
043400                                                                          
043500     IF REQU-DIKOLLIB-UPD NOT = ALL '+' AND                               
043600        REQU-DIKOLLIB-UPD NUMERIC       AND                               
043700        REQU-DIKOLLIB-UPD > ZERO                                          
043800       MOVE REQU-DIKOLLIB-UPD    TO RESP-DIKOLLIB-UPD                     
043900       MOVE MFS-ADD-LAES-IN-FAELT                                         
044000                                 TO RESP-DIKOLLIB-UPD-ATTR                
044100     ELSE                                                                 
044200       MOVE ALL-SPACE            TO RESP-DIKOLLIB-UPD                     
044300     END-IF                                                               
044400                                                                          
044500     IF REQU-DIKOLLIH-UPD NOT = ALL '+' AND                               
044600        REQU-DIKOLLIH-UPD NUMERIC       AND                               
044700        REQU-DIKOLLIH-UPD > ZERO                                          
044800       MOVE REQU-DIKOLLIH-UPD    TO RESP-DIKOLLIH-UPD                     
044900       MOVE MFS-ADD-LAES-IN-FAELT                                         
045000                                 TO RESP-DIKOLLIH-UPD-ATTR                
045100     ELSE                                                                 
045200       MOVE ALL-SPACE            TO RESP-DIKOLLIH-UPD                     
045300     END-IF                                                               
045400                                                                          
045500     IF REQU-KDPERSON-UPD NOT = ALL '+'                                   
045600       MOVE REQU-KDPERSON-UPD    TO RESP-KDPERSON-UPD                     
045700       MOVE MFS-ADD-LAES-IN-FAELT                                         
045800                                 TO RESP-KDPERSON-UPD-ATTR                
045900     ELSE                                                                 
046000       MOVE ALL-SPACE            TO RESP-KDPERSON-UPD                     
046100     END-IF                                                               
046200                                                                          
046300     IF REQU-KDCMD-UPD NOT = ALL '+'                                      
046400       MOVE REQU-KDCMD-UPD       TO RESP-KDCMD-UPD                        
046500       MOVE MFS-ADD-LAES-IN-FAELT                                         
046600                                 TO RESP-KDCMD-UPD-ATTR                   
046700     ELSE                                                                 
046800       MOVE ALL-SPACE            TO RESP-KDCMD-UPD                        
046900     END-IF                                                               
047000     .                                                                    
047100     EJECT                                                                
047200 F-LAES-VISA-INFO SECTION.                                                
047300     MOVE ZERO                   TO RESP-KVRADER                          
047400     PERFORM IMS-GU-W6KVAE01                                              
047500                                                                          
047600     IF SEGMENT-SAKNAS                                                    
047700       MOVE ERR-WRONG-KEY        TO RESP-IDMSG-ERROR                      
047800       MOVE 'IDKR'               TO RESP-IDELMT-ERROR                     
047900       PERFORM MFS-RENSA-FAELT-UT                                         
048000     ELSE                                                                 
048100       PERFORM FA-VISA-W6KVAE01-INFO                                      
048200       PERFORM FB-LAES-VISA-LEV-INFO                                      
048300       PERFORM FC-LAES-VISA-BEN-INFO                                      
048400       PERFORM FD-KOLLA-OM-FARLIGT-GODS                                   
048500       MOVE +1                   TO INDX                                  
048600       PERFORM IMS-GNP-W6KVAE11-NEXT                                      
048700       IF SEGMENT-FINNS                                                   
048800         MOVE KOLL-IDKOLLI       TO RESP-IDKOLLI-START                    
048900       ELSE                                                               
049000         IF REQU-IDKOLLINR-KEY NOT = ALL '+' AND                          
049100            REQU-IDKOLLINR-KEY NOT = SPACE   AND                          
049200            REQU-IDKOLLINR-KEY NOT = ZERO                                 
049300           MOVE ERR-WRONG-KEY    TO RESP-IDMSG-ERROR                      
049400           MOVE 'IDKOLLI'        TO RESP-IDELMT-ERROR                     
049500           PERFORM MFS-RENSA-FAELT-UT                                     
049600           MOVE W-IDKOLLI        TO RESP-IDKOLLI-START                    
049700           MOVE ALL-SPACE        TO RESP-IDMSG-INFO                       
049800         ELSE                                                             
049900           MOVE SPACE            TO RESP-IDKOLLI-START                    
050000         END-IF                                                           
050100       END-IF                                                             
050200                                                                          
050300       PERFORM UNTIL INDX > MAX-KVRADER                                   
050400         IF SEGMENT-FINNS                                                 
050500           MOVE KOLL-IDKOLLI     TO RESP-IDKOLLI-LINE (INDX)              
050600           MOVE KOLL-VKKOLLIB    TO RESP-VKKOLLIB-LINE (INDX)             
050700           MOVE KOLL-DIKOLLIL    TO RESP-DIKOLLIL-LINE (INDX)             
050800           MOVE KOLL-DIKOLLIB    TO RESP-DIKOLLIB-LINE (INDX)             
050900           MOVE KOLL-DIKOLLIH    TO RESP-DIKOLLIH-LINE (INDX)             
051000           PERFORM IMS-GNP-W6KVAE11-NEXT                                  
051100           ADD 1                 TO INDX                                  
051200                                    RESP-KVRADER                          
051300         ELSE                                                             
051400           COMPUTE INDX = MAX-KVRADER + 1                                 
051500         END-IF                                                           
051600       END-PERFORM                                                        
051700                                                                          
051800       IF SEGMENT-FINNS                                                   
051900         MOVE KOLL-IDKOLLI       TO RESP-IDKOLLI-NEXT                     
052000         MOVE INF-MORE-INFO-EXISTS                                        
052100                                 TO RESP-IDMSG-INFO                       
052200       ELSE                                                               
052300         MOVE SPACE              TO RESP-IDKOLLI-NEXT                     
052400       END-IF                                                             
052500                                                                          
052600       IF KR-KDKRSTA = '2' OR '3'                                         
052700         IF TRYCK-PF11                                                    
052800           CONTINUE                                                       
052900         ELSE                                                             
053000           PERFORM MFS-RENSA-FAELT-IN                                     
053100         END-IF                                                           
053200       ELSE                                                               
053300         IF NOT REQU-UPDATE                                               
053400           MOVE INF-UPPDAT-OTILLATET                                      
053500                                 TO RESP-IDMSG-INFO                       
053600           PERFORM MFS-SPAERRA-FAELT                                      
053700         END-IF                                                           
053800       END-IF                                                             
053900     END-IF                                                               
054000     .                                                                    
054100     EJECT                                                                
054200 FA-VISA-W6KVAE01-INFO SECTION.                                           
054300     MOVE KR-IDARTNR             TO RESP-IDARTNR                          
054400     MOVE KR-KDKRSTA             TO RESP-KDKRSTA                          
054500     COMPUTE WS-DAREGDAT = 99999999 - KR-DAREGDAT-9KOMPL                  
054600     MOVE WS-DAREGDAT (3:6)      TO RESP-TIREGDAT                         
054700     MOVE KR-IDLEVNR             TO RESP-IDLEVNR                          
054800     MOVE KR-IDLEVG              TO RESP-IDLEVG                           
054900     MOVE KR-TIKRPACK            TO RESP-TIKRPACK                         
055000     IF REQU-BEKRPACK-UPD = ALL '+'                                       
055100       MOVE KR-BEKRPACK          TO RESP-BEKRPACK-UPD                     
055200     ELSE                                                                 
055300       MOVE REQU-BEKRPACK-UPD    TO RESP-BEKRPACK-UPD                     
055400       MOVE MFS-ADD-LAES-IN-FAELT                                         
055500                                 TO RESP-BEKRPACK-UPD-ATTR                
055600     END-IF                                                               
055700     IF REQU-KVKRPACK-UPD = ALL '+'                                       
055800       MOVE KR-KVKRPACK          TO RESP-KVKRPACK-UT                      
055900     ELSE                                                                 
056000       MOVE REQU-KVKRPACK-UPD    TO RESP-KVKRPACK-UPD                     
056100       MOVE MFS-ADD-LAES-IN-FAELT                                         
056200                                 TO RESP-KVKRPACK-UPD-ATTR                
056300     END-IF                                                               
056400                                                                          
056500     .                                                                    
056600     EJECT                                                                
056700 FB-LAES-VISA-LEV-INFO SECTION.                                           
056800     MOVE KR-IDLEVNR             TO W-IDLEVNR                             
056900     MOVE KR-IDLEVG              TO W-IDLEVG                              
057000                                                                          
057100     PERFORM IMS-GU-WLLEVA01                                              
057200     PERFORM IMS-GNP-WLLEVA14                                             
057300     MOVE  ADR-BELEV             TO RESP-BELEV                            
057400     .                                                                    
057500     EJECT                                                                
057600 FC-LAES-VISA-BEN-INFO SECTION.                                           
057700     MOVE KR-IDARTNR             TO W-IDARTNR                             
057800                                                                          
           MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                              
           IF DCS-UNICODE-IDSKYLT                                               
              MOVE 'UTF8'             TO TRAUTF8-KDCP                           
           ELSE                                                                 
              MOVE '278 '             TO TRAUTF8-KDCP                           
           END-IF                                                               
059300                                                                          
059400     PERFORM IMS-GU-WLBENA11                                              
059500     IF SEGMENT-FINNS                                                     
059600       MOVE TEXT-BEART    TO TRAUTF8-TECONV-FROM                          
059700     ELSE                                                                 
059800       MOVE SPACE         TO TRAUTF8-TECONV-FROM                          
059900                             TEXT-BEART                                   
060000       MOVE WS-CP-EBCDIC  TO TRAUTF8-KDCP                                 
060100     END-IF                                                               
060200                                                                          
060210     MOVE KR-IDDC TO WS-IDDC                                              
060300     IF REQU-IDMSGVER = '001'                                             
060400*      CALL FROM WEB AND NDC-CHINA                                        
060500*      CONVERT TO UNICODE IF NOT ALREADY SO, STRIP TRAILING SPACE         
060600       CALL WTRAUTF8 USING TRAUTF8-AREA                                   
060700       MOVE TRAUTF8-TECONV-TO TO RESP-BEART                               
060800     ELSE                                                                 
060900*      CALL FROM 3270 SCREEN. RETURN AS-IS (EBCDIC)                       
061000       MOVE TEXT-BEART        TO RESP-BEART                               
061100     END-IF                                                               
061200     .                                                                    
061300     EJECT                                                                
061400 FD-KOLLA-OM-FARLIGT-GODS SECTION.                                        
061500     MOVE KR-IDARTNR             TO W-IDARTNR                             
061600                                                                          
061700     PERFORM IMS-GU-WLARTC11                                              
061800     IF SEGMENT-FINNS                                                     
061900       IF CLAG-KDFARLIG = 4 OR                                            
062000          CLAG-KDFARLIG = 7                                               
062100         MOVE INF-FARLIGT-GODS   TO RESP-IDMSG-ERROR                      
062200       END-IF                                                             
062300     ELSE                                                                 
062400       MOVE INF-ARTIKEL-SAKNAS   TO RESP-IDMSG-INFO                       
062500       MOVE 'IDARTNR'            TO RESP-IDELMT-ERROR                     
062600     END-IF                                                               
062700     .                                                                    
062800     EJECT                                                                
062900 G-KOLLA-INPUT SECTION.                                                   
063000     MOVE JA                     TO INDATA-SW                             
063100     MOVE JA                     TO NAGOT-ANDRAT-SW                       
063200                                                                          
063300     IF REQU-W60204I1-001-GRP = ALL '+'                                   
063400       MOVE INF-PF11-AND-NO-DATA TO RESP-IDMSG-INFO                       
063500       PERFORM MFS-ROER-EJ-FAELT-IN                                       
063600       PERFORM MFS-ROER-EJ-FAELT-UT                                       
063700       MOVE NEJ                  TO INDATA-SW                             
063800       MOVE NEJ                  TO ALLT-SW                               
063900       PERFORM IMS-GU-W6KVAE01                                            
064000       IF SEGMENT-FINNS                                                   
064100         IF KR-KDKRSTA = '2' OR '3'                                       
064200           CONTINUE                                                       
064300         ELSE                                                             
064400           MOVE INF-UPPDAT-OTILLATET                                      
064500                                 TO RESP-IDMSG-INFO                       
064600           PERFORM MFS-SPAERRA-FAELT                                      
064700         END-IF                                                           
064800       END-IF                                                             
064900     ELSE                                                                 
065000       PERFORM IMS-GU-W6KVAE01                                            
065100       IF SEGMENT-SAKNAS                                                  
065200         MOVE ERR-WRONG-KEY      TO RESP-IDMSG-ERROR                      
065300         MOVE 'IDKR'             TO RESP-IDELMT-ERROR                     
065400         PERFORM MFS-RENSA-FAELT-UT                                       
065500         MOVE NEJ                TO INDATA-SW                             
065600       ELSE                                                               
065700         PERFORM IMS-GNP-W6KVAE12                                         
065800         IF SEGMENT-FINNS                                                 
065900           MOVE INF-UPPDAT-OTILLATET                                      
066000                                 TO RESP-IDMSG-INFO                       
066100           PERFORM MFS-SPAERRA-FAELT                                      
066200           MOVE NEJ              TO INDATA-SW                             
066300         ELSE                                                             
066400           IF REQU-IDKOLLI-UPD NOT = ALL '+'                              
066500             IF REQU-IDKOLLI-UPD NUMERIC AND                              
066600                REQU-IDKOLLI-UPD > ZERO                                   
066700               MOVE MFS-NUM-FAELT-RAETT                                   
066800                                 TO RESP-IDKOLLI-UPD-ATTR                 
066900             ELSE                                                         
067000               MOVE MFS-NUM-FAELT-FEL                                     
067100                                 TO RESP-IDKOLLI-UPD-ATTR                 
067200               MOVE NEJ          TO INDATA-SW                             
067300             END-IF                                                       
067400           END-IF                                                         
067500                                                                          
067600           IF REQU-KDKOLLI-UPD NOT = ALL '+'                              
067700             MOVE REQU-KDKOLLI-UPD                                        
067800                                 TO W-KDKOLLI                             
067900             PERFORM IMS-GU-WLEMBB01                                      
068000             IF SEGMENT-SAKNAS                                            
068100               MOVE MFS-ALFA-FAELT-FEL                                    
068200                                 TO RESP-KDKOLLI-UPD-ATTR                 
068300               MOVE NEJ          TO INDATA-SW                             
068400             ELSE                                                         
068500               MOVE MFS-ALFA-FAELT-RAETT                                  
068600                                 TO RESP-KDKOLLI-UPD-ATTR                 
068700             END-IF                                                       
068800           END-IF                                                         
068900                                                                          
069000           IF REQU-VKKOLLIB-UPD NOT = ALL '+'                             
069100             MOVE REQU-VKKOLLIB-UPD                                       
069200                                 TO DEC-IDFRIDATA                         
069300             MOVE 5              TO DEC-KVHELTAL                          
069400             MOVE 1              TO DEC-KVDECIMAL                         
069500             CALL WDECEDIT    USING DEC-WDECAREA                          
069600             IF DEC-KDSVAR-OK                                             
069700               MOVE DEC-IDEDITDATA                                        
069800                                 TO WS-VKKOLLIB                           
069900               IF WS-VKKOLLIB > ZERO                                      
070000                 MOVE MFS-NUM-FAELT-RAETT                                 
070100                                 TO RESP-VKKOLLIB-UPD-ATTR                
070200               ELSE                                                       
070300                 MOVE MFS-NUM-FAELT-FEL                                   
070400                                 TO RESP-VKKOLLIB-UPD-ATTR                
070500                 MOVE NEJ        TO INDATA-SW                             
070600               END-IF                                                     
070700             ELSE                                                         
070800               MOVE MFS-NUM-FAELT-FEL                                     
070900                                 TO RESP-VKKOLLIB-UPD-ATTR                
071000               MOVE NEJ          TO INDATA-SW                             
071100             END-IF                                                       
071200           END-IF                                                         
071300                                                                          
071400           IF REQU-DIKOLLIL-UPD NOT = ALL '+'                             
071500             IF REQU-DIKOLLIL-UPD NUMERIC AND                             
071600                REQU-DIKOLLIL-UPD > ZERO                                  
071700               MOVE MFS-NUM-FAELT-RAETT                                   
071800                                 TO RESP-DIKOLLIL-UPD-ATTR                
071900             ELSE                                                         
072000               MOVE MFS-NUM-FAELT-FEL                                     
072100                                 TO RESP-DIKOLLIL-UPD-ATTR                
072200               MOVE NEJ          TO INDATA-SW                             
072300             END-IF                                                       
072400           END-IF                                                         
072500                                                                          
072600           IF REQU-DIKOLLIB-UPD NOT = ALL '+'                             
072700             IF REQU-DIKOLLIB-UPD NUMERIC AND                             
072800                REQU-DIKOLLIB-UPD > ZERO                                  
072900               MOVE MFS-NUM-FAELT-RAETT                                   
073000                                 TO RESP-DIKOLLIB-UPD-ATTR                
073100             ELSE                                                         
073200               MOVE MFS-NUM-FAELT-FEL                                     
073300                                 TO RESP-DIKOLLIB-UPD-ATTR                
073400               MOVE NEJ          TO INDATA-SW                             
073500             END-IF                                                       
073600           END-IF                                                         
073700                                                                          
073800           IF REQU-DIKOLLIH-UPD NOT = ALL '+'                             
073900             IF REQU-DIKOLLIH-UPD NUMERIC AND                             
074000                REQU-DIKOLLIH-UPD > ZERO                                  
074100               MOVE MFS-NUM-FAELT-RAETT                                   
074200                                 TO RESP-DIKOLLIH-UPD-ATTR                
074300             ELSE                                                         
074400               MOVE MFS-NUM-FAELT-FEL                                     
074500                                 TO RESP-DIKOLLIH-UPD-ATTR                
074600               MOVE NEJ          TO INDATA-SW                             
074700             END-IF                                                       
074800           END-IF                                                         
074900                                                                          
075000           IF REQU-KDPERSON-UPD NOT = ALL '+'                             
075100             MOVE REQU-KDPERSON-UPD                                       
075200                                TO WS-KDPERSON                            
075300             MOVE WS-KDPERSON   TO W-IDPERSON                             
075400             PERFORM IMS-GU-WDP311                                        
075500             IF SEGMENT-SAKNAS                                            
075600               MOVE MFS-NUM-FAELT-FEL                                     
075700                                TO RESP-KDPERSON-UPD-ATTR                 
075800               MOVE NEJ         TO INDATA-SW                              
075900             ELSE                                                         
076000               MOVE MFS-NUM-FAELT-RAETT                                   
076100                                TO RESP-KDPERSON-UPD-ATTR                 
076200               MOVE PERS-IDNAMN TO WS-BENAEMN                             
076300             END-IF                                                       
076400           END-IF                                                         
076500                                                                          
076600           IF REQU-KVKRPACK-UPD NOT = ALL '+'                             
076700             MOVE REQU-KVKRPACK-UPD                                       
076800                                 TO DEC-IDFRIDATA                         
076900             MOVE 2              TO DEC-KVHELTAL                          
077000             MOVE 1              TO DEC-KVDECIMAL                         
077100             CALL WDECEDIT    USING DEC-WDECAREA                          
077200             IF DEC-KDSVAR-OK                                             
077300               MOVE DEC-IDEDITDATA                                        
077400                                 TO WS-KVKRPACK                           
077500               IF WS-KVKRPACK > ZERO                                      
077600                 MOVE MFS-NUM-FAELT-RAETT                                 
077700                                 TO RESP-KVKRPACK-UPD-ATTR                
077800               ELSE                                                       
077900                 MOVE MFS-NUM-FAELT-FEL                                   
078000                                 TO RESP-KVKRPACK-UPD-ATTR                
078100                 MOVE NEJ        TO INDATA-SW                             
078200               END-IF                                                     
078300             ELSE                                                         
078400               MOVE MFS-NUM-FAELT-FEL                                     
078500                                 TO RESP-KVKRPACK-UPD-ATTR                
078600               MOVE NEJ          TO INDATA-SW                             
078700             END-IF                                                       
078800           END-IF                                                         
078900                                                                          
079000           IF REQU-KDCMD-UPD NOT =  ALL '+'                               
079100             IF REQU-KDCMD-DELETE OR                                      
079200                REQU-KDCMD-INGENTING                                      
079300               MOVE MFS-ALFA-FAELT-RAETT                                  
079400                                 TO RESP-KDCMD-UPD-ATTR                   
079500             ELSE                                                         
079600               MOVE MFS-ALFA-FAELT-FEL                                    
079700                                 TO RESP-KDCMD-UPD-ATTR                   
079800               MOVE NEJ          TO INDATA-SW                             
079900             END-IF                                                       
080000           END-IF                                                         
080100                                                                          
080200           IF REQU-IDKOLLI-UPD      = ALL '+' AND                         
080300             (REQU-KDKOLLI-UPD  NOT = ALL '+' OR                          
080400              REQU-VKKOLLIB-UPD NOT = ALL '+' OR                          
080500              REQU-DIKOLLIL-UPD NOT = ALL '+' OR                          
080600              REQU-DIKOLLIB-UPD NOT = ALL '+' OR                          
080700              REQU-DIKOLLIH-UPD NOT = ALL '+' OR                          
080800              REQU-KDCMD-UPD    NOT = ALL '+')                            
080900             MOVE MFS-NUM-FAELT-FEL                                       
081000                                 TO RESP-IDKOLLI-UPD-ATTR                 
081100             MOVE NEJ            TO INDATA-SW                             
081200           END-IF                                                         
081300                                                                          
081400           IF REQU-IDKOLLI-UPD NOT = ALL '+'                              
081500             MOVE REQU-IDKOLLI-UPD                                        
081600                                 TO W-IDKOLLI                             
081700             PERFORM IMS-GU-W6KVAE01                                      
081800             PERFORM IMS-GNP-W6KVAE11                                     
081900                                                                          
082000             IF SEGMENT-SAKNAS             AND                            
082100                REQU-KDKOLLI-UPD = ALL '+' AND                            
082200                (REQU-KDCMD-INGENTING      OR                             
082300                REQU-KDCMD-UPD = ALL '+')                                 
082400               IF REQU-VKKOLLIB-UPD = ALL '+'                             
082500                 MOVE MFS-NUM-FAELT-FEL                                   
082600                                 TO RESP-VKKOLLIB-UPD-ATTR                
082700                 MOVE NEJ        TO INDATA-SW                             
082800               END-IF                                                     
082900               IF REQU-DIKOLLIL-UPD = ALL '+'                             
083000                 MOVE MFS-NUM-FAELT-FEL                                   
083100                                 TO RESP-DIKOLLIL-UPD-ATTR                
083200                 MOVE NEJ        TO INDATA-SW                             
083300               END-IF                                                     
083400               IF REQU-DIKOLLIB-UPD = ALL '+'                             
083500                 MOVE MFS-NUM-FAELT-FEL                                   
083600                                 TO RESP-DIKOLLIB-UPD-ATTR                
083700                 MOVE NEJ        TO INDATA-SW                             
083800               END-IF                                                     
083900               IF REQU-DIKOLLIH-UPD = ALL '+'                             
084000                 MOVE MFS-NUM-FAELT-FEL                                   
084100                                 TO RESP-DIKOLLIH-UPD-ATTR                
084200                 MOVE NEJ        TO INDATA-SW                             
084300               END-IF                                                     
084400             END-IF                                                       
084500                                                                          
084600             IF SEGMENT-SAKNAS                 AND                        
084700                REQU-KDKOLLI-UPD NOT = ALL '+' AND                        
084800                REQU-VKKOLLIB-UPD = ALL '+'                               
084900               MOVE MFS-NUM-FAELT-FEL                                     
085000                                 TO RESP-VKKOLLIB-UPD-ATTR                
085100               MOVE NEJ          TO INDATA-SW                             
085200             END-IF                                                       
085300                                                                          
085400             IF SEGMENT-FINNS               AND                           
085500                REQU-KDKOLLI-UPD  = ALL '+' AND                           
085600                REQU-VKKOLLIB-UPD = ALL '+' AND                           
085700                REQU-DIKOLLIL-UPD = ALL '+' AND                           
085800                REQU-DIKOLLIB-UPD = ALL '+' AND                           
085900                REQU-DIKOLLIH-UPD = ALL '+' AND                           
086000                (REQU-KDCMD-INGENTING       OR                            
086100                 REQU-KDCMD-UPD   = ALL '+')                              
086200               MOVE NEJ          TO NAGOT-ANDRAT-SW                       
086300               MOVE NEJ          TO INDATA-SW                             
086400               MOVE NEJ          TO ALLT-SW                               
086500               MOVE INF-INGET-ANDRAT                                      
086600                                 TO RESP-IDMSG-INFO                       
086700               PERFORM MFS-ROER-EJ-FAELT-UT                               
086800               PERFORM MFS-ROER-EJ-FAELT-IN                               
086900             END-IF                                                       
087000                                                                          
087100             IF REQU-KDCMD-DELETE                                         
087200               IF SEGMENT-SAKNAS                                          
087300                 MOVE NEJ        TO INDATA-SW                             
087400                 MOVE MFS-NUM-FAELT-FEL                                   
087500                                 TO RESP-IDKOLLI-UPD-ATTR                 
087600               END-IF                                                     
087700                                                                          
087800               IF REQU-KDKOLLI-UPD  NOT = ALL '+' OR                      
087900                  REQU-VKKOLLIB-UPD NOT = ALL '+' OR                      
088000                  REQU-DIKOLLIL-UPD NOT = ALL '+' OR                      
088100                  REQU-DIKOLLIB-UPD NOT = ALL '+' OR                      
088200                  REQU-DIKOLLIH-UPD NOT = ALL '+'                         
088300                 MOVE MFS-ALFA-FAELT-FEL                                  
088400                                 TO RESP-KDCMD-UPD-ATTR                   
088500                 MOVE NEJ        TO INDATA-SW                             
088600               END-IF                                                     
088700             END-IF                                                       
088800           END-IF                                                         
088900                                                                          
089000           IF REQU-KVKRPACK-UPD NOT = ALL '+'                             
089100             IF REQU-KDPERSON-UPD = ALL '+' AND                           
089200                REQU-BEKRPACK-UPD = ALL '+' AND                           
089300                KR-BEKRPACK = SPACE                                       
089400               MOVE MFS-ALFA-FAELT-FEL                                    
089500                                 TO RESP-KDPERSON-UPD-ATTR                
089600               MOVE NEJ          TO INDATA-SW                             
089700             END-IF                                                       
089800           END-IF                                                         
089900                                                                          
090000           IF (REQU-KDPERSON-UPD NOT = ALL '+' OR                         
090100               REQU-BEKRPACK-UPD NOT = ALL '+') AND                       
090200              KR-BEKRPACK  = SPACE                                        
090300             IF REQU-KVKRPACK-UPD = ALL '+'                               
090400               MOVE MFS-NUM-FAELT-FEL                                     
090500                                 TO RESP-KVKRPACK-UPD-ATTR                
090600               MOVE NEJ          TO INDATA-SW                             
090700             END-IF                                                       
090800           END-IF                                                         
090900                                                                          
091000           IF REQU-KDPERSON-UPD NOT = ALL '+' OR                          
091100              REQU-BEKRPACK-UPD NOT = ALL '+' OR                          
091200              REQU-KVKRPACK-UPD NOT = ALL '+'                             
091300             MOVE ZERO           TO W-IDKOLLI                             
091400             PERFORM IMS-GU-W6KVAE01                                      
091500             PERFORM IMS-GNP-W6KVAE11-NEXT                                
091600             IF SEGMENT-SAKNAS AND                                        
091700                REQU-IDKOLLI-UPD = ALL '+'                                
091800               CONTINUE                                                   
091900             ELSE                                                         
092000               IF REQU-KDCMD-DELETE                                       
092100                 PERFORM IMS-GNP-W6KVAE11-NEXT                            
092200                 IF SEGMENT-SAKNAS                                        
092300                   MOVE MFS-ALFA-FAELT-FEL                                
092400                                 TO RESP-KDCMD-UPD-ATTR                   
092500                   MOVE NEJ      TO INDATA-SW                             
092600                 END-IF                                                   
092700               END-IF                                                     
092800             END-IF                                                       
092900           END-IF                                                         
093000                                                                          
093100           IF REQU-BEKRPACK-UPD NOT = ALL '+' AND                         
093200              REQU-BEKRPACK-UPD NOT = SPACE                               
093300             MOVE MFS-ALFA-FAELT-RAETT                                    
093400                                 TO RESP-BEKRPACK-UPD-ATTR                
093500           END-IF                                                         
093600                                                                          
093700           IF INDATA-OK                                                   
093800             MOVE KR-IDLEVNR     TO W-IDLEVNR                             
093900             MOVE 'SE'           TO W-IDLAND                              
094000             PERFORM IMS-GU-WLLEVA11                                      
094100** KONTROLLERAR ATT EK-SEGMENT FINNS UPPLAGT PÅ LEVERANTÖREN              
094200             IF SEGMENT-SAKNAS                                            
094300               MOVE NEJ          TO INDATA-SW                             
094400               MOVE JA           TO WDF11-SAKNAS-SW                       
094500             END-IF                                                       
094600           END-IF                                                         
094700                                                                          
094800           IF INDATA-FEL AND NAGOT-ANDRAT                                 
094900             MOVE NEJ            TO ALLT-SW                               
095000             IF WDF11-FEL                                                 
095100               MOVE VALUTAKOD-SAKNAS                                      
095200                                 TO RESP-IDMSG-ERROR                      
095300               MOVE 'KDVALISO'   TO RESP-IDELMT-ERROR                     
095400             ELSE                                                         
095500               MOVE ERR-CORR-HILITE-FLDS                                  
095600                                 TO RESP-IDMSG-ERROR                      
095700             END-IF                                                       
095800             PERFORM MFS-ROER-EJ-FAELT-UT                                 
095900             PERFORM MFS-ROER-EJ-FAELT-IN                                 
096000           END-IF                                                         
096100         END-IF                                                           
096200       END-IF                                                             
096300     END-IF                                                               
096400     .                                                                    
096500     EJECT                                                                
096600 H-UPPDATERA SECTION.                                                     
096700     PERFORM IMS-GHU-W6KVAE01                                             
096800     IF SEGMENT-FINNS                                                     
096900                                                                          
097000       IF REQU-BEKRPACK-UPD NOT = ALL '+' AND                             
097100          REQU-BEKRPACK-UPD NOT = SPACE                                   
097200         MOVE REQU-BEKRPACK-UPD  TO KR-BEKRPACK                           
097300       ELSE                                                               
097400         IF REQU-KDPERSON-UPD NOT = ALL '+'                               
097500           MOVE WS-BENAEMN       TO KR-BEKRPACK                           
097600         END-IF                                                           
097700       END-IF                                                             
097800       IF REQU-KVKRPACK-UPD NOT = ALL '+'                                 
097900          MOVE WS-KVKRPACK       TO KR-KVKRPACK                           
098000                                    RESP-KVKRPACK-UT                      
098100       END-IF                                                             
098200                                                                          
098300       IF REQU-KDPERSON-UPD NOT = ALL '+' OR                              
098400          (REQU-BEKRPACK-UPD NOT = ALL '+' AND                            
098500          REQU-BEKRPACK-UPD NOT = SPACE)                                  
098600         MOVE DATUM              TO KR-TIKRPACK                           
098700       END-IF                                                             
098800                                                                          
098900       IF (KR-FLKROMK = JA             OR                                 
099000           KR-FLKROMK = SPACE)         AND                                
099100          (REQU-KDPERSON-UPD NOT = ALL '+' OR                             
099200           REQU-BEKRPACK-UPD NOT = ALL '+')                               
099300         MOVE KR-IDLEVNR         TO TEST-IDLEVNR                          
099400         IF IDLEVNR-HF                                                    
099500           MOVE '5'              TO KR-KDKRSTA                            
099600         ELSE                                                             
099700           MOVE '3'              TO KR-KDKRSTA                            
099800         END-IF                                                           
099900       END-IF                                                             
100000                                                                          
100100       PERFORM IMS-REPL-W6KVAE01                                          
100200                                                                          
100300       IF REQU-IDKOLLI-UPD NOT = ALL '+'                                  
100400                                                                          
100500         MOVE REQU-IDKOLLI-UPD   TO W-IDKOLLI                             
100600         PERFORM IMS-GHNP-W6KVAE11                                        
100700         IF NOT REQU-KDCMD-DELETE                                         
100800           IF REQU-KDKOLLI-UPD NOT = ALL '+'                              
100900             MOVE EMB-DIKOLLIL   TO KOLL-DIKOLLIL                         
101000             MOVE EMB-DIKOLLIB   TO KOLL-DIKOLLIB                         
101100             MOVE EMB-DIKOLLIH   TO KOLL-DIKOLLIH                         
101200           END-IF                                                         
101300           IF REQU-VKKOLLIB-UPD NOT = ALL '+'                             
101400             MOVE WS-VKKOLLIB    TO KOLL-VKKOLLIB                         
101500           END-IF                                                         
101600           IF REQU-DIKOLLIL-UPD NOT = ALL '+'                             
101700             MOVE REQU-DIKOLLIL-UPD                                       
101800                                 TO KOLL-DIKOLLIL                         
101900           END-IF                                                         
102000           IF REQU-DIKOLLIB-UPD NOT = ALL '+'                             
102100             MOVE REQU-DIKOLLIB-UPD                                       
102200                                 TO KOLL-DIKOLLIB                         
102300           END-IF                                                         
102400           IF REQU-DIKOLLIH-UPD NOT = ALL '+'                             
102500             MOVE REQU-DIKOLLIH-UPD                                       
102600                                 TO KOLL-DIKOLLIH                         
102700           END-IF                                                         
102800         END-IF                                                           
102900         IF SEGMENT-FINNS                                                 
103000           IF REQU-KDCMD-DELETE                                           
103100             PERFORM IMS-DLET-W6KVAE11                                    
103200           ELSE                                                           
103300             PERFORM IMS-REPL-W6KVAE11                                    
103400           END-IF                                                         
103500         ELSE                                                             
103600           MOVE REQU-IDKOLLI-UPD TO KOLL-IDKOLLI                          
103700           PERFORM IMS-ISRT-W6KVAE11                                      
103800         END-IF                                                           
103900         IF REQU-KDCMD-DELETE                                             
104000           MOVE ZERO             TO W-IDKOLLI                             
104100         ELSE                                                             
104200           MOVE KOLL-IDKOLLI     TO W-IDKOLLI                             
104300         END-IF                                                           
104400                                                                          
104500       END-IF                                                             
104600                                                                          
104700       IF KR-KDKRSTA > '2'                                                
104800         PERFORM S01-CALL-W602KRUP                                        
104900       END-IF                                                             
105000                                                                          
105100       IF KRUP-IDVERNR-OK = 'F'                                           
105200         PERFORM IMS-ROLLBACK                                             
105300         MOVE ERR-VERNR-OVERSKRIDEN                                       
105400                                 TO RESP-IDMSG-ERROR                      
105500         PERFORM MFS-ROER-EJ-FAELT-UT                                     
105600         PERFORM MFS-ROER-EJ-FAELT-IN                                     
105700       ELSE                                                               
105800         MOVE INF-UPDATE-DONE    TO RESP-IDMSG-INFO                       
105900         PERFORM MFS-RENSA-FAELT-IN                                       
106000         PERFORM MFS-FORM-ATTR                                            
106100       END-IF                                                             
106200     END-IF                                                               
106300     .                                                                    
106400     EJECT                                                                
106500 S01-CALL-W602KRUP SECTION.                                               
106600*    SUBPGM W602KRUP AVSLUTAR KR-RAPPORTERING                             
106700                                                                          
106800     MOVE 'W6020400'             TO KRUP-IDPGM                            
106900     MOVE WS-IDKR                TO KRUP-IDKR                             
107000     MOVE SPACE                  TO KRUP-IDVERNR-OK                       
107100     MOVE NEJ                    TO KRUP-FLANNULL                         
107200                                                                          
107300     CALL W602KRUP            USING KRUP-W602KRUP                         
107400                                    W6H7-PCB                              
107500                                    WDG2-PCB                              
107600                                    LEVA-PCB                              
107700                                    WDK6-PCB                              
107800                                    LOPB-PCB                              
107900                                    FILC-PCB                              
107910                                    WDK7-PCB                              
107920                                    KRUP-WDB6-PCB                         
108000     .                                                                    
108100     EJECT                                                                
108200 MFS-RENSA-FAELT-UT SECTION.                                              
108300                                                                          
108400     MOVE ALL-SPACE              TO RESP-IDKOLLI-START                    
108500                                    RESP-IDKOLLI-NEXT                     
108600                                    RESP-IDARTNR                          
108700                                    RESP-BEART                            
108800                                    RESP-KDKRSTA                          
108900                                    RESP-TIREGDAT                         
109000                                    RESP-IDLEVNR                          
109100                                    RESP-IDLEVG                           
109200                                    RESP-BELEV                            
109300                                    RESP-TIKRPACK                         
109400                                    RESP-BEKRPACK-UPD                     
109500                                    RESP-KVKRPACK-UT                      
109600                                                                          
109700     IF REQU-IDMSGVER = '001'                                             
109800       MOVE ALL-UTF8-SPACE       TO RESP-BEART                            
109900     END-IF                                                               
110000                                                                          
110100     PERFORM                                                              
110200     VARYING INDX FROM +1 BY +1                                           
110300       UNTIL INDX > MAX-KVRADER                                           
110400       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
110500     END-PERFORM                                                          
110600     .                                                                    
110700     SKIP2                                                                
110800 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
110900                                                                          
111000     MOVE ALL-SPACE              TO RESP-IDKOLLI-LINE (INDX)              
111100                                    RESP-VKKOLLIB-LINE (INDX)             
111200                                    RESP-DIKOLLIL-LINE (INDX)             
111300                                    RESP-DIKOLLIB-LINE (INDX)             
111400                                    RESP-DIKOLLIH-LINE (INDX)             
111500     .                                                                    
111600     SKIP2                                                                
111700 MFS-RENSA-FAELT-IN SECTION.                                              
111800                                                                          
111900     MOVE ALL-SPACE              TO RESP-KDKOLLI-UPD                      
112000                                    RESP-IDKOLLI-UPD                      
112100                                    RESP-VKKOLLIB-UPD                     
112200                                    RESP-DIKOLLIL-UPD                     
112300                                    RESP-DIKOLLIB-UPD                     
112400                                    RESP-DIKOLLIH-UPD                     
112500                                    RESP-KDPERSON-UPD                     
112600                                    RESP-KVKRPACK-UPD                     
112700                                    RESP-KDCMD-UPD                        
112800     .                                                                    
112900     EJECT                                                                
113000 MFS-FORM-ATTR      SECTION.                                              
113100                                                                          
113200     MOVE MFS-FORMATETS-ATTR     TO RESP-KDKOLLI-UPD-ATTR                 
113300                                    RESP-IDKOLLI-UPD-ATTR                 
113400                                    RESP-VKKOLLIB-UPD-ATTR                
113500                                    RESP-DIKOLLIL-UPD-ATTR                
113600                                    RESP-DIKOLLIB-UPD-ATTR                
113700                                    RESP-DIKOLLIH-UPD-ATTR                
113800                                    RESP-KDPERSON-UPD-ATTR                
113900                                    RESP-KVKRPACK-UPD-ATTR                
114000                                    RESP-KDCMD-UPD-ATTR                   
114100                                    RESP-BEKRPACK-UPD-ATTR                
114200     .                                                                    
114300     EJECT                                                                
114400 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
114500                                                                          
114600     MOVE ALL-PLUS               TO RESP-IDKOLLI-START                    
114700                                    RESP-IDKOLLI-NEXT                     
114800                                    RESP-IDARTNR                          
114900                                    RESP-BEART                            
115000                                    RESP-KDKRSTA                          
115100                                    RESP-TIREGDAT                         
115200                                    RESP-IDLEVNR                          
115300                                    RESP-IDLEVG                           
115400                                    RESP-BELEV                            
115500                                    RESP-KVKRPACK-UT                      
115600                                    RESP-TIKRPACK                         
115700     IF REQU-IDMSGVER = '001'                                             
115800       MOVE ALL-UTF8-PLUS        TO RESP-BEART                            
115900     END-IF                                                               
116000                                                                          
116100     PERFORM                                                              
116200     VARYING INDX FROM +1 BY +1                                           
116300       UNTIL INDX > MAX-KVRADER                                           
116400       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
116500     END-PERFORM                                                          
116600     .                                                                    
116700     SKIP2                                                                
116800 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
116900                                                                          
117000     MOVE ALL-PLUS               TO RESP-IDKOLLI-LINE  (INDX)             
117100                                    RESP-VKKOLLIB-LINE (INDX)             
117200                                    RESP-DIKOLLIL-LINE (INDX)             
117300                                    RESP-DIKOLLIB-LINE (INDX)             
117400                                    RESP-DIKOLLIH-LINE (INDX)             
117500     .                                                                    
117600     EJECT                                                                
117700 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
117800                                                                          
117900     MOVE ALL-PLUS               TO RESP-KDKOLLI-UPD                      
118000                                    RESP-IDKOLLI-UPD                      
118100                                    RESP-VKKOLLIB-UPD                     
118200                                    RESP-DIKOLLIL-UPD                     
118300                                    RESP-DIKOLLIB-UPD                     
118400                                    RESP-DIKOLLIH-UPD                     
118500                                    RESP-KDPERSON-UPD                     
118600                                    RESP-BEKRPACK-UPD                     
118700                                    RESP-KVKRPACK-UPD                     
118800                                    RESP-KDCMD-UPD                        
118900     .                                                                    
119000     EJECT                                                                
119100 MFS-SPAERRA-FAELT SECTION.                                               
119200                                                                          
119300     MOVE MFS-STAENG-FAELT-NOMOD TO RESP-KDKOLLI-UPD-ATTR                 
119400                                    RESP-IDKOLLI-UPD-ATTR                 
119500                                    RESP-VKKOLLIB-UPD-ATTR                
119600                                    RESP-DIKOLLIL-UPD-ATTR                
119700                                    RESP-DIKOLLIB-UPD-ATTR                
119800                                    RESP-DIKOLLIH-UPD-ATTR                
119900                                    RESP-KDPERSON-UPD-ATTR                
120000                                    RESP-BEKRPACK-UPD-ATTR                
120100                                    RESP-KVKRPACK-UPD-ATTR                
120200                                    RESP-KDCMD-UPD-ATTR                   
120300     .                                                                    
120400     EJECT                                                                
120500* --- IMS SEKTIONER ---                                                   
120600                                                                          
120700 IMS-GU-W6KVAE01 SECTION.                                                 
120800     STRING 'W6KVAE01(IDKR     =' W-IDKR-X ')'                            
120900          DELIMITED BY SIZE INTO SSA1                                     
121000     MOVE '  GE'                 TO GODK-STATUSKODER                      
121100     CALL CBLTDLI USING GU KVAE-PCB DLI-IO-AREA1 SSA1                     
121200     MOVE KVAE-STATUS-CODE       TO STATUS-WS                             
121300     PERFORM IMS-STATUSKONTROLL                                           
121400     .                                                                    
121500     EJECT                                                                
121600 IMS-GHU-W6KVAE01 SECTION.                                                
121700     STRING 'W6KVAE01(IDKR     =' W-IDKR-X ')'                            
121800          DELIMITED BY SIZE INTO SSA1                                     
121900     MOVE '  GE'                 TO GODK-STATUSKODER                      
122000     CALL CBLTDLI USING GHU KVAE-PCB DLI-IO-AREA1 SSA1                    
122100     MOVE KVAE-STATUS-CODE       TO STATUS-WS                             
122200     PERFORM IMS-STATUSKONTROLL                                           
122300     .                                                                    
122400     EJECT                                                                
122500 IMS-GNP-W6KVAE11-NEXT  SECTION.                                          
122600     STRING 'W6KVAE11(IDKOLLI =>' W-IDKOLLI-X ')'                         
122700          DELIMITED BY SIZE INTO SSA1                                     
122800     MOVE '  GE'                 TO GODK-STATUSKODER                      
122900     CALL CBLTDLI USING GNP KVAE-PCB DLI-IO-AREA1 SSA1                    
123000     MOVE KVAE-STATUS-CODE       TO STATUS-WS                             
123100     PERFORM IMS-STATUSKONTROLL                                           
123200     .                                                                    
123300     EJECT                                                                
123400 IMS-GHNP-W6KVAE11 SECTION.                                               
123500     STRING 'W6KVAE11(IDKOLLI  =' W-IDKOLLI-X ')'                         
123600          DELIMITED BY SIZE INTO SSA1                                     
123700     MOVE '  GE'                 TO GODK-STATUSKODER                      
123800     CALL CBLTDLI USING GHNP KVAE-PCB DLI-IO-AREA1 SSA1                   
123900     MOVE KVAE-STATUS-CODE       TO STATUS-WS                             
124000     PERFORM IMS-STATUSKONTROLL                                           
124100     .                                                                    
124200     EJECT                                                                
124300 IMS-GNP-W6KVAE11 SECTION.                                                
124400     STRING 'W6KVAE11(IDKOLLI  =' W-IDKOLLI-X ')'                         
124500          DELIMITED BY SIZE INTO SSA1                                     
124600     MOVE '  GE'                 TO GODK-STATUSKODER                      
124700     CALL CBLTDLI USING GNP KVAE-PCB DLI-IO-AREA1 SSA1                    
124800     MOVE KVAE-STATUS-CODE       TO STATUS-WS                             
124900     PERFORM IMS-STATUSKONTROLL                                           
125000     .                                                                    
125100     SKIP3                                                                
125200 IMS-ISRT-W6KVAE11 SECTION.                                               
125300     STRING 'W6KVAE01(IDKR     =' W-IDKR-X ')'                            
125400          DELIMITED BY SIZE INTO SSA1                                     
125500     MOVE 'W6KVAE11'             TO SSA2                                  
125600     MOVE '  ' TO GODK-STATUSKODER                                        
125700     CALL CBLTDLI USING ISRT KVAE-PCB DLI-IO-AREA1 SSA1 SSA2              
125800     MOVE KVAE-STATUS-CODE       TO STATUS-WS                             
125900     PERFORM IMS-STATUSKONTROLL                                           
126000     .                                                                    
126100     SKIP3                                                                
126200 IMS-REPL-W6KVAE11 SECTION.                                               
126300     MOVE '  '                   TO GODK-STATUSKODER                      
126400     CALL CBLTDLI USING REPL KVAE-PCB DLI-IO-AREA1                        
126500     MOVE KVAE-STATUS-CODE       TO STATUS-WS                             
126600     PERFORM IMS-STATUSKONTROLL                                           
126700     .                                                                    
126800     EJECT                                                                
126900 IMS-REPL-W6KVAE01 SECTION.                                               
127000     MOVE '  '                   TO GODK-STATUSKODER                      
127100     CALL CBLTDLI USING REPL KVAE-PCB DLI-IO-AREA1                        
127200     MOVE KVAE-STATUS-CODE       TO STATUS-WS                             
127300     PERFORM IMS-STATUSKONTROLL                                           
127400     .                                                                    
127500     EJECT                                                                
127600 IMS-DLET-W6KVAE11 SECTION.                                               
127700     MOVE '  '                   TO GODK-STATUSKODER                      
127800     CALL CBLTDLI USING DLET KVAE-PCB DLI-IO-AREA1                        
127900     MOVE KVAE-STATUS-CODE       TO STATUS-WS                             
128000     PERFORM IMS-STATUSKONTROLL                                           
128100     .                                                                    
128200     EJECT                                                                
128300 IMS-GNP-W6KVAE12 SECTION.                                                
128400     MOVE 'W6KVAE12'             TO SSA1                                  
128500     MOVE '  GE'                 TO GODK-STATUSKODER                      
128600     CALL CBLTDLI USING GNP KVAE-PCB DLI-IO-W6H712 SSA1                   
128700     MOVE KVAE-STATUS-CODE       TO STATUS-WS                             
128800     PERFORM IMS-STATUSKONTROLL                                           
128900     .                                                                    
129000     SKIP3                                                                
129100 IMS-GU-WLARTC11 SECTION.                                                 
129200     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
129300          DELIMITED BY SIZE INTO SSA1                                     
129400     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
129500          DELIMITED BY SIZE INTO SSA2                                     
129600     MOVE '  GE'                 TO GODK-STATUSKODER                      
129700     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA6 SSA1 SSA2                
129800     MOVE ARTC-STATUS-CODE       TO STATUS-WS                             
129900     PERFORM IMS-STATUSKONTROLL                                           
130000     .                                                                    
130100     EJECT                                                                
130200 IMS-GU-WLBENA11 SECTION.                                                 
130300     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
130400          DELIMITED BY SIZE INTO SSA1                                     
130500     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
130600          DELIMITED BY SIZE INTO SSA2                                     
130700     MOVE '  GE'                 TO GODK-STATUSKODER                      
130800     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA2 SSA1 SSA2                
130900     MOVE BENA-STATUS-CODE       TO STATUS-WS                             
131000     PERFORM IMS-STATUSKONTROLL                                           
131100     .                                                                    
131200     EJECT                                                                
131300 IMS-GU-WLLEVA01 SECTION.                                                 
131400     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
131500          DELIMITED BY SIZE INTO SSA1                                     
131600     MOVE '  '                   TO GODK-STATUSKODER                      
131700     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-AREA3 SSA1                     
131800     MOVE LEVA-STATUS-CODE       TO STATUS-WS                             
131900     PERFORM IMS-STATUSKONTROLL                                           
132000     .                                                                    
132100     EJECT                                                                
132200 IMS-GU-WLLEVA11 SECTION.                                                 
132300     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
132400              DELIMITED BY SIZE INTO SSA1                                 
132500     STRING 'WLLEVA11(IDLAND   =' W-IDLAND-X ')'                          
132600              DELIMITED BY SIZE INTO SSA2                                 
132700     MOVE '  GE'                 TO GODK-STATUSKODER                      
132800     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-AREA3 SSA1 SSA2                
132900     MOVE LEVA-STATUS-CODE       TO STATUS-WS                             
133000     PERFORM IMS-STATUSKONTROLL                                           
133100     .                                                                    
133200     SKIP2                                                                
133300 IMS-GNP-WLLEVA14 SECTION.                                                
133400     MOVE 'WLLEVA14 '            TO SSA1                                  
133500     MOVE '  '                   TO GODK-STATUSKODER                      
133600     CALL CBLTDLI USING GNP LEVA-PCB DLI-IO-AREA3 SSA1                    
133700     MOVE LEVA-STATUS-CODE       TO STATUS-WS                             
133800     PERFORM IMS-STATUSKONTROLL                                           
133900     .                                                                    
134000     EJECT                                                                
134100 IMS-GU-WLEMBB01 SECTION.                                                 
134200     STRING 'WLEMBB01(KDKOLLI  =' W-KDKOLLI-X ')'                         
134300          DELIMITED BY SIZE INTO SSA1                                     
134400     MOVE '  GE'                 TO GODK-STATUSKODER                      
134500     CALL CBLTDLI USING GU EMBB-PCB DLI-IO-AREA4 SSA1                     
134600     MOVE EMBB-STATUS-CODE       TO STATUS-WS                             
134700     PERFORM IMS-STATUSKONTROLL                                           
134800     .                                                                    
134900     EJECT                                                                
135000 IMS-GU-WDP311 SECTION.                                                   
135100     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
135200          DELIMITED BY SIZE INTO SSA1                                     
135300     STRING 'WDP311  (IDPERSON =' W-IDPERSON-X ')'                        
135400          DELIMITED BY SIZE INTO SSA2                                     
135500     MOVE '  GE'                 TO GODK-STATUSKODER                      
135600     CALL CBLTDLI USING GU  WDP3-PCB DLI-IO-P311 SSA1 SSA2                
135700     MOVE WDP3-STATUS-CODE       TO STATUS-WS                             
135800     PERFORM IMS-STATUSKONTROLL                                           
135900     .                                                                    
136000     EJECT                                                                
       IMS-GU-WDB601    SECTION.                                                
           STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
                DELIMITED BY SIZE INTO SSA1                                     
           MOVE '  GE' TO GODK-STATUSKODER                                      
           CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
           MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
           PERFORM IMS-STATUSKONTROLL                                           
           .                                                                    
           EJECT                                                                
136100 IMS-ROLLBACK    SECTION.                                                 
136200     CALL CBLTDLI USING ROLB MSG-PCB                                      
136300     .                                                                    
136400     SKIP2                                                                
136500 IMS-STATUSKONTROLL SECTION.                                              
136600     SET STATUS-IX               TO 1                                     
136700     SEARCH GODK-STATUS                                                   
136800       AT END                                                             
136900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
137000         DELIMITED BY SIZE INTO FELTEXT                                   
137100         CALL FELLOG                                                      
137200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
137300     END-SEARCH                                                           
137400     .                                                                    
