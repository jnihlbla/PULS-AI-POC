000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W6014810.                                                
000400 AUTHOR.         ANNELIE ENGLUND.                                         
000500 DATE-WRITTEN.   94/07/12.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        ALLMÄN BESKRIVNING:                                              
001000*        BILDEN ANVÄNDS FÖR ATT TITTA PÅ OCH GODKÄNNA                     
001100*        GODS SOM HAR TRANSPORTANMÄRKNING, SOM I DETTA                    
001200*        FALLET INNEBÄR ATT GODSET INTE KOMMIT TILL                       
001300*        LAGRET.                                                          
001400*                                                                         
001500*        PROGRAMMET UPPDATERAR W6INLA (W6D1)                              
001600*        PROGRAMMET LÄSER      W6PLAA (W6G1)                              
001700*        PROGRAMMET SKICKAR TRANS TILL 6193                               
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSAKTION: W6T148                                              
002100*        MID:         W6I14801                                            
002200*        WEB REQU.    W60148I1                                            
002300*                                                                         
002400*    UTDATA.                                                              
002500*        MOD:         W6O14801                                            
002600*        WEB RESP.    W60148O1                                            
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400*    -- CHECKED BY WY2000                                                 
003500 77  IDPGM                       PIC X(08)   VALUE 'W6014810'.            
003600                                                                          
003700*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003800 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003900                                                                          
004000 77  JA                          PIC X       VALUE 'J'.                   
004100 77  NEJ                         PIC X       VALUE 'N'.                   
004200 01  ALL-PLUS.                                                            
004300     03 FILLER                   PIC X(20) VALUE                          
004400     '++++++++++++++++++++'.                                              
004500                                                                          
004600*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004700 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004800 77  MAX-INDX                    PIC S9(4)  VALUE +12   COMP SYNC.        
004900 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
005000 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +628  COMP SYNC.        
005100 77  6191-IX                     PIC S9(2)  VALUE ZERO.                   
005200 77  MAX-6191-IX                 PIC S9(2)  VALUE +24.                    
005300 77  LNG-P-TO-P-PREFIX           PIC S9(4)  VALUE +17  COMP SYNC.         
005400 77  DAGENS-DATUM                PIC 9(6)   VALUE ZERO.                   
005500 77  WS-IDLOPNRM                 PIC 9(8)   VALUE ZERO.                   
005600 77  WS-IDLEVNR                  PIC X(5)   VALUE SPACE.                  
005700 77  WS-IDOKOLLI                 PIC 9(9)   VALUE ZERO.                   
005800 77  W-PRARTSTD                  PIC 9(7)V9(2) VALUE ZERO.                
005900 77  W-KDINLPRIO                 PIC 9(2)   VALUE ZERO.                   
006000 77  W-KVRAPP                    PIC S9(7)  VALUE ZERO COMP-3.            
006100 77  W-FLKLAR                    PIC X(1)   VALUE SPACE.                  
006200                                                                          
006300 01  SPAR-AREA.                                                           
006400     03  WS-AREA                 PIC X(150)  VALUE SPACE.                 
006500     SKIP3                                                                
006600     03  W6INLA21 REDEFINES WS-AREA.                                      
006700*        05  -COPY W6D121    -PRE    SPAR-                                
006800     SKIP3                                                                
006900*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
007000                                                                          
007100 77  INDATA-SW                   PIC X       VALUE 'J'.                   
007200     88  INDATA-OK                           VALUE 'J'.                   
007300     88  INDATA-FEL                          VALUE 'N'.                   
007400                                                                          
007500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007600     88  NYCKLAR-OK                          VALUE 'J'.                   
007700     88  NYCKLAR-FEL                         VALUE 'N'.                   
007800                                                                          
007900 77  NKL-SW                      PIC X       VALUE '0'.                   
008000     88  PARTI-NKL                           VALUE '1'.                   
008100     88  LEVKLI-NKL                          VALUE '2'.                   
008200                                                                          
008300 77  FOERSTA-6191-SW             PIC X       VALUE 'J'.                   
008400     88  FOERSTA-6191                        VALUE 'J'.                   
008500                                                                          
008600                                                                          
008700 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008800     88  EGEN-MID                            VALUE '6148'.                
008900     88  GODK-MID                            VALUE '6148'.                
009000     88  HELP-MID                            VALUE '0551'.                
009100                                                                          
009200     EJECT                                                                
009300*      --- VALID IDDC CODES                                               
009400*                                                                         
009500*01    -COPY WWDC99                                                       
009600       EJECT                                                              
009700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
009800 01  GENERELLA-SUBPROGRAM.                                                
009900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
010000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010100     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
010200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010300     03  W006KOM                 PIC X(8)    VALUE 'W006KOM'.             
010400     EJECT                                                                
010500*01 -COPY WMSGINIT                                                        
010600     EJECT                                                                
010700*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
010800*01 -COPY WMEDAREA                                                        
010900     SKIP3                                                                
011000 01  MESSAGE-CODES.                                                       
011100     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '020'.                 
011200     03  INF-PRESS-PF11          PIC X(3)    VALUE '013'.                 
011300     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '014'.                 
011400     03  INF-UPDATE-DONE         PIC X(3)    VALUE '001'.                 
011500     03  INF-FIRST-PAGE          PIC X(3)    VALUE '010'.                 
011600     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '011'.                 
011700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
011800     03  ERR-NOT-IN-REG          PIC X(3)    VALUE '027'.                 
011900     03  ERR-OTILLATEN-UPPD      PIC X(3)    VALUE '007'.                 
012000     03  ERR-DIVERSEKOLLI        PIC X(3)    VALUE '354'.                 
012100     EJECT                                                                
012200                                                                          
012300     EJECT                                                                
012400                                                                          
012500                                                                          
012600*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
012700*                                                                         
012800*01  -COPY WMFSAREA                                                       
012900     EJECT                                                                
013000 01  KOM-MSG-IO-AREA.                                                     
013100*03  -COPY WMSGKOM                                                        
013200     EJECT                                                                
013300 01  FILLER             PIC X(16)  VALUE 'MSG/KOM-AREA'.                  
013400     SKIP3                                                                
013500*01  -COPY WMSGSNUF     -PRE P-TO-P-                                      
013600     EJECT                                                                
013700 01      FILLER                  PIC X(24)   VALUE                        
013800                                 'MOD6191-MID-W6I19101'.                  
013900     SKIP2                                                                
014000     -COPY W6I19101 -PRE MOD6191-                                         
014100     EJECT                                                                
014200 01      FILLER                  PIC X(24)   VALUE                        
014300                                 'MOD6193-MID-W6I19301'.                  
014400     SKIP2                                                                
014500     -COPY W6I19301 -PRE MOD6193-                                         
014600     EJECT                                                                
014700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014800*                                                                         
014900     EJECT                                                                
015000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015100     SKIP3                                                                
015200 01  FILLER                  PIC X(16)  VALUE 'WTRAUTF8-AREA   '.         
015300*01  -COPY WTRAUTF8                                                       
015400                                                                          
015500 01  WS-IDSKYLT-SE               PIC X(3) VALUE 'S  '.                    
015600 01  WS-IDSKYLT-GB               PIC X(3) VALUE 'GB '.                    
015700 01  WS-IDSKYLT-CN               PIC X(3) VALUE 'RCN'.                    
015800                                                                          
015900 01  WS-CP-UNICODE               PIC X(4)  VALUE 'UTF8'.                  
016000 01  WS-CP-EBCDIC                PIC X(3)  VALUE '278'.                   
016100                                                                          
016200 01  NYCKLAR-TILL-DLI.                                                    
016300                                                                          
016400     03  W-IDDC-X.                                                        
016500         05  W-IDDC             PIC  X(2)   VALUE SPACE.                  
016600                                                                          
016700     03  W-IDRADNR-INL-X.                                                 
016800         05  W-IDRADNR-INL      PIC S9(5)   COMP-3 VALUE ZERO.            
016900                                                                          
017000     03  W-IDRADNR-X.                                                     
017100         05  W-IDRADNR           PIC S9(5)   COMP-3 VALUE ZERO.           
017200                                                                          
017300*--------SEQ NKL TILL INLB                                                
017400     03  W-W6D1BSEQ-X.                                                    
017500         05  W-IDLOPNRM          PIC S9(9)   COMP-3 VALUE ZERO.           
017600                                                                          
017700*--------SEQ NKL TILL INLC                                                
017800     03  W-W6D1CSEQ-X.                                                    
017900         05  W-IDLEVNRK          PIC  X(5)   VALUE SPACE.                 
018000         05  W-IDOKOLLIK         PIC  9(9)   VALUE ZERO.                  
018100                                                                          
018200     03  W-IDLEVNR-X.                                                     
018300         05  W-IDLEVNR           PIC  X(5)   VALUE SPACE.                 
018400                                                                          
018500     03  W-IDOKOLLI-X.                                                    
018600         05  W-IDOKOLLI          PIC  9(9)   VALUE ZERO.                  
018700                                                                          
018800     03  W-IDSKYLT-X.                                                     
018900         05  W-IDSKYLT           PIC X(3)    VALUE 'GB '.                 
019000     SKIP2                                                                
019100     03  W-IDARTNR-X.                                                     
019200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
019300     03  W-IDDC-B6-X.                                                     
               05 W-IDDC-B6            PIC X(2).                                
019400*    --- STATUS-KOD FRÅN IMS                                              
019500 01  STATUS-WS                   PIC XX.                                  
019600     88  SEGMENT-FINNS                       VALUE '  '.                  
019700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
019800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
019900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
020000     SKIP2                                                                
020100 01  GODK-STATUSKODER.                                                    
020200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
020300     SKIP3                                                                
020400 01  SSA1                        PIC X(64).                               
020500 01  SSA2                        PIC X(64).                               
020600     EJECT                                                                
020700*    --- IMS FUNKTIONSKODER                                               
020800*01  -COPY W0003                                                          
020900     EJECT                                                                
021000*    ---  DLI INPUT-OUTPUT AREA                                           
021100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
021200     SKIP3                                                                
021300 01  DLI-IO-AREA.                                                         
021400     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
021500     SKIP3                                                                
021600     03  W6INLA01 REDEFINES IO-AREA.                                      
021700*        05  -COPY W6D101                                                 
021800     SKIP3                                                                
021900     03  W6INLA11 REDEFINES IO-AREA.                                      
022000*        05  -COPY W6D111                                                 
022100     SKIP3                                                                
022200     03  W6INLA12 REDEFINES IO-AREA.                                      
022300*        05  -COPY W6D121                                                 
022400     SKIP3                                                                
022500 01  FILLER                PIC X(16) VALUE 'DLI-IO-WDD311'.               
022600 01  DLI-IO-WDD311.                                                       
022700*    03  -COPY WDD311                                                     
022800                                                                          
022900     EJECT                                                                
       01  FILLER                      PIC X(16)   VALUE 'WDB601 AREA'.         
       01   DLI-IO-AREA-B601.                                                   
      *     03  -COPY WDB601                                                    
           EJECT                                                                
023000 LINKAGE SECTION.                                                         
023100 01  REQU-AREA.                                                           
023200*    03 -COPY WZ01REQU                                                    
023300*    03 -COPY W60148I1                                                    
023400     EJECT                                                                
023500 01  RESP-AREA.                                                           
023600*    03 -COPY WZ01RESP                                                    
023700*    03 -COPY W60148O1                                                    
023800     EJECT                                                                
023900                                                                          
024000 01  MAX-KVRADER                 PIC S9(4) COMP.                          
024100                                                                          
024200*01  -COPY W0009   -PRE MSG-                                              
024300     EJECT                                                                
024400                                                                          
024500*01  -COPY W0009   -PRE ALT1-                                             
024600     EJECT                                                                
024700                                                                          
024800*01  -COPY W0009   -PRE DISP-                                             
024900     EJECT                                                                
025000                                                                          
025100*01  -COPY W0008  -PRE USEA-                                              
025200     05  FILLER                  PIC X.                                   
025300     EJECT                                                                
025400                                                                          
025500*01  -COPY W0008  -PRE INLBSEQ-                                           
025600     05  FILLER                  PIC X.                                   
025700     EJECT                                                                
025800                                                                          
025900*01  -COPY W0008  -PRE INLCSEQ-                                           
026000     05  FILLER                  PIC X.                                   
026100     EJECT                                                                
026200                                                                          
026300*01  -COPY W0008  -PRE KOMA-                                              
026400     05  FILLER                  PIC X.                                   
026500     EJECT                                                                
026600                                                                          
026700*01  -COPY W0008  -PRE WDD3-                                              
026800     05  FILLER                  PIC X.                                   
026900     EJECT                                                                
027000*01  -COPY W0008  -PRE WDB6-                                              
           05  FILLER                  PIC X.                                   
           EJECT                                                                
027100 PROCEDURE DIVISION  USING REQU-AREA RESP-AREA MAX-KVRADER                
027200                           MSG-PCB                                        
027300                           ALT1-PCB                                       
027400                           DISP-PCB                                       
027500                           USEA-PCB                                       
027600                           INLBSEQ-PCB                                    
027700                           INLCSEQ-PCB                                    
027800                           KOMA-PCB                                       
027900                           WDD3-PCB                                       
                                 WDB6-PCB.                                      
028000                                                                          
028100     PERFORM A-INIT                                                       
028200     PERFORM B-KOLLA-NYCKLAR                                              
028300     IF NYCKLAR-OK                                                        
028400       IF REQU-UPDATE                                                     
028500         PERFORM G-KOLLA-INPUT                                            
028600         IF INDATA-OK                                                     
028700           PERFORM H-UPPDATERA                                            
028800         END-IF                                                           
028900       ELSE                                                               
029000         IF REQU-NEXT                                                     
029100           MOVE REQU-IDRADNR-START   TO W-IDRADNR                         
029200         ELSE                                                             
029300           IF REQU-FIRST                                                  
029400             MOVE REQU-IDRADNR-START TO W-IDRADNR                         
029500           ELSE                                                           
029600             IF REQU-QUERY                                                
029700               PERFORM E-SAMMA-SIDA                                       
029800             END-IF                                                       
029900           END-IF                                                         
030000         END-IF                                                           
030100       END-IF                                                             
030200       IF INDATA-OK                                                       
030300         PERFORM F-LAES-VISA-INFO                                         
030400       END-IF                                                             
030500     END-IF                                                               
030600*                                                                         
030700     MOVE ZERO TO RETURN-CODE                                             
030800     GOBACK                                                               
030900     .                                                                    
031000     EJECT                                                                
031100 A-INIT SECTION.                                                          
031200     ACCEPT DAGENS-DATUM       FROM DATE                                  
031300                                                                          
031400*KA IF -- ENDIF                                                           
031500     IF REQU-KVRADER NOT NUMERIC                                          
031600       MOVE ZERO      TO REQU-KVRADER                                     
031700     END-IF                                                               
031800                                                                          
031900     MOVE ALL '+'     TO RESP-RAD-OUT                                     
032000     MOVE 1 TO INDX                                                       
032100     PERFORM UNTIL INDX > MAX-KVRADER                                     
032200       MOVE LOW-VALUE TO RESP-KDCMDVAL-LINE-ATTR (INDX)                   
032300       ADD 1 TO INDX                                                      
032400     END-PERFORM                                                          
032500                                                                          
032600     MOVE 001         TO RESP-IDMSGVER                                    
032700*KA  MOVE MAX-KVRADER TO RESP-KVRADER                                     
032800     MOVE ZERO        TO RESP-KVRADER                                     
032900     MOVE SPACE       TO RESP-IDMSG-ERROR                                 
033000                         RESP-IDMSG-INFO                                  
033100                         RESP-IDELMT-ERROR                                
033200     .                                                                    
033300     EJECT                                                                
033400 B-KOLLA-NYCKLAR SECTION.                                                 
033500     MOVE JA TO NYCKLAR-SW                                                
033600*                                                                         
033700*    VALIDATE IDDC KEY                                                    
033800     MOVE REQU-IDDC-KEY  TO WS-IDDC                                       
                                  W-IDDC-B6                                     
033900     IF CDC OR NDC-PACIFIC OR NDC-CN OR NDC-US OR NDC-AE OR               
              NDC-TR OR NDC-ZA                                                  
033910     OR REQU-IDMSGVER = '001'                                             
034000       MOVE WS-IDDC        TO W-IDDC                                      
034100     ELSE                                                                 
034200       MOVE NEJ            TO NYCKLAR-SW                                  
034300       MOVE ERR-WRONG-KEY  TO RESP-IDMSG-ERROR                            
034400       MOVE 'IDDC'         TO RESP-IDELMT-ERROR                           
034500       MOVE ZERO           TO RESP-KVRADER                                
034600     END-IF                                                               
034700                                                                          
034800     MOVE REQU-IDLOPNRM-KEY      TO WS-IDLOPNRM                           
034900     MOVE REQU-IDLEVNR-KEY       TO WS-IDLEVNR                            
035000     MOVE REQU-IDOKOLLI-KEY      TO WS-IDOKOLLI                           
035100*                                                                         
035200*    VALIDATE KEYS                                                        
035210     IF WS-IDLOPNRM NOT NUMERIC                                           
035220       MOVE ZERO TO WS-IDLOPNRM                                           
035230     END-IF                                                               
035240     IF WS-IDOKOLLI NOT NUMERIC                                           
035250       MOVE ZERO TO WS-IDOKOLLI                                           
035260     END-IF                                                               
035270     IF WS-IDLEVNR = ALL '+'                                              
035280       MOVE SPACE TO WS-IDLEVNR                                           
035290     END-IF                                                               
035300     IF WS-IDLOPNRM > 0                                                   
035400       MOVE '1'              TO NKL-SW                                    
035500       MOVE WS-IDLOPNRM      TO W-IDLOPNRM                                
035600       MOVE SPACE            TO WS-IDLEVNR                                
035700       MOVE ZERO             TO WS-IDOKOLLI                               
035800     ELSE                                                                 
035900       IF WS-IDLEVNR NOT = SPACE AND WS-IDOKOLLI > 0                      
036000         MOVE '2'            TO NKL-SW                                    
036100         MOVE WS-IDLEVNR     TO W-IDLEVNR                                 
036200                                W-IDLEVNRK                                
036300         MOVE WS-IDOKOLLI    TO W-IDOKOLLI                                
036400                                W-IDOKOLLIK                               
036500         MOVE ZERO           TO W-IDLOPNRM                                
036600       ELSE                                                               
036700         MOVE NEJ            TO NYCKLAR-SW                                
036800         MOVE ERR-WRONG-KEY  TO RESP-IDMSG-ERROR                          
037000         MOVE ZERO           TO RESP-KVRADER                              
037100       END-IF                                                             
037200     END-IF                                                               
           PERFORM IMS-GU-WDB601                                                
037300                                                                          
037400     IF REQU-IDRADNR-START = ALL '+'                                      
037500       MOVE ZERO TO REQU-IDRADNR-START                                    
037600     END-IF                                                               
037700     .                                                                    
037800     EJECT                                                                
037900 E-SAMMA-SIDA SECTION.                                                    
038000     IF REQU-INPUT            = ALL '+'                                   
038100       MOVE REQU-IDRADNR-START   TO W-IDRADNR                             
038200     ELSE                                                                 
038300       PERFORM EA-REQU-INDATA-TILL-RESP                                   
038400       MOVE NEJ                  TO INDATA-SW                             
038500       MOVE INF-PRESS-PF11       TO RESP-IDMSG-INFO                       
038600     END-IF                                                               
038900     MOVE REQU-KVRADER TO RESP-KVRADER                                    
039000     .                                                                    
039100     EJECT                                                                
039200 EA-REQU-INDATA-TILL-RESP SECTION.                                        
039300     MOVE +1                    TO INDX                                   
039400     PERFORM UNTIL INDX             >  MAX-KVRADER                        
039500       IF REQU-KDCMDVAL-LINE(INDX) NOT = ALL '+'                          
039600         MOVE REQU-KDCMDVAL-LINE(INDX)                                    
039700                                TO RESP-KDCMDVAL-LINE(INDX)               
039800       END-IF                                                             
039900       ADD +1                   TO INDX                                   
040000     END-PERFORM                                                          
040100     .                                                                    
040200     EJECT                                                                
040300 F-LAES-VISA-INFO SECTION.                                                
040400     PERFORM FA-LAES-GRUNDDATA                                            
040500                                                                          
040600     IF SEGMENT-SAKNAS OR SEGMENT-SLUT                                    
040700       MOVE ERR-NOT-IN-REG     TO RESP-IDMSG-ERROR                        
040800       MOVE NEJ                TO INDATA-SW                               
040900       MOVE ZERO        TO RESP-IDRADNR-START                             
041000       MOVE ZERO        TO RESP-IDRADNR-NEXT                              
041100       MOVE ZERO        TO RESP-KVRADER                                   
041200     ELSE                                                                 
041300       IF PARTI-NKL                                                       
041400         MOVE +1 TO INDX                                                  
041500         MOVE +0                TO RESP-KVRADER                           
041600         MOVE ZERO              TO W-KVRAPP                               
041610         MOVE REQU-IDLOPNRM-KEY TO RESP-IDLOPNRM                          
041700         PERFORM FB-LAES-RADDATA                                          
041800         IF SEGMENT-FINNS                                                 
041900           MOVE RAD-IDRADNR    TO RESP-IDRADNR-START                      
042000           ADD +1 TO INDX                                                 
042100         ELSE                                                             
042200           MOVE ZERO           TO RESP-IDRADNR-START                      
042300         END-IF                                                           
042400                                                                          
042500         PERFORM UNTIL INDX > MAX-KVRADER OR SEGMENT-SAKNAS OR            
042600                                             SEGMENT-SLUT                 
042700           IF SEGMENT-FINNS                                               
042800             PERFORM FB-LAES-RADDATA                                      
042900           END-IF                                                         
043000           ADD +1 TO INDX                                                 
043100         END-PERFORM                                                      
043300                                                                          
043400         IF SEGMENT-FINNS                                                 
043500           MOVE RAD-IDRADNR        TO RESP-IDRADNR-NEXT                   
043600**                                                                        
043700           PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                   
043800             IF  INDX = +1                                                
043900               IF W-IDRADNR > ZERO                                        
044000                 PERFORM IMS-GNP-INLBSEQ-W6D121-KVAL                      
044100               ELSE                                                       
044200                 PERFORM IMS-GNP-INLBSEQ-W6D121-F                         
044300               END-IF                                                     
044400             ELSE                                                         
044500               PERFORM IMS-GNP-INLBSEQ-W6D121                             
044600             END-IF                                                       
044700             IF SEGMENT-FINNS                                             
044800               IF (RAD-KDINLSTA = 'INL' OR 'VOR' OR 'FRD') OR             
044900                  (RAD-FLSATS   = JA AND ART-ADLAGOMR NOT = 30)           
045000                 COMPUTE W-KVRAPP  = W-KVRAPP + RAD-KVINLART              
045100               END-IF                                                     
045200             END-IF                                                       
045300           END-PERFORM                                                    
045600**                                                                        
045700           IF NOT REQU-UPDATE                                             
045800             MOVE INF-MORE-INFO-EXISTS TO RESP-IDMSG-INFO                 
045900           END-IF                                                         
046000         ELSE                                                             
046100           MOVE ZERO               TO RESP-IDRADNR-NEXT                   
046200           SUBTRACT +1 FROM INDX                                          
046300           PERFORM UNTIL INDX > MAX-KVRADER                               
046400             MOVE MFS-STAENG-FAELT TO                                     
046500                                  RESP-KDCMDVAL-LINE-ATTR(INDX)           
046600             ADD +1 TO INDX                                               
046700           END-PERFORM                                                    
046800         END-IF                                                           
046900         MOVE W-KVRAPP             TO RESP-KVRAPP                         
047000       ELSE                                                               
047100         PERFORM FC-LAES-KOLLIDATA                                        
047200       END-IF                                                             
047300                                                                          
047400     END-IF                                                               
047500     .                                                                    
047600     EJECT                                                                
047700 FA-LAES-GRUNDDATA SECTION.                                               
047800                                                                          
047900     IF PARTI-NKL                                                         
048000       PERFORM IMS-GU-INLBSEQ-W6D111                                      
048100     ELSE                                                                 
048200       PERFORM IMS-GU-INLCSEQ-W6D111                                      
048300     END-IF                                                               
048400     IF  SEGMENT-FINNS                                                    
048500       MOVE ART-IDARTNR   TO RESP-IDARTNR                                 
048600       MOVE ART-KVAVIS    TO RESP-KVAVIS                                  
048700       MOVE ART-ADLAGOMR  TO RESP-ADLAGOMR                                
048800       MOVE ART-ADGANG    TO RESP-ADGANG                                  
048900       MOVE ART-ADPLATS   TO RESP-ADPLATS                                 
049000       MOVE ART-IDARTNR   TO W-IDARTNR                                    
049100       MOVE ART-FLKLAR    TO W-FLKLAR                                     
049200*      MOVE ART-BEART     TO RESP-BEART                                   
049300*      -- FETCH A BETTER FLAVOR OF DESCRIPTION                            
049400       PERFORM FAC-GET-BEART                                              
049500                                                                          
049600       IF W-FLKLAR = JA                                                   
049700         MOVE ERR-OTILLATEN-UPPD TO RESP-IDMSG-ERROR                      
049800         MOVE NEJ                TO INDATA-SW                             
049900       END-IF                                                             
050000     END-IF                                                               
050100     .                                                                    
050200     EJECT                                                                
050300 FAC-GET-BEART         SECTION.                                           
050400*    -- SELECT LANGUAGE TO FETCH AND CORRESPONDING CODE-PAGE              
           MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                              
           IF DCS-UNICODE-IDSKYLT                                               
              MOVE 'UTF8'             TO TRAUTF8-KDCP                           
           ELSE                                                                 
              MOVE '278 '             TO TRAUTF8-KDCP                           
           END-IF                                                               
090000                                                                          
100000     PERFORM IMS-GU-WDD311                                                
110000     IF SEGMENT-FINNS                                                     
120000       MOVE TEXT-BEART    TO TRAUTF8-TECONV-FROM                          
130000     ELSE                                                                 
140000       MOVE SPACE         TO TRAUTF8-TECONV-FROM                          
150000                             TEXT-BEART                                   
160000       MOVE WS-CP-EBCDIC  TO TRAUTF8-KDCP                                 
170000     END-IF                                                               
180000                                                                          
           IF TRAUTF8-TECONV-FROM = SPACES                                      
            MOVE 'GB'  TO W-IDSKYLT                                             
            MOVE '278' TO TRAUTF8-KDCP                                          
            PERFORM IMS-GU-WDD311                                               
            MOVE TEXT-BEART    TO TRAUTF8-TECONV-FROM                           
           END-IF                                                               
                                                                                
190000     IF REQU-IDMSGVER = '001'                                             
200000*      CALL FROM WEB AND NDC CHINA                                        
210000*      CONVERT TO UNICODE IF NOT ALREADY SO, STRIP TRAILING SPACE         
220000       CALL WTRAUTF8 USING TRAUTF8-AREA                                   
220100       MOVE TRAUTF8-TECONV-TO TO RESP-BEART                               
220200     ELSE                                                                 
220300*      CALL FROM 3270 SCREEN. RETURN AS-IS (EBCDIC)                       
220400       MOVE TEXT-BEART        TO RESP-BEART                               
220500     END-IF                                                               
220600     .                                                                    
220700     EJECT                                                                
220800 FB-LAES-RADDATA SECTION.                                                 
220900                                                                          
221000     IF  INDX = +1                                                        
221100       IF W-IDRADNR > ZERO                                                
221200         PERFORM IMS-GNP-INLBSEQ-W6D121-KVAL                              
221300       ELSE                                                               
221400         PERFORM IMS-GNP-INLBSEQ-W6D121-F                                 
221500       END-IF                                                             
221600     ELSE                                                                 
221700       PERFORM IMS-GNP-INLBSEQ-W6D121                                     
221800     END-IF                                                               
221900                                                                          
222000     IF SEGMENT-FINNS                                                     
222100       IF RAD-KDINLSTA = 'SAK'                                            
222200         CONTINUE                                                         
222300       ELSE                                                               
222400         MOVE MFS-STAENG-FAELT TO RESP-KDCMDVAL-LINE-ATTR(INDX)           
222500         IF (RAD-KDINLSTA      = 'INL' OR 'VOR' OR 'FRD') OR              
222600            (RAD-FLSATS        = JA AND ART-ADLAGOMR NOT = 30)            
222700           COMPUTE W-KVRAPP  = W-KVRAPP + RAD-KVINLART                    
222800         END-IF                                                           
222900       END-IF                                                             
223000                                                                          
223200       MOVE RAD-IDRADNR       TO RESP-IDRADNR-LINE  (INDX)                
223300       MOVE RAD-IDLEVNR-KOLLI TO RESP-IDLEVNR-KOLLI-LINE (INDX)           
223400       MOVE RAD-IDOKOLLI      TO RESP-IDOKOLLI-LINE (INDX)                
223500       MOVE RAD-KVINLART      TO RESP-KVINLART-LINE (INDX)                
223600       MOVE RAD-ADINLOMR      TO RESP-ADINLOMR-LINE (INDX)                
223700       MOVE RAD-FLPRIO        TO RESP-FLPRIO-LINE   (INDX)                
223800       MOVE RAD-KDINLSTA      TO RESP-KDINLSTA-LINE (INDX)                
223900       IF REQU-IDSPRAK = 'EN'                                             
224000         EVALUATE RESP-KDINLSTA-LINE(INDX)                                
224100           WHEN 'FPK'                                                     
224200              MOVE 'PP '   TO RESP-KDINLSTA-LINE(INDX)                    
224300           WHEN 'INL'                                                     
224400              MOVE 'BIN'   TO RESP-KDINLSTA-LINE(INDX)                    
224500           WHEN 'SAK'                                                     
224600              MOVE 'MIS'   TO RESP-KDINLSTA-LINE(INDX)                    
224700           WHEN 'AVV'                                                     
224800              MOVE 'DEV'   TO RESP-KDINLSTA-LINE(INDX)                    
224900           WHEN 'ANT'                                                     
225000              MOVE 'DEV'   TO RESP-KDINLSTA-LINE(INDX)                    
225100           WHEN 'KVA'                                                     
225200              MOVE 'Q-D'   TO RESP-KDINLSTA-LINE(INDX)                    
225300           WHEN 'RET'                                                     
225400              MOVE 'RET'   TO RESP-KDINLSTA-LINE(INDX)                    
225500           WHEN 'FRD'                                                     
225600              MOVE 'TRP'   TO RESP-KDINLSTA-LINE(INDX)                    
225700           WHEN 'MAK'                                                     
225800              MOVE 'CAN'   TO RESP-KDINLSTA-LINE(INDX)                    
225900         END-EVALUATE                                                     
226000       END-IF                                                             
226100       ADD 1 TO RESP-KVRADER                                              
226200     END-IF                                                               
226300     .                                                                    
226400     EJECT                                                                
226500 FC-LAES-KOLLIDATA SECTION.                                               
226600     MOVE ART-IDLOPNRM TO WS-IDLOPNRM                                     
226700     MOVE WS-IDLOPNRM  TO RESP-IDLOPNRM                                   
226800     INSPECT RESP-IDLOPNRM    REPLACING LEADING ZERO BY SPACE             
226900     MOVE +0               TO RESP-KVRADER                                
227000     MOVE +1 TO INDX                                                      
227100                                                                          
227200     PERFORM IMS-GNP-INLCSEQ-W6D121                                       
227300     IF SEGMENT-FINNS                                                     
227400       IF RAD-FLDIVKLI = JA                                               
227500         MOVE ERR-DIVERSEKOLLI TO RESP-IDMSG-ERROR                        
227600       ELSE                                                               
227700         IF RAD-KDINLSTA = 'SAK'                                          
227800           CONTINUE                                                       
227900         ELSE                                                             
228000           MOVE MFS-STAENG-FAELT TO                                       
228100                                 RESP-KDCMDVAL-LINE-ATTR(INDX)            
228200         END-IF                                                           
228300                                                                          
228400         MOVE RAD-IDRADNR       TO RESP-IDRADNR-LINE(INDX)                
228500         MOVE RAD-IDLEVNR-KOLLI TO RESP-IDLEVNR-KOLLI-LINE (INDX)         
228600         MOVE RAD-IDOKOLLI      TO RESP-IDOKOLLI-LINE (INDX)              
228700         MOVE RAD-KVINLART      TO RESP-KVINLART-LINE (INDX)              
228800         MOVE RAD-ADINLOMR      TO RESP-ADINLOMR-LINE (INDX)              
228900         MOVE RAD-FLPRIO        TO RESP-FLPRIO-LINE  (INDX)               
229000         MOVE RAD-KDINLSTA      TO RESP-KDINLSTA-LINE (INDX)              
229100         IF REQU-IDSPRAK = 'EN'                                           
229200           EVALUATE RESP-KDINLSTA-LINE(INDX)                              
229300             WHEN 'FPK'                                                   
229400                MOVE 'PP '   TO RESP-KDINLSTA-LINE(INDX)                  
229500             WHEN 'INL'                                                   
229600                MOVE 'BIN'   TO RESP-KDINLSTA-LINE(INDX)                  
229700             WHEN 'SAK'                                                   
229800                MOVE 'MIS'   TO RESP-KDINLSTA-LINE(INDX)                  
229900             WHEN 'AVV'                                                   
230000                MOVE 'DEV'   TO RESP-KDINLSTA-LINE(INDX)                  
230100             WHEN 'ANT'                                                   
230200                MOVE 'DEV'   TO RESP-KDINLSTA-LINE(INDX)                  
230300             WHEN 'KVA'                                                   
230400                MOVE 'Q-D'   TO RESP-KDINLSTA-LINE(INDX)                  
230500             WHEN 'RET'                                                   
230600                MOVE 'RET'   TO RESP-KDINLSTA-LINE(INDX)                  
230700             WHEN 'FRD'                                                   
230800                MOVE 'TRP'   TO RESP-KDINLSTA-LINE(INDX)                  
230900             WHEN 'MAK'                                                   
231000                MOVE 'CAN'   TO RESP-KDINLSTA-LINE(INDX)                  
231100           END-EVALUATE                                                   
231200         END-IF                                                           
231300       END-IF                                                             
231400                                                                          
231500       ADD +1 TO INDX                                                     
231600       ADD +1 TO RESP-KVRADER                                             
231700       PERFORM UNTIL INDX > MAX-KVRADER                                   
231800         MOVE MFS-STAENG-FAELT TO RESP-KDCMDVAL-LINE-ATTR(INDX)           
231900         ADD +1 TO INDX                                                   
232000       END-PERFORM                                                        
232100                                                                          
232200     END-IF                                                               
232300     .                                                                    
232400     EJECT                                                                
232500                                                                          
232600 G-KOLLA-INPUT SECTION.                                                   
232700                                                                          
232800     MOVE JA  TO INDATA-SW                                                
232900     IF REQU-INPUT = ALL '+'                                              
233000       MOVE ERR-PF11-AND-NO-DATA TO RESP-IDMSG-ERROR                      
233100       MOVE NEJ TO INDATA-SW                                              
233200     ELSE                                                                 
233300       MOVE +1 TO INDX                                                    
233400       PERFORM UNTIL INDX > MAX-KVRADER                                   
233500         IF REQU-KDCMDVAL-LINE(INDX) NOT = ALL '+'                        
233600           IF REQU-KDCMDVAL-LINE(INDX)  = 'OK '                           
233700              MOVE MFS-ALFA-FAELT-RAETT TO                                
233800                                   RESP-KDCMDVAL-LINE-ATTR(INDX)          
233900            ELSE                                                          
234000              MOVE MFS-ALFA-FAELT-FEL TO                                  
234100                                   RESP-KDCMDVAL-LINE-ATTR(INDX)          
234200              MOVE NEJ TO INDATA-SW                                       
234300            END-IF                                                        
234400         END-IF                                                           
234500         ADD +1 TO INDX                                                   
234600       END-PERFORM                                                        
234700                                                                          
234800       IF INDATA-FEL                                                      
234900         MOVE ERR-CORR-HILITE-FLDS TO RESP-IDMSG-ERROR                    
235000       ELSE                                                               
235100         MOVE +1 TO INDX                                                  
235200         PERFORM UNTIL INDX > REQU-KVRADER OR SEGMENT-SAKNAS              
235300           IF REQU-KDCMDVAL-LINE(INDX) NOT = ALL '+'                      
235400             IF REQU-KDCMDVAL-LINE(INDX)  = 'OK '                         
235500                MOVE REQU-IDRADNR-LINE(INDX) TO W-IDRADNR                 
235600                PERFORM IMS-GU-INLBSEQ-W6D121                             
235700              END-IF                                                      
235800           END-IF                                                         
235900           ADD +1 TO INDX                                                 
236000         END-PERFORM                                                      
236100         IF SEGMENT-FINNS                                                 
236200           CONTINUE                                                       
236300         ELSE                                                             
236400           MOVE ERR-NOT-IN-REG TO RESP-IDMSG-ERROR                        
236500         END-IF                                                           
236600       END-IF                                                             
236700     END-IF                                                               
236800     .                                                                    
236900     EJECT                                                                
237000 H-UPPDATERA SECTION.                                                     
237100                                                                          
237200     MOVE +1 TO INDX                                                      
237300                6191-IX                                                   
237400     PERFORM IMS-GU-INLBSEQ-W6D111                                        
237500     MOVE ART-PRARTSTD  TO W-PRARTSTD                                     
237600     MOVE ART-KDINLPRIO TO W-KDINLPRIO                                    
237700     PERFORM UNTIL INDX > MAX-KVRADER                                     
237800       IF REQU-KDCMDVAL-LINE(INDX) NOT = ALL '+'                          
237900         MOVE REQU-IDRADNR-LINE(INDX) TO W-IDRADNR                        
238000         PERFORM IMS-GHNP-INLBSEQ-W6D121                                  
238100         IF SEGMENT-FINNS                                                 
238200           PERFORM HA-UPPDATERA-TRP                                       
238300           PERFORM HB-FYLL-I-TRANS-TILL-6191                              
238400           PERFORM HC-SKICKA-TRANS-TILL-6193                              
238500         END-IF                                                           
238600       END-IF                                                             
238700       ADD +1    TO INDX                                                  
238800     END-PERFORM                                                          
238900     PERFORM S01-SKICKA-TRANS-TILL-6191                                   
239000     MOVE INF-UPDATE-DONE TO RESP-IDMSG-INFO                              
239100     .                                                                    
239200     EJECT                                                                
239300 HA-UPPDATERA-TRP SECTION.                                                
239400                                                                          
239500     IF RAD-IDRADNR = +1                                                  
239600       MOVE RAD-W6D121          TO SPAR-RAD-W6D121                        
239700       PERFORM IMS-DLET-INLBSEQ                                           
239800       PERFORM HB-FYLL-I-TRANS-TILL-6191                                  
239900       PERFORM IMS-GNP-INLBSEQ-W6D121-L                                   
240000       COMPUTE SPAR-RAD-IDRADNR = RAD-IDRADNR + 1                         
240100       MOVE SPAR-RAD-IDRADNR    TO W-IDRADNR                              
240200       MOVE SPAR-RAD-W6D121     TO RAD-W6D121                             
240300       MOVE 'TRP'               TO RAD-KDINLSTA                           
240400       MOVE DAGENS-DATUM        TO RAD-TIUPPDAT                           
240500       PERFORM IMS-ISRT-INLBSEQ-W6D121                                    
240600     ELSE                                                                 
240700       MOVE 'TRP'               TO RAD-KDINLSTA                           
240800       MOVE DAGENS-DATUM        TO RAD-TIUPPDAT                           
240900       PERFORM IMS-REPL-INLBSEQ                                           
241000     END-IF                                                               
241100     .                                                                    
241200     EJECT                                                                
241300 HB-FYLL-I-TRANS-TILL-6191 SECTION.                                       
241400                                                                          
241500     MOVE 'W6014800'           TO MOD6191-MID-IDPGM                       
241600     MOVE WS-IDDC              TO MOD6191-MID-IDDC                        
241700     MOVE W-IDLOPNRM           TO MOD6191-MID-IDLOPNRM (6191-IX)          
241800     MOVE RAD-IDRADNR          TO MOD6191-MID-IDRADNR  (6191-IX)          
241900     MOVE W-PRARTSTD           TO MOD6191-MID-PRARTSTD (6191-IX)          
242000     MOVE W-KDINLPRIO          TO MOD6191-MID-KDINLPRIO(6191-IX)          
242100     MOVE +0                   TO MOD6191-MID-KVKOLLI  (6191-IX)          
242200     MOVE SPACE                TO MOD6191-MID-ADINLOMR-OLD                
242300                                                 (6191-IX)                
242400                                  MOD6191-MID-ADINLOMR-NXT-OLD            
242500                                                 (6191-IX)                
242600     MOVE 'SAK'                TO MOD6191-MID-KDINLSTA-OLD                
242700                                                 (6191-IX)                
242800     MOVE RAD-KVINLART         TO MOD6191-MID-KVINLART-OLD                
242900                                                 (6191-IX)                
243000                                                                          
243100     MOVE SPACE                TO MOD6191-MID-ADINLOMR-NEW                
243200                                                 (6191-IX)                
243300                                  MOD6191-MID-ADINLOMR-NXT-NEW            
243400                                                 (6191-IX)                
243500     MOVE 'TRP'                TO MOD6191-MID-KDINLSTA-NEW                
243600                                                 (6191-IX)                
243700     IF RAD-IDRADNR = +1                                                  
243800       MOVE 'J'                TO MOD6191-MID-FLINLI   (6191-IX)          
243900       MOVE +0                 TO MOD6191-MID-KVINLART-NEW                
244000                                                       (6191-IX)          
244100       MOVE 'SAK'              TO MOD6191-MID-KDINLSTA-NEW                
244200                                                       (6191-IX)          
244300     ELSE                                                                 
244400       MOVE 'N'                TO MOD6191-MID-FLINLI   (6191-IX)          
244500       MOVE RAD-KVINLART       TO MOD6191-MID-KVINLART-NEW                
244600                                                       (6191-IX)          
244700       MOVE 'TRP'              TO MOD6191-MID-KDINLSTA-NEW                
244800                                                       (6191-IX)          
244900     END-IF                                                               
245000                                                                          
245100     ADD +1                    TO 6191-IX                                 
245200     IF 6191-IX                > MAX-6191-IX                              
245300         PERFORM S01-SKICKA-TRANS-TILL-6191                               
245400     END-IF                                                               
245500     .                                                                    
245600     EJECT                                                                
245700 HC-SKICKA-TRANS-TILL-6193 SECTION.                                       
245800                                                                          
245900     MOVE SPACE                TO MSG-KOM-WMSGKOM                         
246000     COMPUTE MSG-KOM-KVLL = LENGTH OF MSG-KOM-WMSGKOM                     
246100     MOVE LOW-VALUE            TO MSG-KOM-KDZ1                            
246200     MOVE LOW-VALUE            TO MSG-KOM-KDZ2                            
246300     MOVE SPACE                TO MSG-KOM-KDTRANS                         
246400     MOVE 'W6I19301'           TO MSG-KOM-IDCPYTXT                        
246500     MOVE 'INLEV   '           TO MSG-KOM-IDSNDNOD                        
246600     MOVE 'W6014800'           TO MSG-KOM-IDSNDJOB                        
246700     ACCEPT MSG-KOM-TIREGDAT   FROM DATE                                  
246800     ACCEPT MSG-KOM-TIKLOCK    FROM TIME                                  
246900     MOVE SPACE                TO MSG-KOM-IDMFSMED                        
247000                                                                          
247100     MOVE W-IDLOPNRM           TO MOD6193-MID-IDLOPNRM                    
247200     MOVE W-IDRADNR            TO MOD6193-MID-IDRADNR                     
247300                                                                          
247400     COMPUTE P-TO-P-MSG-KVLL   =  LNG-P-TO-P-PREFIX + 12                  
247500     MOVE 'W6T193X '           TO P-TO-P-MSG-KDTRANS                      
247600     MOVE '6148'               TO P-TO-P-MSG-IDTRANS                      
247700     MOVE MFS-KDMFSFOR         TO P-TO-P-MSG-KDMFSFOR                     
247800                                                                          
247900     MOVE MOD6193-MID-W6I19301 TO P-TO-P-MSG-INDATA                       
248000                                                                          
248100     CALL W006KOM USING MSG-PCB                                           
248200                        DISP-PCB                                          
248300                        KOMA-PCB                                          
248400                        MSG-KOM-WMSGKOM                                   
248500                        P-TO-P-MSG-IO-AREA-SNUF                           
248600     .                                                                    
248700     EJECT                                                                
248800 S01-SKICKA-TRANS-TILL-6191 SECTION.                                      
248900                                                                          
249000     COMPUTE MOD6191-MID-KVPOST = 6191-IX - 1                             
249100     COMPUTE P-TO-P-MSG-KVLL    =  LNG-P-TO-P-PREFIX +                    
249200                                  17 + (MOD6191-MID-KVPOST * 64)          
249300     MOVE 'W6T191X '           TO P-TO-P-MSG-KDTRANS                      
249400     MOVE '6148'               TO P-TO-P-MSG-IDTRANS                      
249500     MOVE MFS-KDMFSFOR         TO P-TO-P-MSG-KDMFSFOR                     
249600                                                                          
249700     MOVE MOD6191-MID-W6I19101 TO P-TO-P-MSG-INDATA                       
249800                                                                          
249900     IF FOERSTA-6191                                                      
250000         PERFORM IMS-ISRT-6191-MSG                                        
250100         MOVE NEJ               TO FOERSTA-6191-SW                        
250200      ELSE                                                                
250300         PERFORM IMS-PURG-6191-MSG                                        
250400     END-IF                                                               
250500     MOVE +1                   TO 6191-IX                                 
250600     .                                                                    
250700     EJECT                                                                
250800* --- IMS SEKTIONER ---                                                   
250900     SKIP3                                                                
251000 IMS-ISRT-6191-MSG SECTION.                                               
251100     MOVE SPACE TO GODK-STATUSKODER                                       
251200     CALL CBLTDLI USING ISRT ALT1-PCB P-TO-P-MSG-IO-AREA-SNUF             
251300     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
251400     PERFORM IMS-STATUSKONTROLL                                           
251500     .                                                                    
251600     EJECT                                                                
251700 IMS-PURG-6191-MSG SECTION.                                               
251800     MOVE SPACE TO GODK-STATUSKODER                                       
251900     CALL CBLTDLI USING PURG ALT1-PCB P-TO-P-MSG-IO-AREA-SNUF             
252000     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
252100     PERFORM IMS-STATUSKONTROLL                                           
252200     .                                                                    
252300     EJECT                                                                
252400 IMS-GU-INLBSEQ-W6D111 SECTION.                                           
252500     STRING 'W6INLA11(W6D1BSEQ =' W-W6D1BSEQ-X                            
252600                    '&IDDC     =' W-IDDC-X ')'                            
252700          DELIMITED BY SIZE INTO SSA1                                     
252800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
252900     CALL CBLTDLI USING GU INLBSEQ-PCB DLI-IO-AREA SSA1                   
253000     MOVE INLBSEQ-STATUS-CODE TO STATUS-WS                                
253100     PERFORM IMS-STATUSKONTROLL                                           
253200     .                                                                    
253300     SKIP3                                                                
253400 IMS-GU-INLBSEQ-W6D121 SECTION.                                           
253500     STRING 'W6INLA11(W6D1BSEQ =' W-W6D1BSEQ-X                            
253600                    '&IDDC     =' W-IDDC-X ')'                            
253700          DELIMITED BY SIZE INTO SSA1                                     
253800     STRING 'W6INLA21(IDRADNR  =' W-IDRADNR-X ')'                         
253900          DELIMITED BY SIZE INTO SSA2                                     
254000     MOVE '  GE' TO GODK-STATUSKODER                                      
254100     CALL CBLTDLI USING GU INLBSEQ-PCB DLI-IO-AREA SSA1 SSA2              
254200     MOVE INLBSEQ-STATUS-CODE TO STATUS-WS                                
254300     PERFORM IMS-STATUSKONTROLL                                           
254400     .                                                                    
254500     SKIP3                                                                
254600 IMS-GHNP-INLBSEQ-W6D121 SECTION.                                         
254700     STRING 'W6INLA11(W6D1BSEQ =' W-W6D1BSEQ-X                            
254800                    '&IDDC     =' W-IDDC-X ')'                            
254900          DELIMITED BY SIZE INTO SSA1                                     
255000     STRING 'W6INLA21(IDRADNR  =' W-IDRADNR-X ')'                         
255100          DELIMITED BY SIZE INTO SSA2                                     
255200     MOVE '  GE' TO GODK-STATUSKODER                                      
255300     CALL CBLTDLI USING GHNP INLBSEQ-PCB DLI-IO-AREA SSA1 SSA2            
255400     MOVE INLBSEQ-STATUS-CODE TO STATUS-WS                                
255500     PERFORM IMS-STATUSKONTROLL                                           
255600     .                                                                    
255700     SKIP3                                                                
255800 IMS-GNP-INLBSEQ-W6D121-F SECTION.                                        
255900     MOVE 'W6INLA21*F' TO SSA1                                            
256000     MOVE '  GE' TO GODK-STATUSKODER                                      
256100     CALL CBLTDLI USING GNP INLBSEQ-PCB DLI-IO-AREA SSA1                  
256200     MOVE INLBSEQ-STATUS-CODE TO STATUS-WS                                
256300     PERFORM IMS-STATUSKONTROLL                                           
256400     .                                                                    
256500     SKIP3                                                                
256600 IMS-GNP-INLBSEQ-W6D121-L SECTION.                                        
256700     MOVE 'W6INLA21*L' TO SSA1                                            
256800     MOVE '  GE' TO GODK-STATUSKODER                                      
256900     CALL CBLTDLI USING GNP INLBSEQ-PCB DLI-IO-AREA SSA1                  
257000     MOVE INLBSEQ-STATUS-CODE TO STATUS-WS                                
257100     PERFORM IMS-STATUSKONTROLL                                           
257200     .                                                                    
257300     SKIP3                                                                
257400 IMS-GNP-INLBSEQ-W6D121 SECTION.                                          
257500     MOVE 'W6INLA21 ' TO SSA1                                             
257600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
257700     CALL CBLTDLI USING GNP INLBSEQ-PCB DLI-IO-AREA SSA1                  
257800     MOVE INLBSEQ-STATUS-CODE TO STATUS-WS                                
257900     PERFORM IMS-STATUSKONTROLL                                           
258000     .                                                                    
258100     SKIP2                                                                
258200 IMS-GNP-INLBSEQ-W6D121-KVAL SECTION.                                     
258300     STRING 'W6INLA21(IDRADNR  =' W-IDRADNR-X ')'                         
258400          DELIMITED BY SIZE INTO SSA1                                     
258500     MOVE '  GE' TO GODK-STATUSKODER                                      
258600     CALL CBLTDLI USING GNP INLBSEQ-PCB DLI-IO-AREA SSA1                  
258700     MOVE INLBSEQ-STATUS-CODE TO STATUS-WS                                
258800     PERFORM IMS-STATUSKONTROLL                                           
258900     .                                                                    
259000     SKIP3                                                                
259100 IMS-ISRT-INLBSEQ-W6D121 SECTION.                                         
259200     MOVE 'W6INLA21 ' TO SSA1                                             
259300     MOVE '  ' TO GODK-STATUSKODER                                        
259400     CALL CBLTDLI USING ISRT INLBSEQ-PCB DLI-IO-AREA SSA1                 
259500     MOVE INLBSEQ-STATUS-CODE TO STATUS-WS                                
259600     PERFORM IMS-STATUSKONTROLL                                           
259700     .                                                                    
259800     SKIP2                                                                
259900 IMS-REPL-INLBSEQ SECTION.                                                
260000                                                                          
260100     MOVE '  ' TO GODK-STATUSKODER                                        
260200     CALL CBLTDLI USING REPL INLBSEQ-PCB DLI-IO-AREA                      
260300     MOVE INLBSEQ-STATUS-CODE TO STATUS-WS                                
260400     PERFORM IMS-STATUSKONTROLL                                           
260500     .                                                                    
260600     SKIP2                                                                
260700 IMS-DLET-INLBSEQ SECTION.                                                
260800                                                                          
260900     MOVE '  ' TO GODK-STATUSKODER                                        
261000     CALL CBLTDLI USING DLET INLBSEQ-PCB DLI-IO-AREA                      
261100     MOVE INLBSEQ-STATUS-CODE TO STATUS-WS                                
261200     PERFORM IMS-STATUSKONTROLL                                           
261300     .                                                                    
261400     EJECT                                                                
261500 IMS-GU-INLCSEQ-W6D111 SECTION.                                           
261600     STRING 'W6INLA11(W6D1CSEQ =' W-W6D1CSEQ-X                            
261700                    '&IDDC     =' W-IDDC-X ')'                            
261800          DELIMITED BY SIZE INTO SSA1                                     
261900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
262000     CALL CBLTDLI USING GU INLCSEQ-PCB DLI-IO-AREA SSA1                   
262100     MOVE INLCSEQ-STATUS-CODE TO STATUS-WS                                
262200     PERFORM IMS-STATUSKONTROLL                                           
262300     .                                                                    
262400     SKIP3                                                                
262500 IMS-GNP-INLCSEQ-W6D121 SECTION.                                          
262600                                                                          
262700     STRING 'W6INLA21(IDLEVNRK =' W-IDLEVNR-X                             
262800                    '&IDOKOLLI =' W-IDOKOLLI-X ')'                        
262900          DELIMITED BY SIZE INTO SSA1                                     
263000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
263100     CALL CBLTDLI USING GNP INLCSEQ-PCB DLI-IO-AREA SSA1                  
263200     MOVE INLCSEQ-STATUS-CODE TO STATUS-WS                                
263300     PERFORM IMS-STATUSKONTROLL                                           
263400     .                                                                    
263500     EJECT                                                                
263600 IMS-GU-WDD311 SECTION.                                                   
263700     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
263800             DELIMITED BY SIZE INTO SSA1                                  
263900     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
264000              DELIMITED BY SIZE INTO SSA2                                 
264100     MOVE '  GE' TO GODK-STATUSKODER                                      
264200     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
264300     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
264400     PERFORM IMS-STATUSKONTROLL                                           
264500     .                                                                    
264600     EJECT                                                                
       IMS-GU-WDB601    SECTION.                                                
           STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
                DELIMITED BY SIZE INTO SSA1                                     
           MOVE '  GE' TO GODK-STATUSKODER                                      
           CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
           MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
           PERFORM IMS-STATUSKONTROLL                                           
           .                                                                    
           EJECT                                                                
264700 IMS-STATUSKONTROLL SECTION.                                              
264800                                                                          
264900     SET STATUS-IX TO 1                                                   
265000     SEARCH GODK-STATUS                                                   
265100       AT END                                                             
265200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
265300         DELIMITED BY SIZE INTO FELTEXT                                   
265400         CALL FELLOG                                                      
265500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
265600         CONTINUE                                                         
265700     END-SEARCH                                                           
265800     .                                                                    
