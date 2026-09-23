000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W611PRIO.                                                
000500*AUTHOR.         BERT ANDERSSON.                                          
000600*DATE-WRITTEN.   92/03/25.                                                
000700                                                                          
000800*    REMARKS.                                                             
000900*                                                                         
001000*    FUNKTION:                                                            
001100*        PROGRAMMET ÄR ETT SUBPROGRAM SOM BERÄKNAR OM ETT                 
001200*        KOLLI-PARTI SKALL PRIORITERAS OCH I SÅ FALL HUR MYCKET           
001300*        AV PARTIET SOM SKALL PRIORITERAS.                                
001400*        PROGRAMMET ANROPAS FRÅN AKTIVERINGSPROG.6115 OCH FRÅN            
001500*        PRIORITERINGSPROGRAMMET (W6111100) SOM GÅR TVÅ GGR.              
001600*        PER DAG (RUTIN W611S1).                                          
001700*                                                                         
001800*        PROGRAMMET UPPDATERAR W6INLA (W6D1)                              
001900*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
002000*        PROGRAMMET LÄSER      WLARTS (WDK7)                              
002100*        PROGRAMMET LÄSER      WLARTM (WDK9)                              
002200*        PROGRAMMET LÄSER      WLORDQ (WDA5A)                             
002300*        PROGRAMMET LÄSER      W6KVAE (W6H7)                              
002400*        PROGRAMMET LÄSER      W6KVAI (W6H7C)                             
002500*                                                                         
002600                                                                          
002700 ENVIRONMENT DIVISION.                                                    
002800                                                                          
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003200                                                                          
003300 DATA DIVISION.                                                           
003400                                                                          
003500 FILE SECTION.                                                            
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800     SKIP2                                                                
003900                                                                          
004000*    -- CHECKED BY WY2000                                                 
004100 77  IDPGM                       PIC X(8)    VALUE 'W611PRIO'.            
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004400 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004500 77  WS-FLPRIO-ANT               PIC S9(7) COMP-3 VALUE ZERO.             
004600 77  WS-MOJL-PRIO                PIC S9(7) COMP-3 VALUE ZERO.             
004700 77  WS-ANT-UPPDAT               PIC S9(7) COMP-3 VALUE ZERO.             
004800 77  WS-KDINLPRIO                PIC S9(3) COMP-3 VALUE ZERO.             
004900 77  WS-KDINLPRIO-OLD            PIC S9(3) COMP-3 VALUE ZERO.             
005000 77  WS-KVAVIS-PRIO              PIC S9(7) COMP-3 VALUE ZERO.             
005100 77  WS-KVAVIS-PRIO-OLD          PIC S9(7) COMP-3 VALUE ZERO.             
005200 77  SPAR-KVAVIS-PRIO            PIC S9(7) COMP-3 VALUE ZERO.             
005300 77  TOT-BEHOV              PIC S9(9)V9(1) COMP-3 VALUE ZERO.             
005400 77  WS-TILLGANGAR          PIC S9(9)V9(1) COMP-3 VALUE ZERO.             
005500 77  WS-BALANS              PIC S9(9)V9(1) COMP-3 VALUE ZERO.             
005600 77  WS-BEHOV               PIC S9(9)V9(1) COMP-3 VALUE ZERO.             
005700 77  WS-BEHOV-KLASS3X       PIC S9(9)V9(1) COMP-3 VALUE ZERO.             
005800 77  WS-KVPB-SATS           PIC S9(6)V9(1) COMP-3 VALUE ZERO.             
005900 77  WS-KVPB-SEP            PIC S9(6)V9(1) COMP-3 VALUE ZERO.             
006000 77  WS-KVOKS-TOT                PIC S9(7) COMP-3 VALUE ZERO.             
006100 77  WS-KVOKS                    PIC S9(7) COMP-3 VALUE ZERO.             
006200 77  WS-KVOKS-BULK               PIC S9(7) COMP-3 VALUE ZERO.             
006300 77  WS-KVOKS-DAG                PIC S9(7) COMP-3 VALUE ZERO.             
006400 77  WS-KVSPANT                  PIC S9(7) COMP-3 VALUE ZERO.             
006500 77  WS-KVSPARR-KVAL             PIC S9(7) COMP-3 VALUE ZERO.             
006600 77  WS-KVROS                    PIC S9(7) COMP-3 VALUE ZERO.             
006700 77  WS-KVROS-BULK               PIC S9(7) COMP-3 VALUE ZERO.             
006800 77  WS-KVROS-DAG                PIC S9(7) COMP-3 VALUE ZERO.             
006900 77  WS-KVRESS                   PIC S9(7) COMP-3 VALUE ZERO.             
007000 77  WS-KVAKS-CDC                PIC S9(7) COMP-3 VALUE ZERO.             
007100 77  WS-KVAKS-SDC                PIC S9(7) COMP-3 VALUE ZERO.             
007200 77  WS-KVLS                     PIC S9(7) COMP-3 VALUE ZERO.             
007300 77  WS-KVRORADER                PIC S9(3) COMP-3 VALUE ZERO.             
007400 77  WS-KVUTRS                   PIC S9(7) COMP-3 VALUE ZERO.             
007500 77  WS-KVPB-REF                 PIC S9(6)V9(1) COMP-3 VALUE ZERO.        
007600 77  W-SUMMA-PRIO-OFR            PIC S9(7)   VALUE ZERO COMP-3.           
007700 77  SES-INDX                    PIC S9(3)   VALUE ZERO COMP-3.           
007800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4) COMP VALUE +16.                
007900 77  WS-RAKNA-TILLGANG           PIC X       VALUE SPACE.                 
008000 77  WS-IDLOPNRM                 PIC S9(9) COMP-3 VALUE ZERO.             
008100 77  WS-ART-KVAVIS-PRIO          PIC S9(7) COMP-3 VALUE ZERO.             
008200 01  WS-RESEASON                 PIC S9V9(2) COMP-3 VALUE ZERO.           
008300*                                                                         
008400 77  UPDATE-SW                   PIC X       VALUE 'J'.                   
008500     88  UPDATE-OK                           VALUE 'J'.                   
008600     EJECT                                                                
008700 01  FELTEXT.                                                             
008800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009000     SKIP2                                                                
009100*01  WDATAREA     -COPY WDATAREA                                          
009200     EJECT                                                                
009300*      --- VALID IDDC CODES                                               
009400*                                                                         
009500*01    -COPY WWDC99                                                       
009600       EJECT                                                              
009700 01  DYNAMISKA-SUBPROGRAM.                                                
009800*                                                                         
009900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010100     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
010300*                                                                         
010400     EJECT                                                                
010500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010600     SKIP3                                                                
010700 01  NYCKLAR-TILL-DLI.                                                    
010800     03  W-W6D101KY-X.                                                    
010900         05  W-IDDC              PIC  X(2)   VALUE SPACE.                 
011000         05  W-IDLEVNR           PIC  X(5)   VALUE SPACE.                 
011100         05  W-IDFS              PIC  X(8)   VALUE SPACE.                 
011200         05  W-TIAVIDAT          PIC S9(7)   VALUE ZERO COMP-3.           
011300                                                                          
011400     03  W-IDRADNR-INL-X.                                                 
011500         05  W-IDRADNR-INL       PIC S9(5)   VALUE ZERO COMP-3.           
011600                                                                          
011700     03  W-W6D1I1KY-MIN-X.                                                
011800         05  WI1-MIN-IDARTNR     PIC S9(9)    COMP-3.                     
011900         05  WI1-MIN-IDDC        PIC X(2).                                
012000         05  WI1-MIN-IDLEVNR     PIC X(5).                                
012100         05  WI1-MIN-IDFS        PIC X(8).                                
012200         05  WI1-MIN-TIAVIDAT    PIC S9(7)    COMP-3.                     
012300         05  WI1-MIN-IDRADNR-INL PIC S9(5)    COMP-3.                     
012400                                                                          
012500     03  W-W6D1I1KY-MAX-X.                                                
012600         05  WI1-MAX-IDARTNR     PIC S9(9)    COMP-3.                     
012700         05  WI1-MAX-IDDC        PIC X(2).                                
012800         05  WI1-MAX-IDLEVNR     PIC X(5).                                
012900         05  WI1-MAX-IDFS        PIC X(8).                                
013000         05  WI1-MAX-TIAVIDAT    PIC S9(7)    COMP-3.                     
013100         05  WI1-MAX-IDRADNR-INL PIC S9(5)    COMP-3.                     
013200     SKIP2                                                                
013300     03  W-IDARTNR-X.                                                     
013400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
013500     03  W-IDDC-K7-X.                                                     
013600         05  W-IDDC-K7           PIC X(2)    VALUE SPACE.                 
013700**                                                                        
013800     03  W-IDDC-MIN.                                                      
013900         05  W-IDDC-K7-MIN       PIC X(2)    VALUE '21'.                  
014000     03  W-IDDC-MAX.                                                      
014100         05  W-IDDC-K7-MAX       PIC X(2)    VALUE '62'.                  
014200                                                                          
014300     SKIP2                                                                
014400     03  W-IDLOPNRM-X.                                                    
014500         05  W-IDLOPNRM          PIC S9(9)   VALUE ZERO COMP-3.           
014600                                                                          
014700     03  W-IDKR-X.                                                        
014800         05  W-IDKR              PIC 9(5)    VALUE ZERO.                  
014900     SKIP2                                                                
015000**NDC  DC SKA IN I NYCKELN TILL ORDERKÖBASEN                              
015100*                                                                         
015200     03  W-WDA5A1KY-MIN-X.                                                
015300         05  W-IDARTNR-ORDQ-MIN  PIC S9(9)   VALUE ZERO COMP-3.           
015400         05  W-IDDC-ORDQ-MIN     PIC X(2)    VALUE SPACE.                 
015500         05  FILLER              PIC X(33)   VALUE LOW-VALUE.             
015600     SKIP2                                                                
015700     03  W-WDA5A1KY-MAX-X.                                                
015800         05  W-IDARTNR-ORDQ-MAX  PIC S9(9)   VALUE ZERO COMP-3.           
015900         05  W-IDDC-ORDQ-MAX     PIC X(2)    VALUE SPACE.                 
016000         05  FILLER              PIC X(33)   VALUE HIGH-VALUE.            
016100     SKIP2                                                                
016200*    -- W6H7 INDEXBAS C. (W6KVAI) MIN O MAX.                              
016300     03  W-W6H7C1KY-MIN-X.                                                
016400         05  W-W6H7C1KY-MIN-IDLOPNRM                                      
016500                                 PIC S9(9)   VALUE ZERO COMP-3.           
016600         05  W-W6H7C1KY-MIN-DAAVSDAT                                      
016700                                 PIC 9(8)    VALUE ZERO.                  
016800         05  FILLER              PIC 9(5)    VALUE ZERO.                  
016900     03  W-W6H7C1KY-MAX-X.                                                
017000         05  W-W6H7C1KY-MAX-IDLOPNRM                                      
017100                                 PIC S9(9)   VALUE ZERO COMP-3.           
017200         05  W-W6H7C1KY-MAX-DAAVSDAT                                      
017300                                 PIC 9(8)    VALUE ZERO.                  
017400         05  FILLER              PIC 9(5)    VALUE ZERO.                  
017500     EJECT                                                                
017600*    --- STATUS-KOD FRÅN IMS                                              
017700 01  STATUS-WS                   PIC XX.                                  
017800     88  SEGMENT-FINNS                       VALUE '  '.                  
017900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
018000     88  END-OF-DATA                         VALUE 'GB'.                  
018100     SKIP2                                                                
018200 01  GODK-STATUSKODER.                                                    
018300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018400     SKIP3                                                                
018500 01  SSA1                        PIC X(160).                              
018600 01  SSA2                        PIC X(160).                              
018700     EJECT                                                                
018800*    --- IMS FUNKTIONSKODER                                               
018900*01  -COPY W0003                                                          
019000     EJECT                                                                
019100*    ---  DLI INPUT-OUTPUT AREA                                           
019200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
019300     SKIP3                                                                
019400 01  DLI-IO-AREA.                                                         
019500     03  IO-AREA                 PIC X(900)  VALUE SPACE.                 
019600     SKIP3                                                                
019700     03  WLARTC01 REDEFINES IO-AREA.                                      
019800*        05  -COPY WDK601  -PRE ARTC-                                     
019900     EJECT                                                                
020000     03  WLARTC11 REDEFINES IO-AREA.                                      
020100*        05  -COPY WDK611  -PRE ARTC-                                     
020200     EJECT                                                                
020300     03  WLARTM01 REDEFINES IO-AREA.                                      
020400*        05  -COPY WDK901 -PRE ARTM-                                      
020500     EJECT                                                                
020600     03  WLORDQ01 REDEFINES IO-AREA.                                      
020700*        05  -COPY WDA5A1 -PRE ORDQ-                                      
020800 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-AREA-INLA'.         
020900     SKIP3                                                                
021000 01  DLI-IO-AREA-INLA.                                                    
021100     03  IO-AREA-INLA            PIC X(150)  VALUE SPACE.                 
021200     03  W6INLA11 REDEFINES IO-AREA-INLA.                                 
021300*        05  -COPY W6D111                                                 
021400     EJECT                                                                
021500     03  W6INLA21 REDEFINES IO-AREA-INLA.                                 
021600*        05  -COPY W6D121                                                 
021700     EJECT                                                                
021800 01  FILLER                PIC X(16)   VALUE 'DLI-IO-ARTS01'.             
021900     SKIP3                                                                
022000 01  DLI-IO-AREA-ARTS01.                                                  
022100*    03  -COPY WDK701  -PRE ARTS-                                         
022200     EJECT                                                                
022300 01  FILLER                PIC X(16)   VALUE 'DLI-IO-ARTS11'.             
022400     SKIP3                                                                
022500 01  DLI-IO-AREA-ARTS11.                                                  
022600*    03  -COPY WDK711  -PRE ARTS-                                         
022700                                                                          
022800     EJECT                                                                
022900 01  FILLER                PIC X(16)   VALUE 'DLI-IO-KVAI'.               
023000     SKIP3                                                                
023100 01  DLI-IO-AREA-KVAI.                                                    
023200*    03  -COPY W6H7C1  -PRE KVAI-                                         
023300                                                                          
023400     EJECT                                                                
023500 01  FILLER                PIC X(16)   VALUE 'DLI-IO-W6D1I'.              
023600     SKIP3                                                                
023700 01  DLI-IO-AREA-W6D1I.                                                   
023800*    03  -COPY W6D1I1                                                     
023900     EJECT                                                                
024000 01  FILLER                PIC X(16)   VALUE 'DLI-IO-KVAE'.               
024100     SKIP3                                                                
024200 01  DLI-IO-AREA-KVAE.                                                    
024300*    03  -COPY W6H701                                                     
024400     EJECT                                                                
024500 LINKAGE SECTION.                                                         
024600                                                                          
024700*01  -COPY W611PRIO                                                       
024800     SKIP2                                                                
024900*01  -COPY W0008  -PRE INLA-                                              
025000     05  FILLER                  PIC X.                                   
025100     SKIP2                                                                
025200*01  -COPY W0008  -PRE ARTC-                                              
025300     05  FILLER                  PIC X.                                   
025400     SKIP2                                                                
025500*01  -COPY W0008  -PRE ARTM-                                              
025600     05  FILLER                  PIC X.                                   
025700     SKIP2                                                                
025800*01  -COPY W0008  -PRE ORDQ-                                              
025900     05  FILLER                  PIC X.                                   
026000*01  -COPY W0008  -PRE ARTS-                                              
026100     05  FILLER                  PIC X.                                   
026200*01  -COPY W0008  -PRE W6KVAI-                                            
026300     05  FILLER                  PIC X.                                   
026400*01  -COPY W0008  -PRE INLI1-                                             
026500     05  FILLER                  PIC X.                                   
026600*01  -COPY W0008  -PRE KVAE-                                              
026700     05  FILLER                  PIC X.                                   
026800     EJECT                                                                
026900 PROCEDURE DIVISION  USING PRIO-W611PRIO INLA-PCB ARTC-PCB                
027000                                         ARTM-PCB ORDQ-PCB                
027100                                         ARTS-PCB W6KVAI-PCB              
027200                                         INLI1-PCB                        
027300                                         KVAE-PCB.                        
027400     PERFORM A-INIT                                                       
027600     PERFORM B-LAES-INLA11                                                
027700     PERFORM C-LAES-ARTC                                                  
027800     PERFORM D-LAES-ARTM01                                                
027900     PERFORM I-LAES-ORDQ01                                                
028000     PERFORM E-BERAKNA-PRIORITET                                          
028100     PERFORM F-KONTROLL-AV-VARDE                                          
028200                                                                          
028300     IF UPDATE-OK                                                         
028400                                                                          
028500       PERFORM G-UPPDAT-INLA11                                            
028600       PERFORM H-EV-UPPDAT-INLA21                                         
028700                                                                          
028800     END-IF                                                               
028900                                                                          
029000     MOVE ZERO TO RETURN-CODE                                             
029100     GOBACK                                                               
029200     .                                                                    
029300     EJECT                                                                
029400 A-INIT SECTION.                                                          
029500                                                                          
029600     MOVE PRIO-IDLEVNR              TO W-IDLEVNR                          
029700     MOVE PRIO-IDDC                 TO W-IDDC                             
029800                                       W-IDDC-ORDQ-MIN                    
029900                                       W-IDDC-ORDQ-MAX                    
030000                                       W-IDDC-K7                          
030100                                       WS-IDDC                            
030200** CDC SKA ÄVEN RÄKNA MED NDC:ERNAS RESTORDER (ÄT 99001)                  
030300     IF CDC-SE                                                            
030400       MOVE '62'                    TO W-IDDC-ORDQ-MAX                    
030500     END-IF                                                               
030600                                                                          
030700     MOVE PRIO-IDFS                 TO W-IDFS                             
030800     MOVE PRIO-TIAVIDAT             TO W-TIAVIDAT                         
030900     MOVE PRIO-IDRADNR-INL          TO W-IDRADNR-INL                      
031000     MOVE PRIO-KVUPPDAT             TO WS-ANT-UPPDAT                      
031100     .                                                                    
031200     SKIP2                                                                
031300 B-LAES-INLA11  SECTION.                                                  
031400                                                                          
031500     PERFORM IMS-GU-INLA11                                                
031600     MOVE ART-KVAVIS-PRIO           TO WS-KVAVIS-PRIO-OLD                 
031700     MOVE ART-KDINLPRIO             TO WS-KDINLPRIO-OLD                   
031800     MOVE ART-IDARTNR               TO W-IDARTNR                          
031900                                       W-IDARTNR-ORDQ-MIN                 
032000                                       W-IDARTNR-ORDQ-MAX                 
032100     MOVE ART-IDLOPNRM              TO WS-IDLOPNRM                        
032200     .                                                                    
032300     SKIP2                                                                
032400 C-LAES-ARTC      SECTION.                                                
032500                                                                          
032600     IF CDC                                                               
032700       PERFORM IMS-GU-ARTC01                                              
032800       PERFORM IMS-GNP-ARTC11                                             
032900       MOVE ARTC-CLAG-KVPB-SEP         TO WS-KVPB-SEP                     
033000       MOVE ARTC-CLAG-KVPB-SATS        TO WS-KVPB-SATS                    
033100       MOVE ARTC-CLAG-KVUTRS           TO WS-KVUTRS                       
033200       MOVE ARTC-CLAG-KVLS             TO WS-KVLS                         
033300       MOVE ARTC-CLAG-KVAKS-CDC        TO WS-KVAKS-CDC                    
033400       MOVE ARTC-CLAG-KVRESS           TO WS-KVRESS                       
033500       MOVE ARTC-CLAG-KVSPANT          TO WS-KVSPANT                      
033600       MOVE ARTC-CLAG-KVSPARR-KVAL     TO WS-KVSPARR-KVAL                 
033700       MOVE ARTC-CLAG-KVROS            TO WS-KVROS                        
033800     ELSE                                                                 
033900**NDC                                                                     
034000         PERFORM IMS-GU-ARTS01                                            
034100         IF SEGMENT-FINNS                                                 
034200           PERFORM IMS-GNP-ARTS11                                         
034300           IF SEGMENT-FINNS                                               
034400             MOVE ZERO                       TO WS-KVPB-SEP               
034500                                                WS-KVPB-SATS              
034600             MOVE ARTS-SLAG-KVUTRS           TO WS-KVUTRS                 
034700             MOVE ARTS-SLAG-KVLS             TO WS-KVLS                   
034800             MOVE ARTS-SLAG-KVAKS-SDC        TO WS-KVAKS-SDC              
034900             MOVE ARTS-SLAG-KVRESS           TO WS-KVRESS                 
035000             MOVE ARTS-SLAG-KVSPARR-KVAL     TO WS-KVSPARR-KVAL           
035100             MOVE ARTS-SLAG-KVPB-REF         TO WS-KVPB-REF               
035200             MOVE ARTS-SLAG-KVROS-BULK       TO WS-KVROS-BULK             
035300             MOVE ARTS-SLAG-KVROS-DAG        TO WS-KVROS-DAG              
035400             COMPUTE WS-KVROS = WS-KVROS-BULK + WS-KVROS-DAG              
035500             MOVE ARTS-SLAG-KVOKS-BULK       TO WS-KVOKS-BULK             
035600             MOVE ARTS-SLAG-KVOKS-DAG        TO WS-KVOKS-DAG              
035700             COMPUTE WS-KVOKS-TOT = WS-KVOKS-BULK + WS-KVOKS-DAG          
035800           END-IF                                                         
035900         END-IF                                                           
036000     END-IF                                                               
036100     .                                                                    
036200     EJECT                                                                
036300 D-LAES-ARTM01         SECTION.                                           
036400                                                                          
036500*---------BERÄKNA TOTAL ORDER-KÖ-SALDO FÖR RESP.CLAGER                    
036600*---------GÖRS ENDAST FÖR CDC. NDC.ERNA HÄMTAR KVOKS FRÅN                 
036700*---------WDK7                                                            
036800                                                                          
036900     IF CDC                                                               
037000       PERFORM IMS-GU-ARTM01                                              
037100       IF SEGMENT-FINNS                                                   
037200         COMPUTE WS-KVOKS-TOT = ARTM-ART-KVOKS-BULK +                     
037300                      ARTM-ART-KVOKS-DAG + ARTM-ART-KVOKS-VOR             
037400         END-COMPUTE                                                      
037500       ELSE                                                               
037600         MOVE +0      TO WS-KVOKS-TOT                                     
037700       END-IF                                                             
037800     END-IF                                                               
037900     .                                                                    
038000     EJECT                                                                
038100 E-BERAKNA-PRIORITET  SECTION.                                            
038200                                                                          
038300     PERFORM EA-BERAKNA-TILLGANGAR                                        
038400     PERFORM EB-BERAKNA-BEHOV-4-DAGAR                                     
038500                                                                          
038600     COMPUTE WS-BALANS =                                                  
038700             WS-TILLGANGAR - WS-BEHOV                                     
038800                                                                          
038900     COMPUTE TOT-BEHOV =                                                  
039000             WS-BEHOV - WS-TILLGANGAR                                     
039100                                                                          
039200     IF WS-KVRORADER > +0 AND WS-BALANS < -0.5                            
039300       PERFORM EC-BERAKNA-PRIORITET-KLASS1X                               
039400     ELSE                                                                 
039500       IF WS-BALANS < -0.5                                                
039600         PERFORM ED-BERAKNA-PRIORITET-KLASS2X                             
039700       ELSE                                                               
039800         PERFORM EF-BERAKNA-BEHOV-KLASS3X                                 
039900         PERFORM EG-BERAKNA-PRIORITET-KLASS3X                             
040000       END-IF                                                             
040100     END-IF                                                               
040200     .                                                                    
040300     EJECT                                                                
040400 EA-BERAKNA-TILLGANGAR    SECTION.                                        
040500                                                                          
040600     IF CDC                                                               
040700       COMPUTE WS-TILLGANGAR = WS-KVLS - WS-KVRESS -                      
040800                               WS-KVSPANT - WS-KVOKS-TOT -                
040900                               WS-KVUTRS - WS-KVSPARR-KVAL                
041000       END-COMPUTE                                                        
041100                                                                          
041200       MOVE LOW-VALUE              TO  W-W6D1I1KY-MIN-X                   
041300       MOVE HIGH-VALUE             TO  W-W6D1I1KY-MAX-X                   
041400       MOVE W-IDARTNR              TO  WI1-MIN-IDARTNR                    
041500                                       WI1-MAX-IDARTNR                    
041600       MOVE PRIO-IDDC              TO  WI1-MIN-IDDC                       
041700                                       WI1-MAX-IDDC                       
041800       PERFORM IMS-GU-W6D1I                                               
041900       PERFORM UNTIL SEGMENT-SAKNAS                                       
042000         MOVE SEQI-IDDC             TO W-IDDC                             
042100         MOVE SEQI-IDLEVNR          TO W-IDLEVNR                          
042200         MOVE SEQI-IDFS             TO W-IDFS                             
042300         MOVE SEQI-TIAVIDAT         TO W-TIAVIDAT                         
042400         MOVE SEQI-IDRADNR-INL      TO W-IDRADNR-INL                      
042500         PERFORM IMS-GU-INLA11                                            
042600         IF SEGMENT-FINNS                                                 
042700           IF PRIO-IDLEVNR = SEQI-IDLEVNR AND                             
042800              PRIO-IDFS = SEQI-IDFS AND                                   
042900              PRIO-TIAVIDAT = SEQI-TIAVIDAT AND                           
043000              PRIO-IDRADNR-INL NOT = ART-IDRADNR-INL AND                  
043100              PRIO-IDPGM = 'W6011500'                                     
043200** SAMMA SÄNDNING FAST ÄNNU EJ UPPDATERAT KVAKS-CDC                       
043300             IF ART-IDLOPNRM > 0 AND ART-KVKVAPRIM-BER = 0                
043400                COMPUTE WS-TILLGANGAR = WS-TILLGANGAR +                   
043500                                                ART-KVAVIS                
043600             END-IF                                                       
043700           ELSE                                                           
043800             IF ART-IDLOPNRM NOT = 0                                      
043900               MOVE ART-IDLOPNRM      TO W-IDLOPNRM                       
044000               MOVE ART-KVAVIS-PRIO   TO WS-ART-KVAVIS-PRIO               
044100               MOVE JA                TO WS-RAKNA-TILLGANG                
044200                                                                          
044300               IF ART-KVAVIS-PRIO > 0                                     
044400                 IF W-IDLOPNRM = WS-IDLOPNRM OR                           
044500                    ART-KVKVAPRIM-BER > 0                                 
044600                   MOVE NEJ       TO WS-RAKNA-TILLGANG                    
044700                 END-IF                                                   
044800** PRIORITERAT                                                            
044900** AK INKLUDERAS I TILLGÅNG SÅVIDA INTE KR ÄR SKAPAD PÅ PARTIET           
045000** OCH PARTIET INTE ÄR UTTAGET FÖR PRIMÄRKONTROLL                         
045100                                                                          
045200                 IF WS-RAKNA-TILLGANG = JA                                
045300                   MOVE LOW-VALUE     TO W-W6H7C1KY-MIN-X                 
045400                   MOVE HIGH-VALUE    TO W-W6H7C1KY-MAX-X                 
045500                   MOVE W-IDLOPNRM    TO W-W6H7C1KY-MIN-IDLOPNRM          
045600                                         W-W6H7C1KY-MAX-IDLOPNRM          
045700                   PERFORM IMS-GU-KVAI-SEQC                               
045800                   PERFORM UNTIL SEGMENT-SAKNAS OR                        
045900                                        WS-RAKNA-TILLGANG = NEJ           
046000                     MOVE KVAI-SEQC-IDKR TO W-IDKR                        
046100                     PERFORM IMS-GU-W6KVAE01                              
046200                     IF SEGMENT-FINNS                                     
046300                       IF KR-FLANNULL = NEJ                               
046400                         MOVE NEJ  TO WS-RAKNA-TILLGANG                   
046500                       END-IF                                             
046600                     END-IF                                               
046700                     PERFORM IMS-GN-KVAI-SEQC                             
046800                   END-PERFORM                                            
046900                 END-IF                                                   
047000                                                                          
047100                 IF WS-RAKNA-TILLGANG = JA                                
047200                   PERFORM IMS-GNP-INLA21                                 
047300                   IF SEGMENT-FINNS                                       
047400                     PERFORM UNTIL SEGMENT-SAKNAS                         
047500** DELINRAPPORTERAT PARTI LIGGER KVAR MED DET URSPRUNGLIGA                
047600** PRIO-ANTALET PÅ W6D111.                                                
047700                        IF RAD-KDINLSTA = 'INL' OR 'VOR'                  
047800                          COMPUTE WS-ART-KVAVIS-PRIO =                    
047900                            WS-ART-KVAVIS-PRIO - RAD-KVINLART             
048000                          IF WS-ART-KVAVIS-PRIO < 0                       
048100                            MOVE 0 TO WS-ART-KVAVIS-PRIO                  
048200                          END-IF                                          
048300                        END-IF                                            
048400                        PERFORM IMS-GNP-INLA21                            
048500                     END-PERFORM                                          
048600                   END-IF                                                 
048700                   COMPUTE WS-TILLGANGAR = WS-TILLGANGAR +                
048800                                           WS-ART-KVAVIS-PRIO             
048900                 END-IF                                                   
049000               END-IF                                                     
049100             END-IF                                                       
049200           END-IF                                                         
049300         END-IF                                                           
049400         PERFORM IMS-GN-W6D1I                                             
049500       END-PERFORM                                                        
049600     ELSE                                                                 
049700**FÖR NDC:ERNA                                                            
049800       COMPUTE WS-TILLGANGAR = WS-KVLS - WS-KVRESS -                      
049900                               WS-KVOKS-TOT - WS-KVUTRS -                 
050000                               WS-KVSPARR-KVAL + WS-KVAKS-SDC             
050100       END-COMPUTE                                                        
050200     END-IF                                                               
050300     .                                                                    
050400     SKIP2                                                                
050500 EB-BERAKNA-BEHOV-4-DAGAR SECTION.                                        
050600                                                                          
050700*----------BEHOV BERÄKNAS FÖR FYRA DAGAR                                  
050800*----------FÖR NDC:ERNA RÄKNAS MED 4 DAGARS BEHOV SAMT MED                
050900*----------HÄNSYN TAGEN TILL SÄSONG (HÄMTAS FRÅN WDK7)                    
051000*                                                                         
051100     IF CDC                                                               
051200       COMPUTE WS-BEHOV = WS-KVROS +                                      
051300              (((WS-KVPB-SEP + WS-KVPB-SATS) * 12) / 52) * 0.8            
051400                                                                          
051500       END-COMPUTE                                                        
051600** VID FÖRBRUKNINGSBERÄKNING INKLUDERAS ÄVEN PB-SDC & PB-NDC              
051700** (ÄT 99001)                                                             
051800       PERFORM EBA-HAEMTA-SEASON                                          
051900       PERFORM IMS-GU-ARTS01                                              
052000       IF SEGMENT-FINNS                                                   
052100         PERFORM IMS-GNP-ARTS11-ALLA-DC                                   
052200         PERFORM UNTIL SEGMENT-SAKNAS                                     
052300                                                                          
052400           MOVE ARTS-SLAG-RESEASON (SES-INDX) TO WS-RESEASON              
052500                                                                          
052600           COMPUTE WS-KVROS = ARTS-SLAG-KVROS-DAG  +                      
052700                              ARTS-SLAG-KVROS-BULK                        
052800                                                                          
052900           COMPUTE WS-BEHOV = WS-BEHOV + (WS-KVROS +                      
053000               (((ARTS-SLAG-KVPB-REF * WS-RESEASON * 12) / 52)            
053100               * 0.8))                                                    
053200           END-COMPUTE                                                    
053300           PERFORM IMS-GNP-ARTS11-ALLA-DC                                 
053400         END-PERFORM                                                      
053500       END-IF                                                             
053600**                                                                        
053700       MOVE WS-BEHOV      TO WS-KVAVIS-PRIO                               
053800     ELSE                                                                 
053900**NDC                                                                     
054000       MOVE PRIO-IDDC TO W-IDDC                                           
054100                                                                          
054200       PERFORM IMS-GU-ARTS01                                              
054300       IF SEGMENT-FINNS                                                   
054400         PERFORM IMS-GNP-ARTS11                                           
054500         IF SEGMENT-FINNS                                                 
054600           PERFORM EBA-HAEMTA-SEASON                                      
054700           MOVE ARTS-SLAG-RESEASON (SES-INDX) TO WS-RESEASON              
054800         END-IF                                                           
054900       END-IF                                                             
055000                                                                          
055100       COMPUTE WS-BEHOV = WS-KVROS +                                      
055200           (((WS-KVPB-REF * WS-RESEASON * 12) / 52) * 0.8)                
055300       END-COMPUTE                                                        
055400       MOVE WS-BEHOV      TO WS-KVAVIS-PRIO                               
055500     END-IF                                                               
055600     .                                                                    
055700     EJECT                                                                
055800 EBA-HAEMTA-SEASON SECTION.                                               
055900     SKIP2                                                                
056000     ACCEPT DAGENS-DATUM FROM DATE                                        
056100     MOVE 'AAMMDD'      TO DAT-KDDATFORM                                  
056200     MOVE DAGENS-DATUM  TO DAT-I-TIDATUM                                  
056300     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
056400                         DAT-O-TIDATUM DAT-KDSVAR                         
056500     IF DAT-KDSVAR-OK                                                     
056600         MOVE DAT-TIPP  TO SES-INDX                                       
056700     ELSE                                                                 
056800         MOVE 'FEL VID ANROP TILL WDATKONV' TO FELTEXT-STR                
056900         CALL ABEND USING RKOD-ABEND-UTAN-DUMP                            
057000     END-IF                                                               
057100     .                                                                    
057200     EJECT                                                                
057300 EC-BERAKNA-PRIORITET-KLASS1X  SECTION.                                   
057400*---PRIORITETEN I KLASS 1X BEROR PÅ HUR MYCKET KVRORADER > 0              
057500     EVALUATE TRUE                                                        
057600        WHEN WS-KVRORADER > 250                                           
057700                    MOVE 10    TO WS-KDINLPRIO                            
057800        WHEN WS-KVRORADER > 200                                           
057900                    MOVE 11    TO WS-KDINLPRIO                            
058000        WHEN WS-KVRORADER > 150                                           
058100                    MOVE 12    TO WS-KDINLPRIO                            
058200        WHEN WS-KVRORADER > 100                                           
058300                    MOVE 13    TO WS-KDINLPRIO                            
058400        WHEN WS-KVRORADER > 50                                            
058500                    MOVE 14    TO WS-KDINLPRIO                            
058600        WHEN WS-KVRORADER > 10                                            
058700                    MOVE 15    TO WS-KDINLPRIO                            
058800        WHEN WS-KVRORADER > 7                                             
058900                    MOVE 16    TO WS-KDINLPRIO                            
059000        WHEN WS-KVRORADER > 5                                             
059100                    MOVE 17    TO WS-KDINLPRIO                            
059200        WHEN WS-KVRORADER > 2                                             
059300                    MOVE 18    TO WS-KDINLPRIO                            
059400        WHEN WS-KVRORADER > ZERO                                          
059500                    MOVE 19    TO WS-KDINLPRIO                            
059600     END-EVALUATE                                                         
059700     .                                                                    
059800     EJECT                                                                
059900 ED-BERAKNA-PRIORITET-KLASS2X  SECTION.                                   
060000                                                                          
060100*----PRIORITETEN I KLASS 2X ÄR TILL FÖR ARTKLAR SOM KOMMER ATT            
060200*----GÅ I RESTORDER INOM FYRA DAGAR OCH BEROR PÅ MINSTA                   
060300*----VÄRDET AV TILLGÅNGAR MINUS BEHOV UNDER FYRA DAGAR                    
060400                                                                          
060500     EVALUATE TRUE                                                        
060600        WHEN WS-BALANS < -1000                                            
060700                    MOVE 20    TO WS-KDINLPRIO                            
060800        WHEN WS-BALANS < -500                                             
060900                    MOVE 21    TO WS-KDINLPRIO                            
061000        WHEN WS-BALANS < -300                                             
061100                    MOVE 22    TO WS-KDINLPRIO                            
061200        WHEN WS-BALANS < -100                                             
061300                    MOVE 23    TO WS-KDINLPRIO                            
061400        WHEN WS-BALANS < -50                                              
061500                    MOVE 24    TO WS-KDINLPRIO                            
061600        WHEN WS-BALANS < -40                                              
061700                    MOVE 25    TO WS-KDINLPRIO                            
061800        WHEN WS-BALANS < -30                                              
061900                    MOVE 26    TO WS-KDINLPRIO                            
062000        WHEN WS-BALANS < -20                                              
062100                    MOVE 27    TO WS-KDINLPRIO                            
062200        WHEN WS-BALANS < -10                                              
062300                    MOVE 28    TO WS-KDINLPRIO                            
062400        WHEN WS-BALANS < -0                                               
062500                    MOVE 29    TO WS-KDINLPRIO                            
062600     END-EVALUATE                                                         
062700     .                                                                    
062800     EJECT                                                                
062900 EF-BERAKNA-BEHOV-KLASS3X   SECTION.                                      
063000                                                                          
063100     COMPUTE WS-BEHOV-KLASS3X = WS-KVROS +                                
063200                        WS-KVPB-SEP + WS-KVPB-SATS                        
063300     END-COMPUTE                                                          
063400                                                                          
063500*-----------TILLGÅNGAR HAR BERAKNATS I SECTION EB-BERAKNA-TILLG           
063600     COMPUTE WS-BALANS = WS-TILLGANGAR - WS-BEHOV-KLASS3X                 
063700     END-COMPUTE                                                          
063800     .                                                                    
063900     EJECT                                                                
064000 EG-BERAKNA-PRIORITET-KLASS3X  SECTION.                                   
064100                                                                          
064200*----PRIORITETEN I KLASS 3X ÄR TILL FÖR EJ PRIORITERADE                   
064300*----ARTIKLAR  OCH BEROR PÅ MINSTA VÄRDET AV TILLGÅNGAR                   
064400*----MINUS BEHOV UNDER EN PERIOD.                                         
064500                                                                          
064600     EVALUATE TRUE                                                        
064700        WHEN WS-BALANS < 1                                                
064800                    MOVE 31    TO WS-KDINLPRIO                            
064900        WHEN WS-BALANS < 10                                               
065000                    MOVE 32    TO WS-KDINLPRIO                            
065100        WHEN WS-BALANS < 20                                               
065200                    MOVE 33    TO WS-KDINLPRIO                            
065300        WHEN WS-BALANS < 30                                               
065400                    MOVE 34    TO WS-KDINLPRIO                            
065500        WHEN WS-BALANS < 40                                               
065600                    MOVE 35    TO WS-KDINLPRIO                            
065700        WHEN WS-BALANS < 50                                               
065800                    MOVE 36    TO WS-KDINLPRIO                            
065900        WHEN WS-BALANS < 100                                              
066000                    MOVE 37    TO WS-KDINLPRIO                            
066100        WHEN WS-BALANS < 300                                              
066200                    MOVE 38    TO WS-KDINLPRIO                            
066300        WHEN OTHER                                                        
066400                    MOVE 39    TO WS-KDINLPRIO                            
066500     END-EVALUATE                                                         
066600     .                                                                    
066700     EJECT                                                                
066800 F-KONTROLL-AV-VARDE SECTION.                                             
066900                                                                          
067000*--------OM SAMMA PRIORITET RÄKNATS FRAM SOM STOD PÅ INLA11               
067100*--------TIDIGARE SÅ BEHÖVER INGEN UPPDATERING UTFÖRAS.                   
067200                                                                          
067300     MOVE WS-KDINLPRIO              TO PRIO-KDINLPRIO                     
067400* SÄTTER TILLBAKA URSPRUNGSNYCKELN TILL W6D101                            
067500     MOVE PRIO-IDDC                 TO W-IDDC                             
067600     MOVE PRIO-IDLEVNR              TO W-IDLEVNR                          
067700     MOVE PRIO-IDFS                 TO W-IDFS                             
067800     MOVE PRIO-TIAVIDAT             TO W-TIAVIDAT                         
067900     MOVE PRIO-IDRADNR-INL          TO W-IDRADNR-INL                      
068000     PERFORM IMS-GHU-INLA11                                               
068100                                                                          
068200     MOVE ZERO TO WS-FLPRIO-ANT                                           
068300                  WS-MOJL-PRIO                                            
068400     IF WS-KDINLPRIO < +30                                                
068500       PERFORM FA-BERAKNA-REDAN-PRIO-UPD-INL                              
068600       PERFORM IMS-GHU-INLA11                                             
068700     END-IF                                                               
068800                                                                          
068900     IF WS-KDINLPRIO-OLD    = WS-KDINLPRIO   AND                          
069000        WS-FLPRIO-ANT      >= WS-KVAVIS-PRIO                              
069100       MOVE NEJ            TO UPDATE-SW                                   
069200     ELSE                                                                 
069300       MOVE JA             TO UPDATE-SW                                   
069400     END-IF                                                               
069500     .                                                                    
069600     EJECT                                                                
069700 FA-BERAKNA-REDAN-PRIO-UPD-INL SECTION.                                   
069800     SKIP2                                                                
069900     PERFORM IMS-GHNP-INLA21-FIRST                                        
070000     PERFORM UNTIL SEGMENT-SAKNAS OR END-OF-DATA                          
070100       IF RAD-KDINLSTA = 'FPK' OR '   ' OR 'AVI' OR 'REG'                 
070200         IF RAD-FLPRIO = JA                                               
070300           ADD RAD-KVINLART   TO WS-FLPRIO-ANT                            
070400         END-IF                                                           
070500         ADD RAD-KVINLART     TO WS-MOJL-PRIO                             
070600       ELSE                                                               
070700         IF RAD-KDINLSTA = 'INL' OR 'VOR'                                 
070800           IF RAD-FLPRIO = JA                                             
070900             MOVE NEJ TO RAD-FLPRIO                                       
071000             PERFORM IMS-REPL-INLA                                        
071100             ADD +1                   TO WS-ANT-UPPDAT                    
071200           END-IF                                                         
071300         END-IF                                                           
071400       END-IF                                                             
071500       PERFORM IMS-GHNP-INLA21                                            
071600     END-PERFORM                                                          
071700     EJECT                                                                
071800     .                                                                    
071900 G-UPPDAT-INLA11    SECTION.                                              
072000                                                                          
072100*------UPPDATERA OM NY PRIORITET < 30.                                    
072200                                                                          
072300     MOVE WS-KDINLPRIO              TO ART-KDINLPRIO                      
072400     IF WS-KDINLPRIO < 30                                                 
072500       COMPUTE W-SUMMA-PRIO-OFR = ART-KVAVIS-KIT + TOT-BEHOV              
072600       IF W-SUMMA-PRIO-OFR > WS-MOJL-PRIO                                 
072700         COMPUTE TOT-BEHOV = WS-MOJL-PRIO  - ART-KVAVIS-KIT               
072800       END-IF                                                             
072900       IF TOT-BEHOV > +0                                                  
073000         MOVE TOT-BEHOV               TO ART-KVAVIS-PRIO                  
073100                                         SPAR-KVAVIS-PRIO                 
073200                                         PRIO-KVAVIS-PRIO                 
073300       ELSE                                                               
073400         MOVE +0                      TO ART-KVAVIS-PRIO                  
073500                                         SPAR-KVAVIS-PRIO                 
073600                                         PRIO-KVAVIS-PRIO                 
073700       END-IF                                                             
073800       IF ART-KDINLPRIO              =   29 AND                           
073900          ART-KVAVIS-PRIO            =   ZERO                             
074000           MOVE 31                   TO  WS-KDINLPRIO                     
074100                                         ART-KDINLPRIO                    
074200       END-IF                                                             
074300     ELSE                                                                 
074400         MOVE +0                      TO ART-KVAVIS-PRIO                  
074500                                         SPAR-KVAVIS-PRIO                 
074600                                         PRIO-KVAVIS-PRIO                 
074700     END-IF                                                               
074800     PERFORM IMS-REPL-INLA                                                
074900                                                                          
075000     ADD +1                         TO WS-ANT-UPPDAT                      
075100     .                                                                    
075200     EJECT                                                                
075300 H-EV-UPPDAT-INLA21    SECTION.                                           
075400                                                                          
075500     PERFORM IMS-GHNP-INLA21-FIRST                                        
075600     PERFORM UNTIL SEGMENT-SAKNAS OR END-OF-DATA                          
075700       IF RAD-KDINLSTA = 'FPK' OR 'SAK' OR '   ' OR 'AVI' OR 'REG'        
075800         PERFORM HB-UPPDAT-INLA21-SEGMENT                                 
075900         PERFORM IMS-REPL-INLA                                            
076000         ADD +1                   TO WS-ANT-UPPDAT                        
076100       END-IF                                                             
076200       PERFORM IMS-GHNP-INLA21                                            
076300     END-PERFORM                                                          
076400                                                                          
076500     MOVE WS-ANT-UPPDAT             TO PRIO-KVUPPDAT                      
076600     .                                                                    
076700     EJECT                                                                
076800 HB-UPPDAT-INLA21-SEGMENT SECTION.                                        
076900                                                                          
077000     IF WS-KDINLPRIO < 30                                                 
077100       IF WS-FLPRIO-ANT >= SPAR-KVAVIS-PRIO                               
077200         IF RAD-FLPRIO = JA                                               
077300           MOVE WS-KDINLPRIO          TO RAD-KDINLPRIO                    
077400         ELSE                                                             
077500           MOVE 31                    TO RAD-KDINLPRIO                    
077600         END-IF                                                           
077700       ELSE                                                               
077800         MOVE WS-KDINLPRIO            TO RAD-KDINLPRIO                    
077900       END-IF                                                             
078000     ELSE                                                                 
078100       MOVE WS-KDINLPRIO              TO RAD-KDINLPRIO                    
078200     END-IF                                                               
078300     .                                                                    
078400     EJECT                                                                
078500 I-LAES-ORDQ01 SECTION.                                                   
078600                                                                          
078700                                                                          
078800     MOVE +0 TO WS-KVRORADER                                              
078900                                                                          
079000     PERFORM IMS-GU-ORDQ01                                                
079100     PERFORM UNTIL SEGMENT-SAKNAS OR END-OF-DATA  OR                      
079200                   WS-KVRORADER > 250                                     
079300       IF ORDQ-SEQA-KDSTARAD = 2                                          
079400         ADD +1 TO WS-KVRORADER                                           
079500       END-IF                                                             
079600                                                                          
079700       PERFORM IMS-GN-ORDQ01                                              
079800     END-PERFORM                                                          
079900     .                                                                    
080000     EJECT                                                                
080100* --- IMS SEKTIONER ---                                                   
080200     SKIP2                                                                
080300 IMS-GU-INLA11 SECTION.                                                   
080400                                                                          
080500     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
080600          DELIMITED BY SIZE INTO SSA1                                     
080700     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
080800          DELIMITED BY SIZE INTO SSA2                                     
080900     MOVE '  ' TO GODK-STATUSKODER                                        
081000     CALL CBLTDLI USING GU INLA-PCB DLI-IO-AREA-INLA SSA1 SSA2            
081100     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
081200     PERFORM IMS-STATUSKONTROLL                                           
081300     .                                                                    
081400     SKIP2                                                                
081500 IMS-GHU-INLA11 SECTION.                                                  
081600                                                                          
081700     STRING 'W6INLA01(W6D101KY =' W-W6D101KY-X ')'                        
081800          DELIMITED BY SIZE INTO SSA1                                     
081900     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
082000          DELIMITED BY SIZE INTO SSA2                                     
082100     MOVE '  ' TO GODK-STATUSKODER                                        
082200     CALL CBLTDLI USING GHU INLA-PCB DLI-IO-AREA-INLA SSA1 SSA2           
082300     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
082400     PERFORM IMS-STATUSKONTROLL                                           
082500     .                                                                    
082600     SKIP2                                                                
082700 IMS-GHNP-INLA21-FIRST SECTION.                                           
082800                                                                          
082900     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
083000          DELIMITED BY SIZE INTO SSA1                                     
083100     MOVE 'W6INLA21*F'      TO SSA2                                       
083200     MOVE '  ' TO GODK-STATUSKODER                                        
083300     CALL CBLTDLI USING GHNP INLA-PCB DLI-IO-AREA-INLA SSA1 SSA2          
083400     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
083500     PERFORM IMS-STATUSKONTROLL                                           
083600     .                                                                    
083700     SKIP2                                                                
083800 IMS-GNP-INLA21 SECTION.                                                  
083900                                                                          
084000     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
084100          DELIMITED BY SIZE INTO SSA1                                     
084200     MOVE 'W6INLA21'        TO SSA2                                       
084300     MOVE '  GE' TO GODK-STATUSKODER                                      
084400     CALL CBLTDLI USING GNP INLA-PCB DLI-IO-AREA-INLA SSA1 SSA2           
084500     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
084600     PERFORM IMS-STATUSKONTROLL                                           
084700     .                                                                    
084800     SKIP2                                                                
084900 IMS-GHNP-INLA21 SECTION.                                                 
085000                                                                          
085100     STRING 'W6INLA11(IDRADNRI =' W-IDRADNR-INL-X ')'                     
085200          DELIMITED BY SIZE INTO SSA1                                     
085300     MOVE 'W6INLA21'        TO SSA2                                       
085400     MOVE '  GE' TO GODK-STATUSKODER                                      
085500     CALL CBLTDLI USING GHNP INLA-PCB DLI-IO-AREA-INLA SSA1 SSA2          
085600     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
085700     PERFORM IMS-STATUSKONTROLL                                           
085800     .                                                                    
085900     SKIP2                                                                
086000 IMS-REPL-INLA SECTION.                                                   
086100                                                                          
086200     MOVE '  ' TO GODK-STATUSKODER                                        
086300     CALL CBLTDLI USING REPL INLA-PCB DLI-IO-AREA-INLA                    
086400     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
086500     PERFORM IMS-STATUSKONTROLL                                           
086600     .                                                                    
086700     SKIP2                                                                
086800 IMS-GU-ARTC01 SECTION.                                                   
086900                                                                          
087000     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
087100          DELIMITED BY SIZE INTO SSA1                                     
087200     MOVE '  ' TO GODK-STATUSKODER                                        
087300     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
087400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
087500     PERFORM IMS-STATUSKONTROLL                                           
087600     .                                                                    
087700     SKIP2                                                                
087800 IMS-GNP-ARTC11 SECTION.                                                  
087900                                                                          
088000     MOVE 'WLARTC11'    TO SSA1                                           
088100     MOVE '  ' TO GODK-STATUSKODER                                        
088200     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1                     
088300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
088400     PERFORM IMS-STATUSKONTROLL                                           
088500     .                                                                    
088600     SKIP2                                                                
088700 IMS-GU-ARTS01 SECTION.                                                   
088800                                                                          
088900     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
089000          DELIMITED BY SIZE INTO SSA1                                     
089100     MOVE '  GE' TO GODK-STATUSKODER                                      
089200     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-AREA-ARTS01 SSA1               
089300     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
089400     PERFORM IMS-STATUSKONTROLL                                           
089500     .                                                                    
089600     SKIP2                                                                
089700 IMS-GNP-ARTS11 SECTION.                                                  
089800                                                                          
089900     STRING 'WLARTS11(IDDC     =' W-IDDC-K7-X ')'                         
090000          DELIMITED BY SIZE INTO SSA1                                     
090100     MOVE '  GE' TO GODK-STATUSKODER                                      
090200     CALL CBLTDLI USING GNP ARTS-PCB DLI-IO-AREA-ARTS11 SSA1              
090300     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
090400     PERFORM IMS-STATUSKONTROLL                                           
090500     .                                                                    
090600     SKIP2                                                                
090700 IMS-GNP-ARTS11-ALLA-DC SECTION.                                          
090800                                                                          
090900     STRING 'WLARTS11(IDDC    >=' W-IDDC-MIN                              
091000                    '&IDDC    <=' W-IDDC-MAX  ')'                         
091100          DELIMITED BY SIZE INTO SSA1                                     
091200     MOVE '  GE' TO GODK-STATUSKODER                                      
091300     CALL CBLTDLI USING GNP ARTS-PCB DLI-IO-AREA-ARTS11 SSA1              
091400     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
091500     PERFORM IMS-STATUSKONTROLL                                           
091600     .                                                                    
091700     SKIP2                                                                
091800 IMS-GU-ARTM01 SECTION.                                                   
091900                                                                          
092000     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
092100          DELIMITED BY SIZE INTO SSA1                                     
092200     MOVE '  GE' TO GODK-STATUSKODER                                      
092300     CALL CBLTDLI USING GU ARTM-PCB DLI-IO-AREA SSA1                      
092400     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
092500     PERFORM IMS-STATUSKONTROLL                                           
092600     .                                                                    
092700     SKIP2                                                                
092800 IMS-GU-ORDQ01 SECTION.                                                   
092900                                                                          
093000     STRING 'WLORDQ01(WDA5A1KY>=' W-WDA5A1KY-MIN-X                        
093100                    '&WDA5A1KY<=' W-WDA5A1KY-MAX-X ')'                    
093200          DELIMITED BY SIZE INTO SSA1                                     
093300     MOVE '  GE' TO GODK-STATUSKODER                                      
093400     CALL CBLTDLI USING GU ORDQ-PCB DLI-IO-AREA SSA1                      
093500     MOVE ORDQ-STATUS-CODE TO STATUS-WS                                   
093600     PERFORM IMS-STATUSKONTROLL                                           
093700     .                                                                    
093800     SKIP2                                                                
093900 IMS-GN-ORDQ01 SECTION.                                                   
094000                                                                          
094100     STRING 'WLORDQ01(WDA5A1KY>=' W-WDA5A1KY-MIN-X                        
094200                    '&WDA5A1KY<=' W-WDA5A1KY-MAX-X ')'                    
094300          DELIMITED BY SIZE INTO SSA1                                     
094400     MOVE '  GEGB' TO GODK-STATUSKODER                                    
094500     CALL CBLTDLI USING GN ORDQ-PCB DLI-IO-AREA SSA1                      
094600     MOVE ORDQ-STATUS-CODE TO STATUS-WS                                   
094700     PERFORM IMS-STATUSKONTROLL                                           
094800     .                                                                    
094900     SKIP2                                                                
095000 IMS-GU-KVAI-SEQC SECTION.                                                
095100     STRING 'W6KVAI01(W6H7C1KY>=' W-W6H7C1KY-MIN-X                        
095200                    '&W6H7C1KY<=' W-W6H7C1KY-MAX-X ')'                    
095300          DELIMITED BY SIZE INTO SSA1                                     
095400     MOVE '  GE' TO GODK-STATUSKODER                                      
095500     CALL CBLTDLI USING GU W6KVAI-PCB DLI-IO-AREA-KVAI SSA1               
095600     MOVE W6KVAI-STATUS-CODE TO STATUS-WS                                 
095700     PERFORM IMS-STATUSKONTROLL                                           
095800     .                                                                    
095900     SKIP3                                                                
096000 IMS-GN-KVAI-SEQC SECTION.                                                
096100     STRING 'W6KVAI01(W6H7C1KY>=' W-W6H7C1KY-MIN-X                        
096200                    '&W6H7C1KY<=' W-W6H7C1KY-MAX-X ')'                    
096300          DELIMITED BY SIZE INTO SSA1                                     
096400     MOVE '  GE' TO GODK-STATUSKODER                                      
096500     CALL CBLTDLI USING GN W6KVAI-PCB DLI-IO-AREA-KVAI SSA1               
096600     MOVE W6KVAI-STATUS-CODE TO STATUS-WS                                 
096700     PERFORM IMS-STATUSKONTROLL                                           
096800     .                                                                    
096900     SKIP3                                                                
097000 IMS-GU-W6D1I  SECTION.                                                   
097100     STRING 'W6INLJ01(W6D1I1KY>=' W-W6D1I1KY-MIN-X                        
097200                    '&W6D1I1KY<=' W-W6D1I1KY-MAX-X ')'                    
097300             DELIMITED BY SIZE INTO SSA1                                  
097400     MOVE '  GE'                 TO GODK-STATUSKODER                      
097500     CALL CBLTDLI USING GU       INLI1-PCB                                
097600                                 DLI-IO-AREA-W6D1I                        
097700                                 SSA1                                     
097800     MOVE INLI1-STATUS-CODE      TO STATUS-WS                             
097900     PERFORM IMS-STATUSKONTROLL                                           
098000     .                                                                    
098100     SKIP2                                                                
098200 IMS-GN-W6D1I SECTION.                                                    
098300     STRING 'W6INLJ01(W6D1I1KY>=' W-W6D1I1KY-MIN-X                        
098400                    '&W6D1I1KY<=' W-W6D1I1KY-MAX-X ')'                    
098500             DELIMITED BY SIZE INTO SSA1                                  
098600     MOVE '  GE'                 TO GODK-STATUSKODER                      
098700     CALL CBLTDLI USING GN       INLI1-PCB                                
098800                                 DLI-IO-AREA-W6D1I                        
098900                                 SSA1                                     
099000     MOVE INLI1-STATUS-CODE      TO STATUS-WS                             
099100     PERFORM IMS-STATUSKONTROLL                                           
099200     .                                                                    
099300     EJECT                                                                
099400 IMS-GU-W6KVAE01 SECTION.                                                 
099500     STRING 'W6KVAE01(IDKR     =' W-IDKR-X ')'                            
099600          DELIMITED BY SIZE INTO SSA1                                     
099700     MOVE '  GE' TO GODK-STATUSKODER                                      
099800     CALL CBLTDLI USING GU KVAE-PCB DLI-IO-AREA-KVAE SSA1                 
099900     MOVE KVAE-STATUS-CODE TO STATUS-WS                                   
100000     PERFORM IMS-STATUSKONTROLL                                           
100100     .                                                                    
100200     EJECT                                                                
100300 IMS-STATUSKONTROLL SECTION.                                              
100400                                                                          
100500     SET STATUS-IX TO 1                                                   
100600     SEARCH GODK-STATUS                                                   
100700       AT END                                                             
100800         MOVE 'FELAKTIG STATUSKOD FRÅN IMS' TO FELTEXT-STR                
100900*        DISPLAY FELTEXT                                                  
101000         CALL FELLOG                                                      
101100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
101200         CONTINUE                                                         
101300     END-SEARCH                                                           
101400     .                                                                    
