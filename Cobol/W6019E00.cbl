000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6019E00.                                                
000300 AUTHOR.         UMESH JAIN.                                              
000400 DATE-WRITTEN.   12/03/26.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        BACKGROUND MPP FOR PRINTING PRE TREATMENT REPORT                 
000900*        ON WEB (CHINESE DC'S)                                            
001000*                                                                         
001100*        PROGRAMMET          READS      W6D1                              
001200*        PROGRAMMET          READS      WDK6                              
001300*        PROGRAMMET          READS      WDK7                              
001400*        PROGRAMMET          READS      WDD3                              
002000*                                                                         
002100*    INDATA.                                                              
002200*        TRANSACTION: W6019ET                                             
002300*        REQUEST:     W6019EI1                                            
002400*                                                                         
002500*    OUTDATA.                                                             
002600*        RESPONSE:    W6019EO1                                            
002700                                                                          
002800     SKIP3                                                                
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100                                                                          
003200 DATA DIVISION.                                                           
003300     EJECT                                                                
003400 WORKING-STORAGE SECTION.                                                 
003500                                                                          
003600*    -- CHECKED BY WY2000                                                 
003700 77  IDPGM                       PIC X(08)   VALUE 'W6019E00'.            
003805 77  JA                          PIC X       VALUE 'Y'.                   
003905 77  NEJ                         PIC X       VALUE 'N'.                   
003906 77  UNICODE-SPACE               PIC X(25)   VALUE all X'20'.             
004000                                                                          
004100 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +144  COMP SYNC.        
004200 77  INDX                        PIC S9(9)  VALUE +0    COMP SYNC.        
004300 77  MAX-INDX                    PIC S9(4)  VALUE +6    COMP SYNC.        
004400 77  MID-IX                      PIC S9(9)  VALUE +0    COMP SYNC.        
004500                                                                          
004600 77  WS-IDLEVNR                  PIC X(5)    VALUE SPACE.                 
004700                                                                          
004800 77  W-ADLAGOMR                  PIC 9(2)    VALUE ZERO.                  
004900 77  W-ADBUFFOMR                 PIC 9(2)    VALUE ZERO.                  
005000 77  ANT-ADBUFF                  PIC S9      VALUE ZERO COMP-3.           
005100 77  W-SPAR-IDLBBET              PIC X(12)   VALUE SPACE.                 
005200 77  WS-KVROS                    PIC S9(6)   VALUE ZERO.                  
005305 77  SW-EMB-INFO-REDIGERAD       PIC X(1)    VALUE SPACE.                 
005400                                                                          
005501*     -- AVISERAT ANTAL FÖRUTOM SATS                                      
005601 77  WS-KVAVIS-EJ-KIT        PIC S9(7)   VALUE ZERO COMP-3.               
005705*     -- REMAINDER I DIVIDE                                               
005805 77  WS-REMAINDER            PIC S9(5)   VALUE ZERO COMP-3.               
005901*     -- FÖR EMBALLAGE-RADER                                              
006001 01  WS-EMB.                                                              
006101  10   WS-EMB-IDARTNR-EMBQ     PIC S9(9)   VALUE ZERO COMP-3.             
006201  10   WS-EMB-KVQPACK          PIC S9(5)   VALUE ZERO COMP-3.             
006301  10   WS-EMB-QTYP             PIC 9(1)    VALUE ZERO.                    
006406  10   WS-KVQPACK-0            PIC S9(5)   VALUE ZERO COMP-3.             
006506  10   WS-KVQPACK-1            PIC S9(5)   VALUE ZERO COMP-3.             
006606  10   WS-KVQPACK-2            PIC S9(5)   VALUE ZERO COMP-3.             
006806  10   WS-IDARTNR-EMBQ0        PIC S9(9)   VALUE ZERO COMP-3.             
006906  10   WS-IDARTNR-EMBQ1        PIC S9(9)   VALUE ZERO COMP-3.             
007006  10   WS-IDARTNR-EMBQ2        PIC S9(9)   VALUE ZERO COMP-3.             
007101                                                                          
007200 77  INDATA-SW                   PIC X       VALUE 'Y'.                   
007300     88  INDATA-OK                           VALUE 'Y'.                   
007400     88  INDATA-FEL                          VALUE 'N'.                   
007500                                                                          
007600 77  NYCKLAR-SW                  PIC X       VALUE 'Y'.                   
007700     88  NYCKLAR-OK                          VALUE 'Y'.                   
007800     88  NYCKLAR-FEL                         VALUE 'N'.                   
007900                                                                          
008000*      --- VALID IDDC CODES                                               
008100*                                                                         
008200*01    -COPY WWDC99                                                       
008210*01    -COPY WWDCLAND                                                     
008300       EJECT                                                              
008400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008500 01  GENERELLA-SUBPROGRAM.                                                
008600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
009000     03  W400ARTU                PIC X(8)    VALUE 'W400ARTU'.            
009100     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
009200     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
009300     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
009400     EJECT                                                                
009500*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
009600*01 -COPY WMEDAREA                                                        
009700     EJECT                                                                
009800*    --- PARAMETRAR TILL SUBPROGRAM W400ARTU                              
009900 01  FILLER                      PIC X(8)    VALUE 'W400ARTU'.            
010000*01 -COPY W400ARTU                                                        
010100     EJECT                                                                
010200 01  MESSAGE-CODES.                                                       
010300     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010400     EJECT                                                                
010500 01  KDRC-DISPLAY                PIC Z(5).                                
010600 01  FELTEXT.                                                             
010700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
010800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
010900*                                                                         
011000*    --- PARAMETERS TO ABEND                                              
011100                                                                          
011200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
011300 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
011400 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
011500*                                                                         
011600 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
011700     SKIP3                                                                
011800*01  -COPY WZ01SUB                                                        
011900     SKIP3                                                                
012000 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
012100*01  -COPY WZ01SEND                                                       
012200*                                                                         
012300 01  HDR-AREA.                                                            
012400*    03  -COPY WZ01REQU  -PRE HDR-                                        
012500*    03  -COPY WZ04HDR                                                    
012600     SKIP3                                                                
012700 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
012800     SKIP3                                                                
012900 01  REQU-AREA.                                                           
013000*    03  -COPY WZ01REQU                                                   
013100*    03  -COPY W6019EI1                                                   
013200     EJECT                                                                
013210 01  RESP-AREA.                                                           
013220*    03  -COPY WZ01RESP                                                   
013230     EJECT                                                                
013300 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
013400     SKIP3                                                                
013500 01  SEND-AREA.                                                           
013600*    03  -COPY W6019E1                                                    
013700     EJECT                                                                
013800*01  -COPY WMSGAREA                                                       
013900     EJECT                                                                
014000 01  FILLER                  PIC X(16)  VALUE 'WTRAUTF8-AREA   '.         
014100*01  -COPY WTRAUTF8                                                       
014200                                                                          
014300 01  WS-IDSKYLT-GB           PIC X(3) VALUE 'GB '.                        
014400 01  WS-IDSKYLT-CN           PIC X(3) VALUE 'RCN'.                        
014500                                                                          
014600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014700*                                                                         
014800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014900     SKIP3                                                                
015000 01  NYCKLAR-TILL-DLI.                                                    
015100                                                                          
015200     03  W-IDLOPNRM-X.                                                    
015300         05  W-IDLOPNRM               PIC S9(9) COMP-3 VALUE ZERO.        
015400     03  W-W6D101KY-X.                                                    
015500         05  W-D101KY-IDDC       PIC  X(2)    VALUE SPACE.                
015600         05  W-D101KY-IDLEVNR    PIC  X(5)    VALUE SPACE.                
015700         05  W-D101KY-IDFS       PIC  X(8)    VALUE SPACE.                
015800         05  W-D101KY-TIAVIDAT   PIC S9(7)    COMP-3 VALUE ZERO.          
015900                                                                          
015910     03  W-IDRADNR-INL-X.                                                 
015920         05  W-IDRADNR-INL       PIC S9(5)   VALUE ZERO COMP-3.           
015930                                                                          
016000     03  W-W6GXKEY-6005-X.                                                
016100         05  W-6005-IDHTYP       PIC X(4)    VALUE '6005'.                
016200         05  W-6005-IDDC         PIC X(2)    VALUE SPACE.                 
016300         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
016400                                                                          
016500     03  W-W6GXKEY-6006-X.                                                
016600         05  W-6006-ADINLOMR     PIC X(4)    VALUE SPACE.                 
016700         05  FILLER              PIC X(1)    VALUE LOW-VALUE.             
016800                                                                          
016900     03  W-IDARTNR-X.                                                     
017000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
017100                                                                          
017200     03  W-IDLEVNR-X.                                                     
017300         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
017400                                                                          
017500     03  W-IDDC-X.                                                        
017600         05  W-IDDC              PIC X(2)    VALUE '71'.                  
017700                                                                          
017800     03  W-IDSKYLT-X.                                                     
017900         05  W-IDSKYLT           PIC X(3)    VALUE 'GB '.                 
017901     03  W-IDDC-B6-X.                                                     
               05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
017910     03  W-IDLAND-X.                                                      
017920         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
018000     SKIP2                                                                
018100*    --- STATUS-KOD FRÅN IMS                                              
018200 01  STATUS-WS                   PIC XX.                                  
018300     88  SEGMENT-FOUND                       VALUE '  '.                  
018400     88  SEGMENT-MISSING                     VALUE 'GE'.                  
018500     SKIP2                                                                
018600 01  GODK-STATUSKODER.                                                    
018700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018800     SKIP3                                                                
018900 01  SSA1                        PIC X(128).                              
019000 01  SSA2                        PIC X(64).                               
019100 01  SSA3                        PIC X(64).                               
019200     EJECT                                                                
019300*    --- IMS FUNKTIONSKODER                                               
019400*01  -COPY W0003                                                          
019500     EJECT                                                                
019600*    ---  DLI INPUT-OUTPUT AREA                                           
019700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
019800     SKIP3                                                                
019900 01  FILLER         PIC X(16) VALUE 'DLI-IO-W6D101'.                      
020000 01  DLI-IO-W6D101.                                                       
020100*    03  -COPY W6D101                                                     
020200                                                                          
020300 01  FILLER         PIC X(16) VALUE 'DLI-IO-W6D111'.                      
020400 01  DLI-IO-W6D111.                                                       
020500*    03  -COPY W6D111                                                     
020600                                                                          
020700 01  FILLER                PIC X(16) VALUE 'DLI-IO-WDD311'.               
020800 01  DLI-IO-WDD311.                                                       
020900*    03  -COPY WDD311                                                     
021000                                                                          
021102 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
021202 01  DLI-IO-WDK601.                                                       
021305*    03  -COPY WDK601 -PRE WDK601-                                        
021400                                                                          
021502 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
021602 01  DLI-IO-WDK611.                                                       
021702*    03  -COPY WDK611                                                     
021802                                                                          
021900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
022000 01  DLI-IO-WDK711.                                                       
022100*    03  -COPY WDK711                                                     
022200                                                                          
022210 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK712'.                      
022220 01  DLI-IO-WDK712.                                                       
022230*    03  -COPY WDK712                                                     
       01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
       01  DLI-IO-AREA-B601.                                                    
      *    03  -COPY WDB601                                                     
022240                                                                          
022300     EJECT                                                                
022400 LINKAGE SECTION.                                                         
022500                                                                          
022600*01  -COPY W0009  -PRE MSG-                                               
022700                                                                          
022800 01  DISTRWEB-PCB                PIC X.                                   
022900     EJECT                                                                
023000*01  -COPY W0008  -PRE W6D1-                                              
023100     05  FILLER                  PIC X.                                   
023200                                                                          
023300*01  -COPY W0008  -PRE WDD3-                                              
023400     05  FILLER                  PIC X.                                   
023500                                                                          
023600*01  -COPY W0008  -PRE WDK6-                                              
023700     05  FILLER                  PIC X.                                   
023800                                                                          
024000*01  -COPY W0008  -PRE WDK7-                                              
024100     05  FILLER                  PIC X.                                   
024200*01  -COPY W0008  -PRE WDB6-                                              
           05  FILLER                  PIC X.                                   
025600     EJECT                                                                
025700 PROCEDURE DIVISION  USING MSG-PCB DISTRWEB-PCB W6D1-PCB WDD3-PCB         
025802                           WDK6-PCB WDK7-PCB WDB6-PCB.                    
026100                                                                          
026200 MAIN SECTION.                                                            
026300     ENTRY 'DLITCBL' USING MSG-PCB DISTRWEB-PCB W6D1-PCB WDD3-PCB         
026402                           WDK6-PCB WDK7-PCB WDB6-PCB.                    
026700                                                                          
026800     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
026900     IF SUB-KDRC = 0                                                      
027000       PERFORM A-INIT                                                     
027100       PERFORM B-PROCESS-REQU-DATA                                        
027200*****  PERFORM S02-RETURN-RESPONSE // RESPONSE COMES FROM D&P             
027300     END-IF                                                               
027310     IF INDATA-FEL                                                        
027320       PERFORM S12-RETURN-RESPONSE                                        
027330     END-IF                                                               
027400                                                                          
027500     MOVE ZERO TO RETURN-CODE                                             
027600     GOBACK                                                               
027700     .                                                                    
027800     EJECT                                                                
027900 A-INIT SECTION.                                                          
028105     MOVE JA                TO INDATA-SW                                  
028200                               NYCKLAR-SW                                 
028300                                                                          
028400     INITIALIZE RR-W6019E1                                                
028500     MOVE REQU-IDDC         TO WS-IDDC                                    
028510                               W-IDDC                                     
                                     W-IDDC-B6                                  
028600     MOVE SPACE             TO RESP-AREA                                  
028601     MOVE 001               TO RESP-IDMSGVER                              
           PERFORM IMS-GU-WDB601                                                
028610     PERFORM S02-SEARCH-IDLAND                                            
028700     .                                                                    
028800     EJECT                                                                
028900 B-PROCESS-REQU-DATA SECTION.                                             
029100     MOVE REQU-IDDC         TO W-D101KY-IDDC                              
029200     MOVE REQU-IDLEVNR      TO W-D101KY-IDLEVNR                           
029300                               W-IDLEVNR                                  
029400     MOVE REQU-IDFS         TO W-D101KY-IDFS                              
029500     IF REQU-TIAVIDAT IS NUMERIC                                          
029600       MOVE REQU-TIAVIDAT   TO W-D101KY-TIAVIDAT                          
029700     END-IF                                                               
029710     IF REQU-IDRADNR-INL IS NUMERIC                                       
029720       MOVE REQU-IDRADNR-INL TO W-IDRADNR-INL                             
029730     END-IF                                                               
029800*                                                                         
029900     PERFORM IMS-GU-W6D111                                                
030000     IF SEGMENT-FOUND                                                     
030500       PERFORM S90-OPEN-DAP-SEND-WEB                                      
030600       PERFORM S90-PUT-DAP-HEADER                                         
030700                                                                          
031000       MOVE ART-IDARTNR      TO RR-IDARTNR                                
031100                                W-IDARTNR                                 
031200       MOVE ART-IDLOPNRM      TO RR-IDLOPNRM                              
031400                                                                          
031601       MOVE ART-KVAVIS        TO RR-KVAVIS                                
031700                                                                          
032400       MOVE ART-KDSORT        TO RR-KDSORT                                
032500       MOVE ART-BEFT          TO RR-BEFT                                  
032600       MOVE ART-KVAVIS-PRIO   TO RR-KVAVIS-PRIO                           
032700       MOVE ART-ADLAGOMR      TO RR-ADLAGOMR                              
032800       MOVE ART-ADGANG        TO RR-ADGANG                                
032900       MOVE ART-ADPLATS       TO RR-ADPLATS                               
033001                                                                          
033101       SUBTRACT ART-KVAVIS-KIT FROM ART-KVAVIS                            
033201       GIVING WS-KVAVIS-EJ-KIT                                            
033300                                                                          
033400       IF ART-KDKVAANT > ZERO                                             
033505        MOVE JA               TO RR-KDKVAANT                              
033600       ELSE                                                               
033700        MOVE SPACES           TO RR-KDKVAANT                              
033800       END-IF                                                             
033900                                                                          
034000       MOVE ART-KVKVAPRIM-BER TO RR-KVKVAPRIM-BER                         
034100       MOVE ART-VKART         TO RR-VKART                                 
034200       MOVE ART-VLARTNTO      TO RR-VLARTNTO                              
034300                                                                          
034400       PERFORM IMS-GU-WDK611                                              
034500       IF SEGMENT-FOUND                                                   
034604         MOVE CLAG-KVQPACK-3       TO RR-KVQPACK-3                        
034701         MOVE CLAG-KVQPACK-0       TO WS-KVQPACK-0                        
034801         MOVE CLAG-KVQPACK-1       TO WS-KVQPACK-1                        
034901         MOVE CLAG-KVQPACK-2       TO WS-KVQPACK-2                        
034902         MOVE CLAG-IDARTNR-EMBQ0   TO WS-IDARTNR-EMBQ0                    
034903         MOVE CLAG-IDARTNR-EMBQ1   TO WS-IDARTNR-EMBQ1                    
034904         MOVE CLAG-IDARTNR-EMBQ2   TO WS-IDARTNR-EMBQ2                    
034905         IF NDC-CN OR NDC-US                                              
034907           PERFORM IMS-GU-WDK712                                          
034909           IF LART-IDARTNR-EMBQ0 > 0                                      
034910             MOVE LART-IDARTNR-EMBQ0 TO WS-IDARTNR-EMBQ0                  
035302           END-IF                                                         
035303           IF LART-IDARTNR-EMBQ1 > 0                                      
035304             MOVE LART-IDARTNR-EMBQ1 TO WS-IDARTNR-EMBQ1                  
035307           END-IF                                                         
035308           IF LART-IDARTNR-EMBQ2 > 0                                      
035309             MOVE LART-IDARTNR-EMBQ2 TO WS-IDARTNR-EMBQ2                  
035330           END-IF                                                         
035340           IF LART-KVQPACK-3 > 0                                          
035350             MOVE LART-KVQPACK-3     TO RR-KVQPACK-3                      
035360           END-IF                                                         
035605         END-IF                                                           
035610       END-IF                                                             
035700                                                                          
035800       PERFORM IMS-GU-WDK711                                              
035900       IF SEGMENT-FOUND                                                   
036000         COMPUTE WS-KVROS = SLAG-KVROS-DAG + SLAG-KVROS-BULK              
036100         MOVE WS-KVROS       TO RR-KVROS                                  
036200       END-IF                                                             
036300                                                                          
036400       PERFORM BA-CALL-W400ARTU                                           
036500       MOVE ARTU-BEARTURS-ENG TO  RR-BEARTURS                             
036600                                                                          
036700       PERFORM BC-GET-BEART-CHINESE                                       
036800       MOVE TRAUTF8-TECONV-TO   TO RR-BEART                               
037401       PERFORM BB-GET-PRE-PACK-DETAILS                                    
037700       PERFORM S90-PUT-LINE                                               
038000       PERFORM S90-CLOSE-DAP-SEND                                         
038010     ELSE                                                                 
038020       MOVE NEJ TO INDATA-SW                                              
038030       MOVE '025'      TO RESP-IDMSG-ERROR                                
038040       MOVE 'IDLOPNRM' TO RESP-IDELMT-ERROR                               
038100     END-IF                                                               
038200     .                                                                    
038300     EJECT                                                                
038400 BA-CALL-W400ARTU       SECTION.                                          
038500* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
038600* ÖVERSÄTTER EN ARTIKELS URSPRUNGSKOD TILL KLARTEXT             *         
038700* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
038800     MOVE ART-KDARTURS   TO ARTU-KDARTURS                                 
038900     MOVE ZERO           TO ARTU-IDDISTR                                  
039000     MOVE WS-IDDC        TO ARTU-IDDC                                     
039100                                                                          
039200     CALL W400ARTU USING ARTU-W400ARTU                                    
039300     .                                                                    
039400     EJECT                                                                
039501 BB-GET-PRE-PACK-DETAILS SECTION.                                         
039601     MOVE WS-IDARTNR-EMBQ0    TO WS-EMB-IDARTNR-EMBQ                      
039701     MOVE WS-KVQPACK-0        TO WS-EMB-KVQPACK                           
039801     MOVE +1 TO INDX                                                      
039904     MOVE 0                   TO RR-FP-KDEMBKOD (INDX)                    
040001     PERFORM BBA-BEH-EMB-RAD                                              
040100                                                                          
041001     MOVE WS-IDARTNR-EMBQ1    TO WS-EMB-IDARTNR-EMBQ                      
042001     MOVE WS-KVQPACK-1        TO WS-EMB-KVQPACK                           
042101     MOVE +2 TO INDX                                                      
043004     MOVE 1                   TO RR-FP-KDEMBKOD (INDX)                    
043201     PERFORM BBA-BEH-EMB-RAD                                              
043301                                                                          
043401     MOVE WS-IDARTNR-EMBQ2    TO WS-EMB-IDARTNR-EMBQ                      
043501     MOVE WS-KVQPACK-2        TO WS-EMB-KVQPACK                           
043601     MOVE +3 TO INDX                                                      
043704     MOVE 2                   TO RR-FP-KDEMBKOD (INDX)                    
043901     PERFORM BBA-BEH-EMB-RAD                                              
044101     .                                                                    
044201     EJECT                                                                
044301 BBA-BEH-EMB-RAD SECTION.                                                 
044501     MOVE NEJ  TO SW-EMB-INFO-REDIGERAD                                   
044601                                                                          
044602     MOVE UNICODE-SPACE TO RR-FP-BEART (INDX)                             
044603                                                                          
044701     IF  WS-EMB-IDARTNR-EMBQ > ZERO                                       
044801       MOVE WS-EMB-IDARTNR-EMBQ   TO W-IDARTNR                            
044902                                     RR-FP-IDARTNR  (INDX)                
045002       PERFORM IMS-GU-WDK601                                              
045105       IF SEGMENT-FOUND                                                   
045202         PERFORM BC-GET-BEART-CHINESE                                     
045303         MOVE TRAUTF8-TECONV-TO TO RR-FP-BEART (INDX)                     
045801                                                                          
046002         PERFORM IMS-GU-WDK711                                            
046105         IF SEGMENT-FOUND                                                 
046201           MOVE SLAG-ADLAGOMR     TO RR-FP-ADLAGOMR (INDX)                
046301           MOVE SLAG-ADGANG       TO RR-FP-ADGANG   (INDX)                
046401           MOVE SLAG-ADPLATS      TO RR-FP-ADPLATS  (INDX)                
046501         END-IF                                                           
048101                                                                          
048202         PERFORM BBAA-REDIG-EMB-KVQPACK                                   
048301         MOVE WS-EMB-KVQPACK      TO RR-FP-KVQPACK  (INDX)                
048401                                                                          
048602         MOVE JA                  TO SW-EMB-INFO-REDIGERAD                
048701       END-IF                                                             
048801     END-IF                                                               
048901                                                                          
049001     IF SW-EMB-INFO-REDIGERAD = NEJ                                       
049202       IF WS-EMB-IDARTNR-EMBQ > ZERO                                      
049302          OR WS-EMB-KVQPACK   > ZERO                                      
049502         IF WS-EMB-IDARTNR-EMBQ < 100                                     
049802           PERFORM BBAA-REDIG-EMB-KVQPACK                                 
049902           MOVE WS-EMB-KVQPACK TO RR-FP-KVQPACK (INDX)                    
050201         END-IF                                                           
050301       END-IF                                                             
050401     END-IF                                                               
051701     .                                                                    
051801     EJECT                                                                
051902 BBAA-REDIG-EMB-KVQPACK SECTION.                                          
052101*Ändrade regler för utskrift av KVQPACK, tidigare behandlades             
052201*KVQPACK = ZERO som om den vore +1. Ändrat av Wolfgang Kux 940503.        
052301     IF  WS-EMB-KVQPACK = ZERO                                            
052401       CONTINUE                                                           
052501     ELSE                                                                 
052601       DIVIDE WS-KVAVIS-EJ-KIT BY WS-EMB-KVQPACK                          
052701           GIVING WS-EMB-KVQPACK                                          
052801           REMAINDER WS-REMAINDER                                         
052901                                                                          
053001       IF  WS-REMAINDER > ZERO                                            
053101         ADD +1                    TO WS-EMB-KVQPACK                      
053201       END-IF                                                             
053301     END-IF                                                               
053401     .                                                                    
053501     EJECT                                                                
053801 BC-GET-BEART-CHINESE.                                                    
053901*    -- READ CHINESE OR ENGLISH BEART                                     
054001*    -- ENGLISH WILL BE TRANSLATED TO UNICODE                             
           MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                              
           IF DCS-UNICODE-IDSKYLT                                               
              MOVE 'UTF8'             TO TRAUTF8-KDCP                           
           ELSE                                                                 
              MOVE '278 '             TO TRAUTF8-KDCP                           
           END-IF                                                               
054801                                                                          
054901     PERFORM IMS-GU-WDD311                                                
055001     IF SEGMENT-FOUND                                                     
055101       MOVE TEXT-BEART    TO TRAUTF8-TECONV-FROM                          
055201     ELSE                                                                 
055301       MOVE SPACE         TO TRAUTF8-TECONV-FROM                          
055401       MOVE '278'         TO TRAUTF8-KDCP                                 
055501     END-IF                                                               
           IF TRAUTF8-TECONV-FROM = SPACES                                      
            MOVE 'GB'  TO W-IDSKYLT                                             
            MOVE '278' TO TRAUTF8-KDCP                                          
            PERFORM IMS-GU-WDD311                                               
            MOVE TEXT-BEART    TO TRAUTF8-TECONV-FROM                           
           END-IF                                                               
055601                                                                          
055701*    -- STRIP SPACE OR CONVERT TO UNICODE                                 
055801     CALL WTRAUTF8 USING TRAUTF8-AREA                                     
055901                                                                          
056101     .                                                                    
056201     EJECT                                                                
056300 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
056400     MOVE 'GETARG'               TO SUB-KDFUNC                            
056500     MOVE 'CARPARTS.NDC.CREATEPRETREATMENTREPORT'                         
056600                                 TO SUB-ADDISPABS                         
056700     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
056800                                                                          
056900     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
057000                                                                          
057100     IF SUB-KDRC > 0                                                      
057200       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
057300       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
057400       DELIMITED BY SIZE INTO FELTEXT                                     
057500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
057600     END-IF                                                               
057700     .                                                                    
057800     SKIP3                                                                
057810 S02-SEARCH-IDLAND SECTION.                                               
057820     SEARCH ALL DC-LAND                                                   
057830       AT END                                                             
057840         MOVE 'EJ TRÄFF I TAB DCLAND'                                     
057850                            TO FELTEXT-STR                                
057860         CALL FELLOG                                                      
057870       WHEN DCLAND-IDDC (DCLAND-IX) = W-IDDC                              
057880         MOVE DCLAND-IDLANDX2 (DCLAND-IX)                                 
057890                            TO W-IDLAND                                   
057891     END-SEARCH                                                           
057892     .                                                                    
057893     EJECT                                                                
057894 S12-RETURN-RESPONSE SECTION.                                             
057895     MOVE 'RETURN'                   TO SUB-KDFUNC                        
057896     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
057897                                                                          
057898     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
057899                                                                          
057900     IF SUB-KDRC > 0                                                      
057901       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
057902       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
057903       DELIMITED BY SIZE INTO FELTEXT                                     
057904       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
057905     END-IF                                                               
057906     .                                                                    
057907     EJECT                                                                
059300 S90-OPEN-DAP-SEND-WEB SECTION.                                           
059400     MOVE 'OPEN'                  TO SEND-KDFUNC                          
059500*    -- WEB RESPONSE SHOULD HAVE LOWER PRIO TO FINISH LAST                
059600*    -- DISTRDOC = WZ0420X HAS LOWER PRIO THAN WZ0420U                    
059700     MOVE 'CARPARTS.DAP.DISTRDOC' TO SEND-ADDISPABS                       
059800     CALL WZ01SEND USING SEND-CONTROL-AREA                                
059900                         SEND-OPEN-AREA                                   
060000     IF SEND-KDRC > ZERO                                                  
060100       MOVE SEND-KDRC             TO KDRC-DISPLAY                         
060200       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
060300       DELIMITED BY SIZE INTO FELTEXT                                     
060400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
060500     END-IF                                                               
060600     .                                                                    
060700     SKIP3                                                                
060800 S90-PUT-DAP-HEADER SECTION.                                              
060900     MOVE 1                       TO HDR-REQU-IDMSGVER                    
061000     MOVE REQU-KDPGMACT           TO HDR-REQU-KDPGMACT                    
061100     MOVE REQU-IDUSER             TO HDR-REQU-IDUSER                      
061200     MOVE 'W6019E-001'            TO HDR-IDOUTTYPE                        
061300     MOVE REQU-IDDC               TO HDR-IDOUTREC (1:2)                   
061400     MOVE REQU-IDUSER             TO HDR-IDOUTREC (3:)                    
061500     MOVE SPACE                   TO HDR-IDLIST                           
061600     MOVE 'PUT'                   TO SEND-KDFUNC                          
061700     MOVE LENGTH OF HDR-AREA      TO SEND-KVDLEN                          
061800     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
061900                                     SEND-KVDLEN                          
062000                                     HDR-AREA                             
062100     IF SEND-KDRC > ZERO                                                  
062200       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
062300       STRING 'WZ01RECV GET  ERROR RC= ' KDRC-DISPLAY                     
062400       DELIMITED BY SIZE       INTO FELTEXT-STR                           
062500       DISPLAY FELTEXT                                                    
062600       CALL FELLOG                                                        
062700     END-IF                                                               
062800     .                                                                    
062900     EJECT                                                                
063000 S90-PUT-LINE SECTION.                                                    
063100     MOVE 'PUT'                   TO SEND-KDFUNC                          
063200     MOVE LENGTH OF RR-W6019E1    TO SEND-KVDLEN                          
063300     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
063400                                     SEND-KVDLEN                          
063500                                     RR-W6019E1                           
063600     IF SEND-KDRC > ZERO                                                  
063700       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
063800       STRING 'WZ01RECV GET  ERROR RC= ' KDRC-DISPLAY                     
063900       DELIMITED BY SIZE       INTO FELTEXT-STR                           
064000       DISPLAY FELTEXT                                                    
064100       CALL FELLOG                                                        
064200     END-IF                                                               
064300     .                                                                    
064400     SKIP2                                                                
064500 S90-CLOSE-DAP-SEND SECTION.                                              
064600     MOVE 'CLOSE'                 TO SEND-KDFUNC                          
064700     CALL WZ01SEND USING SEND-CONTROL-AREA                                
064800                                                                          
064900     IF SEND-KDRC > 0                                                     
065000       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
065100       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
065200       DELIMITED BY SIZE INTO FELTEXT                                     
065300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
065400     END-IF                                                               
065500     .                                                                    
065600     SKIP3                                                                
065702* --- IMS SEKTIONER ---                                                   
065802     SKIP3                                                                
065900 IMS-GET-MSG SECTION.                                                     
066000     MOVE '  QC' TO GODK-STATUSKODER                                      
066100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
066200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
066300     PERFORM IMS-STATUSKONTROLL                                           
066400     .                                                                    
066500     SKIP3                                                                
066600 IMS-GU-W6D111 SECTION.                                                   
066700     STRING 'W6D101  (W6D101KY =' W-W6D101KY-X ')'                        
066800          DELIMITED BY SIZE INTO SSA1                                     
066810     STRING 'W6D111  (IDRADNRI =' W-IDRADNR-INL-X ')'                     
066820          DELIMITED BY SIZE INTO SSA2                                     
066900     MOVE '  ' TO GODK-STATUSKODER                                        
067000     CALL CBLTDLI USING GU W6D1-PCB DLI-IO-W6D111 SSA1 SSA2               
067100     MOVE W6D1-STATUS-CODE TO STATUS-WS                                   
067200     PERFORM IMS-STATUSKONTROLL                                           
067300     .                                                                    
067400     SKIP3                                                                
068202 IMS-GU-WDK601 SECTION.                                                   
068300     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
068400          DELIMITED BY SIZE INTO SSA1                                     
068500     MOVE '  ' TO GODK-STATUSKODER                                        
068602     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
068700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
068800     PERFORM IMS-STATUSKONTROLL                                           
068900     .                                                                    
069000     SKIP3                                                                
069102 IMS-GU-WDK611 SECTION.                                                   
069202     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
069302          DELIMITED BY SIZE INTO SSA1                                     
069402     MOVE 'WDK611  ' TO SSA2                                              
069502     MOVE '  ' TO GODK-STATUSKODER                                        
069602     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
069702     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
069802     PERFORM IMS-STATUSKONTROLL                                           
069902     .                                                                    
070002     SKIP3                                                                
070003 IMS-GU-WDK712 SECTION.                                                   
070004     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
070005          DELIMITED BY SIZE INTO SSA1                                     
070006     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
070007          DELIMITED BY SIZE INTO SSA2                                     
070008     MOVE '    ' TO GODK-STATUSKODER                                      
070009     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2               
070010     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
070020     PERFORM IMS-STATUSKONTROLL                                           
070030     .                                                                    
074902 IMS-GU-WDK711 SECTION.                                                   
075002     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
075102          DELIMITED BY SIZE INTO SSA1                                     
075202     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
075302          DELIMITED BY SIZE INTO SSA2                                     
075402     MOVE '  GE' TO GODK-STATUSKODER                                      
075502     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
075602     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
075702     PERFORM IMS-STATUSKONTROLL                                           
075802     .                                                                    
075902     EJECT                                                                
076002 IMS-GU-WDD311 SECTION.                                                   
076102     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
076202             DELIMITED BY SIZE INTO SSA1                                  
076302     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
076402              DELIMITED BY SIZE INTO SSA2                                 
076502     MOVE '  GE' TO GODK-STATUSKODER                                      
076602     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
076702     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
076802     PERFORM IMS-STATUSKONTROLL                                           
076902     .                                                                    
077002     EJECT                                                                
       IMS-GU-WDB601 SECTION.                                                   
                                                                                
           STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
                DELIMITED BY SIZE INTO SSA1                                     
           MOVE '  ' TO GODK-STATUSKODER                                        
           CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
           MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
           PERFORM IMS-STATUSKONTROLL                                           
           .                                                                    
           SKIP3                                                                
077102 IMS-STATUSKONTROLL SECTION.                                              
077202                                                                          
077302     SET STATUS-IX TO 1                                                   
077402     SEARCH GODK-STATUS                                                   
077502       AT END                                                             
077602         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
077702         DELIMITED BY SIZE INTO FELTEXT                                   
077802         CALL FELLOG                                                      
077902       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
078002         CONTINUE                                                         
079002     END-SEARCH                                                           
080000     .                                                                    
