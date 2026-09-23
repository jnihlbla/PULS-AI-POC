000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6019D00.                                                
000300 AUTHOR.         UMESH JAIN.                                              
000400 DATE-WRITTEN.   11/10/26.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        BACKGROUND MPP FOR PRINTING RECEIVING REPORT                     
000900*        ON WEB (CHINESE DC'S)                                            
001000*                                                                         
001100*        PROGRAMMET          READS      W6D1                              
001200*        PROGRAMMET          READS      WDK6                              
001300*        PROGRAMMET          READS      WDK7                              
001400*        PROGRAMMET          READS      WDD3                              
001500*        PROGRAMMET          READS      WDD8                              
001600*        PROGRAMMET          READS      WDF5                              
001700*    SUB PROGRAMMET 611ADR   READS      W6INLA (W6D1)                     
001800*                            READS      W6PLAA (W6G1)                     
001900*                            READS      W6HANA (W6G1)                     
002000*                                                                         
002100*    INDATA.                                                              
002200*        TRANSACTION: W6019DT                                             
002300*        REQUEST:     W6019DI1                                            
002400*                                                                         
002500*    OUTDATA.                                                             
002600*        RESPONSE:    W6019DO1                                            
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
003700 77  IDPGM                       PIC X(08)   VALUE 'W6019D00'.            
003800 77  YES                         PIC X       VALUE 'Y'.                   
003900 77  NOO                         PIC X       VALUE 'N'.                   
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
005300                                                                          
005400 77  INDATA-SW                   PIC X       VALUE 'Y'.                   
005500     88  INDATA-OK                           VALUE 'Y'.                   
005600     88  INDATA-FEL                          VALUE 'N'.                   
005700                                                                          
005800 77  NYCKLAR-SW                  PIC X       VALUE 'Y'.                   
005900     88  NYCKLAR-OK                          VALUE 'Y'.                   
006000     88  NYCKLAR-FEL                         VALUE 'N'.                   
006100                                                                          
006200*      --- VALID IDDC CODES                                               
006300*                                                                         
006400*01    -COPY WWDC99                                                       
006410*01    -COPY WWLNDKON                                                     
006500       EJECT                                                              
006600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006700 01  GENERELLA-SUBPROGRAM.                                                
006800     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007100     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007200     03  W611ADR                 PIC X(8)    VALUE 'W611ADR '.            
007300     03  W400ARTU                PIC X(8)    VALUE 'W400ARTU'.            
007400     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
007500     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
007600     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
007700     EJECT                                                                
007800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007900*01 -COPY WMEDAREA                                                        
008000     EJECT                                                                
008100*    --- COPYTEXT  FÖR W611ADR                                            
008200 01  FILLER                      PIC X(8)    VALUE 'W611ADR '.            
008300*01 -COPY W611ADR                                                         
008400     EJECT                                                                
008500*    --- PARAMETRAR TILL SUBPROGRAM W400ARTU                              
008600 01  FILLER                      PIC X(8)    VALUE 'W400ARTU'.            
008700*01 -COPY W400ARTU                                                        
008800     EJECT                                                                
008900* -COPY WWOMVAND                                                          
009000                                                                          
009100     EJECT                                                                
009200 01  MESSAGE-CODES.                                                       
009300     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
009400     EJECT                                                                
009500 01  KDRC-DISPLAY                PIC Z(5).                                
009600 01  FELTEXT.                                                             
009700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009900*                                                                         
010000*    --- PARAMETERS TO ABEND                                              
010100                                                                          
010200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010300 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
010400 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
010500*                                                                         
010600 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
010700     SKIP3                                                                
010800*01  -COPY WZ01SUB                                                        
010900     SKIP3                                                                
011000 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
011100*01  -COPY WZ01SEND                                                       
011200*                                                                         
011300 01  HDR-AREA.                                                            
011400*    03  -COPY WZ01REQU  -PRE HDR-                                        
011500*    03  -COPY WZ04HDR                                                    
011600     SKIP3                                                                
011700 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
011800     SKIP3                                                                
011900 01  REQU-AREA.                                                           
012000*    03  -COPY WZ01REQU                                                   
012100*    03  -COPY W6019DI1                                                   
012200     EJECT                                                                
012300 01  RESP-AREA.                                                           
012400*    03  -COPY WZ01RESP                                                   
012500     EJECT                                                                
012600 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
012700     SKIP3                                                                
012800 01  SEND-AREA.                                                           
012900*    03  -COPY W6019D1                                                    
013000     EJECT                                                                
013100*01  -COPY WMSGAREA                                                       
013200     EJECT                                                                
013300 01  FILLER                  PIC X(16)  VALUE 'WTRAUTF8-AREA   '.         
013400*01  -COPY WTRAUTF8                                                       
013500                                                                          
013600 01  WS-IDSKYLT-GB           PIC X(3) VALUE 'GB '.                        
013700 01  WS-IDSKYLT-CN           PIC X(3) VALUE 'RCN'.                        
013800                                                                          
013900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014000*                                                                         
014100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014200     SKIP3                                                                
014300 01  NYCKLAR-TILL-DLI.                                                    
014400                                                                          
014500     03  W-IDLOPNRM-X.                                                    
014600         05  W-IDLOPNRM          PIC S9(9) COMP-3 VALUE ZERO.             
014700     03  W-W6D1BSEQ-X.                                                    
014800         05  W-IDLOPNRM-BSEQ     PIC S9(9)   COMP-3 VALUE ZERO.           
014900                                                                          
015000     03  W-W6D101KY-X.                                                    
015100         05  W-D101KY-IDDC       PIC  X(2)    VALUE SPACE.                
015200         05  W-D101KY-IDLEVNR    PIC  X(5)    VALUE SPACE.                
015300         05  W-D101KY-IDFS       PIC  X(8)    VALUE SPACE.                
015400         05  W-D101KY-TIAVIDAT   PIC S9(7)    COMP-3 VALUE ZERO.          
015500                                                                          
015600     03  W-W6GXKEY-6005-X.                                                
015700         05  W-6005-IDHTYP       PIC X(4)    VALUE '6005'.                
015800         05  W-6005-IDDC         PIC X(2)    VALUE SPACE.                 
015900         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
016000                                                                          
016100     03  W-W6GXKEY-6006-X.                                                
016200         05  W-6006-ADINLOMR     PIC X(4)    VALUE SPACE.                 
016300         05  FILLER              PIC X(1)    VALUE LOW-VALUE.             
016400                                                                          
016500     03  W-IDARTNR-X.                                                     
016600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
016700                                                                          
016800     03  W-IDLEVNR-X.                                                     
016900         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
017000                                                                          
017100     03  W-IDDC-X.                                                        
017200         05  W-IDDC              PIC X(2)    VALUE '71'.                  
017300                                                                          
017400     03  W-IDSKYLT-X.                                                     
017500         05  W-IDSKYLT           PIC X(3)    VALUE 'GB '.                 
017501                                                                          
017510     03  W-KDSEGKEY-K722-X.                                               
017520         05  W-KDSEGKEY-K722     PIC X(1)    VALUE '1'.                   
017530                                                                          
017540     03  W-IDLAND-X.                                                      
017550         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
           03  W-IDDC-B6-X.                                                     
               05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
017560                                                                          
017600     SKIP2                                                                
017700*    --- STATUS-KOD FRÅN IMS                                              
017800 01  STATUS-WS                   PIC XX.                                  
017900     88  SEGMENT-FOUND                       VALUE '  '.                  
018000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
018100     88  SEGMENT-MISSING                     VALUE 'GE'.                  
018200     SKIP2                                                                
018300 01  GODK-STATUSKODER.                                                    
018400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018500     SKIP3                                                                
018600 01  SSA1                        PIC X(128).                              
018700 01  SSA2                        PIC X(64).                               
018800 01  SSA3                        PIC X(64).                               
018900     EJECT                                                                
019000*    --- IMS FUNKTIONSKODER                                               
019100*01  -COPY W0003                                                          
019200     EJECT                                                                
019300*    ---  DLI INPUT-OUTPUT AREA                                           
019400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
019500     SKIP3                                                                
019600 01  FILLER         PIC X(16) VALUE 'DLI-IO-W6D101'.                      
019700 01  DLI-IO-W6D101.                                                       
019800*    03  -COPY W6D101                                                     
019900                                                                          
020000 01  FILLER         PIC X(16) VALUE 'DLI-IO-W6D111'.                      
020100 01  DLI-IO-W6D111.                                                       
020200*    03  -COPY W6D111                                                     
020300                                                                          
020400 01  FILLER                PIC X(16) VALUE 'DLI-IO-WDD311'.               
020500 01  DLI-IO-WDD311.                                                       
020600*    03  -COPY WDD311                                                     
020700                                                                          
020800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
020900 01  DLI-IO-WDK611.                                                       
021000*    03  -COPY WDK611                                                     
021100                                                                          
021200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD811'.                      
021300 01  DLI-IO-WDD811.                                                       
021400*    03  -COPY WDD811                                                     
021500                                                                          
021600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF502'.                      
021700 01  DLI-IO-WDF502.                                                       
021800*    03  -COPY WDF502                                                     
021900                                                                          
022000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
022100 01  DLI-IO-WDK711.                                                       
022200*    03  -COPY WDK711                                                     
022300                                                                          
022301 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK712'.                      
022302 01  DLI-IO-WDK712.                                                       
022303*    03  -COPY WDK712                                                     
022304                                                                          
022310 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK722'.                      
022320 01  DLI-IO-WDK722.                                                       
022330*    03  -COPY WDK722                                                     
       01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
       01  DLI-IO-AREA-B601.                                                    
      *    03  -COPY WDB601                                                     
022340                                                                          
022400     EJECT                                                                
022500 LINKAGE SECTION.                                                         
022600                                                                          
022700*01  -COPY W0009  -PRE MSG-                                               
022800                                                                          
022900 01  DISTRWEB-PCB                PIC X.                                   
023000     EJECT                                                                
023100*01  -COPY W0008  -PRE W6D1B-                                             
023200     05  FILLER                  PIC X.                                   
023300                                                                          
023400*01  -COPY W0008  -PRE W6D1-                                              
023500     05  FILLER                  PIC X.                                   
023600                                                                          
023700*01  -COPY W0008  -PRE WDD3-                                              
023800     05  FILLER                  PIC X.                                   
023900                                                                          
024000*01  -COPY W0008  -PRE WDK6-                                              
024100     05  FILLER                  PIC X.                                   
024200                                                                          
024300*01  -COPY W0008  -PRE WDD8-                                              
024400     05  FILLER                  PIC X.                                   
024500                                                                          
024600*01  -COPY W0008  -PRE WDF5-                                              
024700     05  FILLER                  PIC X.                                   
024800                                                                          
024900*01  -COPY W0008  -PRE WDK7-                                              
025000     05  FILLER                  PIC X.                                   
025100                                                                          
      *01  -COPY W0008  -PRE WDB6-                                              
           05  FILLER                  PIC X.                                   
025200*   PCB'ER FÖR SUBPGM W611ADR                                             
025300 01  ADR-W6D1-PCB                PIC X.                                   
025400                                                                          
025500 01  ADR-INLC-PCB                PIC X.                                   
025600                                                                          
025700 01  ADR-PLAA-PCB                PIC X.                                   
025800                                                                          
025900 01  ADR-WDK6-PCB                PIC X.                                   
026000                                                                          
026100 01  ADR-STYR-HANA-PCB           PIC X.                                   
026200                                                                          
026300 01  ADR-STYR-PLAA-PCB           PIC X.                                   
026400                                                                          
026500     EJECT                                                                
026600 PROCEDURE DIVISION  USING MSG-PCB DISTRWEB-PCB                           
026700                           W6D1B-PCB W6D1-PCB WDD3-PCB                    
026800                           WDK6-PCB WDD8-PCB WDF5-PCB WDK7-PCB            
                                 WDB6-PCB                                       
026900              ADR-W6D1-PCB ADR-INLC-PCB      ADR-PLAA-PCB                 
027000              ADR-WDK6-PCB ADR-STYR-HANA-PCB ADR-STYR-PLAA-PCB.           
027100                                                                          
027200 MAIN SECTION.                                                            
027300     ENTRY 'DLITCBL' USING MSG-PCB DISTRWEB-PCB                           
027400                           W6D1B-PCB W6D1-PCB WDD3-PCB                    
027500                           WDK6-PCB WDD8-PCB WDF5-PCB WDK7-PCB            
                   WDB6-PCB                                                     
027600             ADR-W6D1-PCB ADR-INLC-PCB      ADR-PLAA-PCB                  
027700             ADR-WDK6-PCB ADR-STYR-HANA-PCB ADR-STYR-PLAA-PCB.            
027800                                                                          
027900     PERFORM S11-FETCH-REQUEST-ARGUMENT                                   
028000     IF SUB-KDRC = 0                                                      
028100       PERFORM A-INIT                                                     
028200       IF REQU-IDLOPNRM IS NUMERIC                                        
028300*        WHEN CALLED FROM 6109, 6123 WEB                                  
028400         PERFORM B-PROCESS-REQU-6109                                      
028500       ELSE                                                               
028600*        WHEN CALLED FROM 6111, 6115 WEB                                  
028700         PERFORM C-PROCESS-REQU-6115                                      
028800       END-IF                                                             
028900       IF INDATA-FEL                                                      
029000         PERFORM S12-RETURN-RESPONSE                                      
029100       END-IF                                                             
029200     END-IF                                                               
029300                                                                          
029400     MOVE ZERO TO RETURN-CODE                                             
029500     GOBACK                                                               
029600     .                                                                    
029700     EJECT                                                                
029800 A-INIT SECTION.                                                          
029900     MOVE YES               TO INDATA-SW                                  
030000                               NYCKLAR-SW                                 
030100     INITIALIZE RR-W6019D1                                                
030200     MOVE REQU-IDDC         TO WS-IDDC                                    
030300                               W-IDDC                                     
                                     W-IDDC-B6                                  
030400     MOVE SPACE             TO RESP-AREA                                  
030500     MOVE 001               TO RESP-IDMSGVER                              
           PERFORM IMS-GU-WDB601                                                
030600     .                                                                    
030700     EJECT                                                                
030800 B-PROCESS-REQU-6109 SECTION.                                             
030900     MOVE REQU-IDLOPNRM     TO W-IDLOPNRM-BSEQ                            
031000     PERFORM IMS-GU-W6D111-BSEQ                                           
031100     IF SEGMENT-FOUND                                                     
031200       PERFORM S90-OPEN-DAP-SEND-WEB                                      
031300       PERFORM S90-PUT-DAP-HEADER                                         
031400       PERFORM S01-PROCESS-DETAILS                                        
031500       PERFORM IMS-GNP-W6D101-BSEQ                                        
031600       MOVE INL-IDFS        TO RR-IDFS                                    
031700       MOVE INL-TIAVIDAT    TO RR-TIAVIDAT                                
031800       MOVE INL-IDLBBET     TO RR-IDLBBET                                 
031900       MOVE INL-IDLEVNR     TO RR-IDLEVNR                                 
032000       PERFORM S90-PUT-LINE                                               
032100       PERFORM S90-CLOSE-DAP-SEND                                         
032200     ELSE                                                                 
032300       MOVE NOO TO INDATA-SW                                              
032400       MOVE '025'      TO RESP-IDMSG-ERROR                                
032500       MOVE 'IDLOPNRM' TO RESP-IDELMT-ERROR                               
032600     END-IF                                                               
032700     .                                                                    
032800     EJECT                                                                
032900                                                                          
033000 C-PROCESS-REQU-6115 SECTION.                                             
033100     MOVE REQU-IDDC         TO W-D101KY-IDDC                              
033200     MOVE REQU-IDLEVNR      TO W-D101KY-IDLEVNR                           
033300                               W-IDLEVNR                                  
033400     MOVE REQU-IDFS         TO W-D101KY-IDFS                              
033500     IF REQU-TIAVIDAT IS NUMERIC                                          
033600       MOVE REQU-TIAVIDAT   TO W-D101KY-TIAVIDAT                          
033700     END-IF                                                               
033800*                                                                         
033900     PERFORM IMS-GU-W6D101                                                
034000     IF SEGMENT-FOUND                                                     
034100       MOVE INL-IDFS        TO RR-IDFS                                    
034200       MOVE INL-TIAVIDAT    TO RR-TIAVIDAT                                
034300       MOVE INL-IDLBBET     TO RR-IDLBBET                                 
034400       MOVE INL-IDLEVNR     TO RR-IDLEVNR                                 
034500       PERFORM S90-OPEN-DAP-SEND-WEB                                      
034600       PERFORM S90-PUT-DAP-HEADER                                         
034700                                                                          
034800       PERFORM IMS-GNP-W6D111                                             
034900       PERFORM UNTIL SEGMENT-MISSING                                      
035000         PERFORM S01-PROCESS-DETAILS                                      
035100         PERFORM S90-PUT-LINE                                             
035200         PERFORM IMS-GNP-W6D111                                           
035300       END-PERFORM                                                        
035400       PERFORM S90-CLOSE-DAP-SEND                                         
035500     ELSE                                                                 
035600       MOVE NOO TO INDATA-SW                                              
035700       MOVE '025'      TO RESP-IDMSG-ERROR                                
035800       MOVE 'KEY'      TO RESP-IDELMT-ERROR                               
035900     END-IF                                                               
036000     .                                                                    
036100     EJECT                                                                
036200 S01-PROCESS-DETAILS     SECTION.                                         
036300     MOVE ART-IDARTNR      TO RR-IDARTNR                                  
036400                              W-IDARTNR                                   
036500     MOVE ART-IDLOPNRM      TO RR-IDLOPNRM                                
036600                               RR-IDLOPNRM-1                              
036700*                                                                         
036800     MOVE ART-KVAVIS        TO RR-KVAVIS                                  
036900     MOVE SPACES            TO RR-KVANTMOT                                
037000*                                                                         
037100     IF ART-KDFARLIG = 4 OR 6                                             
037200       MOVE YES             TO RR-KDFARLIG                                
037300     ELSE                                                                 
037400       MOVE SPACES          TO RR-KDFARLIG                                
037500     END-IF                                                               
037600*                                                                         
037700     MOVE ART-KDSORT        TO RR-KDSORT                                  
037800     MOVE ART-BEFT          TO RR-BEFT                                    
037900     MOVE ART-KVAVIS-PRIO   TO RR-KVAVIS-PRIO                             
038000     MOVE ART-ADLAGOMR      TO RR-ADLAGOMR                                
038100     MOVE ART-ADGANG        TO RR-ADGANG                                  
038200     MOVE ART-ADPLATS       TO RR-ADPLATS                                 
038300*                                                                         
038400     IF ART-KDKVAANT > ZERO                                               
038500      MOVE YES              TO RR-KDKVAANT                                
038600     ELSE                                                                 
038700      MOVE SPACES           TO RR-KDKVAANT                                
038800     END-IF                                                               
038900*                                                                         
039000     MOVE ART-KVKVAPRIM-BER TO RR-KVKVAPRIM-BER                           
039100     MOVE ART-VKART         TO RR-VKART                                   
039200     MOVE ART-VLARTNTO      TO RR-VLARTNTO                                
039300*                                                                         
039400     PERFORM S01D-GET-NEXT-ADR                                            
039500*                                                                         
039600     PERFORM IMS-GU-WDK611                                                
039700     IF SEGMENT-FOUND                                                     
039800       MOVE CLAG-IDANSK    TO RR-IDANSK                                   
039900       MOVE CLAG-KVQPACK-3 TO RR-KVQPACK-3                                
040000     END-IF                                                               
040100*                                                                         
040101     IF NDC-CN OR NDC-US                                                  
040102        IF NDC-US                                                         
040103           MOVE WC-LAND-US             TO W-IDLAND                        
040104        ELSE                                                              
040105           MOVE WC-LAND-CN             TO W-IDLAND                        
040106        END-IF                                                            
040107                                                                          
040110        PERFORM IMS-GU-WDK712                                             
040120        IF SEGMENT-FOUND                                                  
040121           IF LART-KVQPACK-3 > 0                                          
040122              MOVE LART-KVQPACK-3 TO RR-KVQPACK-3                         
040123           END-IF                                                         
040150        END-IF                                                            
040151     END-IF                                                               
040160*                                                                         
040200     PERFORM IMS-GU-WDK711                                                
040300     IF SEGMENT-FOUND                                                     
040400       COMPUTE WS-KVROS = SLAG-KVROS-DAG + SLAG-KVROS-BULK                
040500       MOVE WS-KVROS        TO RR-KVROS                                   
040600     END-IF                                                               
040700*                                                                         
040701     PERFORM IMS-GU-WDK722                                                
040702     IF SEGMENT-FOUND AND XLAG-IDANSK > 0                                 
040703       MOVE XLAG-IDANSK     TO RR-IDANSK                                  
040704     END-IF                                                               
040710*                                                                         
040800     PERFORM S01A-CALL-W400ARTU                                           
040900     MOVE ARTU-BEARTURS-ENG TO  RR-BEARTURS                               
041000*                                                                         
041100     PERFORM IMS-GU-WDF5-WDF502                                           
041200     IF SEGMENT-FOUND                                                     
041300       MOVE XLEV-BELEVART   TO RR-BELEV                                   
041400     ELSE                                                                 
041500       MOVE SPACE           TO RR-BELEV                                   
041600     END-IF                                                               
041700*                                                                         
041800     PERFORM S01B-GET-BUFFER-ADR                                          
041900     PERFORM S01C-GET-BEART-CHINESE                                       
042000     PERFORM S01D-GET-NEXT-ADR                                            
042200     .                                                                    
042300     EJECT                                                                
042400 S01A-CALL-W400ARTU       SECTION.                                        
042500* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
042600* ÖVERSÄTTER EN ARTIKELS URSPRUNGSKOD TILL KLARTEXT             *         
042700* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
042800     MOVE ART-KDARTURS   TO ARTU-KDARTURS                                 
042900     MOVE ZERO           TO ARTU-IDDISTR                                  
043000     MOVE WS-IDDC        TO ARTU-IDDC                                     
043100                                                                          
043200     CALL W400ARTU USING ARTU-W400ARTU                                    
043300     .                                                                    
043400     EJECT                                                                
043500 S01B-GET-BUFFER-ADR    SECTION.                                          
043600                                                                          
043700     MOVE ZERO             TO RR-ADBUFFGANG(1)                            
043800                              RR-ADBUFFOMR(1)                             
043900                              RR-ADBUFFPL(1)                              
044000                              RR-ADBUFFGANG(2)                            
044100                              RR-ADBUFFOMR(2)                             
044200                              RR-ADBUFFPL(2)                              
044300                              RR-ADBUFFGANG(3)                            
044400                              RR-ADBUFFOMR(3)                             
044500                              RR-ADBUFFPL(3)                              
044600                                                                          
044700     MOVE 1                TO ANT-ADBUFF                                  
044800     PERFORM IMS-GU-WDD801                                                
044900     IF SEGMENT-FOUND                                                     
045000       PERFORM UNTIL SEGMENT-MISSING OR ANT-ADBUFF = 4                    
045100         MOVE SALDO-ADBUFFOMR  TO W-ADBUFFOMR                             
045200         IF W-ADBUFFOMR        =  W-ADLAGOMR                              
045300           PERFORM IMS-GNP-WDD811                                         
045400         ELSE                                                             
045500           EVALUATE ANT-ADBUFF                                            
045600             WHEN 1                                                       
045700              MOVE SALDO-ADBUFFOMR  TO RR-ADBUFFOMR(1)                    
045800              MOVE SALDO-ADBUFFGANG TO RR-ADBUFFGANG(1)                   
045900              MOVE SALDO-ADBUFFPL   TO RR-ADBUFFPL(1)                     
046000             WHEN 2                                                       
046100              MOVE SALDO-ADBUFFOMR  TO RR-ADBUFFOMR(2)                    
046200              MOVE SALDO-ADBUFFGANG TO RR-ADBUFFGANG(2)                   
046300              MOVE SALDO-ADBUFFPL   TO RR-ADBUFFPL(2)                     
046400             WHEN 3                                                       
046500              MOVE SALDO-ADBUFFOMR  TO RR-ADBUFFOMR(3)                    
046600              MOVE SALDO-ADBUFFGANG TO RR-ADBUFFGANG(3)                   
046700              MOVE SALDO-ADBUFFPL   TO RR-ADBUFFPL(3)                     
046800           END-EVALUATE                                                   
046900           PERFORM IMS-GNP-WDD811                                         
047000           ADD +1                TO ANT-ADBUFF                            
047100         END-IF                                                           
047200       END-PERFORM                                                        
047300*                                                                         
047400       IF ANT-ADBUFF         = 1                                          
047500         PERFORM IMS-GNP-WDD811-FIRST                                     
047600         IF SEGMENT-FOUND                                                 
047700           MOVE SALDO-ADBUFFOMR  TO RR-ADBUFFOMR(1)                       
047800           MOVE SALDO-ADBUFFGANG TO RR-ADBUFFGANG(1)                      
047900           MOVE SALDO-ADBUFFPL   TO RR-ADBUFFPL(1)                        
048000         END-IF                                                           
048100       END-IF                                                             
048200     END-IF                                                               
048300     .                                                                    
048400     EJECT                                                                
048500* --- IMS SEKTIONER ---                                                   
048600     SKIP3                                                                
048700 S01C-GET-BEART-CHINESE.                                                  
048800*    -- READ CHINESE OR ENGLISH BEART                                     
048900*    -- ENGLISH WILL BE TRANSLATED TO UNICODE                             
           MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                              
           IF DCS-UNICODE-IDSKYLT                                               
              MOVE 'UTF8'             TO TRAUTF8-KDCP                           
           ELSE                                                                 
              MOVE '278 '             TO TRAUTF8-KDCP                           
           END-IF                                                               
049700                                                                          
049800     PERFORM IMS-GU-WDD311                                                
049900     IF SEGMENT-FOUND                                                     
050000       MOVE TEXT-BEART    TO TRAUTF8-TECONV-FROM                          
050100     ELSE                                                                 
050200       MOVE SPACE         TO TRAUTF8-TECONV-FROM                          
050300       MOVE '278'         TO TRAUTF8-KDCP                                 
050400     END-IF                                                               
           IF TRAUTF8-TECONV-FROM = SPACES                                      
            MOVE 'GB'  TO W-IDSKYLT                                             
            MOVE '278' TO TRAUTF8-KDCP                                          
            PERFORM IMS-GU-WDD311                                               
            MOVE TEXT-BEART    TO TRAUTF8-TECONV-FROM                           
           END-IF                                                               
                                                                                
050500                                                                          
050600*    -- STRIP SPACE OR CONVERT TO UNICODE                                 
050700     CALL WTRAUTF8 USING TRAUTF8-AREA                                     
050800                                                                          
050900     MOVE TRAUTF8-TECONV-TO   TO RR-BEART                                 
051000     .                                                                    
051100     EJECT                                                                
051200 S01D-GET-NEXT-ADR       SECTION.                                         
051300     IF ART-IDLOPNRM           >  ZERO                                    
051400       MOVE ART-IDLOPNRM     TO ADR-IDLOPNRM                              
051500                                                                          
051600       CALL W611ADR USING ADR-W611ADR  ADR-W6D1-PCB                       
051700                          ADR-INLC-PCB ADR-PLAA-PCB                       
051800                          ADR-WDK6-PCB ADR-STYR-HANA-PCB                  
051900                          ADR-STYR-PLAA-PCB                               
052000                                                                          
052100       MOVE ADR-ADINLOMR-NXT1 TO RR-ADINLOMR-NXT1                         
052200     END-IF                                                               
052300     .                                                                    
052400     EJECT                                                                
052500*                                                                         
052600 S11-FETCH-REQUEST-ARGUMENT SECTION.                                      
052700     MOVE 'GETARG'               TO SUB-KDFUNC                            
052800     MOVE 'CARPARTS.NDC.CREATERECEIVINGREPORT'                            
052900                                 TO SUB-ADDISPABS                         
053000     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
053100                                                                          
053200     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
053300                                                                          
053400     IF SUB-KDRC > 0                                                      
053500       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
053600       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
053700       DELIMITED BY SIZE INTO FELTEXT                                     
053800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
053900     END-IF                                                               
054000     .                                                                    
054100     SKIP3                                                                
054200 S12-RETURN-RESPONSE SECTION.                                             
054300     MOVE 'RETURN'                   TO SUB-KDFUNC                        
054400     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
054500                                                                          
054600     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
054700                                                                          
054800     IF SUB-KDRC > 0                                                      
054900       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
055000       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
055100       DELIMITED BY SIZE INTO FELTEXT                                     
055200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
055300     END-IF                                                               
055400     .                                                                    
055500     EJECT                                                                
055600 S90-OPEN-DAP-SEND-WEB SECTION.                                           
055700     MOVE 'OPEN'                  TO SEND-KDFUNC                          
055800*    -- WEB RESPONSE SHOULD HAVE LOWER PRIO TO FINISH LAST                
055900*    -- DISTRDOC = WZ0420X HAS LOWER PRIO THAN WZ0420U                    
056000     MOVE 'CARPARTS.DAP.DISTRDOC' TO SEND-ADDISPABS                       
056100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
056200                         SEND-OPEN-AREA                                   
056300     IF SEND-KDRC > ZERO                                                  
056400       MOVE SEND-KDRC             TO KDRC-DISPLAY                         
056500       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
056600       DELIMITED BY SIZE INTO FELTEXT                                     
056700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
056800     END-IF                                                               
056900     .                                                                    
057000     SKIP3                                                                
057100 S90-PUT-DAP-HEADER SECTION.                                              
057200     MOVE 1                       TO HDR-REQU-IDMSGVER                    
057300     MOVE REQU-KDPGMACT           TO HDR-REQU-KDPGMACT                    
057400     MOVE REQU-IDUSER             TO HDR-REQU-IDUSER                      
057500     MOVE 'W6019D-001'            TO HDR-IDOUTTYPE                        
057600     MOVE REQU-IDDC               TO HDR-IDOUTREC (1:2)                   
057700     MOVE REQU-IDUSER             TO HDR-IDOUTREC (3:)                    
057800     MOVE SPACE                   TO HDR-IDLIST                           
057900     MOVE 'PUT'                   TO SEND-KDFUNC                          
058000     MOVE LENGTH OF HDR-AREA      TO SEND-KVDLEN                          
058100     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
058200                                     SEND-KVDLEN                          
058300                                     HDR-AREA                             
058400     IF SEND-KDRC > ZERO                                                  
058500       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
058600       STRING 'WZ01SEND GET  ERROR RC= ' KDRC-DISPLAY                     
058700       DELIMITED BY SIZE       INTO FELTEXT-STR                           
058800       DISPLAY FELTEXT                                                    
058900       CALL FELLOG                                                        
059000     END-IF                                                               
059100     .                                                                    
059200     EJECT                                                                
059300 S90-PUT-LINE SECTION.                                                    
059400     MOVE 'PUT'                   TO SEND-KDFUNC                          
059500     MOVE LENGTH OF RR-W6019D1    TO SEND-KVDLEN                          
059600     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
059700                                     SEND-KVDLEN                          
059800                                     RR-W6019D1                           
059900     IF SEND-KDRC > ZERO                                                  
060000       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
060100       STRING 'WZ01SEND GET  ERROR RC= ' KDRC-DISPLAY                     
060200       DELIMITED BY SIZE       INTO FELTEXT-STR                           
060300       DISPLAY FELTEXT                                                    
060400       CALL FELLOG                                                        
060500     END-IF                                                               
060600     .                                                                    
060700     SKIP2                                                                
060800 S90-CLOSE-DAP-SEND SECTION.                                              
060900     MOVE 'CLOSE'                 TO SEND-KDFUNC                          
061000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
061100                                                                          
061200     IF SEND-KDRC > 0                                                     
061300       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
061400       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
061500       DELIMITED BY SIZE INTO FELTEXT                                     
061600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
061700     END-IF                                                               
061800     .                                                                    
061900     SKIP3                                                                
062000 IMS-GET-MSG SECTION.                                                     
062100     MOVE '  QC' TO GODK-STATUSKODER                                      
062200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
062300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
062400     PERFORM IMS-STATUSKONTROLL                                           
062500     .                                                                    
062600     SKIP3                                                                
062700 IMS-GU-W6D101 SECTION.                                                   
062800     STRING 'W6D101  (W6D101KY =' W-W6D101KY-X ')'                        
062900          DELIMITED BY SIZE INTO SSA1                                     
063000     MOVE '  GE' TO GODK-STATUSKODER                                      
063100     CALL CBLTDLI USING GU W6D1-PCB DLI-IO-W6D101 SSA1                    
063200     MOVE W6D1-STATUS-CODE TO STATUS-WS                                   
063300     PERFORM IMS-STATUSKONTROLL                                           
063400     .                                                                    
063500     SKIP3                                                                
063600 IMS-GNP-W6D111 SECTION.                                                  
063700     MOVE '  GE' TO GODK-STATUSKODER                                      
063800     CALL CBLTDLI USING GNP W6D1-PCB DLI-IO-W6D111                        
063900     MOVE W6D1-STATUS-CODE TO STATUS-WS                                   
064000     PERFORM IMS-STATUSKONTROLL                                           
064100     .                                                                    
064200     SKIP3                                                                
064300 IMS-GU-W6D111-BSEQ SECTION.                                              
064400     STRING 'W6D111  (W6D1BSEQ =' W-W6D1BSEQ-X                            
064500                    '&IDDC     =' W-IDDC-X ')'                            
064600          DELIMITED BY SIZE INTO SSA1                                     
064700     MOVE '  GEGB' TO GODK-STATUSKODER                                    
064800     CALL CBLTDLI USING GU W6D1B-PCB DLI-IO-W6D111 SSA1                   
064900     MOVE W6D1B-STATUS-CODE TO STATUS-WS                                  
065000     PERFORM IMS-STATUSKONTROLL                                           
065100     .                                                                    
065200     SKIP3                                                                
065300 IMS-GNP-W6D101-BSEQ SECTION.                                             
065400     MOVE '  ' TO GODK-STATUSKODER                                        
065500     CALL CBLTDLI USING GNP W6D1B-PCB DLI-IO-W6D101                       
065600     MOVE W6D1B-STATUS-CODE TO STATUS-WS                                  
065700     PERFORM IMS-STATUSKONTROLL                                           
065800     .                                                                    
065900     SKIP3                                                                
066000 IMS-GU-WDK611 SECTION.                                                   
066100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
066200          DELIMITED BY SIZE INTO SSA1                                     
066300     MOVE 'WDK611  ' TO SSA2                                              
066400     MOVE '  ' TO GODK-STATUSKODER                                        
066500     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
066600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
066700     PERFORM IMS-STATUSKONTROLL                                           
066800     .                                                                    
066900     SKIP3                                                                
067000 IMS-GU-WDD801 SECTION.                                                   
067100     STRING 'WDD801  *P(IDARTNR  =' W-IDARTNR-X ')'                       
067200          DELIMITED BY SIZE INTO SSA1                                     
067300     STRING 'WDD811  (IDDC     =' W-IDDC-X ')'                            
067400          DELIMITED BY SIZE INTO SSA2                                     
067500     MOVE '  GE' TO GODK-STATUSKODER                                      
067600     CALL CBLTDLI USING GU WDD8-PCB DLI-IO-WDD811 SSA1 SSA2               
067700     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
067800     PERFORM IMS-STATUSKONTROLL                                           
067900     .                                                                    
068000     SKIP3                                                                
068100 IMS-GNP-WDD811 SECTION.                                                  
068200     STRING 'WDD811  (IDDC     =' W-IDDC-X ')'                            
068300          DELIMITED BY SIZE INTO SSA1                                     
068400     MOVE '  GE' TO GODK-STATUSKODER                                      
068500     CALL CBLTDLI USING GNP WDD8-PCB DLI-IO-WDD811 SSA1                   
068600     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
068700     PERFORM IMS-STATUSKONTROLL                                           
068800     .                                                                    
068900     SKIP3                                                                
069000 IMS-GNP-WDD811-FIRST SECTION.                                            
069100     STRING 'WDD811  *F(IDDC     =' W-IDDC-X ')'                          
069200          DELIMITED BY SIZE INTO SSA1                                     
069300     MOVE '  GE' TO GODK-STATUSKODER                                      
069400     CALL CBLTDLI USING GNP WDD8-PCB DLI-IO-WDD811 SSA1                   
069500     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
069600     PERFORM IMS-STATUSKONTROLL                                           
069700     .                                                                    
069800     EJECT                                                                
069900 IMS-GU-WDF5-WDF502 SECTION.                                              
070000     STRING 'WDF501  *P(IDARTNR  =' W-IDARTNR-X ')'                       
070100          DELIMITED BY SIZE INTO SSA1                                     
070200     STRING 'WDF502  (IDLEVNR  =' W-IDLEVNR-X ')'                         
070300          DELIMITED BY SIZE INTO SSA2                                     
070400     MOVE '  GE' TO GODK-STATUSKODER                                      
070500     CALL CBLTDLI USING GU WDF5-PCB DLI-IO-WDF502 SSA1 SSA2               
070600     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
070700     PERFORM IMS-STATUSKONTROLL                                           
070800     .                                                                    
070900     EJECT                                                                
071000 IMS-GNP-WDF502 SECTION.                                                  
071100     STRING 'WDF502  (IDLEVNR  =' W-IDLEVNR-X ')'                         
071200          DELIMITED BY SIZE INTO SSA1                                     
071300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
071400     CALL CBLTDLI USING GNP WDF5-PCB DLI-IO-WDF502 SSA1                   
071500     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
071600     PERFORM IMS-STATUSKONTROLL                                           
071700     .                                                                    
071800     EJECT                                                                
071900 IMS-GU-WDK711 SECTION.                                                   
072000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
072100          DELIMITED BY SIZE INTO SSA1                                     
072200     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
072300          DELIMITED BY SIZE INTO SSA2                                     
072400     MOVE '  GE' TO GODK-STATUSKODER                                      
072500     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
072600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
072700     PERFORM IMS-STATUSKONTROLL                                           
072800     .                                                                    
072900     EJECT                                                                
072901 IMS-GU-WDK712 SECTION.                                                   
072902     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
072903          DELIMITED BY SIZE INTO SSA1                                     
072904     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
072905          DELIMITED BY SIZE INTO SSA2                                     
072906     MOVE 'GE  ' TO GODK-STATUSKODER                                      
072907     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2               
072908     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
072909     PERFORM IMS-STATUSKONTROLL                                           
072910     .                                                                    
072911     EJECT                                                                
072920 IMS-GU-WDK722 SECTION.                                                   
072930     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
072940          DELIMITED BY SIZE INTO SSA1                                     
072950     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
072960          DELIMITED BY SIZE INTO SSA2                                     
072970     STRING 'WDK722  (KDSEGKEY =' W-KDSEGKEY-K722-X ')'                   
072980          DELIMITED BY SIZE INTO SSA3                                     
072990     MOVE '  GE'              TO GODK-STATUSKODER                         
072991     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3          
072993     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
072994     PERFORM IMS-STATUSKONTROLL                                           
072995     .                                                                    
072996     EJECT                                                                
073000 IMS-GU-WDD311 SECTION.                                                   
073100     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
073200             DELIMITED BY SIZE INTO SSA1                                  
073300     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
073400              DELIMITED BY SIZE INTO SSA2                                 
073500     MOVE '  GE' TO GODK-STATUSKODER                                      
073600     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
073700     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
073800     PERFORM IMS-STATUSKONTROLL                                           
073900     .                                                                    
074000     EJECT                                                                
       IMS-GU-WDB601 SECTION.                                                   
                                                                                
           STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
                DELIMITED BY SIZE INTO SSA1                                     
           MOVE '  ' TO GODK-STATUSKODER                                        
           CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
           MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
           PERFORM IMS-STATUSKONTROLL                                           
           .                                                                    
           SKIP3                                                                
074100 IMS-STATUSKONTROLL SECTION.                                              
074200                                                                          
074300     SET STATUS-IX TO 1                                                   
074400     SEARCH GODK-STATUS                                                   
074500       AT END                                                             
074600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
074700         DELIMITED BY SIZE INTO FELTEXT                                   
074800         CALL FELLOG                                                      
074900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
075000         CONTINUE                                                         
075100     END-SEARCH                                                           
075200     .                                                                    
