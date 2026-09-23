000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL011000.                                                
000300 AUTHOR.         SUBBARAO PARUCHURI V.                                    
000400 DATE-WRITTEN.   04/10/15.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       'CARPARTS.LDC.REPLACELABEL'                              
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        PROGRAMMET UTFÖR UTSKRIFT AV NYA FLAGGOR FÖR EGET                
001100*        C-LAGER EFTER PÅFYLLNING.                                        
001200*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001300*                   LÄSER      W6D1                                       
001400*                   LÄSER      W6PLAA (W6G1)                              
001500*                   UPPDATERAR W6G1                                       
001600*                                                                         
001700*        WL011000 PROGRAM IS A REPLICA OF W6017300 PROGRAM                
001800*        AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS                          
001900*                                                                         
002000*    INDATA.                                                              
002100*        TRANSACTION: WL0110U                                             
002200*        REQUEST:     WL0110I1                                            
002300*                                                                         
002400*    OUTDATA.                                                             
002500*        RESPONSE:    WL0110O1                                            
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP3                                                                
003600 FILE SECTION.                                                            
003700     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900 77  IDPGM                       PIC X(08)   VALUE 'WL011000'.            
004000                                                                          
004100*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004200 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004300 77  KDRC-DISPLAY                PIC Z(5).                                
004400                                                                          
004500*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004600 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004700                                                                          
004800 77  JA                          PIC X       VALUE 'J'.                   
004900 77  NEJ                         PIC X       VALUE 'N'.                   
005000                                                                          
005100 77  IX                          PIC S9(9)   VALUE +0 COMP SYNC.          
005200 77  6194-IX                     PIC S9(9)   VALUE +0 COMP SYNC.          
005300 77  LNG-P-TO-P-PREFIX           PIC S9(4)  VALUE +17 COMP SYNC.          
005400                                                                          
005500*    --- ARBETSFÄLT FÖR AKTUELLA INDATAVÄRDEN FRÅN SKÄRMEN                
005600 77  WS-KVINLART                 PIC 9(6)    VALUE ZERO.                  
005700 77  WS-KVINLART-LAST            PIC 9(6)    VALUE ZERO.                  
005800 77  WS-TIAAMMDD                 PIC 9(6)    VALUE ZERO COMP-3.           
005900 77  WS-TIINLMOT                 PIC 9(6)    VALUE ZERO COMP-3.           
006000 77  WS-KVFLETI                  PIC 9(2)    VALUE ZERO COMP-3.           
006100*    --- ÖVRIGA ARBETSFÄLT                                                
006200 77  WS-ADLAGOMR                 PIC 9(2)    VALUE ZERO.                  
006300 77  WS-ADGANG                   PIC 9(2)    VALUE ZERO.                  
006400 77  WS-ADPLATS                  PIC 9(5)    VALUE ZERO.                  
006500 77  WS-KDSORT                   PIC X(2)    VALUE SPACE.                 
006600 77  WS-VKKOLLIN                 PIC 9(5)V9(1)  VALUE ZERO.               
006700 77  WS-FL-WDK712                PIC X       VALUE 'N'.                   
006800 77  WS-IDLEVNR                  PIC X(5)    VALUE SPACE.                 
006900 77  WS-IDOKOLLI                 PIC 9(9)    VALUE ZERO.                  
007000 77  WS-IDPRTLST                 PIC X(8)    VALUE SPACE.                 
007010 77  FL-LOPA-UPPDATERAD          PIC X       VALUE 'N'.                   
007100                                                                          
007200* -COPY WY2000W1                                                          
007300                                                                          
007400* -COPY WWOMVAND                                                          
007500                                                                          
007600* -COPY WWDC99                                                            
007700* -COPY WWLNDKON                                                          
007800                                                                          
007900 77  INDATA-SW                   PIC X       VALUE 'J'.                   
008000     88  INDATA-OK                           VALUE 'J'.                   
008100     88  INDATA-FEL                          VALUE 'N'.                   
008200                                                                          
008300 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
008400     88  NYCKLAR-OK                          VALUE 'J'.                   
008500     88  NYCKLAR-FEL                         VALUE 'N'.                   
008600                                                                          
008700 77  ARTIKEL-SW                  PIC X       VALUE 'N'.                   
008800     88  ARTIKEL-SAKNAS                      VALUE 'J'.                   
008900                                                                          
009000 77  ALLT-SW                     PIC X       VALUE 'J'.                   
009100     88  ALLT-OK                             VALUE 'J'.                   
009200                                                                          
009300 77  KOLLINR-SW                  PIC X       VALUE 'N'.                   
009400     88  KOLLINR-FINNS                       VALUE 'J'.                   
009500     88  KOLLINR-SAKNAS                      VALUE 'N'.                   
009600                                                                          
009700 77  EGET-LEVNR-SW               PIC X       VALUE 'N'.                   
009800     88  EGET-LEVNR                          VALUE 'J'.                   
009900                                                                          
010000 77  MID-INPUT-SW                PIC X       VALUE 'N'.                   
010100     88  MID-INPUT-SAKNAS                    VALUE 'J'.                   
010200                                                                          
010300 77  6194-SW                     PIC X       VALUE 'J'.                   
010400     88  FOERSTA-6194                        VALUE 'J'.                   
010500                                                                          
010600 77  WS-IDELMT-ERROR             PIC X(16).                               
010700 77  WS-IDMSG-ERROR              PIC X(03).                               
010800 77  WS-IDMSG-INFO               PIC X(03).                               
010900                                                                          
011000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
011100     88  EGEN-MID                            VALUE '6173'.                
011200     88  GODK-MID                            VALUE '6173'.                
011300     88  HELP-MID                            VALUE '0551'.                
011400     EJECT                                                                
011500*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
011600 01  GENERAL-SUBPROGRAMS.                                                 
011700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011800     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
011900     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
012000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
012100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012200     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
012300     SKIP3                                                                
012400*    --- PARAMETERS TO ABEND                                              
012500                                                                          
012600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
012700 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
012800 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
012900     EJECT                                                                
013000*                                                                         
013100 01  FILLER                      PIC X(16)   VALUE 'W006PRT'.             
013200     SKIP2                                                                
013300*01  -COPY W006PRT                                                        
013400 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
013500     SKIP3                                                                
013600 01  FILLER                      PIC X(24)   VALUE                        
013700                                 'MOD6194-MID-W6I19401'.                  
013800*01  -COPY W6I19401 -PRE MOD6194-                                         
013900     SKIP3                                                                
014000*01  -COPY WZ01SUB                                                        
014100     EJECT                                                                
014200 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
014300     SKIP3                                                                
014400 01  REQU-AREA.                                                           
014500*    03  -COPY WZ01REQU                                                   
014600*    03  -COPY WL0110I1                                                   
014700     EJECT                                                                
014800 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
014900     SKIP3                                                                
015000 01  RESP-AREA.                                                           
015100*    03  -COPY WZ01RESP                                                   
015200*    03  -COPY WL0110O1                                                   
015300     EJECT                                                                
015400 01      FILLER                  PIC X(16)   VALUE 'P-TO-P-SW'.           
015500     SKIP3                                                                
015600 01      P-TO-P-SW.                                                       
015700     03  P-TO-P-KVLL             PIC S9(4)   COMP SYNC.                   
015800     03  P-TO-P-KDZ1             PIC X(1)    VALUE LOW-VALUE.             
015900     03  P-TO-P-KDZ2             PIC X(1)    VALUE LOW-VALUE.             
016000     03  P-TO-P-KDTRANS          PIC X(8).                                
016100     03  P-TO-P-IDTRANS          PIC X(4).                                
016200     03  P-TO-P-KDMFSFOR         PIC X(1).                                
016300     03  P-TO-P-DATA             PIC X(4079).                             
016400     EJECT                                                                
016500*    --- SUBPROGRAM WDATKONV                                              
016600*01 -COPY WDATAREA                                                        
016700     EJECT                                                                
016800 01  MESSAGE-CODES.                                                       
016900     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
017000     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '014'.                 
017100     03  ERR-ARTIKEL-SAKNAS      PIC X(3)    VALUE '017'.                 
017200     03  INF-UPDATE-DONE         PIC X(3)    VALUE '001'.                 
017300     03  ERR-CASE-NOT-STORED     PIC X(3)    VALUE '105'.                 
017400     03  ERR-WRONG-CASE-OR-SUPP  PIC X(3)    VALUE '261'.                 
017500     03  SYS-ERROR               PIC X(3)    VALUE '099'.                 
017600     03  TOO-MANY-LINES          PIC X(3)    VALUE '275'.                 
017700     03  ERR-INVALID-PRINTER     PIC X(3)    VALUE '347'.                 
017800     03  INF-PRINT-REQUESTED     PIC X(3)    VALUE '376'.                 
017900     EJECT                                                                
018000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
018100*                                                                         
018200     SKIP3                                                                
018300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
018400     SKIP3                                                                
018500 01  NYCKLAR-TILL-DLI.                                                    
018600     03  W-W6GX01KEY-X.                                                   
018700         05  W-IDHTYP-01         PIC  X(4)   VALUE '6017'.                
018800         05  W-FILLER            PIC  X(26)  VALUE LOW-VALUE.             
018900     03  W-KDSEGKEY-X.                                                    
019000         05  W-KDSEGKEY          PIC  X      VALUE '1'.                   
019100     03  W-IDARTNR-X.                                                     
019200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
019300     03  W-W6D1CSEQ-X.                                                    
019400         05  W-IDLEVNRK          PIC  X(5)   VALUE SPACE.                 
019500         05  W-IDOKOLLIK         PIC  9(9).                               
019600     03  W-IDLEVNR-X.                                                     
019700         05  W-IDLEVNR           PIC  X(5)   VALUE SPACE.                 
019800     03  W-IDOKOLLI-X.                                                    
019900         05  W-IDOKOLLI          PIC 9(9).                                
020000     03  W-IDDC-X.                                                        
020100         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
020200     03  W-IDLAND-X.                                                      
020300         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
020400     03  W-W6GXKEY-6005-X.                                                
020500         05  W-IDHTYP-6005       PIC  X(4)   VALUE '6005'.                
020600         05  W-IDDC-6005         PIC  X(2)   VALUE SPACE.                 
020700         05  W-FILLER            PIC  X(24)  VALUE LOW-VALUE.             
020800     03  W-W6GXKEY-6006-X.                                                
020900         05  W-ADINLOMR-6006     PIC  X(4)   VALUE SPACE.                 
021000         05  W-FILLER            PIC  X      VALUE LOW-VALUE.             
021100                                                                          
021200     SKIP2                                                                
021300*    --- STATUS-KOD FRÅN IMS                                              
021400 01  STATUS-WS                   PIC XX.                                  
021500     88  SEGMENT-FINNS                       VALUE '  '.                  
021600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
021700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
021800     SKIP2                                                                
021900 01  GODK-STATUSKODER.                                                    
022000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
022100     SKIP3                                                                
022200 01  SSA1                        PIC X(64).                               
022300 01  SSA2                        PIC X(64).                               
022400     EJECT                                                                
022500*    --- IMS FUNKTIONSKODER                                               
022600*01  -COPY W0003                                                          
022700     EJECT                                                                
022800*    ---  DLI INPUT-OUTPUT AREA  1                                        
022900 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA-1'.         
023000     SKIP3                                                                
023100 01  DLI-IO-AREA-1.                                                       
023200     03  IO-AREA-1             PIC X(900)  VALUE SPACE.                   
023300     EJECT                                                                
023400     03  WLARTC01 REDEFINES IO-AREA-1.                                    
023500*        05  -COPY WDK601 -PRE ARTC01-                                    
023600     SKIP3                                                                
023700     03  WLARTC11 REDEFINES IO-AREA-1.                                    
023800*        05  -COPY WDK611 -PRE ARTC11-                                    
023900     SKIP3                                                                
024000     03  WLINLA11 REDEFINES IO-AREA-1.                                    
024100*        05  -COPY W6D111                                                 
024200     SKIP3                                                                
024300     03  WLINLA21 REDEFINES IO-AREA-1.                                    
024400*        05  -COPY W6D121                                                 
024500     SKIP3                                                                
024600     03  W6PLAA01 REDEFINES IO-AREA-1.                                    
024700*        05  -COPY W6GX01                                                 
024800     SKIP3                                                                
024900     03  W6PLAA11 REDEFINES IO-AREA-1.                                    
025000*        05  -COPY W6GX6006                                               
025100     EJECT                                                                
025200*    ---  DLI INPUT-OUTPUT AREA  2                                        
025300 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA-2'.         
025400     SKIP3                                                                
025500 01  DLI-IO-AREA-2.                                                       
025600     03  IO-AREA-2             PIC X(150)  VALUE SPACE.                   
025700     SKIP3                                                                
025800     03  W6LOPA01 REDEFINES IO-AREA-2.                                    
025900*        05  -COPY W6GX01                                                 
026000     SKIP3                                                                
026100     03  W6LOPA11 REDEFINES IO-AREA-2.                                    
026200*        05  -COPY W6GX6018                                               
026300     EJECT                                                                
026400 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK701'.           
026500     SKIP3                                                                
026600 01  DLI-IO-AREA-WDK701.                                                  
026700*    03  -COPY WDK701 -PRE SLAG-                                          
026800     EJECT                                                                
026900 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK711'.           
027000     SKIP3                                                                
027100 01  DLI-IO-AREA-WDK711.                                                  
027200*    03  -COPY WDK711                                                     
027300     EJECT                                                                
027400                                                                          
027500 01  FILLER                    PIC X(16) VALUE 'DLI-IO-WDK711'.           
027600     SKIP3                                                                
027700 01  DLI-IO-AREA-WDK712.                                                  
027800*    03  -COPY WDK712                                                     
027900     EJECT                                                                
028000                                                                          
028100 LINKAGE SECTION.                                                         
028200 01  MSG-PCB                     PIC X.                                   
028300     SKIP2                                                                
028400*01  -COPY W0008  -PRE 6194-                                              
028500     05  FILLER                  PIC X.                                   
028600     SKIP2                                                                
028700*01  -COPY W0008  -PRE ARTC-                                              
028800     05  FILLER                  PIC X.                                   
028900     EJECT                                                                
029000*01  -COPY W0008  -PRE WDK7-                                              
029100     05  FILLER                  PIC X.                                   
029200     EJECT                                                                
029300*01  -COPY W0008  -PRE W6G1-                                              
029400     05  FILLER                  PIC X.                                   
029500     EJECT                                                                
029600*01  -COPY W0008  -PRE W6D1-                                              
029700     05  FILLER                  PIC X.                                   
029800     EJECT                                                                
029900*01  -COPY W0008  -PRE PLAA-                                              
030000     05  FILLER                  PIC X.                                   
030100     EJECT                                                                
030200 PROCEDURE DIVISION  USING MSG-PCB 6194-PCB                               
030300                                   ARTC-PCB                               
030400                                   WDK7-PCB                               
030500                                   W6G1-PCB                               
030600                                   W6D1-PCB                               
030700                                   PLAA-PCB.                              
030800                                                                          
030900 MAIN SECTION.                                                            
031000     ENTRY 'DLITCBL' USING MSG-PCB 6194-PCB                               
031100                                   ARTC-PCB                               
031200                                   WDK7-PCB                               
031300                                   W6G1-PCB                               
031400                                   W6D1-PCB                               
031500                                   PLAA-PCB.                              
031600                                                                          
031700     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
031800     IF SUB-KDRC = 0                                                      
031900       IF REQU-KDPGMACT = 'E'                                             
032000         PERFORM A-INIT                                                   
032100         PERFORM B-KOLLA-NYCKLAR                                          
032200         IF NYCKLAR-OK                                                    
032300             PERFORM G-KOLLA-INPUT                                        
032400             IF INDATA-OK                                                 
032500               PERFORM H-UPPDATERA                                        
032600             END-IF                                                       
032700         END-IF                                                           
032800       ELSE                                                               
032900        MOVE SYS-ERROR    TO RESP-IDMSG-ERROR                             
033000       END-IF                                                             
033100       MOVE RESP-IDMSG-INFO    TO WS-IDMSG-INFO                           
033200       MOVE RESP-IDMSG-ERROR   TO WS-IDMSG-ERROR                          
033300       MOVE RESP-IDELMT-ERROR  TO WS-IDELMT-ERROR                         
033400       IF WS-IDMSG-ERROR NOT = SPACE                                      
033500           MOVE ALL '+' TO RESP-WL0110O1(1:10)                            
033600           MOVE WS-IDMSG-ERROR   TO RESP-IDMSG-ERROR                      
033700           MOVE WS-IDELMT-ERROR  TO RESP-IDELMT-ERROR                     
033800           MOVE WS-IDMSG-INFO    TO RESP-IDMSG-INFO                       
033900           MOVE 001              TO RESP-IDMSGVER                         
034000       END-IF                                                             
034100       PERFORM S02-RETURN-RESPONSE                                        
034200     END-IF                                                               
034300                                                                          
034400                                                                          
034500     MOVE ZERO TO RETURN-CODE                                             
034600     GOBACK                                                               
034700     .                                                                    
034800     EJECT                                                                
034900 A-INIT SECTION.                                                          
035000                                                                          
035100     MOVE '1'                         TO W-KDSEGKEY                       
035200                                                                          
035300     MOVE ALL '+'                     TO RESP-AREA                        
035400     MOVE SPACE                       TO RESP-IDMSG-ERROR                 
035500                                         RESP-IDMSG-INFO                  
035600                                         RESP-IDELMT-ERROR                
035700     MOVE 001                         TO RESP-IDMSGVER                    
035800     ACCEPT WS-TIAAMMDD FROM DATE                                         
035900     MOVE WS-TIAAMMDD                 TO WS-TIINLMOT                      
036100     .                                                                    
036200     EJECT                                                                
036300 B-KOLLA-NYCKLAR SECTION.                                                 
036400                                                                          
036500     MOVE JA            TO NYCKLAR-SW                                     
036600                                                                          
036700     MOVE NEJ           TO ARTIKEL-SW                                     
036800                           MID-INPUT-SW                                   
036900                                                                          
037000     MOVE REQU-IDDC-KEY TO W-IDDC                                         
037100                           WS-IDDC                                        
037200                                                                          
037300     IF NDC-CN OR LDC-CN                                                  
037400       MOVE WC-LAND-CN   TO W-IDLAND                                      
037500     ELSE                                                                 
037600       IF NDC-US                                                          
037700         MOVE WC-LAND-US TO W-IDLAND                                      
037800       ELSE                                                               
037900         MOVE WC-LAND-SE TO W-IDLAND                                      
038000       END-IF                                                             
038100     END-IF                                                               
038200                                                                          
038300     PERFORM BA-KONTROLL-MID-IDARTNR                                      
038400                                                                          
038500     MOVE REQU-IDARTNR-KEY            TO RESP-IDARTNR-KEY(1:)             
038600     INSPECT RESP-IDARTNR-KEY REPLACING LEADING ZERO BY SPACE             
038700     MOVE REQU-IDDC-KEY               TO RESP-IDDC-KEY                    
038800                                                                          
038900     IF NYCKLAR-FEL                                                       
039000        IF ARTIKEL-SAKNAS                                                 
039100          MOVE '041'                    TO RESP-IDMSG-ERROR               
039200          MOVE 'IDARTNR'                TO RESP-IDELMT-ERROR              
039300        END-IF                                                            
039400     END-IF                                                               
039500                                                                          
039600     IF NYCKLAR-OK                                                        
039700       PERFORM BB-KONTROLL-MID-IDLEVNR                                    
039800     END-IF                                                               
039900                                                                          
040000     IF NYCKLAR-OK                                                        
040100       PERFORM BC-KONTROLL-MID-IDOKOLLI                                   
040200     END-IF                                                               
040300                                                                          
040400     IF NYCKLAR-OK AND CDC-SE                                             
040500       PERFORM BD-KONTROLL-IDLEVNR-IDOKOLLI                               
040600     END-IF                                                               
040700                                                                          
040800     IF NYCKLAR-OK AND CDC-SE AND KOLLINR-FINNS                           
040900       PERFORM BE-KONTROLL-INLEV                                          
041000     END-IF                                                               
041100     .                                                                    
041200     EJECT                                                                
041300 BA-KONTROLL-MID-IDARTNR SECTION.                                         
041400                                                                          
041500     INSPECT REQU-IDARTNR-KEY REPLACING ALL SPACE BY ZERO                 
041600                                                                          
041700     IF REQU-IDARTNR-KEY NUMERIC                                          
041800       MOVE REQU-IDARTNR-KEY          TO W-IDARTNR                        
041900       PERFORM IMS-GU-ARTC01                                              
042000       IF SEGMENT-SAKNAS                                                  
042100         MOVE NEJ                     TO NYCKLAR-SW                       
042200         MOVE JA                      TO ARTIKEL-SW                       
042300       ELSE                                                               
042400         IF CDC-SE                                                        
042500           CONTINUE                                                       
042600         ELSE                                                             
042700           PERFORM IMS-GU-WDK711                                          
042800           IF SEGMENT-SAKNAS                                              
042900              MOVE NEJ                  TO NYCKLAR-SW                     
043000              MOVE JA                   TO ARTIKEL-SW                     
043100           END-IF                                                         
043200         END-IF                                                           
043300       END-IF                                                             
043400     ELSE                                                                 
043500       MOVE NEJ                       TO NYCKLAR-SW                       
043600       MOVE '024'                     TO RESP-IDMSG-ERROR                 
043700       MOVE 'IDARTNR'                 TO RESP-IDELMT-ERROR                
043800     END-IF                                                               
043900     .                                                                    
044000     EJECT                                                                
044100 BB-KONTROLL-MID-IDLEVNR SECTION.                                         
044200                                                                          
044300     IF REQU-IDLEVNR-KEY = ALL '+' OR SPACE                               
044400       MOVE SPACE                     TO WS-IDLEVNR                       
044500                                         RESP-IDLEVNR-KEY                 
044600     ELSE                                                                 
044700       IF CDC-SE                                                          
044800         IF REQU-IDLEVNR-KEY > '99399' AND                                
044900            REQU-IDLEVNR-KEY < '99600'                                    
045000           MOVE REQU-IDLEVNR-KEY      TO WS-IDLEVNR                       
045100                                         RESP-IDLEVNR-KEY                 
045200           MOVE JA                    TO EGET-LEVNR-SW                    
045300         ELSE                                                             
045400           MOVE REQU-IDLEVNR-KEY      TO WS-IDLEVNR                       
045500                                         RESP-IDLEVNR-KEY                 
045600         END-IF                                                           
045700       ELSE                                                               
045800         MOVE REQU-IDLEVNR-KEY        TO WS-IDLEVNR                       
045900                                         RESP-IDLEVNR-KEY                 
046000       END-IF                                                             
046100     END-IF                                                               
046200     .                                                                    
046300     EJECT                                                                
046400 BC-KONTROLL-MID-IDOKOLLI SECTION.                                        
046500                                                                          
046600     INSPECT REQU-IDOKOLLI-KEY REPLACING ALL SPACE BY ZERO                
046700                                                                          
046800     IF REQU-IDOKOLLI-KEY = ALL '+'                                       
046900        CONTINUE                                                          
047100     ELSE                                                                 
047200       IF REQU-IDOKOLLI-KEY NUMERIC                                       
047300         IF REQU-IDOKOLLI-KEY > ZERO                                      
047400            MOVE JA                 TO KOLLINR-SW                         
047500            MOVE REQU-IDOKOLLI-KEY  TO WS-IDOKOLLI                        
047600                                       RESP-IDOKOLLI-KEY                  
047700         ELSE                                                             
047800           MOVE NEJ                 TO NYCKLAR-SW                         
047900           MOVE '023'               TO RESP-IDMSG-ERROR                   
048000           MOVE 'IDKOLLI'           TO RESP-IDELMT-ERROR                  
048300         END-IF                                                           
048400       ELSE                                                               
048500         MOVE NEJ                   TO NYCKLAR-SW                         
048600         MOVE '024'                 TO RESP-IDMSG-ERROR                   
048700         MOVE 'IDKOLLI'             TO RESP-IDELMT-ERROR                  
049000       END-IF                                                             
049100     END-IF                                                               
049200     .                                                                    
049300     EJECT                                                                
049400 BD-KONTROLL-IDLEVNR-IDOKOLLI SECTION.                                    
049500     PERFORM IMS-GU-W6G110                                                
049600     IF KOLLINR-FINNS                                                     
049700       IF EGET-LEVNR                                                      
049800         IF WS-IDOKOLLI > 6018-IDOKOLLI                                   
049900           MOVE NEJ                    TO NYCKLAR-SW                      
050000           MOVE ERR-WRONG-CASE-OR-SUPP TO RESP-IDMSG-ERROR                
050100         END-IF                                                           
050110       ELSE                                                               
050120         IF WS-IDLEVNR = SPACE                                            
050121           MOVE NEJ                    TO NYCKLAR-SW                      
050122           MOVE ERR-WRONG-CASE-OR-SUPP TO RESP-IDMSG-ERROR                
050130         END-IF                                                           
050200       END-IF                                                             
050210     ELSE                                                                 
050220       IF WS-IDLEVNR > SPACE                                              
050230         MOVE NEJ                      TO NYCKLAR-SW                      
050240         MOVE ERR-WRONG-CASE-OR-SUPP   TO RESP-IDMSG-ERROR                
050250       END-IF                                                             
050300     END-IF                                                               
050400     .                                                                    
050500     EJECT                                                                
050600 BE-KONTROLL-INLEV SECTION.                                               
050700     IF EGET-LEVNR OR WS-IDLEVNR = SPACE                                  
050800       CONTINUE                                                           
050900     ELSE                                                                 
051000       MOVE WS-IDOKOLLI                TO W-IDOKOLLIK                     
051100                                          W-IDOKOLLI                      
051200       MOVE WS-IDLEVNR                 TO W-IDLEVNRK                      
051300                                          W-IDLEVNR                       
051400       PERFORM IMS-GU-W6D111-CSEQ                                         
051500       IF SEGMENT-FINNS                                                   
051600         PERFORM IMS-GNP-W6D121                                           
051700         PERFORM UNTIL SEGMENT-SAKNAS                                     
051800           IF RAD-KDINLSTA = SPACE OR 'FPK' OR 'SAK'                      
051900             MOVE NEJ                  TO NYCKLAR-SW                      
052000             MOVE ERR-CASE-NOT-STORED  TO RESP-IDMSG-ERROR                
052100           END-IF                                                         
052200           PERFORM IMS-GNP-W6D121                                         
052300         END-PERFORM                                                      
052400       END-IF                                                             
052500     END-IF.                                                              
052600     EJECT                                                                
052700 G-KOLLA-INPUT SECTION.                                                   
052800                                                                          
052900     MOVE JA                          TO INDATA-SW                        
053000     PERFORM S01-KONTROLL-OM-INDATA                                       
053100                                                                          
053200     IF MID-INPUT-SAKNAS                                                  
053300       MOVE ERR-PF11-AND-NO-DATA      TO RESP-IDMSG-ERROR                 
053400       MOVE NEJ                       TO INDATA-SW                        
053500     ELSE                                                                 
053600       PERFORM IMS-GU-ARTC01                                              
053700       IF SEGMENT-FINNS                                                   
053800           IF CDC-SE                                                      
053900             PERFORM GA-KONTROLL-ADINLOMR-PRT                             
054000             PERFORM GB-KONTROLL-KVINLART                                 
054100             PERFORM GC-KONTROLL-TIINLMOT                                 
054200             PERFORM GD-KONTROLL-IDLEVNR                                  
054300             PERFORM GG-KONTROLL-KVFLETI                                  
054400           ELSE                                                           
054500             PERFORM IMS-GU-WDK701                                        
054600             IF SEGMENT-FINNS                                             
054700               PERFORM GB-KONTROLL-KVINLART                               
054800               PERFORM GG-KONTROLL-KVFLETI                                
054900             ELSE                                                         
055000               MOVE NEJ          TO INDATA-SW                             
055100               MOVE '041'        TO RESP-IDMSG-ERROR                      
055200               MOVE 'IDARTNR'    TO RESP-IDELMT-ERROR                     
055300             END-IF                                                       
055400           END-IF                                                         
055500       ELSE                                                               
055600         MOVE NEJ TO INDATA-SW                                            
055700         MOVE '041'                   TO RESP-IDMSG-ERROR                 
055800         MOVE 'IDARTNR'               TO RESP-IDELMT-ERROR                
055900       END-IF                                                             
056000     END-IF                                                               
056100     .                                                                    
056200     SKIP3                                                                
056300 GA-KONTROLL-ADINLOMR-PRT SECTION.                                        
056400                                                                          
056500     IF REQU-ADINLOMR-PRT = ALL '+' OR SPACE                              
056600       MOVE NEJ                   TO INDATA-SW                            
056700       MOVE ERR-INVALID-PRINTER   TO RESP-IDMSG-ERROR                     
056800     ELSE                                                                 
056900       MOVE 001                   TO PRT-KDCALL                           
057000       MOVE SPACE                 TO PRT-IDPRTLST                         
057100       MOVE '6F'                  TO PRT-IDPRTLST(1:2)                    
057200       MOVE REQU-ADINLOMR-PRT     TO PRT-IDPRTLST(3:4)                    
057300                                     RESP-ADINLOMR-PRT                    
057400                                     W-ADINLOMR-6006                      
057500       CALL W006PRT USING PRT-W006PRT                                     
057600       IF PRT-KDSVAR = 'F'                                                
057700         MOVE NEJ TO INDATA-SW                                            
057800         MOVE ERR-INVALID-PRINTER TO RESP-IDMSG-ERROR                     
057900       ELSE                                                               
058000         MOVE PRT-IDPRTLST        TO WS-IDPRTLST                          
058100       END-IF                                                             
058200     END-IF                                                               
058300     .                                                                    
058400     SKIP3                                                                
058500 GB-KONTROLL-KVINLART SECTION.                                            
058600                                                                          
058700     IF REQU-KVINLART = ALL '+'                                           
058800       MOVE NEJ                       TO INDATA-SW                        
058900       MOVE '023'                     TO RESP-IDMSG-ERROR                 
059000       MOVE 'KVINLART'                TO RESP-IDELMT-ERROR                
059100     ELSE                                                                 
059300       IF REQU-KVINLART NUMERIC                                           
059400         MOVE REQU-KVINLART           TO WS-KVINLART                      
059500       ELSE                                                               
059600         MOVE NEJ                     TO INDATA-SW                        
059700         MOVE '024'                   TO RESP-IDMSG-ERROR                 
059800         MOVE 'KVINLART'              TO RESP-IDELMT-ERROR                
059900       END-IF                                                             
060000     END-IF                                                               
060100                                                                          
060200     IF REQU-KVINLART-LAST = ALL '+'                                      
060300       CONTINUE                                                           
060400     ELSE                                                                 
060600       IF REQU-KVINLART-LAST NUMERIC                                      
060700         MOVE REQU-KVINLART-LAST      TO WS-KVINLART-LAST                 
060800       ELSE                                                               
060900         MOVE NEJ                     TO INDATA-SW                        
061000         MOVE '024'                   TO RESP-IDMSG-ERROR                 
061100         MOVE 'KVINLART-LAST'         TO RESP-IDELMT-ERROR                
061200       END-IF                                                             
061300     END-IF                                                               
061400     .                                                                    
061500     EJECT                                                                
061600                                                                          
061700 GC-KONTROLL-TIINLMOT SECTION.                                            
061800                                                                          
061900     IF REQU-TIINLMOT = ALL '+'                                           
062000       CONTINUE                                                           
062300     ELSE                                                                 
062400       IF REQU-TIINLMOT NUMERIC                                           
062500         MOVE REQU-TIINLMOT           TO DAT-I-TIDATUM                    
062600         MOVE 'AAMMDD'                TO DAT-KDDATFORM                    
062700         CALL WDATKONV USING DAT-KDDATFORM, DAT-I-TIDATUM                 
062800                             DAT-O-TIDATUM, DAT-KDSVAR                    
062900         IF DAT-KDSVAR-OK                                                 
063000*DATUM-JÄMFÖRELSE FÖR ATT KLARA ÅR 2000                                   
063100           MOVE REQU-TIINLMOT         TO TMP1-YYMMDD                      
063200           MOVE FUNCTION CURRENT-DATE(3:6)                                
063300                                      TO TMP2-YYMMDD                      
063400           PERFORM WY2000P1                                               
063500           IF TMP1-YYMMDD  <= TMP2-YYMMDD                                 
063600             MOVE REQU-TIINLMOT       TO WS-TIINLMOT                      
063701                                         RESP-TIINLMOT                    
063800           ELSE                                                           
063900             MOVE NEJ                 TO INDATA-SW                        
064000             MOVE '023'               TO RESP-IDMSG-ERROR                 
064100             MOVE 'TIINLMOT'          TO RESP-IDELMT-ERROR                
064200           END-IF                                                         
064300         ELSE                                                             
064400           MOVE NEJ                   TO INDATA-SW                        
064500           MOVE '023'                 TO RESP-IDMSG-ERROR                 
064600           MOVE 'TIINLMOT'            TO RESP-IDELMT-ERROR                
064700         END-IF                                                           
064800       ELSE                                                               
064900         MOVE NEJ                     TO INDATA-SW                        
065000         MOVE '024'                   TO RESP-IDMSG-ERROR                 
065100         MOVE 'TIINLMOT'              TO RESP-IDELMT-ERROR                
065200       END-IF                                                             
065300     END-IF                                                               
065400     .                                                                    
065500     EJECT                                                                
065600 GD-KONTROLL-IDLEVNR SECTION.                                             
065700                                                                          
065800     IF WS-IDLEVNR = SPACE                                                
065900       MOVE W-IDDC       TO W-IDDC-6005                                   
066000       PERFORM IMS-GU-PLAA01                                              
066100       PERFORM IMS-GNP-PLAA11                                             
066200       IF SEGMENT-FINNS                                                   
066300         MOVE 6006-IDLEVNR            TO WS-IDLEVNR                       
066400       ELSE                                                               
066500         MOVE NEJ                     TO INDATA-SW                        
066600         MOVE ERR-INVALID-PRINTER     TO RESP-IDMSG-ERROR                 
066700       END-IF                                                             
066800     END-IF                                                               
066900     .                                                                    
067000 GG-KONTROLL-KVFLETI SECTION.                                             
067100                                                                          
067200     IF REQU-KVFLETI = ALL '+'                                            
067300       MOVE +1 TO WS-KVFLETI                                              
067400     ELSE                                                                 
067500       IF REQU-KVFLETI NUMERIC                                            
067600         MOVE REQU-KVFLETI TO WS-KVFLETI                                  
067700         IF WS-KVFLETI > 1 AND CDC-SE                                     
067800           IF WS-IDOKOLLI = ZERO                                          
067810             CONTINUE                                                     
067820           ELSE                                                           
067821             MOVE NEJ               TO INDATA-SW                          
067822             MOVE '023'             TO RESP-IDMSG-ERROR                   
067823             MOVE 'KVFLETI'         TO RESP-IDELMT-ERROR                  
067830           END-IF                                                         
067900         END-IF                                                           
068000       ELSE                                                               
068100         MOVE NEJ                   TO INDATA-SW                          
068200         MOVE '024'                 TO RESP-IDMSG-ERROR                   
068300         MOVE 'KVFLETI'             TO RESP-IDELMT-ERROR                  
068400       END-IF                                                             
068500     END-IF                                                               
068600                                                                          
068610     IF CDC-SE                                                            
068620       CONTINUE                                                           
068630     ELSE                                                                 
068700       IF WS-KVFLETI  < +16                                               
068800         CONTINUE                                                         
068900       ELSE                                                               
069000         MOVE TOO-MANY-LINES    TO RESP-IDMSG-ERROR                       
069100         MOVE NEJ               TO INDATA-SW                              
069200         MOVE 'KVFLETI'         TO RESP-IDELMT-ERROR                      
069300       END-IF                                                             
069310     END-IF                                                               
069400     .                                                                    
069500     EJECT                                                                
069600 H-UPPDATERA SECTION.                                                     
069700                                                                          
069800     IF CDC-SE                                                            
069900       PERFORM HA-UPDATE-CDC                                              
070000     ELSE                                                                 
070100       PERFORM HB-UPDATE-XDC                                              
071000     END-IF                                                               
080000     .                                                                    
090000     EJECT                                                                
100000 HA-UPDATE-CDC SECTION.                                                   
110000                                                                          
120000     MOVE SPACE                       TO MOD6194-MID-W6I19401             
121000     MOVE 1                           TO MOD6194-MID-KVPOST               
122000     MOVE 'W6017300'                  TO MOD6194-MID-IDPGM                
123000     MOVE WS-IDPRTLST                 TO MOD6194-MID-IDPRTLST             
124000                                                                          
125000     PERFORM IMS-GU-ARTC01                                                
126000     IF SEGMENT-FINNS                                                     
127000       MOVE ARTC01-ART-KDSORT         TO WS-KDSORT                        
127100       PERFORM IMS-GNP-ARTC11                                             
127200       IF SEGMENT-FINNS                                                   
127300         COMPUTE WS-VKKOLLIN = (ARTC11-CLAG-VKART                         
127400                              * WS-KVINLART)                              
127500                              / 1000                                      
127600        MOVE ARTC11-CLAG-ADLAGOMR     TO WS-ADLAGOMR                      
127700        MOVE ARTC11-CLAG-ADGANG       TO WS-ADGANG                        
127800        MOVE ARTC11-CLAG-ADPLATS      TO WS-ADPLATS                       
127900       END-IF                                                             
128000     END-IF                                                               
128700                                                                          
128800     IF WS-KVFLETI = ZERO                                                 
128900       MOVE +1           TO WS-KVFLETI                                    
129000     END-IF                                                               
129100                                                                          
129200     MOVE +1             TO IX                                            
129300                                                                          
129400     PERFORM UNTIL IX > WS-KVFLETI                                        
129500                                                                          
129600       ADD +1            TO 6194-IX                                       
129610                                                                          
129620       IF KOLLINR-SAKNAS                                                  
129621         IF FL-LOPA-UPPDATERAD = NEJ                                      
129630           PERFORM IMS-GHU-W6G110                                         
129640           ADD +1 TO 6018-IDOKOLLI                                        
129650           MOVE 6018-IDOKOLLI TO WS-IDOKOLLI                              
129651           MOVE JA TO FL-LOPA-UPPDATERAD                                  
129652         ELSE                                                             
129653           ADD +1 TO 6018-IDOKOLLI                                        
129654           MOVE 6018-IDOKOLLI TO WS-IDOKOLLI                              
129655         END-IF                                                           
129660       END-IF                                                             
129700                                                                          
129800       MOVE WS-ADLAGOMR         TO MOD6194-MID-ADLAGOMR(6194-IX)          
129900       MOVE WS-ADGANG           TO MOD6194-MID-ADGANG(6194-IX)            
130000       MOVE WS-ADPLATS          TO MOD6194-MID-ADPLATS(6194-IX)           
130100       MOVE WS-KDSORT           TO MOD6194-MID-KDSORT(6194-IX)            
130200       MOVE REQU-IDARTNR-KEY    TO MOD6194-MID-IDARTNR(6194-IX)           
130300       MOVE ZERO                TO MOD6194-MID-BEFT(6194-IX)              
130400       MOVE WS-KVINLART         TO MOD6194-MID-KVINLART(6194-IX)          
130500       MOVE WS-IDLEVNR          TO                                        
130600                                MOD6194-MID-IDLEVNR-KOLLI(6194-IX)        
130700       MOVE WS-IDOKOLLI         TO MOD6194-MID-IDOKOLLI(6194-IX)          
130800       IF IX = WS-KVFLETI AND WS-KVFLETI > +1                             
130900         IF WS-KVINLART-LAST > +0                                         
131000           MOVE WS-KVINLART-LAST TO                                       
131100                                MOD6194-MID-KVINLART(6194-IX)             
131200           COMPUTE WS-VKKOLLIN = (ARTC11-CLAG-VKART                       
131300                               * WS-KVINLART-LAST) / 1000                 
131400         END-IF                                                           
131500       END-IF                                                             
131600                                                                          
131700       MOVE WS-TIINLMOT     TO MOD6194-MID-TIINLMOT(6194-IX)              
131800                                                                          
131900       MOVE WS-VKKOLLIN     TO MOD6194-MID-VKKOLLIN(6194-IX)              
132000                                                                          
132100       MOVE ZERO            TO MOD6194-MID-VKKOLLIB(6194-IX)              
132200                               MOD6194-MID-IDLOPNRM(6194-IX)              
132300       IF WS-KVFLETI > +1                                                 
132400          MOVE WS-KVFLETI   TO MOD6194-MID-IDLOPNRM(6194-IX)              
132500          ADD  +900         TO MOD6194-MID-IDLOPNRM(6194-IX)              
132600       END-IF                                                             
132700                                                                          
132800       IF 6194-IX > 14                                                    
132900         PERFORM HAA-P-TO-P-6194                                          
133000                                                                          
133100         IF FOERSTA-6194                                                  
133200           PERFORM IMS-ISRT-ALT-MSG-6194                                  
133300           MOVE NEJ TO 6194-SW                                            
133400         ELSE                                                             
133500           PERFORM IMS-PURG-6194-MSG                                      
133600         END-IF                                                           
133700       END-IF                                                             
133800                                                                          
133900       ADD +1               TO IX                                         
134000                                                                          
134100     END-PERFORM                                                          
134200                                                                          
134300     IF 6194-IX > 0                                                       
134400       PERFORM HAA-P-TO-P-6194                                            
134500                                                                          
134600       IF FOERSTA-6194                                                    
134700         PERFORM IMS-ISRT-ALT-MSG-6194                                    
134800         MOVE NEJ TO 6194-SW                                              
134900       ELSE                                                               
135000         PERFORM IMS-PURG-6194-MSG                                        
135100       END-IF                                                             
135200     END-IF                                                               
135300                                                                          
135400     IF FL-LOPA-UPPDATERAD = JA                                           
135500       PERFORM IMS-REPL-W6G110                                            
135600     END-IF                                                               
135700                                                                          
135800     MOVE INF-PRINT-REQUESTED    TO RESP-IDMSG-INFO                       
135900     MOVE SPACE                  TO RESP-WL0110O1                         
136000     .                                                                    
136100     EJECT                                                                
136200 HB-UPDATE-XDC SECTION.                                                   
136300                                                                          
136400     MOVE WS-KVFLETI             TO RESP-KVRADER                          
136500                                                                          
136600     PERFORM IMS-GU-ARTC01                                                
136700     IF SEGMENT-FINNS                                                     
136800       MOVE ARTC01-ART-KDSORT    TO WS-KDSORT                             
136900       PERFORM IMS-GNP-ARTC11                                             
137000       IF SEGMENT-FINNS                                                   
137100         COMPUTE WS-VKKOLLIN = (ARTC11-CLAG-VKART                         
137200                            * WS-KVINLART)                                
137300                            / 1000                                        
137400         PERFORM IMS-GU-WDK712                                            
137500         IF SEGMENT-FINNS                                                 
137600           IF LART-VKART > 0                                              
137700                                                                          
137800             COMPUTE WS-VKKOLLIN = (LART-VKART *                          
137900                                   WS-KVINLART) / 1000                    
138000             MOVE JA TO WS-FL-WDK712                                      
138100           END-IF                                                         
138200         END-IF                                                           
138300                                                                          
138400         PERFORM IMS-GU-WDK711                                            
138500         IF SEGMENT-FINNS                                                 
138600            MOVE SLAG-ADLAGOMR   TO WS-ADLAGOMR                           
138700            MOVE SLAG-ADGANG     TO WS-ADGANG                             
138800            MOVE SLAG-ADPLATS    TO WS-ADPLATS                            
138900         END-IF                                                           
139000       END-IF                                                             
139100     END-IF                                                               
139200                                                                          
139300     IF WS-KVFLETI = ZERO                                                 
139400       MOVE +1           TO WS-KVFLETI                                    
139500     END-IF                                                               
139600                                                                          
139700     MOVE +1             TO IX                                            
139800                                                                          
139900     PERFORM UNTIL IX > WS-KVFLETI                                        
140000                                                                          
140100       ADD +1 TO 6194-IX                                                  
140200                                                                          
140300       MOVE WS-ADLAGOMR          TO RESP-ADLAGOMR(6194-IX)                
140400       MOVE WS-ADGANG            TO RESP-ADGANG(6194-IX)                  
140500       MOVE WS-ADPLATS           TO RESP-ADPLATS(6194-IX)                 
140600       MOVE WS-KDSORT            TO RESP-KDSORT(6194-IX)                  
140700       MOVE REQU-IDARTNR-KEY     TO RESP-IDARTNR(6194-IX)                 
140800       MOVE WS-KVINLART          TO RESP-KVINLART-LINE(6194-IX)           
140900       MOVE WS-IDLEVNR           TO RESP-IDLEVNR(6194-IX)                 
141000       MOVE WS-IDOKOLLI          TO RESP-IDOKOLLI(6194-IX)                
141100       IF IX = WS-KVFLETI AND WS-KVFLETI > +1                             
141200         IF WS-KVINLART-LAST > +0                                         
141300           MOVE WS-KVINLART-LAST TO RESP-KVINLART-LINE(6194-IX)           
141400           COMPUTE WS-VKKOLLIN = (ARTC11-CLAG-VKART                       
141500                               * WS-KVINLART-LAST) / 1000                 
141600           IF WS-FL-WDK712 = JA                                           
141700              COMPUTE WS-VKKOLLIN = (LART-VKART *                         
141800                                 WS-KVINLART-LAST) / 1000                 
141900           END-IF                                                         
142000         END-IF                                                           
142100       END-IF                                                             
142200                                                                          
142300       MOVE WS-TIAAMMDD          TO RESP-TIINLMOT-LINE(6194-IX)           
142400       MOVE WS-VKKOLLIN          TO RESP-VKKOLLIN(6194-IX)                
142500       MOVE ZERO                 TO RESP-IDLOPNRM(6194-IX)                
142600                                                                          
142700       ADD +1 TO IX                                                       
142800                                                                          
142900     END-PERFORM                                                          
143000                                                                          
143100     MOVE INF-UPDATE-DONE TO RESP-IDMSG-INFO                              
143200     .                                                                    
143300     EJECT                                                                
143400 HAA-P-TO-P-6194 SECTION.                                                 
143500                                                                          
143600     COMPUTE P-TO-P-KVLL = LNG-P-TO-P-PREFIX + 23 + (67 * 6194-IX)        
143700                                                                          
143800     MOVE 'W6T194X '                  TO P-TO-P-KDTRANS                   
143900     MOVE '6173'                      TO P-TO-P-IDTRANS                   
144000     MOVE '1'                         TO P-TO-P-KDMFSFOR                  
144100                                                                          
144200     MOVE 6194-IX                     TO MOD6194-MID-KVPOST               
144300     MOVE MOD6194-MID-W6I19401        TO P-TO-P-DATA                      
144400                                                                          
144500     MOVE +0                          TO 6194-IX                          
144600     .                                                                    
144700     EJECT                                                                
144800 S01-KONTROLL-OM-INDATA SECTION.                                          
144900                                                                          
145000     IF REQU-IDARTNR-KEY = ALL '+' AND                                    
145100       REQU-IDLEVNR-KEY   = ALL '+' AND                                   
145200       REQU-IDOKOLLI-KEY  = ALL '+' AND                                   
145300       REQU-ADINLOMR-PRT  = ALL '+' AND                                   
145400       REQU-TIINLMOT      = ALL '+' AND                                   
145500       REQU-KVINLART      = ALL '+' AND                                   
145600       REQU-KVFLETI       = ALL '+' AND                                   
145700       REQU-KVINLART-LAST = ALL '+'                                       
145800       MOVE JA                        TO MID-INPUT-SW                     
145900     ELSE                                                                 
146000       MOVE NEJ                       TO MID-INPUT-SW                     
146100     END-IF                                                               
146200     .                                                                    
146300     EJECT                                                                
146400* -COPY WY2000P1                                                          
146500*    --- DISPATCHER SECTIONS                                              
146600 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
146700                                                                          
146800     MOVE 'GETARG'               TO SUB-KDFUNC                            
146900     MOVE 'CARPARTS.LDC.REPLACELABEL'       TO SUB-ADDISPABS              
147000     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
147100                                                                          
147200     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
147300                                                                          
147400     IF SUB-KDRC > 0                                                      
147500       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
147600       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
147700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
147800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
147900     END-IF                                                               
148000     .                                                                    
148100     SKIP3                                                                
148200 S02-RETURN-RESPONSE SECTION.                                             
148300                                                                          
148400     MOVE 'RETURN'                   TO SUB-KDFUNC                        
148500     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
148600                                                                          
148700     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
148800                                                                          
148900     IF SUB-KDRC > 0                                                      
149000       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
149100       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
149200       DELIMITED BY SIZE INTO ERROR-TEXT                                  
149300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
149400     END-IF                                                               
149500     .                                                                    
149600     EJECT                                                                
149700* --- IMS SEKTIONER ---                                                   
149800     EJECT                                                                
149900 IMS-ISRT-ALT-MSG-6194 SECTION.                                           
150000                                                                          
150100     MOVE LOW-VALUE        TO    P-TO-P-KDZ1 P-TO-P-KDZ2                  
150200     MOVE '  '             TO    GODK-STATUSKODER                         
150300     CALL CBLTDLI          USING ISRT 6194-PCB P-TO-P-SW                  
150400     MOVE 6194-STATUS-CODE TO    STATUS-WS                                
150500     PERFORM IMS-STATUSKONTROLL                                           
150600     .                                                                    
150700     SKIP3                                                                
150800 IMS-PURG-6194-MSG SECTION.                                               
150900     MOVE LOW-VALUE        TO    P-TO-P-KDZ1 P-TO-P-KDZ2                  
151000     MOVE SPACE            TO GODK-STATUSKODER                            
151100     CALL CBLTDLI          USING PURG 6194-PCB P-TO-P-SW                  
151200     MOVE 6194-STATUS-CODE TO STATUS-WS                                   
151300     PERFORM IMS-STATUSKONTROLL                                           
151400     .                                                                    
151500     SKIP3                                                                
151600 IMS-GU-ARTC01 SECTION.                                                   
151700                                                                          
151800     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
151900          DELIMITED BY SIZE INTO SSA1                                     
152000     MOVE '  GE' TO GODK-STATUSKODER                                      
152100     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-1 SSA1                    
152200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
152300     PERFORM IMS-STATUSKONTROLL                                           
152400     .                                                                    
152500     SKIP2                                                                
152600 IMS-GNP-ARTC11 SECTION.                                                  
152700                                                                          
152800     MOVE 'WLARTC11 ' TO SSA1                                             
152900     MOVE '  GE' TO GODK-STATUSKODER                                      
153000     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-1 SSA1                   
153100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
153200     PERFORM IMS-STATUSKONTROLL                                           
153300     .                                                                    
153400     SKIP2                                                                
153500 IMS-GU-WDK701 SECTION.                                                   
153600                                                                          
153700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
153800          DELIMITED BY SIZE INTO SSA1                                     
153900     MOVE '  GE' TO GODK-STATUSKODER                                      
154000     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK701 SSA1               
154100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
154200     PERFORM IMS-STATUSKONTROLL                                           
154300     .                                                                    
154400     SKIP2                                                                
154500 IMS-GU-WDK711 SECTION.                                                   
154600                                                                          
154700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
154800          DELIMITED BY SIZE INTO SSA1                                     
154900     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
155000          DELIMITED BY SIZE INTO SSA2                                     
155100     MOVE '  GE' TO GODK-STATUSKODER                                      
155200     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK711 SSA1 SSA2          
155300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
155400     PERFORM IMS-STATUSKONTROLL                                           
155500     .                                                                    
155600     EJECT                                                                
155700 IMS-GU-WDK712 SECTION.                                                   
155800     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
155900          DELIMITED BY SIZE INTO SSA1                                     
156000     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
156100          DELIMITED BY SIZE INTO SSA2                                     
156200     MOVE '  GE' TO GODK-STATUSKODER                                      
156300     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK712 SSA1 SSA2          
156400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
156500     PERFORM IMS-STATUSKONTROLL                                           
156600     .                                                                    
156700     SKIP2                                                                
156800 IMS-GU-W6G110 SECTION.                                                   
156900                                                                          
157000     STRING 'W6G101  (W6GXKEY  =' W-W6GX01KEY-X ')'                       
157100          DELIMITED BY SIZE INTO SSA1                                     
157200     MOVE 'W6G110 '      TO SSA2                                          
157300     MOVE '  ' TO GODK-STATUSKODER                                        
157400     CALL CBLTDLI USING GU W6G1-PCB DLI-IO-AREA-2 SSA1 SSA2               
157500     MOVE W6G1-STATUS-CODE TO STATUS-WS                                   
157600     PERFORM IMS-STATUSKONTROLL                                           
157700     .                                                                    
157800     SKIP2                                                                
157900 IMS-GHU-W6G110 SECTION.                                                  
158000                                                                          
158100     STRING 'W6G101  (W6GXKEY  =' W-W6GX01KEY-X ')'                       
158200          DELIMITED BY SIZE INTO SSA1                                     
158300     MOVE 'W6G110 '      TO SSA2                                          
158400     MOVE '  ' TO GODK-STATUSKODER                                        
158500     CALL CBLTDLI USING GHU W6G1-PCB DLI-IO-AREA-2 SSA1 SSA2              
158600     MOVE W6G1-STATUS-CODE TO STATUS-WS                                   
158700     PERFORM IMS-STATUSKONTROLL                                           
158800     .                                                                    
158900     SKIP2                                                                
159000 IMS-REPL-W6G110 SECTION.                                                 
159100                                                                          
159200     MOVE '  ' TO GODK-STATUSKODER                                        
159300     CALL CBLTDLI USING REPL W6G1-PCB DLI-IO-AREA-2                       
159400     MOVE W6G1-STATUS-CODE TO STATUS-WS                                   
159500     PERFORM IMS-STATUSKONTROLL                                           
159600     .                                                                    
159700     SKIP2                                                                
159800 IMS-GU-W6D111-CSEQ SECTION.                                              
159900     STRING 'W6D111  (W6D1CSEQ =' W-W6D1CSEQ-X ')'                        
160000          DELIMITED BY SIZE INTO SSA1                                     
160100     MOVE '  GE' TO GODK-STATUSKODER                                      
160200     CALL CBLTDLI USING GU W6D1-PCB DLI-IO-AREA-1 SSA1                    
160300     MOVE W6D1-STATUS-CODE TO STATUS-WS                                   
160400     PERFORM IMS-STATUSKONTROLL                                           
160500     .                                                                    
160600     SKIP2                                                                
160700 IMS-GNP-W6D121 SECTION.                                                  
160800     STRING 'W6D121  (IDLEVNRK =' W-IDLEVNR-X                             
160900            '&IDOKOLLI =' W-IDOKOLLI-X ')'                                
161000          DELIMITED BY SIZE INTO SSA1                                     
161100     MOVE '  GE' TO GODK-STATUSKODER                                      
161200     CALL CBLTDLI USING GNP W6D1-PCB DLI-IO-AREA-1 SSA1                   
161300     MOVE W6D1-STATUS-CODE TO STATUS-WS                                   
161400     PERFORM IMS-STATUSKONTROLL                                           
161500     .                                                                    
161600     SKIP3                                                                
161700 IMS-GU-PLAA01 SECTION.                                                   
161800                                                                          
161900     STRING 'W6PLAA01(W6GXKEY  =' W-W6GXKEY-6005-X ')'                    
162000          DELIMITED BY SIZE INTO SSA1                                     
162100     MOVE '  GE' TO GODK-STATUSKODER                                      
162200     CALL CBLTDLI USING GU PLAA-PCB DLI-IO-AREA-1 SSA1                    
162300     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
162400     PERFORM IMS-STATUSKONTROLL                                           
162500     .                                                                    
162600     SKIP3                                                                
162700 IMS-GNP-PLAA11 SECTION.                                                  
162800                                                                          
162900     STRING 'W6PLAA11(W6GXKEY  =' W-W6GXKEY-6006-X ')'                    
163000          DELIMITED BY SIZE INTO SSA2                                     
163100     MOVE '  GE' TO GODK-STATUSKODER                                      
163200     CALL CBLTDLI USING GNP PLAA-PCB DLI-IO-AREA-1 SSA1 SSA2              
163300     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
163400     PERFORM IMS-STATUSKONTROLL                                           
163500     .                                                                    
163600     SKIP2                                                                
163700 IMS-STATUSKONTROLL SECTION.                                              
163800                                                                          
163900     SET STATUS-IX TO 1                                                   
164000     SEARCH GODK-STATUS                                                   
164100       AT END                                                             
164200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
164300         DELIMITED BY SIZE INTO FELTEXT                                   
164400         CALL FELLOG                                                      
164500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
164600         CONTINUE                                                         
164700     END-SEARCH                                                           
164800     .                                                                    
