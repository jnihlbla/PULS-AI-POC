000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL011200.                                                
000300 AUTHOR.         SUBBARAO PARUCHURI V.                                    
000400 DATE-WRITTEN.   04/10/28.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       'CARPARTS.LDC.PARTLOCATIONHIST'                          
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        FRÅGEBILD FÖR ARTIKELS LAGERPLATSHISTORIK                        
001100*        PROGRAMMET LÄSER      WLLOCB (WDJ9)                              
001200*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
001300*                                                                         
001400*        WL011200 PROGRAM IS A REPLICA OF W6031900 PROGRAM                
001500*        AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS                          
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSACTION: WL0112T                                             
001900*        REQUEST:     WL0112I1                                            
002000*                                                                         
002100*    OUTDATA.                                                             
002200*        RESPONSE:    WL0112O1                                            
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP3                                                                
003300 FILE SECTION.                                                            
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600 77  IDPGM                       PIC X(08)   VALUE 'WL011200'.            
003700                                                                          
003800*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003900 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004100 77  KDRC-DISPLAY                PIC Z(5).                                
004200                                                                          
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500                                                                          
004600*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004700 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004800 77  MAX-INDX                    PIC S9(4)  VALUE +500  COMP SYNC.        
004900                                                                          
005000                                                                          
005100*    --- MELLANLAGRING                                                    
005200 77  W-TISTADAT                  PIC 9(8)    VALUE ZERO.                  
005300 77  W-TISTODAT                  PIC 9(8)    VALUE ZERO.                  
005400 77  W-KDLOC                     PIC X       VALUE SPACE.                 
005500                                                                          
005600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005700 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005800     88  NYCKLAR-OK                          VALUE 'J'.                   
005900     88  NYCKLAR-FEL                         VALUE 'N'.                   
006000                                                                          
006100 77  WS-COUNT                    PIC 9(03) VALUE ZERO.                    
006200 77  WS-IDELMT-ERROR             PIC X(16).                               
006300 77  WS-IDMSG-ERROR              PIC X(03).                               
006400 77  WS-IDMSG-INFO               PIC X(03).                               
006500     EJECT                                                                
006600*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006700 01  GENERAL-SUBPROGRAMS.                                                 
006800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007000     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
007100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007200     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
007300     SKIP3                                                                
007400*    --- PARAMETERS TO ABEND                                              
007500                                                                          
007600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007700 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007800 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007900     EJECT                                                                
008000*                                                                         
008100 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
008200     SKIP3                                                                
008300*01  -COPY WZ01SUB                                                        
008400     EJECT                                                                
008500 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
008600     SKIP3                                                                
008700 01  REQU-AREA.                                                           
008800*    03  -COPY WZ01REQU                                                   
008900*    03  -COPY WL0112I1                                                   
009000     EJECT                                                                
009100*01  -COPY WWDC99                                                         
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
009400     SKIP3                                                                
009500 01  RESP-AREA.                                                           
009600*    03  -COPY WZ01RESP                                                   
009700*    03  -COPY WL0112O1                                                   
009800     EJECT                                                                
009900 01  MESSAGE-CODES.                                                       
010000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010100     03  ERR-PARTHIST-MISSING    PIC X(3)    VALUE '263'.                 
010200     03  SYS-ERROR               PIC X(3)    VALUE '099'.                 
010300     EJECT                                                                
010400*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
010500 01  SPAR-AREA.                                                           
010600     03  SPAR-IDTRANS            PIC X(4)    VALUE '6319'.                
010700                                                                          
010800     03  SPAR-WDJ911KY-ENTER.                                             
010900         05  W-IDDC-ENTER        PIC X(2)    VALUE SPACE.                 
011000         05  W-DASTADAT-ENTER    PIC S9(9)   VALUE ZERO COMP-3.           
011100         05  W-TISTATID-ENTER    PIC S9(7)   VALUE ZERO COMP-3.           
011200         05  W-ADLAGOMR-ENTER    PIC 9(2)    VALUE ZERO.                  
011300         05  W-ADGANG-ENTER      PIC 9(2)    VALUE ZERO.                  
011400         05  W-ADPLATS-ENTER     PIC 9(5)    VALUE ZERO.                  
011500                                                                          
011600     03  SPAR-WDJ911KY-NEXT.                                              
011700         05  W-IDDC-NEXT         PIC X(2)    VALUE SPACE.                 
011800         05  W-DASTADAT-NEXT     PIC S9(9)   VALUE ZERO COMP-3.           
011900         05  W-TISTATID-NEXT     PIC S9(7)   VALUE ZERO COMP-3.           
012000         05  W-ADLAGOMR-NEXT     PIC 9(2)    VALUE ZERO.                  
012100         05  W-ADGANG-NEXT       PIC 9(2)    VALUE ZERO.                  
012200         05  W-ADPLATS-NEXT      PIC 9(5)    VALUE ZERO.                  
012300     EJECT                                                                
012400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012500     EJECT                                                                
012600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012700     SKIP3                                                                
012800                                                                          
012900 01  FILLER                  PIC X(16)  VALUE 'WTRAUTF8-AREA   '.         
013000*01  -COPY WTRAUTF8                                                       
013100                                                                          
013200 01  WS-IDSKYLT-GB           PIC X(3) VALUE 'GB '.                        
013300 01  WS-IDSKYLT-CN           PIC X(3) VALUE 'RCN'.                        
013400*    --- VÄRDE PÅ BLÄDDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN            
013500 01  W-WDJ911KY-MIN.                                                      
013600     03  W-IDDC-MIN-X.                                                    
013700         05  W-IDDC-MIN          PIC X(2)    VALUE SPACE.                 
013800     03  W-DASTADAT-MIN-X.                                                
013900         05  W-DASTADAT-MIN      PIC S9(9)   COMP-3   VALUE ZERO.         
014000     03  W-TISTATID-MIN-X.                                                
014100         05  W-TISTATID-MIN      PIC S9(7)   COMP-3   VALUE ZERO.         
014200     03  W-ADLAGOMR-MIN-X.                                                
014300         05  W-ADLAGOMR-MIN      PIC 9(2)    VALUE ZERO.                  
014400     03  W-ADGANG-MIN-X.                                                  
014500         05  W-ADGANG-MIN        PIC 9(2)    VALUE ZERO.                  
014600     03  W-ADPLATS-MIN-X.                                                 
014700         05  W-ADPLATS-MIN       PIC 9(5)    VALUE ZERO.                  
014800                                                                          
014900 01  NYCKLAR-TILL-DLI.                                                    
015000     03  W-IDARTNR-X.                                                     
015100         05  W-IDARTNR           PIC S9(9)   VALUE ZERO  COMP-3.          
015200     03  W-WDJ911KY-X.                                                    
015300         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
015400         05  W-DASTADAT          PIC S9(9)   COMP-3   VALUE ZERO.         
015500         05  W-TISTATID          PIC S9(7)   COMP-3   VALUE ZERO.         
015600         05  W-ADLAGOMR          PIC 9(2)    VALUE ZERO.                  
015700         05  W-ADGANG            PIC 9(2)    VALUE ZERO.                  
015800         05  W-ADPLATS           PIC 9(5)    VALUE ZERO.                  
015900     03  W-IDSKYLT-X.                                                     
016000         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
           03  W-IDDC-B6-X.                                                     
               05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
016100     SKIP2                                                                
016200                                                                          
016300*    --- STATUS-KOD FRÅN IMS                                              
016400 01  STATUS-WS                   PIC XX.                                  
016500     88  SEGMENT-FINNS                       VALUE '  '.                  
016600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
016700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
016800     SKIP2                                                                
016900 01  GODK-STATUSKODER.                                                    
017000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017100     SKIP3                                                                
017200 01  SSA1                        PIC X(64).                               
017300 01  SSA2                        PIC X(64).                               
017400     EJECT                                                                
017500                                                                          
017600*    --- IMS FUNKTIONSKODER                                               
017700*01  -COPY W0003                                                          
017800     EJECT                                                                
017900                                                                          
018000*    ---  DLI INPUT-OUTPUT AREA                                           
018100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTC01'.                    
018200 01  DLI-IO-WLARTC01.                                                     
018300*    03  -COPY WDK601 -PRE ARTC-                                          
018400     EJECT                                                                
018500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLLOCB01'.                    
018600 01  DLI-IO-WLLOCB01.                                                     
018700*    03  -COPY WDJ901                                                     
018800     EJECT                                                                
018900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLLOCB11'.                    
019000 01  DLI-IO-WLLOCB11.                                                     
019100*    03  -COPY WDJ911                                                     
019200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLBENA11'.                    
019300 01  DLI-IO-WLBENA11.                                                     
019400*    03  -COPY WDD311  -PRE BENA-                                         
019500     EJECT                                                                
       01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
       01   DLI-IO-AREA-B601.                                                   
      *     03  -COPY WDB601                                                    
           EJECT                                                                
019600                                                                          
019700 LINKAGE SECTION.                                                         
019800 01  MSG-PCB                     PIC X.                                   
019900     EJECT                                                                
020000*01  -COPY W0008   -PRE ARTC-                                             
020100      05 FILLER                  PIC X.                                   
020200     EJECT                                                                
020300*01  -COPY W0008   -PRE BENA-                                             
020400     05  FILLER                  PIC X.                                   
020500                                                                          
020600*01  -COPY W0008   -PRE LOCB-                                             
020700     05  FILLER                  PIC X.                                   
020800     EJECT                                                                
      *01  -COPY W0008  -PRE WDB6-                                              
           05  FILLER                  PIC X.                                   
           EJECT                                                                
020900 PROCEDURE DIVISION  USING MSG-PCB ARTC-PCB BENA-PCB LOCB-PCB             
021000                           WDB6-PCB.                                      
021100 MAIN SECTION.                                                            
021200     ENTRY 'DLITCBL' USING MSG-PCB ARTC-PCB BENA-PCB LOCB-PCB             
021300                           WDB6-PCB.                                      
021400                                                                          
021500     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
021600     IF SUB-KDRC = 0                                                      
021700       IF REQU-KDPGMACT = 'S'                                             
021800         PERFORM A-INIT                                                   
021900         PERFORM B-KOLLA-NYCKLAR                                          
022000         IF NYCKLAR-OK                                                    
022100           PERFORM F-LAES-VISA-INFO                                       
022200         END-IF                                                           
022300       ELSE                                                               
022400         MOVE SYS-ERROR    TO RESP-IDMSG-ERROR                            
022500       END-IF                                                             
022600                                                                          
022700*      MOVE RESP-IDMSG-INFO    TO WS-IDMSG-INFO                           
022800*      MOVE RESP-IDMSG-ERROR   TO WS-IDMSG-ERROR                          
022900*      MOVE RESP-IDELMT-ERROR  TO WS-IDELMT-ERROR                         
023000*                                                                         
023100*      IF WS-IDMSG-ERROR NOT = SPACE                                      
023200*        MOVE WS-IDMSG-ERROR   TO RESP-IDMSG-ERROR                        
023300*        MOVE WS-IDELMT-ERROR  TO RESP-IDELMT-ERROR                       
023400*        MOVE WS-IDMSG-INFO    TO RESP-IDMSG-INFO                         
023500*        MOVE 001              TO RESP-IDMSGVER                           
023600*        MOVE ZERO             TO RESP-KVRADER                            
023700*      END-IF                                                             
023800                                                                          
023900       PERFORM S02-RETURN-RESPONSE                                        
024000     END-IF                                                               
024100                                                                          
024200                                                                          
024300     MOVE ZERO TO RETURN-CODE                                             
024400     GOBACK                                                               
024500     .                                                                    
024600     EJECT                                                                
024700 A-INIT SECTION.                                                          
024800                                                                          
024900     MOVE ALL '+'                     TO RESP-AREA                        
025000     MOVE SPACE                       TO RESP-IDMSG-ERROR                 
025100                                         RESP-IDMSG-INFO                  
025200                                         RESP-IDELMT-ERROR                
025300     MOVE 001                         TO RESP-IDMSGVER                    
025400     MOVE ZERO                        TO RESP-KVRADER                     
025500                                         WS-COUNT                         
           MOVE REQU-IDDC-KEY TO W-IDDC-B6                                      
           PERFORM IMS-GU-WDB601                                                
025600     .                                                                    
025700     EJECT                                                                
025800                                                                          
025900 B-KOLLA-NYCKLAR SECTION.                                                 
026000                                                                          
026100     MOVE JA TO NYCKLAR-SW                                                
026200                                                                          
026300*    MOVE 'GB ' TO W-IDSKYLT                                              
026400                                                                          
026500     IF REQU-IDARTNR-KEY NUMERIC                                          
026600       IF REQU-IDARTNR-KEY > ZERO                                         
026700         MOVE REQU-IDARTNR-KEY TO W-IDARTNR                               
026800       END-IF                                                             
026900     ELSE                                                                 
027000       MOVE '023'            TO RESP-IDMSG-ERROR                          
027100       MOVE 'IDARTNR'        TO RESP-IDELMT-ERROR                         
027200       MOVE ALL X'20'        TO RESP-BEART                                
027300       MOVE NEJ TO NYCKLAR-SW                                             
027400     END-IF                                                               
027500                                                                          
027600     IF NYCKLAR-OK                                                        
027700       MOVE REQU-IDARTNR-KEY TO RESP-IDARTNR-KEY                          
027800       INSPECT RESP-IDARTNR-KEY REPLACING LEADING ZERO BY SPACE           
027900     END-IF                                                               
028000                                                                          
028100     MOVE REQU-IDDC-KEY TO W-IDDC                                         
028200                           RESP-IDDC-KEY                                  
028300                           WS-IDDC                                        
028400     INSPECT RESP-IDDC-KEY REPLACING LEADING ZERO BY SPACE                
028500                                                                          
028600*    -- KONTROLL AV KDLOC                                                 
028700                                                                          
028800     IF REQU-KDLOC-KEY NOT = ALL '+'                                      
028900       IF REQU-KDLOC-KEY = 'P'                                            
029000       OR REQU-KDLOC-KEY = 'B'                                            
029100       OR REQU-KDLOC-KEY = 'R'                                            
029200       OR REQU-KDLOC-KEY = 'T'                                            
029300       OR REQU-KDLOC-KEY = 'C'                                            
029400       OR REQU-KDLOC-KEY = 'S'                                            
029500       OR REQU-KDLOC-KEY = SPACE                                          
029600         IF REQU-KDLOC-KEY = SPACE                                        
029700           MOVE 'T'            TO W-KDLOC                                 
029800                                  RESP-KDLOC-KEY                          
029900         ELSE                                                             
030000           MOVE REQU-KDLOC-KEY TO W-KDLOC                                 
030100                                  RESP-KDLOC-KEY                          
030200         END-IF                                                           
030300       ELSE                                                               
030400         MOVE '023'            TO RESP-IDMSG-ERROR                        
030500         MOVE 'KDLOC'          TO RESP-IDELMT-ERROR                       
030600         MOVE NEJ              TO NYCKLAR-SW                              
030700       END-IF                                                             
030800     ELSE                                                                 
030900       MOVE 'T'                TO W-KDLOC                                 
031000                                  RESP-KDLOC-KEY                          
031100     END-IF                                                               
031200     .                                                                    
031300     EJECT                                                                
031400                                                                          
031500 F-LAES-VISA-INFO SECTION.                                                
031600                                                                          
031700     PERFORM IMS-GU-LOCB01                                                
031800     IF SEGMENT-SAKNAS                                                    
031900       MOVE 'IDARTNR'                TO RESP-IDELMT-ERROR                 
032000       MOVE '025'                    TO RESP-IDMSG-ERROR                  
032100*      -- FILL DESCRIPTION WITH UNICODE SPACE                             
032200       MOVE ALL X'20'                TO RESP-BEART                        
032300     ELSE                                                                 
032400       MOVE +1 TO INDX                                                    
032500       PERFORM FA-LAES-LOCB11                                             
032600                                                                          
032700       PERFORM UNTIL INDX > MAX-INDX                                      
032800       OR SEGMENT-SAKNAS                                                  
032900       OR HIST-IDDC > W-IDDC                                              
033000         IF INDX = 1                                                      
033100           MOVE HIST-IDDC            TO W-IDDC                            
033200           MOVE HIST-DASTADAT-9KOMPL TO W-DASTADAT                        
033300           MOVE HIST-TISTATID-9KOMPL TO W-TISTATID                        
033400           MOVE HIST-ADLAGOMR        TO W-ADLAGOMR                        
033500           MOVE HIST-ADGANG          TO W-ADGANG                          
033600           MOVE HIST-ADPLATS         TO W-ADPLATS                         
033700           MOVE W-WDJ911KY-X         TO SPAR-WDJ911KY-ENTER               
033800         END-IF                                                           
033900                                                                          
034000         COMPUTE W-TISTADAT = 99999999 - HIST-DASTADAT-9KOMPL             
034100         MOVE W-TISTADAT (3:6)           TO RESP-TISTADAT   (INDX)        
034200         IF RESP-TISTADAT (INDX) = '000000'                               
034300           MOVE SPACE                    TO RESP-TISTADAT   (INDX)        
034400         END-IF                                                           
034500         MOVE HIST-ADLAGOMR              TO RESP-ADLAGOMR   (INDX)        
034600         MOVE HIST-ADGANG                TO RESP-ADGANG     (INDX)        
034700         MOVE HIST-ADPLATS               TO RESP-ADPLATS    (INDX)        
034800         MOVE HIST-KDLOC                 TO RESP-KDLOC      (INDX)        
034900         MOVE HIST-DASTODAT              TO W-TISTODAT                    
035000         MOVE W-TISTODAT (3:6)           TO RESP-TISTODAT   (INDX)        
035100         IF RESP-TISTODAT (INDX) = '000000'                               
035200           MOVE SPACE                    TO RESP-TISTODAT   (INDX)        
035300         END-IF                                                           
035400         MOVE HIST-IDUSER                TO RESP-IDUSER-STA (INDX)        
035500         MOVE HIST-IDUSER-STO            TO RESP-IDUSER-STO (INDX)        
035600         ADD +1                          TO WS-COUNT                      
035700         PERFORM FA-LAES-LOCB11                                           
035800                                                                          
035900         IF WS-COUNT < 501                                                
036000           CONTINUE                                                       
036100         ELSE                                                             
036200           MOVE '028'           TO RESP-IDMSG-ERROR                       
036300         END-IF                                                           
036400                                                                          
036500         MOVE WS-COUNT          TO RESP-KVRADER                           
036600                                                                          
036700         ADD +1 TO INDX                                                   
036800       END-PERFORM                                                        
036900                                                                          
037000       IF SEGMENT-FINNS AND HIST-IDDC = W-IDDC                            
037100         MOVE HIST-IDDC                  TO W-IDDC                        
037200         MOVE HIST-DASTADAT-9KOMPL       TO W-DASTADAT                    
037300         MOVE HIST-TISTATID-9KOMPL       TO W-TISTATID                    
037400         MOVE HIST-ADLAGOMR              TO W-ADLAGOMR                    
037500         MOVE HIST-ADGANG                TO W-ADGANG                      
037600         MOVE HIST-ADPLATS               TO W-ADPLATS                     
037700         MOVE W-WDJ911KY-X               TO SPAR-WDJ911KY-NEXT            
037800       END-IF                                                             
037900       IF INDX = 1                                                        
038000         MOVE '025'                 TO RESP-IDMSG-ERROR                   
038100         MOVE ERR-PARTHIST-MISSING  TO RESP-IDELMT-ERROR                  
038200       END-IF                                                             
038300                                                                          
038400*      -- READ CHINESE OR ENGLISH BEART                                   
038500*      -- ENGLISH WILL BE TRANSLATED TO UNICODE                           
             MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                            
             IF DCS-UNICODE-IDSKYLT                                             
               MOVE 'UTF8'             TO TRAUTF8-KDCP                          
             ELSE                                                               
               MOVE '278 '             TO TRAUTF8-KDCP                          
             END-IF                                                             
039300                                                                          
039400       PERFORM IMS-GU-BENA01-BSEQ                                         
039500       IF SEGMENT-FINNS                                                   
039600         MOVE BENA-TEXT-BEART TO TRAUTF8-TECONV-FROM                      
039700       ELSE                                                               
039800         MOVE SPACE           TO TRAUTF8-TECONV-FROM                      
039900       END-IF                                                             
             IF TRAUTF8-TECONV-FROM = SPACES                                    
              MOVE 'GB'  TO W-IDSKYLT                                           
              MOVE '278' TO TRAUTF8-KDCP                                        
              PERFORM IMS-GU-BENA01-BSEQ                                        
              MOVE BENA-TEXT-BEART    TO TRAUTF8-TECONV-FROM                    
             END-IF                                                             
040000                                                                          
040100*       -- STRIP SPACE OR CONVERT TO UNICODE                              
040200        CALL WTRAUTF8 USING TRAUTF8-AREA                                  
040300                                                                          
040400*       -- MOVE CONVERTED DESCRIPTION TO THE RESPONSE                     
040500        MOVE TRAUTF8-TECONV-TO   TO RESP-BEART                            
040600                                                                          
040700       PERFORM IMS-GU-K601                                                
040800       IF SEGMENT-FINNS                                                   
040900         CONTINUE                                                         
041000       END-IF                                                             
041100     END-IF                                                               
041200     .                                                                    
041300     EJECT                                                                
041400                                                                          
041500 FA-LAES-LOCB11 SECTION.                                                  
041600                                                                          
041700     PERFORM IMS-GNP-LOCB11                                               
041800                                                                          
041900     PERFORM UNTIL                                                        
042000         SEGMENT-SAKNAS                                                   
042100     OR  HIST-IDDC > W-IDDC                                               
042200     OR  HIST-KDLOC = W-KDLOC                                             
042300     OR  W-KDLOC = 'T'                                                    
042400     OR (W-KDLOC = 'R' AND  HIST-KDLOC = 'B')                             
042500     OR (W-KDLOC = 'P' AND (HIST-KDLOC = 'C' OR HIST-KDLOC = 'S'))        
042600                                                                          
042700       PERFORM IMS-GNP-LOCB11                                             
042800     END-PERFORM                                                          
042900     .                                                                    
043000     EJECT                                                                
043100*    --- DISPATCHER SECTIONS                                              
043200 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
043300                                                                          
043400     MOVE 'GETARG'               TO SUB-KDFUNC                            
043500     MOVE 'CARPARTS.LDC.PARTLOCATIONHIST'   TO SUB-ADDISPABS              
043600     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
043700                                                                          
043800     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
043900                                                                          
044000     IF SUB-KDRC > 0                                                      
044100       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
044200       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
044300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
044400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
044500     END-IF                                                               
044600     .                                                                    
044700     SKIP3                                                                
044800 S02-RETURN-RESPONSE SECTION.                                             
044900                                                                          
045000     MOVE 'RETURN'                   TO SUB-KDFUNC                        
045100     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
045200                                                                          
045300     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
045400                                                                          
045500     IF SUB-KDRC > 0                                                      
045600       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
045700       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
045800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
045900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
046000     END-IF                                                               
046100     .                                                                    
046200     EJECT                                                                
046300 IMS-GU-K601 SECTION.                                                     
046400                                                                          
046500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
046600             DELIMITED BY SIZE INTO SSA1                                  
046700     MOVE '  GE' TO GODK-STATUSKODER                                      
046800     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WLARTC01 SSA1                  
046900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
047000     PERFORM IMS-STATUSKONTROLL                                           
047100     SKIP3                                                                
047200     .                                                                    
047300                                                                          
047400 IMS-GU-LOCB01 SECTION.                                                   
047500                                                                          
047600     STRING 'WLLOCB01(IDARTNR  =' W-IDARTNR-X ')'                         
047700          DELIMITED BY SIZE INTO SSA1                                     
047800     MOVE '  GE' TO GODK-STATUSKODER                                      
047900     CALL CBLTDLI USING GU LOCB-PCB DLI-IO-WLLOCB01 SSA1                  
048000     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
048100     PERFORM IMS-STATUSKONTROLL                                           
048200     .                                                                    
048300     EJECT                                                                
048400                                                                          
048500 IMS-GNP-LOCB11 SECTION.                                                  
048600                                                                          
048700     STRING 'WLLOCB11(WDJ911KY=>' W-WDJ911KY-X ')'                        
048800          DELIMITED BY SIZE INTO SSA1                                     
048900     MOVE '  GE' TO GODK-STATUSKODER                                      
049000     CALL CBLTDLI USING GNP LOCB-PCB DLI-IO-WLLOCB11 SSA1                 
049100     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
049200     PERFORM IMS-STATUSKONTROLL                                           
049300     .                                                                    
049400     EJECT                                                                
049500                                                                          
049600 IMS-GU-BENA01-BSEQ SECTION.                                              
049700                                                                          
049800     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
049900          DELIMITED BY SIZE INTO SSA1                                     
050000     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
050100          DELIMITED BY SIZE INTO SSA2                                     
050200     MOVE '  GE'               TO GODK-STATUSKODER                        
050300     CALL CBLTDLI USING GU BENA-PCB DLI-IO-WLBENA11 SSA1 SSA2             
050400     MOVE BENA-STATUS-CODE     TO STATUS-WS                               
050500     PERFORM IMS-STATUSKONTROLL                                           
050600     .                                                                    
050700     EJECT                                                                
       IMS-GU-WDB601 SECTION.                                                   
                                                                                
           STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
                DELIMITED BY SIZE INTO SSA1                                     
           MOVE '  ' TO GODK-STATUSKODER                                        
           CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
           MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
           PERFORM IMS-STATUSKONTROLL                                           
           .                                                                    
           SKIP3                                                                
050800                                                                          
050900 IMS-STATUSKONTROLL SECTION.                                              
051000                                                                          
051100     SET STATUS-IX TO 1                                                   
051200     SEARCH GODK-STATUS                                                   
051300       AT END                                                             
051400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
051500         DELIMITED BY SIZE INTO FELTEXT                                   
051600         CALL FELLOG                                                      
051700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
051800         CONTINUE                                                         
051900     END-SEARCH                                                           
052000     .                                                                    
