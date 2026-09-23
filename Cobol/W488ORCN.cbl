000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W488ORCN.                                                
000500 AUTHOR.         SRINADH NADIMPALLI.                                      
000600 DATE-WRITTEN.   MAJ 2023.                                                
000700 DATE-COMPILED.                                                           
000800                                                                          
000900*    FUNCTION.                                                            
001000*                                                                         
001100*        W488ORCN SENDS EVENT TRANSACTION TO SYNQ VIA AN API.             
001200*                 ORDER cancel INFO ARE SENT.                             
001300*                                                                         
001400*    LÄNKAREA :       W488ORCN                                            
001500*                                                                         
001600*                                                                         
001700 DATA DIVISION.                                                           
001800                                                                          
001900 WORKING-STORAGE SECTION.                                                 
002000     SKIP3                                                                
002100 77    IDPGM                     PIC X(8)   VALUE 'W488ORCN'.             
002200 77    ERRORTEXT                 PIC X(64)  VALUE SPACE.                  
002300 77    CURRENT-SECTION           PIC X(30)  VALUE SPACE.                  
002400 77    YES                       PIC X      VALUE 'Y'.                    
002500 77    NEJ                       PIC X      VALUE 'N'.                    
       01    WS-NILPICK                PIC X      VALUE 'Y'.                    
002600 77    KDRC-DISPLAY              PIC Z(5).                                
002700 77    RKOD-ABEND                PIC S9(4)  VALUE +33   COMP SYNC.        
002800 77    IMS-SECTION               PIC X(30)  VALUE SPACE.                  
002900 77    WS-BEART                  PIC X(25).                               
003000 77    WS-KDSORT                 PIC X(2).                                
003100 77    WS-VLARTNTO               PIC 9(8)V9(1).                           
003200 77    WS-HOUR-NUM               PIC 9(2).                                
003300 01    API-RESPONSE                PIC X(9999).                           
003400 01    WS-TIMESTAMP1 PIC X(21).                                           
003500 01    WS-TIMESTAMP.                                                      
003600       03 WS-YYYY PIC X(4).                                               
003700       03 WS-HYPHEN PIC X(1) VALUE '-'.                                   
003800       03 WS-MM   PIC X(2).                                               
003900       03 WS-HYPHEN PIC X(1) VALUE '-'.                                   
004000       03 WS-DD   PIC X(2).                                               
004100       03 WS-TIME PIC X(1) VALUE 'T'.                                     
004200       03 WS-HOUR PIC X(2).                                               
004300       03 WS-COLON PIC X(1) VALUE ':'.                                    
004400       03 WS-MIN  PIC X(2).                                               
004500       03 WS-COLON PIC X(1) VALUE ':'.                                    
004600       03 WS-SEC PIC X(2).                                                
004700       03 WS-DOT PIC X(1) VALUE '.'.                                      
004800       03 WS-MSEC PIC X(3) VALUE '000'.                                   
004900       03 WS-TIDZON PIC X(3).                                             
005000       03 WS-TIDZON1 PIC X(3) VALUE ':00'.                                
005100*                                                                         
005200 01    WS-ORDERID.                                                        
005300       03 WS-IDDC   PIC X(2).                                             
005400       03 WS-IDARTNR PIC 9(9).                                            
005500       03 WS-TIORDTIME PIC 9(12).                                         
005600                                                                          
005700 01    WS-IDARTNR-NUM PIC Z(8)9.                                          
005800                                                                          
005900 01    WS-ORDERID-PCK.                                                    
006000       03 WS-TIRFSDAT PIC 9(6).                                           
006100       03 WS-IDPRODNR PIC 9(7).                                           
006200       03 WS-IDRADNR  PIC 9(4).                                           
007200 01  DYNAMIC-SUBPROGRAM.                                                  
007300     03 CBLTDLI                  PIC X(8)   VALUE 'CBLTDLI '.             
007400     03 FELLOG                   PIC X(8)   VALUE 'FELLOG  '.             
007500     03 ABEND                    PIC X(8)   VALUE 'ABEND   '.             
007600     03 WZ01SEND                 PIC X(8)   VALUE 'WZ01SEND'.             
007700     03 WZ01CALL                 PIC X(8)   VALUE 'WZ01CALL'.             
007800     SKIP2                                                                
007900*    --- PARAMETERS TO ABEND                                              
008000 77  RKOD-ABEND                  PIC S9(4)  COMP VALUE +0.                
008100 77  RKOD-ABEND-NO-DUMP          PIC S9(4)  COMP VALUE +16.               
008200 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)  COMP VALUE +1000.             
008300     EJECT                                                                
008400*                                                                         
008500*    --- PARAMETERS TO WZ01SEND FOR SYNQ                                  
008600 01  FILLER                     PIC X(16) VALUE 'WZ01SEND-SYNQ'.          
008700     SKIP3                                                                
008800*01  -COPY WZ01SEND                                                       
008900*    --- PARAMETERS TO WZ01CALL                                           
009000 01  FILLER                     PIC X(16) VALUE 'WZ01CALL'.               
009100*01  -COPY WZ01CALL                                                       
009200                                                                          
009300*API INFO FILE                                                            
009400*    01  -COPY WAPIINFO                                                   
009500                                                                          
009600*01  FILLER                     PIC X(16) VALUE 'SENDAREA-SYNQ'.          
009700*                                                                         
009800 01  WSYNQ01-AREA.                                                        
009900*    03  -COPY WSY02Q01                                                   
010000*                                                                         
010100     EJECT                                                                
010200***************************************************************           
010300 01    NYCKLAR-TILL-DLI.                                                  
010400*                                                                         
010500     03 W-WDGXKEY-0103-X.                                                 
010600        05  W-IDHTYP-0103       PIC X(4)    VALUE '0103'.                 
010700        05  FILLER              PIC X(26)   VALUE LOW-VALUE.              
010800     03 W-KY0104-X.                                                       
010900        05  W-ADDISPABS         PIC X(50)                                 
011000                             VALUE 'APIOUT.SYNQ.ORDERDELETE'.             
011100     03  W-WDQ3DSEQ-X.                                                    
011200         05  W-Q3DSEQ-IDPRODNR       PIC S9(7)  COMP-3.                   
011300         05  W-Q3DSEQ-IDPLKLST       PIC S9(3)  COMP-3.                   
011400*                                                                         
011500*    WS-AREAS TO IMS-SECTIONS                                             
011600*                                                                         
011700 01  IMS-WS.                                                              
011800   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
011900   03    STATUS-WS               PIC XX.                                  
012000     88    SEGMENT-FOUND                     VALUE '  '.                  
012100     88    SEGMENT-MISSING                   VALUE 'GE'.                  
012200     88    SEGMENT-END                       VALUE 'GB'.                  
012300     SKIP3                                                                
012400   03    GODK-STATUSKODER.                                                
012500     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
012600     SKIP3                                                                
012700 01    SSA1                      PIC X(128).                              
012800 01    SSA2                      PIC X(128).                              
012900     EJECT                                                                
013000*                            IMS FUNKTIONSKODER                           
013100*01  -COPY W0003                                                          
013200     EJECT                                                                
013300*    ---  DLI INPUT-OUTPUT AREA                                           
013400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR501'.                      
013500 01  DLI-IO-WDR501.                                                       
013600*    03   -COPY WDGX01                                                    
013700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX0104'.                    
013800 01  DLI-IO-WDGX0104.                                                     
013900*    03   -COPY WDGX0104                                                  
014000 01  DLI-IO-WDQ301.                                                       
014100*    03   -COPY WDQ301                                                    
014200     EJECT                                                                
014300                                                                          
014400                                                                          
014500 LINKAGE SECTION.                                                         
014600*                                                                         
014700*    -COPY W488ORCN                                                       
014800     EJECT                                                                
014900*01    -COPY W0009     -PRE SYNQ-                                         
015000     EJECT                                                                
015100*01    -COPY W0008     -PRE SYNQ-ATAB-                                    
015200     05  FILLER                  PIC X.                                   
015300*01  -COPY W0008      -PRE WDQ3-                                          
015400     05  FILLER                  PIC X.                                   
015500     EJECT                                                                
015600                                                                          
015700 PROCEDURE DIVISION USING SYNQC-W488ORCN SYNQ-PCB SYNQ-ATAB-PCB           
015800                          WDQ3-PCB.                                       
015900 MAIN SECTION.                                                            
016000                                                                          
016100     PERFORM A-INIT                                                       
016200     PERFORM B-FILL-SENDAREA                                              
016300     IF SYNQC-ORDERTYPE = 'RPL'                                           
016600     PERFORM D-SEND-SYNQ                                                  
016700     END-IF                                                               
           IF SYNQC-ORDERTYPE = 'PCK' AND                                       
              WS-NILPICK = 'Y'                                                  
           PERFORM C-SEND-SYNQ                                                  
           END-IF                                                               
016800     MOVE ZERO TO RETURN-CODE                                             
016900     GOBACK                                                               
017000     .                                                                    
017100     EJECT                                                                
017200 A-INIT SECTION.                                                          
017300     MOVE FUNCTION CURRENT-DATE TO WS-TIMESTAMP1                          
017400                                                                          
017500     MOVE FUNCTION CURRENT-DATE(1:4) TO WS-YYYY                           
017600     MOVE FUNCTION CURRENT-DATE(5:2) TO WS-MM                             
017700     MOVE FUNCTION CURRENT-DATE(7:2) TO WS-DD                             
017800     MOVE FUNCTION CURRENT-DATE(9:2) TO WS-HOUR                           
017900     MOVE FUNCTION CURRENT-DATE(11:2) TO WS-MIN                           
018000     MOVE FUNCTION CURRENT-DATE(13:2) TO WS-SEC                           
018100*    MOVE FUNCTION CURRENT-DATE(15:2) TO WS-MSEC                          
018200     MOVE FUNCTION CURRENT-DATE(17:3) TO WS-TIDZON                        
021600     MOVE 'Volvo'    TO XOWNER                                            
021700     MOVE 5          TO XOWNER-LENGTH                                     
018300                                                                          
018800     .                                                                    
018900     SKIP2                                                                
019000 B-FILL-SENDAREA SECTION.                                                 
019100     MOVE 'B-FILL-SENDAREA'       TO CURRENT-SECTION                      
           IF SYNQC-ORDERTYPE = 'RPL'                                           
018400      MOVE SYNQC-IDDC TO WS-IDDC                                          
018500      MOVE SYNQC-IDARTNR TO WS-IDARTNR                                    
018600                           WS-IDARTNR-NUM                                 
018700      MOVE SYNQC-TIORDTIME TO WS-TIORDTIME                                
020200      MOVE FUNCTION TRIM(WS-ORDERID) TO ORDERID                           
020300      COMPUTE ORDERID-LENGTH       =                                      
020400              FUNCTION BYTE-LENGTH (                                      
020500              FUNCTION TRIM(ORDERID))                                     
           END-IF                                                               
024100     IF SYNQC-ORDERTYPE = 'PCK'                                           
             MOVE NEJ TO WS-NILPICK                                             
024700       MOVE SYNQC-IDPRODNR TO WS-IDPRODNR                                 
024900                             W-Q3DSEQ-IDPRODNR                            
             MOVE SYNQC-IDPLKLST TO W-Q3DSEQ-IDPLKLST                           
025000       MOVE SYNQC-IDRADNR  TO WS-IDRADNR                                  
             PERFORM IMS-GU-WDQ3DSEQ                                            
             IF SEGMENT-FOUND                                                   
               IF ODEL-IDDC = '11'                                              
                 CONTINUE                                                       
               ELSE                                                             
                 PERFORM IMS-GN-WDQ3DSEQ                                        
               END-IF                                                           
             END-IF                                                             
             IF SEGMENT-FOUND                                                   
             IF (ODEL-IDPRC NOT = '991Q' AND '9920')                            
              MOVE YES TO WS-NILPICK                                            
              MOVE ODEL-DARFS(3:6) TO WS-TIRFSDAT                               
028800        MOVE FUNCTION TRIM(WS-ORDERID-PCK) TO ORDERID                     
028900                                                                          
029000        COMPUTE ORDERID-LENGTH  =                                         
029100             FUNCTION BYTE-LENGTH (                                       
029200             FUNCTION TRIM(ORDERID))                                      
             END-IF                                                             
             END-IF                                                             
031800     END-IF                                                               
031900     .                                                                    
032000     EJECT                                                                
032100 C-SEND-SYNQ SECTION.                                                     
032200     MOVE 'C-SEND-SYNQ'           TO CURRENT-SECTION                      
032300                                                                          
032400     PERFORM CA-SEND-OPEN                                                 
032500     PERFORM CB-SEND-PUT-HEADER                                           
032600     PERFORM CC-SEND-PUT-LINE                                             
032700     PERFORM CD-SEND-CLOSE                                                
032800                                                                          
032900     .                                                                    
033000     EJECT                                                                
033100                                                                          
033200 CA-SEND-OPEN SECTION.                                                    
033300     MOVE 'CA-SEND-OPEN'          TO CURRENT-SECTION                      
033400                                                                          
033500                                                                          
033600     MOVE W-ADDISPABS             TO SEND-ADDISPABS                       
033700     MOVE 'OPEN'                  TO SEND-KDFUNC                          
033800     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
033900                                     SEND-OPEN-AREA                       
034000     IF SEND-KDRC > ZERO                                                  
034100       MOVE SEND-KDRC             TO KDRC-DISPLAY                         
034200       STRING 'WZ01SEND OPEN ERROR RC= ' KDRC-DISPLAY                     
034300       DELIMITED BY SIZE INTO ERRORTEXT                                   
034400       CALL FELLOG                                                        
034500*    ELSE                                                                 
034600*      MOVE SEND-IDCOM            TO WS-SEND-IDCOM                        
034700     END-IF                                                               
034800                                                                          
034900     .                                                                    
035000     SKIP2                                                                
035100 CB-SEND-PUT-HEADER SECTION.                                              
035200     MOVE 'CB-SEND-PUT-HEADER'    TO CURRENT-SECTION                      
035300                                                                          
035400     PERFORM IMS-GU-WDGX0104                                              
035500     IF SEGMENT-FOUND                                                     
035600       MOVE 0104-IDAPI             TO IDAPI                               
035700       COMPUTE IDAPI-LEN       = FUNCTION BYTE-LENGTH (                   
035800                                 FUNCTION TRIM (IDAPI))                   
035900       MOVE 0104-IDPATH-API        TO IDPATH-API                          
036000       COMPUTE IDPATH-API-LEN  = FUNCTION BYTE-LENGTH (                   
036100                                 FUNCTION TRIM (IDPATH-API))              
036200       MOVE 0104-IDPTYP-API        TO IDPTYP-API                          
036300       COMPUTE IDPTYP-API-LEN  = FUNCTION BYTE-LENGTH (                   
036400                                 FUNCTION TRIM (IDPTYP-API))              
036500       MOVE 0104-IDAPPKEY         TO USER-KEY                             
036600       COMPUTE USER-KEY-LENGTH = FUNCTION BYTE-LENGTH (                   
036700                                 FUNCTION TRIM (USER-KEY))                
036800     ELSE                                                                 
036900       CALL FELLOG                                                        
037000     END-IF                                                               
037100                                                                          
037200     MOVE 'PUT'                   TO SEND-KDFUNC                          
037300*    MOVE WS-SEND-IDCOM           TO SEND-IDCOM                           
037400     MOVE LENGTH OF WAPIINFO      TO SEND-KVDLEN                          
037500     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
037600                                     SEND-KVDLEN                          
037700                                     WAPIINFO                             
037800     IF SEND-KDRC > ZERO                                                  
037900       MOVE SEND-KDRC             TO KDRC-DISPLAY                         
038000       STRING 'WZ01SEND PUT HEADER ERROR RC= ' KDRC-DISPLAY               
038100              DELIMITED BY SIZE INTO ERRORTEXT                            
038200       CALL FELLOG                                                        
038300     END-IF                                                               
038400     .                                                                    
038500                                                                          
038600 CC-SEND-PUT-LINE SECTION.                                                
038700     MOVE 'DC-SEND-PUT-LINE'      TO CURRENT-SECTION                      
038800                                                                          
038900     MOVE 'PUT'                   TO SEND-KDFUNC                          
039000*    MOVE WS-SEND-IDCOM           TO SEND-IDCOM                           
039100     MOVE LENGTH OF WSYNQ01-AREA  TO SEND-KVDLEN                          
039200     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
039300                                     SEND-KVDLEN                          
039400                                     WSYNQ01-AREA                         
039500                                                                          
039600     IF SEND-KDRC > ZERO                                                  
039700       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
039800       STRING 'WZ01SEND PUT LINE ERROR RC= ' KDRC-DISPLAY                 
039900       DELIMITED BY SIZE       INTO ERRORTEXT                             
040000       CALL FELLOG                                                        
040100     END-IF                                                               
040200     .                                                                    
040300     SKIP2                                                                
040400                                                                          
040500 CD-SEND-CLOSE SECTION.                                                   
040600     MOVE 'CD-SEND-CLOSE'        TO CURRENT-SECTION                       
040700                                                                          
040800     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
040900*    MOVE WS-SEND-IDCOM           TO SEND-IDCOM                           
041000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
041100                                                                          
041200     IF SEND-KDRC > 0                                                     
041300       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
041400       STRING 'WZ01SEND CLOSE ERROR RC= ' KDRC-DISPLAY                    
041500       DELIMITED BY SIZE INTO ERRORTEXT                                   
041600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
041700     END-IF                                                               
041800     .                                                                    
041900     EJECT                                                                
042000 D-SEND-SYNQ SECTION.                                                     
042100                                                                          
042200     PERFORM IMS-GU-WDGX0104                                              
042300     IF SEGMENT-FOUND                                                     
042400       MOVE 0104-IDAPI           TO CALL-IDAPI                            
042500       COMPUTE CALL-IDAPI-LEN  = FUNCTION BYTE-LENGTH (                   
042600                                 FUNCTION TRIM (CALL-IDAPI))              
042700       MOVE 0104-IDPATH-API      TO CALL-IDPATH-API                       
042800       COMPUTE CALL-IDPATH-API-LEN = FUNCTION BYTE-LENGTH (               
042900                                 FUNCTION TRIM (CALL-IDPATH-API))         
043000       MOVE 0104-IDPTYP-API      TO CALL-IDPTYP-API                       
043100       COMPUTE CALL-IDPTYP-API-LEN = FUNCTION BYTE-LENGTH (               
043200                                 FUNCTION TRIM (CALL-IDPTYP-API))         
043300       MOVE 0104-IDAPPKEY        TO USER-KEY                              
043400       COMPUTE USER-KEY-LENGTH = FUNCTION BYTE-LENGTH (                   
043500                                 FUNCTION TRIM (USER-KEY))                
043600     ELSE                                                                 
043700       CALL FELLOG                                                        
043800     END-IF                                                               
043900                                                                          
044000     MOVE W-ADDISPABS            TO CALL-ADDISPABS                        
044100     MOVE LENGTH OF WSYNQ01-AREA TO CALL-KVDLEN-IN                        
044200                                                                          
044300     CALL WZ01CALL            USING CALL-CONTROL-AREA                     
044400                                    CALL-KVDLEN-IN                        
044500                                    WSYNQ01-AREA                          
044600                                    CALL-KVDLEN-OUT                       
044700                                    API-RESPONSE                          
044800     IF CALL-KDRC = 0                                                     
044900      MOVE SPACES TO SYNQC-KDSVAR                                         
045000     ELSE                                                                 
045100      MOVE 'F' TO SYNQC-KDSVAR                                            
045200     END-IF                                                               
045300     .                                                                    
045400*                                                                         
045500*IMS SECTIONS                                                             
045600*                                                                         
045700 IMS-GU-WDGX0104  SECTION.                                                
045800     MOVE 'IMS-GU-WDGX0104    '   TO IMS-SECTION                          
045900                                                                          
046000     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-0103-X ')'                    
046100             DELIMITED BY SIZE INTO SSA1                                  
046200     STRING 'WDGX0104(ADDISPAB =' W-KY0104-X ')'                          
046300             DELIMITED BY SIZE INTO SSA2                                  
046400     MOVE '  GE'                 TO GODK-STATUSKODER                      
046500     CALL CBLTDLI USING GU SYNQ-ATAB-PCB DLI-IO-WDGX0104 SSA1 SSA2        
046600     MOVE SYNQ-ATAB-STATUS-CODE       TO STATUS-WS                        
046700     PERFORM IMS-STATUSCHECK                                              
046800     .                                                                    
046900     SKIP3                                                                
047000 IMS-GU-WDQ3DSEQ SECTION.                                                 
047100                                                                          
047200     STRING 'WDQ301  (WDQ3DSEQ =' W-WDQ3DSEQ-X ')'                        
047300          DELIMITED BY SIZE INTO SSA1                                     
047400     MOVE '  GE' TO GODK-STATUSKODER                                      
047500     CALL CBLTDLI USING GU WDQ3-PCB DLI-IO-WDQ301 SSA1                    
047600     MOVE WDQ3-STATUS-CODE TO STATUS-WS                                   
047700     PERFORM IMS-STATUSCHECK                                              
047800     .                                                                    
047900                                                                          
048000 IMS-GN-WDQ3DSEQ SECTION.                                                 
048100                                                                          
048200     STRING 'WDQ301  (WDQ3DSEQ =' W-WDQ3DSEQ-X ')'                        
048300          DELIMITED BY SIZE INTO SSA1                                     
048400     MOVE '  GE' TO GODK-STATUSKODER                                      
048500     CALL CBLTDLI USING GN WDQ3-PCB DLI-IO-WDQ301 SSA1                    
048600     MOVE WDQ3-STATUS-CODE TO STATUS-WS                                   
048700     PERFORM IMS-STATUSCHECK                                              
048800     .                                                                    
048900 IMS-STATUSCHECK SECTION.                                                 
049000     SET STATUS-IX TO 1                                                   
049100     SEARCH GODK-STATUS AT END CALL FELLOG                                
049200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
049300     END-SEARCH                                                           
049400     CONTINUE                                                             
049500     .                                                                    
