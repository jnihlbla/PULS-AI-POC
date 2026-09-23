000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4W34300.                                                
000300 AUTHOR.         RAHUL REDDY.                                             
000400 DATE-WRITTEN.   12/12/10.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       CARPARTS.NDC.MOVELINESINCASE                             
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        THIS IS A DRIVER PGM FOR TRANSACTION W4W343T                     
001100*                                                                         
001200*        IT TAKES CARE OF TECHNICAL DETAILS RELATED BEING CALLED          
001300*        VIA IMS-CONNECT AND CALLS SUBPROGRAM W4034310 WHICH              
001400*        CONTAINS ALL BUSINESS LOGIC FOR THIS TRANSACTION.                
001500*                                                                         
001600*        A SIMILAR DRIVER PROGRAM FOR INVOCATIONS FROM A 3270 TER-        
001700*        MINAL EXISTS - W4034300 (TRANSACTION W4T343)                     
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSACTION: W4W343T                                             
002100*        REQUEST:     W40343I1 / W40343I2                                 
002200*                                                                         
002300*    OUTDATA.                                                             
002400*        RESPONSE:    W40343O1                                            
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP3                                                                
003500 FILE SECTION.                                                            
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800 77  IDPGM                       PIC X(08)   VALUE 'W4W34300'.            
003900                                                                          
004000*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004100 77  FILLER                      PIC X(08) VALUE 'ERRORTEX'.              
004200 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004300 77  KDRC-DISPLAY                PIC Z(5).                                
004400                                                                          
004500 77  MAX-KVRADER                 PIC S9(4)   VALUE +500 COMP.             
004510 77  WS-KVRADER                  PIC S9(4)   VALUE +0   COMP.             
004520 77  WS-KDTRANS                  PIC X(6)  VALUE SPACE.                   
004600                                                                          
004700 01  WORK-VARIABLES.                                                      
004800     03 MSG-IX                   PIC S9(9)  VALUE +0   COMP SYNC.         
004900     03 INX-IX                   PIC S9(9)  VALUE +0   COMP SYNC.         
005000     03 LINE-MSG-SW              PIC X.                                   
005100       88 LINE-MSG-FND                 VALUE 'Y'.                         
005200                                                                          
005300 01  ALL-PLUS.                                                            
005400     03  FILLER                  PIC X(80)  VALUE ALL '+'.                
005500                                                                          
005600 01  MESSAGE-CODES.                                                       
005700     03  ERR-UNAUTHORIZED        PIC X(3)    VALUE '00A'.                 
005800     03  ERR-CASE-MISSING        PIC X(3)    VALUE '041'.                 
005900     03  ERR-CASE-INVOICED       PIC X(3)    VALUE '255'.                 
006000                                                                          
006100     EJECT                                                                
006200*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006300 01  GENERAL-SUBPROGRAMS.                                                 
006400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006500     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
006600     03  W4034310                PIC X(8)    VALUE 'W4034310'.            
006700     03  WZ01AUTH                PIC X(8)    VALUE 'WZ01AUTH'.            
006800     03  WMSGCONV                PIC X(8)    VALUE 'WMSGCONV'.            
006900     SKIP3                                                                
007000*    --- PARAMETERS TO ABEND                                              
007100                                                                          
007200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007300 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007400 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007500     EJECT                                                                
007600*                                                                         
007700 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
007800     SKIP3                                                                
007900*01  -COPY WZ01SUB                                                        
008000     EJECT                                                                
008100 01  FILLER                      PIC X(16)   VALUE 'WZ01AUTH   '.         
008200*01  -COPY WZ01AUTH                                                       
008300*                                                                         
008400 01  FILLER                      PIC X(16)   VALUE 'WMSGCONV'.            
008500*01  -COPY WMSGCONV                                                       
008600                                                                          
008700     SKIP3                                                                
008800*    REQU-IN-AREA is used to receive the data from IMS. It has the        
008900*    different version of input layouts W40343I1, W40343I2 etc            
009000*    with -L to be able to have the max length possible.                  
009100                                                                          
009200*    REQU-IN-AREA must be moved to REQU-AREA accordingly before           
009300*    calling the business module.                                         
009400                                                                          
009500 01  FILLER                      PIC X(16)   VALUE 'REQU-IN-AREA'.        
009600     SKIP3                                                                
009700 01  REQU-IN-AREA.                                                        
009800*    03  -COPY WZ01REQ2                                                   
009900     03  REQU-DATA-AREA.                                                  
010000*        05  -COPY W40343I1 -L                                            
010100*        05  -COPY W40343I2 -L                                            
010200                                                                          
010300 01  FILLER                      PIC X(16)   VALUE 'I1-AREA'.             
010400*    -COPY W40343I1                                                       
010500                                                                          
010600 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
010700 01  REQU-AREA.                                                           
010800*    03  -COPY WZ01REQ2                                                   
010900*    03  -COPY W40343I2                                                   
011000                                                                          
011100 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
011200 01  RESP-AREA.                                                           
011300*    03  -COPY WZ01RES2                                                   
011400*    03  -COPY W40343O1                                                   
011500                                                                          
011600 LINKAGE SECTION.                                                         
011700                                                                          
011800*01  -COPY W0009   -PRE MSG-                                              
011900 01  TMS-CRE-PCB                 PIC X.                                   
011910 01  TMS-DEL-PCB                 PIC X.                                   
012000 01  ATAB-PCB                    PIC X.                                   
012100 01  WDP7-PCB                    PIC X.                                   
012200 01  WDE6-PCB                    PIC X.                                   
012300 01  WDE4FSEQ-PCB                PIC X.                                   
012400 01  WDE4-PCB                    PIC X.                                   
012500 01  WDE4F-PCB                   PIC X.                                   
012600 01  WDQ5A-PCB                   PIC X.                                   
012700 01  WDB6-PCB                    PIC X.                                   
012800 01  PLATS-DM-PCB                PIC X.                                   
012900 01  PLATS-DN-PCB                PIC X.                                   
013000 01  PLATS-DP-PCB                PIC X.                                   
013100 01  PLATS-DO-PCB                PIC X.                                   
013200 01  PLATS-WDE6C-PCB             PIC X.                                   
013300 01  PLATS-GMTC-PCB              PIC X.                                   
013400 01  PLATS-WDB6-PCB              PIC X.                                   
013500 01  DNOT-ORQP-PCB               PIC X.                                   
013600 01  DNOT-ORQP2-PCB              PIC X.                                   
013700 01  DNOT-ORQP3-PCB              PIC X.                                   
013800 01  DNOT-4013-PCB               PIC X.                                   
013900 01  DNOT-BENA-PCB               PIC X.                                   
014000 01  TMS-1165-PCB                PIC X.                                   
014100 01  TMS-4141-PCB                PIC X.                                   
014200 01  TMS-WDB2-PCB                PIC X.                                   
014300 01  TMS-WDB6-PCB                PIC X.                                   
014400 01  TMS-WDD3-PCB                PIC X.                                   
014500 01  TMS-WDB1-PCB                PIC X.                                   
014600 01  TMS-WDE4A-PCB               PIC X.                                   
014700 01  TMS-WDE4F-PCB               PIC X.                                   
014800 01  TMS-WDQ2-PCB                PIC X.                                   
014900 01  TMS-WDQ3-PCB                PIC X.                                   
015000 01  TMS-WDK6-PCB                PIC X.                                   
015100 01  TMS-WDE6-PCB                PIC X.                                   
015200 01  TMS-WDK5-PCB                PIC X.                                   
015210 01  TMS-WDQ2C-PCB               PIC X.                                   
015300 01  XXJK-PCB                    PIC X.                                   
015400 01  WDK5-PCB                    PIC X.                                   
015500     EJECT                                                                
015600 PROCEDURE DIVISION  USING MSG-PCB  TMS-CRE-PCB TMS-DEL-PCB               
015610                           ATAB-PCB                                       
015700                           WDP7-PCB WDE6-PCB  WDE4FSEQ-PCB                
015800                           WDE4-PCB WDE4F-PCB WDQ5A-PCB WDB6-PCB          
015900                           PLATS-DM-PCB PLATS-DN-PCB                      
016000                           PLATS-DP-PCB PLATS-DO-PCB                      
016100                           PLATS-WDE6C-PCB PLATS-GMTC-PCB                 
016200                           PLATS-WDB6-PCB                                 
016300                           DNOT-ORQP-PCB                                  
016400                           DNOT-ORQP2-PCB                                 
016500                           DNOT-ORQP3-PCB                                 
016600                           DNOT-4013-PCB                                  
016700                           DNOT-BENA-PCB                                  
016800                          TMS-1165-PCB TMS-4141-PCB TMS-WDB2-PCB          
016900                          TMS-WDB6-PCB TMS-WDD3-PCB TMS-WDB1-PCB          
017000                          TMS-WDE4A-PCB TMS-WDE4F-PCB TMS-WDQ2-PCB        
017100                          TMS-WDQ3-PCB TMS-WDK6-PCB TMS-WDE6-PCB          
017200                          TMS-WDK5-PCB TMS-WDQ2C-PCB                      
017300                           XXJK-PCB                                       
017400                           WDK5-PCB.                                      
017500 MAIN SECTION.                                                            
017600     ENTRY 'DLITCBL' USING MSG-PCB  TMS-CRE-PCB TMS-DEL-PCB               
017610                           ATAB-PCB                                       
017700                           WDP7-PCB WDE6-PCB  WDE4FSEQ-PCB                
017800                           WDE4-PCB WDE4F-PCB WDQ5A-PCB WDB6-PCB          
017900                           PLATS-DM-PCB PLATS-DN-PCB                      
018000                           PLATS-DP-PCB PLATS-DO-PCB                      
018100                           PLATS-WDE6C-PCB PLATS-GMTC-PCB                 
018200                           PLATS-WDB6-PCB                                 
018300                           DNOT-ORQP-PCB                                  
018400                           DNOT-ORQP2-PCB                                 
018500                           DNOT-ORQP3-PCB                                 
018600                           DNOT-4013-PCB                                  
018700                           DNOT-BENA-PCB                                  
018800                          TMS-1165-PCB TMS-4141-PCB TMS-WDB2-PCB          
018900                          TMS-WDB6-PCB TMS-WDD3-PCB TMS-WDB1-PCB          
019000                          TMS-WDE4A-PCB TMS-WDE4F-PCB TMS-WDQ2-PCB        
019100                          TMS-WDQ3-PCB TMS-WDK6-PCB TMS-WDE6-PCB          
019200                          TMS-WDK5-PCB TMS-WDQ2C-PCB                      
019300                           XXJK-PCB                                       
019400                           WDK5-PCB.                                      
019500                                                                          
019600     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
019700     IF SUB-KDRC = 0                                                      
019800       PERFORM A-INIT                                                     
019900                                                                          
020000       CALL W4034310 USING REQU-AREA RESP-AREA                            
020010                     MAX-KVRADER  WS-KDTRANS                              
020020                     MSG-PCB  TMS-CRE-PCB TMS-DEL-PCB ATAB-PCB            
020200                     WDP7-PCB WDE6-PCB  WDE4FSEQ-PCB WDE4-PCB             
020300                     WDE4F-PCB WDQ5A-PCB WDB6-PCB                         
020400                     PLATS-DM-PCB PLATS-DN-PCB                            
020500                     PLATS-DP-PCB PLATS-DO-PCB                            
020600                     PLATS-WDE6C-PCB PLATS-GMTC-PCB                       
020700                     PLATS-WDB6-PCB                                       
020800                     DNOT-ORQP-PCB                                        
020900                     DNOT-ORQP2-PCB                                       
021000                     DNOT-ORQP3-PCB                                       
021100                     DNOT-4013-PCB                                        
021200                     DNOT-BENA-PCB                                        
021300                     TMS-1165-PCB TMS-4141-PCB TMS-WDB2-PCB               
021400                     TMS-WDB6-PCB TMS-WDD3-PCB TMS-WDB1-PCB               
021500                     TMS-WDE4A-PCB TMS-WDE4F-PCB TMS-WDQ2-PCB             
021600                     TMS-WDQ3-PCB TMS-WDK6-PCB TMS-WDE6-PCB               
021700                     TMS-WDK5-PCB TMS-WDQ2C-PCB                           
021800                     XXJK-PCB                                             
021900                     WDK5-PCB                                             
022000                                                                          
022100       IF SUB-KDTRANS(1:6) = 'W4A343'                                     
022200         PERFORM Y-CHECK-LINE-MSG                                         
022300         PERFORM X-MSG-CONV                                               
022400       END-IF                                                             
022500                                                                          
022600       PERFORM S02-RETURN-RESPONSE                                        
022700                                                                          
022800     END-IF                                                               
022900                                                                          
023000     PERFORM Z-FINIT                                                      
023100     MOVE ZERO TO RETURN-CODE                                             
023200     GOBACK                                                               
023300     .                                                                    
023400     EJECT                                                                
023500 A-INIT SECTION.                                                          
023600     CONTINUE                                                             
023700                                                                          
023800     MOVE ALL '+'   TO RESP-AREA                                          
023900     MOVE SPACE     TO RESP-IDMSG-ERROR                                   
024000                       RESP-IDMSG-INFO                                    
024100                       RESP-IDELMT-ERROR                                  
024200     MOVE ZERO      TO RESP-KVRADER                                       
024300     MOVE 002       TO RESP-IDRESVER                                      
024400                                                                          
024410     MOVE SUB-KDTRANS(1:6)  TO WS-KDTRANS                                 
024420                                                                          
024500     PERFORM AA-HANDLE-INPUT-VERSIONS                                     
024600                                                                          
024700     IF SUB-KDTRANS(1:7) = 'W4W343T' OR 'W4W343U'                         
024800       CONTINUE                                                           
024900     ELSE                                                                 
025000       MOVE 001                  TO AUTH-KDCALL                           
025100       CALL WZ01AUTH          USING AUTH-WZ01AUTH                         
025200                                    REQU-WZ01REQ2                         
025300                                    OF REQU-AREA                          
025400       IF AUTH-KDRC > 0                                                   
025500         MOVE ERR-UNAUTHORIZED   TO RESP-IDMSG-ERROR                      
025600       END-IF                                                             
025700                                                                          
025800       MOVE FUNCTION UPPER-CASE (REQU-KDPGMACT OF REQU-AREA)              
025900                                 TO REQU-KDPGMACT OF REQU-AREA            
026000       MOVE FUNCTION UPPER-CASE (REQU-IDDC-KEY OF REQU-AREA)              
026100                                 TO REQU-IDDC-KEY OF REQU-AREA            
026200       MOVE FUNCTION UPPER-CASE (REQU-IDSPRAK  OF REQU-AREA)              
026300                                 TO REQU-IDSPRAK  OF REQU-AREA            
026400       MOVE FUNCTION UPPER-CASE (REQU-KDMATT   OF REQU-AREA)              
026500                                 TO REQU-KDMATT   OF REQU-AREA            
026600       MOVE FUNCTION UPPER-CASE (REQU-FLJANEJ-ALLA OF REQU-AREA)          
026700                                 TO REQU-FLJANEJ-ALLA OF REQU-AREA        
026800       MOVE FUNCTION UPPER-CASE (REQU-KDKOLLI  OF REQU-AREA)              
026900                                 TO REQU-KDKOLLI  OF REQU-AREA            
027000       MOVE FUNCTION UPPER-CASE (REQU-KDEMBTYP OF REQU-AREA)              
027100                                 TO REQU-KDEMBTYP OF REQU-AREA            
027200       PERFORM AB-INITIALIZE                                              
027300     END-IF                                                               
027310                                                                          
027400     .                                                                    
027500                                                                          
027600 AA-HANDLE-INPUT-VERSIONS SECTION.                                        
027700                                                                          
027800     MOVE REQU-WZ01REQ2                                                   
027900          OF REQU-IN-AREA        TO REQU-WZ01REQ2                         
028000                                    OF REQU-AREA                          
028100                                                                          
028200     IF REQU-IDINVER OF REQU-IN-AREA = 2                                  
028300       MOVE REQU-DATA-AREA       TO REQU-W40343I2                         
028400     ELSE                                                                 
028500       MOVE REQU-DATA-AREA       TO REQU-W40343I1                         
028600                                                                          
028700       MOVE REQU-IDPRODNR-KEY                                             
028800            OF REQU-W40343I1     TO REQU-IDPRODNR-KEY                     
028900                                    OF REQU-W40343I2                      
029000       MOVE REQU-IDKOLLI-KEY                                              
029100            OF REQU-W40343I1     TO REQU-IDKOLLI-KEY                      
029200                                    OF REQU-W40343I2                      
029300       MOVE REQU-IDDC-KEY                                                 
029400            OF REQU-W40343I1     TO REQU-IDDC-KEY                         
029500                                    OF REQU-W40343I2                      
029600       MOVE REQU-IDSPRAK                                                  
029700            OF REQU-W40343I1     TO REQU-IDSPRAK                          
029800                                    OF REQU-W40343I2                      
029900       MOVE '+'                  TO REQU-KDMATT                           
030000                                    OF REQU-W40343I2                      
030100       IF REQU-KVRADER                                                    
030200          OF REQU-W40343I1 = ALL '+'                                      
030300         MOVE ZERO               TO REQU-KVRADER                          
030400                                    OF REQU-W40343I2                      
030500       ELSE                                                               
030600         MOVE REQU-KVRADER                                                
030700            OF REQU-W40343I1     TO REQU-KVRADER                          
030800                                    OF REQU-W40343I2                      
030801                                    WS-KVRADER                            
030810         IF REQU-KVRADER OF REQU-W40343I1 > MAX-KVRADER                   
030820            MOVE MAX-KVRADER TO WS-KVRADER                                
030830         END-IF                                                           
030900       END-IF                                                             
031000                                                                          
031100       IF REQU-IDRADNR-FOM                                                
031200          OF REQU-W40343I1 = ALL '+'                                      
031300         MOVE ALL-PLUS           TO REQU-IDRADNR-FOM                      
031400                                    OF REQU-W40343I2                      
031500       ELSE                                                               
031600         MOVE REQU-IDRADNR-FOM                                            
031700            OF REQU-W40343I1     TO REQU-IDRADNR-FOM                      
031800                                    OF REQU-W40343I2                      
031900       END-IF                                                             
032000                                                                          
032100       IF REQU-IDRADNR-TOM                                                
032200          OF REQU-W40343I1 = ALL '+'                                      
032300         MOVE ALL-PLUS           TO REQU-IDRADNR-TOM                      
032400                                    OF REQU-W40343I2                      
032500       ELSE                                                               
032600         MOVE REQU-IDRADNR-TOM                                            
032700            OF REQU-W40343I1     TO REQU-IDRADNR-TOM                      
032800                                    OF REQU-W40343I2                      
032900       END-IF                                                             
033000                                                                          
033100       MOVE REQU-FLJANEJ-ALLA                                             
033200            OF REQU-W40343I1     TO REQU-FLJANEJ-ALLA                     
033300                                    OF REQU-W40343I2                      
033400       MOVE REQU-IDKOLLI-ALLA                                             
033500            OF REQU-W40343I1     TO REQU-IDKOLLI-ALLA                     
033600                                    OF REQU-W40343I2                      
033700       MOVE REQU-IDKOLLI-NY                                               
033800            OF REQU-W40343I1     TO REQU-IDKOLLI-NY                       
033900                                    OF REQU-W40343I2                      
034000       MOVE REQU-KDKOLLI                                                  
034100            OF REQU-W40343I1     TO REQU-KDKOLLI                          
034200                                    OF REQU-W40343I2                      
034300       MOVE REQU-KDEMBTYP                                                 
034400            OF REQU-W40343I1     TO REQU-KDEMBTYP                         
034500                                    OF REQU-W40343I2                      
034600       MOVE REQU-VKORDBTO                                                 
034700            OF REQU-W40343I1     TO REQU-VKORDBTO                         
034800                                    OF REQU-W40343I2                      
034900       MOVE REQU-DIKOLLIL                                                 
035000            OF REQU-W40343I1     TO REQU-DIKOLLIL                         
035100                                    OF REQU-W40343I2                      
035200       MOVE REQU-DIKOLLIB                                                 
035300            OF REQU-W40343I1     TO REQU-DIKOLLIB                         
035400                                    OF REQU-W40343I2                      
035500       MOVE REQU-DIKOLLIH                                                 
035600            OF REQU-W40343I1     TO REQU-DIKOLLIH                         
035700                                    OF REQU-W40343I2                      
035800       PERFORM                                                            
035900       VARYING INX-IX FROM 1 BY 1                                         
036000         UNTIL INX-IX > WS-KVRADER                                        
036100         MOVE REQU-FLNOLLAD-LINE OF REQU-W40343I1 (INX-IX)                
036200                                 TO REQU-FLNOLLAD-LINE                    
036300                                    OF REQU-W40343I2 (INX-IX)             
036400         MOVE REQU-IDARTNR-LINE  OF REQU-W40343I1 (INX-IX)                
036500                                 TO REQU-IDARTNR-LINE                     
036600                                    OF REQU-W40343I2 (INX-IX)             
036700         MOVE REQU-IDKOLLI-LINE  OF REQU-W40343I1 (INX-IX)                
036800                                 TO REQU-IDKOLLI-LINE                     
036900                                    OF REQU-W40343I2 (INX-IX)             
037000         MOVE REQU-KVLEVART-LINE-IN OF REQU-W40343I1 (INX-IX)             
037100                                 TO REQU-KVLEVART-LINE-IN                 
037200                                    OF REQU-W40343I2 (INX-IX)             
037300       END-PERFORM                                                        
037400     END-IF                                                               
037500     .                                                                    
037600                                                                          
037700 AB-INITIALIZE SECTION.                                                   
037800                                                                          
037900     IF (REQU-IDKOLLI-NY OF REQU-AREA NOT = SPACES) AND                   
038000        (REQU-KDKOLLI    OF REQU-AREA NOT = SPACES) AND                   
038100        (REQU-VKORDBTO   OF REQU-AREA NOT = SPACES)                       
038200        IF (REQU-KDEMBTYP OF REQU-AREA = SPACES OR LOW-VALUES) AND        
038300           (REQU-DIKOLLIL OF REQU-AREA = SPACES OR LOW-VALUES) AND        
038400           (REQU-DIKOLLIB OF REQU-AREA = SPACES OR LOW-VALUES) AND        
038500           (REQU-DIKOLLIH OF REQU-AREA = SPACES OR LOW-VALUES)            
038600           MOVE ALL '+' TO REQU-KDEMBTYP OF REQU-AREA                     
038700           MOVE ALL '+' TO REQU-DIKOLLIL OF REQU-AREA                     
038800           MOVE ALL '+' TO REQU-DIKOLLIB OF REQU-AREA                     
038900           MOVE ALL '+' TO REQU-DIKOLLIH OF REQU-AREA                     
039000         END-IF                                                           
039100     END-IF                                                               
039200*IF USER MOVES TO EXISTING CASE,MOVE ALL '+' TO NEW CASE DETAILS          
039300     IF (REQU-IDKOLLI-NY  OF REQU-AREA = SPACES OR LOW-VALUES) AND        
039400        (REQU-KDKOLLI     OF REQU-AREA = SPACES OR LOW-VALUES) AND        
039500        (REQU-VKORDBTO    OF REQU-AREA = SPACES OR LOW-VALUES)            
039600         MOVE ALL '+'   TO  REQU-IDKOLLI-NY  OF REQU-AREA                 
039700                            REQU-KDKOLLI     OF REQU-AREA                 
039800                            REQU-VKORDBTO    OF REQU-AREA                 
039900     END-IF                                                               
040000     MOVE ALL '+'        TO REQU-IDKOLLI-ALLA OF REQU-AREA                
040100     MOVE ALL '+'        TO REQU-FLJANEJ-ALLA OF REQU-AREA                
040200     .                                                                    
040300     EJECT                                                                
040400 Y-CHECK-LINE-MSG SECTION.                                                
040500                                                                          
040600     MOVE +1                     TO INX-IX                                
040700     MOVE 'N'                    TO LINE-MSG-SW                           
040800     PERFORM UNTIL INX-IX > RESP-KVRADER OR LINE-MSG-SW = 'Y'             
040900       IF RESP-KDSVAR-LINE(INX-IX) NOT = SPACES AND                       
041000          RESP-IDMSG-ERROR = SPACES                                       
041100         MOVE 'Y'               TO LINE-MSG-SW                            
041200         IF RESP-KDSVAR-LINE (INX-IX) = 'S' OR 'M'                        
041300           MOVE ERR-CASE-MISSING TO RESP-IDMSG-ERROR                      
041400           MOVE 'IDKOLLI *(new)'                                          
041500                                 TO RESP-IDELMT-ERROR                     
041600         ELSE                                                             
041700           IF RESP-KDSVAR-LINE (INX-IX) = 'F' OR 'I'                      
041800             MOVE ERR-CASE-INVOICED                                       
041900                                 TO RESP-IDMSG-ERROR                      
042000             MOVE 'IDKOLLI *(new)'                                        
042100                                 TO RESP-IDELMT-ERROR                     
042200           END-IF                                                         
042300         END-IF                                                           
042400       END-IF                                                             
042500       ADD +1             TO INX-IX                                       
042600     END-PERFORM                                                          
042700     .                                                                    
042800     EJECT                                                                
042900 X-MSG-CONV SECTION.                                                      
043000     MOVE SPACES                  TO RESP-MESSAGES (1)                    
043100                                     RESP-MESSAGES (2)                    
043200     MOVE 1                       TO MSG-IX                               
043300*    REQUEST OK                                                           
043400     MOVE 200                     TO RESP-KDSTATUS-API                    
043500     IF RESP-IDMSG-INFO > SPACE                                           
043600       MOVE SPACES                TO MSG-CONV-AREA                        
043700       MOVE RESP-IDMSG-INFO       TO MSG-CONV-IDMSG-IN                    
043800       MOVE RESP-IDELMT-ERROR     TO MSG-CONV-IDELMT                      
043900       CALL WMSGCONV           USING MSG-CONV-AREA                        
044000       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
044100       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
044200       ADD 1                      TO MSG-IX                               
044300     END-IF                                                               
044400     IF RESP-IDMSG-ERROR > SPACE                                          
044500*      BAD REQUEST                                                        
044600       MOVE 400                   TO RESP-KDSTATUS-API                    
044700       MOVE SPACES                TO MSG-CONV-AREA                        
044800       MOVE RESP-IDMSG-ERROR      TO MSG-CONV-IDMSG-IN                    
044900       MOVE RESP-IDELMT-ERROR     TO MSG-CONV-IDELMT                      
045000       CALL WMSGCONV           USING MSG-CONV-AREA                        
045100       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
045200       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
045300     END-IF                                                               
045400     IF LINE-MSG-SW = 'Y'                                                 
045500*      BAD REQUEST                                                        
045600       MOVE 400                   TO RESP-KDSTATUS-API                    
045700     END-IF                                                               
045800     .                                                                    
045900                                                                          
046000 Z-FINIT SECTION.                                                         
046100     CONTINUE                                                             
046200     .                                                                    
046300     EJECT                                                                
046400*    --- DISPATCHER SECTIONS                                              
046500 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
046600                                                                          
046700     MOVE 'GETARG'                 TO SUB-KDFUNC                          
046800     MOVE 'CARPARTS.NDC.MOVELINESINCASE' TO SUB-ADDISPABS                 
046900     MOVE LENGTH OF REQU-IN-AREA   TO SUB-KVDLEN                          
047000                                                                          
047100     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-IN-AREA          
047200                                                                          
047300     IF SUB-KDRC > 0                                                      
047400       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
047500       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
047600       DELIMITED BY SIZE INTO ERROR-TEXT                                  
047700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
047800     END-IF                                                               
047900     .                                                                    
048000     SKIP3                                                                
048100 S02-RETURN-RESPONSE SECTION.                                             
048200                                                                          
048300     MOVE 'RETURN'                   TO SUB-KDFUNC                        
048400*    MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
048500     COMPUTE SUB-KVDLEN = LENGTH OF RESP-AREA -                           
048600           (500 - RESP-KVRADER) * LENGTH OF RESP-RAD-GRUPP                
048700                                                                          
048800                                                                          
048900     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
049000                                                                          
049100     IF SUB-KDRC > 0                                                      
049200       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
049300       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
049400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
049500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
049600     END-IF                                                               
049700     .                                                                    
049800     EJECT                                                                
