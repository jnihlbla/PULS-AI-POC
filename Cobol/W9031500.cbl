000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W9031500.                                                
000300 AUTHOR.         ELAINE SJÖBLOM                                           
000400 DATE-WRITTEN.   OCTOBER 2020                                             
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION.                                                            
000800*        API      CANCEL ORDERS     .                                     
000900*                                                                         
001000*                                                                         
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSAKTION: W90315U                                             
001400*        MID:         W903151i1                                           
001500*                                                                         
001600*    UTDATA.                                                              
001700*        MOD:         W903151O1                                           
001800*                                                                         
001900*    STORY 2375089 ADD IDSYSTEM VOUI, ECOM                                
002000*                                                                         
002100                                                                          
002200 ENVIRONMENT DIVISION.                                                    
002300 DATA DIVISION.                                                           
002400                                                                          
002500 WORKING-STORAGE SECTION.                                                 
002600                                                                          
002700 77    IDPGM                     PIC X(8)    VALUE 'W9031500'.            
002800 77    CURRENT-SECTION           PIC X(16)   VALUE SPACE.                 
002900 77    CURRENT-IMS-SECTION       PIC X(16)   VALUE SPACE.                 
003000*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003100 77    ERROR-TEXT                PIC X(80) VALUE SPACE.                   
003200 77    FELTEXT                   PIC X(80) VALUE SPACE.                   
003300 77    KDRC-DISPLAY              PIC Z(5).                                
003400 77    RKOD-ABEND-NO-DUMP        PIC S9(4)   COMP VALUE +16.              
003500 77    RKOD-ABEND-WITH-DUMP      PIC S9(4)   COMP VALUE +1000.            
003600                                                                          
003700 77    JAA                       PIC X       VALUE 'J'.                   
003800 77    YES                       PIC X       VALUE 'Y'.                   
003900 77    NOO                       PIC X       VALUE 'N'.                   
004010 77    WS-IDSYSTEM               PIC X(4)    VALUE SPACE.                 
004500 77    WS-KVBEART                PIC 9(6)    VALUE ZERO.                  
006400                                                                          
006401 77    OK-SW                     PIC X       VALUE 'Y'.                   
006402   88  EVERYTHING-OK                         VALUE 'Y'.                   
006403   88  SOMETHING-WRONG                       VALUE 'N'.                   
006404                                                                          
006405                                                                          
006410 01  WS-WORK-VAR.                                                         
006420   03  KVRADER-IX                  PIC S9(5) VALUE +0 COMP SYNC.          
006430   03  W-BLANKS                    PIC  9(5) VALUE ZERO.                  
006440   03  W-LENGTH                    PIC  9(5) VALUE ZERO.                  
006450                                                                          
006460                                                                          
007100 01  DYNAMISKA-SUBPROGRAM.                                                
007200   03  FELLOG                    PIC X(8)   VALUE 'FELLOG  '.             
007300   03  ABEND                     PIC X(8)   VALUE 'ABEND   '.             
007400   03  WZ01SUB                   PIC X(8)   VALUE 'WZ01SUB '.             
007500   03  CBLTDLI                   PIC X(8)   VALUE 'CBLTDLI '.             
007600   03  WDATKONV                  PIC X(8)   VALUE 'WDATKONV'.             
007800   03  W005INIT                  PIC X(8)   VALUE 'W005INIT'.             
007900   03  WZ01SEND                  PIC X(8)   VALUE 'WZ01SEND'.             
008000   03  WZ01AUTH                  PIC X(8)   VALUE 'WZ01AUTH'.             
008100   03  W006KOM                   PIC X(8)   VALUE 'W006KOM '.             
008110   03  W400ORCL                  PIC X(8)   VALUE 'W400ORCL'.             
008200*                                                                         
008300 01  FILLER                      PIC X(16)  VALUE 'SUB-CONTROL'.          
011400                                                                          
011401*01  -COPY WZ01SUB                                                        
011402*                                                                         
011403 01  FILLER                      PIC X(16)   VALUE 'WZ01AUTH   '.         
011404                                                                          
011405*01  -COPY WZ01AUTH                                                       
011406*                                                                         
011407                                                                          
011408*    --- PARAMETRAR TILL SUBPROGRAM W400ORCL                              
011409*01 -COPY W400ORCL                                                        
011410     EJECT                                                                
011411                                                                          
011412                                                                          
011413                                                                          
011414                                                                          
011500******************************************************************        
011600*                                                                         
011700*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
011800*                                                                         
011900 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
012000                                                                          
012100 01  REQU-AREA.                                                           
012200*    03  -COPY WZ01REQ2                                                   
012300*    03  -COPY W90315I1                                                   
012400                                                                          
012500 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
012600                                                                          
012700 01  RESP-AREA.                                                           
012800*    03  -COPY WZ01RESP                                                   
012900*    03  -COPY W90315O1                                                   
013000                                                                          
013100 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
013200 01  SEND-AREA.                                                           
013300*    03  -COPY WZ01SEND                                                   
013400     EJECT                                                                
013500 01  SEND-RAD.                                                            
013600   03  MAIL-RAD                PIC X(80)  VALUE SPACE.                    
013700                                                                          
024300                                                                          
024301                                                                          
024302 01  MESSAGE-CODES.                                                       
024303     03  SYS-ERROR               PIC X(3)    VALUE '099'.                 
024304     03  ORDER-DELETED           PIC X(3)    VALUE '200'.                 
024305     03  BAD-REQUEST             PIC X(3)    VALUE '400'.                 
024306     03  NOT-FOUND               PIC X(3)    VALUE '404'.                 
024307     03  ALREADY-DELETED         PIC X(3)    VALUE '410'.                 
024308     03  DELETE-NOT-ALLOWED      PIC X(3)    VALUE '405'.                 
024309                                                                          
024310                                                                          
024320                                                                          
024400 LINKAGE SECTION.                                                         
024500                                                                          
025100                                                                          
025110*01  -COPY W0009     -PRE ORCL-MSG-                                       
025111                                                                          
025112*01  -COPY W0009     -PRE ORCL-0693X-                                     
025113                                                                          
025200*01  -COPY W0008     -PRE ORCL-WDP8-                                      
025300     05  FILLER                  PIC X.                                   
025400                                                                          
025401*01  -COPY W0008     -PRE ORCL-WDF5-                                      
025402     05  FILLER                  PIC X.                                   
025403                                                                          
025404*01  -COPY W0008     -PRE ORCL-WDQ2CSEQ-                                  
025405     05  FILLER                  PIC X.                                   
025406                                                                          
025407*01  -COPY W0008     -PRE ORCL-WDQ3-                                      
025408     05  FILLER                  PIC X.                                   
025409                                                                          
025410*01  -COPY W0008     -PRE ORCL-WDQ4-                                      
025411     05  FILLER                  PIC X.                                   
025412                                                                          
025413*01  -COPY W0008     -PRE ORCL-WDA5-                                      
025414     05  FILLER                  PIC X.                                   
025415                                                                          
025416*01  -COPY W0008     -PRE ORCL-WDB6-                                      
025417     05  FILLER                  PIC X.                                   
025418                                                                          
025419*01  -COPY W0008     -PRE ORCL-WDE4-                                      
025420     05  FILLER                  PIC X.                                   
025421                                                                          
025430 01  ATAB-PCB                    PIC X.                                   
027200                                                                          
027300                                                                          
027400                                                                          
027410 PROCEDURE DIVISION  USING ORCL-MSG-PCB ORCL-0693X-PCB                    
027420                           ORCL-WDP8-PCB ORCL-WDF5-PCB                    
027430                           ORCL-WDQ2CSEQ-PCB                              
027440                           ORCL-WDQ3-PCB ORCL-WDQ4-PCB                    
027450                           ORCL-WDA5-PCB                                  
027460                           ORCL-WDB6-PCB                                  
027470                           ORCL-WDE4-PCB ATAB-PCB.                        
027800                                                                          
027900 MAIN SECTION.                                                            
028000                                                                          
028010     ENTRY 'DLITCBL' USING ORCL-MSG-PCB ORCL-0693X-PCB                    
028030                           ORCL-WDP8-PCB ORCL-WDF5-PCB                    
028040                           ORCL-WDQ2CSEQ-PCB                              
028050                           ORCL-WDQ3-PCB ORCL-WDQ4-PCB                    
028060                           ORCL-WDA5-PCB                                  
028070                           ORCL-WDB6-PCB                                  
028080                           ORCL-WDE4-PCB ATAB-PCB.                        
028400                                                                          
028500     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
028700     IF SUB-KDRC = 0                                                      
028800        MOVE 001                 TO AUTH-KDCALL                           
028900        CALL WZ01AUTH         USING AUTH-WZ01AUTH                         
029000                                    REQU-WZ01REQ2                         
029100        IF AUTH-KDRC = 0                                                  
029200           IF REQU-KDPGMACT = 'E'                                         
029300                                                                          
029400              PERFORM A-INIT-SAVE-INPUT                                   
029700                                                                          
029800              IF EVERYTHING-OK                                            
029801                 PERFORM C-CALL-IMS-SUBPROGRAM                            
029804             END-IF                                                       
031300           ELSE                                                           
031400             MOVE SYS-ERROR TO RESP-IDMSG-ERROR                           
031500           END-IF                                                         
031600        ELSE                                                              
031700           IF AUTH-KDRC = 4                                               
031800              MOVE BAD-REQUEST TO RESP-IDMSG-ERROR                        
031900           ELSE                                                           
032000              MOVE AUTH-KDRC TO KDRC-DISPLAY                              
032100              STRING 'WZ01AUTH GETARG ERROR RC=' KDRC-DISPLAY             
032200              DELIMITED BY SIZE INTO ERROR-TEXT                           
032300              CALL ABEND USING RKOD-ABEND-WITH-DUMP                       
032400           END-IF                                                         
032500       END-IF                                                             
032600                                                                          
032700       PERFORM S02-RETURN-RESPONSE                                        
032800     END-IF                                                               
032900                                                                          
033000     MOVE ZERO                         TO RETURN-CODE                     
033100                                                                          
033200     GOBACK                                                               
033300     .                                                                    
033400                                                                          
033500                                                                          
033600 A-INIT-SAVE-INPUT SECTION.                                               
033700     MOVE 'A-INIT-SAVE-INPUT' TO CURRENT-SECTION                          
033800                                                                          
033900     MOVE 001                        TO RESP-IDMSGVER                     
034000     MOVE SPACE                      TO RESP-IDMSG-ERROR                  
034100                                        RESP-IDMSG-INFO                   
034200                                        RESP-IDELMT-ERROR                 
034300     MOVE SPACE                      TO RESP-IDMFSINF                     
034400                                        RESP-TEMFSINF                     
034500                                                                          
034600     MOVE AUTH-IDSYSTEM              TO WS-IDSYSTEM                       
034700                                                                          
034800                                                                          
036400     .                                                                    
036500                                                                          
036600                                                                          
043030  C-CALL-IMS-SUBPROGRAM SECTION.                                          
043031     MOVE 'C-CALL-SUBPGM'        TO CURRENT-SECTION                       
043032     MOVE  001                   TO ORCL-KDCALL                           
043033     MOVE  REQU-IDDISTR          TO ORCL-IDDISTR                          
043034     MOVE  REQU-IDKUNDNR         TO ORCL-IDKUNDNR                         
043035     MOVE  REQU-IDORDNR7         TO ORCL-IDORDNR7                         
043036     MOVE  REQU-TIREGDAT         TO ORCL-TIREGDAT                         
043037     MOVE  REQU-KVRADER          TO ORCL-KVRADER                          
043038     MOVE  WS-IDSYSTEM           TO ORCL-IDSYSTEM                         
043039* MOVING ORDERLINE                                                        
043043     IF ORCL-KVRADER = ZERO                                               
043045         CONTINUE                                                         
043046     ELSE                                                                 
043048        MOVE 1                    TO KVRADER-IX                           
043049                                                                          
043051        PERFORM UNTIL REQU-IDLEVART(KVRADER-IX) = SPACES                  
043052                   OR KVRADER-IX >= 999                                   
043053           MOVE REQU-IDLEVART(KVRADER-IX)                                 
043054                                    TO ORCL-IDLEVART(KVRADER-IX)          
043055           MOVE REQU-KVBEART(KVRADER-IX)                                  
043056                                    TO ORCL-KVBEART(KVRADER-IX)           
043057           MOVE REQU-IDDC(KVRADER-IX)                                     
043058                                    TO ORCL-IDDC(KVRADER-IX)              
043062           ADD 1                    TO KVRADER-IX                         
043063        END-PERFORM                                                       
043064     END-IF                                                               
043066       CALL W400ORCL USING ORCL-W400ORCL                                  
043067                           ORCL-MSG-PCB                                   
043068                           ORCL-0693X-PCB                                 
043069                           ORCL-WDP8-PCB                                  
043070                           ORCL-WDF5-PCB                                  
043071                           ORCL-WDQ2CSEQ-PCB                              
043072                           ORCL-WDQ3-PCB                                  
043073                           ORCL-WDQ4-PCB                                  
043074                           ORCL-WDA5-PCB                                  
043075                           ORCL-WDB6-PCB                                  
043076                           ORCL-WDE4-PCB                                  
043078         IF ORCL-KDSVAR-FEL                                               
043079            MOVE ORCL-IDMSG-ERROR    TO RESP-IDMFSINF                     
043080            MOVE ORCL-FEL-TEXT       TO RESP-TEMFSINF                     
043083         ELSE                                                             
043084             MOVE ORDER-DELETED      TO RESP-IDMFSINF                     
043085             MOVE 'CANCEL  DONE   '  TO RESP-TEMFSINF                     
043089         END-IF                                                           
043090      .                                                                   
043100      EJECT                                                               
063500                                                                          
063600     .                                                                    
063700*    --- DISPATCHER SECTIONS                                              
063800 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
063900                                                                          
064000     MOVE 'GETARG'               TO SUB-KDFUNC                            
064100     MOVE 'CARPARTS.PULS.APIORDERCANCEL'    TO SUB-ADDISPABS              
064200     MOVE SPACE TO REQU-AREA                                              
064210     MOVE 999                         TO REQU-KVRADER                     
064300     MOVE LENGTH OF REQU-AREA         TO SUB-KVDLEN                       
064400                                                                          
064500     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
064600                                                                          
064700     IF SUB-KDRC > 0                                                      
064800       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
064900       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
065000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
065100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
065200     END-IF                                                               
065300     .                                                                    
065400     SKIP3                                                                
065500 S02-RETURN-RESPONSE SECTION.                                             
065600                                                                          
065700     MOVE 'RETURN'                   TO SUB-KDFUNC                        
065800     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
065900                                                                          
066000     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
066100                                                                          
066200     IF SUB-KDRC > 0                                                      
066300       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
066400       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
066500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
066600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
066700     END-IF                                                               
066800     .                                                                    
066900                                                                          
