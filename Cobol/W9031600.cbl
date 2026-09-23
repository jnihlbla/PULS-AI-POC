000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W9031600.                                                
000300 AUTHOR.         PRIYA RC.                                                
000400 DATE-WRITTEN.   MARCH 2024                                               
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:    CARPARTS.PULS.APIORDERUPDATE                                
000800*             ORDERLINE ADDITION/UPDATION                                 
000900*             COMMUNICATION PROGRAM TO GET ORDER ID.                      
001000*             9316 CALLS W400ORUP WHICH CALLS 4258.                       
001100*                                                                         
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSACTION: W9A316                                              
001500*        REQUEST:     W90316I1                                            
001600*                                                                         
001700*    OUTDATA.                                                             
001800*        RESPONSE:    WZ01RES2                                            
001900*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300 INPUT-OUTPUT SECTION.                                                    
002400 FILE-CONTROL.                                                            
002500                                                                          
002600 DATA DIVISION.                                                           
002700 FILE SECTION.                                                            
002800 WORKING-STORAGE SECTION.                                                 
002900                                                                          
003000*    -COPY WY2000W1                                                       
003100 77  IDPGM                       PIC X(08)   VALUE 'W9031600'.            
003110 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
003200                                                                          
003300*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003400 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
003500 77  KDRC-DISPLAY                PIC Z(5).                                
003800                                                                          
003900 77  YES                         PIC X       VALUE 'J'.                   
004000 77  NOO                         PIC X       VALUE 'N'.                   
004400                                                                          
004800 77  KEYS-SW                     PIC X       VALUE 'J'.                   
004900     88  KEYS-OK                             VALUE 'J'.                   
005000     88  KEYS-WRONG                          VALUE 'N'.                   
005100                                                                          
006900 01  WS-WORK-VAR.                                                         
006901     03  KVRADER-IX                PIC S9(5) VALUE +0 COMP SYNC.          
006902     03  MSG-IX                    PIC S9(9) VALUE +0 COMP SYNC.          
006903     03  WS-ORUP-FEL-TEXT          PIC X(55) VALUE SPACE.                 
006914     03  WS-IDSYSTEM               PIC  X(4) VALUE SPACE.                 
006915     03  WS-IDSYSTEM-LYNK          PIC  X(4)   VALUE 'LYNK'.              
006916     03  WS-IDSYSTEM-POLE          PIC  X(4)   VALUE 'POLE'.              
006917     03  WS-IDSYSTEM-ECOM          PIC  X(4)   VALUE 'ECOM'.              
006918     03  WS-IDSYSTEM-VOUI          PIC  X(4)   VALUE 'VOUI'.              
006919     03  WS-IDSYSTEM-TAD           PIC  X(4)   VALUE 'TAD '.              
006920     03  WS-IDSYSTEM-ACC           PIC  X(4)   VALUE 'ACC '.              
006921     03  WS-IDSYSTEM-APA           PIC  X(4)   VALUE 'APA '.              
006922     03  WS-IDSYSTEM-APB           PIC  X(4)   VALUE 'APB '.              
006923     03  WS-IDSYSTEM-APC           PIC  X(4)   VALUE 'APC '.              
006924     03  WS-IDSYSTEM-APD           PIC  X(4)   VALUE 'APD '.              
006925     03  WS-IDSYSTEM-APE           PIC  X(4)   VALUE 'APE '.              
006926     03  WS-IDSYSTEM-APF           PIC  X(4)   VALUE 'APF '.              
006927     03  WS-IDSYSTEM-APG           PIC  X(4)   VALUE 'APG '.              
006928     03  WS-IDSYSTEM-APH           PIC  X(4)   VALUE 'APH '.              
006929     03  WS-IDSYSTEM-API           PIC  X(4)   VALUE 'API '.              
006930     03  WS-IDSYSTEM-APJ           PIC  X(4)   VALUE 'APJ '.              
007600     EJECT                                                                
007700*                                                                         
007710 01  MESSAGE-CODES.                                                       
007820     03 ERR-INVALID              PIC X(3)    VALUE '023'.                 
007830     03 ERR-NO-LINES             PIC X(3)    VALUE '027'.                 
007851     03 ERR-UNAUTHORIZED         PIC X(3)    VALUE '00A'.                 
007860     03 INF-UPDATED              PIC X(3)    VALUE '201'.                 
007900*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
008000 01  GENERAL-SUBPROGRAMS.                                                 
008100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008300     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
008400     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
008500     03  WZ01AUTH                PIC X(8)    VALUE 'WZ01AUTH'.            
008710     03  W400ORUP                PIC X(8)    VALUE 'W400ORUP'.            
008720     03  WMSGCONV                PIC X(8)    VALUE 'WMSGCONV'.            
008800                                                                          
008900*    --- PARAMETERS TO ABEND                                              
009000                                                                          
009100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009200 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
009300 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
009900*                                                                         
010000 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
010100*01  -COPY WZ01SUB                                                        
010200                                                                          
010600 01  FILLER                      PIC X(16)   VALUE 'WZ01AUTH'.            
010700*01  -COPY WZ01AUTH                                                       
010800                                                                          
010801 01  FILLER                      PIC X(16)   VALUE 'WMSGCONV'.            
010802*01  -COPY WMSGCONV                                                       
010803                                                                          
010804*    --- PARAMETRAR TILL SUBPROGRAM W400ORUP                              
010805*01 -COPY W400ORUP                                                        
010806     EJECT                                                                
010807                                                                          
010900***  BELOW COPYBOOKS INCLUDES INPUT AND OUTPUT HEADERS                    
011000***  WZ01REQ2 IN INPUT AND WZ01RES2 IN OUTPUT                             
011100*                                                                         
011200 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
011510 01  REQU-AREA.                                                           
011520*    03  -COPY WZ01REQ2                                                   
011530*    03  -COPY W90316I1                                                   
011540     EJECT                                                                
011600 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
011800 01  RESP-AREA.                                                           
011810*    03  -COPY WZ01RES2                                                   
012000     EJECT                                                                
012100*    --- AREAS FOR SUB MODULES W006KOM                                    
012200 01  FILLER                    PIC X(16) VALUE 'MSG-KOM-WMSGKOM '.        
012300*01  -COPY WMSGKOM                                                        
012400                                                                          
012500 01  FILLER                    PIC X(16) VALUE 'MSG-IO-AREA     '.        
012510*01  -COPY WMSGAREA                                                       
012520                                                                          
013000     EJECT                                                                
013100                                                                          
013200******************************************************************        
013300*****                                                                     
013400*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013500*****                                                                     
013600 01  IMS-KEYS.                                                            
013700   03    FILLER          PIC X(16)   VALUE 'IMS KEYS        '.            
015100*                                                                         
018600 LINKAGE SECTION.                                                         
018700*                                                                         
019600*01  -COPY W0009     -PRE ORUP-MSG-                                       
019700                                                                          
019800*01  -COPY W0009     -PRE ORUP-0693X-                                     
019900                                                                          
020000*01  -COPY W0008     -PRE ORUP-WDP8-                                      
020010     05  FILLER                  PIC X.                                   
020020                                                                          
020030*01  -COPY W0008     -PRE ORUP-USEA-                                      
020040     05  FILLER                  PIC X.                                   
020050                                                                          
020060*01  -COPY W0008     -PRE ORUP-WDK6-                                      
020070     05  FILLER                  PIC X.                                   
020080                                                                          
020090*01  -COPY W0008     -PRE ORUP-WDB2-                                      
020091     05  FILLER                  PIC X.                                   
020092                                                                          
020093*01  -COPY W0008     -PRE ORUP-WDF5-                                      
020094     05  FILLER                  PIC X.                                   
020095                                                                          
020096*01  -COPY W0008     -PRE ORUP-WDQ2CSEQ-                                  
020097     05  FILLER                  PIC X.                                   
020104                                                                          
020105*01  -COPY W0008     -PRE ORUP-WDQ4A-                                     
020106     05  FILLER                  PIC X.                                   
020107                                                                          
020108 01  ORUP-AREG-WDK6-PCB          PIC X.                                   
020109 01  ORUP-AREG-WDK7-PCB          PIC X.                                   
020110                                                                          
020111 01  ORUP-SDCA-ARTS-PCB          PIC X.                                   
020112 01  ORUP-SDCA-WDB6-PCB          PIC X.                                   
020113 01  ORUP-SDCA-WDK9-PCB          PIC X.                                   
020114 01  ORUP-SDCA-WDR6-PCB          PIC X.                                   
020115 01  ORUP-SDCA-WDK6-PCB          PIC X.                                   
020116 01  ORUP-SDCA-WDQ4B-PCB         PIC X.                                   
020117 01  ORUP-SDCA-WDQ2-PCB          PIC X.                                   
020118 01  ORUP-SDCA-WDQ4-PCB          PIC X.                                   
020119 01  ORUP-SDCA-WDB6-2-PCB        PIC X.                                   
020120 01  ORUP-SDCA-WDK6-2-PCB        PIC X.                                   
020121 01  ORUP-SDCA-WDK7-2-PCB        PIC X.                                   
020122 01  ORUP-SDCA-WDK7-3-PCB        PIC X.                                   
020123                                                                          
020124 01  ORUP-KVAN-WDB2-PCB          PIC X.                                   
020125 01  ORUP-KVAN-WDC1-PCB          PIC X.                                   
020126                                                                          
020127 01  ATAB-PCB                    PIC X.                                   
020128                                                                          
020130     EJECT                                                                
020200 PROCEDURE DIVISION USING ORUP-MSG-PCB                                    
020300                          ORUP-0693X-PCB                                  
020400                          ORUP-WDP8-PCB                                   
020500                          ORUP-USEA-PCB                                   
020600                          ORUP-WDK6-PCB                                   
020700                          ORUP-WDB2-PCB                                   
020800                          ORUP-WDF5-PCB                                   
020810                          ORUP-WDQ2CSEQ-PCB                               
020840                          ORUP-WDQ4A-PCB                                  
020841                          ORUP-AREG-WDK6-PCB ORUP-AREG-WDK7-PCB           
020842                          ORUP-SDCA-ARTS-PCB ORUP-SDCA-WDB6-PCB           
020843                          ORUP-SDCA-WDK9-PCB ORUP-SDCA-WDR6-PCB           
020844                          ORUP-SDCA-WDK6-PCB ORUP-SDCA-WDQ4B-PCB          
020845                          ORUP-SDCA-WDQ2-PCB ORUP-SDCA-WDQ4-PCB           
020846                          ORUP-SDCA-WDB6-2-PCB                            
020847                          ORUP-SDCA-WDK6-2-PCB                            
020848                          ORUP-SDCA-WDK7-2-PCB                            
020849                          ORUP-SDCA-WDK7-3-PCB                            
020850                          ORUP-KVAN-WDB2-PCB                              
020851                          ORUP-KVAN-WDC1-PCB                              
020852                          ATAB-PCB.                                       
020860                                                                          
020900 MAIN SECTION.                                                            
021000     ENTRY 'DLITCBL' USING ORUP-MSG-PCB                                   
021100                           ORUP-0693X-PCB                                 
021200                           ORUP-WDP8-PCB                                  
021300                           ORUP-USEA-PCB                                  
021400                           ORUP-WDK6-PCB                                  
021500                           ORUP-WDB2-PCB                                  
021600                           ORUP-WDF5-PCB                                  
021610                           ORUP-WDQ2CSEQ-PCB                              
021640                           ORUP-WDQ4A-PCB                                 
021641                           ORUP-AREG-WDK6-PCB ORUP-AREG-WDK7-PCB          
021642                           ORUP-SDCA-ARTS-PCB ORUP-SDCA-WDB6-PCB          
021643                           ORUP-SDCA-WDK9-PCB ORUP-SDCA-WDR6-PCB          
021644                           ORUP-SDCA-WDK6-PCB ORUP-SDCA-WDQ4B-PCB         
021645                           ORUP-SDCA-WDQ2-PCB ORUP-SDCA-WDQ4-PCB          
021646                           ORUP-SDCA-WDB6-2-PCB                           
021647                           ORUP-SDCA-WDK6-2-PCB                           
021648                           ORUP-SDCA-WDK7-2-PCB                           
021649                           ORUP-SDCA-WDK7-3-PCB                           
021650                           ORUP-KVAN-WDB2-PCB                             
021651                           ORUP-KVAN-WDC1-PCB                             
021660                           ATAB-PCB.                                      
021700                                                                          
021800     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
021810                                                                          
021900     IF SUB-KDRC = 0                                                      
022000       PERFORM A-INIT                                                     
022100       IF KEYS-OK                                                         
022510         IF REQU-UPDATE                                                   
022520            PERFORM F-PREPARE-W400ORUP                                    
022530         ELSE                                                             
022540            MOVE ERR-INVALID        TO RESP-IDMSG-ERROR                   
022550            MOVE 'KDPGMACT'         TO RESP-IDELMT-ERROR                  
022560            MOVE NOO                TO KEYS-SW                            
022570         END-IF                                                           
022600       END-IF                                                             
022700                                                                          
022800       IF SUB-KDTRANS(1:6) = 'W9A316'                                     
022900         PERFORM S11-MSG-CONV                                             
023000       END-IF                                                             
023010                                                                          
023100       PERFORM S02-RETURN-RESPONSE                                        
023200     END-IF                                                               
023300                                                                          
023400     MOVE ZERO TO RETURN-CODE                                             
023500     GOBACK                                                               
023600     .                                                                    
023700     EJECT                                                                
023800 A-INIT SECTION.                                                          
023900     MOVE 'A-INIT'               TO CURRENT-SECTION                       
024000                                                                          
024100     MOVE YES                    TO KEYS-SW                               
024400                                                                          
024500     MOVE SPACE                  TO RESP-IDMSG-ERROR                      
024600                                    RESP-IDMSG-INFO                       
024700                                    RESP-IDELMT-ERROR                     
024710                                    WS-ORUP-FEL-TEXT                      
024800                                                                          
025100     IF SUB-KDTRANS(1:6) = 'W9A316'                                       
025200       MOVE 001                  TO AUTH-KDCALL                           
025300       CALL WZ01AUTH          USING AUTH-WZ01AUTH                         
025400                                    REQU-WZ01REQ2                         
025500       IF AUTH-KDRC > 0                                                   
025600         MOVE ERR-UNAUTHORIZED   TO RESP-IDMSG-ERROR                      
025700         MOVE NOO                TO KEYS-SW                               
025800       END-IF                                                             
025801                                                                          
025810       MOVE AUTH-IDSYSTEM        TO WS-IDSYSTEM                           
026400     END-IF                                                               
026402                                                                          
026403     PERFORM AA-CHECK-IDSYSTEM                                            
026500     .                                                                    
026600     EJECT                                                                
026700                                                                          
027035 AA-CHECK-IDSYSTEM SECTION.                                               
027036     MOVE 'AA-CHECK-IDS'         TO CURRENT-SECTION                       
027040                                                                          
027041     IF WS-IDSYSTEM(1:3) = 'LDC' OR                                       
027042        WS-IDSYSTEM(1:3) = 'LYN' OR                                       
027043        WS-IDSYSTEM(1:3) = 'ECO' OR                                       
027044        WS-IDSYSTEM(1:3) = 'VOU' OR                                       
027045        WS-IDSYSTEM(1:3) = 'TAD' OR                                       
027046        WS-IDSYSTEM(1:3) = 'ACC' OR                                       
027047        WS-IDSYSTEM(1:3) = 'APA' OR                                       
027048        WS-IDSYSTEM(1:3) = 'APB' OR                                       
027049        WS-IDSYSTEM(1:3) = 'APC' OR                                       
027050        WS-IDSYSTEM(1:3) = 'APD' OR                                       
027051        WS-IDSYSTEM(1:3) = 'APE' OR                                       
027052        WS-IDSYSTEM(1:3) = 'APF' OR                                       
027053        WS-IDSYSTEM(1:3) = 'APG' OR                                       
027054        WS-IDSYSTEM(1:3) = 'APH' OR                                       
027055        WS-IDSYSTEM(1:3) = 'API' OR                                       
027056        WS-IDSYSTEM(1:3) = 'APJ'                                          
027057        CONTINUE                                                          
027058     ELSE                                                                 
027059        MOVE ERR-UNAUTHORIZED   TO RESP-IDMSG-ERROR                       
027061        MOVE NOO                TO KEYS-SW                                
027062        MOVE 'IDSYSTEM'         TO RESP-IDELMT-ERROR                      
027064     END-IF                                                               
027065     .                                                                    
027066     EJECT                                                                
027067                                                                          
035500 F-PREPARE-W400ORUP SECTION.                                              
035510     MOVE 'F-PREPARE-OR'         TO CURRENT-SECTION                       
035521                                                                          
035560     MOVE  001                   TO ORUP-KDCALL                           
035561     MOVE SPACES                 TO ORUP-IDAPIORDREF                      
035570     MOVE  REQU-IDDISTR          TO ORUP-IDDISTR                          
035580     MOVE  REQU-IDKUNDNR         TO ORUP-IDKUNDNR                         
035590     MOVE  REQU-IDORDNR7         TO ORUP-IDORDNR7                         
035591     MOVE  REQU-TIREGDAT         TO ORUP-TIREGDAT                         
035592     MOVE  REQU-KVRADER          TO ORUP-KVRADER                          
035593     MOVE  WS-IDSYSTEM           TO ORUP-IDSYSTEM                         
035594                                                                          
035599     IF REQU-KVRADER <= 0                                                 
035600        MOVE ERR-NO-LINES    TO RESP-IDMSG-ERROR                          
035602        MOVE 'KVRADER'       TO RESP-IDELMT-ERROR                         
035604        MOVE NOO             TO KEYS-SW                                   
035605     ELSE                                                                 
035606        MOVE 1                  TO KVRADER-IX                             
035607        PERFORM UNTIL KVRADER-IX > REQU-KVRADER                           
035608                   OR REQU-IDLEVART(KVRADER-IX) = SPACES                  
035609                   OR KVRADER-IX >  999                                   
035610           MOVE REQU-KDBEHX(KVRADER-IX)       TO                          
035611                ORUP-KDBEHX(KVRADER-IX)                                   
035612           MOVE REQU-IDLEVART(KVRADER-IX)     TO                          
035613                ORUP-IDLEVART(KVRADER-IX)                                 
035614           MOVE REQU-KVBEART(KVRADER-IX)      TO                          
035615                ORUP-KVBEART(KVRADER-IX)                                  
035616           MOVE REQU-PRARTNTO-LOC(KVRADER-IX) TO                          
035618                ORUP-PRARTNTO-LOC(KVRADER-IX)                             
035619           MOVE REQU-KDVALISO(KVRADER-IX)     TO                          
035620                ORUP-KDVALISO(KVRADER-IX)                                 
035621           MOVE REQU-BERADREF(KVRADER-IX)     TO                          
035622                ORUP-BERADREF(KVRADER-IX)                                 
035625          ADD 1                TO KVRADER-IX                              
035626        END-PERFORM                                                       
035627                                                                          
035628        PERFORM FA-CALL-W400ORUP                                          
035629     END-IF                                                               
035630     .                                                                    
035700     EJECT                                                                
035710                                                                          
035800 FA-CALL-W400ORUP SECTION.                                                
035900     MOVE 'FA-CALL-W400'         TO CURRENT-SECTION                       
036000                                                                          
036002     CALL W400ORUP USING ORUP-W400ORUP                                    
036003                         ORUP-MSG-PCB                                     
036004                         ORUP-0693X-PCB                                   
036005                         ORUP-WDP8-PCB                                    
036006                         ORUP-USEA-PCB                                    
036007                         ORUP-WDK6-PCB                                    
036008                         ORUP-WDB2-PCB                                    
036009                         ORUP-WDF5-PCB                                    
036010                         ORUP-WDQ2CSEQ-PCB                                
036012                         ORUP-WDQ4A-PCB                                   
036013                         ORUP-AREG-WDK6-PCB ORUP-AREG-WDK7-PCB            
036014                         ORUP-SDCA-ARTS-PCB ORUP-SDCA-WDB6-PCB            
036015                         ORUP-SDCA-WDK9-PCB ORUP-SDCA-WDR6-PCB            
036016                         ORUP-SDCA-WDK6-PCB ORUP-SDCA-WDQ4B-PCB           
036017                         ORUP-SDCA-WDQ2-PCB ORUP-SDCA-WDQ4-PCB            
036018                         ORUP-SDCA-WDB6-2-PCB                             
036019                         ORUP-SDCA-WDK6-2-PCB                             
036020                         ORUP-SDCA-WDK7-2-PCB                             
036021                         ORUP-SDCA-WDK7-3-PCB                             
036030                                                                          
036910     IF ORUP-KDSVAR-FEL                                                   
036911        MOVE ORUP-IDMSG-ERROR    TO RESP-IDMSG-ERROR                      
036921        MOVE ORUP-IDELMT-ERROR   TO RESP-IDELMT-ERROR                     
036922        MOVE ORUP-FEL-TEXT       TO WS-ORUP-FEL-TEXT                      
036950     ELSE                                                                 
036951        MOVE SPACES              TO RESP-IDMSG-ERROR                      
036952                                    RESP-IDELMT-ERROR                     
036953                                    WS-ORUP-FEL-TEXT                      
036970     END-IF                                                               
036980     .                                                                    
037000     EJECT                                                                
037100                                                                          
043200*    --- DISPATCHER SECTIONS                                              
043300 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
043310     MOVE 'S01-FETCH-RE'         TO CURRENT-SECTION                       
043400                                                                          
043500     MOVE 'GETARG'               TO SUB-KDFUNC                            
043600     MOVE 'CARPARTS.PULS.APIORDERUPDATE'    TO SUB-ADDISPABS              
043610     MOVE 999                    TO REQU-KVRADER                          
043700     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
043800                                                                          
043900     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
044000                                                                          
044100     IF SUB-KDRC > 0                                                      
044200       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
044300       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
044400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
044500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
044600     END-IF                                                               
044700     .                                                                    
044800     EJECT                                                                
044900                                                                          
045000 S02-RETURN-RESPONSE SECTION.                                             
045010     MOVE 'S02-RETURN-R'         TO CURRENT-SECTION                       
045100                                                                          
045200     MOVE 'RETURN'               TO SUB-KDFUNC                            
045300     MOVE LENGTH OF RESP-AREA    TO SUB-KVDLEN                            
045400                                                                          
045500     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
045600                                                                          
045700     IF SUB-KDRC > 0                                                      
045800       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
045900       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
046000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
046100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
046200     END-IF                                                               
046300     .                                                                    
046400     EJECT                                                                
046410                                                                          
046500 S11-MSG-CONV SECTION.                                                    
046600     MOVE 'S11-MSG-CONV'         TO CURRENT-SECTION                       
046700                                                                          
046800     MOVE SPACES                 TO RESP-MESSAGE (1)                      
046900                                    RESP-MESSAGE (2)                      
047000     MOVE 1                      TO MSG-IX                                
047100*    REQUEST OK                                                           
047120                                                                          
047200     MOVE 200                    TO RESP-KDSTATUS-API                     
047210     MOVE INF-UPDATED            TO RESP-IDMSG   (MSG-IX)                 
047220     MOVE 'ORDER UPDATED'        TO RESP-MESSAGE (MSG-IX)                 
047230                                                                          
047300     IF RESP-IDMSG-INFO > SPACE                                           
047302       MOVE 400                  TO RESP-KDSTATUS-API                     
047400       MOVE SPACES               TO MSG-CONV-AREA                         
047500       MOVE RESP-IDMSG-INFO      TO MSG-CONV-IDMSG-IN                     
047600       CALL WMSGCONV           USING MSG-CONV-AREA                        
047700       MOVE MSG-CONV-IDMSG-OUT   TO RESP-IDMSG   (MSG-IX)                 
047800       MOVE MSG-CONV-MESSAGE     TO RESP-MESSAGE (MSG-IX)                 
047900       ADD 1                     TO MSG-IX                                
048000     END-IF                                                               
048100     IF RESP-IDMSG-ERROR > SPACE                                          
048200*      BAD REQUEST                                                        
048300       MOVE 400                  TO RESP-KDSTATUS-API                     
048400       MOVE SPACES               TO MSG-CONV-AREA                         
048500       MOVE RESP-IDMSG-ERROR     TO MSG-CONV-IDMSG-IN                     
048600       MOVE RESP-IDELMT-ERROR    TO MSG-CONV-IDELMT                       
048700       CALL WMSGCONV           USING MSG-CONV-AREA                        
048800       MOVE MSG-CONV-IDMSG-OUT   TO RESP-IDMSG   (MSG-IX)                 
048900       MOVE MSG-CONV-MESSAGE     TO RESP-MESSAGE (MSG-IX)                 
048902       IF WS-ORUP-FEL-TEXT > SPACES                                       
048903          MOVE WS-ORUP-FEL-TEXT  TO RESP-MESSAGE (MSG-IX)                 
048904       END-IF                                                             
049000     END-IF                                                               
049003     .                                                                    
049004     EJECT                                                                
