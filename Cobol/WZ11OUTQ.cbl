       ID DIVISION.                                                             
       PROGRAM-ID.     WZ11OUTQ.                                                
       AUTHOR.         RAHUL REDDY.                                             
       DATE-WRITTEN.   21/06/12.                                                
       DATE-COMPILED.                                                           
                                                                                
      *                                                                         
      *    FUNCTION:                                                            
      *        THIS SUB PROGRAM HANDLES MQ CALLS                                
      *                                                                         
      *    ABENDCODES:                                                          
      *        U0016 -  . . . .                                                 
      *        U1000 -  . . . .                                                 
      *                                                                         
                                                                                
       ENVIRONMENT DIVISION.                                                    
       CONFIGURATION SECTION.                                                   
      *SOURCE-COMPUTER. IBM-z WITH DEBUGGING MODE.                              
                                                                                
       DATA DIVISION.                                                           
                                                                                
       WORKING-STORAGE SECTION.                                                 
                                                                                
       77  IDPGM                       PIC X(9)    VALUE 'WZ11OUTQ'.            
       77  YES                         PIC X       VALUE 'J'.                   
       77  NOO                         PIC X       VALUE 'N'.                   
       77  WS-RECLEN                   PIC S9(9) BINARY VALUE ZERO.             
       77  WS-REM-START                PIC S9(9) BINARY VALUE ZERO.             
       77  WS-REM-LEN                  PIC S9(9) BINARY VALUE ZERO.             
       77  MQ-TNT-MSG-LEN              PIC S9(9) BINARY VALUE ZERO.             
       77  WS-DELIM-VAL                PIC X(8).                                
                                                                                
       77  WS-MQ-CONV                  PIC X.                                   
                                                                                
       77  WS-CR                       PIC X(2)    VALUE 'CR'.                  
       77  WS-LF                       PIC X(2)    VALUE 'LF'.                  
       77  CR                          PIC X       VALUE X'0D'.                 
       77  LF-EBCDIC                   PIC X       VALUE X'25'.                 
       77  LF-ASCII                    PIC X       VALUE X'0A'.                 
       77  CRLF-EBCDIC                 PIC X(2)    VALUE X'0D25'.               
       77  CRLF-ASCII                  PIC X(2)    VALUE X'0D0A'.               
       77  SWEDISH-EBCDIC              PIC S9(9)   VALUE 278  BINARY.           
       77  WS-QMGR                     PIC X(4)    VALUE SPACE.                 
                                                                                
       01  W-VIMSID                    PIC X(8)    VALUE SPACE.                 
                                                                                
       77  SW-MODE                     PIC X       VALUE 'B'.                   
           88  MODE-ONLINE                         VALUE 'O'.                   
           88  MODE-BATCH                          VALUE 'B'.                   
                                                                                
       77  SW-MQ-CONN-EXIST            PIC X       VALUE 'N'.                   
           88  MQ-CONN-EXISTS-YES                  VALUE 'Y'.                   
           88  MQ-CONN-EXISTS-NO                   VALUE 'N'.                   
                                                                                
       77  SW-TNT-Q-OPEN               PIC X       VALUE 'N'.                   
           88  TNT-Q-OPEN-YES                      VALUE 'Y'.                   
           88  TNT-Q-OPEN-NO                       VALUE 'N'.                   
                                                                                
       01  WS-CURRENT-DATE             PIC X(21).                               
                                                                                
       77  SW-MQ-GROUP-MSG             PIC X       VALUE 'N'.                   
           88 MSG-GRP-NO                           VALUE 'N'.                   
           88 MSG-GRP-YES                          VALUE 'Y'.                   
                                                                                
       77  SW-MQ-GROUP-INDICATOR       PIC X       VALUE 'N'.                   
           88 FIRST-MSG-IN-GRP                     VALUE 'F'.                   
           88 LAST-MSG-IN-GRP                      VALUE 'L'.                   
           88 MSG-IN-GRP                           VALUE 'M'.                   
           88 MSG-GRP-VALID                        VALUE 'F' 'L' 'M'.           
           88 MSG-GRP-NOT-SET                      VALUE 'N'.                   
                                                                                
       77  SW-MQ-SPLIT-MSG             PIC X       VALUE 'N'.                   
           88 MSG-SPLIT-NO                         VALUE 'N'.                   
           88 MSG-SPLIT-YES                        VALUE 'Y'.                   
                                                                                
       01  WS-EOF-LEN                  PIC 9       VALUE ZERO.                  
       01  WS-DELIM-LEN                PIC 9       VALUE ZERO.                  
       01  WS-DELIM-LEN-TEMP           PIC 9       VALUE ZERO.                  
       01  WS-EOF                      PIC X(4)    VALUE SPACES.                
       01  WS-DELIMITER                PIC X(4)    VALUE SPACES.                
       01  WS-DELIMITER-TEMP           PIC X(4)    VALUE SPACES.                
                                                                                
       01  LEN-DISPLAY                 PIC Z(8)9.                               
       01  MAX-LEN-DISPLAY             PIC Z(8)9.                               
       01  RC-DISPLAY                  PIC Z(8)9.                               
       01  REASON-DISPLAY              PIC Z(8)9.                               
                                                                                
       01  SMALL-LETTERS               PIC X(31)   VALUE                        
           'abcdefghijklmnopqrstuvwxyzåäöüé'.                                   
       01  CAPS-LETTERS                PIC X(31)   VALUE                        
           'ABCDEFGHIJKLMNOPQRSTUVWXYZÅÄÖÜÉ'.                                   
                                                                                
       01  MQ-TNT-MSG                  PIC X(10000) VALUE SPACES.               
       01  TNT-KDFUNC                  PIC X(10).                               
                                                                                
       01  WS-PROP-IX-MAX              PIC 99    VALUE 10.                      
       01  WS-PROP-IX                  PIC 99    VALUE 0.                       
       01  WS-PROP-TABLE.                                                       
           03  FILLER OCCURS 10 TIMES.                                          
               05  WS-PROP-NAME        PIC X(100)  VALUE SPACES.                
               05  WS-PROP-VALUE       PIC X(100)  VALUE SPACES.                
                                                                                
       01  log.                                                                 
           03  messageId               PIC X(48) VALUE SPACES.                  
           03  correlationId           PIC X(48) VALUE SPACES.                  
           03  timestamp               PIC X(24) VALUE SPACES.                  
           03  messageSize             PIC 9(09) VALUE ZERO.                    
           03  integrationId           PIC X(10) VALUE SPACES.                  
           03  contractId              PIC X(10) VALUE SPACES.                  
           03  transactionId           PIC X(48) VALUE SPACES.                  
           03  logicalIds.                                                      
               05  logicalId                       OCCURS 10.                   
                   07  logicalIdType   PIC X(100) VALUE SPACES.                 
                   07  logicalIdValue  PIC X(100) VALUE SPACES.                 
           03  physicalId              PIC X(100) VALUE SPACES.                 
           03  hostName                PIC X(30) VALUE SPACES.                  
           03  hostIp                  PIC X(30) VALUE SPACES.                  
           03  applicationId           PIC X(06) VALUE SPACES.                  
           03  objectId                PIC X(60) VALUE SPACES.                  
           03  level                   PIC X(10) VALUE SPACES.                  
           03  logtext                 PIC X(100) VALUE SPACES.                 
                                                                                
       01  MQ-CALLS.                                                            
           03  MQCONN                  PIC X(8)    VALUE 'CSQBCONN'.            
           03  MQDISC                  PIC X(8)    VALUE 'CSQBDISC'.            
           03  MQOPEN                  PIC X(8)    VALUE 'CSQBOPEN'.            
           03  MQCLOSE                 PIC X(8)    VALUE 'CSQBCLOS'.            
           03  MQPUT                   PIC X(8)    VALUE 'CSQBPUT '.            
           03  MQGET                   PIC X(8)    VALUE 'CSQBGET '.            
           03  MQCRTMH                 PIC X(8)    VALUE 'CSQBCTMH'.            
           03  MQSETMP                 PIC X(8)    VALUE 'CSQBSTMP'.            
           03  MQINQMP                 PIC X(8)    VALUE 'CSQBIQMP'.            
                                                                                
       01  GENERAL-SUBPROGRAMS.                                                 
           03  WZ01ATAB                PIC X(8)    VALUE 'WZ01ATAB'.            
           03  AIBTDLI                 PIC X(8)    VALUE 'AIBTDLI'.             
           03  VIMSID                  PIC X(8)    VALUE 'VIMSID'.              
           03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
                                                                                
       77  RKOD-ABEND-NO-DUMP          PIC S9(4)   BINARY VALUE +16.            
                                                                                
       01  DISPLAY-TEXT                PIC X(80)   VALUE SPACES.                
       01  ERROR-TEXT.                                                          
           03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
           03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
                                                                                
                                                                                
      *    -- FIELDS USED WHEN CUTTING UP THE DATA INTO                         
      *    -- PIECES OF TRANMSITTABLE SIZE                                      
       01  WLEN                        PIC S9(9) BINARY VALUE ZERO.             
       01  WSTART                      PIC S9(9) BINARY VALUE 1.                
                                                                                
       01  WDATA                       PIC X(104857600)  VALUE SPACE.           
                                                                                
       01  MAX-MSG-LEN                 PIC S9(9) BINARY VALUE ZERO.             
       01  MAX-RESULT-LEN              PIC S9(9) BINARY VALUE ZERO.             
       01  WLEN-LINE                   PIC S9(9) BINARY VALUE ZERO.             
       01  WLEN-LINE-START             PIC S9(9) BINARY VALUE ZERO.             
       01  WDATA-LINE                  PIC X(104857600) VALUE SPACE.            
                                                                                
      *    -- PARAMETERS TO WZ01ATAB                                            
       01  -COPY WZ01ATAB                                                       
                                                                                
       01  MQ-AREA-START               PIC X(24)   VALUE  'MQ-AREA'.            
       01  MQ-AREA                     PIC X(80).                               
                                                                                
       01  MQ-DIRECTION                PIC X.                                   
           88  MQ-DIRECTION-IN                   VALUE 'I'.                     
           88  MQ-DIRECTION-OUT                  VALUE 'O'.                     
       01  MQ-INQ-FIRST-OR-NEXT        PIC X     VALUE 'F'.                     
           88  INQ-FIRST                         VALUE 'F'.                     
           88  INQ-NEXT                          VALUE 'N'.                     
       01  MQ-PROPERTY-EXISTENCE       PIC X     VALUE 'Y'.                     
           88  PROPERTY-EXIST                    VALUE 'Y'.                     
           88  PROPERTY-DOESNT-EXIST             VALUE 'N'.                     
                                                                                
       01  MQ-QMGR                     PIC X(48) VALUE SPACE.                   
       01  MQ-OBJNAME                  PIC X(48) VALUE SPACE.                   
       01  MQ-OBJTYPE                  PIC X(12) VALUE SPACE.                   
       01  MQ-COMPCODE                 PIC S9(9) BINARY.                        
       01  MQ-REASON                   PIC S9(9) BINARY.                        
       01  MQ-HCONN                    PIC S9(9) BINARY VALUE ZERO.             
       01  MQ-OPENOPTIONS              PIC S9(9) BINARY VALUE ZERO.             
       01  MQ-HOBJ                     PIC S9(9) BINARY VALUE ZERO.             
       01  MQ-MSGLENGTH                PIC S9(9) BINARY.                        
       01  MQ-DATALENGTH               PIC S9(9) BINARY.                        
                                                                                
       01  MQ-MSG-FORMAT               PIC X(8)    VALUE SPACE.                 
       01  MQ-INTEGRATION-ID           PIC X(100)  VALUE SPACE.                 
       01  MQ-CONTRACT-ID              PIC X(100)  VALUE SPACE.                 
       01  MQ-KDCCSID                  PIC S9(9)   VALUE 0    BINARY.           
       01  MQ-TOPIC-STRING             PIC X(100)  VALUE SPACE.                 
       01  MQ-RECORD-DELIMITER         PIC X(10)   VALUE SPACE.                 
       01  MQ-TYPE                     PIC X(100)  VALUE SPACE.                 
       01  MQ-VCOM-DELIMITER           PIC X(100)  VALUE SPACE.                 
       01  MQ-PROPHANDLE               PIC S9(18) BINARY.                       
       01  MQ-PROPNAME-TO-SET          PIC X(100).                              
       01  MQ-PROPTYPE                 PIC S9(9)  BINARY.                       
       01  MQ-PROPVALUELENGTH          PIC S9(9)  BINARY.                       
       01  MQ-PROPVALUE                PIC X(100).                              
       01  MQ-PROPDATALENGTH           PIC S9(9)  BINARY.                       
       01  MQ-PROPNAME-TO-RETRIEVE     PIC X(100)  VALUE SPACE.                 
       01  MQ-PROPNAME-RETRIEVED       PIC X(100)  VALUE SPACE.                 
       01  WS-PROPNAME-RETRIEVED       PIC X(100)  VALUE SPACE.                 
                                                                                
                                                                                
       01  MQ-COPYBOOKS.                                                        
           COPY CMQODV.          *> OBJECT DESCRIPTOR                           
           COPY CMQMD2V.         *> MESSAGE DESCRIPTOR VER 2                    
           COPY CMQPMOV.         *> PUT MESSAGE OPTIONS                         
           COPY CMQGMOV.         *> GET MESSAGE OPTIONS                         
           COPY CMQCMHOV.        *> CREATE MESSAGE HANDLE OPTIONS               
           COPY CMQSMPOV.        *> SET MESSAGE PROPERTY OPTIONS                
           COPY CMQIMPOV.        *> INQUIRE MESSAGE PROPERTY OPTIONS            
           COPY CMQCHRVV.        *> MQCHARV STRUCTURE. VARIABLE STRING          
           COPY CMQPDV.          *> PROPERTY DESCRIPTOR                         
           COPY CMQV.            *> MQ NAMED CONSTANTS                          
           COPY CMQTMC2V.        *> TRIGGER MESSAGE DESCRIPTOR                  
                                                                                
       01  MQ-COPYBOOKS-TNT.                                                    
           COPY CMQODV   REPLACING LEADING == MQOD == BY == TTOD ==.            
           COPY CMQMD2V  REPLACING LEADING == MQMD == BY == TTMD ==.            
           COPY CMQPMOV  REPLACING LEADING == MQPMO == BY == TTPMO ==.          
       01  TNT-FIELDS.                                                          
           03  TNT-OPENOPTIONS         PIC S9(9) BINARY VALUE ZERO.             
           03  TNT-HOBJ                PIC S9(9) BINARY VALUE ZERO.             
           03  TNT-INTEGRATION-ID      PIC X(10)   VALUE SPACE.                 
           03  TNT-CONTRACT-ID         PIC X(10)   VALUE SPACE.                 
                                                                                
       01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
                                                                                
       01  KEYS-TILL-DLI.                                                       
           03  W-WDGXKEY-X.                                                     
               05  W-IDHTYP            PIC X(04)   VALUE '0103'.                
               05  FILLER              PIC X(26)   VALUE LOW-VALUES.            
           03  W-ADDISPABS-X.                                                   
               05  W-ADDISPABS         PIC X(50)   VALUE SPACES.                
                                                                                
      *    --- STATUS-KOD FROM IMS                                              
       01  STATUS-WS                   PIC XX.                                  
           88  SEGMENT-FOUND                       VALUE '  '.                  
           88  SEGMENT-MISSING                     VALUE 'GE'.                  
                                                                                
       01  GOOD-STATUS-CODES.                                                   
           03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX                        
                                       PIC XX.                                  
                                                                                
       01  SSA1                        PIC X(128).                              
       01  SSA2                        PIC X(128).                              
                                                                                
      *    -- IMS FUNCTION CODES                                                
           COPY W0003.                                                          
                                                                                
       01  DLI-IO-AREA                 PIC X(1000).                             
      *01  -COPY WDGX0104 -RED DLI-IO-AREA.                                     
                                                                                
       01  AIB-AREA-START              PIC X(16)   VALUE                        
                                       'AIB-AREA-START  '.                      
           COPY W0031.                                                          
                                                                                
       LINKAGE SECTION.                                                         
                                                                                
       01 OUTQ-AREA.                                                            
          03  OUTQ-CONTROL-AREA.                                                
              05  OUTQ-KDFUNC          PIC X(10).                               
              05  OUTQ-KDRC            PIC S9(9) COMP.                          
              05  OUTQ-IDCOM           PIC S9(9) COMP.                          
          03  OUTQ-OPEN-AREA.                                                   
              05  OUTQ-ADDISPABS       PIC X(50).                               
          03  OUTQ-PROPERTY-AREA.                                               
              05  OUTQ-PROPERTY-NAME   PIC X(100).                              
              05  OUTQ-PROPERTY-VALUE  PIC X(100).                              
          03  OUTQ-ADDITIONAL-INFO.                                             
              05  OUTQ-PHYSICALID      PIC X(100).                              
          03  OUTQ-KVDLEN              PIC S9(9) BINARY.                        
          03  OUTQ-DATA.                                                        
              05 FILLER OCCURS 1 TO 104857600 TIMES                             
                 DEPENDING ON OUTQ-KVDLEN PIC X.                                
                                                                                
      *01  -COPY W0008  -PRE WDR5-                                              
           05 FILLER                   PIC X.                                   
                                                                                
       PROCEDURE DIVISION USING                                                 
                            OUTQ-AREA.                                          
       MAIN SECTION.                                                            
                                                                                
           PERFORM A-INIT                                                       
           EVALUATE OUTQ-KDFUNC                                                 
             WHEN 'OPEN'                                                        
             WHEN 'OPENONL'                                                     
               PERFORM B-OPEN                                                   
             WHEN 'PUT'                                                         
               PERFORM C-PUT-DATA                                               
             WHEN 'SEND'                                                        
               PERFORM H-SEND-DATA                                              
             WHEN 'GET'                                                         
               PERFORM D-GET-DATA                                               
             WHEN 'CLOSEQ'                                                      
               PERFORM E-CLOSEQ                                                 
             WHEN 'CLOSE'                                                       
               PERFORM F-CLOSE                                                  
             WHEN 'DISCONNECT'                                                  
               PERFORM G-DISCONNECT                                             
             WHEN 'SETPROP'                                                     
               PERFORM I-SET-MSG-PROPERTY                                       
             WHEN 'INQPROP'                                                     
               PERFORM H-INQ-MSG-PROPERTY                                       
             WHEN OTHER                                                         
               MOVE SPACES           TO ERROR-TEXT                              
               STRING 'INVALID FUNCTION CODE' OUTQ-KDFUNC                       
                 DELIMITED BY SIZE INTO ERROR-TEXT                              
               PERFORM S99-ABEND                                                
           END-EVALUATE                                                         
                                                                                
           PERFORM Z-FINIT                                                      
                                                                                
           MOVE ZERO TO RETURN-CODE                                             
           GOBACK                                                               
           .                                                                    
       A-INIT SECTION.                                                          
           MOVE ZERO                   TO OUTQ-KDRC                             
           IF OUTQ-KDFUNC = 'GET'                                               
             MOVE OUTQ-KVDLEN          TO MAX-RESULT-LEN                        
           END-IF                                                               
                                                                                
           IF OUTQ-KDFUNC = 'OPEN' OR 'OPENONL'                                 
             MOVE FUNCTION UPPER-CASE (OUTQ-ADDISPABS)                          
                                       TO ATAB-ADDISPABS                        
                                          W-ADDISPABS                           
             PERFORM IMS-GU-ADDRESS-INFO                                        
             IF SEGMENT-FOUND                                                   
      *        Check for kdcomtype MQ                                           
               PERFORM AA-MOVE-PARMS-FROM-DB                                    
             ELSE                                                               
               CALL WZ01ATAB        USING ATAB-WZ01ATAB                         
                                                                                
               IF RETURN-CODE > ZERO                                            
      *      -- ABSTRACT ADDRESS NOT FOUND IN ATAB                              
                 MOVE SPACES           TO ERROR-TEXT                            
                 STRING ATAB-ADDISPABS      DELIMITED BY SPACE                  
                   'NOT FOUND IN ATAB/0104' DELIMITED BY SIZE                   
                                     INTO ERROR-TEXT                            
                 PERFORM S99-ABEND                                              
               ELSE                                                             
                 PERFORM AB-EXTRACT-PARMS                                       
               END-IF                                                           
             END-IF                                                             
      *      If KDFUNC is OPENONL, override the MQ module names with            
      *      online (IMS) versions of them.                                     
             IF OUTQ-KDFUNC = 'OPENONL'                                         
               SET MODE-ONLINE         TO TRUE                                  
             ELSE                                                               
               SET MODE-BATCH          TO TRUE                                  
             END-IF                                                             
           END-IF                                                               
           .                                                                    
                                                                                
       AA-MOVE-PARMS-FROM-DB SECTION.                                           
                                                                                
           MOVE 0104-IDQMGR            TO MQ-QMGR                               
                                                                                
           MOVE 0104-IDMQOBJ           TO MQ-OBJNAME                            
           MOVE 0104-KDMQOBJ           TO MQ-OBJTYPE                            
                                                                                
           MOVE 0104-FLCOMMDIR         TO MQ-DIRECTION                          
           MOVE 0104-IDINTEGRATION     TO MQ-INTEGRATION-ID                     
           MOVE 0104-IDCONTRACT-MQ     TO MQ-CONTRACT-ID                        
           MOVE 0104-KDMQFMT           TO MQ-MSG-FORMAT                         
           MOVE 0104-FLMSGSPLIT        TO SW-MQ-SPLIT-MSG                       
           MOVE 0104-FLMSGGROUP        TO SW-MQ-GROUP-MSG                       
                                                                                
           IF 0104-KDCCSID = ZERO OR LOW-VALUES OR SPACES                       
             MOVE SWEDISH-EBCDIC       TO MQ-KDCCSID                            
           ELSE                                                                 
             MOVE 0104-KDCCSID         TO MQ-KDCCSID                            
           END-IF                                                               
                                                                                
           IF 0104-KDDELIM-REC = SPACES                                         
             MOVE SPACES               TO WS-DELIMITER                          
             MOVE ZERO                 TO WS-DELIM-LEN                          
           ELSE                                                                 
             MOVE FUNCTION HEX-TO-CHAR (                                        
                  FUNCTION TRIM      (0104-KDDELIM-REC))                        
                                       TO WS-DELIMITER                          
             COMPUTE WS-DELIM-LEN = FUNCTION LENGTH (                           
                                    FUNCTION TRIM (WS-DELIMITER))               
           END-IF                                                               
                                                                                
           IF 0104-KDEOF = SPACES                                               
             MOVE SPACES               TO WS-EOF                                
             MOVE ZERO                 TO WS-EOF-LEN                            
           ELSE                                                                 
             MOVE FUNCTION HEX-TO-CHAR (                                        
                  FUNCTION TRIM      (0104-KDEOF))                              
                                       TO WS-EOF                                
             COMPUTE WS-EOF-LEN = FUNCTION LENGTH (                             
                                    FUNCTION TRIM (WS-EOF))                     
           END-IF                                                               
                                                                                
           EVALUATE 0104-KDMSGLEN                                               
             WHEN 'B '                                                          
               COMPUTE MAX-MSG-LEN = 0104-KVMSGLEN-MAX                          
             WHEN 'KB'                                                          
               COMPUTE MAX-MSG-LEN = 0104-KVMSGLEN-MAX * 1024                   
             WHEN 'MB'                                                          
               COMPUTE MAX-MSG-LEN = 0104-KVMSGLEN-MAX * 1024 * 1024            
             WHEN OTHER                                                         
               CONTINUE *>RRR ERROR                                             
           END-EVALUATE                                                         
                                                                                
           IF MODE-BATCH                                                        
             DISPLAY IDPGM ' Queue Manager      : ' MQ-QMGR                     
             DISPLAY IDPGM ' Object Name        : ' MQ-OBJNAME                  
             DISPLAY IDPGM ' Object Type        : ' MQ-OBJTYPE                  
             DISPLAY IDPGM ' In/Out of PULS     : ' MQ-DIRECTION                
             DISPLAY IDPGM ' Integration ID     : ' MQ-INTEGRATION-ID           
             DISPLAY IDPGM ' Contract ID        : ' MQ-CONTRACT-ID              
             DISPLAY IDPGM ' Message Format     : ' MQ-MSG-FORMAT               
             DISPLAY IDPGM ' Message CCSID      : ' MQ-KDCCSID                  
             DISPLAY IDPGM ' Split large msg?   : ' SW-MQ-SPLIT-MSG             
             DISPLAY IDPGM ' Group large msg?   : ' SW-MQ-GROUP-MSG             
             DISPLAY IDPGM ' Rec delimiter      : ' WS-DELIMITER                
             DISPLAY IDPGM ' Rec delimiter len  : ' WS-DELIM-LEN                
             DISPLAY IDPGM ' EOF delimiter      : ' WS-EOF                      
             DISPLAY IDPGM ' EOF delimiter len  : ' WS-EOF-LEN                  
             DISPLAY IDPGM ' Max Message length : ' MAX-MSG-LEN                 
           END-IF                                                               
                                                                                
           .                                                                    
                                                                                
       AB-EXTRACT-PARMS SECTION.                                                
                                                                                
      *    -- QUEUE MANAGER NAME                                                
           MOVE ZERO                   TO TALLY                                 
           MOVE SPACE                  TO MQ-QMGR                               
           INSPECT ATAB-ADDISPINT TALLYING TALLY                                
                 FOR CHARACTERS BEFORE INITIAL 'QM:'                            
           IF TALLY < LENGTH OF ATAB-ADDISPINT                                  
             UNSTRING ATAB-ADDISPINT(TALLY + 4:) DELIMITED BY ';'               
                                     INTO MQ-QMGR                               
           END-IF                                                               
                                                                                
           IF MQ-QMGR = SPACES                                                  
      *        -- IF QMGR NAME IS NOT GIVEN IN ATAB,                            
      *        -- DETERMINE FROM THE IMS SYSTEM                                 
             CALL VIMSID            USING W-VIMSID                              
             IF W-VIMSID(1:3) = 'IMG'                                           
               MOVE 'MC0P'             TO MQ-QMGR                               
             ELSE                                                               
               IF W-VIMSID(1:3) = 'IMB'                                         
                 MOVE 'MC0Q'           TO MQ-QMGR                               
               ELSE                                                             
                 MOVE 'MC0T'           TO MQ-QMGR                               
               END-IF                                                           
             END-IF                                                             
           END-IF                                                               
                                                                                
      *    -- QUEUE NAME                                                        
           MOVE ZERO                   TO TALLY                                 
           MOVE SPACE                  TO MQ-OBJNAME                            
           INSPECT ATAB-ADDISPINT TALLYING TALLY                                
                   FOR CHARACTERS BEFORE INITIAL 'RQ:'                          
           IF TALLY < LENGTH OF ATAB-ADDISPINT                                  
             UNSTRING ATAB-ADDISPINT (TALLY + 4:)                               
               DELIMITED BY ';'                                                 
                                     INTO MQ-OBJNAME                            
             SET MQ-DIRECTION-OUT      TO TRUE                                  
           END-IF                                                               
                                                                                
           IF MQ-OBJNAME = SPACE                                                
             MOVE ZERO                 TO TALLY                                 
             INSPECT ATAB-ADDISPINT-RECV TALLYING TALLY                         
                   FOR CHARACTERS BEFORE INITIAL 'LQ:'                          
             IF TALLY < LENGTH OF ATAB-ADDISPINT-RECV                           
               UNSTRING ATAB-ADDISPINT-RECV(TALLY + 4:)                         
                 DELIMITED BY ';'                                               
                                     INTO MQ-OBJNAME                            
               SET MQ-DIRECTION-IN     TO TRUE                                  
             END-IF                                                             
           END-IF                                                               
                                                                                
           IF MQ-OBJNAME = SPACES                                               
             MOVE SPACE                TO ERROR-TEXT-STR                        
             MOVE 'LOCAL QUEUE NAME IS MISSING IN WZ01ATAB'                     
                                       TO ERROR-TEXT-STR                        
             PERFORM S99-ABEND                                                  
           END-IF                                                               
                                                                                
      *    -- META DATA: INTEGRATION ID                                         
           MOVE ZERO                   TO TALLY                                 
           MOVE SPACE                  TO MQ-INTEGRATION-ID                     
           INSPECT ATAB-ADDISPMETA TALLYING TALLY                               
                   FOR CHARACTERS BEFORE INITIAL 'I:'                           
           IF TALLY < LENGTH OF ATAB-ADDISPMETA                                 
             UNSTRING ATAB-ADDISPMETA(TALLY + 3:)  DELIMITED BY ';'             
                                       INTO MQ-INTEGRATION-ID                   
           END-IF                                                               
                                                                                
      *    -- META DATA: CONTRACT ID                                            
           MOVE ZERO                   TO TALLY                                 
           MOVE SPACE                  TO MQ-CONTRACT-ID                        
           INSPECT ATAB-ADDISPMETA TALLYING TALLY                               
                   FOR CHARACTERS BEFORE INITIAL 'C:'                           
           IF TALLY < LENGTH OF ATAB-ADDISPMETA                                 
             UNSTRING ATAB-ADDISPMETA(TALLY + 3:)  DELIMITED BY ';'             
                                     INTO MQ-CONTRACT-ID                        
           END-IF                                                               
                                                                                
      *    -- META DATA: TOPIC STRING                                           
           MOVE ZERO                   TO TALLY                                 
           MOVE SPACE                  TO MQ-TOPIC-STRING                       
           INSPECT ATAB-ADDISPMETA TALLYING TALLY                               
                   FOR CHARACTERS BEFORE INITIAL 'T:'                           
           IF TALLY < LENGTH OF ATAB-ADDISPMETA                                 
             UNSTRING ATAB-ADDISPMETA(TALLY + 3:)  DELIMITED BY ';'             
                                     INTO MQ-TOPIC-STRING                       
           END-IF                                                               
                                                                                
      *    -- META DATA: MESSAGE FORMAT                                         
           MOVE ZERO                   TO TALLY                                 
           MOVE 'MQSTR '               TO MQ-MSG-FORMAT                         
           MOVE 'Y'                    TO WS-MQ-CONV                            
           INSPECT ATAB-ADDISPMETA TALLYING TALLY                               
                   FOR CHARACTERS BEFORE INITIAL 'CONV:'                        
           IF TALLY < LENGTH OF ATAB-ADDISPMETA                                 
             UNSTRING ATAB-ADDISPMETA(TALLY + 6:)  DELIMITED BY ';'             
                                     INTO WS-MQ-CONV                            
             IF WS-MQ-CONV = 'N'                                                
               MOVE SPACES             TO MQ-MSG-FORMAT                         
             END-IF                                                             
           END-IF                                                               
                                                                                
      *   CHECK FOR DELIMITER ONLY IF CONVERSION IS YES. WHEN CONVERSION        
      *   IS NO, WE ARE SENDING BINARY DATA. DONT ADD DELIMITER THEN.           
           IF WS-MQ-CONV = 'Y'                                                  
      *      -- META DATA: RECORD DELIMITER                                     
             MOVE ZERO                 TO TALLY                                 
             INITIALIZE MQ-RECORD-DELIMITER                                     
             INSPECT ATAB-ADDISPMETA TALLYING TALLY                             
                     FOR CHARACTERS BEFORE INITIAL 'D:'                         
             IF TALLY < LENGTH OF ATAB-ADDISPMETA                               
               UNSTRING ATAB-ADDISPMETA(TALLY + 3:) DELIMITED BY ';'            
                                       INTO MQ-RECORD-DELIMITER                 
      *        DISPLAY IDPGM ' RECORD DELIMITER   : '                           
      *                MQ-RECORD-DELIMITER                                      
             END-IF                                                             
             EVALUATE MQ-RECORD-DELIMITER                                       
               WHEN WS-CR                                                       
                 MOVE CR               TO WS-DELIMITER                          
                 MOVE 1                TO WS-DELIM-LEN                          
               WHEN WS-LF                                                       
                 MOVE LF-EBCDIC        TO WS-DELIMITER                          
                 MOVE 1                TO WS-DELIM-LEN                          
               WHEN SPACE                                                       
      *          DISPLAY IDPGM                                                  
      *                  ' RECORD DELIMITER   : CRLF (DEFAULT)'                 
                 MOVE 'CRLF'           TO MQ-RECORD-DELIMITER                   
                 STRING CR LF-EBCDIC                                            
                   DELIMITED BY SIZE INTO WS-DELIMITER                          
                 MOVE 2                TO WS-DELIM-LEN                          
               WHEN OTHER                                                       
      *          DISPLAY IDPGM                                                  
      *                  ' RECORD DELIMITER   : CRLF ! OVERRIDED !'             
                 MOVE 'CRLF'           TO MQ-RECORD-DELIMITER                   
                 STRING CR LF-EBCDIC                                            
                   DELIMITED BY SIZE INTO WS-DELIMITER                          
                 MOVE 2                TO WS-DELIM-LEN                          
             END-EVALUATE                                                       
           ELSE                                                                 
             MOVE SPACES               TO WS-DELIMITER                          
             MOVE ZERO                 TO WS-DELIM-LEN                          
           END-IF                                                               
                                                                                
      *    -- META DATA: TYPE                                                   
           MOVE ZERO                   TO TALLY                                 
           MOVE SPACE                  TO MQ-TYPE                               
           INSPECT ATAB-ADDISPMETA TALLYING TALLY                               
                   FOR CHARACTERS BEFORE INITIAL 'TY:'                          
           IF TALLY < LENGTH OF ATAB-ADDISPMETA                                 
             UNSTRING ATAB-ADDISPMETA(TALLY + 4:)  DELIMITED BY ';'             
                                     INTO MQ-TYPE                               
           END-IF                                                               
                                                                                
      *    -- META DATA: VCOM DELIMITER                                         
           MOVE ZERO                   TO TALLY                                 
           MOVE SPACE                  TO MQ-VCOM-DELIMITER                     
           INSPECT ATAB-ADDISPMETA TALLYING TALLY                               
                   FOR CHARACTERS BEFORE INITIAL 'DVCOM:'                       
           IF TALLY < LENGTH OF ATAB-ADDISPMETA                                 
             UNSTRING ATAB-ADDISPMETA(TALLY + 7:)  DELIMITED BY ';'             
                                     INTO MQ-VCOM-DELIMITER                     
           END-IF                                                               
                                                                                
      *    -- META DATA: GROUPING SWITCH                                        
           MOVE ZERO                   TO TALLY                                 
           MOVE 'N'                    TO SW-MQ-GROUP-MSG                       
           INSPECT ATAB-ADDISPMETA TALLYING TALLY                               
                   FOR CHARACTERS BEFORE INITIAL 'GRP:'                         
           IF TALLY < LENGTH OF ATAB-ADDISPMETA                                 
             UNSTRING ATAB-ADDISPMETA(TALLY + 5:)  DELIMITED BY ';'             
                                     INTO SW-MQ-GROUP-MSG                       
           END-IF                                                               
                                                                                
      *    -- META DATA: SPLIT MESSAGE SWITCH                                   
           MOVE ZERO                   TO TALLY                                 
           MOVE 'N'                    TO SW-MQ-SPLIT-MSG                       
           INSPECT ATAB-ADDISPMETA TALLYING TALLY                               
                   FOR CHARACTERS BEFORE INITIAL 'SPL:'                         
           IF TALLY < LENGTH OF ATAB-ADDISPMETA                                 
             UNSTRING ATAB-ADDISPMETA(TALLY + 5:)  DELIMITED BY ';'             
                                     INTO SW-MQ-SPLIT-MSG                       
           END-IF                                                               
                                                                                
           MOVE SWEDISH-EBCDIC         TO MQ-KDCCSID                            
                                                                                
           IF MODE-BATCH                                                        
             DISPLAY IDPGM ' QUEUE MANAGER      : ' MQ-QMGR                     
             DISPLAY IDPGM ' QUEUE NAME         : ' MQ-OBJNAME                  
             DISPLAY IDPGM ' INTEGRATION ID     : ' MQ-INTEGRATION-ID           
             DISPLAY IDPGM ' CONTRACT ID        : ' MQ-CONTRACT-ID              
             DISPLAY IDPGM ' TOPIC STRING       : ' MQ-TOPIC-STRING             
             DISPLAY IDPGM ' MESSAGE FORMAT     : ' MQ-MSG-FORMAT               
             DISPLAY IDPGM ' TYPE               : ' MQ-TYPE                     
             DISPLAY IDPGM ' VCOM-DELIMITER     : ' MQ-VCOM-DELIMITER           
             DISPLAY IDPGM ' RECORD DELIMITER   : ' MQ-RECORD-DELIMITER         
             DISPLAY IDPGM ' GROUP MSG SWITCH   : ' SW-MQ-GROUP-MSG             
             DISPLAY IDPGM ' SPLIT MSG SWITCH   : ' SW-MQ-SPLIT-MSG             
             DISPLAY IDPGM ' Message CCSID      : ' MQ-KDCCSID                  
             DISPLAY IDPGM '                     '                              
             DISPLAY IDPGM '                     '                              
           END-IF                                                               
                                                                                
           COMPUTE MAX-MSG-LEN = LENGTH OF WDATA                                
           .                                                                    
                                                                                
       B-OPEN SECTION.                                                          
                                                                                
           MOVE ZERO                   TO WS-PROP-IX                            
           MOVE SPACES                 TO WS-PROP-TABLE                         
                                                                                
           IF MQ-CONN-EXISTS-NO                                                 
             PERFORM S41-MQ-CONN                                                
             SET MQ-CONN-EXISTS-YES    TO TRUE                                  
           END-IF                                                               
                                                                                
           IF TNT-Q-OPEN-NO                                                     
             PERFORM S41-MQ-OPEN-TNT                                            
             SET TNT-Q-OPEN-YES        TO TRUE                                  
           END-IF                                                               
                                                                                
           PERFORM S41-MQ-OPEN                                                  
                                                                                
           PERFORM S41-MQ-CREATE-MSGHANDLE                                      
                                                                                
           IF MQ-DIRECTION-OUT                                                  
             PERFORM BA-MQ-SET-PREDEFINED-HEADERS                               
           END-IF                                                               
           .                                                                    
                                                                                
       BA-MQ-SET-PREDEFINED-HEADERS SECTION.                                    
                                                                                
      *    -- SET THE MANDATORY PROPERTIES IF THEY HAVE VALUES                  
                                                                                
           MOVE SPACES                 TO TNT-INTEGRATION-ID                    
                                          TNT-CONTRACT-ID                       
                                                                                
           IF MQ-INTEGRATION-ID NOT = SPACE                                     
             MOVE 'IntegrationId'      TO MQ-PROPNAME-TO-SET                    
             MOVE MQ-INTEGRATION-ID    TO MQ-PROPVALUE                          
                                          TNT-INTEGRATION-ID                    
             PERFORM S41-MQ-SET-MSG-PROPERTY                                    
           END-IF                                                               
                                                                                
           IF MQ-CONTRACT-ID NOT = SPACE                                        
             MOVE 'ContractId'         TO MQ-PROPNAME-TO-SET                    
             MOVE MQ-CONTRACT-ID       TO MQ-PROPVALUE                          
                                          TNT-CONTRACT-ID                       
             PERFORM S41-MQ-SET-MSG-PROPERTY                                    
           END-IF                                                               
                                                                                
           IF MQ-TOPIC-STRING NOT = SPACE                                       
             MOVE 'TopicStr'           TO MQ-PROPNAME-TO-SET                    
             MOVE MQ-TOPIC-STRING      TO MQ-PROPVALUE                          
             PERFORM S41-MQ-SET-MSG-PROPERTY                                    
           END-IF                                                               
                                                                                
           IF MQ-TYPE NOT = SPACE                                               
             MOVE 'Type'               TO MQ-PROPNAME-TO-SET                    
             MOVE MQ-TYPE              TO MQ-PROPVALUE                          
             PERFORM S41-MQ-SET-MSG-PROPERTY                                    
           END-IF                                                               
           IF MODE-BATCH                                                        
             DISPLAY IDPGM ' SET PROPERTIES SUCCESSFUL'                         
           END-IF                                                               
                                                                                
           IF MQ-VCOM-DELIMITER NOT = SPACE                                     
             MOVE 'BASELINE_VCOM_Adapter_Delimiter'                             
                                       TO MQ-PROPNAME-TO-SET                    
             MOVE MQ-VCOM-DELIMITER    TO MQ-PROPVALUE                          
             PERFORM S41-MQ-SET-MSG-PROPERTY                                    
           END-IF                                                               
           .                                                                    
                                                                                
       C-PUT-DATA SECTION.                                                      
           ADD 1                       TO WLEN GIVING WSTART                    
                                                                                
           IF WSTART > 1                                                        
             MOVE WS-DELIM-LEN         TO WS-DELIM-LEN-TEMP                     
             MOVE WS-DELIMITER         TO WS-DELIMITER-TEMP                     
           ELSE                                                                 
             MOVE ZERO                 TO WS-DELIM-LEN-TEMP                     
             MOVE SPACES               TO WS-DELIMITER-TEMP                     
           END-IF                                                               
                                                                                
           IF WSTART + WS-DELIM-LEN-TEMP + OUTQ-KVDLEN - 1 >                    
                     MAX-MSG-LEN                                                
      *      MESSAGE LONGER THAN MAX MESSAGE LENGTH,                            
      *        1. IF NOT ALLOWED TO SEND AS A GROUP OR                          
      *           SPLIT MESSAGES, ERROR.                                        
      *        2. IF NOT ALLOWED TO SEND AS A GROUP, BUT                        
      *           CAN SPLIT INTO MULTIPLE MESSAGES.                             
      *        3. IF ALLOWED TO SEND AS A GROUP, BUT                            
      *           EACH MESSAGE AS A CONTINUATION OF PREVIOUS ONE                
      *           (SPLIT CAN HAPPEN IN THE MIDDLE OF A RECORD).                 
      *        4. IF ALLOWED TO SEND AS A GROUP AND                             
      *           SPLIT AT THE END OF A RECORD.                                 
                                                                                
             EVALUATE TRUE                                                      
               WHEN MSG-GRP-NO AND MSG-SPLIT-NO                                 
                 PERFORM CA-SEND-ERROR                                          
               WHEN MSG-GRP-NO AND MSG-SPLIT-YES                                
                 PERFORM CB-SPLIT-SEND                                          
               WHEN MSG-GRP-YES AND MSG-SPLIT-NO                                
                 PERFORM CC-SEND-CONT-GRP                                       
               WHEN MSG-GRP-YES AND MSG-SPLIT-YES                               
                 PERFORM CD-SPLIT-SEND-GRP                                      
             END-EVALUATE                                                       
                                                                                
           ELSE                                                                 
             STRING WS-DELIMITER-TEMP         DELIMITED BY SPACE                
                    OUTQ-DATA (1:OUTQ-KVDLEN) DELIMITED BY SIZE                 
                                     INTO WDATA                                 
                             WITH POINTER WSTART                                
             SUBTRACT 1              FROM WSTART                                
                                   GIVING WLEN                                  
           END-IF                                                               
           .                                                                    
       CA-SEND-ERROR      SECTION.                                              
                                                                                
           MOVE MAX-MSG-LEN            TO RC-DISPLAY                            
           MOVE SPACE                  TO ERROR-TEXT-STR                        
           STRING 'TOTAL MQ MESSAGE LENGTH TOO LONG. '                          
                  ' MAX ALLLOWED IS ' RC-DISPLAY                                
             DELIMITED BY SIZE       INTO ERROR-TEXT-STR                        
                                                                                
           DISPLAY IDPGM ' ERROR!!! ' ERROR-TEXT                                
           PERFORM S99-ABEND                                                    
           .                                                                    
                                                                                
       CB-SPLIT-SEND      SECTION.                                              
                                                                                
           PERFORM S41-MQ-PUT-SINGLE                                            
                                                                                
           MOVE 1                      TO WSTART                                
                                                                                
           STRING OUTQ-DATA (1:OUTQ-KVDLEN) DELIMITED BY SIZE                   
                                     INTO WDATA                                 
                             WITH POINTER WSTART                                
           SUBTRACT 1                FROM WSTART                                
                                   GIVING WLEN                                  
           .                                                                    
                                                                                
       CC-SEND-CONT-GRP   SECTION.                                              
                                                                                
           IF MSG-GRP-NOT-SET                                                   
             SET FIRST-MSG-IN-GRP      TO TRUE                                  
           END-IF                                                               
                                                                                
           COMPUTE WS-RECLEN = MAX-MSG-LEN - WSTART + 1                         
                                                                                
           IF WS-RECLEN > WS-DELIM-LEN-TEMP                                     
             STRING WS-DELIMITER-TEMP   DELIMITED BY SPACE                      
                    OUTQ-DATA (1:WS-RECLEN - WS-DELIM-LEN-TEMP)                 
                                        DELIMITED BY SIZE                       
                                     INTO WDATA                                 
                             WITH POINTER WSTART                                
             COMPUTE WS-REM-START = WS-RECLEN -                                 
                                    WS-DELIM-LEN-TEMP + 1                       
                                                                                
             COMPUTE WS-REM-LEN = OUTQ-KVDLEN -                                 
                                  WS-RECLEN   +                                 
                                  WS-DELIM-LEN-TEMP                             
           ELSE                                                                 
             IF WS-RECLEN > ZERO                                                
               STRING WS-DELIMITER-TEMP (1:WS-RECLEN)                           
                                        DELIMITED BY SIZE                       
                                     INTO WDATA                                 
                             WITH POINTER WSTART                                
             END-IF                                                             
           END-IF                                                               
                                                                                
           COMPUTE WLEN = WSTART - 1                                            
                                                                                
           PERFORM S41-MQ-PUT-GROUP                                             
                                                                                
           MOVE 1                      TO WSTART                                
                                                                                
           IF WS-RECLEN > WS-DELIM-LEN-TEMP                                     
             STRING OUTQ-DATA (WS-REM-START : WS-REM-LEN)                       
                             DELIMITED BY SIZE                                  
                                     INTO WDATA                                 
                             WITH POINTER WSTART                                
                                                                                
           ELSE                                                                 
             STRING WS-DELIMITER-TEMP (WS-RECLEN + 1 :                          
                                       WS-DELIM-LEN-TEMP - WS-RECLEN)           
                             DELIMITED BY SIZE                                  
                                     INTO WDATA                                 
                             WITH POINTER WSTART                                
             STRING OUTQ-DATA (1:OUTQ-KVDLEN)                                   
                             DELIMITED BY SIZE                                  
                                     INTO WDATA                                 
                             WITH POINTER WSTART                                
           END-IF                                                               
           COMPUTE WLEN = WSTART - 1                                            
           .                                                                    
                                                                                
       CD-SPLIT-SEND-GRP  SECTION.                                              
                                                                                
           IF MSG-GRP-NOT-SET                                                   
             SET FIRST-MSG-IN-GRP      TO TRUE                                  
           END-IF                                                               
                                                                                
           PERFORM S41-MQ-PUT-GROUP                                             
                                                                                
           MOVE 1                      TO WSTART                                
                                                                                
           STRING OUTQ-DATA (1 : OUTQ-KVDLEN)                                   
                             DELIMITED BY SIZE                                  
                                     INTO WDATA                                 
                             WITH POINTER WSTART                                
           COMPUTE WLEN = WSTART - 1                                            
           .                                                                    
                                                                                
       D-GET-DATA SECTION.                                                      
           MOVE ZERO                   TO WLEN-LINE-START                       
                                          OUTQ-KVDLEN                           
           IF WLEN = ZERO                                                       
             PERFORM S41-MQ-GET                                                 
             PERFORM S41-GET-ALL-PROPERTIES                                     
           END-IF                                                               
                                                                                
           IF WSTART <= WLEN                                                    
             PERFORM DA-PROCESS-MESSAGE                                         
           END-IF                                                               
           IF WSTART > WLEN                                                     
             IF WLEN > ZERO AND                                                 
                MQGMO-GROUPSTATUS = MQGS-MSG-IN-GROUP                           
               PERFORM S41-MQ-GET                                               
      *        Process only if delimter is spaces which indicates               
      *        the record read from last message is incomplete and              
      *        is continued on the next mq message we just received.            
               IF WS-DELIM-VAL = SPACES                                         
                 PERFORM DA-PROCESS-MESSAGE                                     
               END-IF                                                           
             ELSE                                                               
               IF OUTQ-KVDLEN = ZERO                                            
      D          DISPLAY IDPGM ' NO MORE DATA TO GET'                           
                 MOVE 1                TO OUTQ-KDRC                             
                 MOVE ZERO             TO OUTQ-KVDLEN                           
                 MOVE ZERO             TO WLEN                                  
                 MOVE ZERO             TO WSTART                                
      *          MOVE SPACES           TO WDATA                                 
               END-IF                                                           
             END-IF                                                             
           END-IF                                                               
           .                                                                    
                                                                                
       DA-PROCESS-MESSAGE SECTION.                                              
                                                                                
           MOVE ZERO                   TO WLEN-LINE                             
      *    MOVE SPACES                 TO WDATA-LINE                            
      D    DISPLAY IDPGM ' WSTART = ' WSTART                                    
      D    DISPLAY IDPGM ' WLEN   = ' WLEN                                      
           MOVE SPACES                 TO WS-DELIM-VAL                          
                                                                                
      D    DISPLAY IDPGM ' WDATA=' WDATA (WSTART : 26)                          
                                                                                
      D    DISPLAY IDPGM ' before unstring ' FUNCTION CURRENT-DATE              
           UNSTRING WDATA (1 : WLEN)                                            
                             DELIMITED BY WS-DELIMITER (1:WS-DELIM-LEN)         
                                     INTO WDATA-LINE (1:MAX-RESULT-LEN)         
                             DELIMITER IN WS-DELIM-VAL                          
                                 COUNT IN WLEN-LINE                             
                             WITH POINTER WSTART                                
      D    DISPLAY IDPGM ' AFTER  UNSTRING ' FUNCTION CURRENT-DATE              
      D    DISPLAY IDPGM ' WS-DELIM-VAL = ' WS-DELIM-VAL '!'                    
      D    DISPLAY IDPGM ' WDATA-LINE   = ' WDATA-LINE (1:30)                   
      D    DISPLAY IDPGM ' WSTART = ' WSTART                                    
      D    DISPLAY IDPGM ' WLEN-LINE = ' WLEN-LINE                              
      D    DISPLAY IDPGM ' OUTQ-KVDLEN = ' OUTQ-KVDLEN                          
      D    DISPLAY IDPGM ' MAX-RESULT-LEN = ' MAX-RESULT-LEN                    
           IF WLEN-LINE + OUTQ-KVDLEN > MAX-RESULT-LEN                          
             MOVE SPACES               TO ERROR-TEXT                            
             MOVE WLEN-LINE            TO LEN-DISPLAY                           
             MOVE MAX-RESULT-LEN       TO MAX-LEN-DISPLAY                       
             STRING 'DATA TRUNCATION. RECEIVED LENGTH > '                       
                 LEN-DISPLAY '. MAX ALLOWED = ' MAX-LEN-DISPLAY                 
                 DELIMITED BY SIZE   INTO ERROR-TEXT                            
             PERFORM S99-ABEND                                                  
           END-IF                                                               
                                                                                
           COMPUTE WLEN-LINE-START = OUTQ-KVDLEN + 1                            
           COMPUTE OUTQ-KVDLEN = OUTQ-KVDLEN + WLEN-LINE                        
                                                                                
           IF WLEN-LINE > 0                                                     
             MOVE WDATA-LINE (1:WLEN-LINE)                                      
                                       TO OUTQ-DATA                             
                                            (WLEN-LINE-START:WLEN-LINE)         
           END-IF                                                               
                                                                                
           .                                                                    
                                                                                
       H-INQ-MSG-PROPERTY SECTION.                                              
           IF WLEN = ZERO                                                       
             PERFORM S41-MQ-GET                                                 
             PERFORM S41-GET-ALL-PROPERTIES                                     
           END-IF                                                               
           IF OUTQ-PROPERTY-NAME NOT = SPACE                                    
             MOVE OUTQ-PROPERTY-NAME   TO MQ-PROPNAME-TO-RETRIEVE               
             SET INQ-FIRST             TO TRUE                                  
             PERFORM S41-MQ-INQ-PROPERTY                                        
             MOVE MQ-PROPVALUE         TO OUTQ-PROPERTY-VALUE                   
           END-IF                                                               
           .                                                                    
       I-SET-MSG-PROPERTY SECTION.                                              
           IF OUTQ-PROPERTY-NAME NOT = SPACE AND                                
              OUTQ-PROPERTY-VALUE NOT = SPACE                                   
             MOVE OUTQ-PROPERTY-NAME   TO MQ-PROPNAME-TO-SET                    
             MOVE OUTQ-PROPERTY-VALUE  TO MQ-PROPVALUE                          
             PERFORM S41-MQ-SET-MSG-PROPERTY                                    
      *      Save it, so it can be added to TNT message later.                  
             IF WS-PROP-IX < WS-PROP-IX-MAX                                     
               ADD +1                  TO WS-PROP-IX                            
               MOVE OUTQ-PROPERTY-NAME TO WS-PROP-NAME (WS-PROP-IX)             
               MOVE OUTQ-PROPERTY-VALUE                                         
                                       TO WS-PROP-VALUE (WS-PROP-IX)            
             END-IF                                                             
           END-IF                                                               
                                                                                
           .                                                                    
       H-SEND-DATA SECTION.                                                     
           PERFORM S41-MQ-PUT                                                   
           .                                                                    
       E-CLOSEQ SECTION.                                                        
           PERFORM S41-MQ-CLOSEQ                                                
           .                                                                    
       F-CLOSE SECTION.                                                         
           PERFORM S41-MQ-CLOSEQ                                                
           PERFORM S41-MQ-CLOSEQ-TNT                                            
           PERFORM S41-MQ-DISC                                                  
           .                                                                    
       G-DISCONNECT SECTION.                                                    
           PERFORM S41-MQ-CLOSEQ-TNT                                            
           PERFORM S41-MQ-DISC                                                  
           .                                                                    
       T-TRACK-AND-TRACE SECTION.                                               
           MOVE FUNCTION HEX-OF (MQMD-MSGID)                                    
                                       TO messageId                             
                                                                                
           IF TNT-KDFUNC = 'MQGET'                                              
             MOVE MQ-DATALENGTH        TO messageSize                           
             MOVE 'Received message'   TO logtext                               
           ELSE                                                                 
             MOVE MQ-MSGLENGTH         TO messageSize                           
             MOVE 'Sent message'       TO logtext                               
           END-IF                                                               
                                                                                
           MOVE SPACES                 TO correlationId                         
                                                                                
           MOVE FUNCTION                                                        
             FORMATTED-CURRENT-DATE('YYYY-MM-DDThh:mm:ss.sssZ')                 
                                       TO timestamp                             
                                                                                
           MOVE TNT-INTEGRATION-ID     TO integrationId                         
           MOVE TNT-CONTRACT-ID        TO contractId                            
      D      DISPLAY IDPGM ' TNT INTEG    ID' TNT-INTEGRATION-ID                
      D      DISPLAY IDPGM ' TNT CONTRACT ID' TNT-CONTRACT-ID                   
                                                                                
           MOVE SPACES                 TO transactionId                         
                                                                                
           MOVE SPACES                 TO logicalIds                            
           PERFORM                                                              
           VARYING WS-PROP-IX FROM 1 BY 1                                       
             UNTIL WS-PROP-IX > WS-PROP-IX-MAX                                  
             MOVE WS-PROP-NAME  (WS-PROP-IX)                                    
                                       TO logicalIdType  (WS-PROP-IX)           
             MOVE WS-PROP-VALUE (WS-PROP-IX)                                    
                                       TO logicalIdValue (WS-PROP-IX)           
           END-PERFORM                                                          
                                                                                
           IF OUTQ-PHYSICALID = LOW-VALUES                                      
             MOVE SPACES               TO physicalId                            
           ELSE                                                                 
             MOVE OUTQ-PHYSICALID      TO physicalId                            
           END-IF                                                               
                                                                                
      *    MOVE 'sg01.volvocars.net'   TO hostName                              
                                                                                
      *    MOVE '194.71.121.135'       TO hostIp                                
                                                                                
           MOVE '000573'               TO applicationId                         
                                                                                
           MOVE SPACES                 TO objectId                              
                                                                                
           MOVE 'INFO'                 TO level                                 
                                                                                
      *    MOVE 'test log'             TO logtext                               
                                                                                
      *    XML GENERATE MQ-TNT-MSG                                              
      *      FROM log                                                           
      *      COUNT IN MQ-TNT-MSG-LEN                                            
      *      WITH ENCODING 1208                                                 
      *      WITH XML-DECLARATION                                               
      *      NAME OF logtext           IS "text"                                
      *      SUPPRESS WHEN ZERO OR SPACES                                       
      *      ON EXCEPTION                                                       
      *        DISPLAY IDPGM ' ERROR IN XML GEN :' XML-CODE                     
      *    END-XML                                                              
      *    DISPLAY IDPGM ' TNT MSG=' MQ-TNT-MSG (1:MQ-TNT-MSG-LEN)              
                                                                                
           MOVE ZEROES                 TO MQ-TNT-MSG-LEN                        
           XML GENERATE MQ-TNT-MSG                                              
             FROM log                                                           
             COUNT IN MQ-TNT-MSG-LEN                                            
             WITH ENCODING 1208                                                 
             WITH XML-DECLARATION                                               
             NAME OF logtext           IS "text"                                
                     logicalIdType     IS "type"                                
             TYPE OF logicalIdType     IS ATTRIBUTE                             
                     logicalIdValue    IS CONTENT                               
             SUPPRESS WHEN ZERO OR SPACES                                       
             ON EXCEPTION                                                       
               DISPLAY IDPGM ' ERROR IN XML GEN :' XML-CODE                     
           END-XML                                                              
      D    DISPLAY IDPGM ' TNT MSG=' MQ-TNT-MSG (1:MQ-TNT-MSG-LEN)              
                                                                                
           PERFORM S41-MQ-PUT-TNT                                               
           .                                                                    
                                                                                
       Z-FINIT SECTION.                                                         
      D    DISPLAY IDPGM ' END OF ' IDPGM                                       
           CONTINUE                                                             
           .                                                                    
       S41-MQ-CONN SECTION.                                                     
                                                                                
           IF MODE-ONLINE                                                       
             CALL 'MQCONN'          USING MQ-QMGR                               
                                          MQ-HCONN                              
                                          MQ-COMPCODE                           
                                          MQ-REASON                             
           ELSE                                                                 
             CALL MQCONN            USING MQ-QMGR                               
                                          MQ-HCONN                              
                                          MQ-COMPCODE                           
                                          MQ-REASON                             
           END-IF                                                               
                                                                                
           IF MQ-COMPCODE NOT = MQCC-OK                                         
             MOVE SPACE                TO ERROR-TEXT-STR                        
             MOVE MQ-COMPCODE          TO RC-DISPLAY                            
             MOVE MQ-REASON            TO REASON-DISPLAY                        
             STRING 'RC FROM MQCONN ' RC-DISPLAY                                
                    ' REASON: ' REASON-DISPLAY                                  
                    ' ' MQ-QMGR                                                 
               DELIMITED BY SIZE                                                
                                     INTO ERROR-TEXT-STR                        
             DISPLAY IDPGM ' ' ERROR-TEXT                                       
             PERFORM S99-ABEND                                                  
           END-IF                                                               
                                                                                
      D    DISPLAY IDPGM ' MQCONN SUCCESSFUL    '                               
           .                                                                    
       S41-MQ-OPEN SECTION.                                                     
           MOVE MQ-OBJNAME             TO MQOD-OBJECTNAME                       
                                                                                
           MOVE MQOO-FAIL-IF-QUIESCING TO MQ-OPENOPTIONS                        
           IF MQ-DIRECTION-OUT                                                  
             ADD MQOO-OUTPUT           TO MQ-OPENOPTIONS                        
           ELSE                                                                 
             ADD MQOO-INPUT-SHARED     TO MQ-OPENOPTIONS                        
           END-IF                                                               
                                                                                
           IF MODE-ONLINE                                                       
             CALL 'MQOPEN'          USING MQ-HCONN                              
                                          MQOD                                  
                                          MQ-OPENOPTIONS                        
                                          MQ-HOBJ                               
                                          MQ-COMPCODE                           
                                          MQ-REASON                             
           ELSE                                                                 
             CALL MQOPEN            USING MQ-HCONN                              
                                          MQOD                                  
                                          MQ-OPENOPTIONS                        
                                          MQ-HOBJ                               
                                          MQ-COMPCODE                           
                                          MQ-REASON                             
           END-IF                                                               
                                                                                
           IF MQ-COMPCODE NOT = MQCC-OK                                         
             MOVE MQ-COMPCODE          TO RC-DISPLAY                            
             MOVE MQ-REASON            TO REASON-DISPLAY                        
             MOVE SPACE                TO ERROR-TEXT-STR                        
             STRING 'RC FROM MQOPEN ' RC-DISPLAY                                
                    ' REASON: ' REASON-DISPLAY                                  
                    ' ' MQ-OBJNAME                                              
                DELIMITED BY SIZE                                               
                                     INTO ERROR-TEXT-STR                        
             DISPLAY IDPGM ' ' ERROR-TEXT                                       
             PERFORM S99-ABEND                                                  
           END-IF                                                               
      D    DISPLAY IDPGM ' MQOPEN SUCCESSFUL    '                               
      *    -- INITIATE A NEW FRESH MESSAGE                                      
           MOVE ZERO                   TO WLEN                                  
           MOVE SPACES                 TO WDATA                                 
           .                                                                    
       S41-MQ-OPEN-TNT SECTION.                                                 
           MOVE 'VCC.TNT.LOGEVENT'     TO TTOD-OBJECTNAME                       
                                                                                
           MOVE MQOO-FAIL-IF-QUIESCING TO TNT-OPENOPTIONS                       
           ADD MQOO-OUTPUT             TO TNT-OPENOPTIONS                       
                                                                                
           IF MODE-ONLINE                                                       
             CALL 'MQOPEN'          USING MQ-HCONN                              
                                          TTOD                                  
                                          TNT-OPENOPTIONS                       
                                          TNT-HOBJ                              
                                          MQ-COMPCODE                           
                                          MQ-REASON                             
           ELSE                                                                 
             CALL MQOPEN            USING MQ-HCONN                              
                                          TTOD                                  
                                          TNT-OPENOPTIONS                       
                                          TNT-HOBJ                              
                                          MQ-COMPCODE                           
                                          MQ-REASON                             
           END-IF                                                               
                                                                                
           IF MQ-COMPCODE NOT = MQCC-OK                                         
             MOVE MQ-COMPCODE          TO RC-DISPLAY                            
             MOVE MQ-REASON            TO REASON-DISPLAY                        
             MOVE SPACE                TO ERROR-TEXT-STR                        
             STRING 'RC FROM TNT MQOPEN ' RC-DISPLAY                            
                    ' REASON: ' REASON-DISPLAY                                  
                    ' ' TTOD-OBJECTNAME                                         
                DELIMITED BY SIZE                                               
                                     INTO ERROR-TEXT-STR                        
             DISPLAY IDPGM ' ' ERROR-TEXT                                       
      D      PERFORM S99-ABEND                                                  
           END-IF                                                               
      D    DISPLAY IDPGM ' TNT MQOPEN SUCCESSFUL    '                           
           .                                                                    
                                                                                
       S41-MQ-CREATE-MSGHANDLE SECTION.                                         
                                                                                
      *    -- FIRST, CREATE A HANDLE FOR HEADER PROPERTIES                      
           MOVE MQCMHO-VALIDATE        TO MQCMHO-OPTIONS                        
           IF MODE-ONLINE                                                       
             CALL 'MQCRTMH'         USING MQ-HCONN                              
                                          MQCMHO                                
                                          MQ-PROPHANDLE                         
                                          MQ-COMPCODE                           
                                          MQ-REASON                             
           ELSE                                                                 
             CALL MQCRTMH           USING MQ-HCONN                              
                                          MQCMHO                                
                                          MQ-PROPHANDLE                         
                                          MQ-COMPCODE                           
                                          MQ-REASON                             
           END-IF                                                               
                                                                                
           IF MQ-COMPCODE NOT = MQCC-OK                                         
             MOVE MQ-COMPCODE          TO RC-DISPLAY                            
             MOVE MQ-REASON            TO REASON-DISPLAY                        
                                                                                
             MOVE SPACE                TO ERROR-TEXT-STR                        
             STRING 'RC FROM MQCRTMH' RC-DISPLAY                                
                    ' REASON: ' REASON-DISPLAY                                  
                    ' ' MQ-OBJNAME                                              
                DELIMITED BY SIZE    INTO ERROR-TEXT-STR                        
                                                                                
             DISPLAY IDPGM ' ' ERROR-TEXT                                       
             PERFORM S99-ABEND                                                  
           END-IF                                                               
           .                                                                    
                                                                                
       S41-MQ-SET-MSG-PROPERTY SECTION.                                         
                                                                                
           SET MQCHARV-VSPTR           TO ADDRESS OF MQ-PROPNAME-TO-SET         
                                                                                
           MOVE SWEDISH-EBCDIC         TO MQCHARV-VSCCSID                       
           MOVE MQTYPE-STRING          TO MQ-PROPTYPE                           
                                                                                
           MOVE ZERO                   TO MQCHARV-VSLENGTH                      
           COMPUTE MQCHARV-VSLENGTH     = FUNCTION LENGTH (                     
                                          FUNCTION TRIM (                       
                                            MQ-PROPNAME-TO-SET))                
                                                                                
           MOVE ZERO                   TO MQ-PROPVALUELENGTH                    
           COMPUTE MQ-PROPVALUELENGTH   = FUNCTION LENGTH (                     
                                          FUNCTION TRIM (                       
                                            MQ-PROPVALUE))                      
                                                                                
           IF MODE-ONLINE                                                       
             CALL 'MQSETMP'         USING MQ-HCONN                              
                                          MQ-PROPHANDLE                         
                                          MQSMPO                                
                                          MQCHARV                               
                                          MQPD                                  
                                          MQ-PROPTYPE                           
                                          MQ-PROPVALUELENGTH                    
                                          MQ-PROPVALUE                          
                                          MQ-COMPCODE                           
                                          MQ-REASON                             
           ELSE                                                                 
             CALL MQSETMP           USING MQ-HCONN                              
                                          MQ-PROPHANDLE                         
                                          MQSMPO                                
                                          MQCHARV                               
                                          MQPD                                  
                                          MQ-PROPTYPE                           
                                          MQ-PROPVALUELENGTH                    
                                          MQ-PROPVALUE                          
                                          MQ-COMPCODE                           
                                          MQ-REASON                             
           END-IF                                                               
                                                                                
           IF MQ-COMPCODE NOT = MQCC-OK                                         
             MOVE MQ-COMPCODE          TO RC-DISPLAY                            
             MOVE MQ-REASON            TO REASON-DISPLAY                        
             MOVE SPACE                TO ERROR-TEXT-STR                        
             STRING 'RC FROM MQSETMP ' RC-DISPLAY                               
                    ' REASON: ' REASON-DISPLAY                                  
                    ' FOR ' MQ-PROPNAME-TO-SET                                  
                  DELIMITED BY SIZE  INTO ERROR-TEXT-STR                        
             DISPLAY IDPGM ' ' ERROR-TEXT                                       
             PERFORM S99-ABEND                                                  
           END-IF                                                               
                                                                                
                                                                                
           .                                                                    
                                                                                
       S41-MQ-PUT SECTION.                                                      
                                                                                
           IF MSG-GRP-VALID                                                     
             SET LAST-MSG-IN-GRP       TO TRUE                                  
             PERFORM S41-MQ-PUT-GROUP                                           
           ELSE                                                                 
             PERFORM S41-MQ-PUT-SINGLE                                          
           END-IF                                                               
                                                                                
      *    -- INITIATE A NEW FRESH MESSAGE                                      
           INITIALIZE WLEN                                                      
                      WDATA                                                     
           .                                                                    
                                                                                
       S41-MQ-PUT-SINGLE SECTION.                                               
                                                                                
           IF WS-EOF-LEN > ZERO                                                 
      *    Add end-of-file at the end. Not applicable if its a group            
      *    message and the file is not being split at end of a record,          
      *    unless its the last message in group                                 
             IF MSG-GRP-YES AND MSG-SPLIT-NO                                    
               IF LAST-MSG-IN-GRP                                               
                 COMPUTE WSTART = WLEN + 1                                      
                 STRING WS-EOF DELIMITED BY SPACE                               
                                     INTO WDATA                                 
                             WITH POINTER WSTART                                
                 COMPUTE WLEN = WSTART - 1                                      
               END-IF                                                           
             ELSE                                                               
               COMPUTE WSTART = WLEN + 1                                        
               STRING WS-EOF DELIMITED BY SPACE                                 
                                     INTO WDATA                                 
                             WITH POINTER WSTART                                
               COMPUTE WLEN = WSTART - 1                                        
             END-IF                                                             
           END-IF                                                               
                                                                                
      *    -- PUT THE MESSAGE                                                   
                                                                                
           COMPUTE MQPMO-OPTIONS        = MQPMO-FAIL-IF-QUIESCING               
                                                                                
           MOVE MQMI-NONE              TO MQMD-MSGID                            
           MOVE MQCI-NONE              TO MQMD-CORRELID                         
           MOVE MQ-MSG-FORMAT          TO MQMD-FORMAT                           
           MOVE MQENC-NATIVE           TO MQMD-ENCODING                         
           MOVE MQ-KDCCSID             TO MQMD-CODEDCHARSETID                   
                                                                                
           MOVE MQPMO-VERSION-3        TO MQPMO-VERSION                         
           MOVE MQ-PROPHANDLE          TO MQPMO-ORIGINALMSGHANDLE               
           MOVE MQACTP-NEW             TO MQPMO-ACTION                          
                                                                                
           MOVE WLEN                   TO MQ-MSGLENGTH                          
                                                                                
           IF MODE-ONLINE                                                       
             CALL 'MQPUT'           USING MQ-HCONN                              
                                          MQ-HOBJ                               
                                          MQMD                                  
                                          MQPMO                                 
                                          MQ-MSGLENGTH                          
                                          WDATA                                 
                                          MQ-COMPCODE                           
                                          MQ-REASON                             
           ELSE                                                                 
             CALL MQPUT             USING MQ-HCONN                              
                                          MQ-HOBJ                               
                                          MQMD                                  
                                          MQPMO                                 
                                          MQ-MSGLENGTH                          
                                          WDATA                                 
                                          MQ-COMPCODE                           
                                          MQ-REASON                             
           END-IF                                                               
                                                                                
           IF MQ-COMPCODE NOT = MQCC-OK                                         
             MOVE MQ-COMPCODE          TO RC-DISPLAY                            
             MOVE MQ-REASON            TO REASON-DISPLAY                        
             MOVE SPACE                TO ERROR-TEXT-STR                        
             STRING 'RC FROM MQPUT '   RC-DISPLAY                               
                    ' REASON: ' REASON-DISPLAY                                  
                DELIMITED BY SIZE    INTO ERROR-TEXT-STR                        
             DISPLAY IDPGM ' ' ERROR-TEXT                                       
             PERFORM S99-ABEND                                                  
           END-IF                                                               
      D    DISPLAY IDPGM ' MQPUT   SUCCESSFUL   '                               
           MOVE 'MQPUT'                TO TNT-KDFUNC                            
                                                                                
           PERFORM T-TRACK-AND-TRACE                                            
           .                                                                    
                                                                                
       S41-MQ-PUT-GROUP  SECTION.                                               
                                                                                
           IF WS-EOF-LEN > ZERO                                                 
      *    Add end-of-file at the end. Not applicable if its a group            
      *    message and the file is not being split at end of a record,          
      *    unless its the last message in group                                 
             IF MSG-GRP-YES AND MSG-SPLIT-NO                                    
               IF LAST-MSG-IN-GRP                                               
                 COMPUTE WSTART = WLEN + 1                                      
                 STRING WS-EOF DELIMITED BY SPACE                               
                                     INTO WDATA                                 
                             WITH POINTER WSTART                                
                 COMPUTE WLEN = WSTART - 1                                      
               END-IF                                                           
             ELSE                                                               
               COMPUTE WSTART = WLEN + 1                                        
               STRING WS-EOF DELIMITED BY SPACE                                 
                                     INTO WDATA                                 
                             WITH POINTER WSTART                                
               COMPUTE WLEN = WSTART - 1                                        
             END-IF                                                             
           END-IF                                                               
                                                                                
      *    -- PUT THE MESSAGE                                                   
                                                                                
           COMPUTE MQPMO-OPTIONS        = MQPMO-FAIL-IF-QUIESCING               
                                        + MQPMO-LOGICAL-ORDER                   
                                        + MQPMO-NEW-MSG-ID                      
                                                                                
           MOVE MQMI-NONE              TO MQMD-MSGID                            
           MOVE MQCI-NONE              TO MQMD-CORRELID                         
           MOVE MQFMT-STRING           TO MQMD-FORMAT                           
           MOVE MQENC-NATIVE           TO MQMD-ENCODING                         
           MOVE MQ-KDCCSID             TO MQMD-CODEDCHARSETID                   
                                                                                
           MOVE MQPMO-VERSION-3        TO MQPMO-VERSION                         
           MOVE MQ-PROPHANDLE          TO MQPMO-ORIGINALMSGHANDLE               
           MOVE MQACTP-NEW             TO MQPMO-ACTION                          
                                                                                
           EVALUATE TRUE                                                        
             WHEN FIRST-MSG-IN-GRP                                              
               MOVE 1                  TO MQMD-MSGSEQNUMBER                     
               MOVE MQMF-MSG-IN-GROUP  TO MQMD-MSGFLAGS                         
               SET MSG-IN-GRP          TO TRUE                                  
             WHEN LAST-MSG-IN-GRP                                               
               ADD 1                   TO MQMD-MSGSEQNUMBER                     
               MOVE MQMF-LAST-MSG-IN-GROUP                                      
                                       TO MQMD-MSGFLAGS                         
             WHEN OTHER                                                         
               ADD 1                   TO MQMD-MSGSEQNUMBER                     
               MOVE MQMF-MSG-IN-GROUP  TO MQMD-MSGFLAGS                         
           END-EVALUATE                                                         
                                                                                
           MOVE WLEN                   TO MQ-MSGLENGTH                          
                                                                                
           IF MODE-ONLINE                                                       
             CALL 'MQPUT'           USING MQ-HCONN                              
                                          MQ-HOBJ                               
                                          MQMD                                  
                                          MQPMO                                 
                                          MQ-MSGLENGTH                          
                                          WDATA                                 
                                          MQ-COMPCODE                           
                                          MQ-REASON                             
           ELSE                                                                 
             CALL MQPUT             USING MQ-HCONN                              
                                          MQ-HOBJ                               
                                          MQMD                                  
                                          MQPMO                                 
                                          MQ-MSGLENGTH                          
                                          WDATA                                 
                                          MQ-COMPCODE                           
                                          MQ-REASON                             
           END-IF                                                               
                                                                                
           IF MQ-COMPCODE NOT = MQCC-OK                                         
             MOVE MQ-COMPCODE          TO RC-DISPLAY                            
             MOVE MQ-REASON            TO REASON-DISPLAY                        
             MOVE SPACE                TO ERROR-TEXT-STR                        
             STRING 'RC FROM MQPUT GROUP '   RC-DISPLAY                         
                    ' REASON: ' REASON-DISPLAY                                  
                DELIMITED BY SIZE    INTO ERROR-TEXT-STR                        
             DISPLAY IDPGM ' ' ERROR-TEXT                                       
             PERFORM S99-ABEND                                                  
           END-IF                                                               
      D    DISPLAY IDPGM ' MQPUT   SUCCESSFUL   '                               
                                                                                
           PERFORM T-TRACK-AND-TRACE                                            
           .                                                                    
                                                                                
       S41-MQ-PUT-TNT SECTION.                                                  
      *    -- PUT THE TNT MESSAGE                                               
                                                                                
           COMPUTE TTPMO-OPTIONS        = MQPMO-FAIL-IF-QUIESCING               
                                                                                
           MOVE MQMI-NONE              TO TTMD-MSGID                            
           MOVE MQCI-NONE              TO TTMD-CORRELID                         
           MOVE MQFMT-NONE             TO TTMD-FORMAT                           
           MOVE 1208                   TO TTMD-CODEDCHARSETID                   
                                                                                
           MOVE MQPMO-VERSION-3        TO TTPMO-VERSION                         
           MOVE MQACTP-NEW             TO TTPMO-ACTION                          
                                                                                
           MOVE MQ-TNT-MSG-LEN         TO MQ-MSGLENGTH                          
                                                                                
           IF MODE-ONLINE                                                       
             CALL 'MQPUT'           USING MQ-HCONN                              
                                          TNT-HOBJ                              
                                          TTMD                                  
                                          TTPMO                                 
                                          MQ-MSGLENGTH                          
                                          MQ-TNT-MSG                            
                                          MQ-COMPCODE                           
                                          MQ-REASON                             
           ELSE                                                                 
             CALL MQPUT             USING MQ-HCONN                              
                                          TNT-HOBJ                              
                                          TTMD                                  
                                          TTPMO                                 
                                          MQ-MSGLENGTH                          
                                          MQ-TNT-MSG                            
                                          MQ-COMPCODE                           
                                          MQ-REASON                             
           END-IF                                                               
                                                                                
           IF MQ-COMPCODE NOT = MQCC-OK                                         
             MOVE MQ-COMPCODE          TO RC-DISPLAY                            
             MOVE MQ-REASON            TO REASON-DISPLAY                        
             MOVE SPACE                TO ERROR-TEXT-STR                        
             STRING 'RC FROM TNT MQPUT '   RC-DISPLAY                           
                    ' REASON: ' REASON-DISPLAY                                  
                DELIMITED BY SIZE    INTO ERROR-TEXT-STR                        
             DISPLAY IDPGM ' ' ERROR-TEXT                                       
      D      PERFORM S99-ABEND                                                  
           END-IF                                                               
      D    DISPLAY IDPGM ' TNT MQPUT   SUCCESSFUL   '                           
           .                                                                    
                                                                                
       S41-MQ-GET SECTION.                                                      
      *    -- GET THE MESSAGE                                                   
                                                                                
           COMPUTE MQGMO-OPTIONS        = MQGMO-FAIL-IF-QUIESCING               
                                        + MQGMO-NO-WAIT                         
                                        + MQGMO-CONVERT                         
                                        + MQGMO-PROPERTIES-IN-HANDLE            
           IF MSG-GRP-YES                                                       
             COMPUTE MQGMO-OPTIONS      = MQGMO-OPTIONS                         
                                        + MQGMO-LOGICAL-ORDER                   
                                        + MQGMO-ALL-MSGS-AVAILABLE              
           END-IF                                                               
                                                                                
           MOVE MQ-PROPHANDLE          TO MQGMO-MSGHANDLE                       
                                                                                
           MOVE MQMI-NONE              TO MQMD-MSGID                            
           MOVE MQMI-NONE              TO MQMD-CORRELID                         
           MOVE MQFMT-STRING           TO MQMD-FORMAT                           
           MOVE MQENC-NATIVE           TO MQMD-ENCODING                         
           MOVE MQCCSI-Q-MGR           TO MQMD-CODEDCHARSETID                   
                                                                                
           MOVE MQGMO-VERSION-4        TO MQGMO-VERSION                         
                                                                                
           MOVE LENGTH OF WDATA        TO MQ-MSGLENGTH                          
                                                                                
           IF MODE-ONLINE                                                       
             CALL 'MQGET'           USING MQ-HCONN                              
                                          MQ-HOBJ                               
                                          MQMD                                  
                                          MQGMO                                 
                                          MQ-MSGLENGTH                          
                                          WDATA                                 
                                          MQ-DATALENGTH                         
                                          MQ-COMPCODE                           
                                          MQ-REASON                             
           ELSE                                                                 
             CALL MQGET             USING MQ-HCONN                              
                                          MQ-HOBJ                               
                                          MQMD                                  
                                          MQGMO                                 
                                          MQ-MSGLENGTH                          
                                          WDATA                                 
                                          MQ-DATALENGTH                         
                                          MQ-COMPCODE                           
                                          MQ-REASON                             
           END-IF                                                               
                                                                                
           IF MQ-COMPCODE NOT = MQCC-OK                                         
             MOVE MQ-COMPCODE          TO RC-DISPLAY                            
             MOVE MQ-REASON            TO REASON-DISPLAY                        
             MOVE SPACE                TO ERROR-TEXT-STR                        
             STRING 'RC FROM MQGET '   RC-DISPLAY                               
                    ' REASON: ' REASON-DISPLAY                                  
                DELIMITED BY SIZE    INTO ERROR-TEXT-STR                        
             PERFORM S99-ABEND                                                  
           ELSE                                                                 
             IF MQ-DATALENGTH > LENGTH OF WDATA                                 
               MOVE MQ-DATALENGTH      TO RC-DISPLAY                            
               MOVE SPACE              TO ERROR-TEXT-STR                        
               STRING 'MQGET ERROR. MSG TOO LONG'                               
                      '. MSG LENGTH: ' RC-DISPLAY                               
                      '. MAX ALLOWED: ' LENGTH OF WDATA                         
                  DELIMITED BY SIZE  INTO ERROR-TEXT-STR                        
                                                                                
               PERFORM S99-ABEND                                                
             ELSE                                                               
               MOVE MQ-DATALENGTH      TO WLEN                                  
               MOVE 1                  TO WSTART                                
             END-IF                                                             
           END-IF                                                               
      D    DISPLAY IDPGM ' MQGET   SUCCESSFUL   '                               
           MOVE 'MQGET'                TO TNT-KDFUNC                            
           .                                                                    
                                                                                
       S41-GET-ALL-PROPERTIES SECTION.                                          
           MOVE SPACES                 TO TNT-INTEGRATION-ID                    
           MOVE SPACES                 TO TNT-CONTRACT-ID                       
           SET INQ-FIRST               TO TRUE                                  
           MOVE '%'                    TO MQ-PROPNAME-TO-RETRIEVE               
           PERFORM S41-MQ-INQ-PROPERTY                                          
           SET INQ-NEXT                TO TRUE                                  
           PERFORM UNTIL PROPERTY-DOESNT-EXIST                                  
             PERFORM S41A-SAVE-PROPERTIES                                       
             MOVE '%'                  TO MQ-PROPNAME-TO-RETRIEVE               
             PERFORM S41-MQ-INQ-PROPERTY                                        
           END-PERFORM                                                          
           .                                                                    
                                                                                
       S41A-SAVE-PROPERTIES SECTION.                                            
                                                                                
      *    Not all properties need to be saved. Lets exclude some               
      *    known common properties so we capture only business                  
      *    related items.                                                       
           MOVE FUNCTION TRIM(                                                  
                FUNCTION UPPER-CASE (MQ-PROPNAME-RETRIEVED))                    
                                       TO WS-PROPNAME-RETRIEVED                 
           IF WS-PROPNAME-RETRIEVED (1:11) = 'INTEGRATION'  OR                  
              WS-PROPNAME-RETRIEVED (1:8)  = 'CONTRACT'  OR                     
              WS-PROPNAME-RETRIEVED (1:2)  = 'MQ'  OR                           
              WS-PROPNAME-RETRIEVED (1:4)  = 'VCOM'  OR                         
              WS-PROPNAME-RETRIEVED (1:4)  = 'VIDB'                             
             CONTINUE                                                           
           ELSE                                                                 
             IF WS-PROP-IX < WS-PROP-IX-MAX                                     
               ADD +1                  TO WS-PROP-IX                            
               MOVE WS-PROPNAME-RETRIEVED                                       
                                       TO WS-PROP-NAME (WS-PROP-IX)             
               MOVE FUNCTION TRIM (MQ-PROPVALUE)                                
                                       TO WS-PROP-VALUE (WS-PROP-IX)            
             END-IF                                                             
           END-IF                                                               
           .                                                                    
                                                                                
       S41-MQ-INQ-PROPERTY SECTION.                                             
                                                                                
           SET PROPERTY-EXIST          TO TRUE                                  
           MOVE SWEDISH-EBCDIC         TO MQIMPO-REQUESTEDCCSID                 
           MOVE MQTYPE-STRING          TO MQ-PROPTYPE                           
                                                                                
           MOVE MQIMPO-CONVERT-VALUE   TO MQIMPO-OPTIONS                        
           ADD MQIMPO-CONVERT-TYPE     TO MQIMPO-OPTIONS                        
           IF INQ-NEXT                                                          
             ADD MQIMPO-INQ-NEXT       TO MQIMPO-OPTIONS                        
           ELSE                                                                 
             ADD MQIMPO-INQ-FIRST      TO MQIMPO-OPTIONS                        
           END-IF                                                               
                                                                                
           MOVE ZERO                   TO MQCHARV-VSLENGTH                      
           INSPECT MQ-PROPNAME-TO-RETRIEVE                                      
                                 TALLYING MQCHARV-VSLENGTH                      
                   FOR CHARACTERS BEFORE INITIAL SPACE                          
                                                                                
           SET MQCHARV-VSPTR           TO ADDRESS                               
                                       OF MQ-PROPNAME-TO-RETRIEVE               
           MOVE LENGTH OF MQ-PROPNAME-TO-RETRIEVE                               
                                       TO MQCHARV-VSBUFSIZE                     
                                                                                
           MOVE SPACES                 TO MQ-PROPNAME-RETRIEVED                 
                                          MQ-PROPVALUE                          
                                                                                
           SET MQIMPO-RETURNEDNAME-VSPTR                                        
                                       TO ADDRESS                               
                                       OF MQ-PROPNAME-RETRIEVED                 
           MOVE LENGTH OF MQ-PROPNAME-RETRIEVED                                 
                                       TO MQIMPO-RETURNEDNAME-VSBUFSIZE         
                                                                                
                                                                                
           MOVE LENGTH OF MQ-PROPVALUE TO MQ-PROPVALUELENGTH                    
                                                                                
           IF MODE-ONLINE                                                       
             CALL 'MQINQMP'         USING MQ-HCONN                              
                                          MQ-PROPHANDLE                         
                                          MQIMPO                                
                                          MQCHARV                               
                                          MQPD                                  
                                          MQ-PROPTYPE                           
                                          MQ-PROPVALUELENGTH                    
                                          MQ-PROPVALUE                          
                                          MQ-PROPDATALENGTH                     
                                          MQ-COMPCODE                           
                                          MQ-REASON                             
           ELSE                                                                 
             CALL MQINQMP           USING MQ-HCONN                              
                                          MQ-PROPHANDLE                         
                                          MQIMPO                                
                                          MQCHARV                               
                                          MQPD                                  
                                          MQ-PROPTYPE                           
                                          MQ-PROPVALUELENGTH                    
                                          MQ-PROPVALUE                          
                                          MQ-PROPDATALENGTH                     
                                          MQ-COMPCODE                           
                                          MQ-REASON                             
           END-IF                                                               
                                                                                
           IF MQ-COMPCODE = MQCC-OK                                             
             MOVE SPACES               TO DISPLAY-TEXT                          
             STRING MQ-PROPNAME-RETRIEVED DELIMITED BY SPACE                    
                    ' = '                 DELIMITED BY SIZE                     
                    FUNCTION TRIM(MQ-PROPVALUE)                                 
                                          DELIMITED BY SIZE                     
                                     INTO DISPLAY-TEXT                          
             IF MODE-BATCH                                                      
               DISPLAY IDPGM ' MQ PROP=' DISPLAY-TEXT                           
             END-IF                                                             
           ELSE                                                                 
             SET PROPERTY-DOESNT-EXIST TO TRUE                                  
             IF MQ-REASON = MQRC-PROPERTY-NOT-AVAILABLE                         
               IF MQ-PROPNAME-TO-RETRIEVE = '%'                                 
                 CONTINUE                                                       
               ELSE                                                             
                 IF MODE-BATCH                                                  
                   DISPLAY IDPGM ' PROP '                                       
                                FUNCTION TRIM (MQ-PROPNAME-TO-RETRIEVE)         
                                 ' NOT AVAILABLE'                               
                 END-IF                                                         
               END-IF                                                           
             ELSE                                                               
               MOVE MQ-COMPCODE        TO RC-DISPLAY                            
               MOVE MQ-REASON          TO REASON-DISPLAY                        
               MOVE SPACE              TO ERROR-TEXT-STR                        
               STRING 'RC FROM MQINQMP ' RC-DISPLAY                             
                      ' REASON: ' REASON-DISPLAY                                
                  DELIMITED BY SIZE  INTO ERROR-TEXT-STR                        
               PERFORM S99-ABEND                                                
             END-IF                                                             
           END-IF                                                               
                                                                                
           IF MQ-PROPNAME-RETRIEVED = 'IntegrationId'                           
             MOVE MQ-PROPVALUE         TO TNT-INTEGRATION-ID                    
      D      DISPLAY IDPGM ' TNT IntegrationId=' TNT-INTEGRATION-ID             
           END-IF                                                               
           IF MQ-PROPNAME-RETRIEVED = 'ContractId'                              
             MOVE MQ-PROPVALUE         TO TNT-CONTRACT-ID                       
      D      DISPLAY IDPGM ' TNT ContractId=' TNT-CONTRACT-ID                   
           END-IF                                                               
                                                                                
      D    DISPLAY IDPGM ' INQ PROPERTIES SUCCESSFUL'                           
                                                                                
           .                                                                    
                                                                                
       S41-MQ-CLOSEQ  SECTION.                                                  
                                                                                
           IF MQ-DIRECTION-OUT AND                                              
              WLEN  > 0                                                         
      *      -- PUT THE LAST MESSAGE TO QUEUE                                   
             PERFORM S41-MQ-PUT                                                 
           END-IF                                                               
                                                                                
           IF MQ-DIRECTION-IN                                                   
             PERFORM T-TRACK-AND-TRACE                                          
           END-IF                                                               
                                                                                
      *    -- FINALLY, CLOSE THE QUEUE                                          
           IF MODE-ONLINE                                                       
             CALL 'MQCLOSE'         USING MQ-HCONN                              
                                          MQ-HOBJ                               
                                          MQCO-NONE                             
                                          MQ-COMPCODE                           
                                          MQ-REASON                             
           ELSE                                                                 
             CALL MQCLOSE           USING MQ-HCONN                              
                                          MQ-HOBJ                               
                                          MQCO-NONE                             
                                          MQ-COMPCODE                           
                                          MQ-REASON                             
           END-IF                                                               
                                                                                
           IF MQ-COMPCODE NOT = MQCC-OK                                         
             MOVE MQ-COMPCODE          TO RC-DISPLAY                            
             MOVE MQ-REASON            TO REASON-DISPLAY                        
             MOVE SPACE                TO ERROR-TEXT-STR                        
             STRING 'RC FROM MQCLOSE ' RC-DISPLAY                               
                    ' REASON: ' REASON-DISPLAY                                  
                DELIMITED BY SIZE    INTO ERROR-TEXT-STR                        
             DISPLAY IDPGM ' ' ERROR-TEXT                                       
             PERFORM S99-ABEND                                                  
           END-IF                                                               
                                                                                
      D    DISPLAY IDPGM ' MQCLOSE SUCCESSFUL   '                               
           .                                                                    
                                                                                
       S41-MQ-CLOSEQ-TNT  SECTION.                                              
                                                                                
           IF MODE-ONLINE                                                       
             CALL 'MQCLOSE'         USING MQ-HCONN                              
                                          TNT-HOBJ                              
                                          MQCO-NONE                             
                                          MQ-COMPCODE                           
                                          MQ-REASON                             
           ELSE                                                                 
             CALL MQCLOSE           USING MQ-HCONN                              
                                          TNT-HOBJ                              
                                          MQCO-NONE                             
                                          MQ-COMPCODE                           
                                          MQ-REASON                             
           END-IF                                                               
                                                                                
           IF MQ-COMPCODE NOT = MQCC-OK                                         
             MOVE MQ-COMPCODE          TO RC-DISPLAY                            
             MOVE MQ-REASON            TO REASON-DISPLAY                        
             MOVE SPACE                TO ERROR-TEXT-STR                        
             STRING 'RC FROM TNT MQCLOSE ' RC-DISPLAY                           
                    ' REASON: ' REASON-DISPLAY                                  
                DELIMITED BY SIZE    INTO ERROR-TEXT-STR                        
             DISPLAY IDPGM ' ' ERROR-TEXT                                       
      D      PERFORM S99-ABEND                                                  
           END-IF                                                               
                                                                                
      D    DISPLAY IDPGM ' TNT MQCLOSE SUCCESSFUL   '                           
           .                                                                    
                                                                                
       S41-MQ-DISC  SECTION.                                                    
                                                                                
      *    -- BREAK CONTACT WITH MQ QUEUE MANAGER                               
                                                                                
           IF MODE-ONLINE                                                       
             CALL 'MQDISC'          USING MQ-HCONN                              
                                          MQ-COMPCODE                           
                                          MQ-REASON                             
           ELSE                                                                 
             CALL MQDISC            USING MQ-HCONN                              
                                          MQ-COMPCODE                           
                                          MQ-REASON                             
           END-IF                                                               
                                                                                
           IF MQ-COMPCODE NOT = MQCC-OK                                         
             MOVE MQ-COMPCODE          TO RC-DISPLAY                            
             MOVE MQ-REASON            TO REASON-DISPLAY                        
             MOVE SPACE                TO ERROR-TEXT-STR                        
             STRING 'RC FROM MQDISC '  RC-DISPLAY                               
                    ' REASON: ' REASON-DISPLAY                                  
                DELIMITED BY SIZE    INTO ERROR-TEXT-STR                        
             DISPLAY IDPGM ' ' ERROR-TEXT                                       
             PERFORM S99-ABEND                                                  
           END-IF                                                               
                                                                                
      D    DISPLAY IDPGM ' MQDISC SUCCESSFUL    '                               
           .                                                                    
                                                                                
       IMS-GU-ADDRESS-INFO SECTION.                                             
                                                                                
           STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-X ')'                         
                    DELIMITED BY SIZE INTO SSA1                                 
           STRING 'WDGX0104(ADDISPAB =' W-ADDISPABS-X ')'                       
                    DELIMITED BY SIZE INTO SSA2                                 
                                                                                
           MOVE 128                    TO AIB-LEN                               
           MOVE SPACES                 TO AIB-SUB-FUNCTION                      
           MOVE 'ATAB'                 TO AIB-PCB-NAME                          
           MOVE LENGTH OF DLI-IO-AREA  TO AIB-IOAREA-LENGTH                     
                                                                                
           MOVE '  GE'                 TO GOOD-STATUS-CODES                     
                                                                                
           CALL AIBTDLI             USING GU                                    
                                          AIB-AREA                              
                                          DLI-IO-AREA                           
                                          SSA1                                  
                                          SSA2                                  
                                                                                
           SET ADDRESS OF WDR5-PCB     TO AIB-PCB-PTR                           
           MOVE WDR5-STATUS-CODE       TO STATUS-WS                             
           PERFORM IMS-STATUS-CHECK                                             
           .                                                                    
                                                                                
       IMS-STATUS-CHECK SECTION.                                                
                                                                                
           IF STATUS-WS = LOW-VALUE                                             
             STRING ' CAN NOT FIND PCB WITH NAME: ATAB IN THE PSB'              
                   DELIMITED BY SIZE INTO ERROR-TEXT                            
             DISPLAY IDPGM ' ' ERROR-TEXT                                       
             PERFORM S99-ABEND                                                  
           ELSE                                                                 
             SET STATUS-IX             TO 1                                     
             SEARCH GOOD-STATUS                                                 
               AT END                                                           
                 STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS             
                   DELIMITED BY SIZE INTO ERROR-TEXT                            
                 DISPLAY IDPGM ' ' ERROR-TEXT                                   
                 PERFORM S99-ABEND                                              
               WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                         
                 CONTINUE                                                       
             END-SEARCH                                                         
           END-IF                                                               
           .                                                                    
                                                                                
       S99-ABEND SECTION.                                                       
                                                                                
           DISPLAY IDPGM ' ' ERROR-TEXT                                         
           CALL ABEND               USING RKOD-ABEND-NO-DUMP                    
           .                                                                    
