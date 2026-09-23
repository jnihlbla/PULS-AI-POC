      *COMPOPT VPOSIX=YES                                                       
       ID DIVISION.                                                             
       PROGRAM-ID.     W4067900.                                                
       AUTHOR.         RAHUL REDDY.                                             
       DATE-WRITTEN.   25/01/29.                                                
       DATE-COMPILED.                                                           
                                                                                
      *    NAME:       CARPARTS.PULS.SHIPMENTTOTMS                              
      *                                                                         
      *    FUNCTION:                                                            
      *        PROGRAM TO SEND SHIPMENT INFO TO TMS SYSTEMS                     
      *                                                                         
      *        THE PROGRAM READS     WDE1 WDD3 WDK6 WDR2                        
      *        THE PROGRAM UPDATES   XXXX                                       
      *                                                                         
      *    INDATA.                                                              
      *        TRANSACTION: W40679X                                             
      *        REQUEST:     W40679I1                                            
      *                                                                         
      *    OUTDATA.                                                             
      *        RESPONSE:    XXXXXXXX                                            
                                                                                
       ENVIRONMENT DIVISION.                                                    
       INPUT-OUTPUT SECTION.                                                    
       FILE-CONTROL.                                                            
       DATA DIVISION.                                                           
       FILE SECTION.                                                            
       WORKING-STORAGE SECTION.                                                 
       77  IDPGM                       PIC X(08)   VALUE 'W4067900'.            
                                                                                
      *    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
       01  FILLER                      PIC X(16)   VALUE 'ERROR-TEXT:'.         
       01  ERROR-TEXT                  PIC X(100).                              
                                                                                
       01  MESSAGE-TEXT-GRP.                                                    
           03  MESSAGE-TEXT            PIC X(100) OCCURS 10 TIMES.              
       01  IX-M                        PIC S9(9) COMP.                          
       77  WS-MESSAGE                  PIC X(5120).                             
       77  WS-IDTONR-NI                PIC S9(4) COMP.                          
                                                                                
       77  KDRC-DISPLAY                PIC Z(5).                                
       77  WS-SECTION                  PIC X(50).                               
       77  FELTEXT                     PIC X(80) VALUE SPACE.                   
                                                                                
       77  YES                         PIC X       VALUE 'J'.                   
       77  NOO                         PIC X       VALUE 'N'.                   
                                                                                
       77  IX                          PIC S9(9) COMP.                          
       77  IX-F                        PIC S9(9) COMP.                          
       77  IX-K                        PIC S9(9) COMP.                          
       77  IX-A                        PIC S9(9) COMP.                          
       77  TIX-K                       PIC S9(9) COMP.                          
       77  TIX-A                       PIC S9(9) COMP.                          
                                                                                
       01  WS-DATE-FIELDS.                                                      
           03  WS-DATE                 PIC 9(8).                                
           03  FILLER REDEFINES WS-DATE.                                        
               05  WS-DATE-CC          PIC 9(2).                                
               05  WS-DATE-YYMMDD      PIC 9(6).                                
           03  WS-TIME                 PIC 9(6).                                
           03  WS-TIME-X REDEFINES WS-TIME.                                     
               05  WS-TIME-HHMM        PIC 9(4).                                
               05  WS-TIME-SS          PIC 9(2).                                
                                                                                
       01  WS-DATE-UTC-FIELDS.                                                  
           03  WS-DATE-UTC             PIC 9(8).                                
           03  FILLER REDEFINES WS-DATE-UTC.                                    
               05  WS-DATE-UTC-CC      PIC 9(2).                                
               05  WS-DATE-UTC-YYMMDD  PIC 9(6).                                
           03  WS-TIME-UTC             PIC 9(6).                                
           03  WS-TIME-UTC-X REDEFINES WS-TIME-UTC.                             
               05  WS-TIME-UTC-HHMM    PIC 9(4).                                
               05  WS-TIME-UTC-SS      PIC 9(2).                                
                                                                                
       77  WS-TIMESTAMP-UTC            PIC X(20).                               
                                                                                
       01  WS-PACKAGE-ID.                                                       
           03  WS-IDDISTR              PIC 9(4).                                
           03  WS-IDKUNDNR             PIC 9(6).                                
           03  WS-IDORDNR7             PIC 9(7).                                
           03  WS-IDKOLLI              PIC 9(5).                                
                                                                                
       77  WS-IDARTNR                  PIC Z(8)9.                               
       77  WS-IDFAKT                   PIC Z(7).                                
       77  WS-IDTRPTNR                 PIC Z(3).                                
                                                                                
       77  WS-VOLUME-MTQ               PIC S9(9)V9(4) COMP-3 VALUE ZERO.        
                                                                                
       77  UNCODE-SW                   PIC X.                                   
           88  UNCODE-FOUND                        VALUE 'J'.                   
           88  UNCODE-MISSING                      VALUE 'N'.                   
                                                                                
       77  EXPORT-SW                   PIC X.                                   
           88  EXPORT-YES                          VALUE 'J'.                   
           88  EXPORT-NO                           VALUE 'N'.                   
                                                                                
       77  INV-SW                      PIC X.                                   
           88  INV-YES                             VALUE 'J'.                   
           88  INV-NO                              VALUE 'N'.                   
                                                                                
       77  KEYS-SW                     PIC X       VALUE 'J'.                   
           88  KEYS-OK                             VALUE 'J'.                   
           88  KEYS-WRONG                          VALUE 'N'.                   
                                                                                
       77  CONTINUE-SW                 PIC X       VALUE 'J'.                   
           88  CONTINUE-YES                        VALUE 'J'.                   
           88  CONTINUE-NO                         VALUE 'N'.                   
                                                                                
       77  UPDATE-SW                   PIC X       VALUE 'J'.                   
           88  UPDATE-OK                           VALUE 'J'.                   
           88  UPDATE-NOT-OK                       VALUE 'N'.                   
                                                                                
       77  DIST-STEER-SW               PIC X       VALUE 'N'.                   
           88  DIST-STEER-YES                      VALUE 'J'.                   
           88  DIST-STEER-NO                       VALUE 'N'.                   
                                                                                
       01  TO-PAYLOAD.                                                          
           03  toNumber                PIC X(20).                               
                                                                                
       01  WS-SAVE-FIELDS.                                                      
           03  WS-SAVE-DAUTSKR         PIC X(8)    VALUE SPACES.                
           03  WS-SAVE-IDPARTNER-REC   PIC X(5)    VALUE SPACES.                
                                                                                
       01  WS-SHIPMENT-WORK-TABLE.                                              
           03  TAB-IDSHIPM             PIC 9(7).                                
           03  TAB-IDDC-SEND           PIC X(2).                                
           03  TAB-IDPARTNER-SEND      PIC X(5).                                
           03  TAB-IDLBBET             PIC X(12).                               
           03  TAB-IDTRPTNR            PIC 9(3).                                
           03  TAB-TISKEPPN            PIC S9(7) COMP-3.                        
           03  TAB-TISKPTID            PIC S9(7) COMP-3.                        
           03  TAB-SEAL                PIC X(25).                               
           03  TAB-VGM-KG              PIC 9(5).                                
           03  TAB-NO-OF-CASES         PIC S9(9) COMP SYNC.                     
           03  TAB-CASES OCCURS 1500 TIMES.                                     
               05  TAB-IDDC-REC        PIC X(2).                                
               05  TAB-IDPARTNER-REC   PIC X(5).                                
               05  TAB-IDDISTR         PIC S9(5) COMP-3.                        
               05  TAB-IDKUNDNR        PIC S9(7) COMP-3.                        
               05  TAB-IDORDNR7        PIC 9(7).                                
               05  TAB-IDKOLLI         PIC S9(5) COMP-3.                        
               05  TAB-IDPRODNR        PIC S9(7) COMP-3.                        
               05  TAB-KDFRAKT         PIC S9(3) COMP-3.                        
               05  TAB-KDORDKL         PIC 9.                                   
               05  TAB-KDEMBTYP        PIC 9.                                   
               05  TAB-KDKOLLI         PIC X(8).                                
               05  TAB-FLEXPORT        PIC X.                                   
               05  TAB-IDFAKT          PIC S9(7) COMP-3.                        
               05  TAB-VKORDNTO-KOLLI  PIC S9(6)V9(3) COMP-3.                   
               05  TAB-VKORDBTO-KOLLI  PIC S9(6)V9(1) COMP-3.                   
               05  TAB-DIKOLLIH        PIC S9(3) COMP-3.                        
               05  TAB-DIKOLLIL        PIC S9(5) COMP-3.                        
               05  TAB-DIKOLLIB        PIC S9(3) COMP-3.                        
               05  TAB-NO-OF-PARTS     PIC S9(9) COMP SYNC.                     
               05  TAB-PARTS OCCURS 300 TIMES.                                  
                   07  TAB-IDARTNR     PIC S9(9) COMP-3.                        
                   07  TAB-KVLEVART    PIC S9(7) COMP-3.                        
                   07  TAB-VKARTNTO    PIC S9(4)V9(3) COMP-3.                   
                                                                                
      *    --- SUBPROGRAMS AND PARAMETER AREAS                                  
       01  GENERAL-SUBPROGRAMS.                                                 
           03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
           03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
           03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
           03  WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.            
           03  WZ01CALL                PIC X(8)    VALUE 'WZ01CALL'.            
           03  WL01TIDZ                PIC X(8)    VALUE 'WL01TIDZ'.            
                                                                                
      *    --- PARAMETERS TO ABEND                                              
                                                                                
       77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
       77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
       77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
       77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
                                                                                
       01  MESSAGE-CODES.                                                       
           03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
                                                                                
      *                                                                         
       01  FILLER                      PIC X(16)   VALUE 'WZ01RECV'.            
                                                                                
      *01  -COPY WZ01RECV                                                       
                                                                                
       01  FILLER                      PIC X(16)   VALUE 'WZ01CALL'.            
                                                                                
      *01  -COPY WZ01CALL                                                       
                                                                                
      *01  -COPY WL01TIDZ                                                       
                                                                                
      *01  -COPY WWDCKONS                                                       
                                                                                
       01  TEST-IDDISTR                PIC S9(5) COMP-3.                        
      *01  -COPY WWDIST42 -RED TEST-IDDISTR                                     
      *01  -COPY WWDIST97 -RED TEST-IDDISTR                                     
                                                                                
       01  FILLER                      PIC X(16)   VALUE 'REFIL DC TAB'.        
      *01  -COPY WWDIST57                                                       
                                                                                
       01  FILLER                      PIC X(16)   VALUE 'DC CONSTANT'.         
      *01  -COPY WWDC99                                                         
                                                                                
       01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
                                                                                
       01  REQU-AREA.                                                           
           03 REQU-IDSHIPM             PIC X(7).                                
           03 REQU-KDCALL              PIC X(1).                                
                                                                                
      *    --- WORK-AREAS FOR IMS-SECTIONS                                      
      *                                                                         
       01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
                                                                                
       01  KEYS-FOR-DLI.                                                        
                                                                                
           03  W-IDSHIPM-X.                                                     
               05  W-IDSHIPM           PIC  9(07)  VALUE ZERO.                  
           03  W-WDE111KY-X.                                                    
               05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
               05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
           03  W-WDE121KY-X.                                                    
               05  W-IDPRODNR          PIC S9(7)   VALUE ZERO COMP-3.           
               05  W-IDKOLLI           PIC S9(5)   VALUE ZERO COMP-3.           
                                                                                
           03  W-IDARTNR-X.                                                     
               05  W-IDARTNR           PIC  S9(9) COMP-3.                       
                                                                                
           03  W-IDSKYLT-X.                                                     
               05  W-IDSKYLT           PIC X(3).                                
                                                                                
           03  W-IDDC-SEND-X.                                                   
               05  W-IDDC-SEND         PIC X(2).                                
                                                                                
           03  W-IDDC-REC-X.                                                    
               05  W-IDDC-REC          PIC X(2).                                
                                                                                
           03 W-WDGXKEY-0103-X.                                                 
              05  W-IDHTYP-0103        PIC X(4)    VALUE '0103'.                
              05  FILLER               PIC X(26)   VALUE LOW-VALUE.             
           03 W-KY0104-X.                                                       
              05  W-ADDISPABS          PIC X(50)                                
                                   VALUE 'APIOUT.TMS.TOCREATE'.                 
                                                                                
           03  W-WDR201-1165-X.                                                 
               05  W-IDHTYP            PIC X(4)    VALUE '1165'.                
               05  W-IDPSN             PIC 9(3).                                
               05  W-IDSPRAK           PIC X(2).                                
               05  W-LOW-VALUE         PIC X(21)   VALUE LOW-VALUE.             
                                                                                
           03  W-KDFGTRP-X.                                                     
               05  W-KDFGTRP           PIC 9(2)    VALUE 0.                     
                                                                                
           03  W-WDR101-4401-X.                                                 
               05  FILLER              PIC X(4)    VALUE '4401'.                
               05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
                                                                                
           03  W-KY4402-X.                                                      
               05  W-4402-IDDC         PIC X(2).                                
               05  W-4402-IDDISTR      PIC S9(5) COMP-3.                        
               05  W-4402-IDKUNDNR     PIC S9(7) COMP-3.                        
               05  W-4402-KDFRAKT      PIC S9(3) COMP-3.                        
               05  W-4402-KDORDKLX     PIC X.                                   
                                                                                
           03  W-IDTRPTNR-X.                                                    
               05  W-4402-IDTRPTNR     PIC S9(3) COMP-3.                        
                                                                                
           03  W-WDR101-4405-X.                                                 
               05  FILLER              PIC X(4)    VALUE '4405'.                
               05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
                                                                                
           03  W-KY4406-X.                                                      
               05  W-4406-IDTRPTNR     PIC S9(3)   COMP-3.                      
               05  W-4406-IDDC         PIC X(02).                               
               05  FILLER              PIC X(6)    VALUE LOW-VALUE.             
                                                                                
           03  W-IDGMT-X.                                                       
               05  W-IDDISTR-WDB2      PIC S9(5) VALUE ZERO COMP-3.             
               05  W-IDKUNDNR-WDB2     PIC S9(7) VALUE ZERO COMP-3.             
                                                                                
      *    --- STATUS-KOD FRÅN IMS                                              
       01  STATUS-WS                   PIC XX.                                  
           88  SEGMENT-FOUND                       VALUE '  '.                  
           88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
           88  SEGMENT-MISSING                     VALUE 'GE'.                  
                                                                                
       01  GOOD-STATUSCODES.                                                    
           03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
                                                                                
       01  SSA1                        PIC X(128).                              
       01  SSA2                        PIC X(128).                              
       01  SSA3                        PIC X(128).                              
                                                                                
       01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
             EXEC SQL INCLUDE SQLCA END-EXEC.                                   
                                                                                
       01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
       01  DB2-WS.                                                              
           03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
               88  INSERT-OK                       VALUE 000.                   
               88  ROW-FOUND                       VALUE 000.                   
               88  ROW-MISSING                     VALUE 100.                   
               88  DUPLICATE-FOUND                 VALUE 803.                   
           03  GOOD-SQLCODECODES.                                               
               05  GOOD-SQLCODE OCCURS 5                                        
                   INDEXED BY SQLCODE-IX PIC 9(3).                              
                                                                                
      *    --- IMS FUNCTION CODES                                               
      *01  -COPY W0003                                                          
                                                                                
       01  FILLER                PIC X(16) VALUE 'DLI-IO-WDE101'.               
       01  DLI-IO-WDE101.                                                       
      *    03  -COPY WDE101                                                     
       01  FILLER                PIC X(16) VALUE 'DLI-IO-WDE111'.               
       01  DLI-IO-WDE111.                                                       
      *    03  -COPY WDE111                                                     
       01  FILLER                PIC X(16) VALUE 'DLI-IO-WDE121'.               
       01  DLI-IO-WDE121.                                                       
      *    03  -COPY WDE121                                                     
       01  FILLER                PIC X(16) VALUE 'DLI-IO-WDE122'.               
       01  DLI-IO-WDE122.                                                       
      *    03  -COPY WDE122                                                     
       01  FILLER                PIC X(16) VALUE 'DLI-IO-WDE131'.               
       01  DLI-IO-WDE131.                                                       
      *    03  -COPY WDE131                                                     
                                                                                
       01  FILLER                PIC X(16) VALUE 'DLI-IO-WDD311'.               
       01  DLI-IO-WDD311.                                                       
      *    03  -COPY WDD311                                                     
                                                                                
       01  FILLER                PIC X(16) VALUE 'DLI-IO-WDK601'.               
       01  DLI-IO-WDK601.                                                       
      *    03  -COPY WDK601                                                     
                                                                                
       01  FILLER                PIC X(16) VALUE 'DLI-IO-WDK611'.               
       01  DLI-IO-WDK611.                                                       
      *    03  -COPY WDK611                                                     
                                                                                
       01  FILLER                PIC X(20) VALUE 'DLI-IO-WDB601-SEND'.          
       01  DLI-IO-WDB601-SEND.                                                  
      *    03  -COPY WDB601 -PRE SEND-                                          
                                                                                
       01  FILLER                PIC X(20) VALUE 'DLI-IO-WDB601-REC'.           
       01  DLI-IO-WDB601-REC.                                                   
      *    03  -COPY WDB601 -PRE REC-                                           
                                                                                
       01  FILLER                PIC X(16) VALUE 'DLI-IO-WDGX0104'.             
       01  DLI-IO-WDGX0104.                                                     
      *    03  -COPY WDGX0104                                                   
                                                                                
       01  FILLER                PIC X(16) VALUE 'DLI-IO-WDR2-1165'.            
       01  DLI-IO-WDGX1165.                                                     
      *    03  -COPY WDGX1165                                                   
                                                                                
       01  FILLER                PIC X(16) VALUE 'DLI-IO-WDGX1168'.             
       01  DLI-IO-WDGX1168.                                                     
      *    03  -COPY WDGX1168.                                                  
                                                                                
       01  FILLER                PIC X(16) VALUE 'DLI-IO-WDGX4402'.             
       01  DLI-IO-WDGX4402.                                                     
      *    03  -COPY WDGX4402.                                                  
                                                                                
       01  FILLER                PIC X(16) VALUE 'DLI-IO-WDGX4406'.             
       01  DLI-IO-WDGX4406.                                                     
      *    03  -COPY WDGX4406.                                                  
                                                                                
       01  FILLER                PIC X(16) VALUE 'DLI-IO-WDB201'.               
       01  DLI-IO-WDB201.                                                       
      *    03  -COPY WDB201                                                     
                                                                                
       01  FILLER                PIC X(16)  VALUE 'SHIP2TO-AREA'.               
                                                                                
      * -COPY SHIP2TO -PRE SH2TO-                                               
                                                                                
       EXEC SQL INCLUDE SHIP2TO END-EXEC.                                       
                                                                                
       01  TO-RESPONSE                 PIC X(5120).                             
                                                                                
       01  TO-REQUEST.                                                          
      *03  -COPY WTO0001                                                        
                                                                                
       LINKAGE SECTION.                                                         
      *01  -COPY W0009  -PRE MSG-                                               
                                                                                
      *01  -COPY W0008  -PRE ATAB-                                              
           05  FILLER                  PIC X.                                   
                                                                                
      *01  -COPY W0008  -PRE WDE1-                                              
           05  FILLER                  PIC X.                                   
                                                                                
      *01  -COPY W0008  -PRE WDD3-                                              
           05  FILLER                  PIC X.                                   
                                                                                
      *01  -COPY W0008  -PRE WDK6-                                              
           05  FILLER                  PIC X.                                   
                                                                                
      *01  -COPY W0008  -PRE WDB6-                                              
           05  FILLER                  PIC X.                                   
                                                                                
      *01  -COPY W0008  -PRE 1165-                                              
           05  FILLER                  PIC X.                                   
                                                                                
      *01  -COPY W0008  -PRE WDR1-                                              
           05  FILLER                  PIC X.                                   
                                                                                
      *01  -COPY W0008  -PRE WDB2-                                              
           05  FILLER                  PIC X.                                   
                                                                                
       PROCEDURE DIVISION  USING MSG-PCB   ATAB-PCB                             
                                 WDE1-PCB  WDD3-PCB  WDK6-PCB  WDB6-PCB         
                                 1165-PCB  WDR1-PCB  WDB2-PCB                   
                                 .                                              
       MAIN SECTION.                                                            
                                                                                
           PERFORM S01-RECV-OPEN                                                
           PERFORM S02-RECV-MESSAGE                                             
                                                                                
           IF RECV-KDRC = 0                                                     
             PERFORM A-INIT                                                     
             PERFORM B-CHECK-KEYS                                               
             IF KEYS-OK                                                         
               PERFORM C-READ-SHIPMENT-INFO                                     
                                                                                
               IF CONTINUE-YES                                                  
                 SORT TAB-CASES DESCENDING TAB-IDPARTNER-REC                    
                                           TAB-IDDISTR                          
                                           TAB-IDKUNDNR                         
                                           TAB-IDPRODNR                         
                                           TAB-IDKOLLI                          
                                                                                
                 PERFORM D-SEND-SHIPMENT-INFO                                   
               END-IF                                                           
             ELSE                                                               
               MOVE 'F'                TO RECV-KDKOMSTA                         
               IF IX-M < 10                                                     
                 COMPUTE IX-M = IX-M + 1                                        
                 STRING                                                         
                   'Ship id:' REQU-IDSHIPM                                      
                   ' Invalid Keys'                                              
                   DELIMITED BY SIZE                                            
                                     INTO MESSAGE-TEXT (IX-M)                   
               END-IF                                                           
             END-IF                                                             
           END-IF                                                               
                                                                                
           IF RECV-KDKOMSTA = 'F'                                               
             CONTINUE                                                           
           ELSE                                                                 
             MOVE 'A'                  TO RECV-KDKOMSTA                         
           END-IF                                                               
           MOVE MESSAGE-TEXT-GRP       TO RECV-MESSAGE                          
           COMPUTE RECV-MESSAGE-KVDLEN = FUNCTION LENGTH (                      
                                           FUNCTION TRIM (RECV-MESSAGE))        
           PERFORM S03-RECV-CLOSE                                               
           PERFORM Z-FINIT                                                      
           MOVE ZERO                   TO RETURN-CODE                           
           GOBACK                                                               
           .                                                                    
                                                                                
       A-INIT SECTION.                                                          
                                                                                
           SET KEYS-OK                 TO TRUE                                  
           INITIALIZE RECV-CLOSE-AREA                                           
                      WS-SHIPMENT-WORK-TABLE                                    
                      WS-SAVE-FIELDS                                            
                      MESSAGE-TEXT-GRP                                          
           MOVE ZERO                   TO IX-M                                  
           .                                                                    
                                                                                
       B-CHECK-KEYS SECTION.                                                    
                                                                                
           IF KEYS-OK                                                           
             IF REQU-IDSHIPM IS NUMERIC                                         
               MOVE REQU-IDSHIPM       TO W-IDSHIPM                             
             ELSE                                                               
               SET KEYS-WRONG          TO TRUE                                  
             END-IF                                                             
           END-IF                                                               
                                                                                
           IF KEYS-OK                                                           
             IF REQU-KDCALL = '1' OR '2' OR '3'                                 
               CONTINUE                                                         
             ELSE                                                               
               SET KEYS-WRONG          TO TRUE                                  
             END-IF                                                             
           END-IF                                                               
                                                                                
           .                                                                    
                                                                                
       Z-FINIT SECTION.                                                         
                                                                                
           CONTINUE                                                             
           .                                                                    
                                                                                
       C-READ-SHIPMENT-INFO SECTION.                                            
                                                                                
           SET CONTINUE-NO             TO TRUE                                  
           MOVE ZERO                   TO TIX-K                                 
                                          TIX-A                                 
           PERFORM IMS-GU-WDE101                                                
           IF SEGMENT-MISSING                                                   
             IF IX-M < 10                                                       
               COMPUTE IX-M = IX-M + 1                                          
               STRING                                                           
                 'Ship id:' REQU-IDSHIPM                                        
                 ' not found on WDE1 '                                          
                 DELIMITED BY SIZE                                              
                               INTO MESSAGE-TEXT (IX-M)                         
             END-IF                                                             
           ELSE                                                                 
             PERFORM CA-MOVE-SHIPMENT-INFO                                      
                                                                                
             IF CONTINUE-YES                                                    
               PERFORM IMS-GNP-WDE111                                           
                                                                                
               IF TAB-SEAL = SPACES OR LOW-VALUES OR                            
                  TAB-VGM-KG = LOW-VALUES OR ZERO                               
                 MOVE SGMT-IDDISTR     TO W-IDDISTR                             
                 MOVE SGMT-IDKUNDNR    TO W-IDKUNDNR                            
                 PERFORM IMS-GNP-WDE122                                         
                 IF SEGMENT-FOUND                                               
                   IF TAB-SEAL = SPACES AND                                     
                      TILL-IDSIGILL > SPACES                                    
                     MOVE TILL-IDSIGILL TO TAB-SEAL                             
                   END-IF                                                       
                   IF TAB-VGM-KG = LOW-VALUES OR ZERO AND                       
                      TILL-BESLULEV > SPACES                                    
                     IF FUNCTION TRIM(TILL-BESLULEV) IS NUMERIC AND             
                        FUNCTION TRIM(TILL-BESLULEV) <= 99999                   
                       MOVE FUNCTION TRIM(TILL-BESLULEV)                        
                                         TO TAB-VGM-KG                          
                     END-IF                                                     
                   END-IF                                                       
                 END-IF                                                         
               END-IF                                                           
                                                                                
      * Reset the segment found flag                                            
               SET SEGMENT-FOUND       TO TRUE                                  
               PERFORM UNTIL SEGMENT-MISSING OR CONTINUE-NO                     
                 MOVE SGMT-IDDISTR     TO W-IDDISTR                             
                 MOVE SGMT-IDKUNDNR    TO W-IDKUNDNR                            
                 PERFORM IMS-GNP-WDE121                                         
                                                                                
                 PERFORM UNTIL SEGMENT-MISSING OR CONTINUE-NO                   
                   MOVE SKOLLI-IDPRODNR TO W-IDPRODNR                           
                   MOVE SKOLLI-IDKOLLI TO W-IDKOLLI                             
                   PERFORM CB-MOVE-CASE-INFO                                    
                                                                                
                   PERFORM IMS-GNP-WDE131                                       
                   MOVE ZERO           TO TIX-A                                 
                   PERFORM UNTIL SEGMENT-MISSING                                
                     PERFORM CC-MOVE-PART-INFO                                  
                     PERFORM IMS-GNP-WDE131                                     
                   END-PERFORM                                                  
                                                                                
                   COMPUTE TAB-NO-OF-PARTS (TIX-K) = TIX-A                      
                                                                                
                   PERFORM IMS-GNP-WDE121                                       
                 END-PERFORM                                                    
                                                                                
                 PERFORM IMS-GNP-WDE111                                         
               END-PERFORM                                                      
               COMPUTE TAB-NO-OF-CASES = TIX-K                                  
             END-IF                                                             
           END-IF                                                               
                                                                                
           .                                                                    
                                                                                
       CA-MOVE-SHIPMENT-INFO SECTION.                                           
                                                                                
           MOVE SHIP-IDDC              TO TAB-IDDC-SEND                         
                                                                                
           IF TAB-IDDC-SEND = WC-DDC-SE                                         
             MOVE WC-CDC-SE            TO W-4406-IDDC                           
           ELSE                                                                 
             MOVE TAB-IDDC-SEND        TO W-4406-IDDC                           
           END-IF                                                               
                                                                                
           MOVE SHIP-IDSHIPM           TO TAB-IDSHIPM                           
           MOVE SHIP-IDLBBET           TO TAB-IDLBBET                           
           MOVE SHIP-IDTRPTNR          TO TAB-IDTRPTNR                          
                                          W-4406-IDTRPTNR                       
           MOVE SHIP-TISKEPPN          TO TAB-TISKEPPN                          
           MOVE SHIP-TISKPTID          TO TAB-TISKPTID                          
                                                                                
                                                                                
           PERFORM IMS-GU-WDGX4406                                              
           IF SEGMENT-FOUND                                                     
             IF TRPT-FLTOTMS = YES                                              
               SET CONTINUE-YES        TO TRUE                                  
             ELSE                                                               
               IF IX-M < 10                                                     
                 COMPUTE IX-M = IX-M + 1                                        
                 MOVE SHIP-IDTRPTNR    TO WS-IDTRPTNR                           
                 STRING                                                         
                   'Ship id:' REQU-IDSHIPM                                      
                   ' Send to TMS flag is N for transport '                      
                   WS-IDTRPTNR                                                  
                   DELIMITED BY SIZE                                            
                               INTO MESSAGE-TEXT (IX-M)                         
               END-IF                                                           
             END-IF                                                             
           ELSE                                                                 
             IF IX-M < 10                                                       
               COMPUTE IX-M = IX-M + 1                                          
               MOVE SHIP-IDTRPTNR      TO WS-IDTRPTNR                           
               STRING                                                           
                 'Ship id:' REQU-IDSHIPM                                        
                 ' Transport '                                                  
                 WS-IDTRPTNR                                                    
                 ' not found on 4406 '                                          
                 DELIMITED BY SIZE                                              
                               INTO MESSAGE-TEXT (IX-M)                         
             END-IF                                                             
           END-IF                                                               
           .                                                                    
                                                                                
       CB-MOVE-CASE-INFO SECTION.                                               
                                                                                
           COMPUTE TIX-K = TIX-K + 1                                            
                                                                                
           MOVE SKOLLI-IDDISTR         TO TAB-IDDISTR (TIX-K)                   
           MOVE SKOLLI-IDKUNDNR        TO TAB-IDKUNDNR (TIX-K)                  
           MOVE SKOLLI-IDORDNR7        TO TAB-IDORDNR7 (TIX-K)                  
           MOVE SKOLLI-IDKOLLI         TO TAB-IDKOLLI (TIX-K)                   
           MOVE SKOLLI-IDPRODNR        TO TAB-IDPRODNR (TIX-K)                  
           MOVE SKOLLI-KDFRAKT         TO TAB-KDFRAKT (TIX-K)                   
           MOVE SKOLLI-KDORDKL         TO TAB-KDORDKL (TIX-K)                   
           MOVE SKOLLI-KDEMBTYP        TO TAB-KDEMBTYP (TIX-K)                  
           MOVE SKOLLI-KDKOLLI         TO TAB-KDKOLLI (TIX-K)                   
      *    MOVE SKOLLI-VKORDNTO-KOLLI  TO TAB-VKORDNTO-KOLLI (TIX-K)            
      *    Initialize here and calculate individually at part level             
           MOVE ZERO                   TO TAB-VKORDNTO-KOLLI (TIX-K)            
           MOVE SKOLLI-VKORDBTO-KOLLI  TO TAB-VKORDBTO-KOLLI (TIX-K)            
           MOVE SKOLLI-DIKOLLIH        TO TAB-DIKOLLIH (TIX-K)                  
           MOVE SKOLLI-DIKOLLIL        TO TAB-DIKOLLIL (TIX-K)                  
           MOVE SKOLLI-DIKOLLIB        TO TAB-DIKOLLIB (TIX-K)                  
                                                                                
           PERFORM CBA-GET-REC-IDDC-IDPARTNER                                   
           PERFORM CBB-CHECK-EXPORT                                             
           .                                                                    
                                                                                
       CBA-GET-REC-IDDC-IDPARTNER SECTION.                                      
                                                                                
           MOVE SPACES                 TO TAB-IDDC-REC (TIX-K)                  
           MOVE SPACES                 TO TAB-IDPARTNER-REC (TIX-K)             
           IF TAB-IDDC-SEND = WC-DDC-SE                                         
             MOVE WC-CDC-SE            TO W-4402-IDDC                           
                                          WS-IDDC                               
           ELSE                                                                 
             MOVE TAB-IDDC-SEND        TO W-4402-IDDC                           
                                          WS-IDDC                               
           END-IF                                                               
           MOVE TAB-IDDISTR (TIX-K)    TO W-4402-IDDISTR                        
           MOVE TAB-IDKUNDNR (TIX-K)   TO W-4402-IDKUNDNR                       
           MOVE TAB-KDFRAKT  (TIX-K)   TO W-4402-KDFRAKT                        
           MOVE TAB-KDORDKL  (TIX-K)   TO W-4402-KDORDKLX                       
           MOVE TAB-IDTRPTNR           TO W-4402-IDTRPTNR                       
                                                                                
           PERFORM CBAA-CHECK-DIST-STEERING                                     
           IF DIST-STEER-YES                                                    
             MOVE ZERO                 TO W-4402-IDKUNDNR                       
           END-IF                                                               
                                                                                
           PERFORM IMS-GU-WDGX4402                                              
           IF SEGMENT-MISSING                                                   
             MOVE SPACE                TO W-4402-KDORDKLX                       
             PERFORM IMS-GU-WDGX4402                                            
             IF SEGMENT-MISSING                                                 
               MOVE ZERO               TO W-4402-KDFRAKT                        
               PERFORM IMS-GU-WDGX4402                                          
               IF SEGMENT-MISSING                                               
                 MOVE ZERO             TO W-4402-IDKUNDNR                       
                 PERFORM IMS-GU-WDGX4402                                        
               END-IF                                                           
             END-IF                                                             
           END-IF                                                               
                                                                                
           IF SEGMENT-FOUND                                                     
             MOVE 4402-IDDC-CROSS      TO TAB-IDDC-REC (TIX-K)                  
           END-IF                                                               
                                                                                
           IF TAB-IDDC-REC (TIX-K) = SPACES                                     
             SEARCH ALL DIST57-REFILL-DC                                        
                AT END                                                          
                   CONTINUE                                                     
                WHEN DIST57-SOK-IDDISTR(DIST57-IX) = TAB-IDDISTR (TIX-K)        
                   MOVE DIST57-REFILL-TO-DC(DIST57-IX)                          
                                       TO TAB-IDDC-REC (TIX-K)                  
             END-SEARCH                                                         
           END-IF                                                               
                                                                                
           IF TAB-IDDC-REC (TIX-K) = SPACES                                     
             MOVE TAB-IDDISTR (TIX-K)  TO W-IDDISTR-WDB2                        
             MOVE TAB-IDKUNDNR (TIX-K) TO W-IDKUNDNR-WDB2                       
             PERFORM IMS-GU-WDB201                                              
             IF SEGMENT-FOUND                                                   
      *        For importers, we will use the parma partner code                
      *        instead of receiving DC.                                         
               IF GMT-KDKUNDKAT = 04 OR 08                                      
                 MOVE FUNCTION TRIM (GMT-IDPARTNER)                             
                                       TO TAB-IDPARTNER-REC (TIX-K)             
               ELSE                                                             
                 MOVE GMT-IDDC-BULK (1)                                         
                                       TO TAB-IDDC-REC (TIX-K)                  
      *          EVALUATE TAB-KDORDKL (TIX-K)                                   
      *            WHEN 0                                                       
      *              MOVE GMT-IDDC-VOR (1)                                      
      *                                TO TAB-IDDC-REC (TIX-K)                  
      *            WHEN 1                                                       
      *              MOVE GMT-IDDC-DAY (1)                                      
      *                                TO TAB-IDDC-REC (TIX-K)                  
      *            WHEN OTHER                                                   
      *              MOVE GMT-IDDC-BULK (1)                                     
      *                                TO TAB-IDDC-REC (TIX-K)                  
      *          END-EVALUATE                                                   
               END-IF                                                           
             END-IF                                                             
           END-IF                                                               
                                                                                
           IF TAB-IDDC-REC (TIX-K) = SPACES                                     
             MOVE 'XX'                 TO TAB-IDDC-REC (TIX-K)                  
           ELSE                                                                 
             IF TAB-IDDC-REC (TIX-K) = W-IDDC-REC                               
               MOVE REC-DCS-IDLEVNR-EMB                                         
                                       TO TAB-IDPARTNER-REC (TIX-K)             
             ELSE                                                               
               MOVE TAB-IDDC-REC (TIX-K)                                        
                                       TO W-IDDC-REC                            
               PERFORM IMS-GU-WDB601-REC                                        
               MOVE REC-DCS-IDLEVNR-EMB                                         
                                       TO TAB-IDPARTNER-REC (TIX-K)             
             END-IF                                                             
           END-IF                                                               
                                                                                
           IF TAB-IDPARTNER-REC (TIX-K) = SPACES                                
             MOVE 'F'                  TO RECV-KDKOMSTA                         
             SET CONTINUE-NO           TO TRUE                                  
             IF IX-M < 10                                                       
               COMPUTE IX-M = IX-M + 1                                          
               MOVE TAB-IDDISTR (TIX-K)                                         
                                       TO WS-IDDISTR                            
               MOVE TAB-IDKUNDNR (TIX-K)                                        
                                       TO WS-IDKUNDNR                           
               STRING                                                           
                    'Ship id:' REQU-IDSHIPM                                     
                    ' Cant determine receiver for '                             
                    'dist :' WS-IDDISTR                                         
                    ', cust :' WS-IDKUNDNR                                      
                    DELIMITED BY SIZE                                           
                                     INTO MESSAGE-TEXT (IX-M)                   
             END-IF                                                             
           END-IF                                                               
           .                                                                    
                                                                                
       CBAA-CHECK-DIST-STEERING SECTION.                                        
                                                                                
      *    Always keep the below conditions in sync with                        
      *    W403PLAT, AA-TRANSPORT SECTION.                                      
                                                                                
           SET DIST-STEER-NO           TO TRUE                                  
           MOVE TAB-IDDISTR (TIX-K)    TO TEST-IDDISTR                          
           IF DIST97-STYRNING                                                   
             IF SEND-DCS-FLKNDVAL = YES                                         
               CONTINUE                                                         
             ELSE                                                               
               IF (DIST97-STYRNING-2635 AND (CDC-SE OR LDC-FI))                 
                  OR                                                            
                  (DIST97-STYRNING-2638 AND (CDC-SE OR LDC-FI))                 
                 CONTINUE                                                       
               ELSE                                                             
                 SET DIST-STEER-YES    TO TRUE                                  
               END-IF                                                           
             END-IF                                                             
           ELSE                                                                 
             IF (DIST97-STYRNING-878 AND (LDC-SE-1C OR LDC-NO-3J))              
                OR                                                              
                (DIST97-STYRNING-1090 AND LDC-FI)                               
               CONTINUE                                                         
             ELSE                                                               
               IF (DIST97-STYRNING-878 OR DIST97-STYRNING-1090)                 
                  AND                                                           
                  (SEND-DCS-KDDC = 'C ' OR 'D ')                                
                   SET DIST-STEER-YES    TO TRUE                                
               ELSE                                                             
                 IF DIST97-UNDANTAG-VOR AND (SEND-DCS-KDDC NOT = 'C ')          
                   SET DIST-STEER-YES    TO TRUE                                
                 END-IF                                                         
               END-IF                                                           
             END-IF                                                             
           END-IF                                                               
                                                                                
           .                                                                    
                                                                                
       CBB-CHECK-EXPORT SECTION.                                                
                                                                                
           IF TAB-IDDC-SEND = W-IDDC-SEND                                       
             CONTINUE                                                           
           ELSE                                                                 
             MOVE TAB-IDDC-SEND        TO W-IDDC-SEND                           
             PERFORM IMS-GU-WDB601-SEND                                         
           END-IF                                                               
           MOVE TAB-IDDC-SEND          TO WS-IDDC                               
           MOVE TAB-IDDISTR (TIX-K)    TO TEST-IDDISTR                          
                                                                                
           IF (DIST42-EJ-EU-MIC  AND                                            
               (SEND-DCS-CDC OR (SEND-DCS-DDC AND SEND-DCS-SWEDEN))) OR         
              (DIST42-EJ-EU-MIC  AND SDC-ES) OR                                 
              (DIST42-EJ-EU-DC21 AND SDC-NL) OR                                 
              (DIST42-EJ-EU-MIC  AND NDC-AE) OR                                 
              (DIST42-MIC-CN     AND NDC-CN) OR                                 
              (DIST42-MIC-USA    AND NDC-US)                                    
             MOVE YES                  TO TAB-FLEXPORT (TIX-K)                  
             MOVE SKOLLI-IDFAKT        TO TAB-IDFAKT   (TIX-K)                  
           ELSE                                                                 
             MOVE NOO                  TO TAB-FLEXPORT (TIX-K)                  
             MOVE ZERO                 TO TAB-IDFAKT   (TIX-K)                  
           END-IF                                                               
           .                                                                    
                                                                                
       CC-MOVE-PART-INFO SECTION.                                               
                                                                                
           COMPUTE TIX-A = TIX-A + 1                                            
                                                                                
           MOVE SRAD-IDARTNR           TO TAB-IDARTNR (TIX-K, TIX-A)            
           MOVE SRAD-KVLEVART          TO TAB-KVLEVART (TIX-K, TIX-A)           
           MOVE SRAD-VKART-NTO-KG      TO TAB-VKARTNTO (TIX-K, TIX-A)           
           COMPUTE TAB-VKORDNTO-KOLLI (TIX-K)                                   
                                        = TAB-VKORDNTO-KOLLI (TIX-K)            
                                          + (SRAD-VKART-NTO-KG *                
                                             SRAD-KVLEVART)                     
           .                                                                    
                                                                                
       D-SEND-SHIPMENT-INFO SECTION.                                            
                                                                                
           IF TAB-IDDC-SEND = WC-DDC-SE                                         
             MOVE WC-CDC-SE            TO W-IDDC-SEND                           
           ELSE                                                                 
             MOVE TAB-IDDC-SEND        TO W-IDDC-SEND                           
           END-IF                                                               
           PERFORM IMS-GU-WDB601-SEND                                           
           MOVE SEND-DCS-IDLEVNR-EMB   TO TAB-IDPARTNER-SEND                    
                                                                                
           MOVE LOW-VALUES             TO TO-REQUEST                            
           MOVE ZERO                   TO IX-K                                  
                                          IX-A                                  
                                          IX-F                                  
                                                                                
           PERFORM                                                              
           VARYING TIX-K FROM 1 BY 1                                            
             UNTIL TIX-K > TAB-NO-OF-CASES                                      
             IF TAB-IDPARTNER-REC (TIX-K) = WS-SAVE-IDPARTNER-REC               
               IF CONTINUE-YES                                                  
                 PERFORM DA-CHECK-EXPORT                                        
               END-IF                                                           
               IF CONTINUE-YES                                                  
                 PERFORM DC-MOVE-CASE-INFO                                      
                 PERFORM DD-MOVE-PART-INFO                                      
               END-IF                                                           
             ELSE                                                               
               PERFORM DE-CHECK-AND-SEND                                        
                                                                                
               MOVE TAB-IDPARTNER-REC (TIX-K)                                   
                                       TO WS-SAVE-IDPARTNER-REC                 
               MOVE LOW-VALUES         TO TO-REQUEST                            
               MOVE ZERO               TO IX-K                                  
                                        IX-A                                    
                                        IX-F                                    
               SET CONTINUE-YES        TO TRUE                                  
               SET EXPORT-NO           TO TRUE                                  
                                                                                
               IF TAB-IDPARTNER-SEND = TAB-IDPARTNER-REC (TIX-K)                
                 SET CONTINUE-NO       TO TRUE                                  
                 MOVE 'F'              TO RECV-KDKOMSTA                         
                 IF IX-M < 10                                                   
                   COMPUTE IX-M = IX-M + 1                                      
                   STRING                                                       
                     'Ship id:' REQU-IDSHIPM                                    
                     ' Not sent! Send/Recv are same'                            
                     ', sender :' TAB-IDDC-SEND '-'                             
                                  TAB-IDPARTNER-SEND                            
                     ', receiver :' TAB-IDDC-REC (TIX-K) '-'                    
                                  TAB-IDPARTNER-REC (TIX-K)                     
                     DELIMITED BY SIZE                                          
                               INTO MESSAGE-TEXT (IX-M)                         
                 END-IF                                                         
               ELSE                                                             
                 PERFORM DA-CHECK-EXPORT                                        
                 IF CONTINUE-YES                                                
                   PERFORM DB-MOVE-SHIPMENT-INFO                                
                   PERFORM DC-MOVE-CASE-INFO                                    
                   PERFORM DD-MOVE-PART-INFO                                    
                 END-IF                                                         
               END-IF                                                           
             END-IF                                                             
           END-PERFORM                                                          
                                                                                
           PERFORM DE-CHECK-AND-SEND                                            
           .                                                                    
                                                                                
       DA-CHECK-EXPORT SECTION.                                                 
                                                                                
           IF TAB-FLEXPORT (TIX-K) = YES                                        
             SET EXPORT-YES            TO TRUE                                  
             IF TAB-IDFAKT (TIX-K) IS NUMERIC AND                               
                TAB-IDFAKT (TIX-K) > ZERO                                       
               MOVE TAB-IDFAKT (TIX-K) TO WS-IDFAKT                             
               SET INV-NO              TO TRUE                                  
               PERFORM                                                          
               VARYING IX FROM 1 BY 1                                           
                 UNTIL IX > 1500 OR INV-YES                                     
                 IF invoiceNumbers2 (IX) = FUNCTION TRIM (WS-IDFAKT)            
                   SET INV-YES         TO TRUE                                  
                 ELSE                                                           
                   IF IX > IX-F                                                 
                     COMPUTE IX-F = IX-F + 1                                    
                     COMPUTE invoiceNumbers-num = IX-F                          
                     MOVE FUNCTION TRIM (WS-IDFAKT)                             
                                       TO invoiceNumbers2 (IX-F)                
                     COMPUTE invoiceNumbers2-length (IX-F) =                    
                                       FUNCTION LENGTH (                        
                                       FUNCTION TRIM(WS-IDFAKT))                
                     SET INV-YES       TO TRUE                                  
                   END-IF                                                       
                 END-IF                                                         
               END-PERFORM                                                      
             ELSE                                                               
               SET CONTINUE-NO         TO TRUE                                  
               IF IX-M < 10                                                     
                 COMPUTE IX-M = IX-M + 1                                        
                 STRING                                                         
                   'Ship id:' REQU-IDSHIPM                                      
                   ' Not sent! Exp, but no invoice yet '                        
                   ', sender :' TAB-IDDC-SEND '-'                               
                                TAB-IDPARTNER-SEND                              
                   ', receiver :' TAB-IDDC-REC (TIX-K) '-'                      
                                TAB-IDPARTNER-REC (TIX-K)                       
                   DELIMITED BY SIZE                                            
                                     INTO MESSAGE-TEXT (IX-M)                   
               END-IF                                                           
             END-IF                                                             
           END-IF                                                               
           .                                                                    
                                                                                
       DB-MOVE-SHIPMENT-INFO SECTION.                                           
                                                                                
           MOVE 1                      TO request2-num                          
                                                                                
           MOVE TAB-IDSHIPM            TO shipmentId                            
                                          SH2TO-IDSHIPM                         
           COMPUTE shipmentId-length = FUNCTION LENGTH (                        
                                       FUNCTION TRIM(shipmentId))               
                                                                                
      *    For Brazil, "GREENLIGHT" shipments the status must                   
      *    be set to IN_PROCESS so they are not automatically released          
      *    in TMS. Since these shipments need to wait until an OK is            
      *    received from Brazil                                                 
           IF TAB-IDTRPTNR = 73 OR 714                                          
             MOVE 'IN_PROCESS'         TO state                                 
             MOVE 10                   TO state-length                          
           ELSE                                                                 
             MOVE 'READY_FOR_DISPATCH' TO state                                 
             MOVE 18                   TO state-length                          
           END-IF                                                               
                                                                                
           MOVE 1                      TO groupingId-num                        
           MOVE TAB-IDLBBET            TO groupingId2                           
                                          SH2TO-IDLBBET                         
           COMPUTE groupingId2-length = FUNCTION LENGTH (                       
                                        FUNCTION TRIM(groupingId2))             
                                                                                
           MOVE 1                      TO transportNumber-num                   
           MOVE TAB-IDTRPTNR           TO transportNumber2                      
                                          SH2TO-IDTRPTNR                        
           MOVE 3                      TO transportNumber2-length               
                                                                                
           IF TAB-IDDC-REC (TIX-K) = 'XX'                                       
             MOVE 0                    TO receivingDC-num                       
           ELSE                                                                 
             MOVE 1                    TO receivingDC-num                       
             MOVE TAB-IDDC-REC (TIX-K) TO receivingDC2                          
             MOVE 2                    TO receivingDC2-length                   
           END-IF                                                               
                                                                                
           MOVE 1                      TO sendingDC-num                         
           MOVE TAB-IDDC-SEND          TO sendingDC2                            
                                          SH2TO-IDDC-SEND                       
           MOVE 2                      TO sendingDC2-length                     
                                                                                
           MOVE TAB-IDPARTNER-SEND     TO pickup                                
           MOVE 5                      TO pickup-length                         
                                                                                
           MOVE TAB-IDPARTNER-REC (TIX-K)                                       
                                       TO delivery                              
                                          SH2TO-IDPARTNER-REC                   
           MOVE 5                      TO delivery-length                       
                                                                                
           MOVE 20                     TO WS-DATE-CC                            
           MOVE TAB-TISKEPPN           TO WS-DATE-YYMMDD                        
           MOVE TAB-TISKPTID           TO WS-TIME                               
           PERFORM S10-GET-UTC-TIMESTAMP                                        
           MOVE WS-TIMESTAMP-UTC       TO requestedPickup                       
           MOVE 20                     TO requestedPickup-length                
                                                                                
           MOVE FUNCTION FORMATTED-DATE (                                       
                  'YYYY-MM-DD',                                                 
                  FUNCTION INTEGER-OF-DATE (WS-DATE))                           
                                       TO SH2TO-TISKEPPN                        
                                                                                
           MOVE FUNCTION FORMATTED-TIME (                                       
                  'hh:mm:ss',                                                   
                  FUNCTION SECONDS-FROM-FORMATTED-TIME (                        
                  'hhmmss', WS-TIME-X))                                         
                                       TO SH2TO-TISKPTID                        
                                                                                
           MOVE 1                      TO incoterm-num                          
           MOVE 'FCA'                  TO incoterm2                             
           COMPUTE incoterm2-length = FUNCTION LENGTH (                         
                                      FUNCTION TRIM(incoterm2))                 
                                                                                
           MOVE 0                      TO volumeInMTQ-num                       
                                                                                
      * Equipment info - VGM and Seal                                           
           IF TAB-SEAL > SPACES OR TAB-VGM-KG > ZERO                            
             MOVE 1                    TO equipment2-num                        
             IF TAB-SEAL > SPACES                                               
               MOVE 1                  TO sealNo-num                            
               MOVE TAB-SEAL           TO sealNo2 (1)                           
               COMPUTE sealNo2-length (1) = FUNCTION LENGTH (                   
                                             FUNCTION TRIM(sealNo2(1)))         
             END-IF                                                             
             IF TAB-VGM-KG > ZERO                                               
               MOVE 1                  TO vgmWeightInKGM-num                    
               MOVE TAB-VGM-KG         TO vgmWeightInKGM                        
             END-IF                                                             
           END-IF                                                               
           .                                                                    
                                                                                
       DC-MOVE-CASE-INFO SECTION.                                               
                                                                                
                                                                                
           COMPUTE IX-K = IX-K + 1                                              
           COMPUTE packages2-num = IX-K                                         
           COMPUTE SH2TO-KVKOLLI = IX-K                                         
                                                                                
           MOVE TAB-IDDISTR (TIX-K)    TO WS-IDDISTR                            
           MOVE TAB-IDKUNDNR (TIX-K)   TO WS-IDKUNDNR                           
           MOVE TAB-IDORDNR7 (TIX-K)   TO WS-IDORDNR7                           
           MOVE TAB-IDKOLLI (TIX-K)    TO WS-IDKOLLI                            
           MOVE WS-PACKAGE-ID          TO packageId (IX-K)                      
           MOVE 22                     TO packageId-length (IX-K)               
                                                                                
           EVALUATE TAB-KDEMBTYP (TIX-K)                                        
             WHEN 1                                                             
               MOVE 1                  TO packageDescription-num (IX-K)         
               MOVE 'CASE'             TO packageDescription2 (IX-K)            
               MOVE 4                  TO packageDescription2-length            
                                                                 (IX-K)         
               MOVE 'PARCEL'           TO packageType (IX-K)                    
               MOVE 6                  TO packageType-length (IX-K)             
             WHEN 2                                                             
               MOVE 1                  TO packageDescription-num (IX-K)         
               MOVE 'PACKAGE'          TO packageDescription2 (IX-K)            
               MOVE 7                  TO packageDescription2-length            
                                                                 (IX-K)         
               MOVE 'PARCEL'           TO packageType (IX-K)                    
               MOVE 6                  TO packageType-length (IX-K)             
             WHEN 3                                                             
               MOVE 1                  TO packageDescription-num (IX-K)         
               MOVE 'BUNDLE'           TO packageDescription2 (IX-K)            
               MOVE 6                  TO packageDescription2-length            
                                                                 (IX-K)         
               MOVE 'PARCEL'           TO packageType (IX-K)                    
               MOVE 6                  TO packageType-length (IX-K)             
             WHEN 4                                                             
               MOVE 1                  TO packageDescription-num (IX-K)         
               MOVE 'CRATE'            TO packageDescription2 (IX-K)            
               MOVE 5                  TO packageDescription2-length            
                                                                 (IX-K)         
               MOVE 'PARCEL'           TO packageType (IX-K)                    
               MOVE 6                  TO packageType-length (IX-K)             
             WHEN 5                                                             
               MOVE 1                  TO packageDescription-num (IX-K)         
               MOVE 'PIECE'            TO packageDescription2 (IX-K)            
               MOVE 5                  TO packageDescription2-length            
                                                                 (IX-K)         
               MOVE 'PARCEL'           TO packageType (IX-K)                    
               MOVE 6                  TO packageType-length (IX-K)             
             WHEN 6                                                             
               MOVE 1                  TO packageDescription-num (IX-K)         
               MOVE 'CONTAINER'        TO packageDescription2 (IX-K)            
               MOVE 9                  TO packageDescription2-length            
                                                                 (IX-K)         
               MOVE 'PARCEL'           TO packageType (IX-K)                    
               MOVE 6                  TO packageType-length (IX-K)             
             WHEN 7                                                             
               MOVE 1                  TO packageDescription-num (IX-K)         
               MOVE 'PALLET'           TO packageDescription2 (IX-K)            
               MOVE 6                  TO packageDescription2-length            
                                                                 (IX-K)         
               MOVE 'PAL'              TO packageType (IX-K)                    
               MOVE 3                  TO packageType-length (IX-K)             
             WHEN 8                                                             
               MOVE 1                  TO packageDescription-num (IX-K)         
               MOVE 'RETURN BLUE CARTON'                                        
                                       TO packageDescription2 (IX-K)            
               MOVE 18                 TO packageDescription2-length            
                                                                 (IX-K)         
               MOVE 'PARCEL'           TO packageType (IX-K)                    
               MOVE 6                  TO packageType-length (IX-K)             
             WHEN OTHER                                                         
               MOVE 1                  TO packageDescription-num (IX-K)         
               MOVE 'PARCEL'           TO packageDescription2 (IX-K)            
               MOVE 6                  TO packageDescription2-length            
                                                                 (IX-K)         
               MOVE 'PARCEL'           TO packageType (IX-K)                    
               MOVE 6                  TO packageType-length (IX-K)             
           END-EVALUATE                                                         
                                                                                
           MOVE 1                      TO packageCode-num (IX-K)                
           MOVE TAB-KDKOLLI (TIX-K)    TO packageCode2 (IX-K)                   
           COMPUTE packageCode2-length (IX-K) =                                 
                                      FUNCTION LENGTH (                         
                                      FUNCTION TRIM(packageCode2(IX-K)))        
                                                                                
           MOVE 1                      TO netWeight-num (IX-K)                  
      *    Do below as a temporary fix where net weight is greater than         
      *    gross weight. In which case the TO creation fails.                   
           IF TAB-VKORDNTO-KOLLI (TIX-K) > TAB-VKORDBTO-KOLLI (TIX-K)           
             MOVE TAB-VKORDBTO-KOLLI (TIX-K)                                    
                                       TO TAB-VKORDNTO-KOLLI (TIX-K)            
           END-IF                                                               
           MOVE TAB-VKORDNTO-KOLLI (TIX-K)                                      
                                       TO netWeight (IX-K)                      
                                                                                
           MOVE 1                      TO grossWeight-num (IX-K)                
           MOVE TAB-VKORDBTO-KOLLI (TIX-K)                                      
                                       TO grossWeight (IX-K)                    
                                                                                
           MOVE 1                      TO height-num (IX-K)                     
           COMPUTE height (IX-K) = TAB-DIKOLLIH (TIX-K) * 10                    
                                                                                
           MOVE 1                      TO Xlength-num (IX-K)                    
           COMPUTE Xlength (IX-K) = TAB-DIKOLLIL (TIX-K) * 10                   
                                                                                
           MOVE 1                      TO width-num (IX-K)                      
           COMPUTE width (IX-K) = TAB-DIKOLLIB (TIX-K) * 10                     
                                                                                
           .                                                                    
                                                                                
       DD-MOVE-PART-INFO SECTION.                                               
                                                                                
           MOVE ZERO                   TO IX-A                                  
           PERFORM                                                              
           VARYING TIX-A FROM 1 BY 1                                            
             UNTIL TIX-A > TAB-NO-OF-PARTS (TIX-K)                              
             COMPUTE IX-A = IX-A + 1                                            
             COMPUTE parts2-num (IX-K) = IX-A                                   
                                                                                
             MOVE 'GB'                 TO W-IDSKYLT                             
             MOVE TAB-IDARTNR (TIX-K, TIX-A)                                    
                                       TO W-IDARTNR                             
             PERFORM IMS-GU-WDK601                                              
             PERFORM IMS-GNP-WDK611                                             
                                                                                
             MOVE TAB-IDARTNR (TIX-K, TIX-A)                                    
                                       TO WS-IDARTNR                            
             MOVE FUNCTION TRIM(WS-IDARTNR)                                     
                                       TO partNumber (IX-K, IX-A)               
             COMPUTE partNumber-length (IX-K, IX-A) =                           
                                      FUNCTION LENGTH (                         
                                      FUNCTION TRIM (                           
                                      partNumber (IX-K, IX-A)))                 
                                                                                
             MOVE 1                    TO partQty-num (IX-K, IX-A)              
             MOVE TAB-KVLEVART (TIX-K, TIX-A)                                   
                                       TO partQty (IX-K, IX-A)                  
                                                                                
             MOVE 1                    TO partWeight-num (IX-K, IX-A)           
             MOVE TAB-VKARTNTO (TIX-K, TIX-A)                                   
                                       TO partWeight (IX-K, IX-A)               
                                                                                
             MOVE 1                    TO partUOM-num (IX-K, IX-A)              
             EVALUATE FUNCTION TRIM(ART-KDSORT)                                 
               WHEN 'ST'                                                        
                 MOVE 'PCE'            TO partUOM2 (IX-K, IX-A)                 
               WHEN 'G'                                                         
                 MOVE 'GRM'            TO partUOM2 (IX-K, IX-A)                 
               WHEN 'KG'                                                        
                 MOVE 'KGM'            TO partUOM2 (IX-K, IX-A)                 
               WHEN 'L'                                                         
                 MOVE 'LTR'            TO partUOM2 (IX-K, IX-A)                 
               WHEN 'M3'                                                        
                 MOVE 'MTQ'            TO partUOM2 (IX-K, IX-A)                 
               WHEN 'M'                                                         
                 MOVE 'MTR'            TO partUOM2 (IX-K, IX-A)                 
               WHEN OTHER                                                       
                 MOVE 'PCE'            TO partUOM2 (IX-K, IX-A)                 
             END-EVALUATE                                                       
                                                                                
             COMPUTE partUOM2-length (IX-K, IX-A) =                             
                                      FUNCTION LENGTH (                         
                                      FUNCTION TRIM (                           
                                      partUOM2 (IX-K, IX-A)))                   
                                                                                
             PERFORM IMS-GU-WDD311                                              
             IF SEGMENT-FOUND                                                   
               MOVE TEXT-BEART         TO partDescription2 (IX-K, IX-A)         
             ELSE                                                               
               MOVE 'DESCRIPTION MISSING'                                       
                                       TO partDescription2 (IX-K, IX-A)         
             END-IF                                                             
             MOVE 1                    TO partDescription-num                   
                                                            (IX-K, IX-A)        
             COMPUTE partDescription2-length (IX-K, IX-A) =                     
                                      FUNCTION LENGTH (                         
                                      FUNCTION TRIM (                           
                                      partDescription2 (IX-K, IX-A)))           
                                                                                
             PERFORM DDA-GET-UNCODE                                             
           END-PERFORM                                                          
                                                                                
           .                                                                    
                                                                                
       DDA-GET-UNCODE SECTION.                                                  
                                                                                
           MOVE 1                      TO dangerous-num (IX-K, IX-A)            
           MOVE X'00'                  TO dangerous     (IX-K, IX-A)            
           MOVE ZERO                   TO unNumber-num  (IX-K, IX-A)            
           IF CLAG-IDPSN = 000 OR 048 OR 093 OR 098 OR 099 OR                   
                           900 OR 901 OR 902 OR 903 OR 904 OR                   
                           905 OR 907 OR 909 OR 910 OR 911 OR                   
                           912 OR 913                                           
             CONTINUE                                                           
           ELSE                                                                 
             MOVE 1                    TO dangerous-num (IX-K, IX-A)            
             MOVE X'01'                TO dangerous     (IX-K, IX-A)            
                                                                                
             MOVE CLAG-IDPSN           TO W-IDPSN                               
             SET UNCODE-MISSING        TO TRUE                                  
                                                                                
      *      Try English first.                                                 
             MOVE 'GB'                 TO W-IDSPRAK                             
             PERFORM IMS-GU-WDR201-1165                                         
                                                                                
             IF SEGMENT-FOUND                                                   
               PERFORM IMS-GNP-WDGX1168                                         
               PERFORM                                                          
                 UNTIL SEGMENT-MISSING OR UNCODE-FOUND                          
                 IF 1168-BEPSN (1) (1:2) = 'UN'                                 
                   MOVE 1168-BEPSN (1) (3:4)                                    
                                       TO unNumber      (IX-K, IX-A)            
                   MOVE 1              TO unNumber-num  (IX-K, IX-A)            
                 END-IF                                                         
                 PERFORM IMS-GNP-WDGX1168                                       
               END-PERFORM                                                      
             END-IF                                                             
                                                                                
      *      If we haven't found UN code under GB, search under SE.             
             IF UNCODE-MISSING                                                  
               MOVE 'SE'               TO W-IDSPRAK                             
               PERFORM IMS-GU-WDR201-1165                                       
                                                                                
               IF SEGMENT-FOUND                                                 
                 PERFORM IMS-GNP-WDGX1168                                       
                 PERFORM                                                        
                   UNTIL SEGMENT-MISSING OR UNCODE-FOUND                        
                   IF 1168-BEPSN (1) (1:2) = 'UN'                               
                     MOVE 1168-BEPSN (1) (3:4)                                  
                                       TO unNumber      (IX-K, IX-A)            
                     MOVE 1            TO unNumber-num  (IX-K, IX-A)            
                   END-IF                                                       
                   PERFORM IMS-GNP-WDGX1168                                     
                 END-PERFORM                                                    
               END-IF                                                           
             END-IF                                                             
           END-IF                                                               
           .                                                                    
                                                                                
       DE-CHECK-AND-SEND SECTION.                                               
                                                                                
           IF IX-K > 0 AND CONTINUE-YES                                         
             IF (EXPORT-NO AND REQU-KDCALL = 1) OR                              
                (EXPORT-YES AND REQU-KDCALL = 2) OR                             
                REQU-KDCALL = 3                                                 
               PERFORM DEA-SEND-TO-REQUEST                                      
             ELSE                                                               
               SET CONTINUE-NO         TO TRUE                                  
               IF IX-M < 10                                                     
                 COMPUTE IX-M = IX-M + 1                                        
                 STRING                                                         
                   'Ship id:' REQU-IDSHIPM                                      
                   ' Not sent! Exp is '                                         
                   EXPORT-SW ' & CALL is ' REQU-KDCALL                          
                   ', sender :' sendingDC2      '-'                             
                                pickup                                          
                   ', receiver :' receivingDC2  '-'                             
                                delivery                                        
                   DELIMITED BY SIZE                                            
                               INTO MESSAGE-TEXT (IX-M)                         
               END-IF                                                           
             END-IF                                                             
           END-IF                                                               
           .                                                                    
                                                                                
       DEA-SEND-TO-REQUEST SECTION.                                             
                                                                                
           IF EXPORT-YES                                                        
             MOVE YES                  TO SH2TO-FLEXPORT                        
           ELSE                                                                 
             MOVE NOO                  TO SH2TO-FLEXPORT                        
           END-IF                                                               
           MOVE 'PEND'                 TO SH2TO-TETORESP-D                      
           MOVE 4                      TO SH2TO-TETORESP-L                      
                                                                                
           MOVE SPACES                 TO SH2TO-IDTONR                          
           MOVE -1                     TO WS-IDTONR-NI                          
                                                                                
           PERFORM DB2-INSERT-SHIP2TO                                           
                                                                                
           IF INSERT-OK                                                         
             PERFORM DEAA-CALL-API-HANDLE-RESPONSE                              
           ELSE                                                                 
             PERFORM DB2-SELECT-SHIP2TO                                         
             IF ROW-FOUND AND                                                   
                WS-IDTONR-NI = -1 AND                                           
                (SH2TO-TETORESP-D NOT = 'PEND')                                 
               PERFORM DEAA-CALL-API-HANDLE-RESPONSE                            
             ELSE                                                               
               IF IX-M < 10                                                     
                 COMPUTE IX-M = IX-M + 1                                        
                 STRING                                                         
                   'Ship id:' REQU-IDSHIPM                                      
                   ' Not sent! Previously triggered'                            
                   ', sender :' sendingDC2      '-'                             
                                pickup                                          
                   ', receiver :' receivingDC2  '-'                             
                                delivery                                        
                        DELIMITED BY SIZE                                       
                                     INTO MESSAGE-TEXT (IX-M)                   
               END-IF                                                           
             END-IF                                                             
           END-IF                                                               
                                                                                
           .                                                                    
                                                                                
       DEAA-CALL-API-HANDLE-RESPONSE SECTION.                                   
                                                                                
           PERFORM S20-CALL-API                                                 
                                                                                
           IF CALL-KDRC = 0                                                     
             INITIALIZE TO-PAYLOAD                                              
             JSON PARSE TO-RESPONSE(1:CALL-KVDLEN-OUT)                          
               INTO TO-PAYLOAD                                                  
               NAME TO-PAYLOAD IS OMITTED                                       
             END-JSON                                                           
             IF IX-M < 10                                                       
               COMPUTE IX-M = IX-M + 1                                          
               STRING                                                           
                 'Ship id:' shipmentId                                          
                 ' TO : '                                                       
                 FUNCTION TRIM (toNumber)                                       
                 ', sender :' sendingDC2      '-'                               
                              pickup                                            
                 ', receiver :' receivingDC2  '-'                               
                              delivery                                          
                      DELIMITED BY SIZE                                         
                                     INTO MESSAGE-TEXT (IX-M)                   
             END-IF                                                             
             MOVE SPACES               TO SH2TO-TETORESP-D                      
             MOVE FUNCTION TRIM (FUNCTION DISPLAY-OF (                          
                    FUNCTION NATIONAL-OF (                                      
                      TO-RESPONSE (1:CALL-KVDLEN-OUT), 1208), 278))             
                                         TO SH2TO-TETORESP-D                    
             COMPUTE SH2TO-TETORESP-L = FUNCTION LENGTH (                       
                                   FUNCTION TRIM (SH2TO-TETORESP-D))            
                                                                                
             MOVE toNumber             TO SH2TO-IDTONR                          
             MOVE 0                    TO WS-IDTONR-NI                          
           ELSE                                                                 
             MOVE 'F'                  TO RECV-KDKOMSTA                         
             MOVE SPACES               TO SH2TO-IDTONR                          
             MOVE -1                   TO WS-IDTONR-NI                          
             IF CALL-KVDLEN-OUT > 0                                             
               INSPECT TO-RESPONSE (1:CALL-KVDLEN-OUT)                          
                                        REPLACING ALL X'00'                     
                                                   BY X'40'                     
               MOVE SPACES             TO WS-MESSAGE                            
                                          SH2TO-TETORESP                        
               MOVE FUNCTION TRIM (FUNCTION DISPLAY-OF (                        
                    FUNCTION NATIONAL-OF (                                      
                      TO-RESPONSE (1:CALL-KVDLEN-OUT), 1047), 278))             
                                       TO WS-MESSAGE                            
                                          SH2TO-TETORESP-D                      
               COMPUTE SH2TO-TETORESP-L = FUNCTION LENGTH (                     
                                   FUNCTION TRIM (SH2TO-TETORESP-D))            
             ELSE                                                               
               MOVE 'Unknown error!'   TO WS-MESSAGE                            
                                          SH2TO-TETORESP-D                      
               MOVE 14                 TO SH2TO-TETORESP-L                      
             END-IF                                                             
             IF IX-M < 10                                                       
               COMPUTE IX-M = IX-M + 1                                          
               STRING                                                           
                 'Ship id:' shipmentId                                          
                 ' TO create err : '                                            
                 ', sender :' sendingDC2      '-'                               
                              pickup                                            
                 ', receiver :' receivingDC2  '-'                               
                              delivery                                          
                    DELIMITED BY SIZE                                           
                                     INTO MESSAGE-TEXT (IX-M)                   
             END-IF                                                             
             IF IX-M < 10                                                       
               COMPUTE IX-M = IX-M + 1                                          
               MOVE WS-MESSAGE (1:100) TO MESSAGE-TEXT (IX-M)                   
             END-IF                                                             
           END-IF                                                               
           PERFORM DB2-UPDATE-SHIP2TO                                           
           .                                                                    
                                                                                
      *    --- DISPATCHER SECTIONS                                              
       S01-RECV-OPEN SECTION.                                                   
           MOVE 'S01-RECV-OPEN'        TO WS-SECTION                            
                                                                                
           MOVE 'OPEN'                 TO RECV-KDFUNC                           
           MOVE 'CARPARTS.PULS.SHIP2TMS'                                        
                                       TO RECV-ADDISPABS                        
                                                                                
           CALL WZ01RECV            USING RECV-CONTROL-AREA                     
                                          RECV-OPEN-AREA                        
                                                                                
           IF RECV-KDRC > 0                                                     
             MOVE RECV-KDRC            TO KDRC-DISPLAY                          
             STRING 'WZ01RECV OPEN ERROR RC= ' KDRC-DISPLAY                     
                   DELIMITED BY SIZE INTO FELTEXT                               
             CALL FELLOG                                                        
           END-IF                                                               
           .                                                                    
                                                                                
       S02-RECV-MESSAGE SECTION.                                                
           MOVE 'S02-RECV-MESSAGE'     TO WS-SECTION                            
                                                                                
           MOVE 'GET'                  TO RECV-KDFUNC                           
           MOVE LENGTH OF REQU-AREA    TO RECV-KVDLEN                           
           CALL WZ01RECV            USING RECV-CONTROL-AREA                     
                                          RECV-KVDLEN                           
                                          REQU-AREA                             
           IF RECV-KDRC > 1                                                     
             MOVE RECV-KDRC            TO KDRC-DISPLAY                          
             STRING 'WZ01RECV GET  ERROR RC= ' KDRC-DISPLAY                     
                   DELIMITED BY SIZE INTO FELTEXT                               
             CALL FELLOG                                                        
           END-IF                                                               
           .                                                                    
                                                                                
       S03-RECV-CLOSE SECTION.                                                  
           MOVE 'S03-RECV-CLOSE'       TO WS-SECTION                            
                                                                                
           MOVE 'CLOSE'                TO RECV-KDFUNC                           
           CALL WZ01RECV            USING RECV-CONTROL-AREA                     
                                          RECV-CLOSE-AREA                       
                                                                                
           IF RECV-KDRC > 0                                                     
             MOVE RECV-KDRC            TO KDRC-DISPLAY                          
             STRING 'WZ01RECV CLOSE ERROR RC= ' KDRC-DISPLAY                    
                   DELIMITED BY SIZE INTO FELTEXT                               
             CALL FELLOG                                                        
           END-IF                                                               
           .                                                                    
                                                                                
       S10-GET-UTC-TIMESTAMP SECTION.                                           
                                                                                
           MOVE '014'                  TO MSGI-KDCALL                           
           MOVE SEND-DCS-IDTIDZON      TO MSGI-IDTIDZON                         
           MOVE SEND-DCS-IDDC          TO MSGI-IDDC                             
           MOVE WS-DATE-YYMMDD         TO MSGI-TILOKDAT                         
           MOVE WS-TIME-HHMM           TO MSGI-TILOKTID                         
           CALL WL01TIDZ            USING MSGI-WL01TIDZ                         
           MOVE 20                     TO WS-DATE-UTC-CC                        
           MOVE MSGI-TILOKDAT          TO WS-DATE-UTC-YYMMDD                    
           MOVE MSGI-TILOKTID          TO WS-TIME-UTC-HHMM                      
           MOVE WS-TIME-SS             TO WS-TIME-UTC-SS                        
                                                                                
           MOVE FUNCTION FORMATTED-DATETIME (                                   
                  'YYYY-MM-DDThh:mm:ssZ',                                       
                  FUNCTION INTEGER-OF-DATE (WS-DATE-UTC),                       
                  FUNCTION SECONDS-FROM-FORMATTED-TIME (                        
                    'hhmmss', WS-TIME-UTC-X), 0)                                
                                       TO WS-TIMESTAMP-UTC                      
           .                                                                    
                                                                                
       S20-CALL-API SECTION.                                                    
                                                                                
           PERFORM IMS-GU-WDGX0104                                              
           IF SEGMENT-FOUND                                                     
             MOVE 0104-IDUSERKEY       TO user-key                              
             COMPUTE user-key-length = FUNCTION LENGTH (                        
                                       FUNCTION TRIM (user-key))                
           ELSE                                                                 
             CALL FELLOG                                                        
           END-IF                                                               
                                                                                
           MOVE W-ADDISPABS            TO CALL-ADDISPABS                        
           MOVE LENGTH OF TO-REQUEST   TO CALL-KVDLEN-IN                        
           MOVE LENGTH OF TO-RESPONSE  TO CALL-KVDLEN-OUT                       
           CALL WZ01CALL            USING CALL-CONTROL-AREA                     
                                          CALL-KVDLEN-IN                        
                                          TO-REQUEST                            
                                          CALL-KVDLEN-OUT                       
                                          TO-RESPONSE                           
           .                                                                    
                                                                                
       IMS-GU-WDGX0104  SECTION.                                                
                                                                                
           MOVE SPACES                 TO SSA1                                  
                                          SSA2                                  
           STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-0103-X ')'                    
                   DELIMITED BY SIZE INTO SSA1                                  
           STRING 'WDGX0104(ADDISPAB =' W-KY0104-X ')'                          
                   DELIMITED BY SIZE INTO SSA2                                  
           MOVE '  '                   TO GOOD-STATUSCODES                      
           CALL CBLTDLI             USING GU                                    
                                          ATAB-PCB                              
                                          DLI-IO-WDGX0104                       
                                          SSA1                                  
                                          SSA2                                  
           MOVE ATAB-STATUS-CODE       TO STATUS-WS                             
           PERFORM IMS-STATUSCHECK                                              
           .                                                                    
       IMS-GU-WDE101 SECTION.                                                   
                                                                                
           MOVE SPACES                 TO SSA1                                  
           STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
                   DELIMITED BY SIZE INTO SSA1                                  
           MOVE '  GE'                 TO GOOD-STATUSCODES                      
           CALL CBLTDLI             USING GU                                    
                                          WDE1-PCB                              
                                          DLI-IO-WDE101                         
                                          SSA1                                  
           MOVE WDE1-STATUS-CODE       TO STATUS-WS                             
           PERFORM IMS-STATUSCHECK                                              
           .                                                                    
                                                                                
       IMS-GNP-WDE111 SECTION.                                                  
                                                                                
           MOVE 'WDE111  '             TO SSA1                                  
           MOVE '  GE'                 TO GOOD-STATUSCODES                      
           CALL CBLTDLI             USING GNP                                   
                                          WDE1-PCB                              
                                          DLI-IO-WDE111                         
                                          SSA1                                  
           MOVE WDE1-STATUS-CODE       TO STATUS-WS                             
           PERFORM IMS-STATUSCHECK                                              
           .                                                                    
                                                                                
       IMS-GNP-WDE122 SECTION.                                                  
                                                                                
           MOVE SPACES                 TO SSA1                                  
           STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
                   DELIMITED BY SIZE INTO SSA1                                  
           MOVE 'WDE122  '             TO SSA2                                  
           MOVE '  GE'                 TO GOOD-STATUSCODES                      
           CALL CBLTDLI             USING GNP                                   
                                          WDE1-PCB                              
                                          DLI-IO-WDE122                         
                                          SSA1                                  
                                          SSA2                                  
           MOVE WDE1-STATUS-CODE       TO STATUS-WS                             
           PERFORM IMS-STATUSCHECK                                              
           .                                                                    
                                                                                
       IMS-GNP-WDE121 SECTION.                                                  
                                                                                
           MOVE SPACES                 TO SSA1                                  
           STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
                   DELIMITED BY SIZE INTO SSA1                                  
           MOVE 'WDE121  '             TO SSA2                                  
           MOVE '  GE'                 TO GOOD-STATUSCODES                      
           CALL CBLTDLI             USING GNP                                   
                                          WDE1-PCB                              
                                          DLI-IO-WDE121                         
                                          SSA1                                  
                                          SSA2                                  
           MOVE WDE1-STATUS-CODE       TO STATUS-WS                             
           PERFORM IMS-STATUSCHECK                                              
           .                                                                    
                                                                                
       IMS-GNP-WDE131 SECTION.                                                  
                                                                                
           MOVE SPACES                 TO SSA1                                  
                                          SSA2                                  
           STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
                   DELIMITED BY SIZE INTO SSA1                                  
           STRING 'WDE121  (WDE121KY =' W-WDE121KY-X ')'                        
                   DELIMITED BY SIZE INTO SSA2                                  
           MOVE 'WDE131  '             TO SSA3                                  
           MOVE '  GE'                 TO GOOD-STATUSCODES                      
           CALL CBLTDLI             USING GNP                                   
                                          WDE1-PCB                              
                                          DLI-IO-WDE131                         
                                          SSA1                                  
                                          SSA2                                  
                                          SSA3                                  
           MOVE WDE1-STATUS-CODE       TO STATUS-WS                             
           PERFORM IMS-STATUSCHECK                                              
           .                                                                    
                                                                                
       IMS-GU-WDD311 SECTION.                                                   
                                                                                
           MOVE SPACES                 TO SSA1                                  
                                          SSA2                                  
           STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
                   DELIMITED BY SIZE INTO SSA1                                  
           STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
                   DELIMITED BY SIZE INTO SSA2                                  
           MOVE '  GE'                 TO GOOD-STATUSCODES                      
           CALL CBLTDLI             USING GU                                    
                                          WDD3-PCB                              
                                          DLI-IO-WDD311                         
                                          SSA1                                  
                                          SSA2                                  
           MOVE WDD3-STATUS-CODE       TO STATUS-WS                             
           PERFORM IMS-STATUSCHECK                                              
           .                                                                    
                                                                                
       IMS-GU-WDB601-SEND SECTION.                                              
                                                                                
           MOVE SPACES                 TO SSA1                                  
           STRING 'WDB601  (IDDC     =' W-IDDC-SEND-X ')'                       
                   DELIMITED BY SIZE INTO SSA1                                  
           MOVE '  '                   TO GOOD-STATUSCODES                      
           CALL CBLTDLI             USING GU                                    
                                          WDB6-PCB                              
                                          DLI-IO-WDB601-SEND                    
                                          SSA1                                  
           MOVE WDB6-STATUS-CODE       TO STATUS-WS                             
           PERFORM IMS-STATUSCHECK                                              
           .                                                                    
                                                                                
       IMS-GU-WDB601-REC SECTION.                                               
                                                                                
           MOVE SPACES                 TO SSA1                                  
           STRING 'WDB601  (IDDC     =' W-IDDC-REC-X ')'                        
                   DELIMITED BY SIZE INTO SSA1                                  
           MOVE '  '                   TO GOOD-STATUSCODES                      
           CALL CBLTDLI             USING GU                                    
                                          WDB6-PCB                              
                                          DLI-IO-WDB601-REC                     
                                          SSA1                                  
           MOVE WDB6-STATUS-CODE       TO STATUS-WS                             
           PERFORM IMS-STATUSCHECK                                              
           .                                                                    
                                                                                
                                                                                
       IMS-GU-WDK601 SECTION.                                                   
                                                                                
           MOVE SPACES                 TO SSA1                                  
           STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
                   DELIMITED BY SIZE INTO SSA1                                  
           MOVE '  '                   TO GOOD-STATUSCODES                      
           CALL CBLTDLI             USING GU                                    
                                          WDK6-PCB                              
                                          DLI-IO-WDK601                         
                                          SSA1                                  
           MOVE WDK6-STATUS-CODE       TO STATUS-WS                             
           PERFORM IMS-STATUSCHECK                                              
           .                                                                    
                                                                                
       IMS-GNP-WDK611 SECTION.                                                  
                                                                                
           MOVE 'WDK611  '             TO SSA1                                  
           MOVE '  '                   TO GOOD-STATUSCODES                      
           CALL CBLTDLI             USING GNP                                   
                                          WDK6-PCB                              
                                          DLI-IO-WDK611                         
                                          SSA1                                  
           MOVE WDK6-STATUS-CODE       TO STATUS-WS                             
           PERFORM IMS-STATUSCHECK                                              
           .                                                                    
                                                                                
       IMS-GU-WDR201-1165 SECTION.                                              
                                                                                
           STRING 'WDR201  (WDGXKEY  =' W-WDR201-1165-X ')'                     
                   DELIMITED BY SIZE INTO SSA1                                  
           MOVE '  GE'                 TO GOOD-STATUSCODES                      
           CALL CBLTDLI             USING GU                                    
                                          1165-PCB                              
                                          DLI-IO-WDGX1165                       
                                          SSA1                                  
           MOVE 1165-STATUS-CODE       TO STATUS-WS                             
           PERFORM IMS-STATUSCHECK                                              
           .                                                                    
                                                                                
       IMS-GNP-WDGX1168 SECTION.                                                
                                                                                
           MOVE 'WDGX1168 '            TO SSA1                                  
           MOVE '  GE'                 TO GOOD-STATUSCODES                      
           CALL CBLTDLI             USING GNP                                   
                                          1165-PCB                              
                                          DLI-IO-WDGX1168                       
                                          SSA1                                  
           MOVE 1165-STATUS-CODE       TO STATUS-WS                             
           PERFORM IMS-STATUSCHECK                                              
           .                                                                    
                                                                                
       IMS-GU-WDGX4402 SECTION.                                                 
                                                                                
           STRING 'WDR101  (WDGXKEY  =' W-WDR101-4401-X ')'                     
                   DELIMITED BY SIZE INTO SSA1                                  
           STRING 'WDR113  (KY4402   =' W-KY4402-X                              
                          '&IDTRPTNR =' W-IDTRPTNR-X ')'                        
                   DELIMITED BY SIZE INTO SSA2                                  
           MOVE '  GE'                 TO GOOD-STATUSCODES                      
           CALL CBLTDLI             USING GU                                    
                                          WDR1-PCB                              
                                          DLI-IO-WDGX4402                       
                                          SSA1                                  
                                          SSA2                                  
           MOVE WDR1-STATUS-CODE       TO STATUS-WS                             
           PERFORM IMS-STATUSCHECK                                              
           .                                                                    
                                                                                
       IMS-GU-WDGX4406 SECTION.                                                 
                                                                                
           STRING 'WDR101  (WDGXKEY  =' W-WDR101-4405-X ')'                     
                   DELIMITED BY SIZE INTO SSA1                                  
           STRING 'WDR116  (WDGXKEY  =' W-KY4406-X ')'                          
                   DELIMITED BY SIZE INTO SSA2                                  
           MOVE '  GE'                 TO GOOD-STATUSCODES                      
           CALL CBLTDLI             USING GU                                    
                                          WDR1-PCB                              
                                          DLI-IO-WDGX4406                       
                                          SSA1                                  
                                          SSA2                                  
           MOVE WDR1-STATUS-CODE       TO STATUS-WS                             
           PERFORM IMS-STATUSCHECK                                              
           .                                                                    
                                                                                
       IMS-GU-WDB201 SECTION.                                                   
                                                                                
           MOVE SPACES                 TO SSA1                                  
           STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
                   DELIMITED BY SIZE INTO SSA1                                  
           MOVE '  '                   TO GOOD-STATUSCODES                      
           CALL CBLTDLI             USING GU                                    
                                          WDB2-PCB                              
                                          DLI-IO-WDB201                         
                                          SSA1                                  
           MOVE WDB2-STATUS-CODE       TO STATUS-WS                             
           PERFORM IMS-STATUSCHECK                                              
           .                                                                    
                                                                                
       IMS-STATUSCHECK SECTION.                                                 
                                                                                
           SET STATUS-IX               TO 1                                     
           SEARCH GOOD-STATUS                                                   
             AT END                                                             
               STRING 'INVALID IMS STATUS CODE: ' STATUS-WS                     
                   DELIMITED BY SIZE INTO ERROR-TEXT                            
               CALL FELLOG                                                      
             WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
               CONTINUE                                                         
           END-SEARCH                                                           
           .                                                                    
                                                                                
       DB2-INSERT-SHIP2TO  SECTION.                                             
                                                                                
           MOVE 000803                 TO GOOD-SQLCODECODES                     
                                                                                
           EXEC SQL                                                             
             INSERT INTO SHIP2TO                                                
               (  IDSHIPM                                                       
                , TISKEPPN                                                      
                , TISKPTID                                                      
                , IDLBBET                                                       
                , IDTRPTNR                                                      
                , IDDC_SEND                                                     
                , IDPARTNER_REC                                                 
                , KVKOLLI                                                       
                , FLEXPORT                                                      
                , TITOREQ                                                       
                , IDTONR                                                        
                , TETORESP)                                                     
                                                                                
             VALUES                                                             
               (  :SH2TO-IDSHIPM                                                
                , :SH2TO-TISKEPPN                                               
                , :SH2TO-TISKPTID                                               
                , :SH2TO-IDLBBET                                                
                , :SH2TO-IDTRPTNR                                               
                , :SH2TO-IDDC-SEND                                              
                , :SH2TO-IDPARTNER-REC                                          
                , :SH2TO-KVKOLLI                                                
                , :SH2TO-FLEXPORT                                               
                , CURRENT TIMESTAMP                                             
                , :SH2TO-IDTONR :WS-IDTONR-NI                                   
                , :SH2TO-TETORESP)                                              
           END-EXEC                                                             
                                                                                
           MOVE SQLCODE                TO SQLCODE-WS                            
           PERFORM DB2-STATUS-CHECK                                             
           .                                                                    
                                                                                
       DB2-UPDATE-SHIP2TO  SECTION.                                             
                                                                                
           MOVE 000                    TO GOOD-SQLCODECODES                     
                                                                                
           EXEC SQL                                                             
             UPDATE SHIP2TO                                                     
             SET                                                                
                  IDTONR        = :SH2TO-IDTONR :WS-IDTONR-NI                   
                , TETORESP      = :SH2TO-TETORESP                               
                , TITOREQ       = CURRENT TIMESTAMP                             
                                                                                
             WHERE                                                              
                  IDSHIPM       = :SH2TO-IDSHIPM                                
              AND TISKEPPN      = :SH2TO-TISKEPPN                               
              AND IDPARTNER_REC = :SH2TO-IDPARTNER-REC                          
           END-EXEC                                                             
                                                                                
           MOVE SQLCODE                TO SQLCODE-WS                            
           PERFORM DB2-STATUS-CHECK                                             
           .                                                                    
                                                                                
       DB2-SELECT-SHIP2TO  SECTION.                                             
                                                                                
           MOVE 000100                 TO GOOD-SQLCODECODES                     
                                                                                
           EXEC SQL                                                             
             SELECT                                                             
                  IDTONR                                                        
                , TETORESP                                                      
             INTO                                                               
                  :SH2TO-IDTONR :WS-IDTONR-NI                                   
                , :SH2TO-TETORESP                                               
                                                                                
             FROM SHIP2TO                                                       
             WHERE                                                              
                  IDSHIPM       = :SH2TO-IDSHIPM                                
              AND TISKEPPN      = :SH2TO-TISKEPPN                               
              AND IDPARTNER_REC = :SH2TO-IDPARTNER-REC                          
           END-EXEC                                                             
                                                                                
           MOVE SQLCODE                TO SQLCODE-WS                            
           PERFORM DB2-STATUS-CHECK                                             
           .                                                                    
                                                                                
       DB2-STATUS-CHECK  SECTION.                                               
                                                                                
           SET SQLCODE-IX              TO 1                                     
           SEARCH GOOD-SQLCODE                                                  
             AT END                                                             
               STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS                
                   DELIMITED BY SIZE INTO ERROR-TEXT                            
               CALL ABEND           USING RKOD-ABEND-DB2                        
             WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                        
               CONTINUE                                                         
           END-SEARCH                                                           
           .                                                                    
