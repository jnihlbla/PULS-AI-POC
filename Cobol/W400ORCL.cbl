000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W400ORCL.                                                
000400 AUTHOR.         ARUP DATTA.                                              
000500 DATE-WRITTEN.   2024-04-15.                                              
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*                                                                         
001000*    FUNCTION:                                                            
001100*        THE IS A SUBPROGRAM WHICH HANDLES ORDER LINE                     
001200*        CANCELLATION                                                     
001400*        TRANSACTION W4T254 INVOKED FOR ORDERLINE CANCELLATION            
001500*                                                                         
001600*        THE PROGRAM READS     WDQ2                                       
001700*                              WDQ3                                       
001800*                              WDQ4                                       
001900*                              WDA5                                       
002000*                              WDF5                                       
002100*                                                                         
002200*    ABENDCODES:                                                          
002300*        U0016 -  . . . .                                                 
002400*        U1000 -  . . . .                                                 
002500*                                                                         
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP2                                                                
003600 FILE SECTION.                                                            
003700     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900                                                                          
004000 77  IDPGM                       PIC X(8)    VALUE 'W400ORCL'.            
004100 77  WS-CURRENT-SECTION          PIC X(30)   VALUE SPACE.                 
004200 77  WS-CURRENT-IMS-SECTION      PIC X(30)   VALUE SPACE.                 
004210 77  WS-CALL-4254                PIC X       VALUE 'Y'.                   
004300 77  YES                         PIC X       VALUE 'Y'.                   
004310 77  JAA                         PIC X       VALUE 'J'.                   
004400 77  NOO                         PIC X       VALUE 'N'.                   
004500 77  FEL                         PIC X       VALUE 'F'.                   
004600     EJECT                                                                
004700 77  KEYS-SW                     PIC X       VALUE 'J'.                   
004800     88  KEYS-OK                             VALUE 'J'.                   
004900     88  KEYS-ERR                            VALUE 'N'.                   
005000*                                                                         
005100 77  OK-SW                       PIC X       VALUE 'Y'.                   
005200     88  EVERYTHING-OK                       VALUE 'Y'.                   
005300     88  SOMETHING-WRONG                     VALUE 'N'.                   
005400                                                                          
005500 77  PRINTED-SW                  PIC X       VALUE 'N'.                   
005600     88  ORDER-IS-PRINTED                    VALUE 'Y'.                   
005700                                                                          
005800 77  ORDER-HEAD-SW               PIC X       VALUE 'N'.                   
005900     88  DEL-COMP-ORDER                      VALUE 'Y'.                   
006000                                                                          
006100 77  ORDER-LINE-SW               PIC X       VALUE 'N'.                   
006200     88  DEL-ORDER-LINE                      VALUE 'Y'.                   
006300                                                                          
006400 77  BO-CHECK-SW                 PIC X       VALUE 'N'.                   
006500     88  BO-FOUND                            VALUE 'Y'.                   
006510     88  BO-NOT-FOUND                        VALUE 'N'.                   
006520     88  BO-DELETE                           VALUE 'P'.                   
006600                                                                          
006700 77  DDGS-CHECK-SW               PIC X       VALUE 'N'.                   
006800     88  DDGS-FOUND                          VALUE 'Y'.                   
006810     88  DDGS-NOT-FOUND                      VALUE 'N'.                   
006900                                                                          
007000 77  SW-VALID-LINE               PIC X       VALUE 'N'.                   
007100     88  VALID-ORD-LINE                      VALUE 'Y'.                   
007200                                                                          
007300     EJECT                                                                
007400 01  WS-WORK-AREA.                                                        
007500     03  KVRADER-IX                PIC S9(5)  VALUE +0 COMP SYNC.         
007600     03  KVRADER-IX-MAX            PIC S9(5)  VALUE +0 COMP SYNC.         
007700     03  MSG-IX                    PIC S9(9)  VALUE +0 COMP SYNC.         
007800     03  W-BLANKS                  PIC  9(5)  VALUE ZERO.                 
007900     03  W-LENGTH                  PIC  9(5)  VALUE ZERO.                 
008000     03  W-IDARTNR                 PIC  9(9)  VALUE ZERO.                 
008100     03  WS-KDSTARAD               PIC  9(1)  VALUE ZERO.                 
008200     03  WS-TIREGDAT               PIC  9(6)  VALUE ZERO.                 
008300     03  WS-IDDISTR                PIC  9(4)  VALUE ZERO.                 
008400     03  WS-IDKUNDNR               PIC  9(6)  VALUE ZERO.                 
008500     03  WS-IDORDNR                PIC  9(7)  VALUE ZERO.                 
008600     03  WS-IDARTNR                PIC  9(9)  VALUE ZERO.                 
008700     03  WS-KVBEART                PIC  9(6)  VALUE ZERO.                 
008800     03  WS-IDDC                   PIC  X(2)  VALUE SPACE.                
008810     03  WS-IDDC-WDA5              PIC  X(2)  VALUE SPACE.                
008900     03  WS-KVRADER-IX-NUM         PIC  9(5)  VALUE ZERO.                 
009000     03  WS-KVRADER-IX-DISP        PIC  X(5)  VALUE SPACES.               
009100*                                                                         
009200     03  WS-UPD-FUNC               PIC X(1)   VALUE SPACES.               
009300         88 UPD-ORD-LINE                      VALUE 'U'.                  
009400         88 ADD-ORD-LINE                      VALUE 'A'.                  
009500*                                                                         
009600     03  WS-IDSYSTEM               PIC  X(4)  VALUE SPACES.               
009700         88 IDSYSTEM-LYNK                     VALUE 'LYNK'.               
009800         88 IDSYSTEM-POLE                     VALUE 'POLE'.               
009900         88 IDSYSTEM-ECOM                     VALUE 'ECOM'.               
010000         88 IDSYSTEM-VOUI                     VALUE 'VOUI'.               
010100         88 IDSYSTEM-TAD                      VALUE 'TAD '.               
010200         88 IDSYSTEM-ACC                      VALUE 'ACC '.               
010300         88 IDSYSTEM-APA                      VALUE 'APA '.               
010400         88 IDSYSTEM-APB                      VALUE 'APB '.               
010500         88 IDSYSTEM-APC                      VALUE 'APC '.               
010600         88 IDSYSTEM-APD                      VALUE 'APD '.               
010700         88 IDSYSTEM-APE                      VALUE 'APE '.               
010800         88 IDSYSTEM-APF                      VALUE 'APF '.               
010900         88 IDSYSTEM-APG                      VALUE 'APG '.               
011000         88 IDSYSTEM-APH                      VALUE 'APH '.               
011100         88 IDSYSTEM-API                      VALUE 'API '.               
011200         88 IDSYSTEM-APJ                      VALUE 'APJ '.               
011300         88 IDSYSTEM-NO-ECOM                  VALUE 'LYNK' 'POLE'         
011400                                                    'VOUI' 'TAD '         
011500                                                    'ACC ' 'APA '         
011600                                                    'APB ' 'APC '         
011700                                                    'APD ' 'APE '         
011800                                                    'APF ' 'APG '         
011900                                                    'APH ' 'API '         
012000                                                    'APJ '.               
012100         88 IDSYSTEM-VALID                    VALUE 'LYNK' 'POLE'         
012200                                                    'ECOM'                
012300                                                    'VOUI' 'TAD '         
012400                                                    'ACC ' 'APA '         
012500                                                    'APB ' 'APC '         
012600                                                    'APD ' 'APE '         
012700                                                    'APF ' 'APG '         
012800                                                    'APH ' 'API '         
012900                                                    'APJ '.               
013000                                                                          
024900*working storage for ddgs                                                 
024901     03  WS-WDE401-DDGS.                                                  
024902       05 WS-401-IDDISTR-DDGS    PIC S9(5)      VALUE ZERO COMP-3.        
024903       05 WS-401-IDKUNDNR-DDGS   PIC S9(7)      VALUE ZERO COMP-3.        
024904       05 WS-401-IDKUNDRF-DDGS.                                           
024905           07  WS-401-IDORDNR-DDGS PIC 9(5)     VALUE ZERO.               
024906           07  FILLER              PIC X(5)     VALUE SPACE.              
024907       05  WS-401-IDPRODNR-DDGS  PIC S9(7)      VALUE ZERO COMP-3.        
024908       05  WS-401-IDPLKLST-DDGS  PIC S9(3)      VALUE ZERO COMP-3.        
024909*                                                                         
024910 01  GENERAL-SUBPROGRAMS.                                                 
024911*                                                                         
024912     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
024913     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
024914     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
024915     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
024916     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
024917     SKIP2                                                                
024918*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
024919                                                                          
024920 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
024921 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
024922 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
024923     SKIP2                                                                
024924 01  ERROR-TEXT.                                                          
024925     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
024926     03  ERROR-TEXT-STR          PIC X(70)   VALUE SPACE.                 
024927     EJECT                                                                
024928 01  FILLER                      PIC X(16)   VALUE 'WMSGCONV'.            
024929*01  -COPY WMSGCONV                                                       
024930                                                                          
024931*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
024932 01  FILLER                      PIC X(16)   VALUE 'WDATKONV'.            
024933*01 -COPY WDATAREA                                                        
024934                                                                          
024935*    --- AREAS FOR SUB MODULES W006KOM                                    
024936 01  FILLER                    PIC X(16) VALUE 'MSG-KOM-WMSGKOM '.        
024937*01  -COPY WMSGKOM                                                        
024938                                                                          
024939 01  FILLER                    PIC X(16) VALUE 'MSG-IO-AREA     '.        
024940*01  -COPY WMSGAREA                                                       
024941                                                                          
024942 01  FILLER                    PIC X(16) VALUE '4254-MID-AREA   '.        
024943*01  -COPY W4I25401 -PRE 4254-                                            
024944     EJECT                                                                
024945 01  FILLER                    PIC X(16) VALUE '4256-MID-AREA   '.        
024946*01  -COPY W4I25601 -PRE 4256-                                            
024947     EJECT                                                                
024948                                                                          
024949*    --- AREAS FOR IMS-SECTIONS                                           
024950*                                                                         
024951     EJECT                                                                
024952 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
024953     SKIP3                                                                
024954 01  KEYS-FOR-DLI.                                                        
024955                                                                          
024956     03  W-WDF5BSEQ.                                                      
024957         05  W-SEQB-IDLEVART     PIC X(30)   VALUE LOW-VALUE.             
024958                                                                          
024959     03  W-IDORDER-X.                                                     
024960         05  W-IDORDER           PIC S9(7)      COMP-3.                   
024961                                                                          
024962     03  W-WDQ2CSEQ.                                                      
024963         05  W-IDDISTR-CSEQ      PIC S9(5)   VALUE ZERO COMP-3.           
024964         05  W-IDKUNDNR-CSEQ     PIC S9(7)   VALUE ZERO COMP-3.           
024965         05  W-IDORDNR7-CSEQ     PIC 9(7)    VALUE ZERO.                  
024966         05  FILLER              PIC X(3)    VALUE SPACE.                 
024967                                                                          
024968     03  W-WDQ301KY-MIN.                                                  
024969         05  W-Q3-IDORDER-MIN    PIC S9(7)   COMP-3.                      
024970         05  FILLER              PIC X(8)    VALUE LOW-VALUE.             
024971                                                                          
024972     03  W-WDQ301KY-MAX.                                                  
024973         05  W-Q3-IDORDER-MAX    PIC S9(7)   COMP-3.                      
024974         05  FILLER              PIC X(8)    VALUE HIGH-VALUE.            
024975                                                                          
024976     03  W-WDA501KY-A5-MIN-X.                                             
024977         05  W-IDDISTR-MIN        PIC S9(5) COMP-3   VALUE ZERO.          
024978         05  W-IDKUNDNR-MIN       PIC S9(7) COMP-3   VALUE ZERO.          
024979         05  W-IDKUNDRF-MIN.                                              
024980             07  W-IDORDNR5-MIN   PIC 9(5)           VALUE ZERO.          
024981             07  FILLER           PIC X(5)           VALUE SPACE.         
024982         05  W-IDARTNR-MIN        PIC S9(9) COMP-3   VALUE ZERO.          
024983         05  FILLER               PIC X(2)  VALUE LOW-VALUE.              
024984                                                                          
024985     03  W-WDA501KY-A5-MAX-X.                                             
024986         05  W-IDDISTR-MAX        PIC S9(5) COMP-3   VALUE ZERO.          
024987         05  W-IDKUNDNR-MAX       PIC S9(7) COMP-3   VALUE ZERO.          
024988         05  W-IDKUNDRF-MAX.                                              
024989             07  W-IDORDNR5-MAX   PIC 9(5)           VALUE ZERO.          
024990             07  FILLER           PIC X(5)           VALUE SPACE.         
024991         05  W-IDARTNR-MAX        PIC S9(9) COMP-3   VALUE ZERO.          
024992         05  FILLER               PIC X(2)  VALUE HIGH-VALUE.             
024993                                                                          
024994     03  W-KVART-MIN-X.                                                   
024995         05  W-KVART-MIN          PIC S9(7)  COMP-3   VALUE ZERO.         
024996                                                                          
024997     03  W-KVART-MAX-X.                                                   
024998         05  W-KVART-MAX          PIC S9(7)  COMP-3   VALUE ZERO.         
024999                                                                          
025000     03  W-IDDC-MIN-X.                                                    
025001         05  W-IDDC-MIN           PIC X(02)  VALUE SPACES.                
025002                                                                          
025003     03  W-IDDC-MAX-X.                                                    
025004         05  W-IDDC-MAX           PIC X(02)  VALUE SPACES.                
025005                                                                          
025006     03  W-WDQ401KY-MIN-X.                                                
025007         05  W-Q4-IDORDER-MIN     PIC S9(7)  VALUE ZERO COMP-3.           
025008         05  FILLER               PIC X(16)  VALUE SPACE.                 
025009                                                                          
025010     03  W-WDQ401KY-MAX-X.                                                
025011         05  W-Q4-IDORDER-MAX     PIC S9(7)  VALUE ZERO COMP-3.           
025012         05  FILLER               PIC X(16)  VALUE SPACE.                 
025013                                                                          
025014     03  W-KDODELST-P.                                                    
025015         05  W-KDODELSTP          PIC X(1)   VALUE 'P'.                   
025016                                                                          
025017     03  W-KDODELST-U.                                                    
025018         05  W-KDODELSTU          PIC X(1)   VALUE 'U'.                   
025019                                                                          
025020*   -------- TILL WDB6                                                    
025021     03  W-IDDC-B6-X.                                                     
025022         05 W-IDDC-B6             PIC X(2).                               
025023     03  W-WDE401-X.                                                      
025024         05  W-401-IDDISTR      PIC S9(5)      VALUE ZERO COMP-3.         
025025         05  W-401-IDKUNDNR     PIC S9(7)      VALUE ZERO COMP-3.         
025026         05  W-401-IDKUNDRF.                                              
025027           07  W-401-IDORDNR    PIC 9(5)       VALUE ZERO.                
025028           07  FILLER           PIC X(5)       VALUE SPACE.               
025029         05  W-401-IDPRODNR     PIC S9(7)      VALUE ZERO COMP-3.         
025030         05  W-401-IDPLKLST     PIC S9(3)      VALUE ZERO COMP-3.         
025031*                                                                         
025032     03  W-WDE411-X.                                                      
025100         05  W-411-IDPURAD      PIC S9(5)      VALUE ZERO COMP-3.         
025200     SKIP2                                                                
025300*    --- STATUS-KOD FRÅN IMS                                              
025400 01  STATUS-WS                   PIC XX.                                  
025500     88  SEGMENT-FOUND                       VALUE '  '.                  
025600     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
025700     88  SEGMENT-MISSING                     VALUE 'GE'.                  
025800     88  SEGMENT-FINAL                       VALUE 'GB'.                  
025900     SKIP2                                                                
026000 01  GOOD-STATUSCODES.                                                    
026100     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
026200     SKIP3                                                                
026300 01  ALL-SSA.                                                             
026400     03 SSA1                     PIC X(160).                              
026500     03 SSA2                     PIC X(64).                               
026600     EJECT                                                                
026700*    --- IMS FUNCTION CODES                                               
026800*01  -COPY W0003                                                          
026900     EJECT                                                                
027000*                                                                         
027100*    --- VALID IDDC CODES                                                 
027200*                                                                         
027300*01    -COPY WWDC99                                                       
027400     EJECT                                                                
027500*    ---  DLI INPUT-OUTPUT AREA                                           
027600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF501'.                      
027700 01  DLI-IO-WDF501.                                                       
027800*    03  -COPY WDF501                                                     
027900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ201'.                      
028000 01  DLI-IO-WDQ201.                                                       
028100*    03  -COPY WDQ201                                                     
028200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ301'.                      
028300 01  DLI-IO-WDQ301.                                                       
028400*    03  -COPY WDQ301                                                     
028500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ401'.                      
028600 01  DLI-IO-WDQ401.                                                       
028700*    03  -COPY WDQ401                                                     
028800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA501'.                      
028900 01  DLI-IO-WDA501.                                                       
029000*    03  -COPY WDA501                                                     
029100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
029200 01   DLI-IO-WDB601.                                                      
029300*     03  -COPY WDB601                                                    
029400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE401'.                      
029500 01   DLI-IO-WDE401.                                                      
029600*     03  -COPY WDE401                                                    
029700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE411'.                      
029800 01   DLI-IO-WDE411.                                                      
029900*     03  -COPY WDE411 -PRE E4-                                           
030000                                                                          
030100     EJECT                                                                
030200 LINKAGE SECTION.                                                         
030300*    -COPY W400ORCL                                                       
030400*                                                                         
030500*01  -COPY W0009  -PRE MSG-                                               
030600                                                                          
030700*01  -COPY W0009  -PRE 0693X-                                             
030800                                                                          
030900*01  -COPY W0008  -PRE WDP8-                                              
031000     05  FILLER                  PIC X.                                   
031100                                                                          
031200*01  -COPY W0008  -PRE WDF5-                                              
031300     05  FILLER                  PIC X.                                   
031400*01  -COPY W0008  -PRE WDQ2CSEQ-                                          
031500     05  FILLER                  PIC X.                                   
031600*01  -COPY W0008  -PRE WDQ3-                                              
031700     05  FILLER                  PIC X.                                   
031800*01  -COPY W0008  -PRE WDQ4-                                              
031900     05  FILLER                  PIC X.                                   
032000*01  -COPY W0008  -PRE WDA5-                                              
032100     05  FILLER                  PIC X.                                   
032200*01  -COPY W0008  -PRE WDB6-                                              
032300     05  FILLER                  PIC X.                                   
032400*01  -COPY W0008  -PRE WDE4-                                              
032500     05  FILLER                  PIC X.                                   
032600     EJECT                                                                
032700 PROCEDURE DIVISION  USING ORCL-W400ORCL                                  
032800                           MSG-PCB                                        
032900                           0693X-PCB                                      
033000                           WDP8-PCB                                       
033100                           WDF5-PCB                                       
033200                           WDQ2CSEQ-PCB                                   
033300                           WDQ3-PCB                                       
033400                           WDQ4-PCB                                       
033500                           WDA5-PCB                                       
033600                           WDB6-PCB                                       
033700                           WDE4-PCB.                                      
033800 MAIN SECTION.                                                            
033900                                                                          
034000     PERFORM A-INIT                                                       
034100                                                                          
034200     PERFORM B-VALIDATE                                                   
034300                                                                          
034400     IF KEYS-OK                                                           
034500       EVALUATE ORCL-KDCALL                                               
034600         WHEN 001                                                         
034700           PERFORM C-VALIDATE-ORDERLINE-CANCEL                            
034800           IF EVERYTHING-OK                                               
035000              PERFORM D-DELETE-ORDERLINES                                 
035100           END-IF                                                         
035400         WHEN OTHER                                                       
035500            MOVE NOO              TO KEYS-SW                              
035600            MOVE '023'            TO ORCL-IDMSG-ERROR                     
035700            MOVE 'KDCALL'         TO ORCL-IDELMT-ERROR                    
035800            MOVE 'CALL OPTION INVALID'                                    
035900                                  TO ORCL-FEL-TEXT                        
036000            SET ORCL-KDSVAR-FEL   TO TRUE                                 
036100       END-EVALUATE                                                       
036200     END-IF                                                               
036300                                                                          
036400     PERFORM Z-FINIT                                                      
036500                                                                          
036600     MOVE ZERO TO RETURN-CODE                                             
036700     GOBACK                                                               
036800     .                                                                    
036900     EJECT                                                                
037000 A-INIT SECTION.                                                          
037100                                                                          
037200     MOVE 'A-INIT'                   TO WS-CURRENT-SECTION                
037300                                                                          
037400     MOVE  SPACES                    TO ORCL-OUTPUT-DATA                  
037500                                                                          
037900                                                                          
038000     MOVE ORCL-IDSYSTEM              TO WS-IDSYSTEM                       
038400                                                                          
038500                                                                          
038600*    -- INITIALIZE W006KOM AREAS WITH FIXED VALUES                        
038700*    -- FIELDS WITH VARYING CONTENT ARE SET LATER                         
038800     MOVE SPACE                      TO MSG-KOM-WMSGKOM                   
038900     MOVE LENGTH OF MSG-KOM-WMSGKOM  TO MSG-KOM-KVLL                      
039000     MOVE LOW-VALUE                  TO MSG-KOM-KDZ1                      
039100     MOVE LOW-VALUE                  TO MSG-KOM-KDZ2                      
039200     MOVE 'W400ORCL'                 TO MSG-KOM-IDSNDJOB                  
039300     MOVE FUNCTION CURRENT-DATE(3:6) TO MSG-KOM-TIREGDAT                  
039400*    -- TIKLOCK WILL BE INCREMENTENTED FOR EACH part                      
039500*    -- THIS IS THE START VALUE                                           
039600     MOVE FUNCTION CURRENT-DATE(9:8) TO MSG-KOM-TIKLOCK                   
039700                                                                          
039800*    -- INITIALIZE TARGET TRANSACTION AREA WITH FIXED VALUES              
039900     MOVE LOW-VALUE                  TO MSG-KDZ1                          
040000     MOVE LOW-VALUE                  TO MSG-KDZ2                          
040100     .                                                                    
040200     EJECT                                                                
040300 B-VALIDATE SECTION.                                                      
040400                                                                          
040500     MOVE 'B-VALIDATE'                TO WS-CURRENT-SECTION               
040600*                                                                         
040700***  VALIDATE ORDER ID - ORDERREF                                         
040800***  THIS IS COMBINATION OF DISTR, CUSTOMER, ORDER, DATE                  
040900*                                                                         
041000     IF KEYS-OK                                                           
041100        INSPECT ORCL-IDDISTR REPLACING LEADING SPACE BY ZERO              
041200        IF ORCL-IDDISTR        NOT NUMERIC                                
041300           MOVE '022'           TO ORCL-IDMSG-ERROR                       
041400           MOVE 'IDDISTR'       TO ORCL-IDELMT-ERROR                      
041500           MOVE 'DIST NOT NUMERIC'                                        
041600                                TO ORCL-FEL-TEXT                          
041700           MOVE NOO             TO KEYS-SW                                
041800           SET ORCL-KDSVAR-FEL  TO TRUE                                   
041900        ELSE                                                              
042000           MOVE ORCL-IDDISTR    TO WS-IDDISTR                             
042100        END-IF                                                            
042200     END-IF                                                               
042300                                                                          
042400     IF KEYS-OK                                                           
042500        INSPECT ORCL-IDKUNDNR REPLACING LEADING SPACE BY ZERO             
042600        IF ORCL-IDKUNDNR       NOT NUMERIC                                
042700           MOVE '024'           TO ORCL-IDMSG-ERROR                       
042800           MOVE 'IDKUNDNR'      TO ORCL-IDELMT-ERROR                      
042900           MOVE 'CUST NO MUST BE NUMERIC'                                 
043000                                TO ORCL-FEL-TEXT                          
043100           MOVE NOO             TO KEYS-SW                                
043200           SET ORCL-KDSVAR-FEL  TO TRUE                                   
043300        ELSE                                                              
043400           MOVE ORCL-IDKUNDNR   TO WS-IDKUNDNR                            
043500        END-IF                                                            
043600     END-IF                                                               
043700                                                                          
043800     IF KEYS-OK                                                           
043900        INSPECT ORCL-IDORDNR7 REPLACING LEADING SPACE BY ZERO             
044000        IF ORCL-IDORDNR7       NOT NUMERIC                                
044100           MOVE '024'           TO ORCL-IDMSG-ERROR                       
044200           MOVE 'IDORDR7'       TO ORCL-IDELMT-ERROR                      
044300           MOVE 'ORDER NO NOT NUMERIC'                                    
044400                                TO ORCL-FEL-TEXT                          
044500           MOVE NOO             TO KEYS-SW                                
044600           SET ORCL-KDSVAR-FEL  TO TRUE                                   
044900        END-IF                                                            
045000     END-IF                                                               
045100                                                                          
045200     IF KEYS-OK                                                           
045300        INSPECT ORCL-TIREGDAT REPLACING LEADING SPACE BY ZERO             
045400        IF ORCL-TIREGDAT       NOT NUMERIC                                
045500           MOVE '024'           TO ORCL-IDMSG-ERROR                       
045600           MOVE 'TIREGDAT'      TO ORCL-IDELMT-ERROR                      
045700           MOVE 'DATE NOT NUMERIC'                                        
045800                                TO ORCL-FEL-TEXT                          
045900           MOVE NOO             TO KEYS-SW                                
046000           SET ORCL-KDSVAR-FEL  TO TRUE                                   
046100        ELSE                                                              
046200           MOVE 'AAMMDD'           TO DAT-KDDATFORM                       
046300           MOVE ORCL-TIREGDAT      TO DAT-I-TIDATUM                       
046400                                                                          
046500           CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                
046600                               DAT-O-TIDATUM DAT-KDSVAR                   
046700                                                                          
046800           IF DAT-KDSVAR-OK                                               
046900              MOVE ORCL-TIREGDAT   TO WS-TIREGDAT                         
047000           ELSE                                                           
047100              MOVE '023'           TO ORCL-IDMSG-ERROR                    
047200              MOVE 'REGDAT'        TO ORCL-IDELMT-ERROR                   
047300              MOVE 'DATE INVALID'                                         
047400                                TO ORCL-FEL-TEXT                          
047500              MOVE NOO             TO KEYS-SW                             
047600              SET ORCL-KDSVAR-FEL  TO TRUE                                
047700           END-IF                                                         
047800        END-IF                                                            
047900     END-IF                                                               
048000*                                                                         
048100***  VALIDATE MAX NUMBER OF ORDER LINES                                   
048200*                                                                         
048300     IF ORCL-KVRADER  > 999                                               
048400        MOVE NOO              TO OK-SW                                    
048500        MOVE '028'            TO ORCL-IDMSG-ERROR                         
048600        MOVE 'KVRADER '       TO ORCL-IDELMT-ERROR                        
048700        MOVE 'TOO MANY LINES' TO ORCL-FEL-TEXT                            
048800        SET ORCL-KDSVAR-FEL   TO TRUE                                     
048900     END-IF                                                               
049000     .                                                                    
049100     EJECT                                                                
049200 C-VALIDATE-ORDERLINE-CANCEL SECTION.                                     
049300                                                                          
049400     MOVE 'C-VALIDATE-ORDERLINE-CANCEL'                                   
049500                                      TO WS-CURRENT-SECTION               
049600                                                                          
049700     MOVE NOO                       TO ORDER-HEAD-SW                      
049800                                       ORDER-LINE-SW                      
049900     MOVE ZERO                      TO WS-KVRADER-IX-NUM                  
050000     MOVE SPACES                    TO WS-KVRADER-IX-DISP                 
050100                                                                          
050200     IF EVERYTHING-OK                                                     
050300        IF ORCL-KVRADER >= 1                                              
050400           MOVE  1                  TO KVRADER-IX                         
050500           MOVE  ORCL-KVRADER       TO KVRADER-IX-MAX                     
050600           SET DEL-ORDER-LINE       TO TRUE                               
050700                                                                          
050800           PERFORM UNTIL KVRADER-IX  > KVRADER-IX-MAX                     
050900                      OR SOMETHING-WRONG                                  
051000                                                                          
051100             MOVE KVRADER-IX        TO WS-KVRADER-IX-NUM                  
051200             MOVE WS-KVRADER-IX-NUM TO WS-KVRADER-IX-DISP                 
051300             PERFORM S01-CHK-COMM-LINE-INP                                
051400                                                                          
051500             IF EVERYTHING-OK                                             
051600                PERFORM CA-CHK-CNCL-LINE-INP                              
051700             END-IF                                                       
051800                                                                          
051900             IF EVERYTHING-OK                                             
052000                PERFORM CB-VALIDATE-IN-DATABASE                           
052100             END-IF                                                       
052200                                                                          
052300             ADD 1                  TO KVRADER-IX                         
052400           END-PERFORM                                                    
052500        ELSE                                                              
052600           SET DEL-COMP-ORDER       TO TRUE                               
052700           PERFORM CB-VALIDATE-IN-DATABASE                                
052800        END-IF                                                            
052900     END-IF                                                               
053000     .                                                                    
053100     EJECT                                                                
053200 CA-CHK-CNCL-LINE-INP SECTION.                                            
053300                                                                          
053400     MOVE 'CA-CHK-CNCL-LINE-INP'    TO WS-CURRENT-SECTION                 
053500                                                                          
053600     IF ORCL-IDDC   (KVRADER-IX) = SPACE                                  
053700        MOVE NOO                TO OK-SW                                  
053800        MOVE '025'              TO ORCL-IDMSG-ERROR                       
053900        MOVE 'IDDC'             TO ORCL-IDELMT-ERROR                      
054000        STRING 'DELIVERING DC MISSING ON LINE '                           
054100                                   DELIMITED BY SIZE                      
054200        WS-KVRADER-IX-DISP         DELIMITED BY SPACES                    
054300                              INTO ORCL-FEL-TEXT                          
054400        SET ORCL-KDSVAR-FEL     TO TRUE                                   
054500     END-IF                                                               
054600     .                                                                    
054700     EJECT                                                                
054800 CB-VALIDATE-IN-DATABASE   SECTION.                                       
054900                                                                          
055000     MOVE 'CB-VALIDATE-IN-DATABASE' TO WS-CURRENT-SECTION                 
055200                                                                          
055300     IF IDSYSTEM-LYNK   AND                                               
055400        DEL-ORDER-LINE AND                                                
055500        ORCL-KVRADER > 0                                                  
055600        MOVE ORCL-IDLEVART (KVRADER-IX)                                   
055700                                     TO W-SEQB-IDLEVART                   
055800        PERFORM IMS-GU-WDF501-BSEQ                                        
055900        IF SEGMENT-MISSING                                                
056100           MOVE NOO                  TO OK-SW                             
056200           MOVE '025'                TO ORCL-IDMSG-ERROR                  
056300           MOVE 'IDLEVART'           TO ORCL-IDELMT-ERROR                 
056400           STRING 'PARTNUMER ' DELIMITED BY SIZE                          
056500           W-SEQB-IDLEVART     DELIMITED BY SPACES                        
056600           ' ON LINE '         DELIMITED BY SIZE                          
056700           WS-KVRADER-IX-DISP  DELIMITED BY SPACES                        
056800           ' NOT FOUND'        DELIMITED BY SIZE                          
056900                                     INTO ORCL-FEL-TEXT                   
057000           SET ORCL-KDSVAR-FEL  TO TRUE                                   
057100        ELSE                                                              
057200           MOVE XART-IDARTNR         TO W-IDARTNR                         
057400        END-IF                                                            
057500     END-IF                                                               
057600*                                                                         
057700     IF EVERYTHING-OK                                                     
057800        MOVE ORCL-IDDISTR        TO W-IDDISTR-CSEQ                        
057900        MOVE ORCL-IDKUNDNR       TO W-IDKUNDNR-CSEQ                       
058000        MOVE ORCL-IDORDNR7       TO W-IDORDNR7-CSEQ                       
058100                                                                          
058500        PERFORM IMS-GU-WDQ201-CSEQ                                        
058600        IF SEGMENT-FOUND                                                  
059230           IF OHUV-TIREGDAT NOT = ORCL-TIREGDAT OR                        
059300              OHUV-IDSYSTEM (1:3) NOT = WS-IDSYSTEM (1:3)                 
059600               MOVE NOO            TO OK-SW                               
059700               MOVE '025'          TO ORCL-IDMSG-ERROR                    
059800               MOVE 'ORDER'        TO ORCL-IDELMT-ERROR                   
059900               MOVE 'ORDER NOT FOUND'                                     
060000                                   TO ORCL-FEL-TEXT                       
060100             SET ORCL-KDSVAR-FEL  TO TRUE                                 
060200           ELSE                                                           
060400             IF OHUV-FLBORT = YES OR JAA                                  
060600                MOVE NOO              TO OK-SW                            
060700                MOVE '040'            TO ORCL-IDMSG-ERROR                 
060800                MOVE 'ORDER'          TO ORCL-IDELMT-ERROR                
060900                MOVE 'ORDER ALREADY DELETED'                              
061000                                      TO ORCL-FEL-TEXT                    
061100                SET ORCL-KDSVAR-FEL  TO TRUE                              
061200             ELSE                                                         
061400                IF OHUV-KDTPOTYP > 0                                      
061600                  MOVE NOO            TO OK-SW                            
061700                  MOVE '009'          TO ORCL-IDMSG-ERROR                 
061800                  MOVE 'ORDER'        TO ORCL-IDELMT-ERROR                
061900                  MOVE 'ORDER DELETE NOT ALLOWED'                         
062000                                      TO ORCL-FEL-TEXT                    
062100                  SET ORCL-KDSVAR-FEL TO TRUE                             
062200                ELSE                                                      
062400                  MOVE OHUV-IDORDER   TO W-Q3-IDORDER-MIN                 
062500                                         W-Q3-IDORDER-MAX                 
062600                                         W-IDORDER                        
062900                END-IF                                                    
063000             END-IF                                                       
063100           END-IF                                                         
063200        ELSE                                                              
063400           MOVE NOO              TO OK-SW                                 
063500           MOVE '025'            TO ORCL-IDMSG-ERROR                      
063600           MOVE 'ORDER'          TO ORCL-IDELMT-ERROR                     
063700           MOVE 'ORDER NOT FOUND'                                         
063800                                 TO ORCL-FEL-TEXT                         
063900           SET ORCL-KDSVAR-FEL   TO TRUE                                  
064000        END-IF                                                            
064100     END-IF                                                               
064200                                                                          
064300     IF EVERYTHING-OK                                                     
064400        PERFORM CBA-CHK-ORDER-PRINTED                                     
064600        IF ORDER-IS-PRINTED                                               
064800           MOVE NOO                    TO OK-SW                           
064900           MOVE '127'                  TO ORCL-IDMSG-ERROR                
065000           MOVE 'ORDER'                TO ORCL-IDELMT-ERROR               
065100           MOVE 'ORDER PRINTED DELETE NOT ALLOWED'                        
065200                                       TO ORCL-FEL-TEXT                   
065300           SET ORCL-KDSVAR-FEL         TO TRUE                            
065400        END-IF                                                            
065500     END-IF                                                               
065610                                                                          
065700     IF EVERYTHING-OK   AND                                               
065800        DEL-ORDER-LINE  AND                                               
065900        ORCL-KVRADER > 0                                                  
065910*       BO-NOT-FOUND                                                      
066002           MOVE ORCL-IDDC(KVRADER-IX)   TO W-IDDC-B6                      
066003           PERFORM IMS-GU-WDB601                                          
066005           IF DCS-DDC                                                     
066008             PERFORM CBC-CHK-ORD-LINE-E4                                  
066009           ELSE                                                           
066012             IF BO-FOUND                                                  
066014               PERFORM CBD-CHK-ORD-LINE-A5                                
066015             ELSE                                                         
066017               PERFORM CBB-CHK-ORD-LINE-Q4                                
066018             END-IF                                                       
066019           END-IF                                                         
066020                                                                          
066200        IF ORDER-IS-PRINTED                                               
066400           MOVE NOO                    TO OK-SW                           
066500           MOVE '127'                  TO ORCL-IDMSG-ERROR                
066600           MOVE 'ORDER'                TO ORCL-IDELMT-ERROR               
066700           MOVE 'ORDER PRINTED DELETE NOT ALLOWED'                        
066800                                       TO ORCL-FEL-TEXT                   
066900           SET ORCL-KDSVAR-FEL         TO TRUE                            
067000        END-IF                                                            
067100     END-IF                                                               
067200     .                                                                    
067300     EJECT                                                                
067400 CBA-CHK-ORDER-PRINTED SECTION.                                           
067500     MOVE 'CBA-CHK-ORDER-PRINTED'      TO WS-CURRENT-SECTION              
067600                                                                          
067700     MOVE NOO              TO PRINTED-SW                                  
067800                                                                          
067900     PERFORM IMS-GU-WDQ301-STATUS-U-P                                     
068000     IF SEGMENT-FOUND                                                     
068100        PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-FINAL OR                 
068200                      ORDER-IS-PRINTED                                    
068400          IF ODEL-KDODELSTA = 'U' OR 'P'                                  
068510              IF ODEL-KDODELSTA = 'U'                                     
068600                IF ODEL-IDDC NOT = W-IDDC-B6                              
068700                   MOVE ODEL-IDDC TO W-IDDC-B6                            
068800                   PERFORM IMS-GU-WDB601                                  
068900                END-IF                                                    
069100                IF DCS-DDC                                                
069200                   MOVE 'Y' TO DDGS-CHECK-SW                              
069300                   MOVE ODEL-IDDISTR       TO WS-401-IDDISTR-DDGS         
069400                   MOVE ODEL-IDKUNDNR      TO WS-401-IDKUNDNR-DDGS        
069500                   MOVE ODEL-IDKUNDRF(3:5) TO WS-401-IDORDNR-DDGS         
069600                   MOVE ODEL-IDPRODNR      TO WS-401-IDPRODNR-DDGS        
069610                   MOVE ODEL-IDPLKLST      TO WS-401-IDPLKLST-DDGS        
069700                ELSE                                                      
069701                    MOVE YES TO PRINTED-SW                                
069702                END-IF                                                    
069703              ELSE                                                        
069707                    IF ODEL-IDDC NOT = W-IDDC-B6                          
069708                       MOVE ODEL-IDDC TO W-IDDC-B6                        
069709                       PERFORM IMS-GU-WDB601                              
069710                    END-IF                                                
069711                    IF DCS-DDC                                            
069712*AFTER CANCEL DDGS IS P                                                   
069714                      MOVE ODEL-IDDISTR    TO WS-401-IDDISTR-DDGS         
069715                      MOVE ODEL-IDKUNDNR   TO WS-401-IDKUNDNR-DDGS        
069716                      MOVE ODEL-IDKUNDRF(3:5)                             
069717                                           TO WS-401-IDORDNR-DDGS         
069718                      MOVE ODEL-IDPRODNR   TO WS-401-IDPRODNR-DDGS        
069719                      MOVE ODEL-IDPLKLST   TO WS-401-IDPLKLST-DDGS        
069721                    ELSE                                                  
069722*CHECK IF PRESENT IN WDA5 OR WDE4                                         
069723                      PERFORM CBAA-CHK-ORD-E4A5                           
069724                    END-IF                                                
069727              END-IF                                                      
069730                                                                          
069800          ELSE                                                            
069801              IF ODEL-KDODELSTA NOT = 'R'                                 
069810                 MOVE YES TO PRINTED-SW                                   
070300              END-IF                                                      
070310          END-IF                                                          
070400          PERFORM IMS-GN-WDQ301-STATUS-U-P                                
070500        END-PERFORM                                                       
116253     END-IF                                                               
116254     .                                                                    
116255     EJECT                                                                
116256                                                                          
116257 CBAA-CHK-ORD-E4A5   SECTION.                                             
116258     MOVE 'CBAA-CHK-ORD-E4/A5'    TO WS-CURRENT-SECTION                   
116259     MOVE ODEL-IDDISTR            TO W-401-IDDISTR                        
116260     MOVE ODEL-IDKUNDNR           TO W-401-IDKUNDNR                       
116261     MOVE ODEL-IDKUNDRF(3:5)      TO W-401-IDORDNR                        
116262     MOVE ODEL-IDPRODNR           TO W-401-IDPRODNR                       
116263     MOVE ODEL-IDPLKLST           TO W-401-IDPLKLST                       
116264                                                                          
116265     PERFORM IMS-GU-WDE401                                                
116266     IF SEGMENT-FOUND                                                     
116267       MOVE YES TO PRINTED-SW                                             
116268     ELSE                                                                 
116269       PERFORM CBAAB-CHK-BO                                               
116270     END-IF                                                               
116271     .                                                                    
116272     EJECT                                                                
116273                                                                          
116274 CBAAB-CHK-BO SECTION.                                                    
116275     MOVE 'CBAAB-CHK-BO'                TO WS-CURRENT-SECTION             
116276                                                                          
116277     MOVE NOO                  TO BO-CHECK-SW                             
116278                                                                          
116279     PERFORM S02-READ-BO-WDA5                                             
116280*                                                                         
116281     MOVE ZERO               TO WS-KDSTARAD                               
116282                                                                          
116283*IF ORDER NOT FOUND IN WDE4/WDA5 THAT MEANS IT IS DELETED                 
116284     IF BO-FOUND                                                          
116285        CONTINUE                                                          
116286     ELSE                                                                 
116287        MOVE NOO              TO OK-SW                                    
116288        MOVE '040'            TO ORCL-IDMSG-ERROR                         
116289        MOVE 'ORDER'          TO ORCL-IDELMT-ERROR                        
116290        MOVE 'ORDER ALREADY CANCELLED'                                    
116291                              TO ORCL-FEL-TEXT                            
116292        SET ORCL-KDSVAR-FEL  TO TRUE                                      
116294     END-IF                                                               
116295     .                                                                    
116296     EJECT                                                                
116297 CBB-CHK-ORD-LINE-Q4   SECTION.                                           
116298     MOVE 'CBB-CHK-ORD-LINE-Q4'    TO WS-CURRENT-SECTION                  
116299     MOVE ORCL-IDDISTR           TO WS-IDDISTR                            
116300     MOVE ORCL-IDKUNDNR          TO WS-IDKUNDNR                           
116301     MOVE ORCL-IDORDNR7          TO WS-IDORDNR                            
116302     MOVE LOW-VALUE  TO W-WDQ401KY-MIN-X                                  
116303     MOVE HIGH-VALUE TO W-WDQ401KY-MAX-X                                  
116304                                                                          
116305     MOVE W-IDORDER  TO W-Q4-IDORDER-MIN                                  
116306                        W-Q4-IDORDER-MAX                                  
116307                                                                          
116308*    *If wdq4 orderline exists they shall be deleted.                     
116309*    *These lines are not printed.                                        
116310     PERFORM IMS-GU-WDQ401                                                
116311     IF SEGMENT-FOUND                                                     
116312       PERFORM UNTIL SEGMENT-MISSING  OR SEGMENT-FINAL OR                 
116313                     VALID-ORD-LINE                                       
116314         IF ORAD-IDARTNR   = W-IDARTNR AND                                
116315            ORAD-IDDC      = ORCL-IDDC    (KVRADER-IX) AND                
116316            ORAD-KVBEART-Q = ORCL-KVBEART (KVRADER-IX)                    
116317            SET VALID-ORD-LINE  TO TRUE                                   
116318         END-IF                                                           
116319         PERFORM IMS-GN-WDQ401                                            
116320       END-PERFORM                                                        
116321     END-IF                                                               
116322     IF SW-VALID-LINE = NOO                                               
116323        MOVE NOO                  TO OK-SW                                
116324        MOVE '025'                TO ORCL-IDMSG-ERROR                     
116325        MOVE 'IDLEVART'           TO ORCL-IDELMT-ERROR                    
116326        STRING 'ORDERLINE '          DELIMITED BY SIZE                    
116327        ' INVALID '                  DELIMITED BY SIZE                    
116328        'ON LINE '                   DELIMITED BY SIZE                    
116329        WS-KVRADER-IX-DISP           DELIMITED BY SPACES                  
116330                                INTO ORCL-FEL-TEXT                        
116331        SET ORCL-KDSVAR-FEL  TO TRUE                                      
116332     END-IF                                                               
116333     .                                                                    
116334     EJECT                                                                
116335 CBC-CHK-ORD-LINE-E4   SECTION.                                           
116336          MOVE 'CBC-CHK-ORD-LINE-E4'     TO WS-CURRENT-SECTION            
116337          MOVE WS-401-IDDISTR-DDGS       TO W-401-IDDISTR                 
116338          MOVE WS-401-IDKUNDNR-DDGS      TO W-401-IDKUNDNR                
116339          MOVE WS-401-IDORDNR-DDGS       TO W-401-IDORDNR                 
116340          MOVE WS-401-IDPRODNR-DDGS      TO W-401-IDPRODNR                
116341          MOVE WS-401-IDPLKLST-DDGS      TO W-401-IDPLKLST                
116342          PERFORM IMS-GU-WDE401                                           
116343          IF SEGMENT-FOUND                                                
116344            PERFORM IMS-GNP-WDE411                                        
116345            PERFORM UNTIL SEGMENT-MISSING  OR SEGMENT-FINAL OR            
116346                           VALID-ORD-LINE                                 
116347              IF E4-ORAD-IDARTNR = W-IDARTNR AND                          
116348                 KORD-IDDC       = ORCL-IDDC    (KVRADER-IX) AND          
116349                 E4-ORAD-KVBEART = ORCL-KVBEART (KVRADER-IX)              
116350                 SET VALID-ORD-LINE  TO TRUE                              
116351              END-IF                                                      
116352              PERFORM IMS-GNP-WDE411                                      
116353            END-PERFORM                                                   
116354          IF SW-VALID-LINE = NOO                                          
116355             MOVE NOO                  TO OK-SW                           
116356             MOVE '025'                TO ORCL-IDMSG-ERROR                
116357             MOVE 'IDLEVART'           TO ORCL-IDELMT-ERROR               
116358             STRING 'ORDERLINE '          DELIMITED BY SIZE               
116359             ' INVALID '                  DELIMITED BY SIZE               
116360             'ON LINE '                   DELIMITED BY SIZE               
116361             WS-KVRADER-IX-DISP           DELIMITED BY SPACES             
116362                                      INTO ORCL-FEL-TEXT                  
116363             SET ORCL-KDSVAR-FEL       TO TRUE                            
116364          END-IF                                                          
116365*if ddgs is cancelled we dont call 4254                                   
116366          IF E4-ORAD-KDANNULL > 0                                         
116367             MOVE NOO              TO OK-SW                               
116368             MOVE '040'            TO ORCL-IDMSG-ERROR                    
116369             MOVE 'ORDER'          TO ORCL-IDELMT-ERROR                   
116370             MOVE 'ORDER ALREADY CANCELLED'                               
116371                                   TO ORCL-FEL-TEXT                       
116372             SET ORCL-KDSVAR-FEL   TO TRUE                                
116373             MOVE 'N'              TO WS-CALL-4254                        
116374          END-IF                                                          
116375       END-IF                                                             
116376     .                                                                    
116377     EJECT                                                                
116378 CBD-CHK-ORD-LINE-A5   SECTION.                                           
116379     MOVE 'CBD-CHK-ORD-LINE-A5'    TO WS-CURRENT-SECTION                  
116380     MOVE ODEL-IDDISTR         TO W-IDDISTR-MIN                           
116381                                  W-IDDISTR-MAX                           
116382     MOVE ODEL-IDKUNDNR        TO W-IDKUNDNR-MIN                          
116383                                  W-IDKUNDNR-MAX                          
116384     MOVE OHUV-IDORDNR7(3:5)   TO W-IDORDNR5-MIN                          
116385                                  W-IDORDNR5-MAX                          
116386     MOVE LOW-VALUES           TO W-IDDC-MIN                              
116387     MOVE HIGH-VALUES          TO W-IDDC-MAX                              
116388     MOVE ALL ZERO             TO W-IDARTNR-MIN                           
116389                                  W-KVART-MIN                             
116390     MOVE 999999999            TO W-IDARTNR-MAX                           
116391     MOVE 9999999              TO W-KVART-MAX                             
116392                                                                          
116393     IF DEL-ORDER-LINE                                                    
116394        MOVE W-IDARTNR         TO W-IDARTNR-MIN                           
116395                                  W-IDARTNR-MAX                           
116396        MOVE ORCL-KVBEART (KVRADER-IX)                                    
116397                               TO W-KVART-MIN                             
116398                                  W-KVART-MAX                             
116399*WE ARE SKIPPING IDDC BECAUSE CODE90 CAN BE ON NOTDC11 AND WILL           
116400*NOT FIND IN WDA5 BECASUSE OF THIS                                        
116401*       MOVE ORCL-IDDC (KVRADER-IX)                                       
116402*                              TO W-IDDC-MIN                              
116403*                                 W-IDDC-MAX                              
116404        MOVE ZERO              TO WS-KDSTARAD                             
116405                                                                          
116406        PERFORM IMS-GU-WDA501                                             
116407        PERFORM UNTIL SEGMENT-MISSING  OR SEGMENT-FINAL OR                
116408                      VALID-ORD-LINE                                      
116409          IF RAD-IDARTNR    = W-IDARTNR AND                               
116410             RAD-KVART      = ORCL-KVBEART (KVRADER-IX)                   
116411              SET VALID-ORD-LINE  TO TRUE                                 
116412          END-IF                                                          
116413        PERFORM IMS-GN-WDA501                                             
116414        END-PERFORM                                                       
116415        IF SW-VALID-LINE = NOO                                            
116416            MOVE NOO                TO OK-SW                              
116417            MOVE '025'              TO ORCL-IDMSG-ERROR                   
116418            MOVE 'IDLEVART'         TO ORCL-IDELMT-ERROR                  
116419            STRING 'ORDERLINE '        DELIMITED BY SIZE                  
116420            ' INVALID '                DELIMITED BY SIZE                  
116421            'ON LINE '                 DELIMITED BY SIZE                  
116422            WS-KVRADER-IX-DISP         DELIMITED BY SPACES                
116423                                   INTO ORCL-FEL-TEXT                     
116424           SET ORCL-KDSVAR-FEL TO TRUE                                    
116425        END-IF                                                            
116426        MOVE RAD-KDSTARAD          TO WS-KDSTARAD                         
116427        IF WS-KDSTARAD <= 3                                               
116428           SET BO-DELETE         TO TRUE                                  
116429        END-IF                                                            
116430       .                                                                  
116431       EJECT                                                              
116432 D-DELETE-ORDERLINES   SECTION.                                           
116433     MOVE 'D-DELETE-ORDERLINES'    TO WS-CURRENT-SECTION                  
116434                                                                          
116438     IF DEL-ORDER-LINE                                                    
116439        MOVE ORCL-IDDISTR           TO WS-IDDISTR                         
116440        MOVE ORCL-IDKUNDNR          TO WS-IDKUNDNR                        
116441        MOVE ORCL-IDORDNR7          TO WS-IDORDNR                         
116442        IF DEL-ORDER-LINE                                                 
116443           MOVE  1                  TO KVRADER-IX                         
116444           MOVE  ORCL-KVRADER       TO KVRADER-IX-MAX                     
116445                                                                          
116446           PERFORM UNTIL KVRADER-IX  > KVRADER-IX-MAX                     
116447             PERFORM S03-GET-PART-NUM                                     
116448             MOVE W-IDARTNR         TO WS-IDARTNR                         
116449             MOVE ORCL-IDDC (KVRADER-IX)                                  
116450                                    TO WS-IDDC                            
116451             MOVE ORCL-KVBEART (KVRADER-IX)                               
116452                                    TO WS-KVBEART                         
116453             IF BO-DELETE                                                 
116454                 PERFORM DC-START-4256-DISPATCHER                         
116455             ELSE                                                         
116456                 IF WS-CALL-4254 ='Y'                                     
116457                    PERFORM DC-START-4254-DISPATCHER                      
116458                 END-IF                                                   
116459             END-IF                                                       
116460             ADD 1                  TO KVRADER-IX                         
116461           END-PERFORM                                                    
116462        END-IF                                                            
116463     ELSE                                                                 
116465        PERFORM DB-DELETE-OTH-ORDERLINES                                  
116466        IF BO-FOUND                                                       
116467           PERFORM DA-DELETE-BO-ORDERLINES                                
116468        END-IF                                                            
116469     END-IF                                                               
116470                                                                          
116481     .                                                                    
116482     EJECT                                                                
116483 DA-DELETE-BO-ORDERLINES   SECTION.                                       
116484     MOVE 'DA-DELETE-BO-ORDERLINES'    TO WS-CURRENT-SECTION              
116485                                                                          
116486     MOVE ORCL-IDDISTR           TO WS-IDDISTR                            
116487     MOVE ORCL-IDKUNDNR          TO WS-IDKUNDNR                           
116488     MOVE ORCL-IDORDNR7          TO WS-IDORDNR                            
116506     PERFORM DAA-READ-WDA5-ORD-LINES                                      
116508     .                                                                    
116509     EJECT                                                                
116510 DAA-READ-WDA5-ORD-LINES  SECTION.                                        
116511     MOVE 'DA-DELETE-OTH-ORDERLINES'   TO WS-CURRENT-SECTION              
116512                                                                          
116513     MOVE ORCL-IDDISTR         TO W-IDDISTR-MIN                           
116514                                  W-IDDISTR-MAX                           
116515     MOVE ORCL-IDKUNDNR        TO W-IDKUNDNR-MIN                          
116516                                  W-IDKUNDNR-MAX                          
116517     MOVE ORCL-IDORDNR7(3:5)   TO W-IDORDNR5-MIN                          
116518                                  W-IDORDNR5-MAX                          
116519     MOVE LOW-VALUES           TO W-IDDC-MIN                              
116520     MOVE HIGH-VALUES          TO W-IDDC-MAX                              
116521     MOVE ALL ZERO             TO W-IDARTNR-MIN                           
116522                                  W-KVART-MIN                             
116523     MOVE 999999999            TO W-IDARTNR-MAX                           
116524     MOVE 9999999              TO W-KVART-MAX                             
116525                                                                          
116526     PERFORM IMS-GU-WDA501                                                
116527     PERFORM UNTIL SEGMENT-MISSING                                        
116528       MOVE RAD-IDDISTR           TO WS-IDDISTR                           
116529       MOVE RAD-IDKUNDNR          TO WS-IDKUNDNR                          
116530       MOVE RAD-IDORDNR5          TO WS-IDORDNR(3:5)                      
116531       MOVE RAD-IDARTNR           TO WS-IDARTNR                           
116532       MOVE RAD-IDDC              TO WS-IDDC                              
116533       MOVE RAD-KVART             TO WS-KVBEART                           
116535       PERFORM DC-START-4256-DISPATCHER                                   
116536       PERFORM IMS-GN-WDA501                                              
116537     END-PERFORM                                                          
116538     .                                                                    
116539     EJECT                                                                
116540 DB-DELETE-OTH-ORDERLINES   SECTION.                                      
116541     MOVE 'DB-DELETE-OTH-ORDERLINES'   TO WS-CURRENT-SECTION              
116543                                                                          
116544     MOVE ORCL-IDDISTR           TO WS-IDDISTR                            
116545     MOVE ORCL-IDKUNDNR          TO WS-IDKUNDNR                           
116546     MOVE ORCL-IDORDNR7          TO WS-IDORDNR                            
116547     MOVE LOW-VALUE  TO W-WDQ401KY-MIN-X                                  
116548     MOVE HIGH-VALUE TO W-WDQ401KY-MAX-X                                  
116549                                                                          
116550     MOVE W-IDORDER  TO W-Q4-IDORDER-MIN                                  
116551                        W-Q4-IDORDER-MAX                                  
116552                                                                          
116555*    *If wdq4 orderline exists they shall be deleted.                     
116556*    *These lines are not printed.                                        
116557     PERFORM IMS-GU-WDQ401                                                
116558     IF SEGMENT-FOUND                                                     
116560       PERFORM UNTIL SEGMENT-MISSING  OR SEGMENT-FINAL                    
116561         MOVE ORAD-IDARTNR      TO WS-IDARTNR                             
116562         MOVE ORAD-KVBEART-Q    TO WS-KVBEART                             
116563         MOVE ORAD-IDDC         TO WS-IDDC                                
116566         PERFORM DC-START-4254-DISPATCHER                                 
116567         PERFORM IMS-GN-WDQ401                                            
116568       END-PERFORM                                                        
116569     END-IF                                                               
116570     IF DDGS-FOUND                                                        
116580         MOVE WS-401-IDDISTR-DDGS       TO W-401-IDDISTR                  
116581         MOVE WS-401-IDKUNDNR-DDGS      TO W-401-IDKUNDNR                 
116582         MOVE WS-401-IDORDNR-DDGS       TO W-401-IDORDNR                  
116583         MOVE WS-401-IDPRODNR-DDGS      TO W-401-IDPRODNR                 
116584         MOVE WS-401-IDPLKLST-DDGS      TO W-401-IDPLKLST                 
116585         PERFORM IMS-GU-WDE401                                            
116587         IF SEGMENT-FOUND                                                 
116588            MOVE KORD-IDDISTR           TO WS-IDDISTR                     
116589            MOVE KORD-IDKUNDNR          TO WS-IDKUNDNR                    
116590            MOVE ZEROS                  TO WS-IDORDNR                     
116591            MOVE KORD-IDORDNR5          TO WS-IDORDNR(3:5)                
116592            MOVE KORD-IDDC              TO WS-IDDC                        
116593            PERFORM IMS-GNP-WDE411                                        
116595            PERFORM UNTIL SEGMENT-MISSING                                 
116596             MOVE E4-ORAD-IDARTNR       TO WS-IDARTNR                     
116597             MOVE E4-ORAD-KVBEART       TO WS-KVBEART                     
116605*IF ddgs order not cancelled then we call 4254                            
116607             IF E4-ORAD-KDANNULL = 0                                      
116617               PERFORM DC-START-4254-DISPATCHER                           
116618             END-IF                                                       
116619             PERFORM IMS-GNP-WDE411                                       
116620           END-PERFORM                                                    
116621         END-IF                                                           
116622     END-IF                                                               
116623                                                                          
116624     .                                                                    
116625     EJECT                                                                
116626 DC-START-4254-DISPATCHER     SECTION.                                    
116627     MOVE 'DC-START-4254-D ' TO WS-CURRENT-SECTION                        
116628                                                                          
116629*    -- INITIALIZE MID DATA TO W40251                                     
116630     MOVE SPACE               TO 4254-MID-W4I25401-CTX                    
116631     MOVE WS-IDSYSTEM         TO 4254-MID-IDSYSTEM                        
116632     MOVE WS-IDDISTR          TO 4254-MID-IDDISTR                         
116633     MOVE WS-IDKUNDNR         TO 4254-MID-IDKUNDNR                        
116634     MOVE WS-IDORDNR          TO 4254-MID-IDORDNR                         
116635     MOVE YES                 TO 4254-MID-FLSLUT                          
116636     MOVE WS-IDARTNR          TO 4254-MID-IDARTNR                         
116637     MOVE WS-KVBEART          TO 4254-MID-KVBEART                         
116638     MOVE WS-IDDC             TO 4254-MID-IDDC                            
116639                                                                          
116640*    -- MOVE TO MSG-IO-AREA AND ADD TRANSACTION PREFIX                    
116641*    -- OUTSIDE THE MID COPYTEXT                                          
116642     COMPUTE MSG-KVLL = LENGTH OF 4254-MID-W4I25401-CTX + 17              
116643     MOVE 4254-MID-W4I25401-CTX TO MSG-INDATA-MINUS-1-TRANSKOD            
116644     MOVE 'W4T254X '          TO MSG-KDTRANS-1                            
116645     MOVE '4254'              TO MSG-IDTRANS-1                            
116646     MOVE '1'                 TO MSG-KDMFSFOR-1                           
116647                                                                          
116648*    -- INITIALIZE W006KOM FIELDS WITH VARIABLE CONTENT                   
116649*    -- FIXED DATA HAS BEEN SET IN A-INIT                                 
116650     MOVE 'W4I25401'          TO MSG-KOM-IDCPYTXT                         
116661     IF IDSYSTEM-VALID                                                    
116662         MOVE  WS-IDSYSTEM     TO MSG-KOM-IDSNDNOD(1:4)                   
116663         MOVE 'DLET'           TO MSG-KOM-IDSNDNOD(5:4)                   
116664     END-IF                                                               
116666                                                                          
116667     CALL W006KOM USING MSG-PCB                                           
116668                        0693X-PCB                                         
116669                        WDP8-PCB                                          
116670                        MSG-KOM-WMSGKOM                                   
116671                        MSG-IO-AREA                                       
116672                                                                          
116673     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
116674        STRING ' ERROR FROM W006KOM. '  MSG-KOM-IDMFSMED                  
116675          DELIMITED BY SIZE  INTO ERROR-TEXT                              
116676        CALL ABEND USING RKOD-ABEND-NO-DUMP                               
116677     END-IF                                                               
116678     .                                                                    
116679     EJECT                                                                
116680 DC-START-4256-DISPATCHER     SECTION.                                    
116681     MOVE 'DC-START-4256-D ' TO WS-CURRENT-SECTION                        
116682                                                                          
116683*    -- INITIALIZE MID DATA TO W40251                                     
116684     MOVE SPACE               TO 4256-MID-W4I25601-CTX                    
116685     MOVE WS-IDDISTR          TO 4256-MID-IDDISTR                         
116686     MOVE WS-IDKUNDNR         TO 4256-MID-IDKUNDNR                        
116687     MOVE WS-IDORDNR          TO 4256-MID-IDORDNR7                        
116688     MOVE ZERO                TO 4256-MID-TITPO                           
116689     MOVE WS-IDARTNR          TO 4256-MID-IDARTNR                         
116690     MOVE WS-KVBEART          TO 4256-MID-KVBEART                         
116691     MOVE WS-IDDC             TO 4256-MID-IDDC                            
116692                                                                          
116693*    -- MOVE TO MSG-IO-AREA AND ADD TRANSACTION PREFIX                    
116694*    -- OUTSIDE THE MID COPYTEXT                                          
116695     COMPUTE MSG-KVLL = LENGTH OF 4256-MID-W4I25601-CTX + 17              
116696     MOVE 4256-MID-W4I25601-CTX TO MSG-INDATA-MINUS-1-TRANSKOD            
116697     MOVE 'W4T256X '          TO MSG-KDTRANS-1                            
116698     MOVE '4256'              TO MSG-IDTRANS-1                            
116699     MOVE '1'                 TO MSG-KDMFSFOR-1                           
116700                                                                          
116701*    -- INITIALIZE W006KOM FIELDS WITH VARIABLE CONTENT                   
116702*    -- FIXED DATA HAS BEEN SET IN A-INIT                                 
116703     MOVE 'W4I25601'          TO MSG-KOM-IDCPYTXT                         
116714     IF IDSYSTEM-VALID                                                    
116715         MOVE  WS-IDSYSTEM     TO MSG-KOM-IDSNDNOD(1:4)                   
116716         MOVE 'DLET'           TO MSG-KOM-IDSNDNOD(5:4)                   
116717     END-IF                                                               
116719                                                                          
116720     CALL W006KOM USING MSG-PCB                                           
116721                        0693X-PCB                                         
116722                        WDP8-PCB                                          
116723                        MSG-KOM-WMSGKOM                                   
116724                        MSG-IO-AREA                                       
116725                                                                          
116726     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
116727        STRING ' ERROR FROM W006KOM. '  MSG-KOM-IDMFSMED                  
116728          DELIMITED BY SIZE  INTO ERROR-TEXT                              
116730        CALL ABEND USING RKOD-ABEND-NO-DUMP                               
116731     END-IF                                                               
116732     .                                                                    
116733     EJECT                                                                
116812 S01-CHK-COMM-LINE-INP SECTION.                                           
116813                                                                          
116814     MOVE 'S01-CHK-COMM-LINE-INP'     TO WS-CURRENT-SECTION               
116815***                                                                       
116816***  VALIDATE SUPPLIER PART NUMBER -IDLEVART                              
116817***                                                                       
116818     MOVE ZERO                  TO W-BLANKS                               
116819     INSPECT FUNCTION REVERSE (ORCL-IDLEVART (KVRADER-IX))                
116820             TALLYING W-BLANKS FOR LEADING SPACES                         
116821     COMPUTE W-LENGTH = 30 - W-BLANKS                                     
116822     IF ORCL-IDLEVART(KVRADER-IX) (1:W-LENGTH) NUMERIC                    
116823        IF (W-LENGTH < 9 AND W-LENGTH > 0 )                               
116824           MOVE ORCL-IDLEVART  (KVRADER-IX) (1:W-LENGTH)                  
116825                                TO W-IDARTNR                              
116826        END-IF                                                            
116827     ELSE                                                                 
116828        MOVE NOO                  TO OK-SW                                
116829        MOVE '025'                TO ORCL-IDMSG-ERROR                     
116830        MOVE 'IDLEVART'           TO ORCL-IDELMT-ERROR                    
116831        STRING 'PARTNUMER '          DELIMITED BY SIZE                    
116832        ' INVALID '                  DELIMITED BY SIZE                    
116833        'ON LINE '                   DELIMITED BY SIZE                    
116834        WS-KVRADER-IX-DISP           DELIMITED BY SPACES                  
116835                                INTO ORCL-FEL-TEXT                        
116836        SET ORCL-KDSVAR-FEL  TO TRUE                                      
116837     END-IF                                                               
116838***                                                                       
116839***  VALIDATE ORDER QUANTITY                                              
116840***                                                                       
116841     IF ORCL-KVBEART (KVRADER-IX) NOT NUMERIC OR                          
116842        ORCL-KVBEART (KVRADER-IX) = ZERO                                  
116843        MOVE NOO              TO OK-SW                                    
116844        MOVE '330'            TO ORCL-IDMSG-ERROR                         
116845        MOVE 'KVBEART'        TO ORCL-IDELMT-ERROR                        
116846        STRING 'QUANTITY '       DELIMITED BY SIZE                        
116847        'INVALID '               DELIMITED BY SIZE                        
116848        'ON LINE '               DELIMITED BY SIZE                        
116849        WS-KVRADER-IX-DISP       DELIMITED BY SPACES                      
116850                            INTO ORCL-FEL-TEXT                            
116851        SET ORCL-KDSVAR-FEL  TO TRUE                                      
116852     END-IF                                                               
116853     .                                                                    
116854     EJECT                                                                
116855 S02-READ-BO-WDA5 SECTION.                                                
116856     MOVE 'S02-CHK-BO-WDA5'    TO WS-CURRENT-SECTION                      
116858                                                                          
116859     MOVE ODEL-IDDISTR         TO W-IDDISTR-MIN                           
116860                                  W-IDDISTR-MAX                           
116861     MOVE ODEL-IDKUNDNR        TO W-IDKUNDNR-MIN                          
116862                                  W-IDKUNDNR-MAX                          
116863     MOVE OHUV-IDORDNR7(3:5)   TO W-IDORDNR5-MIN                          
116864                                  W-IDORDNR5-MAX                          
116865     MOVE ODEL-IDDC            TO WS-IDDC-WDA5                            
116866     MOVE LOW-VALUES           TO W-IDDC-MIN                              
116867     MOVE HIGH-VALUES          TO W-IDDC-MAX                              
116868     MOVE ALL ZERO             TO W-IDARTNR-MIN                           
116869                                  W-KVART-MIN                             
116870     MOVE 999999999            TO W-IDARTNR-MAX                           
116871     MOVE 9999999              TO W-KVART-MAX                             
116872                                                                          
116918* here we need full order check hence calling wda5                        
116919       MOVE ZERO               TO WS-KDSTARAD                             
116920                                                                          
116921       PERFORM IMS-GU-WDA501                                              
116922       PERFORM UNTIL SEGMENT-MISSING OR BO-FOUND                          
116923          MOVE RAD-KDSTARAD        TO WS-KDSTARAD                         
116926          IF WS-KDSTARAD <= 3                                             
116927             SET BO-FOUND          TO TRUE                                
116928          END-IF                                                          
116929          PERFORM IMS-GN-WDA501                                           
116930       END-PERFORM                                                        
116932     .                                                                    
116933     EJECT                                                                
116940 S03-GET-PART-NUM SECTION.                                                
117000                                                                          
117100     MOVE 'S03-GET-PART-NUM'    TO WS-CURRENT-SECTION                     
117200***                                                                       
117300***  GET VALID PARTNUMBER                                                 
117400***                                                                       
117500     IF IDSYSTEM-LYNK                                                     
117600        MOVE ORCL-IDLEVART (KVRADER-IX)                                   
117700                                     TO W-SEQB-IDLEVART                   
117800        PERFORM IMS-GU-WDF501-BSEQ                                        
117900        IF SEGMENT-FOUND                                                  
118000           MOVE XART-IDARTNR         TO W-IDARTNR                         
118100        END-IF                                                            
118200     ELSE                                                                 
118300       MOVE ZERO                     TO W-BLANKS                          
118400       INSPECT FUNCTION REVERSE (ORCL-IDLEVART (KVRADER-IX))              
118500               TALLYING W-BLANKS FOR LEADING SPACES                       
118600       COMPUTE W-LENGTH = 30 - W-BLANKS                                   
118700       IF ORCL-IDLEVART(KVRADER-IX) (1:W-LENGTH) NUMERIC                  
118800          IF (W-LENGTH < 9 AND W-LENGTH > 0 )                             
118900             MOVE ORCL-IDLEVART  (KVRADER-IX) (1:W-LENGTH)                
119000                                     TO W-IDARTNR                         
119100          END-IF                                                          
119200       END-IF                                                             
119300     END-IF                                                               
119400     .                                                                    
119500     EJECT                                                                
119600                                                                          
119700 Z-FINIT SECTION.                                                         
119800     CONTINUE                                                             
119900     .                                                                    
120000     EJECT                                                                
120100*** ---------------------                                                 
120200*** --- IMS SECTIONS  ---                                                 
120300*** ---------------------                                                 
120400                                                                          
120500     EJECT                                                                
120600 IMS-GU-WDF501-BSEQ SECTION.                                              
120700     MOVE 'IMS-GU-WDF501-BS' TO WS-CURRENT-IMS-SECTION                    
120800                                                                          
120900     MOVE SPACE                 TO ALL-SSA                                
121000     STRING 'WDF501  (WDF5BSEQ =' W-WDF5BSEQ ')'                          
121100            DELIMITED BY SIZE INTO SSA1                                   
121200     MOVE '  GEGB'              TO GOOD-STATUSCODES                       
121300     CALL CBLTDLI USING GU WDF5-PCB DLI-IO-WDF501 SSA1                    
121400     MOVE WDF5-STATUS-CODE      TO STATUS-WS                              
121600     PERFORM IMS-STATUSCHECK                                              
121700     .                                                                    
121800     EJECT                                                                
121900 IMS-GU-WDQ201-CSEQ SECTION.                                              
122000     MOVE 'GU-WDQ2-CSEQ    '  TO WS-CURRENT-IMS-SECTION                   
122200                                                                          
122300     MOVE SPACE               TO ALL-SSA                                  
122400     STRING 'WDQ201  (WDQ2CSEQ =' W-WDQ2CSEQ ')'                          
122500          DELIMITED BY SIZE INTO SSA1                                     
122600     MOVE '  GE'              TO GOOD-STATUSCODES                         
122700     CALL CBLTDLI USING GU WDQ2CSEQ-PCB DLI-IO-WDQ201 SSA1                
122800     MOVE WDQ2CSEQ-STATUS-CODE TO STATUS-WS                               
123000     PERFORM IMS-STATUSCHECK                                              
123100     .                                                                    
123200     EJECT                                                                
123300 IMS-GU-WDQ301-STATUS-U-P SECTION.                                        
123400     MOVE 'IMS-GU-WDQ301-UP' TO WS-CURRENT-IMS-SECTION                    
123600                                                                          
123700     STRING 'WDQ301  (WDQ301KY>=' W-WDQ301KY-MIN                          
123800                    '&WDQ301KY<=' W-WDQ301KY-MAX                          
123900                    '&KDODELST =' W-KDODELST-U                            
124000                    '!WDQ301KY>=' W-WDQ301KY-MIN                          
124100                    '&WDQ301KY<=' W-WDQ301KY-MAX                          
124200                    '&KDODELST =' W-KDODELST-P ')'                        
124300            DELIMITED BY SIZE INTO SSA1                                   
124400     MOVE '  GE' TO GOOD-STATUSCODES                                      
124500     CALL CBLTDLI USING GU   WDQ3-PCB DLI-IO-WDQ301 SSA1                  
124600     MOVE WDQ3-STATUS-CODE TO STATUS-WS                                   
124800     PERFORM IMS-STATUSCHECK                                              
124900     .                                                                    
125000     EJECT                                                                
125100 IMS-GN-WDQ301-STATUS-U-P SECTION.                                        
125200     MOVE 'IMS-GN-WDQ301-UP' TO WS-CURRENT-IMS-SECTION                    
125300                                                                          
125400     STRING 'WDQ301  (WDQ301KY>=' W-WDQ301KY-MIN                          
125500                    '&WDQ301KY<=' W-WDQ301KY-MAX                          
125600                    '&KDODELST =' W-KDODELST-U                            
125700                    '!WDQ301KY>=' W-WDQ301KY-MIN                          
125800                    '&WDQ301KY<=' W-WDQ301KY-MAX                          
125900                    '&KDODELST =' W-KDODELST-P ')'                        
126000            DELIMITED BY SIZE INTO SSA1                                   
126100     MOVE 'GBGE' TO GOOD-STATUSCODES                                      
126200     CALL CBLTDLI USING GN   WDQ3-PCB DLI-IO-WDQ301 SSA1                  
126300     MOVE WDQ3-STATUS-CODE TO STATUS-WS                                   
126500     PERFORM IMS-STATUSCHECK                                              
126600     .                                                                    
126700     EJECT                                                                
126800 IMS-GU-WDA501 SECTION.                                                   
126900                                                                          
127000     MOVE 'IMS-GU-WDA501   '        TO WS-CURRENT-IMS-SECTION             
127100                                                                          
127200     STRING 'WDA501  (WDA501KY>=' W-WDA501KY-A5-MIN-X                     
127300                    '&WDA501KY<=' W-WDA501KY-A5-MAX-X                     
127400                    '&IDDC    >=' W-IDDC-MIN-X                            
127500                    '&IDDC    <=' W-IDDC-MAX-X                            
127600                    '&KVART   >=' W-KVART-MIN-X                           
127700                    '&KVART   <=' W-KVART-MAX-X ')'                       
127800          DELIMITED BY SIZE INTO SSA1                                     
127900     MOVE '  GE'              TO GOOD-STATUSCODES                         
128000     CALL CBLTDLI USING GU WDA5-PCB DLI-IO-WDA501  SSA1                   
128100     MOVE WDA5-STATUS-CODE    TO STATUS-WS                                
128300     PERFORM IMS-STATUSCHECK                                              
128400     .                                                                    
128500     EJECT                                                                
128600 IMS-GN-WDA501 SECTION.                                                   
128700                                                                          
128800     MOVE 'IMS-GU-WDA501   '        TO WS-CURRENT-IMS-SECTION             
128900                                                                          
129000     STRING 'WDA501  (WDA501KY>=' W-WDA501KY-A5-MIN-X                     
129100                    '&WDA501KY<=' W-WDA501KY-A5-MAX-X                     
129200                    '&IDDC    >=' W-IDDC-MIN-X                            
129300                    '&IDDC    <=' W-IDDC-MAX-X                            
129400                    '&KVART   >=' W-KVART-MIN-X                           
129500                    '&KVART   <=' W-KVART-MAX-X ')'                       
129600          DELIMITED BY SIZE INTO SSA1                                     
129700     MOVE '  GE'              TO GOOD-STATUSCODES                         
129800     CALL CBLTDLI USING GN WDA5-PCB DLI-IO-WDA501  SSA1                   
129900     MOVE WDA5-STATUS-CODE    TO STATUS-WS                                
130000     PERFORM IMS-STATUSCHECK                                              
130100     .                                                                    
130200     EJECT                                                                
130300 IMS-GU-WDQ401 SECTION.                                                   
130400     MOVE 'IMS-GU-WDQ401'   TO WS-CURRENT-IMS-SECTION                     
130600                                                                          
130700     STRING 'WDQ401  (WDQ401KY>=' W-WDQ401KY-MIN-X                        
130800                    '&WDQ401KY<=' W-WDQ401KY-MAX-X ')'                    
130900          DELIMITED BY SIZE INTO SSA1                                     
131000     MOVE '  GE' TO GOOD-STATUSCODES                                      
131100     CALL CBLTDLI USING GU WDQ4-PCB DLI-IO-WDQ401 SSA1                    
131200     MOVE WDQ4-STATUS-CODE TO STATUS-WS                                   
131300     PERFORM IMS-STATUSCHECK                                              
131400     .                                                                    
131500     EJECT                                                                
131600 IMS-GN-WDQ401 SECTION.                                                   
131700     MOVE 'IMS-GN-WDQ401   ' TO WS-CURRENT-IMS-SECTION                    
131800                                                                          
131900     MOVE SPACE            TO ALL-SSA                                     
132000                                                                          
132100     STRING 'WDQ401  (WDQ401KY>=' W-WDQ401KY-MIN-X                        
132200                    '&WDQ401KY<=' W-WDQ401KY-MAX-X ')'                    
132300       DELIMITED BY SIZE INTO SSA1                                        
132400     MOVE '  GBGE'         TO GOOD-STATUSCODES                            
132500     CALL CBLTDLI USING GN WDQ4-PCB DLI-IO-WDQ401 SSA1                    
132600     MOVE WDQ4-STATUS-CODE TO STATUS-WS                                   
132700     PERFORM IMS-STATUSCHECK                                              
132800     .                                                                    
132900     EJECT                                                                
133000 IMS-GU-WDB601    SECTION.                                                
133100     MOVE 'IMS-GU-WDB601   ' TO WS-CURRENT-IMS-SECTION                    
133200                                                                          
133300     MOVE SPACE            TO ALL-SSA                                     
133400     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
133500          DELIMITED BY SIZE INTO SSA1                                     
133600     MOVE '  GE' TO GOOD-STATUSCODES                                      
133700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
133800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
133900     PERFORM IMS-STATUSCHECK                                              
134000     IF SEGMENT-MISSING                                                   
134100         MOVE SPACE TO DCS-KDDC                                           
134200     END-IF                                                               
134300     .                                                                    
134400 IMS-GU-WDE401    SECTION.                                                
134500     MOVE 'IMS-GU-WDE401   ' TO WS-CURRENT-IMS-SECTION                    
134600                                                                          
134700     MOVE SPACE            TO ALL-SSA                                     
134800     STRING 'WDE401  (WDE401KY =' W-WDE401-X ')'                          
134900            DELIMITED BY SIZE INTO SSA1                                   
135000     MOVE '  GE' TO GOOD-STATUSCODES                                      
135100     CALL CBLTDLI USING GU  WDE4-PCB KORD-WDE401 SSA1                     
135200     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
135300     PERFORM IMS-STATUSCHECK                                              
135400     SKIP2                                                                
135500     .                                                                    
135600 IMS-GNP-WDE411    SECTION.                                               
135700     MOVE 'IMS-GU-WDE401   ' TO WS-CURRENT-IMS-SECTION                    
135800                                                                          
135900     MOVE SPACE            TO ALL-SSA                                     
136000     MOVE   'WDE411   '         TO SSA1                                   
136100     MOVE '  GE' TO GOOD-STATUSCODES                                      
136200     CALL CBLTDLI USING GNP  WDE4-PCB E4-ORAD-WDE411 SSA1                 
136300     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
136400     PERFORM IMS-STATUSCHECK                                              
136500     SKIP2                                                                
136600     .                                                                    
136700 IMS-STATUSCHECK SECTION.                                                 
136800                                                                          
136900     SET STATUS-IX TO 1                                                   
137000     SEARCH GOOD-STATUS                                                   
137100       AT END                                                             
137200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
137300           DELIMITED BY SIZE INTO ERROR-TEXT                              
137400         DISPLAY ERROR-TEXT                                               
137500         CALL FELLOG                                                      
137600       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
137700         CONTINUE                                                         
137800     END-SEARCH                                                           
137900     .                                                                    
