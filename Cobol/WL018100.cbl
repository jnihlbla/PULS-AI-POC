001400 ID DIVISION.                                                             
001500 PROGRAM-ID.     WL018100.                                                
001600 AUTHOR.         TAPAS KUMAR GHOSH.                                       
001700 DATE-WRITTEN.   2004/11/08.                                              
001800 DATE-COMPILED.                                                           
001900                                                                          
002000*    FUNCTION:                                                            
002100*        SHOWS IDSHIPMENT PER DISTRICT,IDKUNDNR AND DC. ROW CAN BE        
002200*        SELECTED TO JUMP TO SCREEN 4624 BOOKING INFORMATION BILL-        
002300*        IT TO SHOW MORE ABOUT THIS IDSHIPMENT                            
002400*                                                                         
002501*        THE PROGRAM READS     WDE1B                                      
002510*        THE PROGRAM READS     WDE1                                       
002511*                                                                         
002512*        WL018100 PROGRAM IS A REPLICA OF W4062300 PROGRAM                
002513*        AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS                          
002514*                                                                         
002520*    ADDRESS: 'CARPARTS.LDC.SHIPPINGPERDISTRICT'                          
002600*                                                                         
002700*    INDATA.                                                              
002800*        TRANSACTION: WL0181U                                             
002900*        RESQUEST:    WZ01REQU                                            
002910*                     WL0181I1                                            
003000*                                                                         
003020*                                                                         
003100*    OUTDATA.                                                             
003200*        RESPONSE:    WZ01RESP                                            
003210*                     WL0181O1                                            
003300                                                                          
003400     SKIP3                                                                
003500 ENVIRONMENT DIVISION.                                                    
003600                                                                          
003700 DATA DIVISION.                                                           
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000 77  IDPGM                       PIC X(08)   VALUE 'WL018100'.            
004100                                                                          
004200*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004300 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004320 77  KDRC-DISPLAY                PIC Z(5)   VALUE ZERO.                   
004400                                                                          
004500 77  YES                         PIC X       VALUE 'J'.                   
004600 77  NOO                         PIC X       VALUE 'N'.                   
004700                                                                          
004801*    --- INDEX FOR SCROLL LINES                                           
004802 77  WS-COUNT                    PIC S9(4)   VALUE +0   COMP SYNC.        
004804 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004810 77  MAX-INDX                    PIC S9(4)  VALUE +500  COMP SYNC.        
004860                                                                          
004900*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
005100                                                                          
005300                                                                          
005400 77  KEYS-SW                     PIC X       VALUE 'J'.                   
005500     88  KEYS-OK                             VALUE 'J'.                   
005600     88  KEYS-WRONG                          VALUE 'N'.                   
006513                                                                          
006520 77  W-KEY-DIST-KUND             PIC X(1)    VALUE 'N'.                   
006530     88  KEY-DIST-KUND                       VALUE 'J'.                   
006540     88  KEY-DIST                            VALUE 'N'.                   
006600     EJECT                                                                
006610                                                                          
006620 77  WS-IDELMT-ERROR             PIC X(16).                               
006630 77  WS-IDMSG-ERROR              PIC X(03).                               
006640 77  WS-IDMSG-INFO               PIC X(03).                               
006650                                                                          
006660     EJECT                                                                
006670 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006680 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006690 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006691                                                                          
006700*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006800 01  GENERAL-SUBPROGRAMS.                                                 
007100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007300     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007310     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
007400     EJECT                                                                
007500*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
007600*01 -COPY WMEDAREA                                                        
007700     SKIP3                                                                
008309*    ----DISTR-DEALER-PRICE----                                           
008310 01  TEST-IDDISTR                PIC  9(5)   COMP-3.                      
008340     EJECT                                                                
008910*    --- AREA  FOR WZ01  ------                                           
008920 01  FILLER                      PIC X(16)   VALUE 'WZ01SUB '.            
008930*01  -COPY WZ01SUB                                                        
008940     EJECT                                                                
008980 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
008990 01  REQU-AREA.                                                           
008991*    03  -COPY WZ01REQU                                                   
008992*    03  -COPY WL0181I1                                                   
008993     EJECT                                                                
008994 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
008995 01  RESP-AREA.                                                           
008996*    03  -COPY WZ01RESP                                                   
008997*    03  -COPY WL0181O1                                                   
008998     EJECT                                                                
008999                                                                          
011300*    --- WORK-AREAS FOR IMS-SECTIONS                                      
011500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011600     SKIP3                                                                
011700 01  KEYS-TO-DLI.                                                         
011801*    --- VALUE OF SCROLLING KEY FOR FIRST LINE ON THE SCREEN              
011802     03  W-WDE1B1KY-MIN.                                                  
011804         05  W-WDE1B-IDDC-MIN        PIC X(2).                            
011805         05  W-WDE1B-IDDISTR-MIN     PIC S9(5)        COMP-3.             
011806         05  W-WDE1B-TISKEPP9-MIN    PIC S9(7)        COMP-3.             
011807         05  W-WDE1B-IDSHIPM-MIN     PIC  9(7).                           
011808         05  W-WDE1B-IDKUNDNR-MIN    PIC S9(7)        COMP-3.             
011810                                                                          
011811     03  W-WDE1B1KY-MAX.                                                  
011812         05  W-WDE1B-IDDC-MAX        PIC  X(2).                           
011813         05  W-WDE1B-IDDISTR-MAX     PIC S9(5)        COMP-3.             
011814         05  W-WDE1B-TISKEPP9-MAX    PIC S9(7)        COMP-3.             
011815         05  W-WDE1B-IDSHIPM-MAX     PIC  9(7).                           
011816         05  W-WDE1B-IDKUNDNR-MAX    PIC S9(7)        COMP-3.             
011818                                                                          
011819     03  W-WDE1B-IDKUNDNR-KVAL-X.                                         
011820         05  W-WDE1B-IDKUNDNR-KVAL   PIC S9(7)        COMP-3.             
011830                                                                          
011832     03  W-WDE1-IDSHIPM-X.                                                
011833         05  W-WDE1-IDSHIPM          PIC 9(7)     VALUE ZERO.             
011834                                                                          
011835     03  W-WDE1B-IDSHIPM-PREV        PIC  9(7).                           
011836                                                                          
011900     SKIP2                                                                
012000*    --- STATUS-KOD FRÅN IMS                                              
012100 01  STATUS-WS                       PIC XX.                              
012200     88  SEGMENT-FOUND                       VALUE '  '.                  
012300     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
012400     88  SEGMENT-MISSING                     VALUE 'GE'.                  
012500     SKIP2                                                                
012600 01  GOOD-STATUSCODES.                                                    
012700     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012800     SKIP3                                                                
012900 01  SSA1                        PIC X(256).                              
013000 01  SSA2                        PIC X(128).                              
013100     EJECT                                                                
013200*    --- IMS FUNCTION CODES                                               
013300*01  -COPY W0003                                                          
013500     EJECT                                                                
013600*    ---  DLI INPUT-OUTPUT AREA                                           
013700                                                                          
013801 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE1B1'.                      
013802 01  DLI-IO-WDE1B1.                                                       
013803*    03  -COPY WDE1B1                                                     
013804     EJECT                                                                
013805 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE101'.                      
013806 01  DLI-IO-WDE101.                                                       
013807*    03  -COPY WDE101                                                     
014100     EJECT                                                                
014200 LINKAGE SECTION.                                                         
014300 01  MSG-PCB                     PIC X.                                   
014602*01  -COPY W0008  -PRE WDE1B-                                             
014603     05  FILLER                  PIC X.                                   
014604                                                                          
014605*01  -COPY W0008  -PRE WDE1-                                              
014606     05  FILLER                  PIC X.                                   
014607                                                                          
014700     EJECT                                                                
014801 PROCEDURE DIVISION  USING MSG-PCB                                        
014802                           WDE1B-PCB WDE1-PCB.                            
014803                                                                          
014804 MAIN SECTION.                                                            
014810     ENTRY 'DLITCBL' USING MSG-PCB                                        
014820                           WDE1B-PCB WDE1-PCB.                            
014900                                                                          
015110     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
015210     IF SUB-KDRC = 0                                                      
015300       PERFORM A-INIT                                                     
015400       PERFORM B-CHECK-KEYS                                               
015500       IF KEYS-OK                                                         
016000         PERFORM F-READ-SHOW-INFO                                         
016100       END-IF                                                             
016530       PERFORM S02-RETURN-RESPONSE                                        
016600     END-IF                                                               
016800                                                                          
016900     MOVE ZERO TO RETURN-CODE                                             
017000     GOBACK                                                               
017100     .                                                                    
017200     EJECT                                                                
017300 A-INIT SECTION.                                                          
017410                                                                          
017420     MOVE ALL '+' TO RESP-AREA                                            
017430     MOVE SPACE   TO RESP-IDMSG-INFO                                      
017440                     RESP-IDMSG-ERROR                                     
017450                     RESP-IDELMT-ERROR                                    
017460     MOVE 001     TO RESP-IDMSGVER                                        
017470     MOVE ZERO    TO RESP-KVRADER                                         
017480                                                                          
020110     MOVE ZERO      TO  W-WDE1B-TISKEPP9-MIN                              
020120                        W-WDE1B-IDSHIPM-MIN                               
020130                        W-WDE1B-IDKUNDNR-MIN                              
020131                        W-WDE1B-IDKUNDNR-KVAL                             
020140     MOVE ALL '9'   TO  W-WDE1B-TISKEPP9-MAX                              
020150                        W-WDE1B-IDSHIPM-MAX                               
020160                        W-WDE1B-IDKUNDNR-MAX                              
020200     .                                                                    
020300     EJECT                                                                
020400 B-CHECK-KEYS SECTION.                                                    
020401                                                                          
022000     MOVE YES TO KEYS-SW                                                  
022201                                                                          
022202*    -- CHECK OF INPUT KEY                                                
022206                                                                          
022238     IF REQU-IDDISTR-KEY  NUMERIC                                         
022239       IF REQU-IDDISTR-KEY > ZERO                                         
022240         MOVE REQU-IDDISTR-KEY TO TEST-IDDISTR                            
022242         MOVE REQU-IDDISTR-KEY  TO W-WDE1B-IDDISTR-MIN                    
022243                                   W-WDE1B-IDDISTR-MAX                    
022250       ELSE                                                               
022251         MOVE NOO       TO KEYS-SW                                        
022252         MOVE 'IDDISTR' TO RESP-IDELMT-ERROR                              
022253         MOVE '126'     TO RESP-IDMSG-ERROR                               
022254       END-IF                                                             
022255     ELSE                                                                 
022256       MOVE NOO          TO KEYS-SW                                       
022257       MOVE 'IDDISTR'    TO RESP-IDELMT-ERROR                             
022258       MOVE '024'        TO RESP-IDMSG-ERROR                              
022259     END-IF                                                               
022260                                                                          
022261                                                                          
022262     IF REQU-IDKUNDNR-KEY NOT = ALL '+' AND                               
022263        REQU-IDKUNDNR-KEY NOT = ZERO                                      
022266       IF REQU-IDKUNDNR-KEY NOT NUMERIC                                   
022267         MOVE NOO        TO KEYS-SW                                       
022268         MOVE 'IDKUNDNR' TO RESP-IDELMT-ERROR                             
022269         MOVE '024'      TO RESP-IDMSG-ERROR                              
022270       ELSE                                                               
022271         MOVE YES        TO W-KEY-DIST-KUND                               
022272       END-IF                                                             
022273     ELSE                                                                 
022274       MOVE NOO          TO W-KEY-DIST-KUND                               
022275       MOVE ZERO         TO REQU-IDKUNDNR-KEY                             
022276     END-IF                                                               
022277                                                                          
022278     IF REQU-IDKUNDNR-KEY NUMERIC                                         
022281       MOVE REQU-IDKUNDNR-KEY  TO W-WDE1B-IDKUNDNR-MIN                    
022282                                  W-WDE1B-IDKUNDNR-MAX                    
022283                                  W-WDE1B-IDKUNDNR-KVAL                   
022284       IF REQU-IDKUNDNR-KEY NOT = ZERO                                    
022285         MOVE YES    TO W-KEY-DIST-KUND                                   
022286       ELSE                                                               
022287         MOVE NOO    TO W-KEY-DIST-KUND                                   
022288       END-IF                                                             
022289     ELSE                                                                 
022290       MOVE NOO                TO KEYS-SW                                 
022291       MOVE 'IDKUNDNR'         TO RESP-IDELMT-ERROR                       
022292       MOVE '024'              TO RESP-IDMSG-ERROR                        
022293     END-IF                                                               
022294                                                                          
022297     MOVE REQU-IDDC-KEY        TO W-WDE1B-IDDC-MIN                        
022298                                  W-WDE1B-IDDC-MAX                        
022310                                                                          
022318     IF KEYS-OK                                                           
022319       MOVE REQU-IDDISTR-KEY   TO RESP-IDDISTR-KEY                        
022322       MOVE REQU-IDKUNDNR-KEY  TO RESP-IDKUNDNR-KEY                       
022324       MOVE REQU-IDDC-KEY      TO RESP-IDDC-KEY                           
022330     END-IF                                                               
022331     .                                                                    
022340     EJECT                                                                
022400                                                                          
023600 F-READ-SHOW-INFO SECTION.                                                
023745                                                                          
023750     MOVE +1 TO INDX                                                      
023801     IF KEY-DIST-KUND                                                     
023810        PERFORM IMS-GU-WDE1B-KUND                                         
023820     ELSE                                                                 
023822        PERFORM IMS-GU-WDE1B                                              
023830     END-IF                                                               
023831     IF SEGMENT-FOUND                                                     
023850        MOVE SEQB-IDSHIPM  TO W-WDE1-IDSHIPM                              
023860        PERFORM IMS-GU-WDE1                                               
023870     END-IF                                                               
023900                                                                          
024000     IF SEGMENT-MISSING                                                   
024120        MOVE 'SHIPMENT'         TO RESP-IDELMT-ERROR                      
024130        MOVE '041'              TO RESP-IDMSG-ERROR                       
024500     ELSE                                                                 
024638                                                                          
024639       MOVE +1 TO INDX                                                    
024640       MOVE +0 TO WS-COUNT                                                
024641       PERFORM UNTIL INDX > MAX-INDX                                      
024642         IF SEGMENT-FOUND                                                 
024644           IF SHIP-IDTRPTNR <= 998                                        
024646             MOVE SHIP-IDSHIPM   TO RESP-IDSHIPM (INDX)                   
024648             MOVE SHIP-IDSHIPM   TO W-WDE1B-IDSHIPM-PREV                  
024650             MOVE SEQB-IDKUNDNR  TO RESP-IDKUNDNR (INDX)                  
024651             MOVE SHIP-IDTRPTNR  TO RESP-IDTRPTNR (INDX)                  
024652             MOVE SHIP-IDLBBET   TO RESP-IDLBBET (INDX)                   
024653             MOVE SHIP-TISKPTID  TO RESP-TISKPTID (INDX)                  
024654             MOVE SHIP-TISKEPPN  TO RESP-TISKEPPN (INDX)                  
024660             ADD 1 TO INDX                                                
024661             ADD +1 TO WS-COUNT                                           
024662           ELSE                                                           
024668             MOVE SHIP-IDSHIPM   TO W-WDE1B-IDSHIPM-PREV                  
024682           END-IF                                                         
024683           IF KEY-DIST-KUND                                               
024684              PERFORM IMS-GN-WDE1B-KUND                                   
024685           ELSE                                                           
024686              PERFORM IMS-GN-WDE1B                                        
024687           END-IF                                                         
024688           IF SEGMENT-FOUND                                               
024689              MOVE SEQB-IDSHIPM  TO W-WDE1-IDSHIPM                        
024690              PERFORM IMS-GU-WDE1                                         
024691           END-IF                                                         
024692         ELSE                                                             
024699           ADD 1 TO INDX                                                  
024700         END-IF                                                           
024703       END-PERFORM                                                        
024704       MOVE WS-COUNT    TO RESP-KVRADER                                   
024705                                                                          
024706       IF WS-COUNT = 500                                                  
024707          MOVE '028'  TO RESP-IDMSG-ERROR                                 
024708       END-IF                                                             
024730     END-IF                                                               
024800     .                                                                    
024900     EJECT                                                                
024901                                                                          
025200*    --- DISPATCHER SECTIONS                                              
025300 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
025400                                                                          
025500     MOVE 'GETARG'               TO SUB-KDFUNC                            
025600     MOVE 'CARPARTS.LDC.SHIPPINGPERDISTRICT'   TO SUB-ADDISPABS           
025700     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
025800                                                                          
025900     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
025910                                                                          
025920     IF SUB-KDRC > 0                                                      
025930       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
025940       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
025950       DELIMITED BY SIZE INTO ERROR-TEXT                                  
025960       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
025970     END-IF                                                               
025980     .                                                                    
025990     SKIP3                                                                
025991 S02-RETURN-RESPONSE SECTION.                                             
025992                                                                          
025993     MOVE 'RETURN'                   TO SUB-KDFUNC                        
025994     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
025995                                                                          
025996     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
025997                                                                          
025998     IF SUB-KDRC > 0                                                      
025999       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
026000       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
026001       DELIMITED BY SIZE INTO ERROR-TEXT                                  
026002       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
026003     END-IF                                                               
026004     .                                                                    
026005     EJECT                                                                
032710 IMS-GU-WDE1B SECTION.                                                    
032711                                                                          
032712     STRING 'WDE1B1  (WDE1B1KY>=' W-WDE1B1KY-MIN                          
032713                    '&WDE1B1KY<=' W-WDE1B1KY-MAX ')'                      
032714          DELIMITED BY SIZE INTO SSA1                                     
032715     MOVE '  GE' TO GOOD-STATUSCODES                                      
032716     CALL CBLTDLI USING GU WDE1B-PCB DLI-IO-WDE1B1 SSA1                   
032717     MOVE WDE1B-STATUS-CODE TO STATUS-WS                                  
032718     PERFORM IMS-STATUSCHECK                                              
032730     .                                                                    
032731     EJECT                                                                
032732 IMS-GN-WDE1B SECTION.                                                    
032733                                                                          
032734     STRING 'WDE1B1  (WDE1B1KY>=' W-WDE1B1KY-MIN                          
032735                    '&WDE1B1KY<=' W-WDE1B1KY-MAX                          
032736                    '&IDSHIPM NE' W-WDE1B-IDSHIPM-PREV ')'                
032737          DELIMITED BY SIZE INTO SSA1                                     
032738     MOVE '  GE' TO GOOD-STATUSCODES                                      
032739     CALL CBLTDLI USING GN WDE1B-PCB DLI-IO-WDE1B1 SSA1                   
032740     MOVE WDE1B-STATUS-CODE TO STATUS-WS                                  
032741     PERFORM IMS-STATUSCHECK                                              
032742     .                                                                    
032743     EJECT                                                                
032744 IMS-GU-WDE1B-KUND SECTION.                                               
032745                                                                          
032750     STRING 'WDE1B1  (WDE1B1KY>=' W-WDE1B1KY-MIN                          
032751                    '&WDE1B1KY<=' W-WDE1B1KY-MAX                          
032752                    '&IDKUNDNR =' W-WDE1B-IDKUNDNR-KVAL-X ')'             
032753          DELIMITED BY SIZE INTO SSA1                                     
032754     MOVE '  GE' TO GOOD-STATUSCODES                                      
032755     CALL CBLTDLI USING GU WDE1B-PCB DLI-IO-WDE1B1 SSA1                   
032756     MOVE WDE1B-STATUS-CODE TO STATUS-WS                                  
032773     PERFORM IMS-STATUSCHECK                                              
032774     .                                                                    
032775     EJECT                                                                
032776 IMS-GN-WDE1B-KUND SECTION.                                               
032777                                                                          
032778     STRING 'WDE1B1  (WDE1B1KY>=' W-WDE1B1KY-MIN                          
032779                    '&WDE1B1KY<=' W-WDE1B1KY-MAX                          
032780                    '&IDKUNDNR =' W-WDE1B-IDKUNDNR-KVAL-X ')'             
032782          DELIMITED BY SIZE INTO SSA1                                     
032783     MOVE '  GE' TO GOOD-STATUSCODES                                      
032784     CALL CBLTDLI USING GN WDE1B-PCB DLI-IO-WDE1B1 SSA1                   
032785     MOVE WDE1B-STATUS-CODE TO STATUS-WS                                  
032786     PERFORM IMS-STATUSCHECK                                              
032787     .                                                                    
032788     EJECT                                                                
032789 IMS-GU-WDE1      SECTION.                                                
032790                                                                          
032791     STRING 'WDE101  (IDSHIPM  =' W-WDE1-IDSHIPM-X ')'                    
032792          DELIMITED BY SIZE INTO SSA1                                     
032793     MOVE '  GE' TO GOOD-STATUSCODES                                      
032794     CALL CBLTDLI USING GU WDE1-PCB DLI-IO-WDE101 SSA1                    
032795     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
032796     PERFORM IMS-STATUSCHECK                                              
032803     .                                                                    
032804     EJECT                                                                
032900 IMS-STATUSCHECK SECTION.                                                 
033000                                                                          
033100     SET STATUS-IX TO 1                                                   
033200     SEARCH GOOD-STATUS                                                   
033300       AT END                                                             
033400         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
033500         DELIMITED BY SIZE INTO ERROR-TEXT                                
033600         CALL FELLOG                                                      
033700       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
033800         CONTINUE                                                         
033900     END-SEARCH                                                           
034000     .                                                                    
