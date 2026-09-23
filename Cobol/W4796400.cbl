001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     W4796400.                                                
001400 AUTHOR.         GÖRAN KJELLSON   GUIDE                                   
001500 DATE-WRITTEN.   NOVEMBER  2005                                           
001600 DATE-COMPILED.                                                           
001700                                                                          
001800                                                                          
001900*    FUNCTION:                                                            
002000*    PROGRAMMET LÄSER EXTRAKRFIL MED FAKTURAINFORMATION PER DC.           
002100*    FÖR VARJE DC SKICKAS DATA TILL D&P MED WZ01SEND                      
002200*    SOM SEDAN SKICKAR MAIL TILL AKTUELLA DC                              
002210*                                                                         
002600*                                                                         
002800 ENVIRONMENT DIVISION.                                                    
003000 INPUT-OUTPUT SECTION.                                                    
003200 FILE-CONTROL.                                                            
003302*          --- FAKTURAINFORMATION SENASTE VECKAN PER DC                   
003310     SELECT W47968                     ASSIGN TO W47964D1.                
003500                                                                          
003510                                                                          
003600 DATA DIVISION.                                                           
003700                                                                          
003800 FILE SECTION.                                                            
003905 FD  W47968                                                               
003906     RECORDING       F                                                    
003907     BLOCK CONTAINS  0.                                                   
003908                                                                          
003909 01  IN-DATA         PIC X(82).                                           
004000                                                                          
004010                                                                          
004200 WORKING-STORAGE SECTION.                                                 
004300                                                                          
004310 77  IDPGM                       PIC X(8)    VALUE 'W4796400'.            
004320 77  CURRENT-SECTION             PIC X(16)   VALUE 'MAIN'.                
004330 77  CURRENT-DP-SECTION          PIC X(16)   VALUE SPACE.                 
004331 77  CURRENT-IDDC                PIC X(2)    VALUE SPACE.                 
004340                                                                          
004400 77  JA                          PIC X(1)    VALUE 'J'.                   
004500 77  NEJ                         PIC X(1)    VALUE 'N'.                   
004501                                                                          
004502 77  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
004527 77  WS-ADRESS                   PIC X(50)                                
004528                           VALUE 'CARPARTS.DAP.DISTRDOC'.                 
004541                                                                          
004546 77  W47968-EOF-SW               PIC X       VALUE 'N'.                   
004550     88  END-OF-W47968                       VALUE 'J'.                   
004551                                                                          
004552 77  DC-SW                       PIC X       VALUE 'J'.                   
004553     88  FORSTA-DC                           VALUE 'J'.                   
004560                                                                          
004696                                                                          
004700 01  ERRTEXT.                                                             
004800     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
004900     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
005000 01  KDRC-DISPLAY                PIC Z(5).                                
005400                                                                          
006010                                                                          
006100 01  GENERAL-SUBPROGRAMS.                                                 
006400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006401     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
006403                                                                          
006404                                                                          
006405 01  DAGENS-DATUM                PIC 9(6)  VALUE ZERO.                    
006406 01  FILLER REDEFINES DAGENS-DATUM.                                       
006407     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006408     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006409     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006410 01  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
006411                                                                          
006420*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
006430                                                                          
006440 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006450 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006460 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006601                                                                          
006603                                                                          
006604*    --- AREOR FÖR KOMMUNIKATION                                          
006609 01  FILLER                  PIC X(16)   VALUE 'SEND-CONTROL'.            
006610*01  -COPY WZ01SEND                                                       
006620                                                                          
009001                                                                          
009002*    --- IN-AREOR                                                         
009003 01  INPUT-AREA                 PIC X(24)   VALUE                         
009004                                'INPUT-AREA     '.                        
009009 01  IN-AREA.                                                             
009010     03  IN-IDDC                PIC X(2).                                 
009011     03  IN-POST                PIC X(80).                                
009012                                                                          
009023                                                                          
009028*    --- UT-AREOR                                                         
009029 01  OUTPUT-AREA                 PIC X(24)   VALUE                        
009030                                'OUTPUT-AREA     '.                       
009031 01  HDR-AREA.                                                            
009032*    03  -COPY WZ01REQU                                                   
009033*    03  -COPY WZ04HDR                                                    
009034                                                                          
009040                                                                          
009300 01  DOC-LINE-AREA               PIC X(80).                               
009320                                                                          
009330                                                                          
010097                                                                          
010100 LINKAGE SECTION.                                                         
010300*01  -COPY W0009   -PRE MSG-                                              
010700     EJECT                                                                
010800                                                                          
010801 PROCEDURE DIVISION  USING MSG-PCB.                                       
010802 MAIN SECTION.                                                            
010803                                                                          
010810     ENTRY 'DLITCBL' USING MSG-PCB.                                       
010900                                                                          
011200     PERFORM A-INIT                                                       
011201     PERFORM S01-READ-W47968                                              
011202     IF NOT END-OF-W47968                                                 
011203        PERFORM UNTIL END-OF-W47968                                       
011204           IF IN-IDDC NOT = CURRENT-IDDC                                  
011205              IF FORSTA-DC                                                
011206                 MOVE NEJ TO DC-SW                                        
011209              ELSE                                                        
011210                 PERFORM S92-SEND-CLOSE                                   
011212              END-IF                                                      
011213              MOVE IN-IDDC TO CURRENT-IDDC                                
011214              PERFORM B-INIT-NEW-DC                                       
011215           END-IF                                                         
011216           MOVE IN-POST TO DOC-LINE-AREA                                  
011217           PERFORM S91-PUT-DOC-LINE                                       
011218           PERFORM S01-READ-W47968                                        
011220        END-PERFORM                                                       
011221                                                                          
011230        PERFORM S92-SEND-CLOSE                                            
011240     END-IF                                                               
011250                                                                          
012510     PERFORM Z-FINIT                                                      
012600                                                                          
012700     MOVE ZERO TO RETURN-CODE                                             
012800     GOBACK                                                               
012900     .                                                                    
013000                                                                          
013100 A-INIT SECTION.                                                          
013200     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
013300                                                                          
013310     OPEN INPUT W47968                                                    
013320     ACCEPT DAGENS-DATUM     FROM DATE                                    
013330     ACCEPT DAGENS-TID       FROM TIME                                    
014200     .                                                                    
014300                                                                          
014459 B-INIT-NEW-DC    SECTION.                                                
014460     MOVE 'B-INIT-NEW-DC   ' TO CURRENT-SECTION                           
014461                                                                          
014462     MOVE 1                          TO REQU-IDMSGVER                     
014463     MOVE 'R'                        TO REQU-KDPGMACT                     
014464     MOVE IDPGM                      TO REQU-IDUSER                       
014466                                                                          
014467     MOVE 'W47964'                   TO HDR-IDOUTTYPE                     
014468     MOVE CURRENT-IDDC               TO HDR-IDOUTREC                      
014469     MOVE DAGENS-DATUM-DAG           TO HDR-IDLIST(1:2)                   
014470     MOVE DAGENS-TID                 TO HDR-IDLIST(3:8)                   
014511                                                                          
014515*    IF WZ04-SEND-IDCOM = ZERO                                            
014516       PERFORM S90-SEND-OPEN                                              
014517       MOVE SEND-IDCOM TO WZ04-SEND-IDCOM                                 
014518*    END-IF                                                               
014519                                                                          
014520     PERFORM S91-PUT-HEADER                                               
014771     .                                                                    
014772                                                                          
015150                                                                          
015151 Z-FINIT SECTION.                                                         
015152     MOVE 'Z-FINIT         ' TO CURRENT-SECTION                           
015153                                                                          
015154     CLOSE W47968                                                         
015155     .                                                                    
015156                                                                          
015157 S01-READ-W47968  SECTION.                                                
015158     MOVE 'S01-READ-W47968 ' TO CURRENT-SECTION                           
015159                                                                          
015160     READ W47968 INTO IN-AREA                                             
015161       AT END                                                             
015162         SET END-OF-W47968 TO TRUE                                        
015163     END-READ                                                             
015164     .                                                                    
015170                                                                          
015456 S90-SEND-OPEN SECTION.                                                   
015457     MOVE 'S90-SEND-OPEN   ' TO CURRENT-DP-SECTION                        
015458                                                                          
015459     MOVE WS-ADRESS                       TO SEND-ADDISPABS               
015461     MOVE 'OPEN'                          TO SEND-KDFUNC                  
015462     CALL WZ01SEND USING SEND-CONTROL-AREA                                
015463                         SEND-OPEN-AREA                                   
015464     IF SEND-KDRC > ZERO                                                  
015465       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
015466       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
015467       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
015468       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
015469     END-IF                                                               
015470     .                                                                    
015471                                                                          
015472 S91-PUT-HEADER SECTION.                                                  
015473     MOVE 'S91-PUT-HEADER  ' TO CURRENT-DP-SECTION                        
015474                                                                          
015475     MOVE 'PUT'                           TO SEND-KDFUNC                  
015476     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
015477     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
015478     CALL WZ01SEND USING SEND-CONTROL-AREA                                
015479                         SEND-KVDLEN                                      
015480                         HDR-AREA                                         
015481     IF SEND-KDRC > ZERO                                                  
015482       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
015483       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
015484       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
015485       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
015486     END-IF                                                               
015487     .                                                                    
015488                                                                          
015505 S91-PUT-DOC-LINE SECTION.                                                
015506     MOVE 'S91-PUT-DOC-LINE' TO CURRENT-DP-SECTION                        
015507                                                                          
015508     MOVE 'PUT'                           TO SEND-KDFUNC                  
015509     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
015510     MOVE LENGTH OF DOC-LINE-AREA         TO SEND-KVDLEN                  
015512     CALL WZ01SEND USING SEND-CONTROL-AREA                                
015513                         SEND-KVDLEN                                      
015514                         DOC-LINE-AREA                                    
015515     IF SEND-KDRC > ZERO                                                  
015516       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
015517       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
015518       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
015519       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
015520     END-IF                                                               
015521     .                                                                    
015522                                                                          
015623                                                                          
015624 S92-SEND-CLOSE SECTION.                                                  
015625     MOVE 'S92-SEND-CLOSE  ' TO CURRENT-DP-SECTION                        
015626                                                                          
015627     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
015628     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
015630     CALL WZ01SEND USING SEND-CONTROL-AREA                                
015700     .                                                                    
