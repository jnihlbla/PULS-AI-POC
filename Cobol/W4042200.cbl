000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4042200.                                                
000300 AUTHOR.         BO SVENSSON.                                             
000400 DATE-WRITTEN.   96/12/02.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        THIS IS A QUESTION PROGRAM ON CUSTOMER DATABASE.                 
000900*        GIVEN DISTRICT AND CUSTOMERNO WILL DISPLAY INFO                  
001000*        ABOUT THIS GOODS RECIVER.                                        
001100*        IF ALSO DC IS GIVEN INFO ABOUT DELIVERIES FROM THIS DC           
001200*        TO THE GOODS RECIEVER IS ADDED.                                  
001300*                                                                         
001400*        THE PROGRAM READS     WLGMTA (WDB2)                              
001500*        THE PROGRAM READS     WLGMTB (WDB3)                              
001600*        THE PROGRAM READS     WDB101                                     
001700*        THE PROGRAM READS     WDB601                                     
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSACTION: W4T422                                              
002100*        MID:         W4I42201                                            
002200*                                                                         
002300*    OUTDATA.                                                             
002400*        MOD:         W4O42201                                            
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000 WORKING-STORAGE SECTION.                                                 
003100*    -COPY WY2000W1                                                       
003200     SKIP3                                                                
003300 77  IDPGM                       PIC X(08)   VALUE 'W4042200'.            
003400                                                                          
003500*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003600 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003700                                                                          
003800 77  INDX                        PIC S9(4)   VALUE +0  COMP SYNC.         
003900 77  MAX-INDX                    PIC S9(4)   VALUE +7  COMP SYNC.         
004000 77  YES                         PIC X       VALUE 'Y'.                   
004100 77  NOO                         PIC X       VALUE 'N'.                   
004200                                                                          
004300 77  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
004400                                                                          
004500*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
004600                                                                          
004700                                                                          
004800 77  KEYS-SW                     PIC X       VALUE 'J'.                   
004900     88  KEYS-OK                             VALUE 'Y'.                   
005000     88  KEYS-WRONG                          VALUE 'N'.                   
005100                                                                          
005200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005300     88  OWN-MID                             VALUE '4422'.                
005400     88  GOOD-MID                            VALUE '4421' '4422'          
005500                                                   '4423' '4424'          
005600                                                   '4425' '4426'          
005700                                                   '4427' '4428'          
005800                                                   '4429'.                
005900     88  HELP-MID                            VALUE '0551'.                
006000                                                                          
006100*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006200 01  GENERAL-SUBPROGRAMS.                                                 
006300     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006400     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006500     03  WSECURIT                PIC X(8)    VALUE 'WSECURIT'.            
006600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006800     EJECT                                                                
006900*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
007000*01 -COPY WMEDAREA                                                        
007100     EJECT                                                                
007200 01  FILLER                      PIC X(16)   VALUE 'WSEC-AREA  '.         
007300*01  -COPY WSECAREA                                                       
007400     SKIP3                                                                
007500 01  MESSAGE-CODES.                                                       
007600     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007700     03  ERR-OBEHORIG            PIC X(3)    VALUE '405'.                 
007800     EJECT                                                                
007900*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
008000*                                                                         
008100 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008200     SKIP3                                                                
008300*01 -COPY WMSGINIT                                                        
008400*                                                                         
008500     EJECT                                                                
008600*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
008700*                                                                         
008800 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
008900     SKIP3                                                                
009000*01  MID -COPY W4I42201                                                   
009100     EJECT                                                                
009200 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009300     SKIP3                                                                
009400*01  -COPY WMSGAREA                                                       
009500     EJECT                                                                
009600     03  MOD REDEFINES MSG-AREA.                                          
009700*      05  -COPY W4O42201                                                 
009800     EJECT                                                                
009900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010000     SKIP3                                                                
010100*01  -COPY WMFSAREA                                                       
010200     EJECT                                                                
010300*    --- WORK-AREAS FOR IMS-SECTIONS                                      
010400*                                                                         
010500     EJECT                                                                
010600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010700     SKIP3                                                                
010800 01  KEYS-TO-DLI.                                                         
010900     03  W-IDGMT-X.                                                       
011000         05  W-IDDISTR           PIC S9(5) COMP-3 VALUE ZERO.             
011100         05  W-IDKUNDNR          PIC S9(7) COMP-3 VALUE ZERO.             
011200     03  W-WDB301KY-X.                                                    
011300         05  W-IDDC-301          PIC X(2)         VALUE SPACE.            
011400         05  W-IDDISTR-301       PIC S9(5) COMP-3 VALUE ZERO.             
011500         05  W-IDKUNDNR-301      PIC S9(7) COMP-3 VALUE ZERO.             
011600     03  W-WDB301KY-DEF.                                                  
011700         05  W-IDDC-301-DEF      PIC X(2)         VALUE SPACE.            
011800         05  W-IDDISTR-301-DEF   PIC S9(5) COMP-3 VALUE ZERO.             
011900         05  W-IDKUNDNR-301-DEF  PIC S9(7) COMP-3 VALUE +9999999.         
012000     03  W-WDB501KY-X.                                                    
012100         05  W-IDDC-501          PIC X(2)         VALUE SPACE.            
012200         05  W-KDFRAKT-501       PIC S9(3) COMP-3 VALUE ZERO.             
012300         05  W-IDDISTR-501       PIC S9(5) COMP-3 VALUE ZERO.             
012400         05  W-IDKUNDNR-501      PIC S9(7) COMP-3 VALUE ZERO.             
012500     03  W-WDB501KY-DEF.                                                  
012600         05  W-IDDC-501-DEF      PIC X(2)         VALUE SPACE.            
012700         05  W-KDFRAKT-501-DEF   PIC S9(3) COMP-3 VALUE ZERO.             
012800         05  W-IDDISTR-501-DEF   PIC S9(5) COMP-3 VALUE ZERO.             
012900         05  W-IDKUNDNR-501-DEF  PIC S9(7) COMP-3 VALUE +9999999.         
013000     03  W-WDB101KY-X.                                                    
013100         05  W-IDPARTNR          PIC X(9)         VALUE SPACE.            
013200         05  W-IDFTG             PIC 9(2)         VALUE ZERO.             
013300                                                                          
013400*----> FÖRRÅDSDATATEXT/EMBALLAGEKOD.                                      
013500                                                                          
013600     03  W-4535-IDHTYP-X.                                                 
013700         05  W-4535-IDHTYP       PIC  X(4)  VALUE '4535'.                 
013800         05  W-4535-KDFDKRAV     PIC S9(3)  COMP-3.                       
013900         05  W-4535-LOW-VALUE    PIC  X(24) VALUE LOW-VALUE.              
014000                                                                          
014100     03  W-4536-IDSKYLT-X.                                                
014200         05  W-4536-IDSKYLT      PIC  X(3).                               
014300         05  W-4536-LOW-VALUE    PIC  X(2)  VALUE LOW-VALUE.              
014400                                                                          
014500*----> TRANSPORTÖR NDC-NA                                                 
014600                                                                          
014700     03  W-WDGXKEY-4433-X.                                                
014800         05  W-4433-IDHTYP       PIC X(4)    VALUE '4433'.                
014900         05  W-4433-IDDC         PIC X(2).                                
015000         05  W-4433-LOWVALUE     PIC X(24)   VALUE LOW-VALUE.             
015100                                                                          
015200     03  W-WDGXKEY-4434-X.                                                
015300         05  W-4434-IDTRP        PIC X(5)    VALUE SPACE.                 
015400         05  W-4434-TITRPAVG     PIC S9(7)   COMP-3 VALUE ZERO.           
015500         05  W-4434-LOW-VALUE    PIC X       VALUE LOW-VALUE.             
015600                                                                          
015700     03  W-IDDC-B6-X.                                                     
015800         05 W-IDDC-B6                  PIC X(2).                          
015900                                                                          
016000     SKIP2                                                                
016100*    --- STATUS-KOD FRÅN IMS                                              
016200 01  STATUS-WS                   PIC XX.                                  
016300     88  SEGMENT-FOUND                       VALUE '  '.                  
016400     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
016500     88  SEGMENT-MISSING                     VALUE 'GE'.                  
016600     SKIP2                                                                
016700 01  GOOD-STATUSCODES.                                                    
016800     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016900     SKIP3                                                                
017000 01  SSA1                        PIC X(64).                               
017100 01  SSA2                        PIC X(64).                               
017200     EJECT                                                                
017300*    --- IMS FUNCTION CODES                                               
017400*01  -COPY W0003                                                          
017500     EJECT                                                                
017600*    ---  DLI INPUT-OUTPUT AREA                                           
017700                                                                          
017800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLGMTA01'.                    
017900 01  DLI-IO-WLGMTA01.                                                     
018000*    03  -COPY WDB201                                                     
018100 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLGMTB01'.                    
018200 01  DLI-IO-WLGMTB01.                                                     
018300*    03  -COPY WDB301                                                     
018400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLGMTC01'.                    
018500 01  DLI-IO-WLGMTC01.                                                     
018600*    03  -COPY WDB501                                                     
018700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDB101'.                      
018800 01  DLI-IO-WDB101.                                                       
018900*    03  -COPY WDB101                                                     
019000     EJECT                                                                
019100 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLXXKU11'.                    
019200 01  DLI-IO-WLXXKU11.                                                     
019300*    03 -COPY WDGX4536                                                    
019400     EJECT                                                                
019500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLXXKB11'.                    
019600 01  DLI-IO-WLXXKB11.                                                     
019700*    03 -COPY WDGX4434                                                    
019800                                                                          
019900 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
020000 01   DLI-IO-AREA-B601.                                                   
020100*     03  -COPY WDB601                                                    
020200                                                                          
020300     EJECT                                                                
020400 LINKAGE SECTION.                                                         
020500*01  -COPY W0009   -PRE MSG-                                              
020600*01  -COPY W0008   -PRE USEA-                                             
020700     05  FILLER                  PIC X.                                   
020800     EJECT                                                                
020900*01  -COPY W0008  -PRE GMTA-                                              
021000     05  FILLER                  PIC X.                                   
021100     EJECT                                                                
021200*01  -COPY W0008  -PRE GMTB-                                              
021300     05  FILLER                  PIC X.                                   
021400     EJECT                                                                
021500*01  -COPY W0008  -PRE GMTC-                                              
021600     05  FILLER                  PIC X.                                   
021700     EJECT                                                                
021800*01  -COPY W0008  -PRE XXKU-                                              
021900     05  FILLER                  PIC X.                                   
022000     EJECT                                                                
022100*01  -COPY W0008  -PRE XXKB-                                              
022200     05  FILLER                  PIC X.                                   
022300     EJECT                                                                
022400*01  -COPY W0008  -PRE WDB1-                                              
022500     05  FILLER                  PIC X.                                   
022600     EJECT                                                                
022700*01  -COPY W0008  -PRE WDB6-                                              
022800     05  FILLER                  PIC X.                                   
022900     EJECT                                                                
023000 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB GMTA-PCB GMTB-PCB             
023100     GMTC-PCB XXKU-PCB XXKB-PCB WDB1-PCB WDB6-PCB.                        
023200 MAIN SECTION.                                                            
023300     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB GMTA-PCB GMTB-PCB             
023400     GMTC-PCB XXKU-PCB XXKB-PCB WDB1-PCB WDB6-PCB.                        
023500                                                                          
023600     PERFORM IMS-GET-MSG                                                  
023700     IF SEGMENT-FOUND                                                     
023800       PERFORM A-INIT                                                     
023900       PERFORM B-CHECK-KEYS                                               
024000       IF KEYS-OK                                                         
024100         PERFORM F-READ-SHOW-INFO                                         
024200       END-IF                                                             
024300*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
024400*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
024500       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O42201 + 4                      
024600       PERFORM IMS-INSERT-MSG                                             
024700     END-IF                                                               
024800                                                                          
024900     MOVE ZERO TO RETURN-CODE                                             
025000     GOBACK                                                               
025100     .                                                                    
025200     EJECT                                                                
025300 A-INIT SECTION.                                                          
025400                                                                          
025500     IF MSG-DOUBLE-TRANSACTIONS                                           
025600       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W4I42201                 
025700       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
025800       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
025900     ELSE                                                                 
026000       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W4I42201                  
026100       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
026200       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
026300     END-IF                                                               
026400                                                                          
026500     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
026600     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
026700     MOVE MFS-IDTRANS TO W-IDTRANS                                        
026800                                                                          
026900     MOVE LOW-VALUE TO MSG-AREA                                           
027000     MOVE 'W4O422N1' TO MFS-IDMOD                                         
027100     MOVE '4422' TO MOD-IDTRANS                                           
027200     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
027300                                                                          
027400     IF OWN-MID OR HELP-MID                                               
027500       CONTINUE                                                           
027600     ELSE                                                                 
027700       MOVE SPACE TO MFS-KDTRTYP                                          
027800       MOVE '7' TO MFS-IDPFK                                              
027900     END-IF                                                               
028000                                                                          
028100     ACCEPT TODAYS-DATE  FROM DATE                                        
028200     .                                                                    
028300     EJECT                                                                
028400 B-CHECK-KEYS SECTION.                                                    
028500                                                                          
028600     MOVE ALL '+'           TO MSGI-WMSGINIT                              
028700     MOVE '001'             TO MSGI-KDCALL                                
028800     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
028900     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
029000     MOVE '4422'            TO MSGI-IDTRANS                               
029100     IF OWN-MID                                                           
029200         MOVE MID-IDDISTR-IN   TO MSGI-IDDISTR                            
029300         MOVE MID-IDKUNDNR-IN  TO MSGI-IDKUNDNR                           
029400         MOVE MID-KDFRAKT-IN   TO MSGI-KDFRAKT                            
029500     ELSE                                                                 
029600         IF  MID-IDDISTR-IN NUMERIC                                       
029700             MOVE MID-IDDISTR-IN TO MSGI-IDDISTR                          
029800         END-IF                                                           
029900         IF  MID-IDKUNDNR-IN NUMERIC                                      
030000             MOVE MID-IDKUNDNR-IN TO MSGI-IDKUNDNR                        
030100         END-IF                                                           
030200         IF  MID-KDFRAKT-IN NUMERIC                                       
030300             MOVE MID-KDFRAKT-IN TO MSGI-KDFRAKT                          
030400         END-IF                                                           
030500     END-IF                                                               
030600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
030700     MOVE MSGI-IDLAND-SPR      TO MED-IDSKYLT                             
030800                                                                          
030900     MOVE YES TO KEYS-SW                                                  
031000                                                                          
031100                                                                          
031200*    -- CHECK OF IDDISTR                                                  
031300     MOVE MFS-ERASE-FIELD TO MOD-IDDISTR-IN                               
031400                                                                          
031500     IF MID-IDDISTR-IN NOT = ALL '+'                                      
031600       MOVE '7'         TO MFS-IDPFK                                      
031700       MOVE SPACE       TO MFS-KDTRTYP                                    
031800     END-IF                                                               
031900     INSPECT MSGI-IDDISTR REPLACING LEADING SPACE BY ZERO                 
032000     IF MSGI-IDDISTR NUMERIC                                              
032100       MOVE MSGI-IDDISTR TO W-IDDISTR                                     
032200                            W-IDDISTR-301                                 
032300                            W-IDDISTR-501                                 
032400                            W-IDDISTR-301-DEF                             
032500                            W-IDDISTR-501-DEF                             
032600     ELSE                                                                 
032700       MOVE NOO TO KEYS-SW                                                
032800     END-IF                                                               
032900                                                                          
033000*    -- CHECK OF IDKUNDNR                                                 
033100     MOVE MFS-ERASE-FIELD TO MOD-IDKUNDNR-IN                              
033200                                                                          
033300     IF MID-IDKUNDNR-IN NOT = ALL '+'                                     
033400       MOVE '7'         TO MFS-IDPFK                                      
033500       MOVE SPACE       TO MFS-KDTRTYP                                    
033600     END-IF                                                               
033700     INSPECT MSGI-IDKUNDNR REPLACING LEADING SPACE BY ZERO                
033800     IF MSGI-IDKUNDNR NUMERIC                                             
033900       MOVE MSGI-IDKUNDNR TO W-IDKUNDNR                                   
034000                             W-IDKUNDNR-301                               
034100                             W-IDKUNDNR-501                               
034200       MOVE +9999999      TO W-IDKUNDNR-301-DEF                           
034300                             W-IDKUNDNR-501-DEF                           
034400     ELSE                                                                 
034500       MOVE NOO TO KEYS-SW                                                
034600     END-IF                                                               
034700                                                                          
034800*    -- CHECK OF IDDC                                                     
034900     MOVE MFS-ERASE-FIELD TO MOD-IDDC-IN                                  
035000                                                                          
035100     IF MID-IDDC-IN NOT = ALL '+'                                         
035200       MOVE '7'         TO MFS-IDPFK                                      
035300       MOVE SPACE       TO MFS-KDTRTYP                                    
035400       MOVE MID-IDDC-IN TO W-IDDC-B6                                      
035500     ELSE                                                                 
035600       MOVE MID-IDDC-UT TO W-IDDC-B6                                      
035700     END-IF                                                               
035800     PERFORM IMS-GU-WDB601                                                
035900                                                                          
036000     IF  DCS-KDDC = SPACE OR DCS-DDC OR DCS-CDC-TR                        
036100         MOVE MSGI-IDDC TO W-IDDC-B6                                      
036200         PERFORM IMS-GU-WDB601                                            
036300     END-IF                                                               
036400                                                                          
036500*    -- CHECK OF KDFRAKT                                                  
036600     MOVE MFS-ERASE-FIELD TO MOD-KDFRAKT-IN                               
036700                                                                          
036800     IF MID-KDFRAKT-IN NOT = ALL '+'                                      
036900       MOVE '7'         TO MFS-IDPFK                                      
037000       MOVE SPACE       TO MFS-KDTRTYP                                    
037100     END-IF                                                               
037200     INSPECT MSGI-KDFRAKT REPLACING LEADING SPACE BY ZERO                 
037300     IF MSGI-KDFRAKT NUMERIC                                              
037400       MOVE MSGI-KDFRAKT  TO W-KDFRAKT-501                                
037500                             W-KDFRAKT-501-DEF                            
037600     ELSE                                                                 
037700       MOVE NOO TO KEYS-SW                                                
037800     END-IF                                                               
037900                                                                          
038000     IF GOOD-MID OR KEYS-OK                                               
038100       MOVE MSGI-IDDISTR      TO MOD-IDDISTR-UT                           
038200       INSPECT MOD-IDDISTR-UT    REPLACING LEADING ZERO BY SPACE          
038300       MOVE MSGI-IDKUNDNR     TO MOD-IDKUNDNR-UT                          
038400       INSPECT MOD-IDKUNDNR-UT   REPLACING LEADING ZERO BY SPACE          
038500       MOVE DCS-IDDC          TO MOD-IDDC-UT                              
038600       MOVE MSGI-KDFRAKT      TO MOD-KDFRAKT-UT                           
038700     ELSE                                                                 
038800       MOVE MFS-ERASE-FIELD TO MOD-IDDISTR-UT                             
038900       MOVE MFS-ERASE-FIELD TO MOD-IDKUNDNR-UT                            
039000       MOVE MFS-ERASE-FIELD TO MOD-IDDC-UT                                
039100       MOVE MFS-ERASE-FIELD TO MOD-KDFRAKT-UT                             
039200     END-IF                                                               
039300                                                                          
039400     IF KEYS-WRONG                                                        
039500       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
039600       CALL WMEDKONV USING MED-WMEDAREA                                   
039700       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
039800       PERFORM MFS-ERASE-FIELD-OUT                                        
039900     ELSE                                                                 
040000                                                                          
040100       MOVE MSG-SIGNON-USERID TO    SEC-IDUSER                            
040200       MOVE '4422'            TO    SEC-IDTRANS                           
040300       MOVE MSGI-IDDISTR      TO    SEC-IDKEY                             
040400                                                                          
040500       CALL WSECURIT          USING SEC-IDUSER                            
040600                                    SEC-IDTRANS                           
040700                                    SEC-IDKEY                             
040800                                    SEC-KDSVAR                            
040900                                                                          
041000       IF SEC-KDSVAR = 'F'                                                
041100         MOVE ERR-OBEHORIG TO MED-IDMFSFEL                                
041200         CALL WMEDKONV USING MED-WMEDAREA                                 
041300         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
041400         PERFORM MFS-ERASE-FIELD-OUT                                      
041500         MOVE NOO          TO KEYS-SW                                     
041600       ELSE                                                               
041700          CONTINUE                                                        
041800       END-IF                                                             
041900     END-IF                                                               
042000     .                                                                    
042100     EJECT                                                                
042200 F-READ-SHOW-INFO SECTION.                                                
042300                                                                          
042400     PERFORM FA-READ-BASICDATA                                            
042500                                                                          
042600     IF SEGMENT-MISSING                                                   
042700* MOVE RIGHT ERRORMESSAGE                                                 
042800*       CALL WMEDKONV USING MED-WMEDAREA                                  
042900*       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
043000        MOVE 'GOODS RECEIVER MISSING' TO MOD-TEMFSFEL                     
043100        PERFORM MFS-ERASE-FIELD-OUT                                       
043200     ELSE                                                                 
043300                                                                          
043400        MOVE DCS-IDDC   TO W-IDDC-301                                     
043500                           W-IDDC-301-DEF                                 
043600        PERFORM IMS-GET-GMTB                                              
043700        IF  SEGMENT-FOUND                                                 
043800            IF  W-KDFRAKT-501 = 0                                         
043900                MOVE DC-KDGENFRA-MO TO W-KDFRAKT-501                      
044000                                       W-KDFRAKT-501-DEF                  
044100            END-IF                                                        
044200                                                                          
044300            MOVE DCS-IDDC   TO W-IDDC-501                                 
044400                               W-IDDC-501-DEF                             
044500                                                                          
044600            PERFORM IMS-GET-GMTC                                          
044700            IF  SEGMENT-FOUND                                             
044800                MOVE FK-KDFDKRAV TO MOD-KDFDKRAV                          
044900                MOVE FK-KDTRPKAT TO MOD-KDTRPKAT                          
045000                MOVE FK-IDTRP-0  TO MOD-IDTRP-0                           
045100                MOVE FK-IDTRP-1  TO MOD-IDTRP-1                           
045200                MOVE FK-IDTRP-2  TO MOD-IDTRP-2                           
045300                MOVE FK-IDTRP-3  TO MOD-IDTRP-3                           
045400                MOVE FK-IDTRP-4  TO MOD-IDTRP-4                           
045500                                                                          
045600                MOVE FK-KDFDKRAV TO W-4535-KDFDKRAV                       
045700                MOVE 'GB '       TO W-4536-IDSKYLT                        
045800                PERFORM IMS-GU-XXKU-WLXXKU11                              
045900                IF SEGMENT-FOUND                                          
046000                    MOVE 4536-BEFDKRAV TO MOD-BEFDKRAV                    
046100                ELSE                                                      
046200                    MOVE MFS-ERASE-FIELD TO MOD-BEFDKRAV                  
046300                END-IF                                                    
046400                                                                          
046500                PERFORM FB-CARRIER                                        
046600            ELSE                                                          
046700                MOVE 'NO SUPPORT FOR GIVEN FC' TO MOD-TEMFSFEL            
046800                PERFORM MFS-ERASE-FIELD-OUT-FC                            
046900            END-IF                                                        
047000        ELSE                                                              
047100            MOVE 'NO SUPPORT FROM GIVEN DC' TO MOD-TEMFSFEL               
047200            PERFORM MFS-ERASE-FIELD-OUT-FC                                
047300        END-IF                                                            
047400     END-IF                                                               
047500     .                                                                    
047600     EJECT                                                                
047700 FA-READ-BASICDATA SECTION.                                               
047800                                                                          
047900     PERFORM IMS-GET-GMTA                                                 
048000                                                                          
048100     IF  SEGMENT-FOUND                                                    
048200         MOVE GMT-BEGMT-RAD1        TO MOD-BEGMT-RAD1                     
048300         MOVE GMT-BEGMT-RAD2        TO MOD-BEGMT-RAD2                     
048400         MOVE GMT-ADGMT-GATA        TO MOD-ADGMT-GATA                     
048500         MOVE GMT-ADGMT-PADR        TO MOD-ADGMT-PADR                     
048600         MOVE GMT-ADGMT-LAND        TO MOD-ADGMT-LAND                     
048700                                                                          
048800         MOVE GMT-IDPARTNR          TO W-IDPARTNR                         
048810         IF DCS-LAND-NON-VCC-OWNED                                        
048900           MOVE DCS-IDFTG             TO W-IDFTG                          
048910         ELSE                                                             
048911           MOVE 57                    TO W-IDFTG                          
048920         END-IF                                                           
049000                                                                          
049100         PERFORM IMS-GET-WDB1                                             
049200         IF  SEGMENT-FOUND                                                
049300           MOVE BET-KDBETVIL        TO MOD-KDBETVIL                       
049400           MOVE BET-BEBETVIL        TO MOD-BEBETVIL-X27                   
049500         ELSE                                                             
049600             MOVE MFS-ERASE-FIELD   TO MOD-KDBETVIL                       
049700                                       MOD-BEBETVIL-X27                   
049800         END-IF                                                           
049900                                                                          
050000         MOVE +1 TO INDX                                                  
050100         PERFORM UNTIL INDX > MAX-INDX                                    
050200           MOVE GMT-IDDC-BULK(INDX) TO MOD-IDDC-BULK(INDX)                
050300           MOVE GMT-IDDC-DAY(INDX)  TO MOD-IDDC-DAY(INDX)                 
050400           MOVE GMT-IDDC-VOR(INDX)  TO MOD-IDDC-VOR(INDX)                 
050500           ADD +1 TO INDX                                                 
050600         END-PERFORM                                                      
050700                                                                          
050800         MOVE +1 TO INDX                                                  
050900         PERFORM UNTIL INDX > MAX-INDX                                    
051000           IF  GMT-IDDC-BULK(INDX) NOT = SPACE                            
051100               MOVE GMT-IDDC-BULK(INDX) TO W-IDDC-301                     
051200                                           W-IDDC-301-DEF                 
051300               PERFORM IMS-GET-GMTB-2                                     
051400               IF  SEGMENT-MISSING                                        
051500                   MOVE MFS-ERASE-FIELD                                   
051600                                      TO MOD-KDGENFRA-MO(INDX)            
051700                   MOVE 'SOME DC-INFORMATION MISSING!!'                   
051800                                      TO MOD-TEMFSFEL                     
051900               ELSE                                                       
052000                   MOVE DC-KDGENFRA-MO                                    
052100                                      TO MOD-KDGENFRA-MO(INDX)            
052200               END-IF                                                     
052300           ELSE                                                           
052400               MOVE MFS-ERASE-FIELD   TO MOD-KDGENFRA-MO(INDX)            
052500           END-IF                                                         
052600                                                                          
052700           IF  GMT-IDDC-DAY(INDX) NOT = SPACE                             
052800               MOVE GMT-IDDC-DAY(INDX) TO W-IDDC-301                      
052900                                          W-IDDC-301-DEF                  
053000               PERFORM IMS-GET-GMTB-2                                     
053100               IF  SEGMENT-MISSING                                        
053200                   MOVE MFS-ERASE-FIELD                                   
053300                                      TO MOD-KDGENFRA-DO(INDX)            
053400                   MOVE 'SOME DC-INFORMATION MISSING!!'                   
053500                                      TO MOD-TEMFSFEL                     
053600               ELSE                                                       
053700                   MOVE DC-KDGENFRA-DO                                    
053800                                      TO MOD-KDGENFRA-DO(INDX)            
053900               END-IF                                                     
054000           ELSE                                                           
054100               MOVE MFS-ERASE-FIELD   TO MOD-KDGENFRA-DO(INDX)            
054200           END-IF                                                         
054300                                                                          
054400           IF  GMT-IDDC-VOR(INDX) NOT = SPACE                             
054500               MOVE GMT-IDDC-VOR(INDX) TO W-IDDC-301                      
054600                                          W-IDDC-301-DEF                  
054700               PERFORM IMS-GET-GMTB-2                                     
054800               IF  SEGMENT-MISSING                                        
054900                   MOVE MFS-ERASE-FIELD                                   
055000                                      TO MOD-KDGENFRA-VOR(INDX)           
055100                   MOVE 'SOME DC-INFORMATION MISSING!!'                   
055200                                      TO MOD-TEMFSFEL                     
055300               ELSE                                                       
055400                   MOVE DC-KDGENFRA-VOR                                   
055500                                      TO MOD-KDGENFRA-VOR(INDX)           
055600               END-IF                                                     
055700           ELSE                                                           
055800               MOVE MFS-ERASE-FIELD   TO MOD-KDGENFRA-VOR(INDX)           
055900           END-IF                                                         
056000                                                                          
056100           ADD +1 TO INDX                                                 
056200         END-PERFORM                                                      
056300                                                                          
056400         MOVE GMT-TISTADAT   TO TMP1-YYMMDD                               
056500         MOVE TODAYS-DATE    TO TMP2-YYMMDD                               
056600         PERFORM WY2000P1                                                 
056700         IF  TMP1-YYMMDD > TMP2-YYMMDD                                    
056800         OR  GMT-TISTADAT  = ZERO                                         
056900             MOVE 'GOODS RECEIVER NOT READY FOR ORDER'                    
057000                                      TO MOD-TEMFSINF                     
057100         END-IF                                                           
057200                                                                          
057300         MOVE GMT-TISTODAT   TO TMP1-YYMMDD                               
057400         MOVE TODAYS-DATE    TO TMP2-YYMMDD                               
057500         PERFORM WY2000P1                                                 
057600         IF  GMT-TISTODAT > ZERO                                          
057700         AND TMP1-YYMMDD   <= TMP2-YYMMDD                                 
057800             MOVE 'GOODS RECEIVER NOT READY FOR ORDER'                    
057900                                      TO MOD-TEMFSINF                     
058000         END-IF                                                           
058100                                                                          
058200     END-IF                                                               
058300     .                                                                    
058400     EJECT                                                                
058500 FB-CARRIER SECTION.                                                      
058600                                                                          
058700     MOVE DCS-IDDC   TO W-4433-IDDC                                       
058800     MOVE FK-IDTRP-0 TO W-4434-IDTRP                                      
058900*    MOVE ZERO       TO W-4434-IDTRP (4:2)                                
059000     PERFORM IMS-GU-WLXXKB11                                              
059100     IF SEGMENT-FOUND                                                     
059200     AND 4434-IDTRP = W-4434-IDTRP                                        
059300        MOVE 4434-BETRPFIR   TO MOD-BETRPFIR-0                            
059400     ELSE                                                                 
059500        MOVE MFS-ERASE-FIELD TO MOD-BETRPFIR-0                            
059600     END-IF                                                               
059700                                                                          
059800     MOVE FK-IDTRP-1 TO W-4434-IDTRP                                      
059900*    MOVE ZERO       TO W-4434-IDTRP (4:2)                                
060000     PERFORM IMS-GU-WLXXKB11                                              
060100     IF SEGMENT-FOUND                                                     
060200     AND 4434-IDTRP = W-4434-IDTRP                                        
060300        MOVE 4434-BETRPFIR   TO MOD-BETRPFIR-1                            
060400     ELSE                                                                 
060500        MOVE MFS-ERASE-FIELD TO MOD-BETRPFIR-1                            
060600     END-IF                                                               
060700                                                                          
060800     MOVE FK-IDTRP-2 TO W-4434-IDTRP                                      
060900*    MOVE ZERO       TO W-4434-IDTRP (4:2)                                
061000     PERFORM IMS-GU-WLXXKB11                                              
061100     IF SEGMENT-FOUND                                                     
061200     AND 4434-IDTRP = W-4434-IDTRP                                        
061300        MOVE 4434-BETRPFIR   TO MOD-BETRPFIR-2                            
061400     ELSE                                                                 
061500        MOVE MFS-ERASE-FIELD TO MOD-BETRPFIR-2                            
061600     END-IF                                                               
061700                                                                          
061800     MOVE FK-IDTRP-3 TO W-4434-IDTRP                                      
061900*    MOVE ZERO       TO W-4434-IDTRP (4:2)                                
062000     PERFORM IMS-GU-WLXXKB11                                              
062100     IF SEGMENT-FOUND                                                     
062200     AND 4434-IDTRP = W-4434-IDTRP                                        
062300        MOVE 4434-BETRPFIR   TO MOD-BETRPFIR-3                            
062400     ELSE                                                                 
062500        MOVE MFS-ERASE-FIELD TO MOD-BETRPFIR-3                            
062600     END-IF                                                               
062700                                                                          
062800     MOVE FK-IDTRP-4 TO W-4434-IDTRP                                      
062900*    MOVE ZERO       TO W-4434-IDTRP (4:2)                                
063000     PERFORM IMS-GU-WLXXKB11                                              
063100     IF SEGMENT-FOUND                                                     
063200     AND 4434-IDTRP = W-4434-IDTRP                                        
063300        MOVE 4434-BETRPFIR   TO MOD-BETRPFIR-4                            
063400     ELSE                                                                 
063500        MOVE MFS-ERASE-FIELD TO MOD-BETRPFIR-4                            
063600     END-IF                                                               
063700                                                                          
063800*----------------------- LÄS ALTERNATIV FRAKT                             
063900                                                                          
064000     IF  DCS-NDC-NA                                                       
064100       ADD +1       TO W-KDFRAKT-501                                      
064200                       W-KDFRAKT-501-DEF                                  
064300       PERFORM IMS-GET-GMTC                                               
064400       IF  SEGMENT-FOUND                                                  
064500           MOVE FK-IDTRP-0  TO MOD-IDTRP-0-ALT                            
064600           MOVE FK-IDTRP-1  TO MOD-IDTRP-1-ALT                            
064700           MOVE FK-IDTRP-2  TO MOD-IDTRP-2-ALT                            
064800           MOVE FK-IDTRP-3  TO MOD-IDTRP-3-ALT                            
064900           MOVE FK-IDTRP-4  TO MOD-IDTRP-4-ALT                            
065000                                                                          
065100           MOVE FK-IDTRP-0 TO W-4434-IDTRP                                
065200*          MOVE ZERO       TO W-4434-IDTRP (4:2)                          
065300           PERFORM IMS-GU-WLXXKB11                                        
065400           IF SEGMENT-FOUND                                               
065500           AND 4434-IDTRP = W-4434-IDTRP                                  
065600              MOVE 4434-BETRPFIR                                          
065700                             TO MOD-BETRPFIR-0-ALT                        
065800           ELSE                                                           
065900              MOVE MFS-ERASE-FIELD                                        
066000                             TO MOD-BETRPFIR-0-ALT                        
066100           END-IF                                                         
066200                                                                          
066300           MOVE FK-IDTRP-1 TO W-4434-IDTRP                                
066400*          MOVE ZERO       TO W-4434-IDTRP (4:2)                          
066500           PERFORM IMS-GU-WLXXKB11                                        
066600           IF SEGMENT-FOUND                                               
066700           AND 4434-IDTRP = W-4434-IDTRP                                  
066800              MOVE 4434-BETRPFIR                                          
066900                             TO MOD-BETRPFIR-1-ALT                        
067000           ELSE                                                           
067100              MOVE MFS-ERASE-FIELD                                        
067200                             TO MOD-BETRPFIR-1-ALT                        
067300           END-IF                                                         
067400                                                                          
067500           MOVE FK-IDTRP-2 TO W-4434-IDTRP                                
067600*          MOVE ZERO       TO W-4434-IDTRP (4:2)                          
067700           PERFORM IMS-GU-WLXXKB11                                        
067800           IF SEGMENT-FOUND                                               
067900           AND 4434-IDTRP = W-4434-IDTRP                                  
068000              MOVE 4434-BETRPFIR                                          
068100                             TO MOD-BETRPFIR-2-ALT                        
068200           ELSE                                                           
068300              MOVE MFS-ERASE-FIELD                                        
068400                             TO MOD-BETRPFIR-2-ALT                        
068500           END-IF                                                         
068600                                                                          
068700           MOVE FK-IDTRP-3 TO W-4434-IDTRP                                
068800*          MOVE ZERO       TO W-4434-IDTRP (4:2)                          
068900           PERFORM IMS-GU-WLXXKB11                                        
069000           IF SEGMENT-FOUND                                               
069100           AND 4434-IDTRP = W-4434-IDTRP                                  
069200              MOVE 4434-BETRPFIR                                          
069300                             TO MOD-BETRPFIR-3-ALT                        
069400           ELSE                                                           
069500              MOVE MFS-ERASE-FIELD                                        
069600                             TO MOD-BETRPFIR-3-ALT                        
069700           END-IF                                                         
069800                                                                          
069900           MOVE FK-IDTRP-4 TO W-4434-IDTRP                                
070000*          MOVE ZERO       TO W-4434-IDTRP (4:2)                          
070100           PERFORM IMS-GU-WLXXKB11                                        
070200           IF SEGMENT-FOUND                                               
070300           AND 4434-IDTRP = W-4434-IDTRP                                  
070400              MOVE 4434-BETRPFIR                                          
070500                             TO MOD-BETRPFIR-4-ALT                        
070600           ELSE                                                           
070700              MOVE MFS-ERASE-FIELD                                        
070800                             TO MOD-BETRPFIR-4-ALT                        
070900           END-IF                                                         
071000                                                                          
071100       ELSE                                                               
071200           PERFORM MFS-ERASE-FIELD-OUT-FC-ALT                             
071300       END-IF                                                             
071400     END-IF                                                               
071500     .                                                                    
071600     EJECT                                                                
071700 MFS-ERASE-FIELD-OUT SECTION.                                             
071800                                                                          
071900*    --- ALLA UTDATA-FÄLT                                                 
072000     MOVE MFS-ERASE-FIELD TO MOD-BEGMT-RAD1                               
072100                             MOD-BEGMT-RAD2                               
072200                             MOD-ADGMT-GATA                               
072300                             MOD-ADGMT-PADR                               
072400                             MOD-ADGMT-LAND                               
072500                             MOD-KDFDKRAV                                 
072600                             MOD-BEFDKRAV                                 
072700                             MOD-KDBETVIL                                 
072800                             MOD-BEBETVIL-X27                             
072900                             MOD-KDTRPKAT                                 
073000                             MOD-IDTRP-0                                  
073100                             MOD-BETRPFIR-0                               
073200                             MOD-IDTRP-1                                  
073300                             MOD-BETRPFIR-1                               
073400                             MOD-IDTRP-2                                  
073500                             MOD-BETRPFIR-2                               
073600                             MOD-IDTRP-3                                  
073700                             MOD-BETRPFIR-3                               
073800                             MOD-IDTRP-4                                  
073900                             MOD-BETRPFIR-4                               
074000                             MOD-IDTRP-0-ALT                              
074100                             MOD-BETRPFIR-0-ALT                           
074200                             MOD-IDTRP-1-ALT                              
074300                             MOD-BETRPFIR-1-ALT                           
074400                             MOD-IDTRP-2-ALT                              
074500                             MOD-BETRPFIR-2-ALT                           
074600                             MOD-IDTRP-3-ALT                              
074700                             MOD-BETRPFIR-3-ALT                           
074800                             MOD-IDTRP-4-ALT                              
074900                             MOD-BETRPFIR-4-ALT                           
075000     MOVE +1 TO INDX                                                      
075100     PERFORM UNTIL INDX > MAX-INDX                                        
075200       PERFORM MFS-ERASE-LINE-FIELD                                       
075300       ADD +1 TO INDX                                                     
075400     END-PERFORM                                                          
075500     .                                                                    
075600     SKIP3                                                                
075700 MFS-ERASE-LINE-FIELD SECTION.                                            
075800                                                                          
075900     MOVE MFS-ERASE-FIELD TO MOD-IDDC-BULK    (INDX)                      
076000                             MOD-KDGENFRA-MO  (INDX)                      
076100                             MOD-IDDC-DAY     (INDX)                      
076200                             MOD-KDGENFRA-DO  (INDX)                      
076300                             MOD-IDDC-VOR     (INDX)                      
076400                             MOD-KDGENFRA-VOR (INDX)                      
076500     .                                                                    
076600     EJECT                                                                
076700 MFS-ERASE-FIELD-OUT-FC SECTION.                                          
076800                                                                          
076900*    --- DC FIELDS                                                        
077000     MOVE MFS-ERASE-FIELD TO MOD-KDFDKRAV                                 
077100                             MOD-BEFDKRAV                                 
077200                             MOD-KDBETVIL                                 
077300                             MOD-BEBETVIL-X27                             
077400                             MOD-KDTRPKAT                                 
077500                             MOD-IDTRP-0                                  
077600                             MOD-BETRPFIR-0                               
077700                             MOD-IDTRP-1                                  
077800                             MOD-BETRPFIR-1                               
077900                             MOD-IDTRP-2                                  
078000                             MOD-BETRPFIR-2                               
078100                             MOD-IDTRP-3                                  
078200                             MOD-BETRPFIR-3                               
078300                             MOD-IDTRP-4                                  
078400                             MOD-BETRPFIR-4                               
078500                             MOD-IDTRP-0-ALT                              
078600                             MOD-BETRPFIR-0-ALT                           
078700                             MOD-IDTRP-1-ALT                              
078800                             MOD-BETRPFIR-1-ALT                           
078900                             MOD-IDTRP-2-ALT                              
079000                             MOD-BETRPFIR-2-ALT                           
079100                             MOD-IDTRP-3-ALT                              
079200                             MOD-BETRPFIR-3-ALT                           
079300                             MOD-IDTRP-4-ALT                              
079400                             MOD-BETRPFIR-4-ALT                           
079500     .                                                                    
079600     EJECT                                                                
079700 MFS-ERASE-FIELD-OUT-FC-ALT SECTION.                                      
079800                                                                          
079900*    --- DC FIELDS                                                        
080000     MOVE MFS-ERASE-FIELD TO MOD-IDTRP-0-ALT                              
080100                             MOD-BETRPFIR-0-ALT                           
080200                             MOD-IDTRP-1-ALT                              
080300                             MOD-BETRPFIR-1-ALT                           
080400                             MOD-IDTRP-2-ALT                              
080500                             MOD-BETRPFIR-2-ALT                           
080600                             MOD-IDTRP-3-ALT                              
080700                             MOD-BETRPFIR-3-ALT                           
080800                             MOD-IDTRP-4-ALT                              
080900                             MOD-BETRPFIR-4-ALT                           
081000     .                                                                    
081100     EJECT                                                                
081200* --- IMS SECTIONS ---                                                    
081300     SKIP3                                                                
081400 IMS-GET-MSG SECTION.                                                     
081500                                                                          
081600     MOVE '  QC' TO GOOD-STATUSCODES                                      
081700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
081800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
081900     PERFORM IMS-STATUSCHECK                                              
082000     .                                                                    
082100     SKIP3                                                                
082200 IMS-INSERT-MSG SECTION.                                                  
082300                                                                          
082400     IF MSGI-IDLAND-SPR = 'GB'                                            
082500       MOVE 'N' TO MFS-KDHUVOMR                                           
082600     END-IF                                                               
082700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
082800     MOVE SPACE TO GOOD-STATUSCODES                                       
082900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
083000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
083100     PERFORM IMS-STATUSCHECK                                              
083200     .                                                                    
083300     EJECT                                                                
083400 IMS-GET-GMTA SECTION.                                                    
083500                                                                          
083600     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-X ')'                           
083700          DELIMITED BY SIZE INTO SSA1                                     
083800     MOVE '  GE' TO GOOD-STATUSCODES                                      
083900     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-WLGMTA01 SSA1                  
084000     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
084100     PERFORM IMS-STATUSCHECK                                              
084200     .                                                                    
084300     EJECT                                                                
084400 IMS-GET-GMTB SECTION.                                                    
084500                                                                          
084600     STRING 'WLGMTB01(WDB301KY =' W-WDB301KY-X                            
084700                    '+WDB301KY =' W-WDB301KY-DEF ')'                      
084800          DELIMITED BY SIZE INTO SSA1                                     
084900     MOVE '  GE' TO GOOD-STATUSCODES                                      
085000     CALL CBLTDLI USING GU GMTB-PCB DLI-IO-WLGMTB01 SSA1                  
085100     MOVE GMTB-STATUS-CODE TO STATUS-WS                                   
085200     PERFORM IMS-STATUSCHECK                                              
085300     .                                                                    
085400     EJECT                                                                
085500 IMS-GET-GMTB-2 SECTION.                                                  
085600                                                                          
085700     STRING 'WLGMTB01(WDB301KY =' W-WDB301KY-X                            
085800                    '+WDB301KY =' W-WDB301KY-DEF ')'                      
085900          DELIMITED BY SIZE INTO SSA1                                     
086000     MOVE '  GE' TO GOOD-STATUSCODES                                      
086100     CALL CBLTDLI USING GU GMTB-PCB DLI-IO-WLGMTB01 SSA1                  
086200     MOVE GMTB-STATUS-CODE TO STATUS-WS                                   
086300     PERFORM IMS-STATUSCHECK                                              
086400     .                                                                    
086500     EJECT                                                                
086600 IMS-GET-GMTC SECTION.                                                    
086700                                                                          
086800     STRING 'WLGMTC01(WDB501KY =' W-WDB501KY-X                            
086900                    '+WDB501KY =' W-WDB501KY-DEF ')'                      
087000          DELIMITED BY SIZE INTO SSA1                                     
087100     MOVE '  GE' TO GOOD-STATUSCODES                                      
087200     CALL CBLTDLI USING GU GMTC-PCB DLI-IO-WLGMTC01 SSA1                  
087300     MOVE GMTC-STATUS-CODE TO STATUS-WS                                   
087400     PERFORM IMS-STATUSCHECK                                              
087500     .                                                                    
087600     EJECT                                                                
087700 IMS-GU-XXKU-WLXXKU11 SECTION.                                            
087800                                                                          
087900     STRING 'WLXXKU01(WDGXKEY  =' W-4535-IDHTYP-X ')'                     
088000          DELIMITED BY SIZE INTO SSA1                                     
088100     STRING 'WLXXKU11(WDGXKEY  =' W-4536-IDSKYLT-X ')'                    
088200          DELIMITED BY SIZE INTO SSA2                                     
088300     MOVE '  GE' TO GOOD-STATUSCODES                                      
088400     CALL CBLTDLI USING GU XXKU-PCB DLI-IO-WLXXKU11 SSA1 SSA2             
088500     MOVE XXKU-STATUS-CODE TO STATUS-WS                                   
088600     PERFORM IMS-STATUSCHECK                                              
088700     .                                                                    
088800     EJECT                                                                
088900 IMS-GU-WLXXKB11 SECTION.                                                 
089000                                                                          
089100     STRING 'WLXXKB01(WDGXKEY  =' W-WDGXKEY-4433-X ')'                    
089200          DELIMITED BY SIZE INTO SSA1                                     
089300     STRING 'WLXXKB11(WDGXKEY >=' W-WDGXKEY-4434-X ')'                    
089400          DELIMITED BY SIZE INTO SSA2                                     
089500     MOVE '  GE' TO GOOD-STATUSCODES                                      
089600     CALL CBLTDLI USING GU XXKB-PCB DLI-IO-WLXXKB11 SSA1 SSA2             
089700     MOVE XXKB-STATUS-CODE TO STATUS-WS                                   
089800     PERFORM IMS-STATUSCHECK                                              
089900     .                                                                    
090000     EJECT                                                                
090100 IMS-GET-WDB1 SECTION.                                                    
090200                                                                          
090300     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
090400          DELIMITED BY SIZE INTO SSA1                                     
090500     MOVE '  GE' TO GOOD-STATUSCODES                                      
090600     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
090700     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
090800     PERFORM IMS-STATUSCHECK                                              
090900     .                                                                    
091000     EJECT                                                                
091100 IMS-GU-WDB601    SECTION.                                                
091200     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
091300          DELIMITED BY SIZE INTO SSA1                                     
091400     MOVE '  GE' TO GOOD-STATUSCODES                                      
091500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
091600     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
091700     PERFORM IMS-STATUSCHECK                                              
091800     IF SEGMENT-MISSING                                                   
091900         MOVE SPACE TO DCS-KDDC                                           
092000     END-IF                                                               
092100     .                                                                    
092200 IMS-STATUSCHECK SECTION.                                                 
092300                                                                          
092400     SET STATUS-IX TO 1                                                   
092500     SEARCH GOOD-STATUS                                                   
092600       AT END                                                             
092700         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
092800         DELIMITED BY SIZE INTO ERROR-TEXT                                
092900         CALL FELLOG                                                      
093000       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
093100         CONTINUE                                                         
093200     END-SEARCH                                                           
093300     .                                                                    
093400     EJECT                                                                
093500*    -COPY WY2000P1                                                       
