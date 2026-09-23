000100 PROCESS DYNAM                                                            
000200*COMPOPT DB2BIND=YES                                                      
000300 ID DIVISION.                                                             
000400 PROGRAM-ID.     W4040700.                                                
000500 AUTHOR.         ANDERSSON BERT.                                          
000600 DATE-WRITTEN.   11/01/04.                                                
000700 DATE-COMPILED.                                                           
000800                                                                          
000900*                                                                         
001000*                                                                         
001100*    FUNCTION:                                                            
001200*        PARAMETERS FOR DESTOCKING - TRANSFER SETTINGS.                   
001300*                                                                         
001400*        THE PROGRAM UPDATES   WDB6                                       
001500*        THE PROGRAM UPDATES   WDP7                                       
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSACTION: W4T407                                              
001900*        MID:         W4I40701                                            
002000*                                                                         
002100*    OUTDATA.                                                             
002200*        MOD:         W4O40701                                            
002210*                                                                         
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600                                                                          
002700 DATA DIVISION.                                                           
002800     EJECT                                                                
002900 WORKING-STORAGE SECTION.                                                 
003000 77  IDPGM                       PIC X(08)   VALUE 'W4040700'.            
003100 77  FILLER                      PIC X(08)   VALUE 'ERRORTEX'.            
003200 77  ERRORTEXT                   PIC X(40)   VALUE SPACE.                 
003300 77  FILLER                      PIC X(08)   VALUE 'CURRENT:'.            
003400 77  WS-CURRENT-SECTION          PIC X(24)   VALUE SPACE.                 
003500 77  FILLER                      PIC X(08)   VALUE 'IMS-SEC:'.            
003600 77  WS-CURRENT-IMS-SECTION      PIC X(24)   VALUE SPACE.                 
003700 77  WS-TID-TRS                  PIC S9(1)   VALUE ZERO.                  
003800 77  WS-TID-TRS-9                PIC  9(1)   VALUE ZERO.                  
003900 77  WS-DCS-TID-TRS              PIC S9(1)   VALUE ZERO.                  
004000 77  WS-IDDC-RET                 PIC X(02)   VALUE SPACE.                 
004100 77  W-IDDC-TPAS                 PIC X(02)   VALUE SPACE.                 
004200 77  FILLER                      PIC X(08)   VALUE 'KVPB-LIM'.            
004300 77  WS-KVPB-LIM                 PIC 9(6)V9  VALUE ZERO.                  
004400 77  WS-RED-KVPB-LIM             PIC Z(5)9.9 VALUE ZERO.                  
004500 77  WS-DCS-FLRETUR-PAS          PIC X(01)   VALUE SPACE.                 
004600 77  WS-DCS-FLTRANS-PAS          PIC X(01)   VALUE SPACE.                 
004700 77  WS-DCS-FLTRANS-ERS          PIC X(01)   VALUE SPACE.                 
004800 77  WS-DCS-IDDC-TPAS-1          PIC X(02)   VALUE SPACE.                 
004900 77  WS-DCS-IDDC-TPAS-2          PIC X(02)   VALUE SPACE.                 
005000 77  WS-DCS-IDDC-TPAS-3          PIC X(02)   VALUE SPACE.                 
005100 77  WS-DCS-SUARTMIN-RPAS        PIC 9(06)   VALUE ZERO.                  
005200 77  WS-DCS-KVVECKOR-RPAS        PIC 9(02)   VALUE ZERO.                  
005300 77  WS-DCS-KVVECKOR-TPAS        PIC 9(02)   VALUE ZERO.                  
005400 77  WS-DCS-FLSKROT-PAS          PIC X(02)   VALUE ZERO.                  
005600 77  WS-DCS-KVPERIOD-TPAS        PIC 9(03)   VALUE ZERO.                  
005700 77  WS-DCS-SUARTMIN-TPAS        PIC 9(06)   VALUE ZERO.                  
005800 77  WS-DCS-KDDC                 PIC X(02)   VALUE SPACE.                 
005801 77  WS-DCS-KVSKROT-SPAS         PIC 9(03)   VALUE ZERO.                  
005802 77  WS-DCS-KVVECKOR-SPAS        PIC 9(03)   VALUE ZERO.                  
005803 77  WS-DCS-IDTECKEN-SPAS        PIC X(01)   VALUE SPACE.                 
005804 77  WS-DCS-PRARTSTD-SPAS        PIC 9(07)   VALUE ZERO.                  
005805 77  WS-DCS-ADLAGOMR-SPAS        PIC 9(03)   VALUE ZERO.                  
005806 77  WS-DCS-IDPERSON-SPAS        PIC 9(03)   VALUE ZERO.                  
005807 77  WS-DCS-KDPRODSL-SPAS        PIC 9(03)   VALUE ZERO.                  
005810 77  WS-KVVECKOR-BIN             PIC X(03)   VALUE SPACE.                 
005900*                                                                         
006000*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
006100 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
006200                                                                          
006300 77  YES                         PIC X       VALUE 'Y'.                   
006400 77  NOO                         PIC X       VALUE 'N'.                   
006500 77  JA                          PIC X       VALUE 'J'.                   
006600 77  NEJ                         PIC X       VALUE 'N'.                   
006700                                                                          
006800*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
006900                                                                          
007000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
007100     88  INDATA-OK                           VALUE 'J'.                   
007200     88  INDATA-WRONG                        VALUE 'N'.                   
007300                                                                          
007400 77  KEYS-SW                     PIC X       VALUE 'J'.                   
007500     88  KEYS-OK                             VALUE 'J'.                   
007600     88  KEYS-WRONG                          VALUE 'N'.                   
007700                                                                          
007800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007900     88  OWN-MID                             VALUE '4407'.                
008000     88  GOOD-MID                            VALUE '4407'.                
008100     88  HELP-MID                            VALUE '0551'.                
008200                                                                          
008300 77  WS-VALID-TIVV-SW            PIC X(2)    VALUE SPACE.                 
008400     88 VALID-TIVV                           VALUE                        
008500                                             '00' THRU '52'.              
008600     EJECT                                                                
008700                                                                          
008802 77  WS-VALID-KVVECKOR-SW        PIC X(3)    VALUE SPACE.                 
008900     88 VALID-KVVECKOR                       VALUE                        
009002                                             '000' THRU '260'.            
009100     EJECT                                                                
009200                                                                          
009300 01 NYCKLAR-TP4TRAN.                                                      
009400     03 WS-IDDC-SEND             PIC X(2)    VALUE SPACE.                 
009500     03 WS-IDDC-REC              PIC X(2)    VALUE SPACE.                 
009600*                                                                         
009700*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
009800 01  GENERAL-SUBPROGRAMS.                                                 
009900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
010000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
010100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
010400     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
010500*    --- PARAMETRAR TILL ABEND                                            
010600                                                                          
010700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
010900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
011000     SKIP2                                                                
011100*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
011200*01 -COPY WMEDAREA                                                        
011300     SKIP3                                                                
011400*    --- COPYTEXT TILL SUBPROGRAM WDECEDIT                                
011500*01  -COPY WDECAREA                                                       
011600     SKIP3                                                                
011700 01  MESSAGE-CODES.                                                       
011800     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
011900     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
012000     03  ERR-NOT-NUMERIC         PIC X(3)    VALUE '020'.                 
012100     03  ERR-DC-MISSING          PIC X(3)    VALUE '026'.                 
012200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
012300     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
012400     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
012500*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
012600     EJECT                                                                
012700*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
012800*                                                                         
012900 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
013000     SKIP3                                                                
013100*01 -COPY WMSGINIT                                                        
013200     EJECT                                                                
013300*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
013400*                                                                         
013500 01  SAVE-AREA.                                                           
013600     03  SAVE-IDTRANS           PIC X(4)    VALUE '4407'.                 
013700     EJECT                                                                
013800*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
013900*                                                                         
014000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
014100     SKIP3                                                                
014200*01  MID -COPY W4I40701                                                   
014300     EJECT                                                                
014400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
014500     SKIP3                                                                
014600*01  -COPY WMSGAREA                                                       
014700     EJECT                                                                
014800     03  MOD REDEFINES MSG-AREA.                                          
014900*      05  -COPY W4O40701                                                 
015000     EJECT                                                                
015100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
015200     SKIP3                                                                
015300*01  -COPY WMFSAREA                                                       
015400     EJECT                                                                
015500*    --- WORK-AREAS FOR IMS-SECTIONS                                      
015600*                                                                         
015700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015800     SKIP3                                                                
015900 01  KEYS-FOR-DLI.                                                        
016000     03  W-IDDC-X.                                                        
016100         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
016200     03  W-IDUSER-X.                                                      
016300         05  W-IDUSER            PIC X(8)    VALUE SPACE.                 
016400     SKIP2                                                                
016500*    --- STATUS CODES FROM IMS                                            
016600 01  STATUS-WS                   PIC XX.                                  
016700     88  SEGMENT-FOUND                       VALUE '  '.                  
016800     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
016900     88  SEGMENT-MISSING                     VALUE 'GE'.                  
017000     SKIP2                                                                
017100 01  GOOD-STATUSCODES.                                                    
017200     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017300     SKIP3                                                                
017400 01  SSA1                        PIC X(64).                               
017500 01  SSA2                        PIC X(64).                               
017600     EJECT                                                                
017700********DB2                                                               
017800 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
017900       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
018000                                                                          
018100 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
018200 01  DB2-WS.                                                              
018300     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
018400         88  CURSOR-OK                      VALUE 000.                    
018500         88  LINES-FOUND                    VALUE 000.                    
018600         88  LINES-MISSING                  VALUE 100.                    
018700         88  RESOURCE-WRONG                 VALUE 904.                    
018800     03  GOOD-SQLCODECODES.                                               
018900         05  GOOD-SQLCODE OCCURS 5                                        
019000             INDEXED BY SQLCODE-IX PIC 9(3).                              
019100 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
019200     EJECT                                                                
019300*******************************************                               
019400*    --- IMS FUNCTION CODES                                               
019500*01  -COPY W0003                                                          
019600     EJECT                                                                
019700*    ---  DLI INPUT-OUTPUT AREA                                           
019800                                                                          
019900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
020000 01  DLI-IO-WDB601.                                                       
020100*    03  -COPY WDB601                                                     
020720                                                                          
020730                                                                          
020800 01  FILLER                      PIC X(16)  VALUE 'TP4TRAN-AREA'.         
020900                                                                          
021000*01  -COPY TP4TRAN -PRE TP4TRAN-                                          
021100*                                                                         
021200     EXEC SQL INCLUDE TP4TRAN END-EXEC.                                   
021300*                                                                         
021400                                                                          
021500*******************************************                               
021600 LINKAGE SECTION.                                                         
021700*01  -COPY W0009   -PRE MSG-                                              
021800*01  -COPY W0008   -PRE WDP7-                                             
021900     05  FILLER                  PIC X.                                   
022000                                                                          
022100*01  -COPY W0008  -PRE WDB6-                                              
022200     05  FILLER                  PIC X.                                   
022300     EJECT                                                                
022400                                                                          
022500 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDB6-PCB.                     
022600 MAIN SECTION.                                                            
022700     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDB6-PCB.                     
022800                                                                          
022900     PERFORM IMS-GET-MSG                                                  
023000     IF SEGMENT-FOUND                                                     
023100       PERFORM A-INIT                                                     
023200       PERFORM B-CHECK-KEYS                                               
023300       IF KEYS-OK                                                         
023400         IF MFS-UPDATE                                                    
023500           PERFORM G-CHECK-INPUT                                          
023600           IF INDATA-OK                                                   
023700             PERFORM H-UPDATE                                             
023800           ELSE                                                           
023900             PERFORM EA-MID-INDATA-TO-MOD                                 
024000           END-IF                                                         
024100         ELSE                                                             
024200           IF MFS-FIRST                                                   
024300             PERFORM C-FIRST-PAGE                                         
024400           ELSE                                                           
024500             PERFORM E-SAME-PAGE                                          
024600           END-IF                                                         
024700         END-IF                                                           
024800         IF INDATA-OK                                                     
024900           PERFORM F-READ-SHOW-INFO                                       
025000         END-IF                                                           
025100       END-IF                                                             
025200       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O40701 + 4                      
025300       PERFORM IMS-INSERT-MSG                                             
025400     END-IF                                                               
025500**                                                                        
025600     MOVE ZERO TO RETURN-CODE                                             
025700     GOBACK                                                               
025800     .                                                                    
025900     EJECT                                                                
026000 A-INIT SECTION.                                                          
026100     MOVE 'A-INIT         '   TO WS-CURRENT-SECTION                       
026200                                                                          
026300     IF MSG-DOUBLE-TRANSACTIONS                                           
026400       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W4I40701                 
026500       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
026600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
026700     ELSE                                                                 
026800       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W4I40701                  
026900       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
027000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
027100     END-IF                                                               
027200                                                                          
027300     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
027400     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
027500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
027600     MOVE JA   TO INDATA-SW                                               
027700                                                                          
027800     MOVE LOW-VALUE TO MSG-AREA                                           
027900     MOVE 'W4O407N1' TO MFS-IDMOD                                         
028000     MOVE '4407' TO MOD-IDTRANS                                           
028100     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
028200                                                                          
028300     IF OWN-MID OR HELP-MID                                               
028400       CONTINUE                                                           
028500     ELSE                                                                 
028600       MOVE SPACE TO MFS-KDTRTYP                                          
028700       MOVE '7' TO MFS-IDPFK                                              
028800     END-IF                                                               
028900     .                                                                    
029000     EJECT                                                                
029100 B-CHECK-KEYS SECTION.                                                    
029200     MOVE 'B-CHECK-KEYS   '   TO WS-CURRENT-SECTION                       
029300                                                                          
029400     MOVE ALL '+'           TO MSGI-WMSGINIT                              
029500     MOVE '001'             TO MSGI-KDCALL                                
029600     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
029700     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
029800     MOVE '4407'            TO MSGI-IDTRANS                               
029900     MOVE JA  TO KEYS-SW                                                  
030000                                                                          
030100     MOVE MID-IDDC-IN        TO MSGI-IDDC-KEY                             
030200     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
030300                                                                          
030400     MOVE MSGI-SPAR-AREA TO SAVE-AREA                                     
030500                                                                          
030600*    - LANGUAGE TO BE USED BY MEDKONV                                     
030700     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
030800                                                                          
030900*    -- CHECK OF IDDC                                                     
031000     MOVE MFS-ERASE-FIELD TO MOD-IDDC-IN                                  
031100                                                                          
031200     IF MID-IDDC-IN NOT = ALL '+'                                         
031300       MOVE '7'         TO MFS-IDPFK                                      
031400       MOVE SPACE       TO MFS-KDTRTYP                                    
031500     END-IF                                                               
031600     MOVE MSGI-IDDC-KEY TO W-IDDC                                         
031700                           MOD-IDDC-UT                                    
031800                                                                          
031900     PERFORM IMS-GU-WDB601                                                
032000                                                                          
032100     IF SEGMENT-MISSING                                                   
032200       MOVE NEJ         TO KEYS-SW                                        
032300       MOVE ERR-DC-MISSING TO MED-IDMFSFEL                                
032400       CALL WMEDKONV USING MED-WMEDAREA                                   
032500       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
032600       PERFORM MFS-ERASE-FIELD-OUT                                        
032700     ELSE                                                                 
032800                                                                          
032900       IF DCS-SDC                                                         
033000       OR DCS-NDC                                                         
033100                                                                          
033200         MOVE JA        TO KEYS-SW                                        
033300                                                                          
033400         MOVE DCS-TID-TRS       TO WS-DCS-TID-TRS                         
033500                                                                          
033600         MOVE DCS-FLRETUR-PAS   TO WS-DCS-FLRETUR-PAS                     
033700         MOVE DCS-FLTRANS-PAS   TO WS-DCS-FLTRANS-PAS                     
033800         MOVE DCS-FLTRANS-ERS   TO WS-DCS-FLTRANS-ERS                     
033900                                                                          
034000         MOVE DCS-FLSKROT-PAS   TO WS-DCS-FLSKROT-PAS                     
034100         MOVE DCS-IDDC-TPAS-1   TO WS-DCS-IDDC-TPAS-1                     
034200         MOVE DCS-IDDC-TPAS-2   TO WS-DCS-IDDC-TPAS-2                     
034300         MOVE DCS-IDDC-TPAS-3   TO WS-DCS-IDDC-TPAS-3                     
034400                                                                          
034500         MOVE DCS-SUARTMIN-RPAS TO WS-DCS-SUARTMIN-RPAS                   
034600         MOVE DCS-KVVECKOR-RPAS TO WS-DCS-KVVECKOR-RPAS                   
034700         MOVE DCS-KVSKROT-SPAS  TO WS-DCS-KVSKROT-SPAS                    
034800                                                                          
034900         MOVE DCS-KVVECKOR-TPAS TO WS-DCS-KVVECKOR-TPAS                   
035000         MOVE DCS-KVPERIOD-TPAS TO WS-DCS-KVPERIOD-TPAS                   
035100         MOVE DCS-SUARTMIN-TPAS TO WS-DCS-SUARTMIN-TPAS                   
035200                                                                          
035220         MOVE DCS-KVVECKOR-SPAS TO WS-DCS-KVVECKOR-SPAS                   
035260         MOVE DCS-IDTECKEN-SPAS TO WS-DCS-IDTECKEN-SPAS                   
035292         MOVE DCS-PRARTSTD-SPAS TO WS-DCS-PRARTSTD-SPAS                   
035297         MOVE DCS-ADLAGOMR-SPAS TO WS-DCS-ADLAGOMR-SPAS                   
035302         MOVE DCS-IDPERSON-SPAS TO WS-DCS-IDPERSON-SPAS                   
035307         MOVE DCS-KDPRODSL-SPAS TO WS-DCS-KDPRODSL-SPAS                   
035320         MOVE DCS-KDDC          TO WS-DCS-KDDC                            
035400                                                                          
035500       ELSE                                                               
035600         MOVE NEJ       TO KEYS-SW                                        
035700                                                                          
035800         MOVE 'ONLY DC TYPE S, NP OR NA ALLOWED' TO MOD-TEMFSFEL          
035900       END-IF                                                             
036000     END-IF                                                               
036001                                                                          
036010     IF MID-KVVECKOR-IN NOT = ALL '+'                                     
036011        MOVE  MID-KVVECKOR-IN   TO WS-KVVECKOR-BIN                        
036020        INSPECT WS-KVVECKOR-BIN REPLACING LEADING SPACES BY ZERO          
036031                                                                          
036032        MOVE WS-KVVECKOR-BIN    TO MID-KVVECKOR-IN                        
036040     END-IF                                                               
036100                                                                          
036200     IF GOOD-MID OR KEYS-OK                                               
036300       CONTINUE                                                           
036400     ELSE                                                                 
036500       MOVE NEJ             TO KEYS-SW                                    
036600       MOVE MFS-ERASE-FIELD TO MOD-IDDC-UT                                
036700     END-IF                                                               
036800                                                                          
036900     IF KEYS-WRONG                                                        
037000                                                                          
037100         MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                               
037200         CALL WMEDKONV USING MED-WMEDAREA                                 
037300         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
037400         PERFORM MFS-ERASE-FIELD-IN                                       
037500         PERFORM MFS-ERASE-FIELD-OUT                                      
037600                                                                          
037700     END-IF                                                               
037800     .                                                                    
037900     EJECT                                                                
038000 C-FIRST-PAGE SECTION.                                                    
038100     MOVE 'C-FIRST-PAGE'      TO WS-CURRENT-SECTION                       
038200                                                                          
038300     PERFORM MFS-ERASE-FIELD-IN                                           
038400     .                                                                    
038500     EJECT                                                                
038600 E-SAME-PAGE SECTION.                                                     
038700     MOVE 'E-SAME-PAGE '      TO WS-CURRENT-SECTION                       
038800                                                                          
038900     IF OWN-MID OR HELP-MID                                               
039000                                                                          
039100       IF MID-TID-TRS        = ALL '+'                                    
039200       AND MID-TIVV          = ALL '+'                                    
039300       AND MID-KVVECKOR-IN   = ALL '+'                                    
039400       AND MID-FLTRANS-PAS   = ALL '+'                                    
039500       AND MID-IDDC-TPAS-1   = ALL '+'                                    
039600       AND MID-IDDC-TPAS-2   = ALL '+'                                    
039700       AND MID-IDDC-TPAS-3   = ALL '+'                                    
039800       AND MID-SUARTMIN-TPAS = ALL '+'                                    
039900       AND MID-KVPERIOD-TPAS = ALL '+'                                    
040000       AND MID-KVVECKOR-TPAS = ALL '+'                                    
040100       AND MID-FLTRANS-ERS   = ALL '+'                                    
040200       AND MID-KVPB-LIM      = ALL '+'                                    
040300       AND MID-SUVARLIM-TPAS = ALL '+'                                    
040400       AND MID-FLRETUR-PAS   = ALL '+'                                    
040500       AND MID-SUARTMIN-RPAS = ALL '+'                                    
040600       AND MID-KVVECKOR-RPAS = ALL '+'                                    
040700       AND MID-FLSKROT-PAS   = ALL '+'                                    
040800       AND MID-KVSKROT-SPAS  = ALL '+'                                    
040810       AND MID-KVVECKOR-SPAS = ALL '+'                                    
040820       AND MID-IDTECKEN-SPAS = ALL '+'                                    
040821       AND MID-PRARTSTD-SPAS = ALL '+'                                    
040830       AND MID-ADLAGOMR-SPAS = ALL '+'                                    
040840       AND MID-IDPERSON-SPAS = ALL '+'                                    
040850       AND MID-KDPRODSL-SPAS = ALL '+'                                    
040900                                                                          
041000         PERFORM MFS-ERASE-FIELD-IN                                       
041100       ELSE                                                               
041200         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
041300         CALL WMEDKONV USING MED-WMEDAREA                                 
041400         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
041500         PERFORM EA-MID-INDATA-TO-MOD                                     
041600       END-IF                                                             
041700     ELSE                                                                 
041800       PERFORM MFS-ERASE-FIELD-IN                                         
041900     END-IF                                                               
042000     .                                                                    
042100     EJECT                                                                
042200 EA-MID-INDATA-TO-MOD SECTION.                                            
042300     MOVE 'EA-MID-INDATA-TO-MOD'       TO WS-CURRENT-SECTION              
042400                                                                          
042500* * * * * FOR EVERY MID-FIELD                                             
042600* * * * * IF MID-FIELD NOT = ALL '+' MOVE MID-FIELD TILL MOD-INPUT        
042700* * * * *        MOVE MFS-ADD-READ-FIELD TO MOD-INDATA-ATTR               
042800* * * * * ELSE ERASE MOD-INPUT-FIELD                                      
042900                                                                          
043000*TRANSFER                                                                 
043100     IF MID-TID-TRS = ALL '+'                                             
043200       MOVE MFS-ERASE-FIELD            TO MOD-TID-TRS-IN                  
043300     ELSE                                                                 
043400       MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-TID-TRS-IN                  
043500       MOVE MFS-ADD-READ-FIELD         TO MOD-TID-TRS-IN-ATTR             
043600     END-IF                                                               
043700                                                                          
043800     IF MID-TIVV = ALL '+'                                                
043900       MOVE MFS-ERASE-FIELD            TO MOD-TIVV-IN                     
044000     ELSE                                                                 
044100       MOVE MID-TIVV                   TO MOD-TIVV-IN                     
044200       MOVE MFS-ADD-READ-FIELD         TO MOD-TIVV-IN-ATTR                
044300     END-IF                                                               
044400                                                                          
044500     IF MID-KVVECKOR-IN = ALL '+'                                         
044600       MOVE MFS-ERASE-FIELD            TO MOD-KVVECKOR-IN                 
044700     ELSE                                                                 
044800       MOVE MID-KVVECKOR-IN            TO MOD-KVVECKOR-IN                 
044900       MOVE MFS-ADD-READ-FIELD         TO MOD-KVVECKOR-IN-ATTR            
045000     END-IF                                                               
045100                                                                          
045200     IF MID-FLTRANS-PAS = ALL '+'                                         
045300       MOVE MFS-ERASE-FIELD            TO MOD-FLTRANS-PAS-IN              
045400     ELSE                                                                 
045500       MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-FLTRANS-PAS-IN              
045600       MOVE MFS-ADD-READ-FIELD         TO MOD-FLTRANS-PAS-IN-ATTR         
045700     END-IF                                                               
045800                                                                          
045900     IF MID-IDDC-TPAS-1 = ALL '+'                                         
046000       MOVE MFS-ERASE-FIELD            TO MOD-IDDC-TPAS-1-IN              
046100     ELSE                                                                 
046200       MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-IDDC-TPAS-1-IN              
046300       MOVE MFS-ADD-READ-FIELD    TO MOD-IDDC-TPAS-1-IN-ATTR              
046400     END-IF                                                               
046500                                                                          
046600     IF MID-IDDC-TPAS-2 = ALL '+'                                         
046700       MOVE MFS-ERASE-FIELD            TO MOD-IDDC-TPAS-2-IN              
046800     ELSE                                                                 
046900       MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-IDDC-TPAS-2-IN              
047000       MOVE MFS-ADD-READ-FIELD         TO MOD-IDDC-TPAS-2-IN-ATTR         
047100     END-IF                                                               
047200                                                                          
047300     IF MID-IDDC-TPAS-3 = ALL '+'                                         
047400       MOVE MFS-ERASE-FIELD            TO MOD-IDDC-TPAS-3-IN              
047500     ELSE                                                                 
047600       MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-IDDC-TPAS-3-IN              
047700       MOVE MFS-ADD-READ-FIELD         TO MOD-IDDC-TPAS-3-IN-ATTR         
047800     END-IF                                                               
047900                                                                          
048000     IF MID-SUARTMIN-TPAS = ALL '+'                                       
048100       MOVE MFS-ERASE-FIELD            TO MOD-SUARTMIN-TPAS-IN            
048200     ELSE                                                                 
048300       MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-SUARTMIN-TPAS-IN                
048400       MOVE MFS-ADD-READ-FIELD       TO MOD-SUARTMIN-TPAS-IN-ATTR         
048500     END-IF                                                               
048600                                                                          
048700     IF MID-KVPERIOD-TPAS = ALL '+'                                       
048800       MOVE MFS-ERASE-FIELD            TO MOD-KVPERIOD-TPAS-IN            
048900     ELSE                                                                 
049000       MOVE MFS-DO-NOT-TOUCH-FIELD  TO MOD-KVPERIOD-TPAS-IN               
049100       MOVE MFS-ADD-READ-FIELD      TO MOD-KVPERIOD-TPAS-IN-ATTR          
049200     END-IF                                                               
049300                                                                          
049400     IF MID-KVVECKOR-TPAS = ALL '+'                                       
049500       MOVE MFS-ERASE-FIELD            TO MOD-KVVECKOR-TPAS-IN            
049600     ELSE                                                                 
049700       MOVE MFS-DO-NOT-TOUCH-FIELD    TO MOD-KVVECKOR-TPAS-IN             
049800       MOVE MFS-ADD-READ-FIELD      TO MOD-KVVECKOR-TPAS-IN-ATTR          
049900     END-IF                                                               
050000                                                                          
050100     IF MID-FLTRANS-ERS = ALL '+'                                         
050200       MOVE MFS-ERASE-FIELD            TO MOD-FLTRANS-ERS-IN              
050300     ELSE                                                                 
050400       MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-FLTRANS-ERS-IN              
050500       MOVE MFS-ADD-READ-FIELD         TO MOD-FLTRANS-ERS-IN-ATTR         
050600     END-IF                                                               
050700                                                                          
050800     IF MID-KVPB-LIM = ALL '+'                                            
050900       MOVE MFS-ERASE-FIELD            TO MOD-KVPB-LIM-IN                 
051000     ELSE                                                                 
051100       MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-KVPB-LIM-IN                 
051200       MOVE MFS-ADD-READ-FIELD         TO MOD-KVPB-LIM-IN-ATTR            
051300     END-IF                                                               
051400                                                                          
051500     IF MID-SUVARLIM-TPAS = ALL '+'                                       
051600       MOVE MFS-ERASE-FIELD            TO MOD-SUVARLIM-TPAS-IN            
051700     ELSE                                                                 
051800       MOVE MFS-DO-NOT-TOUCH-FIELD    TO MOD-SUVARLIM-TPAS-IN             
051900       MOVE MFS-ADD-READ-FIELD       TO MOD-SUVARLIM-TPAS-IN-ATTR         
052000     END-IF                                                               
052100*RETURN                                                                   
052200     IF MID-FLRETUR-PAS = ALL '+'                                         
052300       MOVE MFS-ERASE-FIELD            TO MOD-FLRETUR-PAS-IN              
052400     ELSE                                                                 
052500       MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-FLRETUR-PAS-IN              
052600       MOVE MFS-ADD-READ-FIELD       TO MOD-FLRETUR-PAS-IN-ATTR           
052700     END-IF                                                               
052800                                                                          
052900     IF MID-SUARTMIN-RPAS = ALL '+'                                       
053000       MOVE MFS-ERASE-FIELD            TO MOD-SUARTMIN-RPAS-IN            
053100     ELSE                                                                 
053200       MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-SUARTMIN-RPAS-IN            
053300       MOVE MFS-ADD-READ-FIELD       TO MOD-SUARTMIN-RPAS-IN-ATTR         
053400     END-IF                                                               
053500                                                                          
053600     IF MID-KVVECKOR-RPAS = ALL '+'                                       
053700       MOVE MFS-ERASE-FIELD            TO MOD-KVVECKOR-RPAS-IN            
053800     ELSE                                                                 
053900       MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-KVVECKOR-RPAS-IN            
054000       MOVE MFS-ADD-READ-FIELD       TO MOD-KVVECKOR-RPAS-IN-ATTR         
054100     END-IF                                                               
054200*SCRAP                                                                    
054300     IF MID-FLSKROT-PAS = ALL '+'                                         
054400       MOVE MFS-ERASE-FIELD            TO MOD-FLSKROT-PAS-IN              
054500     ELSE                                                                 
054600       MOVE MFS-DO-NOT-TOUCH-FIELD    TO MOD-FLSKROT-PAS-IN               
054700       MOVE MFS-ADD-READ-FIELD         TO MOD-FLSKROT-PAS-IN-ATTR         
054800     END-IF                                                               
054900                                                                          
055000     IF MID-KVSKROT-SPAS = ALL '+'                                        
055100       MOVE MFS-ERASE-FIELD            TO MOD-KVSKROT-SPAS-IN             
055200     ELSE                                                                 
055300       MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-KVSKROT-SPAS-IN             
055400       MOVE MFS-ADD-READ-FIELD         TO MOD-KVSKROT-SPAS-IN-ATTR        
055500     END-IF                                                               
055501                                                                          
055502     IF MID-KVVECKOR-SPAS = ALL '+'                                       
055503       MOVE MFS-ERASE-FIELD           TO MOD-KVVECKOR-SPAS-IN             
055504     ELSE                                                                 
055505       MOVE MFS-DO-NOT-TOUCH-FIELD    TO MOD-KVVECKOR-SPAS-IN             
055506       MOVE MFS-ADD-READ-FIELD        TO MOD-KVVECKOR-SPAS-IN-ATTR        
055508     END-IF                                                               
055509                                                                          
055510     IF MID-IDTECKEN-SPAS = ALL '+'                                       
055511       MOVE MFS-ERASE-FIELD           TO MOD-IDTECKEN-SPAS-IN             
055512     ELSE                                                                 
055513       MOVE MFS-DO-NOT-TOUCH-FIELD    TO MOD-IDTECKEN-SPAS-IN             
055514       MOVE MFS-ADD-READ-FIELD        TO MOD-IDTECKEN-SPAS-IN-ATTR        
055515     END-IF                                                               
055516                                                                          
055517     IF MID-PRARTSTD-SPAS = ALL '+'                                       
055518       MOVE MFS-ERASE-FIELD           TO MOD-PRARTSTD-SPAS-IN             
055519     ELSE                                                                 
055520       MOVE MFS-DO-NOT-TOUCH-FIELD    TO MOD-PRARTSTD-SPAS-IN             
055521       MOVE MFS-ADD-READ-FIELD        TO MOD-PRARTSTD-SPAS-IN-ATTR        
055522     END-IF                                                               
055523                                                                          
055524     IF MID-ADLAGOMR-SPAS = ALL '+'                                       
055525       MOVE MFS-ERASE-FIELD           TO MOD-ADLAGOMR-SPAS-IN             
055526     ELSE                                                                 
055527       MOVE MFS-DO-NOT-TOUCH-FIELD    TO MOD-ADLAGOMR-SPAS-IN             
055528       MOVE MFS-ADD-READ-FIELD        TO MOD-ADLAGOMR-SPAS-IN-ATTR        
055529     END-IF                                                               
055530                                                                          
055531     IF MID-IDPERSON-SPAS = ALL '+'                                       
055532       MOVE MFS-ERASE-FIELD           TO MOD-IDPERSON-SPAS-IN             
055533     ELSE                                                                 
055534       MOVE MFS-DO-NOT-TOUCH-FIELD    TO MOD-IDPERSON-SPAS-IN             
055535       MOVE MFS-ADD-READ-FIELD        TO MOD-IDPERSON-SPAS-IN-ATTR        
055536     END-IF                                                               
055537                                                                          
055538     IF MID-KDPRODSL-SPAS = ALL '+'                                       
055539       MOVE MFS-ERASE-FIELD           TO MOD-KDPRODSL-SPAS-IN             
055540     ELSE                                                                 
055541       MOVE MFS-DO-NOT-TOUCH-FIELD    TO MOD-KDPRODSL-SPAS-IN             
055542       MOVE MFS-ADD-READ-FIELD        TO MOD-KDPRODSL-SPAS-IN-ATTR        
055543     END-IF                                                               
055600     .                                                                    
055700     EJECT                                                                
055800 F-READ-SHOW-INFO SECTION.                                                
055900     MOVE 'F-READ-SHOW-INFO  '         TO WS-CURRENT-SECTION              
056000                                                                          
056100     MOVE MSGI-IDDC-KEY TO W-IDDC                                         
056200     PERFORM IMS-GU-WDB601                                                
056300                                                                          
056400     IF SEGMENT-MISSING                                                   
056500       MOVE ERR-DC-MISSING TO MED-IDMFSFEL                                
056600       CALL WMEDKONV USING MED-WMEDAREA                                   
056700       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
056800       PERFORM MFS-ERASE-FIELD-OUT                                        
056900     ELSE                                                                 
057000                                                                          
057100       EVALUATE DCS-TID-TRS                                               
057200         WHEN +0                                                          
057300           MOVE NEJ             TO MOD-TID-TRS                            
057400         WHEN +7                                                          
057500           MOVE 'MO'            TO MOD-TID-TRS                            
057600         WHEN +2                                                          
057700           MOVE 'TU'            TO MOD-TID-TRS                            
057800         WHEN +3                                                          
057900           MOVE 'WE'            TO MOD-TID-TRS                            
058000         WHEN +4                                                          
058100           MOVE 'TH'            TO MOD-TID-TRS                            
058200         WHEN +5                                                          
058300           MOVE 'FR'            TO MOD-TID-TRS                            
058400       END-EVALUATE                                                       
058500                                                                          
058600       MOVE DCS-TIVV              TO MOD-TIVV                             
058700                                                                          
058801       MOVE DCS-KVVECKOR-BIN      TO MOD-KVVECKOR-UT                      
058900                                                                          
059000       IF DCS-FLTRANS-PAS = JA                                            
059100         MOVE YES                 TO MOD-FLTRANS-PAS                      
059200       ELSE                                                               
059300         MOVE DCS-FLTRANS-PAS     TO MOD-FLTRANS-PAS                      
059400       END-IF                                                             
059500                                                                          
059600       MOVE DCS-IDDC-TPAS-1       TO MOD-IDDC-TPAS-1                      
059700       MOVE DCS-IDDC-TPAS-2       TO MOD-IDDC-TPAS-2                      
059800       MOVE DCS-IDDC-TPAS-3       TO MOD-IDDC-TPAS-3                      
059900       MOVE DCS-SUARTMIN-TPAS     TO MOD-SUARTMIN-TPAS                    
060000       MOVE DCS-KVPERIOD-TPAS     TO MOD-KVPERIOD-TPAS                    
060100       MOVE DCS-KVVECKOR-TPAS     TO MOD-KVVECKOR-TPAS                    
060200                                                                          
060300       IF DCS-FLTRANS-ERS = JA                                            
060400         MOVE YES                 TO MOD-FLTRANS-ERS                      
060500       ELSE                                                               
060600         MOVE DCS-FLTRANS-ERS     TO MOD-FLTRANS-ERS                      
060700       END-IF                                                             
060800                                                                          
060900       MOVE DCS-KVPB-LIM          TO MOD-KVPB-LIM                         
061000       MOVE DCS-SUVARLIM-TPAS     TO MOD-SUVARLIM-TPAS                    
061100                                                                          
061200       IF DCS-FLRETUR-PAS = JA                                            
061300         MOVE YES                 TO MOD-FLRETUR-PAS                      
061400       ELSE                                                               
061500         MOVE DCS-FLRETUR-PAS     TO MOD-FLRETUR-PAS                      
061600       END-IF                                                             
061700                                                                          
061800       MOVE DCS-SUARTMIN-RPAS     TO MOD-SUARTMIN-RPAS                    
061900       MOVE DCS-KVVECKOR-RPAS     TO MOD-KVVECKOR-RPAS                    
062000                                                                          
062100       IF DCS-FLSKROT-PAS = JA                                            
062200         MOVE YES                 TO MOD-FLSKROT-PAS                      
062300       ELSE                                                               
062400         MOVE DCS-FLSKROT-PAS     TO MOD-FLSKROT-PAS                      
062500       END-IF                                                             
062600                                                                          
062700       MOVE DCS-KVSKROT-SPAS      TO MOD-KVSKROT-SPAS                     
062720       MOVE DCS-KVVECKOR-SPAS     TO MOD-KVVECKOR-SPAS                    
062780       MOVE DCS-IDTECKEN-SPAS     TO MOD-IDTECKEN-SPAS                    
062795       MOVE DCS-PRARTSTD-SPAS     TO MOD-PRARTSTD-SPAS                    
062801       MOVE DCS-ADLAGOMR-SPAS     TO MOD-ADLAGOMR-SPAS                    
062807       MOVE DCS-IDPERSON-SPAS     TO MOD-IDPERSON-SPAS                    
062813       MOVE DCS-KDPRODSL-SPAS     TO MOD-KDPRODSL-SPAS                    
062820     END-IF                                                               
062900     .                                                                    
063000     EJECT                                                                
063100 G-CHECK-INPUT SECTION.                                                   
063200     MOVE 'G-CHECK-INPUT     '         TO WS-CURRENT-SECTION              
063300                                                                          
063400     MOVE JA   TO INDATA-SW                                               
063500     IF  MID-TID-TRS         = ALL '+'                                    
063600     AND MID-TIVV            = ALL '+'                                    
063700     AND MID-KVVECKOR-IN     = ALL '+'                                    
063800     AND MID-FLTRANS-PAS     = ALL '+'                                    
063900     AND MID-IDDC-TPAS-1     = ALL '+'                                    
064000     AND MID-IDDC-TPAS-2     = ALL '+'                                    
064100     AND MID-IDDC-TPAS-3     = ALL '+'                                    
064200     AND MID-SUARTMIN-TPAS   = ALL '+'                                    
064300     AND MID-KVPERIOD-TPAS   = ALL '+'                                    
064400     AND MID-KVVECKOR-TPAS   = ALL '+'                                    
064500     AND MID-FLTRANS-ERS     = ALL '+'                                    
064600     AND MID-KVPB-LIM        = ALL '+'                                    
064700     AND MID-SUVARLIM-TPAS   = ALL '+'                                    
064800     AND MID-FLRETUR-PAS     = ALL '+'                                    
064900     AND MID-SUARTMIN-RPAS   = ALL '+'                                    
065000     AND MID-KVVECKOR-RPAS   = ALL '+'                                    
065100     AND MID-FLSKROT-PAS     = ALL '+'                                    
065200     AND MID-KVSKROT-SPAS    = ALL '+'                                    
065210     AND MID-KVVECKOR-SPAS   = ALL '+'                                    
065220     AND MID-IDTECKEN-SPAS   = ALL '+'                                    
065230     AND MID-PRARTSTD-SPAS   = ALL '+'                                    
065240     AND MID-ADLAGOMR-SPAS   = ALL '+'                                    
065250     AND MID-IDPERSON-SPAS   = ALL '+'                                    
065260     AND MID-KDPRODSL-SPAS   = ALL '+'                                    
065300                                                                          
065400       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
065500       CALL WMEDKONV USING MED-WMEDAREA                                   
065600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
065700                                                                          
065800       PERFORM MFS-DONT-TOUCH-FIELD-IN                                    
065900       PERFORM MFS-DONT-TOUCH-FIELD-OUT                                   
066000       MOVE NEJ TO INDATA-SW                                              
066100     ELSE                                                                 
066200                                                                          
066300       PERFORM GA-CHECK-INPUT-FIELDS-TRANSFER                             
066400                                                                          
066500       PERFORM GB-CHECK-INPUT-FIELDS-RETURN                               
066600                                                                          
066700       PERFORM GC-CHECK-INPUT-FIELDS-SCRAP                                
066800                                                                          
066900       IF INDATA-WRONG                                                    
067000                                                                          
067100         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
067200         CALL WMEDKONV USING MED-WMEDAREA                                 
067300         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
067400                                                                          
067500         PERFORM MFS-DONT-TOUCH-FIELD-OUT                                 
067600         PERFORM MFS-DONT-TOUCH-FIELD-IN                                  
067700       END-IF                                                             
067800     END-IF                                                               
067900     .                                                                    
068000     EJECT                                                                
068100 GA-CHECK-INPUT-FIELDS-TRANSFER     SECTION.                              
068200     MOVE 'GA-CHECK-INPUT-FIELDS'      TO WS-CURRENT-SECTION              
068300                                                                          
068400     IF MID-TID-TRS = ALL '+'                                             
068500     OR MID-TID-TRS = SPACE                                               
068600       IF WS-DCS-TID-TRS > SPACE                                          
068700         MOVE MFS-ALPHA-FIELD-OK       TO MOD-TID-TRS-IN-ATTR             
068800       ELSE                                                               
068900         MOVE MFS-ALPHA-FIELD-WRONG    TO MOD-TID-TRS-IN-ATTR             
069000         MOVE NEJ TO INDATA-SW                                            
069100       END-IF                                                             
069200     ELSE                                                                 
069300                                                                          
069400       EVALUATE MID-TID-TRS                                               
069500         WHEN NEJ                                                         
069600           MOVE NEJ                    TO MOD-TID-TRS                     
069700           MOVE +0                     TO WS-TID-TRS                      
069800           MOVE MFS-ALPHA-FIELD-OK     TO MOD-TID-TRS-IN-ATTR             
069900         WHEN 'MO'                                                        
070000           MOVE '07'                   TO MOD-TID-TRS                     
070100           MOVE  +7                    TO WS-TID-TRS                      
070200           MOVE MFS-ALPHA-FIELD-OK     TO MOD-TID-TRS-IN-ATTR             
070300         WHEN 'TU'                                                        
070400           MOVE '02'                   TO MOD-TID-TRS                     
070500           MOVE  +2                    TO WS-TID-TRS                      
070600           MOVE MFS-ALPHA-FIELD-OK     TO MOD-TID-TRS-IN-ATTR             
070700         WHEN 'WE'                                                        
070800           MOVE '03'                   TO MOD-TID-TRS                     
070900           MOVE  +3                    TO WS-TID-TRS                      
071000           MOVE MFS-ALPHA-FIELD-OK     TO MOD-TID-TRS-IN-ATTR             
071100         WHEN 'TH'                                                        
071200           MOVE '04'                   TO MOD-TID-TRS                     
071300           MOVE  +4                    TO WS-TID-TRS                      
071400           MOVE MFS-ALPHA-FIELD-OK     TO MOD-TID-TRS-IN-ATTR             
071500         WHEN 'FR'                                                        
071600           MOVE '05'                   TO MOD-TID-TRS                     
071700           MOVE  +5                    TO WS-TID-TRS                      
071800           MOVE MFS-ALPHA-FIELD-OK     TO MOD-TID-TRS-IN-ATTR             
071900         WHEN OTHER                                                       
072000           MOVE MFS-ALPHA-FIELD-WRONG  TO MOD-TID-TRS-IN-ATTR             
072100           MOVE NEJ TO INDATA-SW                                          
072200           MOVE 'NOT VALID DAY'        TO MOD-TEMFSINF                    
072300       END-EVALUATE                                                       
072400     END-IF                                                               
072500                                                                          
072600*                                                                         
072700     IF MID-TIVV NOT = ALL '+'                                            
072800        IF MID-TIVV NUMERIC                                               
072900           MOVE MID-TIVV                TO WS-VALID-TIVV-SW               
073000           IF VALID-TIVV                                                  
073100              MOVE MFS-NUM-FIELD-OK     TO MOD-TIVV-IN-ATTR               
073200           ELSE                                                           
073300              MOVE MFS-NUM-FIELD-WRONG  TO MOD-TIVV-IN-ATTR               
073400              MOVE NEJ                  TO INDATA-SW                      
073500           END-IF                                                         
073600        ELSE                                                              
073700           MOVE MFS-NUM-FIELD-WRONG     TO MOD-TIVV-IN-ATTR               
073800           MOVE NEJ                     TO INDATA-SW                      
073900        END-IF                                                            
074000     END-IF                                                               
074100                                                                          
074200*                                                                         
074300     IF MID-KVVECKOR-IN NOT = ALL '+'                                     
074400        IF MID-KVVECKOR-IN NUMERIC                                        
074500           MOVE MID-KVVECKOR-IN         TO WS-VALID-KVVECKOR-SW           
074600           IF VALID-KVVECKOR                                              
074700              MOVE MFS-NUM-FIELD-OK     TO MOD-KVVECKOR-IN-ATTR           
074800           ELSE                                                           
074900              MOVE MFS-NUM-FIELD-WRONG  TO MOD-KVVECKOR-IN-ATTR           
075000              MOVE NEJ                  TO INDATA-SW                      
075100           END-IF                                                         
075200        ELSE                                                              
075300           MOVE MFS-NUM-FIELD-WRONG     TO MOD-KVVECKOR-IN-ATTR           
075400           MOVE NEJ                     TO INDATA-SW                      
075500        END-IF                                                            
075600     END-IF                                                               
075700*TRANSFER                                                                 
075800*                                                                         
075900*FLTRANS-PAS                                                              
076000                                                                          
076100     IF MID-FLTRANS-PAS = ALL '+'                                         
076200                                                                          
076300       MOVE MFS-ALPHA-FIELD-OK        TO MOD-FLTRANS-PAS-IN-ATTR          
076400                                                                          
076500     ELSE                                                                 
076600       IF MID-FLTRANS-PAS = JA OR NEJ OR YES                              
076700         MOVE MFS-ALPHA-FIELD-OK      TO MOD-FLTRANS-PAS-IN-ATTR          
076800       ELSE                                                               
076900         MOVE MFS-ALPHA-FIELD-WRONG TO MOD-FLTRANS-PAS-IN-ATTR            
077000         MOVE NEJ TO INDATA-SW                                            
077100         MOVE 'ONLY Y/N ALLOWED'     TO MOD-TEMFSINF                      
077200       END-IF                                                             
077300     END-IF                                                               
077400                                                                          
077500*IDDC-TPAS-1                                                              
077600     IF MID-IDDC-TPAS-1 = ALL '+'                                         
077700       IF MID-FLTRANS-PAS = JA OR YES                                     
077800         IF WS-DCS-IDDC-TPAS-1 > SPACE                                    
077900           MOVE MFS-ALPHA-FIELD-OK TO MOD-IDDC-TPAS-1-IN-ATTR             
078000         ELSE                                                             
078100           MOVE 'DC 1 MISSING WHEN TRANSFER = J'                          
078200             TO MOD-TEMFSINF                                              
078300           MOVE MFS-ALPHA-FIELD-WRONG TO MOD-IDDC-TPAS-1-IN-ATTR          
078400           MOVE NEJ TO INDATA-SW                                          
078500         END-IF                                                           
078600       ELSE                                                               
078700         MOVE MFS-ALPHA-FIELD-OK TO MOD-IDDC-TPAS-1-IN-ATTR               
078800       END-IF                                                             
078900     ELSE                                                                 
079000                                                                          
079100       IF MID-IDDC-TPAS-1    = '00'                                       
079200           MOVE 'DC 0 NOT ALLOWED' TO MOD-TEMFSINF                        
079300           MOVE MFS-ALPHA-FIELD-WRONG TO MOD-IDDC-TPAS-1-IN-ATTR          
079400           MOVE NEJ TO INDATA-SW                                          
079500       END-IF                                                             
079600                                                                          
079700       IF INDATA-OK                                                       
079800         IF MID-FLTRANS-PAS  = JA OR YES                                  
079900         OR WS-DCS-FLTRANS-PAS = JA OR YES                                
080000                                                                          
080100           IF MID-IDDC-TPAS-1 = '  '                                      
080200           MOVE 'NOT ALLOWED WHEN TRANSFER IS YES' TO MOD-TEMFSINF        
080300             MOVE MFS-ALPHA-FIELD-WRONG TO MOD-IDDC-TPAS-1-IN-ATTR        
080400             MOVE NEJ TO INDATA-SW                                        
080500           ELSE                                                           
080600             MOVE MFS-ALPHA-FIELD-OK  TO MOD-IDDC-TPAS-1-IN-ATTR          
080700           END-IF                                                         
080800         END-IF                                                           
080900       END-IF                                                             
081000                                                                          
081100       IF INDATA-OK                                                       
081200         IF MID-IDDC-TPAS-1  > SPACE                                      
081300           IF MID-IDDC-TPAS-1    = MID-IDDC-TPAS-2                        
081400           OR MID-IDDC-TPAS-1    = WS-DCS-IDDC-TPAS-2                     
081500           OR WS-DCS-IDDC-TPAS-1 = MID-IDDC-TPAS-2                        
081600             MOVE 'TRANSFER DC 2 IDENTICAL' TO MOD-TEMFSINF               
081700             MOVE MFS-ALPHA-FIELD-WRONG TO MOD-IDDC-TPAS-1-IN-ATTR        
081800             MOVE NEJ TO INDATA-SW                                        
081900           END-IF                                                         
082000         END-IF                                                           
082100       END-IF                                                             
082200                                                                          
082300       IF INDATA-OK                                                       
082400         IF MID-IDDC-TPAS-1  > SPACE                                      
082500           IF MID-IDDC-TPAS-1    = MID-IDDC-TPAS-3                        
082600           OR MID-IDDC-TPAS-1    = WS-DCS-IDDC-TPAS-3                     
082700           OR WS-DCS-IDDC-TPAS-1 = MID-IDDC-TPAS-3                        
082800             MOVE 'TRANSFER DC 3 IDENTICAL' TO MOD-TEMFSINF               
082900             MOVE MFS-ALPHA-FIELD-WRONG TO MOD-IDDC-TPAS-1-IN-ATTR        
083000             MOVE NEJ TO INDATA-SW                                        
083100           END-IF                                                         
083200         END-IF                                                           
083300       END-IF                                                             
083400                                                                          
083500       IF INDATA-OK                                                       
083600           IF  MID-IDDC-TPAS-1    = SPACE                                 
083700           AND WS-DCS-IDDC-TPAS-2 > SPACE                                 
083800           MOVE 'NOT ALLOWED WHEN DC-2 IS SET' TO MOD-TEMFSINF            
083900             MOVE MFS-ALPHA-FIELD-WRONG TO MOD-IDDC-TPAS-1-IN-ATTR        
084000             MOVE NEJ TO INDATA-SW                                        
084100           ELSE                                                           
084200             MOVE MFS-ALPHA-FIELD-OK  TO MOD-IDDC-TPAS-1-IN-ATTR          
084300           END-IF                                                         
084400       END-IF                                                             
084500                                                                          
084600       IF INDATA-OK                                                       
084700         IF MID-IDDC-TPAS-1  > SPACE                                      
084800         MOVE MID-IDDC-TPAS-1 TO W-IDDC-TPAS                              
084900         PERFORM IMS-GU-WDB601                                            
085000         IF SEGMENT-MISSING                                               
085100           MOVE 'DC MISSING'          TO MOD-TEMFSINF                     
085200           MOVE MFS-ALPHA-FIELD-WRONG TO MOD-IDDC-TPAS-1-IN-ATTR          
085300           MOVE NEJ TO INDATA-SW                                          
085400         ELSE                                                             
085500****KOLLAR OM GODKÄND TRANSFER VÄG SE BILD 2349                           
085600           MOVE MOD-IDDC-UT       TO WS-IDDC-SEND                         
085700           MOVE MID-IDDC-TPAS-1 TO WS-IDDC-REC                            
085800                                                                          
085900           PERFORM DB2-SELECT-TP4TRAN                                     
086000                                                                          
086100           IF LINES-FOUND                                                 
086200             MOVE MFS-ALPHA-FIELD-OK TO MOD-IDDC-TPAS-1-IN-ATTR           
086300           ELSE                                                           
086400                                                                          
086500             MOVE 'NOT ALLOWED FOR TRANSFER DC1' TO MOD-TEMFSINF          
086600             MOVE MFS-ALPHA-FIELD-WRONG TO MOD-IDDC-TPAS-1-IN-ATTR        
086700             MOVE NEJ TO INDATA-SW                                        
086800           END-IF                                                         
086900         END-IF                                                           
087000         END-IF                                                           
087100       END-IF                                                             
087200                                                                          
087300     END-IF                                                               
087400                                                                          
087500*IDDC-TPAS-2                                                              
087600     IF MID-IDDC-TPAS-2 = ALL '+'                                         
087700       MOVE MFS-ALPHA-FIELD-OK TO MOD-IDDC-TPAS-2-IN-ATTR                 
087800     ELSE                                                                 
087900                                                                          
088000       IF MID-IDDC-TPAS-2    = '00'                                       
088100         MOVE 'DC 0 NOT ALLOWED' TO MOD-TEMFSINF                          
088200         MOVE MFS-ALPHA-FIELD-WRONG TO MOD-IDDC-TPAS-2-IN-ATTR            
088300         MOVE NEJ TO INDATA-SW                                            
088400       END-IF                                                             
088500                                                                          
088600       IF INDATA-OK                                                       
088700           IF  MID-IDDC-TPAS-2    = SPACE                                 
088800           AND WS-DCS-IDDC-TPAS-3 > SPACE                                 
088900             MOVE 'NOT ALLOWED WHEN DC-3 IS SET' TO MOD-TEMFSINF          
089000             MOVE MFS-ALPHA-FIELD-WRONG TO MOD-IDDC-TPAS-2-IN-ATTR        
089100             MOVE NEJ TO INDATA-SW                                        
089200           ELSE                                                           
089300             MOVE MFS-ALPHA-FIELD-OK  TO MOD-IDDC-TPAS-2-IN-ATTR          
089400           END-IF                                                         
089500       END-IF                                                             
089600                                                                          
089700       IF INDATA-OK                                                       
089800         IF MID-IDDC-TPAS-2  > SPACE                                      
089900           IF MID-IDDC-TPAS-2    = MID-IDDC-TPAS-1                        
090000           OR MID-IDDC-TPAS-2    = WS-DCS-IDDC-TPAS-1                     
090100           OR WS-DCS-IDDC-TPAS-2 = MID-IDDC-TPAS-1                        
090200             MOVE 'TRANSFER DC 2 IDENTICAL' TO MOD-TEMFSINF               
090300             MOVE MFS-ALPHA-FIELD-WRONG TO MOD-IDDC-TPAS-2-IN-ATTR        
090400             MOVE NEJ TO INDATA-SW                                        
090500           END-IF                                                         
090600         END-IF                                                           
090700       END-IF                                                             
090800                                                                          
090900       IF INDATA-OK                                                       
091000         IF MID-IDDC-TPAS-2  > SPACE                                      
091100           IF MID-IDDC-TPAS-2    = MID-IDDC-TPAS-3                        
091200           OR MID-IDDC-TPAS-2    = WS-DCS-IDDC-TPAS-3                     
091300           OR WS-DCS-IDDC-TPAS-2 = MID-IDDC-TPAS-3                        
091400             MOVE 'TRANSFER DC 3 IDENTICAL' TO MOD-TEMFSINF               
091500             MOVE MFS-ALPHA-FIELD-WRONG TO MOD-IDDC-TPAS-2-IN-ATTR        
091600             MOVE NEJ TO INDATA-SW                                        
091700                                                                          
091800           END-IF                                                         
091900         END-IF                                                           
092000       END-IF                                                             
092100                                                                          
092200       IF INDATA-OK                                                       
092300         IF MID-IDDC-TPAS-2  > SPACE                                      
092400         MOVE MID-IDDC-TPAS-2 TO W-IDDC-TPAS                              
092500         PERFORM IMS-GU-WDB601                                            
092600         IF SEGMENT-MISSING                                               
092700           MOVE 'DC MISSING'          TO MOD-TEMFSINF                     
092800           MOVE MFS-ALPHA-FIELD-WRONG TO MOD-IDDC-TPAS-2-IN-ATTR          
092900           MOVE NEJ TO INDATA-SW                                          
093000         ELSE                                                             
093100****KOLLAR OM GODKÄND TRANSFER VÄG SE BILD 2349                           
093200           MOVE MOD-IDDC-UT       TO WS-IDDC-SEND                         
093300           MOVE MID-IDDC-TPAS-2 TO WS-IDDC-REC                            
093400                                                                          
093500           PERFORM DB2-SELECT-TP4TRAN                                     
093600                                                                          
093700           IF LINES-FOUND                                                 
093800             MOVE MFS-ALPHA-FIELD-OK TO MOD-IDDC-TPAS-2-IN-ATTR           
093900           ELSE                                                           
094000                                                                          
094100             MOVE 'NOT ALLOWED FOR TRANSFER DC2' TO MOD-TEMFSINF          
094200             MOVE MFS-ALPHA-FIELD-WRONG TO MOD-IDDC-TPAS-2-IN-ATTR        
094300             MOVE NEJ TO INDATA-SW                                        
094400           END-IF                                                         
094500         END-IF                                                           
094600         END-IF                                                           
094700       END-IF                                                             
094800                                                                          
094900     END-IF                                                               
095000                                                                          
095100*IDDC-TPAS-3                                                              
095200     IF MID-IDDC-TPAS-3 = ALL '+'                                         
095300       MOVE MFS-ALPHA-FIELD-OK TO MOD-IDDC-TPAS-3-IN-ATTR                 
095400     ELSE                                                                 
095500                                                                          
095600       IF MID-IDDC-TPAS-3    = '00'                                       
095700           MOVE 'DC 0 NOT ALLOWED' TO MOD-TEMFSINF                        
095800           MOVE MFS-ALPHA-FIELD-WRONG TO MOD-IDDC-TPAS-3-IN-ATTR          
095900           MOVE NEJ TO INDATA-SW                                          
096000       END-IF                                                             
096100                                                                          
096200       IF INDATA-OK                                                       
096300         IF MID-IDDC-TPAS-3  > SPACE                                      
096400           IF MID-IDDC-TPAS-3    = MID-IDDC-TPAS-1                        
096500           OR MID-IDDC-TPAS-3    = WS-DCS-IDDC-TPAS-1                     
096600           OR WS-DCS-IDDC-TPAS-3 = MID-IDDC-TPAS-1                        
096700             MOVE 'TRANSFER DC 1 IDENTICAL' TO MOD-TEMFSINF               
096800             MOVE MFS-ALPHA-FIELD-WRONG TO MOD-IDDC-TPAS-3-IN-ATTR        
096900             MOVE NEJ TO INDATA-SW                                        
097000           END-IF                                                         
097100         END-IF                                                           
097200       END-IF                                                             
097300                                                                          
097400       IF INDATA-OK                                                       
097500         IF MID-IDDC-TPAS-3  > SPACE                                      
097600           IF MID-IDDC-TPAS-3    = MID-IDDC-TPAS-2                        
097700           OR MID-IDDC-TPAS-3    = WS-DCS-IDDC-TPAS-2                     
097800           OR WS-DCS-IDDC-TPAS-3 = MID-IDDC-TPAS-2                        
097900             MOVE 'TRANSFER DC 2 IDENTICAL' TO MOD-TEMFSINF               
098000             MOVE MFS-ALPHA-FIELD-WRONG TO MOD-IDDC-TPAS-3-IN-ATTR        
098100             MOVE NEJ TO INDATA-SW                                        
098200           END-IF                                                         
098300         END-IF                                                           
098400       END-IF                                                             
098500                                                                          
098600       IF INDATA-OK                                                       
098700         IF MID-IDDC-TPAS-3  > SPACE                                      
098800                                                                          
098900           IF WS-DCS-IDDC-TPAS-2 = SPACE                                  
099000                                                                          
099100             IF MID-IDDC-TPAS-2 <= SPACE                                  
099200             OR MID-IDDC-TPAS-2  = '++'                                   
099300                                                                          
099400               MOVE 'TRANSFER DC 2 NOT SET' TO MOD-TEMFSINF               
099500             MOVE MFS-ALPHA-FIELD-WRONG TO MOD-IDDC-TPAS-3-IN-ATTR        
099600               MOVE NEJ TO INDATA-SW                                      
099700             END-IF                                                       
099800           END-IF                                                         
099900         END-IF                                                           
100000       END-IF                                                             
100100                                                                          
100200       IF INDATA-OK                                                       
100300         IF MID-IDDC-TPAS-3  > SPACE                                      
100400         MOVE MID-IDDC-TPAS-3 TO W-IDDC-TPAS                              
100500         PERFORM IMS-GU-WDB601                                            
100600         IF SEGMENT-MISSING                                               
100700           MOVE 'DC MISSING'          TO MOD-TEMFSINF                     
100800           MOVE MFS-ALPHA-FIELD-WRONG TO MOD-IDDC-TPAS-2-IN-ATTR          
100900           MOVE NEJ TO INDATA-SW                                          
101000         ELSE                                                             
101100****KOLLAR OM GODKÄND TRANSFER VÄG SE BILD 2349                           
101200           MOVE MOD-IDDC-UT       TO WS-IDDC-SEND                         
101300           MOVE MID-IDDC-TPAS-3 TO WS-IDDC-REC                            
101400                                                                          
101500           PERFORM DB2-SELECT-TP4TRAN                                     
101600                                                                          
101700           IF LINES-FOUND                                                 
101800             MOVE MFS-ALPHA-FIELD-OK TO MOD-IDDC-TPAS-3-IN-ATTR           
101900           ELSE                                                           
102000                                                                          
102100             MOVE 'NOT ALLOWED FOR TRANSFER DC3' TO MOD-TEMFSINF          
102200             MOVE MFS-ALPHA-FIELD-WRONG TO MOD-IDDC-TPAS-3-IN-ATTR        
102300             MOVE NEJ TO INDATA-SW                                        
102400           END-IF                                                         
102500         END-IF                                                           
102600         END-IF                                                           
102700       END-IF                                                             
102800                                                                          
102900     END-IF                                                               
103000*                                                                         
103100*VALUE >                                                                  
103200*SUARTMIN-TPAS                                                            
103300     IF MID-SUARTMIN-TPAS = ALL '+'                                       
103400     OR MID-SUARTMIN-TPAS = SPACE                                         
103500                                                                          
103600       IF WS-DCS-SUARTMIN-TPAS > ZERO                                     
103700         MOVE MFS-NUM-FIELD-OK TO MOD-SUARTMIN-TPAS-IN-ATTR               
103800       ELSE                                                               
103900                                                                          
104000         IF MID-FLTRANS-PAS = JA OR YES                                   
104100         OR WS-DCS-FLTRANS-PAS = JA OR YES                                
104200                                                                          
104300          MOVE MFS-NUM-FIELD-WRONG TO MOD-SUARTMIN-TPAS-IN-ATTR           
104400             MOVE NEJ TO INDATA-SW                                        
104500         ELSE                                                             
104600           MOVE MFS-NUM-FIELD-OK TO MOD-SUARTMIN-TPAS-IN-ATTR             
104700         END-IF                                                           
104800       END-IF                                                             
104900     ELSE                                                                 
105000       IF MID-SUARTMIN-TPAS NUMERIC                                       
105100                                                                          
105200         IF MID-FLTRANS-PAS = JA OR YES                                   
105300         OR WS-DCS-FLTRANS-PAS = JA OR YES                                
105400           IF MID-SUARTMIN-TPAS >= 000000                                 
105500             MOVE MFS-NUM-FIELD-OK TO MOD-SUARTMIN-TPAS-IN-ATTR           
105600           ELSE                                                           
105700             MOVE MFS-NUM-FIELD-WRONG                                     
105800               TO MOD-SUARTMIN-TPAS-IN-ATTR                               
105900             MOVE NEJ TO INDATA-SW                                        
106000           END-IF                                                         
106100         ELSE                                                             
106200           IF MID-SUARTMIN-TPAS >= 000000                                 
106300             MOVE MFS-NUM-FIELD-OK TO MOD-SUARTMIN-TPAS-IN-ATTR           
106400           ELSE                                                           
106500             MOVE MFS-NUM-FIELD-WRONG                                     
106600               TO MOD-SUARTMIN-TPAS-IN-ATTR                               
106700             MOVE NEJ TO INDATA-SW                                        
106800           END-IF                                                         
106900         END-IF                                                           
107000       ELSE                                                               
107100         MOVE MFS-NUM-FIELD-WRONG                                         
107200           TO MOD-SUARTMIN-TPAS-IN-ATTR                                   
107300         MOVE NEJ TO INDATA-SW                                            
107400       END-IF                                                             
107500     END-IF                                                               
107600*                                                                         
107700*                                                                         
107800*STOCK(PERIODS) <                                                         
107900*KVPERIOD-TPAS                                                            
108000     IF MID-KVPERIOD-TPAS      = ALL '+' OR SPACE                         
108100                                                                          
108200       IF MID-FLTRANS-PAS      = JA OR YES                                
108300                                                                          
108400         IF WS-DCS-KVPERIOD-TPAS > 00                                     
108500           MOVE MFS-NUM-FIELD-OK  TO MOD-KVPERIOD-TPAS-IN-ATTR            
108600         ELSE                                                             
108700           MOVE MFS-NUM-FIELD-WRONG TO MOD-KVPERIOD-TPAS-IN-ATTR          
108800           MOVE NEJ TO INDATA-SW                                          
108900                                                                          
109000         END-IF                                                           
109100       ELSE                                                               
109200         MOVE MFS-NUM-FIELD-OK TO MOD-KVPERIOD-TPAS-IN-ATTR               
109300       END-IF                                                             
109400     ELSE                                                                 
109500                                                                          
109600       IF MID-FLTRANS-PAS    = JA OR YES                                  
109700       OR WS-DCS-FLTRANS-PAS = JA OR YES                                  
109800         IF  MID-KVPERIOD-TPAS NUMERIC                                    
109900         AND MID-KVPERIOD-TPAS > 00                                       
110000           MOVE MFS-NUM-FIELD-OK TO MOD-KVPERIOD-TPAS-IN-ATTR             
110100         ELSE                                                             
110200        MOVE MFS-NUM-FIELD-WRONG TO MOD-KVPERIOD-TPAS-IN-ATTR             
110300           MOVE NEJ TO INDATA-SW                                          
110400         END-IF                                                           
110500       ELSE                                                               
110600                                                                          
110700         IF MID-KVPERIOD-TPAS NUMERIC                                     
110800         AND MID-KVPERIOD-TPAS >= 00                                      
110900           MOVE MFS-NUM-FIELD-OK TO MOD-KVPERIOD-TPAS-IN-ATTR             
111000         ELSE                                                             
111100          MOVE MFS-NUM-FIELD-WRONG TO MOD-KVPERIOD-TPAS-IN-ATTR           
111200           MOVE NEJ TO INDATA-SW                                          
111300         END-IF                                                           
111400       END-IF                                                             
111500     END-IF                                                               
111600*                                                                         
111700*SALES(WEEKS)                                                             
111800*KVVECKOR-TPAS = SALES (WEEKS)                                            
111900*                                                                         
112000     IF MID-KVVECKOR-TPAS    = ALL '+'                                    
112100                                                                          
112200       IF MID-FLTRANS-PAS    = JA OR YES                                  
112300                                                                          
112400         IF WS-DCS-KVVECKOR-TPAS > 000                                    
112500           MOVE MFS-NUM-FIELD-OK TO MOD-KVVECKOR-TPAS-IN-ATTR             
112600         ELSE                                                             
112700           MOVE MFS-NUM-FIELD-WRONG TO MOD-KVVECKOR-TPAS-IN-ATTR          
112800           MOVE NEJ TO INDATA-SW                                          
112900         END-IF                                                           
113000       ELSE                                                               
113100         MOVE MFS-NUM-FIELD-OK       TO MOD-KVVECKOR-TPAS-IN-ATTR         
113200       END-IF                                                             
113300     ELSE                                                                 
113400                                                                          
113500       IF MID-FLTRANS-PAS      = JA OR YES                                
113600       OR WS-DCS-FLTRANS-PAS = JA OR YES                                  
113700*        IF WS-DCS-KVVECKOR-TPAS > 000                                    
113800*          MOVE MFS-NUM-FIELD-OK   TO MOD-KVVECKOR-TPAS-IN-ATTR           
113900*        ELSE                                                             
114000                                                                          
114100         IF MID-KVVECKOR-TPAS NUMERIC                                     
114200         AND MID-KVVECKOR-TPAS > 000                                      
114300         AND MID-KVVECKOR-TPAS < 54                                       
114400           MOVE MFS-NUM-FIELD-OK TO MOD-KVVECKOR-TPAS-IN-ATTR             
114500         ELSE                                                             
114600           MOVE MFS-NUM-FIELD-WRONG TO MOD-KVVECKOR-TPAS-IN-ATTR          
114700           MOVE NEJ TO INDATA-SW                                          
114800         END-IF                                                           
114900*        END-IF                                                           
115000       ELSE                                                               
115100         IF  MID-KVVECKOR-TPAS NUMERIC                                    
115200         AND MID-KVVECKOR-TPAS <= 53                                      
115300                                                                          
115400           IF MID-KVVECKOR-TPAS >= 000                                    
115500             MOVE MFS-NUM-FIELD-OK   TO MOD-KVVECKOR-TPAS-IN-ATTR         
115600           ELSE                                                           
115700             MOVE MFS-NUM-FIELD-WRONG TO MOD-KVVECKOR-TPAS-IN-ATTR        
115800             MOVE NEJ TO INDATA-SW                                        
115900           END-IF                                                         
116000         ELSE                                                             
116100                                                                          
116200          MOVE MFS-NUM-FIELD-WRONG TO MOD-KVVECKOR-TPAS-IN-ATTR           
116300           MOVE NEJ TO INDATA-SW                                          
116400         END-IF                                                           
116500       END-IF                                                             
116600     END-IF                                                               
116700*                                                                         
116800*FLTRANS-ERS                                                              
116900* REPL Y/N                                                                
117000     IF MID-FLTRANS-ERS = '+'                                             
117100                                                                          
117200       IF WS-DCS-FLTRANS-PAS    = JA OR YES                               
117300                                                                          
117400         IF WS-DCS-FLTRANS-ERS = JA OR YES OR NEJ                         
117500           MOVE MFS-ALPHA-FIELD-OK    TO MOD-FLTRANS-ERS-IN-ATTR          
117600         ELSE                                                             
117700           MOVE MFS-ALPHA-FIELD-WRONG TO MOD-FLTRANS-ERS-IN-ATTR          
117800           MOVE NEJ TO INDATA-SW                                          
117900         END-IF                                                           
118000       ELSE                                                               
118100         MOVE MFS-ALPHA-FIELD-OK      TO MOD-FLTRANS-ERS-IN-ATTR          
118200         IF WS-DCS-FLTRANS-ERS NOT = JA OR YES OR NEJ                     
118300           MOVE 'N'                   TO DCS-FLTRANS-ERS                  
118400         END-IF                                                           
118500*                                                                         
118600       END-IF                                                             
118700     ELSE                                                                 
118800       IF MID-FLTRANS-ERS = JA OR YES OR NEJ                              
118900         MOVE MFS-ALPHA-FIELD-OK      TO MOD-FLTRANS-ERS-IN-ATTR          
119000       ELSE                                                               
119100         MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-FLTRANS-ERS-IN-ATTR          
119200         MOVE NEJ TO INDATA-SW                                            
119300         MOVE 'ONLY Y/N ALLOWED'     TO MOD-TEMFSINF                      
119400       END-IF                                                             
119500     END-IF                                                               
119600*                                                                         
119700*F/C LIMIT <                                                              
119800*KVPB-LIM                                                                 
119900*                                                                         
120000     IF MID-KVPB-LIM = ALL '+'                                            
120100     OR MID-KVPB-LIM = SPACE                                              
120200       MOVE MFS-NUM-FIELD-OK TO MOD-KVPB-LIM-IN-ATTR                      
120300     ELSE                                                                 
120400*      KONVERTERA IFRÅN FRITT FORMAT TILL 6 + 1 DECIMAL                   
120500       MOVE MID-KVPB-LIM TO DEC-IDFRIDATA                                 
120600                                                                          
120700       IF MID-FLTRANS-PAS    = JA OR YES                                  
120800       OR WS-DCS-FLTRANS-PAS = JA OR YES                                  
120900         IF MID-KVPB-LIM > 0                                              
121000                                                                          
121100           MOVE +6          TO DEC-KVHELTAL                               
121200           MOVE +1          TO DEC-KVDECIMAL                              
121300           CALL WDECEDIT USING DEC-WDECAREA                               
121400           IF DEC-KDSVAR-OK                                               
121500             MOVE MFS-NUM-FIELD-OK TO MOD-KVPB-LIM-IN-ATTR                
121600             MOVE DEC-IDEDITDATA                                          
121700                               TO WS-RED-KVPB-LIM                         
121800           ELSE                                                           
121900             MOVE MFS-NUM-FIELD-WRONG TO MOD-KVPB-LIM-IN-ATTR             
122000             MOVE NEJ      TO INDATA-SW                                   
122100           END-IF                                                         
122200         ELSE                                                             
122300           MOVE MFS-NUM-FIELD-WRONG TO MOD-SUVARLIM-TPAS-IN-ATTR          
122400           MOVE NEJ        TO INDATA-SW                                   
122500         END-IF                                                           
122600       ELSE                                                               
122700         IF MID-KVPB-LIM >= 0                                             
122800                                                                          
122900           MOVE +6          TO DEC-KVHELTAL                               
123000           MOVE +1          TO DEC-KVDECIMAL                              
123100           CALL WDECEDIT USING DEC-WDECAREA                               
123200           IF DEC-KDSVAR-OK                                               
123300             MOVE MFS-NUM-FIELD-OK TO MOD-KVPB-LIM-IN-ATTR                
123400             MOVE DEC-IDEDITDATA                                          
123500                               TO WS-RED-KVPB-LIM                         
123600           ELSE                                                           
123700             MOVE MFS-NUM-FIELD-WRONG TO MOD-KVPB-LIM-IN-ATTR             
123800             MOVE NEJ      TO INDATA-SW                                   
123900           END-IF                                                         
124000         ELSE                                                             
124100           MOVE MFS-NUM-FIELD-WRONG TO MOD-SUVARLIM-TPAS-IN-ATTR          
124200           MOVE NEJ        TO INDATA-SW                                   
124300         END-IF                                                           
124400       END-IF                                                             
124500     END-IF                                                               
124600*                                                                         
124700*QTY LIMIT <                                                              
124800*SUVARLIM-TPAS                                                            
124900     IF MID-SUVARLIM-TPAS = ALL '+'                                       
125000     OR MID-SUVARLIM-TPAS = SPACE                                         
125100                                                                          
125200       MOVE MFS-NUM-FIELD-OK TO MOD-SUVARLIM-TPAS-IN-ATTR                 
125300     ELSE                                                                 
125400       IF MID-SUVARLIM-TPAS NUMERIC                                       
125500         IF MID-FLTRANS-PAS    = JA OR YES                                
125600         OR WS-DCS-FLTRANS-PAS = JA OR YES                                
125700           IF MID-SUVARLIM-TPAS >= 000000                                 
125800             MOVE MFS-NUM-FIELD-OK TO MOD-SUVARLIM-TPAS-IN-ATTR           
125900           ELSE                                                           
126000            MOVE MFS-NUM-FIELD-WRONG TO MOD-SUVARLIM-TPAS-IN-ATTR         
126100             MOVE NEJ TO INDATA-SW                                        
126200           END-IF                                                         
126300         ELSE                                                             
126400           IF MID-SUVARLIM-TPAS >= 000000                                 
126500             MOVE MFS-NUM-FIELD-OK TO MOD-SUVARLIM-TPAS-IN-ATTR           
126600           ELSE                                                           
126700            MOVE MFS-NUM-FIELD-WRONG TO MOD-SUVARLIM-TPAS-IN-ATTR         
126800             MOVE NEJ TO INDATA-SW                                        
126900           END-IF                                                         
127000         END-IF                                                           
127100       ELSE                                                               
127200         MOVE MFS-NUM-FIELD-WRONG TO MOD-SUVARLIM-TPAS-IN-ATTR            
127300         MOVE NEJ TO INDATA-SW                                            
127400       END-IF                                                             
127500     END-IF                                                               
127600     .                                                                    
127700     EJECT                                                                
127800                                                                          
127900                                                                          
128000 GB-CHECK-INPUT-FIELDS-RETURN       SECTION.                              
128100     MOVE 'GB-CHECK-INPUT-FIELDS-RETURN' TO WS-CURRENT-SECTION            
128200*                                                                         
128300*                                                                         
128400*RETURN                                                                   
128500*                                                                         
128600*FLRETUR-PAS                                                              
128700     IF MID-FLRETUR-PAS = ALL '+'                                         
128800                                                                          
128900       MOVE MFS-ALPHA-FIELD-OK      TO MOD-FLRETUR-PAS-IN-ATTR            
129000                                                                          
129100     ELSE                                                                 
129200       IF MID-FLRETUR-PAS = JA OR YES OR NEJ                              
129300         MOVE MFS-ALPHA-FIELD-OK      TO MOD-FLRETUR-PAS-IN-ATTR          
129400       ELSE                                                               
129500       MOVE MFS-ALPHA-FIELD-WRONG     TO MOD-FLRETUR-PAS-IN-ATTR          
129600         MOVE NEJ                     TO INDATA-SW                        
129700         MOVE 'ONLY Y/N ALLOWED'     TO MOD-TEMFSINF                      
129800       END-IF                                                             
129900     END-IF                                                               
130000*                                                                         
130100*SUARTMIN-RPAS                                                            
130200*                                                                         
130300     IF MID-SUARTMIN-RPAS = ALL '+'                                       
130400       IF MID-FLRETUR-PAS      = JA OR YES                                
130500                                                                          
130600         IF WS-DCS-SUARTMIN-RPAS > 0                                      
130700           MOVE MFS-NUM-FIELD-OK TO MOD-SUARTMIN-RPAS-IN-ATTR             
130800         ELSE                                                             
130900           MOVE MFS-NUM-FIELD-WRONG TO MOD-SUARTMIN-RPAS-IN-ATTR          
131000           MOVE NEJ TO INDATA-SW                                          
131100         END-IF                                                           
131200       ELSE                                                               
131300         MOVE MFS-NUM-FIELD-OK TO MOD-SUARTMIN-RPAS-IN-ATTR               
131400       END-IF                                                             
131500     ELSE                                                                 
131600       IF MID-SUARTMIN-RPAS NUMERIC                                       
131700         IF MID-FLRETUR-PAS    = JA OR YES                                
131800         OR WS-DCS-FLRETUR-PAS = JA OR YES                                
131900           IF MID-SUARTMIN-RPAS > 0                                       
132000             MOVE MFS-NUM-FIELD-OK TO MOD-SUARTMIN-RPAS-IN-ATTR           
132100           ELSE                                                           
132200             MOVE MFS-NUM-FIELD-WRONG TO MOD-SUARTMIN-RPAS-IN-ATTR        
132300             MOVE NEJ TO INDATA-SW                                        
132400           END-IF                                                         
132500         ELSE                                                             
132600           IF MID-SUARTMIN-RPAS >= 0                                      
132700             MOVE MFS-NUM-FIELD-OK TO MOD-SUARTMIN-RPAS-IN-ATTR           
132800           ELSE                                                           
132900             MOVE MFS-NUM-FIELD-WRONG TO MOD-SUARTMIN-RPAS-IN-ATTR        
133000             MOVE NEJ TO INDATA-SW                                        
133100           END-IF                                                         
133200         END-IF                                                           
133300       ELSE                                                               
133400                                                                          
133500         MOVE MFS-NUM-FIELD-WRONG TO MOD-SUARTMIN-RPAS-IN-ATTR            
133600         MOVE NEJ TO INDATA-SW                                            
133700       END-IF                                                             
133800     END-IF                                                               
133900*                                                                         
134000*KVVECKOR-RPAS                                                            
134100*                                                                         
134200     IF MID-KVVECKOR-RPAS = ALL '+'                                       
134300                                                                          
134400       IF MID-FLRETUR-PAS = JA OR YES                                     
134500                                                                          
134600         IF WS-DCS-KVVECKOR-RPAS > 0                                      
134700           MOVE MFS-NUM-FIELD-OK TO MOD-KVVECKOR-RPAS-IN-ATTR             
134800         ELSE                                                             
134900           MOVE MFS-NUM-FIELD-WRONG TO MOD-KVVECKOR-RPAS-IN-ATTR          
135000           MOVE NEJ TO INDATA-SW                                          
135100         END-IF                                                           
135200       ELSE                                                               
135300         MOVE MFS-NUM-FIELD-OK TO MOD-KVVECKOR-RPAS-IN-ATTR               
135400       END-IF                                                             
135500     ELSE                                                                 
135600                                                                          
135700       IF MID-FLRETUR-PAS    = JA OR YES                                  
135800       OR WS-DCS-FLRETUR-PAS = JA OR YES                                  
135900         IF MID-KVVECKOR-RPAS NUMERIC                                     
136000         AND MID-KVVECKOR-RPAS <= 53                                      
136100         AND MID-KVVECKOR-RPAS >  0                                       
136200           MOVE MFS-NUM-FIELD-OK TO MOD-KVVECKOR-RPAS-IN-ATTR             
136300         ELSE                                                             
136400           MOVE MFS-NUM-FIELD-WRONG TO MOD-KVVECKOR-RPAS-IN-ATTR          
136500           MOVE NEJ TO INDATA-SW                                          
136600         END-IF                                                           
136700       ELSE                                                               
136800         IF MID-KVVECKOR-RPAS NUMERIC                                     
136900         AND MID-KVVECKOR-RPAS <= 53                                      
137000         AND MID-KVVECKOR-RPAS >= 0                                       
137100           MOVE MFS-NUM-FIELD-OK TO MOD-KVVECKOR-RPAS-IN-ATTR             
137200         ELSE                                                             
137300           MOVE MFS-NUM-FIELD-WRONG TO MOD-KVVECKOR-RPAS-IN-ATTR          
137400           MOVE NEJ TO INDATA-SW                                          
137500         END-IF                                                           
137600       END-IF                                                             
137700     END-IF                                                               
137800     .                                                                    
137900     EJECT                                                                
138000                                                                          
138100                                                                          
138200 GC-CHECK-INPUT-FIELDS-SCRAP        SECTION.                              
138300     MOVE 'GC-CHECK-INPUT-FIELDS-SCRAP'  TO WS-CURRENT-SECTION            
138400*                                                                         
138500*SCRAP                                                                    
138600*                                                                         
138700*                                                                         
138800*FLSKROT-PAS                                                              
138900*                                                                         
139000     IF MID-FLSKROT-PAS = ALL '+'                                         
139100       MOVE MFS-ALPHA-FIELD-OK TO MOD-FLSKROT-PAS-IN-ATTR                 
139200                                                                          
139300     ELSE                                                                 
139400       IF MID-FLSKROT-PAS = JA OR YES OR NEJ                              
140300         MOVE MFS-ALPHA-FIELD-OK TO MOD-FLSKROT-PAS-IN-ATTR               
140500       ELSE                                                               
140600         MOVE MFS-ALPHA-FIELD-WRONG TO MOD-FLSKROT-PAS-IN-ATTR            
140700         MOVE NEJ TO INDATA-SW                                            
140800         MOVE 'ONLY Y/N ALLOWED'     TO MOD-TEMFSINF                      
140900       END-IF                                                             
141000     END-IF                                                               
141100*                                                                         
141200*KVSKROT-TRS                                                              
141300*                                                                         
141400     IF MID-KVSKROT-SPAS = ALL '+'                                        
141500                                                                          
141600       IF MID-FLSKROT-PAS = JA OR YES                                     
141700                                                                          
141800         IF WS-DCS-KVSKROT-SPAS > 0                                       
141900           MOVE MFS-NUM-FIELD-OK    TO MOD-KVSKROT-SPAS-IN-ATTR           
142000         ELSE                                                             
142100           MOVE MFS-NUM-FIELD-WRONG TO MOD-KVSKROT-SPAS-IN-ATTR           
142200           MOVE NEJ TO INDATA-SW                                          
142300         END-IF                                                           
142400       ELSE                                                               
142500         MOVE MFS-NUM-FIELD-OK      TO MOD-KVSKROT-SPAS-IN-ATTR           
142600       END-IF                                                             
142700     ELSE                                                                 
142800                                                                          
142900       IF  MID-KVSKROT-SPAS NUMERIC                                       
143000       AND MID-KVSKROT-SPAS >= 0                                          
143100         MOVE MFS-NUM-FIELD-OK      TO MOD-KVSKROT-SPAS-IN-ATTR           
143200       ELSE                                                               
143300         MOVE MFS-NUM-FIELD-WRONG   TO MOD-KVSKROT-SPAS-IN-ATTR           
143400         MOVE NEJ TO INDATA-SW                                            
143500       END-IF                                                             
143600     END-IF                                                               
143610*                                                                         
143620*PASSIVE(WEEKS)                                                           
143630*KVVECKOR-SPAS = PASSIVE (WEEKS)                                          
143640*                                                                         
143650     IF MID-KVVECKOR-SPAS    = ALL '+'                                    
143660                                                                          
143670       IF MID-FLSKROT-PAS    = JA OR YES                                  
143680                                                                          
143690         IF WS-DCS-KVVECKOR-SPAS > 000                                    
143692           MOVE MFS-NUM-FIELD-OK TO MOD-KVVECKOR-SPAS-IN-ATTR             
143693         ELSE                                                             
143694           MOVE MFS-NUM-FIELD-WRONG TO MOD-KVVECKOR-SPAS-IN-ATTR          
143695           MOVE NEJ TO INDATA-SW                                          
143696         END-IF                                                           
143697       ELSE                                                               
143698         MOVE MFS-NUM-FIELD-OK       TO MOD-KVVECKOR-SPAS-IN-ATTR         
143699       END-IF                                                             
143700     ELSE                                                                 
143701                                                                          
143703       IF  WS-DCS-FLSKROT-PAS = JA OR YES                                 
143704       AND MID-FLSKROT-PAS NOT = NEJ                                      
143707                                                                          
143708         IF MID-KVVECKOR-SPAS NUMERIC                                     
143711         AND MID-KVVECKOR-SPAS < 261                                      
143712           MOVE MFS-NUM-FIELD-OK TO MOD-KVVECKOR-SPAS-IN-ATTR             
143713         ELSE                                                             
143714           MOVE MFS-NUM-FIELD-WRONG TO MOD-KVVECKOR-SPAS-IN-ATTR          
143715           MOVE NEJ TO INDATA-SW                                          
143716         END-IF                                                           
143717       ELSE                                                               
143718         IF MID-KVVECKOR-SPAS NUMERIC                                     
143720                                                                          
143721            MOVE MFS-NUM-FIELD-OK   TO MOD-KVVECKOR-SPAS-IN-ATTR          
143726         ELSE                                                             
143727            MOVE MFS-NUM-FIELD-WRONG TO MOD-KVVECKOR-SPAS-IN-ATTR         
143728            MOVE NEJ TO INDATA-SW                                         
143733         END-IF                                                           
143734       END-IF                                                             
143735     END-IF                                                               
143736*                                                                         
143738*PRICE SIGN                                                               
143739*IDTECKEN = PRICE SIGN ( < = > )                                          
143740*                                                                         
143741     IF MID-IDTECKEN-SPAS    = ALL '+'                                    
143742                                                                          
143743        IF MID-PRARTSTD-SPAS  = ALL '+'                                   
143745           MOVE MFS-NUM-FIELD-OK TO MOD-IDTECKEN-SPAS-IN-ATTR             
143746        ELSE                                                              
143747           IF WS-DCS-IDTECKEN-SPAS NOT = SPACE                            
143748              MOVE MFS-NUM-FIELD-OK TO MOD-IDTECKEN-SPAS-IN-ATTR          
143749           ELSE                                                           
143750             MOVE MFS-ALPHA-FIELD-WRONG TO                                
143751                                     MOD-IDTECKEN-SPAS-IN-ATTR            
143752             MOVE NEJ TO INDATA-SW                                        
143753           END-IF                                                         
143754        END-IF                                                            
143755     ELSE                                                                 
143756                                                                          
143757       IF MID-IDTECKEN-SPAS = SPACE OR '<' OR '=' OR '>'                  
143758                                                                          
143759         IF MID-IDTECKEN-SPAS = SPACE                                     
143760            IF WS-DCS-PRARTSTD-SPAS NOT > ZERO                            
143761            AND (MID-PRARTSTD-SPAS = ALL '+'                              
143762             OR MID-PRARTSTD-SPAS = ZERO)                                 
143763               MOVE MFS-NUM-FIELD-OK TO MOD-IDTECKEN-SPAS-IN-ATTR         
143764            ELSE                                                          
143765             MOVE MFS-ALPHA-FIELD-WRONG TO                                
143766                                     MOD-IDTECKEN-SPAS-IN-ATTR            
143768             MOVE NEJ TO INDATA-SW                                        
143769            END-IF                                                        
143770         ELSE                                                             
143771           IF (MID-PRARTSTD-SPAS NOT = ALL '+'                            
143772           AND MID-PRARTSTD-SPAS > ZERO)                                  
143773           OR                                                             
143774              (MID-PRARTSTD-SPAS  = ALL '+'                               
143775              AND WS-DCS-PRARTSTD-SPAS > ZERO)                            
143776             MOVE MFS-NUM-FIELD-OK TO MOD-IDTECKEN-SPAS-IN-ATTR           
143777           ELSE                                                           
143778             MOVE MFS-ALPHA-FIELD-WRONG TO                                
143779                                     MOD-IDTECKEN-SPAS-IN-ATTR            
143781             MOVE NEJ TO INDATA-SW                                        
143782           END-IF                                                         
143783         END-IF                                                           
143784       ELSE                                                               
143785          MOVE MFS-ALPHA-FIELD-WRONG TO                                   
143786                                     MOD-IDTECKEN-SPAS-IN-ATTR            
143788          MOVE NEJ TO INDATA-SW                                           
143789       END-IF                                                             
143790     END-IF                                                               
143831*                                                                         
143832*PRICE                                                                    
143833*PRARTSTD = STD PRICE (MATERIAL PRICE FOR CHINA)                          
143834*                                                                         
143835     IF MID-PRARTSTD-SPAS    = ALL '+'                                    
143836                                                                          
143837        MOVE MFS-NUM-FIELD-OK TO MOD-IDTECKEN-SPAS-IN-ATTR                
143838     ELSE                                                                 
143839                                                                          
143840       IF MID-PRARTSTD-SPAS NUMERIC                                       
143841                                                                          
143842          MOVE MFS-NUM-FIELD-OK TO MOD-PRARTSTD-SPAS-IN-ATTR              
143843       ELSE                                                               
143844          MOVE MFS-NUM-FIELD-WRONG TO MOD-PRARTSTD-SPAS-IN-ATTR           
143845          MOVE NEJ TO INDATA-SW                                           
143847       END-IF                                                             
143848     END-IF                                                               
143849*                                                                         
143850*AREA                                                                     
143851*ADLAGOMR = AREA                                                          
143852*                                                                         
143853     IF MID-ADLAGOMR-SPAS    = ALL '+'                                    
143854                                                                          
143855        MOVE MFS-NUM-FIELD-OK TO MOD-ADLAGOMR-SPAS-IN-ATTR                
143856     ELSE                                                                 
143857                                                                          
143858       IF MID-ADLAGOMR-SPAS NUMERIC                                       
143859                                                                          
143860          MOVE MFS-NUM-FIELD-OK TO MOD-ADLAGOMR-SPAS-IN-ATTR              
143861       ELSE                                                               
143862          MOVE MFS-NUM-FIELD-WRONG TO MOD-ADLAGOMR-SPAS-IN-ATTR           
143863          MOVE NEJ TO INDATA-SW                                           
143864       END-IF                                                             
143865     END-IF                                                               
143866*                                                                         
143867*BUYER                                                                    
143868*IDPERSON = BUYER                                                         
143869*                                                                         
143870     IF MID-IDPERSON-SPAS    = ALL '+'                                    
143871                                                                          
143872        MOVE MFS-NUM-FIELD-OK TO MOD-IDPERSON-SPAS-IN-ATTR                
143873     ELSE                                                                 
143874                                                                          
143875       IF MID-IDPERSON-SPAS NUMERIC                                       
143876                                                                          
143877          MOVE MFS-NUM-FIELD-OK TO MOD-IDPERSON-SPAS-IN-ATTR              
143878       ELSE                                                               
143879          MOVE MFS-NUM-FIELD-WRONG TO MOD-IDPERSON-SPAS-IN-ATTR           
143880          MOVE NEJ TO INDATA-SW                                           
143881       END-IF                                                             
143882     END-IF                                                               
143883*                                                                         
143884*PG                                                                       
143885*KDPRODSL = PRODUCT GROUP                                                 
143886*                                                                         
143887     IF MID-KDPRODSL-SPAS    = ALL '+'                                    
143888                                                                          
143889        MOVE MFS-NUM-FIELD-OK TO MOD-KDPRODSL-SPAS-IN-ATTR                
143890     ELSE                                                                 
143891                                                                          
143892       IF MID-KDPRODSL-SPAS NUMERIC                                       
143893                                                                          
143894          MOVE MFS-NUM-FIELD-OK TO MOD-KDPRODSL-SPAS-IN-ATTR              
143895       ELSE                                                               
143896          MOVE MFS-NUM-FIELD-WRONG TO MOD-KDPRODSL-SPAS-IN-ATTR           
143897          MOVE NEJ TO INDATA-SW                                           
143898       END-IF                                                             
143899     END-IF                                                               
143900     .                                                                    
143901     EJECT                                                                
143910                                                                          
144000 H-UPDATE SECTION.                                                        
144100     MOVE 'H-UPDATE      '         TO WS-CURRENT-SECTION                  
144200                                                                          
144300     MOVE MSGI-IDDC-KEY TO W-IDDC                                         
144400                                                                          
144500     PERFORM IMS-GHU-WDB601                                               
144600     IF SEGMENT-FOUND                                                     
144700                                                                          
144800       IF MID-TID-TRS NOT = ALL '+'                                       
144900         MOVE WS-TID-TRS        TO DCS-TID-TRS                            
145000       END-IF                                                             
145100                                                                          
145200       IF MID-TIVV NOT = ALL '+'                                          
145300         MOVE MID-TIVV          TO DCS-TIVV                               
145400       END-IF                                                             
145500                                                                          
145601       IF MID-KVVECKOR-IN NOT = ALL '+'                                   
145701         MOVE MID-KVVECKOR-IN   TO DCS-KVVECKOR-BIN                       
145801       END-IF                                                             
145900*TRANSFER                                                                 
146000       IF MID-FLTRANS-PAS NOT = ALL '+'                                   
146100         IF MID-FLTRANS-PAS   = JA                                        
146200           MOVE YES             TO DCS-FLTRANS-PAS                        
146300         ELSE                                                             
146400           MOVE MID-FLTRANS-PAS TO DCS-FLTRANS-PAS                        
146500         END-IF                                                           
146600       END-IF                                                             
146700       IF MID-IDDC-TPAS-1 NOT = ALL '+'                                   
146800         MOVE MID-IDDC-TPAS-1   TO DCS-IDDC-TPAS-1                        
146900       END-IF                                                             
147000       IF MID-IDDC-TPAS-2 NOT = ALL '+'                                   
147100         MOVE MID-IDDC-TPAS-2   TO DCS-IDDC-TPAS-2                        
147200       END-IF                                                             
147300       IF MID-IDDC-TPAS-3 NOT = ALL '+'                                   
147400         MOVE MID-IDDC-TPAS-3   TO DCS-IDDC-TPAS-3                        
147500       END-IF                                                             
147600       IF MID-SUARTMIN-TPAS NOT = ALL '+'                                 
147700         MOVE MID-SUARTMIN-TPAS TO DCS-SUARTMIN-TPAS                      
147800       END-IF                                                             
147900       IF MID-KVPERIOD-TPAS NOT = ALL '+'                                 
148000         MOVE MID-KVPERIOD-TPAS TO DCS-KVPERIOD-TPAS                      
148100       END-IF                                                             
148200       IF MID-KVVECKOR-TPAS NOT = ALL '+'                                 
148300         MOVE MID-KVVECKOR-TPAS TO DCS-KVVECKOR-TPAS                      
148400       END-IF                                                             
148500       IF MID-FLTRANS-ERS NOT = ALL '+'                                   
148600         MOVE MID-FLTRANS-ERS   TO DCS-FLTRANS-ERS                        
148700       END-IF                                                             
148800       IF MID-KVPB-LIM NOT = ALL '+'                                      
148900         MOVE WS-RED-KVPB-LIM   TO DCS-KVPB-LIM                           
149000       END-IF                                                             
149100       IF MID-SUVARLIM-TPAS NOT = ALL '+'                                 
149200         MOVE MID-SUVARLIM-TPAS TO DCS-SUVARLIM-TPAS                      
149300       END-IF                                                             
149400*RETUR                                                                    
149500       IF MID-FLRETUR-PAS NOT = ALL '+'                                   
149600         IF MID-FLRETUR-PAS = JA                                          
149700           MOVE YES             TO DCS-FLRETUR-PAS                        
149800         ELSE                                                             
149900           MOVE MID-FLRETUR-PAS TO DCS-FLRETUR-PAS                        
150000         END-IF                                                           
150100       END-IF                                                             
150200       IF MID-SUARTMIN-RPAS NOT = ALL '+'                                 
150300         MOVE MID-SUARTMIN-RPAS TO DCS-SUARTMIN-RPAS                      
150400       END-IF                                                             
150500       IF MID-KVVECKOR-RPAS NOT = ALL '+'                                 
150600         MOVE MID-KVVECKOR-RPAS TO DCS-KVVECKOR-RPAS                      
150700       END-IF                                                             
150800*SKROT                                                                    
150900       IF MID-FLSKROT-PAS NOT = ALL '+'                                   
151000         IF MID-FLSKROT-PAS = JA                                          
151100           MOVE YES             TO DCS-FLSKROT-PAS                        
151200         ELSE                                                             
151300           MOVE MID-FLSKROT-PAS TO DCS-FLSKROT-PAS                        
151400         END-IF                                                           
151500       END-IF                                                             
151600       IF MID-KVSKROT-SPAS NOT = ALL '+'                                  
151700         MOVE MID-KVSKROT-SPAS  TO DCS-KVSKROT-SPAS                       
151800       END-IF                                                             
151810       IF MID-KVVECKOR-SPAS NOT = ALL '+'                                 
151820         MOVE MID-KVVECKOR-SPAS TO DCS-KVVECKOR-SPAS                      
151830       END-IF                                                             
151840       IF MID-IDTECKEN-SPAS NOT = ALL '+'                                 
151850         MOVE MID-IDTECKEN-SPAS TO DCS-IDTECKEN-SPAS                      
151860       END-IF                                                             
151870       IF MID-PRARTSTD-SPAS NOT = ALL '+'                                 
151880         MOVE MID-PRARTSTD-SPAS TO DCS-PRARTSTD-SPAS                      
151890       END-IF                                                             
151891       IF MID-ADLAGOMR-SPAS NOT = ALL '+'                                 
151892         MOVE MID-ADLAGOMR-SPAS TO DCS-ADLAGOMR-SPAS                      
151893       END-IF                                                             
151894       IF MID-IDPERSON-SPAS NOT = ALL '+'                                 
151895         MOVE MID-IDPERSON-SPAS TO DCS-IDPERSON-SPAS                      
151896       END-IF                                                             
151897       IF MID-KDPRODSL-SPAS NOT = ALL '+'                                 
151898         MOVE MID-KDPRODSL-SPAS TO DCS-KDPRODSL-SPAS                      
151899       END-IF                                                             
151900                                                                          
152000       PERFORM IMS-REPL-WDB601                                            
152100                                                                          
152200       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
152300       CALL WMEDKONV USING MED-WMEDAREA                                   
152400       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
152500                                                                          
152600       PERFORM MFS-FORM-ATTR                                              
152700       PERFORM MFS-ERASE-FIELD-IN                                         
152800     END-IF                                                               
152900     .                                                                    
153000     EJECT                                                                
153100* --- MFS SECTIONS ---                                                    
153200* --- MFS SECTIONS ---                                                    
153300* --- MFS SECTIONS ---                                                    
153400 MFS-ERASE-FIELD-OUT SECTION.                                             
153500     MOVE 'MFS-ERASE-FIELD-OUT'    TO WS-CURRENT-SECTION                  
153600                                                                          
153700*    --- ALLA UTDATA-FÄLT                                                 
153800     MOVE MFS-ERASE-FIELD TO MOD-TID-TRS                                  
153900                             MOD-TIVV                                     
154000                             MOD-KVVECKOR-UT                              
154100                             MOD-FLTRANS-PAS                              
154200                             MOD-IDDC-TPAS-1                              
154300                             MOD-IDDC-TPAS-2                              
154400                             MOD-IDDC-TPAS-3                              
154500                             MOD-SUARTMIN-TPAS                            
154600                             MOD-KVPERIOD-TPAS                            
154700                             MOD-KVVECKOR-TPAS                            
154800                             MOD-FLTRANS-ERS                              
154900                             MOD-KVPB-LIM                                 
155000                             MOD-SUVARLIM-TPAS                            
155100                             MOD-FLRETUR-PAS                              
155200                             MOD-SUARTMIN-RPAS                            
155300                             MOD-KVVECKOR-RPAS                            
155400                             MOD-FLSKROT-PAS                              
155500                             MOD-KVSKROT-SPAS                             
155510                             MOD-KVVECKOR-SPAS                            
155520                             MOD-IDTECKEN-SPAS                            
155530                             MOD-PRARTSTD-SPAS                            
155540                             MOD-ADLAGOMR-SPAS                            
155550                             MOD-IDPERSON-SPAS                            
155560                             MOD-KDPRODSL-SPAS                            
155600                                                                          
155700     .                                                                    
155800     SKIP3                                                                
155900 MFS-ERASE-FIELD-IN SECTION.                                              
156000     MOVE 'MFS-ERASE-FIELD-IN '    TO WS-CURRENT-SECTION                  
156100                                                                          
156200*    --- ALLA INDATA-FÄLT                                                 
156300     MOVE MFS-ERASE-FIELD TO MOD-TID-TRS-IN                               
156400                             MOD-TIVV-IN                                  
156500                             MOD-KVVECKOR-IN                              
156600                             MOD-FLTRANS-PAS-IN                           
156700                             MOD-IDDC-TPAS-1-IN                           
156800                             MOD-IDDC-TPAS-2-IN                           
156900                             MOD-IDDC-TPAS-3-IN                           
157000                             MOD-SUARTMIN-TPAS-IN                         
157100                             MOD-KVPERIOD-TPAS-IN                         
157200                             MOD-KVVECKOR-TPAS-IN                         
157300                             MOD-FLTRANS-ERS-IN                           
157400                             MOD-KVPB-LIM-IN                              
157500                             MOD-SUVARLIM-TPAS-IN                         
157600                             MOD-FLRETUR-PAS-IN                           
157700                             MOD-SUARTMIN-RPAS-IN                         
157800                             MOD-KVVECKOR-RPAS-IN                         
157900                             MOD-FLSKROT-PAS-IN                           
158000                             MOD-KVSKROT-SPAS-IN                          
158010                             MOD-KVVECKOR-SPAS-IN                         
158020                             MOD-IDTECKEN-SPAS-IN                         
158030                             MOD-PRARTSTD-SPAS-IN                         
158040                             MOD-ADLAGOMR-SPAS-IN                         
158050                             MOD-IDPERSON-SPAS-IN                         
158060                             MOD-KDPRODSL-SPAS-IN                         
158100                                                                          
158200     .                                                                    
158300     EJECT                                                                
158400 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
158500     MOVE 'MFS-DONT-TOUCH-FIELD-OUT'    TO WS-CURRENT-SECTION             
158600                                                                          
158700*    --- ALLA UTDATA-FÄLT                                                 
158800     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-TID-TRS                           
158900                                    MOD-TIVV                              
159000                                    MOD-KVVECKOR-UT                       
159100                                    MOD-FLTRANS-PAS                       
159200                                    MOD-IDDC-TPAS-1                       
159300                                    MOD-IDDC-TPAS-2                       
159400                                    MOD-IDDC-TPAS-3                       
159500                                    MOD-SUARTMIN-TPAS                     
159600                                    MOD-KVPERIOD-TPAS                     
159700                                    MOD-KVVECKOR-TPAS                     
159800                                    MOD-FLTRANS-ERS                       
159900                                    MOD-KVPB-LIM                          
160000                                    MOD-SUVARLIM-TPAS                     
160100                                    MOD-FLRETUR-PAS                       
160200                                    MOD-SUARTMIN-RPAS                     
160300                                    MOD-KVVECKOR-RPAS                     
160400                                    MOD-FLSKROT-PAS                       
160500                                    MOD-KVSKROT-SPAS                      
160510                                    MOD-KVVECKOR-SPAS                     
160520                                    MOD-IDTECKEN-SPAS                     
160530                                    MOD-PRARTSTD-SPAS                     
160540                                    MOD-ADLAGOMR-SPAS                     
160550                                    MOD-IDPERSON-SPAS                     
160560                                    MOD-KDPRODSL-SPAS                     
160600                                                                          
160700     .                                                                    
160800     SKIP3                                                                
160900 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
161000     MOVE 'MFS-DONT-TOUCH-FIELD-IN '    TO WS-CURRENT-SECTION             
161100                                                                          
161200*    --- ALLA INDATA-FÄLT                                                 
161300     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-TID-TRS-IN                        
161400                                    MOD-TIVV-IN                           
161500                                    MOD-KVVECKOR-IN                       
161600                                    MOD-FLTRANS-PAS-IN                    
161700                                    MOD-IDDC-TPAS-1-IN                    
161800                                    MOD-IDDC-TPAS-2-IN                    
161900                                    MOD-IDDC-TPAS-3-IN                    
162000                                    MOD-SUARTMIN-TPAS-IN                  
162100                                    MOD-KVPERIOD-TPAS-IN                  
162200                                    MOD-KVVECKOR-TPAS-IN                  
162300                                    MOD-FLTRANS-ERS-IN                    
162400                                    MOD-KVPB-LIM-IN                       
162500                                    MOD-SUVARLIM-TPAS-IN                  
162600                                    MOD-FLRETUR-PAS-IN                    
162700                                    MOD-SUARTMIN-RPAS-IN                  
162800                                    MOD-KVVECKOR-RPAS-IN                  
162900                                    MOD-FLSKROT-PAS-IN                    
163000                                    MOD-KVSKROT-SPAS-IN                   
163010                                    MOD-KVVECKOR-SPAS-IN                  
163020                                    MOD-IDTECKEN-SPAS-IN                  
163030                                    MOD-PRARTSTD-SPAS-IN                  
163040                                    MOD-ADLAGOMR-SPAS-IN                  
163050                                    MOD-IDPERSON-SPAS-IN                  
163060                                    MOD-KDPRODSL-SPAS-IN                  
163100                                                                          
163200     .                                                                    
163300     EJECT                                                                
163400 MFS-FORM-ATTR SECTION.                                                   
163500     MOVE 'MFS-FORM-ATTR        '    TO WS-CURRENT-SECTION                
163600                                                                          
163700*    --- ALL INDATA-FIELDS                                                
163800     MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-TID-TRS-IN-ATTR                  
163900                                    MOD-TIVV-IN-ATTR                      
164000                                    MOD-KVVECKOR-IN-ATTR                  
164100                                    MOD-FLTRANS-PAS-IN-ATTR               
164200                                    MOD-IDDC-TPAS-1-IN-ATTR               
164300                                    MOD-IDDC-TPAS-2-IN-ATTR               
164400                                    MOD-IDDC-TPAS-3-IN-ATTR               
164500                                    MOD-SUARTMIN-TPAS-IN-ATTR             
164600                                    MOD-KVPERIOD-TPAS-IN-ATTR             
164700                                    MOD-KVVECKOR-TPAS-IN-ATTR             
164800                                    MOD-FLTRANS-ERS-IN-ATTR               
164900                                    MOD-KVPB-LIM-IN-ATTR                  
165000                                    MOD-SUVARLIM-TPAS-IN-ATTR             
165100                                    MOD-FLRETUR-PAS-IN-ATTR               
165200                                    MOD-SUARTMIN-RPAS-IN-ATTR             
165300                                    MOD-KVVECKOR-RPAS-IN-ATTR             
165400                                    MOD-FLSKROT-PAS-IN-ATTR               
165500                                    MOD-KVSKROT-SPAS-IN-ATTR              
165510                                    MOD-KVVECKOR-SPAS-IN-ATTR             
165520                                    MOD-IDTECKEN-SPAS-IN-ATTR             
165530                                    MOD-PRARTSTD-SPAS-IN-ATTR             
165540                                    MOD-ADLAGOMR-SPAS-IN-ATTR             
165550                                    MOD-IDPERSON-SPAS-IN-ATTR             
165560                                    MOD-KDPRODSL-SPAS-IN-ATTR             
165600                                                                          
165700     .                                                                    
165800     SKIP2                                                                
165900* --- IMS SECTIONS ---                                                    
166000* --- IMS SECTIONS ---                                                    
166100* --- IMS SECTIONS ---                                                    
166200* --- IMS SECTIONS ---                                                    
166300* --- IMS SECTIONS ---                                                    
166400     SKIP3                                                                
166500 IMS-GET-MSG SECTION.                                                     
166600     MOVE 'IMS-GET-MSG'           TO WS-CURRENT-IMS-SECTION               
166700                                                                          
166800     MOVE '  QC' TO GOOD-STATUSCODES                                      
166900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
167000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
167100     PERFORM IMS-STATUSCHECK                                              
167200     .                                                                    
167300     SKIP3                                                                
167400 IMS-INSERT-MSG SECTION.                                                  
167500     MOVE 'IMS-INSERT-MSG'        TO WS-CURRENT-IMS-SECTION               
167600                                                                          
167700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
167800     MOVE SPACE TO GOOD-STATUSCODES                                       
167900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
168000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
168100     PERFORM IMS-STATUSCHECK                                              
168200     .                                                                    
168300     EJECT                                                                
168400 IMS-GU-WDB601 SECTION.                                                   
168500     MOVE 'IMS-GU-WDB601'        TO WS-CURRENT-IMS-SECTION                
168600                                                                          
168700     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
168800          DELIMITED BY SIZE INTO SSA1                                     
168900     MOVE '  GE' TO GOOD-STATUSCODES                                      
169000     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
169100     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
169200     PERFORM IMS-STATUSCHECK                                              
169300     .                                                                    
169400     SKIP3                                                                
169500 IMS-GHU-WDB601 SECTION.                                                  
169600     MOVE 'IMS-GHU-WDB601'        TO WS-CURRENT-IMS-SECTION               
169700                                                                          
169800     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
169900          DELIMITED BY SIZE INTO SSA1                                     
170000     MOVE '  ' TO GOOD-STATUSCODES                                        
170100     CALL CBLTDLI USING GHU WDB6-PCB DLI-IO-WDB601 SSA1                   
170200     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
170300     PERFORM IMS-STATUSCHECK                                              
170400     .                                                                    
170500     SKIP3                                                                
170600 IMS-REPL-WDB601 SECTION.                                                 
170700     MOVE 'IMS-REPL-WDB601'       TO WS-CURRENT-IMS-SECTION               
170800                                                                          
170900     MOVE '  ' TO GOOD-STATUSCODES                                        
171000     CALL CBLTDLI USING REPL WDB6-PCB DLI-IO-WDB601                       
171100     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
171200     PERFORM IMS-STATUSCHECK                                              
171300     .                                                                    
171400     SKIP3                                                                
171500 DB2-SELECT-TP4TRAN     SECTION.                                          
171600     MOVE 000100  TO GOOD-SQLCODECODES                                    
171700                                                                          
171800     EXEC SQL                                                             
171900           SELECT  IDDC_SEND                                              
172000                  ,IDDC_REC                                               
172100                                                                          
172200           INTO   :TP4TRAN-IDDC-SEND                                      
172300                 ,:TP4TRAN-IDDC-REC                                       
172400                                                                          
172500           FROM    TP4TRAN                                                
172600                                                                          
172700           WHERE IDDC_SEND = :WS-IDDC-SEND                                
172800           AND   IDDC_REC  = :WS-IDDC-REC                                 
172900           AND   KDARBTYP  = 'ESC'                                        
173000     END-EXEC                                                             
173100                                                                          
173200     MOVE SQLCODE TO SQLCODE-WS                                           
173300     PERFORM DB2-STATUS-CHECK                                             
173400     .                                                                    
173500     EJECT                                                                
173600                                                                          
173700 DB2-STATUS-CHECK  SECTION.                                               
173800     SET SQLCODE-IX TO 1                                                  
173900     SEARCH GOOD-SQLCODE                                                  
174000       AT END                                                             
174100          CALL ABEND USING RKOD-ABEND-DB2                                 
174200       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
174300     END-SEARCH                                                           
174400     .                                                                    
174500     EJECT                                                                
174600                                                                          
174700 IMS-STATUSCHECK SECTION.                                                 
174800     MOVE 'IMS-STATUSCHECK'      TO WS-CURRENT-IMS-SECTION                
174900                                                                          
175000     SET STATUS-IX TO 1                                                   
175100     SEARCH GOOD-STATUS                                                   
175200       AT END                                                             
175300         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
175400         DELIMITED BY SIZE INTO ERROR-TEXT                                
175500         CALL FELLOG                                                      
175600       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
175700         CONTINUE                                                         
175800     END-SEARCH                                                           
176000     .                                                                    
