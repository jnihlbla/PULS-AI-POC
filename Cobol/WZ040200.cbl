000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WZ040200.                                                
000400 AUTHOR.         LUNDH BERNT.                                             
000500 DATE-WRITTEN.   02/07/01.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    NAME:                                                                
000900*        CARPARTS.DAP.DISTRRULESMAINTENANCE                               
001000*    FUNCTION:                                                            
001100*        READ/UPDATE/INSERT DISTRIBUTION RULES TABLE TZ4DIRU              
001200*        DEPENDING ON REQUESTED PROGRAMS ACTION CODE (KDPGMACT)           
001300*        KDPGMACT = 'S' READ                                              
001400*        KDPGMACT = 'U' UPDATE                                            
001500*        KDPGMACT = 'I' INSERT                                            
001600*        KDPGMACT = 'D' DELETE                                            
001700*                                                                         
001800*        THE PROGRAM UPDATES TABLE TZ4DIRU                                
001900*                                                                         
002000*    INDATA.                                                              
002100*        TRANSACTION: WZ0402U                                             
002200*        REQUEST:     WZ0402I1                                            
002300*                                                                         
002400*    OUTDATA.                                                             
002500*        RESPONSE:    WZ0402O1                                            
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP3                                                                
003600 FILE SECTION.                                                            
003700     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900 77  IDPGM                       PIC X(08)   VALUE 'WZ040200'.            
004000 77  IDSYSTEM                    PIC X(04)   VALUE 'WZ04'.                
004100                                                                          
004200*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND.               
004300 77  ERROR-TEXT                  PIC X(80)  VALUE SPACE.                  
004400 77  KDRC-DISPLAY                PIC Z(5).                                
004500                                                                          
004600*    --- CONSTANT WORK FIELDS                                             
004700 77  JA                          PIC X       VALUE 'J'.                   
004800 77  YES                         PIC X       VALUE 'Y'.                   
004900 77  NOO                         PIC X       VALUE 'N'.                   
005000 77  WS-PRINT                    PIC X(4)    VALUE 'PRT '.                
005100 77  WS-FAX                      PIC X(4)    VALUE 'FAX '.                
005200 77  WS-VCOM                     PIC X(4)    VALUE 'VCOM'.                
005300 77  WS-EDI                      PIC X(4)    VALUE 'EDI '.                
005400 77  WS-MAIL                     PIC X(4)    VALUE 'MAIL'.                
005500 77  WS-ONDEMAND                 PIC X(4)    VALUE 'ONDE'.                
005600 77  WS-SAVE                     PIC X(4)    VALUE 'SAVE'.                
005700 77  WS-GET-IT                   PIC X(4)    VALUE 'GETI'.                
005800 77  WS-WEB                      PIC X(4)    VALUE 'WEB '.                
005900 77  WS-ADRESS                   PIC X(50)                                
006000                    VALUE 'CARPARTS.DAP.DISTRRULESMAINTENANCE'.           
006100 77  KEYS-SW                     PIC X       VALUE SPACE.                 
006200     88  KEYS-OK                             VALUE 'Y'.                   
006300     88  KEYS-WRONG                          VALUE 'N'.                   
006400                                                                          
006500 77  ACTION-CODE-SW              PIC X   VALUE SPACE.                     
006600     88  ACT-CODE-VALID                  VALUE 'S', 'U', 'I', 'D'.        
006700     88  ACT-CODE-SEARCH                 VALUE 'S'.                       
006800     88  ACT-CODE-UPDATE                 VALUE 'U'.                       
006900     88  ACT-CODE-INSERT                 VALUE 'I'.                       
007000     88  ACT-CODE-DELETE                 VALUE 'D'.                       
007100                                                                          
007200 77  KDOUTMETH-SW                PIC X(4)    VALUE SPACE.                 
007300     88  KDOUTMETH-VALID             VALUE 'PRT ', 'EDI ', 'MAIL',        
007400                                           'VCOM', 'FAX ', 'ONDE',        
007500                                           'SAVE', 'GETI', 'WEB '.        
007600 77  WS-VALID-IDPFDEF            PIC X(36)                                
007700                     VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'.        
007800 77  WS-TEST-IDPFDEF             PIC X(36)                                
007900                     VALUE '                                    '.        
008000                                                                          
008100 77  WS-TEFAX-MAY-BE-SPECIFIED   PIC X(1)    VALUE 'N'.                   
008200 77  WS-IDMAIL-MAY-BE-SPECIFIED  PIC X(1)    VALUE 'N'.                   
008300                                                                          
008400*    --- MAPPING-FIELDS                                                   
008500 01  MAP-KDOUTMETH-1             PIC X(4)    VALUE SPACE.                 
008600 01  MAP-KDOUTMETH-2             PIC X(4)    VALUE SPACE.                 
008700 01  MAP-KDOUTMETH-3             PIC X(4)    VALUE SPACE.                 
008800 01  MAP-KDOUTMETH-4             PIC X(4)    VALUE SPACE.                 
008900 01  MAP-KDOUTMETH-5             PIC X(4)    VALUE SPACE.                 
009000 01  MAP-KDOUTMETH-6             PIC X(4)    VALUE SPACE.                 
009100 01  MAP-KDOUTMETH-7             PIC X(4)    VALUE SPACE.                 
009200 01  MAP-KDOUTMETH-8             PIC X(4)    VALUE SPACE.                 
009300 01  MAP-KDOUTMETH-9             PIC X(4)    VALUE SPACE.                 
009400 01  MAP-KDOUTMETH-10            PIC X(4)    VALUE SPACE.                 
009500 01  MAP-KDOUTMETH-11            PIC X(4)    VALUE SPACE.                 
009600 01  MAP-KDOUTMETH-12            PIC X(4)    VALUE SPACE.                 
009700 01  MAP-KDOUTMETH-13            PIC X(4)    VALUE SPACE.                 
009800 01  MAP-KDOUTMETH-14            PIC X(4)    VALUE SPACE.                 
009900 01  MAP-KDOUTMETH-15            PIC X(4)    VALUE SPACE.                 
010000 01  MAP-IDOUTDEST-1             PIC X(60)   VALUE SPACE.                 
010100 01  MAP-IDOUTDEST-2             PIC X(60)   VALUE SPACE.                 
010200 01  MAP-IDOUTDEST-3             PIC X(60)   VALUE SPACE.                 
010300 01  MAP-IDOUTDEST-4             PIC X(60)   VALUE SPACE.                 
010400 01  MAP-IDOUTDEST-5             PIC X(60)   VALUE SPACE.                 
010500 01  MAP-IDOUTDEST-6             PIC X(60)   VALUE SPACE.                 
010600 01  MAP-IDOUTDEST-7             PIC X(60)   VALUE SPACE.                 
010700 01  MAP-IDOUTDEST-8             PIC X(60)   VALUE SPACE.                 
010800 01  MAP-IDOUTDEST-9             PIC X(60)   VALUE SPACE.                 
010900 01  MAP-IDOUTDEST-10            PIC X(60)   VALUE SPACE.                 
011000 01  MAP-IDOUTDEST-11            PIC X(60)   VALUE SPACE.                 
011100 01  MAP-IDOUTDEST-12            PIC X(60)   VALUE SPACE.                 
011200 01  MAP-IDOUTDEST-13            PIC X(60)   VALUE SPACE.                 
011300 01  MAP-IDOUTDEST-14            PIC X(60)   VALUE SPACE.                 
011400 01  MAP-IDOUTDEST-15            PIC X(60)   VALUE SPACE.                 
011500 01  MAP-KVCOPIES-1              PIC X       VALUE SPACE.                 
011600 01  MAP-KVCOPIES-2              PIC X       VALUE SPACE.                 
011700 01  MAP-KVCOPIES-3              PIC X       VALUE SPACE.                 
011800 01  MAP-KVCOPIES-4              PIC X       VALUE SPACE.                 
011900 01  MAP-KVCOPIES-5              PIC X       VALUE SPACE.                 
012000 01  MAP-KVCOPIES-6              PIC X       VALUE SPACE.                 
012100 01  MAP-KVCOPIES-7              PIC X       VALUE SPACE.                 
012200 01  MAP-KVCOPIES-8              PIC X       VALUE SPACE.                 
012300 01  MAP-KVCOPIES-9              PIC X       VALUE SPACE.                 
012400 01  MAP-KVCOPIES-10             PIC X       VALUE SPACE.                 
012500 01  MAP-KVCOPIES-11             PIC X       VALUE SPACE.                 
012600 01  MAP-KVCOPIES-12             PIC X       VALUE SPACE.                 
012700 01  MAP-KVCOPIES-13             PIC X       VALUE SPACE.                 
012800 01  MAP-KVCOPIES-14             PIC X       VALUE SPACE.                 
012900 01  MAP-KVCOPIES-15             PIC X       VALUE SPACE.                 
013000 01  MAP-FLCARRCNTL-1            PIC X       VALUE SPACE.                 
013100 01  MAP-FLCARRCNTL-2            PIC X       VALUE SPACE.                 
013200 01  MAP-FLCARRCNTL-3            PIC X       VALUE SPACE.                 
013300 01  MAP-FLCARRCNTL-4            PIC X       VALUE SPACE.                 
013400 01  MAP-FLCARRCNTL-5            PIC X       VALUE SPACE.                 
013500 01  MAP-FLCARRCNTL-6            PIC X       VALUE SPACE.                 
013600 01  MAP-FLCARRCNTL-7            PIC X       VALUE SPACE.                 
013700 01  MAP-FLCARRCNTL-8            PIC X       VALUE SPACE.                 
013800 01  MAP-FLCARRCNTL-9            PIC X       VALUE SPACE.                 
013900 01  MAP-FLCARRCNTL-10           PIC X       VALUE SPACE.                 
014000 01  MAP-FLCARRCNTL-11           PIC X       VALUE SPACE.                 
014100 01  MAP-FLCARRCNTL-12           PIC X       VALUE SPACE.                 
014200 01  MAP-FLCARRCNTL-13           PIC X       VALUE SPACE.                 
014300 01  MAP-FLCARRCNTL-14           PIC X       VALUE SPACE.                 
014400 01  MAP-FLCARRCNTL-15           PIC X       VALUE SPACE.                 
014500 01  MAP-FLACIF-1                PIC X       VALUE SPACE.                 
014600 01  MAP-FLACIF-2                PIC X       VALUE SPACE.                 
014700 01  MAP-FLACIF-3                PIC X       VALUE SPACE.                 
014800 01  MAP-FLACIF-4                PIC X       VALUE SPACE.                 
014900 01  MAP-FLACIF-5                PIC X       VALUE SPACE.                 
015000 01  MAP-FLACIF-6                PIC X       VALUE SPACE.                 
015100 01  MAP-FLACIF-7                PIC X       VALUE SPACE.                 
015200 01  MAP-FLACIF-8                PIC X       VALUE SPACE.                 
015300 01  MAP-FLACIF-9                PIC X       VALUE SPACE.                 
015400 01  MAP-FLACIF-10               PIC X       VALUE SPACE.                 
015500 01  MAP-FLACIF-11               PIC X       VALUE SPACE.                 
015600 01  MAP-FLACIF-12               PIC X       VALUE SPACE.                 
015700 01  MAP-FLACIF-13               PIC X       VALUE SPACE.                 
015800 01  MAP-FLACIF-14               PIC X       VALUE SPACE.                 
015900 01  MAP-FLACIF-15               PIC X       VALUE SPACE.                 
016000 01  MAP-IDPFDEF-1               PIC X(8)    VALUE SPACE.                 
016100 01  MAP-IDPFDEF-2               PIC X(8)    VALUE SPACE.                 
016200 01  MAP-IDPFDEF-3               PIC X(8)    VALUE SPACE.                 
016300 01  MAP-IDPFDEF-4               PIC X(8)    VALUE SPACE.                 
016400 01  MAP-IDPFDEF-5               PIC X(8)    VALUE SPACE.                 
016500 01  MAP-IDPFDEF-6               PIC X(8)    VALUE SPACE.                 
016600 01  MAP-IDPFDEF-7               PIC X(8)    VALUE SPACE.                 
016700 01  MAP-IDPFDEF-8               PIC X(8)    VALUE SPACE.                 
016800 01  MAP-IDPFDEF-9               PIC X(8)    VALUE SPACE.                 
016900 01  MAP-IDPFDEF-10              PIC X(8)    VALUE SPACE.                 
017000 01  MAP-IDPFDEF-11              PIC X(8)    VALUE SPACE.                 
017100 01  MAP-IDPFDEF-12              PIC X(8)    VALUE SPACE.                 
017200 01  MAP-IDPFDEF-13              PIC X(8)    VALUE SPACE.                 
017300 01  MAP-IDPFDEF-14              PIC X(8)    VALUE SPACE.                 
017400 01  MAP-IDPFDEF-15              PIC X(8)    VALUE SPACE.                 
017500 01  MAP-IDFORMSNM-1             PIC X(8)    VALUE SPACE.                 
017600 01  MAP-IDFORMSNM-2             PIC X(8)    VALUE SPACE.                 
017700 01  MAP-IDFORMSNM-3             PIC X(8)    VALUE SPACE.                 
017800 01  MAP-IDFORMSNM-4             PIC X(8)    VALUE SPACE.                 
017900 01  MAP-IDFORMSNM-5             PIC X(8)    VALUE SPACE.                 
018000 01  MAP-IDFORMSNM-6             PIC X(8)    VALUE SPACE.                 
018100 01  MAP-IDFORMSNM-7             PIC X(8)    VALUE SPACE.                 
018200 01  MAP-IDFORMSNM-8             PIC X(8)    VALUE SPACE.                 
018300 01  MAP-IDFORMSNM-9             PIC X(8)    VALUE SPACE.                 
018400 01  MAP-IDFORMSNM-10            PIC X(8)    VALUE SPACE.                 
018500 01  MAP-IDFORMSNM-11            PIC X(8)    VALUE SPACE.                 
018600 01  MAP-IDFORMSNM-12            PIC X(8)    VALUE SPACE.                 
018700 01  MAP-IDFORMSNM-13            PIC X(8)    VALUE SPACE.                 
018800 01  MAP-IDFORMSNM-14            PIC X(8)    VALUE SPACE.                 
018900 01  MAP-IDFORMSNM-15            PIC X(8)    VALUE SPACE.                 
019000 01  MAP-TEVCOMST-1              PIC X(20)   VALUE SPACE.                 
019100 01  MAP-TEVCOMST-2              PIC X(20)   VALUE SPACE.                 
019200 01  MAP-TEVCOMST-3              PIC X(20)   VALUE SPACE.                 
019300 01  MAP-TEVCOMST-4              PIC X(20)   VALUE SPACE.                 
019400 01  MAP-TEVCOMST-5              PIC X(20)   VALUE SPACE.                 
019500 01  MAP-TEVCOMST-6              PIC X(20)   VALUE SPACE.                 
019600 01  MAP-TEVCOMST-7              PIC X(20)   VALUE SPACE.                 
019700 01  MAP-TEVCOMST-8              PIC X(20)   VALUE SPACE.                 
019800 01  MAP-TEVCOMST-9              PIC X(20)   VALUE SPACE.                 
019900 01  MAP-TEVCOMST-10             PIC X(20)   VALUE SPACE.                 
020000 01  MAP-TEVCOMST-11             PIC X(20)   VALUE SPACE.                 
020100 01  MAP-TEVCOMST-12             PIC X(20)   VALUE SPACE.                 
020200 01  MAP-TEVCOMST-13             PIC X(20)   VALUE SPACE.                 
020300 01  MAP-TEVCOMST-14             PIC X(20)   VALUE SPACE.                 
020400 01  MAP-TEVCOMST-15             PIC X(20)   VALUE SPACE.                 
020500 01  MAP-IDVCINIT-1              PIC X(8)    VALUE SPACE.                 
020600 01  MAP-IDVCINIT-2              PIC X(8)    VALUE SPACE.                 
020700 01  MAP-IDVCINIT-3              PIC X(8)    VALUE SPACE.                 
020800 01  MAP-IDVCINIT-4              PIC X(8)    VALUE SPACE.                 
020900 01  MAP-IDVCINIT-5              PIC X(8)    VALUE SPACE.                 
021000 01  MAP-IDVCINIT-6              PIC X(8)    VALUE SPACE.                 
021100 01  MAP-IDVCINIT-7              PIC X(8)    VALUE SPACE.                 
021200 01  MAP-IDVCINIT-8              PIC X(8)    VALUE SPACE.                 
021300 01  MAP-IDVCINIT-9              PIC X(8)    VALUE SPACE.                 
021400 01  MAP-IDVCINIT-10             PIC X(8)    VALUE SPACE.                 
021500 01  MAP-IDVCINIT-11             PIC X(8)    VALUE SPACE.                 
021600 01  MAP-IDVCINIT-12             PIC X(8)    VALUE SPACE.                 
021700 01  MAP-IDVCINIT-13             PIC X(8)    VALUE SPACE.                 
021800 01  MAP-IDVCINIT-14             PIC X(8)    VALUE SPACE.                 
021900 01  MAP-IDVCINIT-15             PIC X(8)    VALUE SPACE.                 
022000 01  MAP-TEFAX-1                 PIC X(50)   VALUE SPACE.                 
022100 01  MAP-TEFAX-2                 PIC X(50)   VALUE SPACE.                 
022200 01  MAP-TEFAX-3                 PIC X(50)   VALUE SPACE.                 
022300 01  MAP-TEFAX-4                 PIC X(50)   VALUE SPACE.                 
022400 01  MAP-TEFAX-5                 PIC X(50)   VALUE SPACE.                 
022500 01  MAP-KVDAGAR-RESEND          PIC S9(3)   VALUE ZERO COMP-3.           
022600 01  MAP-TIREGDAT                PIC S9(7)   VALUE ZERO COMP-3.           
022700 01  MAP-TIUPPDAT                PIC S9(7)   VALUE ZERO COMP-3.           
022800 01  MAP-TIANVDAT                PIC S9(7)   VALUE ZERO COMP-3.           
022900 01  MAP-TENOTE.                                                          
023000     49  MAP-TENOTE-L            PIC S9(4) COMP.                          
023100     49  MAP-TENOTE-D            PIC X(300).                              
023200                                                                          
023300*    --- WORK-FIELDS                                                      
023400 01  WS-CURRENT-DATE             PIC 9(6)    VALUE ZERO.                  
023500 01  IX                          PIC 99      VALUE ZERO.                  
023600 01  IX2                         PIC 99      VALUE ZERO.                  
023700 01  TZ4DIRU-COUNTER             PIC S9(5)   VALUE ZERO COMP-3.           
023800 01  WS-IDPFDEF                  PIC X(8)    VALUE SPACE.                 
023900                                                                          
024000*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
024100 01  GENERAL-SUBPROGRAMS.                                                 
024200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
024300     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
024400     03  WSECURIT                PIC X(8)    VALUE 'WSECURIT'.            
024500     03  W009EMAD                PIC X(8)    VALUE 'W009EMAD'.            
024600     03  WZ20TEFA                PIC X(8)    VALUE 'WZ20TEFA'.            
024700     SKIP3                                                                
024800                                                                          
024900*    --- PARAMETERS TO ABEND                                              
025000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
025100 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
025200 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
025300 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
025400                                                                          
025500 01  MESSAGE-CODES.                                                       
025600     03  ERROR-CODES.                                                     
025700         05  ERR-AUTHORIZATION-MISSING PIC X(3)    VALUE '00A'.           
025800         05  ERR-INVALID-KEY           PIC X(3)    VALUE '022'.           
025900         05  ERR-INVALID-FIELD         PIC X(3)    VALUE '023'.           
026000         05  ERR-MUST-BE-NUMERIC       PIC X(3)    VALUE '024'.           
026100         05  ERR-FIELD-NOT-FOUND       PIC X(3)    VALUE '025'.           
026200         05  ERR-MUST-BE-ENTERED       PIC X(3)    VALUE '026'.           
026300         05  ERR-ALREADY-EXIST         PIC X(3)    VALUE '030'.           
026400         05  ERR-SYSTEM-ERROR          PIC X(3)    VALUE '099'.           
026500         05  ERR-MUST-BE-UNIQUE        PIC X(3)    VALUE '101'.           
026600         05  ERR-TEFAX-NOT-ALLOWED     PIC X(3)    VALUE '102'.           
026700         05  ERR-IDMAIL-NOT-ALLOWED    PIC X(3)    VALUE '103'.           
026800     03  INFO-CODES.                                                      
026900         05  INF-UPDATE-OK             PIC X(3)    VALUE '001'.           
027000         05  INF-INSERT-OK             PIC X(3)    VALUE '002'.           
027100         05  INF-DELETE-OK             PIC X(3)    VALUE '003'.           
027200                                                                          
027300*     --- PARAMETRAR TILL SUBPROGRAM                                      
027400 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
027500     SKIP3                                                                
027600 01  -COPY WZ01SUB                                                        
027700     EJECT                                                                
027800                                                                          
027900 01  FILLER                      PIC X(16)  VALUE 'W009EMAD-AREA'.        
028000     SKIP3                                                                
028100*01 -COPY W009EMAD                                                        
028200     EJECT                                                                
028300                                                                          
028400 01  FILLER                      PIC X(16)  VALUE 'WZ20TEFA-AREA'.        
028500     SKIP3                                                                
028600*01 -COPY WZ20TEFA                                                        
028700     EJECT                                                                
028800                                                                          
028900 01  FILLER                      PIC X(16)   VALUE 'SEC-AREA'.            
029000     SKIP3                                                                
029100*01 -COPY WSECAREA                                                        
029200     EJECT                                                                
029300                                                                          
029400*     --- PARAMETRAR TILL OLIKA AREOR                                     
029500 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
029600     SKIP3                                                                
029700 01  REQU-AREA.                                                           
029800*    03  -COPY WZ01REQU                                                   
029900*    03  -COPY WZ0402I1                                                   
030000     EJECT                                                                
030100                                                                          
030200 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
030300     SKIP3                                                                
030400 01  RESP-AREA.                                                           
030500*    03  -COPY WZ01RESP                                                   
030600*    03  -COPY WZ0402O1                                                   
030700     EJECT                                                                
030800                                                                          
030900 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
031000       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
031100                                                                          
031200 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
031300 01  DB2-WS.                                                              
031400     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
031500         88  LINES-FOUND                     VALUE 000.                   
031600         88  LINES-MISSING                   VALUE 100.                   
031700         88  RESOURCE-WRONG                  VALUE 904.                   
031800     03  GOOD-SQLCODECODES.                                               
031900         05  GOOD-SQLCODE OCCURS 5                                        
032000             INDEXED BY SQLCODE-IX PIC 9(3).                              
032100     EJECT                                                                
032200 01  FILLER                      PIC X(16)   VALUE 'TZ4DIRU-AREA'.        
032300*01  -COPY TZ4DIRU -PRE DIRU-                                             
032400                                                                          
032500     EXEC SQL INCLUDE TZ4DIRU END-EXEC.                                   
032600     EJECT                                                                
032700 LINKAGE SECTION.                                                         
032800     EJECT                                                                
032900 PROCEDURE DIVISION.                                                      
033000 MAIN SECTION.                                                            
033100                                                                          
033200     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
033300     IF SUB-KDRC = 0                                                      
033400       PERFORM A-INIT                                                     
033500       PERFORM B-CHECK-KEYS                                               
033600       IF KEYS-OK                                                         
033700         PERFORM F-READ-SHOW-INFO                                         
033800       END-IF                                                             
033900       PERFORM S02-RETURN-RESPONSE                                        
034000     END-IF                                                               
034100                                                                          
034200     MOVE ZERO TO RETURN-CODE                                             
034300     GOBACK                                                               
034400     .                                                                    
034500     EJECT                                                                
034600 A-INIT SECTION.                                                          
034700                                                                          
034800     INITIALIZE GOOD-SQLCODECODES                                         
034900     MOVE ALL '+' TO RESP-AREA                                            
035000     MOVE FUNCTION CURRENT-DATE (3:6) TO WS-CURRENT-DATE                  
035100     MOVE SPACE TO RESP-IDMSG-ERROR                                       
035200                   RESP-IDELMT-ERROR                                      
035300                   RESP-IDMSG-INFO                                        
035400     .                                                                    
035500*** - CHECK REQUESTED KEYS AND COMPULSORY FIELDS                          
035600 B-CHECK-KEYS SECTION.                                                    
035700                                                                          
035800     MOVE YES TO KEYS-SW                                                  
035900     MOVE REQU-KDPGMACT TO ACTION-CODE-SW                                 
036000                                                                          
036100     IF REQU-IDMSGVER NUMERIC                                             
036200     AND ACT-CODE-VALID                                                   
036300     AND REQU-IDOUTTYPE-KEY > SPACE AND NOT = ALL '+'                     
036400     AND REQU-IDOUTREC-FROM-KEY >= SPACE AND NOT = ALL '+'                
036500     AND REQU-IDOUTREC-TO-KEY >= SPACE AND NOT = ALL '+'                  
036600     AND REQU-IDOUTREC-FROM-KEY <= REQU-IDOUTREC-TO-KEY                   
036700       CONTINUE                                                           
036800     ELSE                                                                 
036900       MOVE NOO TO KEYS-SW                                                
037000     END-IF                                                               
037100                                                                          
037200     IF KEYS-WRONG                                                        
037300       MOVE ERR-INVALID-KEY TO RESP-IDMSG-ERROR                           
037400                                                                          
037500       IF ACT-CODE-VALID                                                  
037600         CONTINUE                                                         
037700       ELSE                                                               
037800         MOVE ERR-SYSTEM-ERROR TO RESP-IDMSG-ERROR                        
037900         MOVE 'KDPGMACT'       TO RESP-IDELMT-ERROR                       
038000       END-IF                                                             
038100                                                                          
038200       IF REQU-IDMSGVER NUMERIC                                           
038300         CONTINUE                                                         
038400       ELSE                                                               
038500         MOVE ERR-SYSTEM-ERROR TO RESP-IDMSG-ERROR                        
038600         MOVE 'IDMSGVER'       TO RESP-IDELMT-ERROR                       
038700       END-IF                                                             
038800                                                                          
038900       IF  REQU-IDUSER > SPACE                                            
039000       AND REQU-IDUSER NOT = ALL '+'                                      
039100         CONTINUE                                                         
039200       ELSE                                                               
039300         MOVE ERR-SYSTEM-ERROR TO RESP-IDMSG-ERROR                        
039400         MOVE 'IDUSER' TO RESP-IDELMT-ERROR                               
039500       END-IF                                                             
039600                                                                          
039700     END-IF                                                               
039800     .                                                                    
039900*** - MOVE SEARCHING KEYS TO RESPOND                                      
040000 F-READ-SHOW-INFO SECTION.                                                
040100                                                                          
040200     MOVE REQU-IDOUTTYPE-KEY     TO RESP-IDOUTTYPE-KEY                    
040300     MOVE REQU-IDOUTREC-FROM-KEY TO RESP-IDOUTREC-FROM-KEY                
040400     MOVE REQU-IDOUTREC-TO-KEY   TO RESP-IDOUTREC-TO-KEY                  
040500                                                                          
040600     PERFORM FA-READ-BASICDATA                                            
040700     .                                                                    
040800*** - CHECK WHICH TYPE OF HANDLING DEPENDING ON REQUESTED TYPE            
040900 FA-READ-BASICDATA SECTION.                                               
041000                                                                          
041100     IF RESP-IDMSG-ERROR = SPACE                                          
041200       IF ACT-CODE-SEARCH                                                 
041300         PERFORM FAA-SEARCH-TZ4DIRU                                       
041400       ELSE                                                               
041500         PERFORM S03-CHECK-SECURITY                                       
041600         IF RESP-IDMSG-ERROR = SPACE                                      
041700           IF ACT-CODE-UPDATE                                             
041800             PERFORM S04-CHECK-REQU-DATA                                  
041900             IF RESP-IDMSG-ERROR = SPACE                                  
042000               PERFORM FAB-UPDATE-TZ4DIRU                                 
042100             END-IF                                                       
042200           ELSE                                                           
042300             IF ACT-CODE-INSERT                                           
042400               PERFORM S04-CHECK-REQU-DATA                                
042500               IF RESP-IDMSG-ERROR = SPACE                                
042600                 PERFORM FAC-INSERT-TZ4DIRU                               
042700               END-IF                                                     
042800             ELSE                                                         
042900               PERFORM FAD-DELETE-TZ4DIRU                                 
043000             END-IF                                                       
043100           END-IF                                                         
043200         END-IF                                                           
043300       END-IF                                                             
043400     END-IF                                                               
043500     .                                                                    
043600*** - SEARCH FOR RIGHT DISTRIBUTION RULE                                  
043700 FAA-SEARCH-TZ4DIRU SECTION.                                              
043800                                                                          
043900     PERFORM DB2-SELECT-TZ4DIRU-TAB                                       
044000                                                                          
044100     IF LINES-FOUND                                                       
044200       PERFORM S05-MOVE-SEARCH-TO-RESPOND                                 
044300     ELSE                                                                 
044400       MOVE ERR-FIELD-NOT-FOUND TO RESP-IDMSG-ERROR                       
044500       MOVE 'KEY'               TO RESP-IDELMT-ERROR                      
044600       PERFORM S07-SCRATCH-RESPOND                                        
044700     END-IF                                                               
044800     .                                                                    
044900 FAB-UPDATE-TZ4DIRU SECTION.                                              
045000                                                                          
045100     PERFORM DB2-SELECT-TZ4DIRU-TAB                                       
045200                                                                          
045300     IF LINES-FOUND                                                       
045400       MOVE WS-CURRENT-DATE TO MAP-TIUPPDAT                               
045500       PERFORM DB2-UPDATE-TZ4DIRU-TAB                                     
045600       MOVE INF-UPDATE-OK TO RESP-IDMSG-INFO                              
045700       MOVE DIRU-TIREGDAT TO MAP-TIREGDAT                                 
045800       PERFORM S06-MOVE-UPD-INS-TO-RESPOND                                
045900     ELSE                                                                 
046000       MOVE ERR-FIELD-NOT-FOUND TO RESP-IDMSG-ERROR                       
046100       MOVE 'KEY' TO RESP-IDELMT-ERROR                                    
046200     END-IF                                                               
046300     .                                                                    
046400 FAC-INSERT-TZ4DIRU SECTION.                                              
046500                                                                          
046600     PERFORM DB2-COUNT-TZ4DIRU-TAB                                        
046700                                                                          
046800     IF TZ4DIRU-COUNTER > ZERO                                            
046900       MOVE ERR-ALREADY-EXIST TO RESP-IDMSG-ERROR                         
047000       MOVE 'KEY' TO RESP-IDELMT-ERROR                                    
047100     ELSE                                                                 
047200       MOVE WS-CURRENT-DATE TO MAP-TIREGDAT                               
047300       MOVE ZERO TO MAP-TIUPPDAT                                          
047400       PERFORM DB2-INSERT-TZ4DIRU-TAB                                     
047500       PERFORM S06-MOVE-UPD-INS-TO-RESPOND                                
047600       MOVE INF-INSERT-OK TO RESP-IDMSG-INFO                              
047700     END-IF                                                               
047800     .                                                                    
047900 FAD-DELETE-TZ4DIRU SECTION.                                              
048000                                                                          
048100     PERFORM DB2-SELECT-TZ4DIRU-TAB                                       
048200                                                                          
048300     IF LINES-FOUND                                                       
048400       PERFORM DB2-DELETE-TZ4DIRU-TAB                                     
048500       MOVE INF-DELETE-OK TO RESP-IDMSG-INFO                              
048600       PERFORM S07-SCRATCH-RESPOND                                        
048700     ELSE                                                                 
048800       MOVE ERR-FIELD-NOT-FOUND TO RESP-IDMSG-ERROR                       
048900       MOVE 'KEY' TO RESP-IDELMT-ERROR                                    
049000     END-IF                                                               
049100     .                                                                    
049200                                                                          
049300*    --- DISPATCHER SECTIONS                                              
049400 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
049500                                                                          
049600     MOVE 'GETARG'                   TO SUB-KDFUNC                        
049700     MOVE WS-ADRESS                  TO SUB-ADDISPABS                     
049800     MOVE LENGTH OF REQU-AREA        TO SUB-KVDLEN                        
049900                                                                          
050000     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
050100                                                                          
050200     IF SUB-KDRC > 0                                                      
050300       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
050400       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
050500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
050600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
050700     END-IF                                                               
050800     .                                                                    
050900     SKIP3                                                                
051000 S02-RETURN-RESPONSE SECTION.                                             
051100                                                                          
051200     MOVE 'RETURN'                   TO SUB-KDFUNC                        
051300     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
051400                                                                          
051500     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
051600                                                                          
051700     IF SUB-KDRC > 0                                                      
051800       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
051900       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
052000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
052100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
052200     END-IF                                                               
052300     .                                                                    
052400 S03-CHECK-SECURITY SECTION.                                              
052500                                                                          
052600     MOVE REQU-IDUSER            TO SEC-IDUSER                            
052700     MOVE IDSYSTEM               TO SEC-IDTRANS                           
052800     MOVE REQU-IDOUTREC-FROM-KEY TO SEC-IDKEY                             
052900                                                                          
053000     CALL WSECURIT USING SEC-IDUSER SEC-IDTRANS                           
053100                         SEC-IDKEY  SEC-KDSVAR                            
053200                                                                          
053300     IF SEC-KDSVAR > SPACE                                                
053400       MOVE ERR-AUTHORIZATION-MISSING TO RESP-IDMSG-ERROR                 
053500     END-IF                                                               
053600     .                                                                    
053700*** - VALIDATE REQUESTED FIELDS FOR UPDATE/INSERT ON DISTRIBUTION         
053800***   RULES.                                                              
053900 S04-CHECK-REQU-DATA SECTION.                                             
054000                                                                          
054100     PERFORM S04A-COMPULSORY-FIELDS                                       
054200                                                                          
054300     IF RESP-IDMSG-ERROR = SPACE                                          
054400       PERFORM S04B-VALIDATE-KDOUTMETH                                    
054500     END-IF                                                               
054600                                                                          
054700     IF RESP-IDMSG-ERROR = SPACE                                          
054800       PERFORM S04C-RELATION-CNTL-KDOUTMETH                               
054900     END-IF                                                               
055000                                                                          
055100     IF RESP-IDMSG-ERROR = SPACE                                          
055200       PERFORM S04D-RELATION-CNTL-IDOUTDEST                               
055300     END-IF                                                               
055400                                                                          
055500     IF RESP-IDMSG-ERROR = SPACE                                          
055600       PERFORM S04E-RELATION-CNTL-KVCOPIES                                
055700     END-IF                                                               
055800                                                                          
055900     IF RESP-IDMSG-ERROR = SPACE                                          
056000       PERFORM S04F-RELATION-CNTL-FLCARRCNTL                              
056100     END-IF                                                               
056200                                                                          
056300     IF RESP-IDMSG-ERROR = SPACE                                          
056400       PERFORM S04G-RELATION-CNTL-IDPFDEF                                 
056500     END-IF                                                               
056600                                                                          
056700     IF RESP-IDMSG-ERROR = SPACE                                          
056800       PERFORM S04H-RELATION-CNTL-IDFORMSNM                               
056900     END-IF                                                               
057000                                                                          
057100     IF RESP-IDMSG-ERROR = SPACE                                          
057200       PERFORM S04I-RELATION-CNTL-TEVCOMST                                
057300     END-IF                                                               
057400                                                                          
057500     IF RESP-IDMSG-ERROR = SPACE                                          
057600       PERFORM S04J-RELATION-CNTL-IDVCINIT                                
057700     END-IF                                                               
057800                                                                          
057900     IF RESP-IDMSG-ERROR = SPACE                                          
058000       PERFORM S04K-RELATION-CNTL-TEFAX                                   
058100     END-IF                                                               
058200                                                                          
058300     IF RESP-IDMSG-ERROR = SPACE                                          
058400       PERFORM S04M-RELATION-CNTL-IDMAIL                                  
058500     END-IF                                                               
058600                                                                          
058700     IF RESP-IDMSG-ERROR = SPACE                                          
058800       PERFORM S04N-RELATION-CNTL-FLACIF                                  
058900     END-IF                                                               
059000                                                                          
059100     IF RESP-IDMSG-ERROR = SPACE                                          
059200       PERFORM S04P-VALIDATE-TENOTE                                       
059300     END-IF                                                               
059400                                                                          
059500     IF RESP-IDMSG-ERROR = SPACE                                          
059600       PERFORM S04L-MOVE-TO-MAP                                           
059700     END-IF                                                               
059800     .                                                                    
059900*** - KVDAGAR-RESEND IS A COMPULSORY FIELD                                
060000 S04A-COMPULSORY-FIELDS SECTION.                                          
060100                                                                          
060200     IF REQU-KVDAGAR-RESEND NUMERIC                                       
060300       CONTINUE                                                           
060400     ELSE                                                                 
060500       MOVE ERR-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                       
060600       MOVE 'KVDAGAR-RESEND' TO RESP-IDELMT-ERROR                         
060700     END-IF                                                               
060800     .                                                                    
060900*** - VALIDATE KDOUTMETH                                                  
061000 S04B-VALIDATE-KDOUTMETH SECTION.                                         
061100                                                                          
061200     MOVE 1 TO IX                                                         
061300     PERFORM UNTIL IX > 15                                                
061400     OR RESP-IDMSG-ERROR > SPACE                                          
061500       IF REQU-KDOUTMETH(IX) > SPACE AND NOT = ALL '+'                    
061600         MOVE REQU-KDOUTMETH(IX) TO KDOUTMETH-SW                          
061700         IF KDOUTMETH-VALID                                               
061800           CONTINUE                                                       
061900         ELSE                                                             
062000           MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                     
062100           MOVE 'KDOUTMETH*' TO RESP-IDELMT-ERROR                         
062200           MOVE IX TO RESP-IDELMT-ERROR(11:1)                             
062300         END-IF                                                           
062400       ELSE                                                               
062500         IF REQU-KDOUTMETH(IX) = ALL '+'                                  
062600           MOVE SPACE TO REQU-KDOUTMETH(IX)                               
062700         END-IF                                                           
062800       END-IF                                                             
062900       ADD 1 TO IX                                                        
063000     END-PERFORM                                                          
063100     .                                                                    
063200*** - KDOUTMETH RELATION CONTROL                                          
063300 S04C-RELATION-CNTL-KDOUTMETH SECTION.                                    
063400                                                                          
063500     MOVE 2 TO IX                                                         
063600     MOVE 1 TO IX2                                                        
063700     PERFORM UNTIL IX > 15                                                
063800     OR RESP-IDMSG-ERROR > SPACE                                          
063900       IF REQU-KDOUTMETH(IX) > SPACE                                      
064000       AND REQU-KDOUTMETH(IX2) = SPACE                                    
064100         MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                       
064200         MOVE 'KDOUTMETH*' TO RESP-IDELMT-ERROR                           
064300         MOVE IX TO RESP-IDELMT-ERROR(11:1)                               
064400       END-IF                                                             
064500       ADD 1 TO IX IX2                                                    
064600     END-PERFORM                                                          
064700     .                                                                    
064800*** - IDOUTDEST RELATION CONTROL                                          
064900 S04D-RELATION-CNTL-IDOUTDEST SECTION.                                    
065000                                                                          
065100     MOVE 1 TO IX                                                         
065200     PERFORM UNTIL IX > 15                                                
065300     OR RESP-IDMSG-ERROR > SPACE                                          
065400       IF REQU-IDOUTDEST(IX) > SPACE AND NOT = ALL '+'                    
065500* CONTROL WHEN DEST SPECIFIED - METHOD MUST ALSO BE SPECIFIED             
065600         IF REQU-KDOUTMETH(IX) = SPACE                                    
065700           MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                     
065800           MOVE 'IDOUTDEST*' TO RESP-IDELMT-ERROR                         
065900           MOVE IX TO RESP-IDELMT-ERROR(11:1)                             
066000         END-IF                                                           
066100* CONTROL WHEN METHOD PRINT, VCOM OR EDI - MAX 8 CHAR DEST                
066200         IF REQU-KDOUTMETH(IX) = WS-PRINT                                 
066300         OR WS-VCOM OR WS-EDI                                             
066400           IF REQU-IDOUTDEST(IX)(9:1) > SPACE                             
066500             MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                   
066600             MOVE 'IDOUTDEST*' TO RESP-IDELMT-ERROR                       
066700             MOVE IX TO RESP-IDELMT-ERROR(11:1)                           
066800           END-IF                                                         
066900         END-IF                                                           
067000* CONTROL WHEN METHOD FAX                                                 
067100         IF REQU-KDOUTMETH(IX) = WS-FAX                                   
067200           MOVE REQU-IDOUTDEST(IX) TO TEFA-IDTFN-IDTFX                    
067300           CALL WZ20TEFA USING TEFA-WZ20TEFA                              
067400           IF TEFA-KDRC > 4                                               
067500             MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                   
067600             MOVE 'IDOUTDEST*' TO RESP-IDELMT-ERROR                       
067700             MOVE IX TO RESP-IDELMT-ERROR(11:1)                           
067800           ELSE                                                           
067900*            GET COMPLETED FAX NUMBER                                     
068000             MOVE TEFA-IDTFN-IDTFX TO REQU-IDOUTDEST(IX)                  
068100           END-IF                                                         
068200         END-IF                                                           
068300* CONTROL WHEN METHOD MAIL                                                
068400         IF REQU-KDOUTMETH(IX) = WS-MAIL                                  
068500           MOVE REQU-IDOUTDEST(IX) TO EMAD-IDMAIL                         
068600           CALL W009EMAD USING EMAD-W009EMAD                              
068700           IF EMAD-KDSVAR > SPACE                                         
068800             MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                   
068900             MOVE 'IDOUTDEST*' TO RESP-IDELMT-ERROR                       
069000             MOVE IX TO RESP-IDELMT-ERROR(11:1)                           
069100           END-IF                                                         
069200         END-IF                                                           
069300* CONTROL WHEN METHOD ONDEMAND                                            
069400         IF REQU-KDOUTMETH(IX) = WS-ONDEMAND                              
069500           IF REQU-IDFORMSNM(IX) > SPACE AND NOT = ALL '+'                
069600             MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                   
069700             MOVE 'IDOUTDEST*' TO RESP-IDELMT-ERROR                       
069800             MOVE IX TO RESP-IDELMT-ERROR(11:1)                           
069900           END-IF                                                         
070000         END-IF                                                           
070100* CONTROL DESTINATION MUST BE UNIQUE                                      
070200         IF IX = 2                                                        
070300           IF REQU-IDOUTDEST(IX - 1) = REQU-IDOUTDEST(IX)                 
070400             MOVE ERR-MUST-BE-UNIQUE TO RESP-IDMSG-ERROR                  
070500             MOVE 'IDOUTDEST' TO RESP-IDELMT-ERROR                        
070600           END-IF                                                         
070700         END-IF                                                           
070800         IF IX = 3                                                        
070900           IF REQU-IDOUTDEST(IX - 1) = REQU-IDOUTDEST(IX)                 
071000           OR REQU-IDOUTDEST(IX - 2) = REQU-IDOUTDEST(IX)                 
071100             MOVE ERR-MUST-BE-UNIQUE TO RESP-IDMSG-ERROR                  
071200             MOVE 'IDOUTDEST' TO RESP-IDELMT-ERROR                        
071300           END-IF                                                         
071400         END-IF                                                           
071500         IF IX = 4                                                        
071600           IF REQU-IDOUTDEST(IX - 1) = REQU-IDOUTDEST(IX)                 
071700           OR REQU-IDOUTDEST(IX - 2) = REQU-IDOUTDEST(IX)                 
071800           OR REQU-IDOUTDEST(IX - 3) = REQU-IDOUTDEST(IX)                 
071900             MOVE ERR-MUST-BE-UNIQUE TO RESP-IDMSG-ERROR                  
072000             MOVE 'IDOUTDEST' TO RESP-IDELMT-ERROR                        
072100           END-IF                                                         
072200         END-IF                                                           
072300         IF IX = 5                                                        
072400           IF REQU-IDOUTDEST(IX - 1) = REQU-IDOUTDEST(IX)                 
072500           OR REQU-IDOUTDEST(IX - 2) = REQU-IDOUTDEST(IX)                 
072600           OR REQU-IDOUTDEST(IX - 3) = REQU-IDOUTDEST(IX)                 
072700           OR REQU-IDOUTDEST(IX - 4) = REQU-IDOUTDEST(IX)                 
072800             MOVE ERR-MUST-BE-UNIQUE TO RESP-IDMSG-ERROR                  
072900             MOVE 'IDOUTDEST' TO RESP-IDELMT-ERROR                        
073000           END-IF                                                         
073100         END-IF                                                           
073200         IF IX = 6                                                        
073300           IF REQU-IDOUTDEST(IX - 1) = REQU-IDOUTDEST(IX)                 
073400           OR REQU-IDOUTDEST(IX - 2) = REQU-IDOUTDEST(IX)                 
073500           OR REQU-IDOUTDEST(IX - 3) = REQU-IDOUTDEST(IX)                 
073600           OR REQU-IDOUTDEST(IX - 4) = REQU-IDOUTDEST(IX)                 
073700           OR REQU-IDOUTDEST(IX - 5) = REQU-IDOUTDEST(IX)                 
073800             MOVE ERR-MUST-BE-UNIQUE TO RESP-IDMSG-ERROR                  
073900             MOVE 'IDOUTDEST' TO RESP-IDELMT-ERROR                        
074000           END-IF                                                         
074100         END-IF                                                           
074200         IF IX = 7                                                        
074300           IF REQU-IDOUTDEST(IX - 1) = REQU-IDOUTDEST(IX)                 
074400           OR REQU-IDOUTDEST(IX - 2) = REQU-IDOUTDEST(IX)                 
074500           OR REQU-IDOUTDEST(IX - 3) = REQU-IDOUTDEST(IX)                 
074600           OR REQU-IDOUTDEST(IX - 4) = REQU-IDOUTDEST(IX)                 
074700           OR REQU-IDOUTDEST(IX - 5) = REQU-IDOUTDEST(IX)                 
074800           OR REQU-IDOUTDEST(IX - 6) = REQU-IDOUTDEST(IX)                 
074900             MOVE ERR-MUST-BE-UNIQUE TO RESP-IDMSG-ERROR                  
075000             MOVE 'IDOUTDEST' TO RESP-IDELMT-ERROR                        
075100           END-IF                                                         
075200         END-IF                                                           
075300         IF IX = 8                                                        
075400           IF REQU-IDOUTDEST(IX - 1) = REQU-IDOUTDEST(IX)                 
075500           OR REQU-IDOUTDEST(IX - 2) = REQU-IDOUTDEST(IX)                 
075600           OR REQU-IDOUTDEST(IX - 3) = REQU-IDOUTDEST(IX)                 
075700           OR REQU-IDOUTDEST(IX - 4) = REQU-IDOUTDEST(IX)                 
075800           OR REQU-IDOUTDEST(IX - 5) = REQU-IDOUTDEST(IX)                 
075900           OR REQU-IDOUTDEST(IX - 6) = REQU-IDOUTDEST(IX)                 
076000           OR REQU-IDOUTDEST(IX - 7) = REQU-IDOUTDEST(IX)                 
076100             MOVE ERR-MUST-BE-UNIQUE TO RESP-IDMSG-ERROR                  
076200             MOVE 'IDOUTDEST' TO RESP-IDELMT-ERROR                        
076300           END-IF                                                         
076400         END-IF                                                           
076500         IF IX = 9                                                        
076600           IF REQU-IDOUTDEST(IX - 1) = REQU-IDOUTDEST(IX)                 
076700           OR REQU-IDOUTDEST(IX - 2) = REQU-IDOUTDEST(IX)                 
076800           OR REQU-IDOUTDEST(IX - 3) = REQU-IDOUTDEST(IX)                 
076900           OR REQU-IDOUTDEST(IX - 4) = REQU-IDOUTDEST(IX)                 
077000           OR REQU-IDOUTDEST(IX - 5) = REQU-IDOUTDEST(IX)                 
077100           OR REQU-IDOUTDEST(IX - 6) = REQU-IDOUTDEST(IX)                 
077200           OR REQU-IDOUTDEST(IX - 7) = REQU-IDOUTDEST(IX)                 
077300           OR REQU-IDOUTDEST(IX - 8) = REQU-IDOUTDEST(IX)                 
077400             MOVE ERR-MUST-BE-UNIQUE TO RESP-IDMSG-ERROR                  
077500             MOVE 'IDOUTDEST' TO RESP-IDELMT-ERROR                        
077600           END-IF                                                         
077700         END-IF                                                           
077800         IF IX = 10                                                       
077900           IF REQU-IDOUTDEST(IX - 1) = REQU-IDOUTDEST(IX)                 
078000           OR REQU-IDOUTDEST(IX - 2) = REQU-IDOUTDEST(IX)                 
078100           OR REQU-IDOUTDEST(IX - 3) = REQU-IDOUTDEST(IX)                 
078200           OR REQU-IDOUTDEST(IX - 4) = REQU-IDOUTDEST(IX)                 
078300           OR REQU-IDOUTDEST(IX - 5) = REQU-IDOUTDEST(IX)                 
078400           OR REQU-IDOUTDEST(IX - 6) = REQU-IDOUTDEST(IX)                 
078500           OR REQU-IDOUTDEST(IX - 7) = REQU-IDOUTDEST(IX)                 
078600           OR REQU-IDOUTDEST(IX - 8) = REQU-IDOUTDEST(IX)                 
078700           OR REQU-IDOUTDEST(IX - 9) = REQU-IDOUTDEST(IX)                 
078800             MOVE ERR-MUST-BE-UNIQUE TO RESP-IDMSG-ERROR                  
078900             MOVE 'IDOUTDEST' TO RESP-IDELMT-ERROR                        
079000           END-IF                                                         
079100         END-IF                                                           
079200         IF IX = 11                                                       
079300           IF REQU-IDOUTDEST(IX - 1) = REQU-IDOUTDEST(IX)                 
079400           OR REQU-IDOUTDEST(IX - 2) = REQU-IDOUTDEST(IX)                 
079500           OR REQU-IDOUTDEST(IX - 3) = REQU-IDOUTDEST(IX)                 
079600           OR REQU-IDOUTDEST(IX - 4) = REQU-IDOUTDEST(IX)                 
079700           OR REQU-IDOUTDEST(IX - 5) = REQU-IDOUTDEST(IX)                 
079800           OR REQU-IDOUTDEST(IX - 6) = REQU-IDOUTDEST(IX)                 
079900           OR REQU-IDOUTDEST(IX - 7) = REQU-IDOUTDEST(IX)                 
080000           OR REQU-IDOUTDEST(IX - 8) = REQU-IDOUTDEST(IX)                 
080100           OR REQU-IDOUTDEST(IX - 9) = REQU-IDOUTDEST(IX)                 
080200           OR REQU-IDOUTDEST(IX - 10) = REQU-IDOUTDEST(IX)                
080300             MOVE ERR-MUST-BE-UNIQUE TO RESP-IDMSG-ERROR                  
080400             MOVE 'IDOUTDEST' TO RESP-IDELMT-ERROR                        
080500           END-IF                                                         
080600         END-IF                                                           
080700         IF IX = 12                                                       
080800           IF REQU-IDOUTDEST(IX - 1) = REQU-IDOUTDEST(IX)                 
080900           OR REQU-IDOUTDEST(IX - 2) = REQU-IDOUTDEST(IX)                 
081000           OR REQU-IDOUTDEST(IX - 3) = REQU-IDOUTDEST(IX)                 
081100           OR REQU-IDOUTDEST(IX - 4) = REQU-IDOUTDEST(IX)                 
081200           OR REQU-IDOUTDEST(IX - 5) = REQU-IDOUTDEST(IX)                 
081300           OR REQU-IDOUTDEST(IX - 6) = REQU-IDOUTDEST(IX)                 
081400           OR REQU-IDOUTDEST(IX - 7) = REQU-IDOUTDEST(IX)                 
081500           OR REQU-IDOUTDEST(IX - 8) = REQU-IDOUTDEST(IX)                 
081600           OR REQU-IDOUTDEST(IX - 9) = REQU-IDOUTDEST(IX)                 
081700           OR REQU-IDOUTDEST(IX - 10) = REQU-IDOUTDEST(IX)                
081800           OR REQU-IDOUTDEST(IX - 11) = REQU-IDOUTDEST(IX)                
081900             MOVE ERR-MUST-BE-UNIQUE TO RESP-IDMSG-ERROR                  
082000             MOVE 'IDOUTDEST' TO RESP-IDELMT-ERROR                        
082100           END-IF                                                         
082200         END-IF                                                           
082300         IF IX = 13                                                       
082400           IF REQU-IDOUTDEST(IX - 1) = REQU-IDOUTDEST(IX)                 
082500           OR REQU-IDOUTDEST(IX - 2) = REQU-IDOUTDEST(IX)                 
082600           OR REQU-IDOUTDEST(IX - 3) = REQU-IDOUTDEST(IX)                 
082700           OR REQU-IDOUTDEST(IX - 4) = REQU-IDOUTDEST(IX)                 
082800           OR REQU-IDOUTDEST(IX - 5) = REQU-IDOUTDEST(IX)                 
082900           OR REQU-IDOUTDEST(IX - 6) = REQU-IDOUTDEST(IX)                 
083000           OR REQU-IDOUTDEST(IX - 7) = REQU-IDOUTDEST(IX)                 
083100           OR REQU-IDOUTDEST(IX - 8) = REQU-IDOUTDEST(IX)                 
083200           OR REQU-IDOUTDEST(IX - 9) = REQU-IDOUTDEST(IX)                 
083300           OR REQU-IDOUTDEST(IX - 10) = REQU-IDOUTDEST(IX)                
083400           OR REQU-IDOUTDEST(IX - 11) = REQU-IDOUTDEST(IX)                
083500           OR REQU-IDOUTDEST(IX - 12) = REQU-IDOUTDEST(IX)                
083600             MOVE ERR-MUST-BE-UNIQUE TO RESP-IDMSG-ERROR                  
083700             MOVE 'IDOUTDEST' TO RESP-IDELMT-ERROR                        
083800           END-IF                                                         
083900         END-IF                                                           
084000         IF IX = 14                                                       
084100           IF REQU-IDOUTDEST(IX - 1) = REQU-IDOUTDEST(IX)                 
084200           OR REQU-IDOUTDEST(IX - 2) = REQU-IDOUTDEST(IX)                 
084300           OR REQU-IDOUTDEST(IX - 3) = REQU-IDOUTDEST(IX)                 
084400           OR REQU-IDOUTDEST(IX - 4) = REQU-IDOUTDEST(IX)                 
084500           OR REQU-IDOUTDEST(IX - 5) = REQU-IDOUTDEST(IX)                 
084600           OR REQU-IDOUTDEST(IX - 6) = REQU-IDOUTDEST(IX)                 
084700           OR REQU-IDOUTDEST(IX - 7) = REQU-IDOUTDEST(IX)                 
084800           OR REQU-IDOUTDEST(IX - 8) = REQU-IDOUTDEST(IX)                 
084900           OR REQU-IDOUTDEST(IX - 9) = REQU-IDOUTDEST(IX)                 
085000           OR REQU-IDOUTDEST(IX - 10) = REQU-IDOUTDEST(IX)                
085100           OR REQU-IDOUTDEST(IX - 11) = REQU-IDOUTDEST(IX)                
085200           OR REQU-IDOUTDEST(IX - 12) = REQU-IDOUTDEST(IX)                
085300           OR REQU-IDOUTDEST(IX - 13) = REQU-IDOUTDEST(IX)                
085400             MOVE ERR-MUST-BE-UNIQUE TO RESP-IDMSG-ERROR                  
085500             MOVE 'IDOUTDEST' TO RESP-IDELMT-ERROR                        
085600           END-IF                                                         
085700         END-IF                                                           
085800         IF IX = 15                                                       
085900           IF REQU-IDOUTDEST(IX - 1) = REQU-IDOUTDEST(IX)                 
086000           OR REQU-IDOUTDEST(IX - 2) = REQU-IDOUTDEST(IX)                 
086100           OR REQU-IDOUTDEST(IX - 3) = REQU-IDOUTDEST(IX)                 
086200           OR REQU-IDOUTDEST(IX - 4) = REQU-IDOUTDEST(IX)                 
086300           OR REQU-IDOUTDEST(IX - 5) = REQU-IDOUTDEST(IX)                 
086400           OR REQU-IDOUTDEST(IX - 6) = REQU-IDOUTDEST(IX)                 
086500           OR REQU-IDOUTDEST(IX - 7) = REQU-IDOUTDEST(IX)                 
086600           OR REQU-IDOUTDEST(IX - 8) = REQU-IDOUTDEST(IX)                 
086700           OR REQU-IDOUTDEST(IX - 9) = REQU-IDOUTDEST(IX)                 
086800           OR REQU-IDOUTDEST(IX - 10) = REQU-IDOUTDEST(IX)                
086900           OR REQU-IDOUTDEST(IX - 11) = REQU-IDOUTDEST(IX)                
087000           OR REQU-IDOUTDEST(IX - 12) = REQU-IDOUTDEST(IX)                
087100           OR REQU-IDOUTDEST(IX - 13) = REQU-IDOUTDEST(IX)                
087200           OR REQU-IDOUTDEST(IX - 14) = REQU-IDOUTDEST(IX)                
087300             MOVE ERR-MUST-BE-UNIQUE TO RESP-IDMSG-ERROR                  
087400             MOVE 'IDOUTDEST' TO RESP-IDELMT-ERROR                        
087500           END-IF                                                         
087600         END-IF                                                           
087700       ELSE                                                               
087800         IF REQU-KDOUTMETH(IX) > SPACE                                    
087900         AND NOT = WS-ONDEMAND AND NOT = WS-SAVE                          
088000           MOVE ERR-MUST-BE-ENTERED TO RESP-IDMSG-ERROR                   
088100           MOVE 'IDOUTDEST*' TO RESP-IDELMT-ERROR                         
088200           MOVE IX TO RESP-IDELMT-ERROR(11:1)                             
088300         ELSE                                                             
088400           IF REQU-IDOUTDEST(IX) = ALL '+'                                
088500             MOVE SPACE TO REQU-IDOUTDEST(IX)                             
088600           END-IF                                                         
088700         END-IF                                                           
088800       END-IF                                                             
088900       ADD 1 TO IX                                                        
089000     END-PERFORM                                                          
089100     .                                                                    
089200*** - KVCOPIES RELATION CONTROL                                           
089300 S04E-RELATION-CNTL-KVCOPIES SECTION.                                     
089400                                                                          
089500     MOVE 1 TO IX                                                         
089600     PERFORM UNTIL IX > 15                                                
089700     OR RESP-IDMSG-ERROR > SPACE                                          
089800       IF REQU-KDOUTMETH(IX) = WS-PRINT                                   
089900         IF REQU-KVCOPIES(IX) NUMERIC                                     
090000           IF REQU-KVCOPIES(IX) = '0'                                     
090100             MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                   
090200             MOVE 'KVCOPIES*' TO RESP-IDELMT-ERROR                        
090300             MOVE IX TO RESP-IDELMT-ERROR(10:1)                           
090400           END-IF                                                         
090500         ELSE                                                             
090600           MOVE ERR-MUST-BE-NUMERIC TO RESP-IDMSG-ERROR                   
090700           MOVE 'KVCOPIES*' TO RESP-IDELMT-ERROR                          
090800           MOVE IX TO RESP-IDELMT-ERROR(10:1)                             
090900         END-IF                                                           
091000       ELSE                                                               
091100         IF REQU-KVCOPIES(IX) > '0'                                       
091200           MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                     
091300           MOVE 'KVCOPIES*' TO RESP-IDELMT-ERROR                          
091400           MOVE IX TO RESP-IDELMT-ERROR(10:1)                             
091500         ELSE                                                             
091600           MOVE SPACE TO REQU-KVCOPIES(IX)                                
091700         END-IF                                                           
091800       END-IF                                                             
091900       ADD 1 TO IX                                                        
092000     END-PERFORM                                                          
092100     .                                                                    
092200*** - FLCARRCNTL RELATION CONTROL                                         
092300 S04F-RELATION-CNTL-FLCARRCNTL SECTION.                                   
092400                                                                          
092500     MOVE 1 TO IX                                                         
092600     PERFORM UNTIL IX > 15                                                
092700     OR RESP-IDMSG-ERROR > SPACE                                          
092800       IF REQU-FLCARRCNTL(IX) = JA                                        
092900         IF REQU-KDOUTMETH(IX) = WS-PRINT                                 
093000         OR REQU-KDOUTMETH(IX) = WS-MAIL                                  
093100         OR REQU-KDOUTMETH(IX) = WS-FAX                                   
093200         OR REQU-KDOUTMETH(IX) = WS-ONDEMAND                              
093300         OR REQU-KDOUTMETH(IX) = WS-GET-IT                                
093400           CONTINUE                                                       
093500         ELSE                                                             
093600           MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                     
093700           MOVE 'FLCARRCNTL*' TO RESP-IDELMT-ERROR                        
093800           MOVE IX TO RESP-IDELMT-ERROR(12:1)                             
093900         END-IF                                                           
094000       ELSE                                                               
094100         IF REQU-KDOUTMETH(IX) > SPACE                                    
094200           MOVE NOO TO REQU-FLCARRCNTL(IX)                                
094300         ELSE                                                             
094400           MOVE SPACE TO REQU-FLCARRCNTL(IX)                              
094500         END-IF                                                           
094600       END-IF                                                             
094700       ADD 1 TO IX                                                        
094800     END-PERFORM                                                          
094900     .                                                                    
095000*** - IDPFDEF RELATION CONTROL                                            
095100 S04G-RELATION-CNTL-IDPFDEF SECTION.                                      
095200                                                                          
095300     MOVE 1 TO IX                                                         
095400     PERFORM UNTIL IX > 15                                                
095500     OR RESP-IDMSG-ERROR > SPACE                                          
095600       IF REQU-IDPFDEF(IX) > SPACE AND NOT = ALL '+'                      
095700         IF REQU-KDOUTMETH(IX) = WS-PRINT                                 
095800         OR REQU-KDOUTMETH(IX) = WS-FAX                                   
095900         OR REQU-KDOUTMETH(IX) = WS-MAIL                                  
096000         OR REQU-KDOUTMETH(IX) = WS-SAVE                                  
096100         OR REQU-KDOUTMETH(IX) = WS-GET-IT                                
096200         OR REQU-KDOUTMETH(IX) = WS-ONDEMAND                              
096300*          -- CHECK FOR ALPHANUMERIC LAYOUT NAME                          
096400           MOVE REQU-IDPFDEF(IX) TO WS-IDPFDEF                            
096500           INSPECT WS-IDPFDEF CONVERTING                                  
096600                              WS-VALID-IDPFDEF TO WS-TEST-IDPFDEF         
096700           IF WS-IDPFDEF = SPACE                                          
096800*          -- ALSO ALLOW A TYPE SUFFIX (.XXX) FOR MAIL                    
096900           OR (REQU-KDOUTMETH(IX) = WS-MAIL AND WS-IDPFDEF = '.')         
097000*          -- ALSO ALLOW AN *-PREFIX TO FIX PDF LANDSCAPE PROBLEM         
097100           OR (REQU-KDOUTMETH(IX) = WS-MAIL AND WS-IDPFDEF = '*')         
097200             CONTINUE                                                     
097300           ELSE                                                           
097400             MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                   
097500             MOVE 'IDPFDEF*' TO RESP-IDELMT-ERROR                         
097600             MOVE IX TO RESP-IDELMT-ERROR(9:1)                            
097700           END-IF                                                         
097800         ELSE                                                             
097900           MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                     
098000           MOVE 'IDPFDEF*' TO RESP-IDELMT-ERROR                           
098100           MOVE IX TO RESP-IDELMT-ERROR(9:1)                              
098200         END-IF                                                           
098300       ELSE                                                               
098400         IF REQU-IDPFDEF(IX) = ALL '+'                                    
098500           MOVE SPACE TO REQU-IDPFDEF(IX)                                 
098600         END-IF                                                           
098700       END-IF                                                             
098800       ADD 1 TO IX                                                        
098900     END-PERFORM                                                          
099000     .                                                                    
099100*** - IDFORMSNM RELATION CONTROL                                          
099200 S04H-RELATION-CNTL-IDFORMSNM SECTION.                                    
099300                                                                          
099400     MOVE 1 TO IX                                                         
099500     PERFORM UNTIL IX > 15                                                
099600     OR RESP-IDMSG-ERROR > SPACE                                          
099700       IF REQU-IDFORMSNM(IX) > SPACE AND NOT = ALL '+'                    
099800         IF REQU-KDOUTMETH(IX) = WS-PRINT                                 
099900         OR REQU-KDOUTMETH(IX) = WS-ONDEMAND                              
100000         OR REQU-KDOUTMETH(IX) = WS-MAIL                                  
100100         OR REQU-KDOUTMETH(IX) = WS-SAVE                                  
100200           CONTINUE                                                       
100300         ELSE                                                             
100400           MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                     
100500           MOVE 'IDFORMSNM*' TO RESP-IDELMT-ERROR                         
100600           MOVE IX TO RESP-IDELMT-ERROR(11:1)                             
100700         END-IF                                                           
100800       ELSE                                                               
100900         IF REQU-IDFORMSNM(IX) = ALL '+'                                  
101000           MOVE SPACE TO REQU-IDFORMSNM(IX)                               
101100         END-IF                                                           
101200       END-IF                                                             
101300       ADD 1 TO IX                                                        
101400     END-PERFORM                                                          
101500     .                                                                    
101600*** - TEVCOMST RELATION CONTROL                                           
101700 S04I-RELATION-CNTL-TEVCOMST SECTION.                                     
101800                                                                          
101900     MOVE 1 TO IX                                                         
102000     PERFORM UNTIL IX > 15                                                
102100     OR RESP-IDMSG-ERROR > SPACE                                          
102200       IF REQU-TEVCOMST(IX) > SPACE AND NOT = ALL '+'                     
102300         IF REQU-KDOUTMETH(IX) = WS-VCOM                                  
102400         OR REQU-KDOUTMETH(IX) = WS-EDI                                   
102500           CONTINUE                                                       
102600         ELSE                                                             
102700           MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                     
102800           MOVE 'TEVCOMST*' TO RESP-IDELMT-ERROR                          
102900           MOVE IX TO RESP-IDELMT-ERROR(10:1)                             
103000         END-IF                                                           
103100       ELSE                                                               
103200         IF REQU-TEVCOMST(IX) = ALL '+'                                   
103300           MOVE SPACE TO REQU-TEVCOMST(IX)                                
103400         END-IF                                                           
103500       END-IF                                                             
103600       ADD 1 TO IX                                                        
103700     END-PERFORM                                                          
103800     .                                                                    
103900*** - IDVCINIT RELATION CONTROL                                           
104000 S04J-RELATION-CNTL-IDVCINIT SECTION.                                     
104100                                                                          
104200     MOVE 1 TO IX                                                         
104300     PERFORM UNTIL IX > 15                                                
104400     OR RESP-IDMSG-ERROR > SPACE                                          
104500       IF REQU-IDVCINIT(IX) > SPACE AND NOT = ALL '+'                     
104600         IF REQU-KDOUTMETH(IX) = WS-VCOM                                  
104700         OR REQU-KDOUTMETH(IX) = WS-EDI                                   
104800           CONTINUE                                                       
104900         ELSE                                                             
105000           MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                     
105100           MOVE 'IDVCINIT*' TO RESP-IDELMT-ERROR                          
105200           MOVE IX TO RESP-IDELMT-ERROR(10:1)                             
105300         END-IF                                                           
105400       ELSE                                                               
105500         IF REQU-IDVCINIT(IX) = ALL '+'                                   
105600           MOVE SPACE TO REQU-IDVCINIT(IX)                                
105700         END-IF                                                           
105800       END-IF                                                             
105900       ADD 1 TO IX                                                        
106000     END-PERFORM                                                          
106100     .                                                                    
106200*** - TEFAX RELATION CONTROL                                              
106300 S04K-RELATION-CNTL-TEFAX SECTION.                                        
106400                                                                          
106500     MOVE NOO TO WS-TEFAX-MAY-BE-SPECIFIED                                
106600     MOVE 1 TO IX                                                         
106700     PERFORM UNTIL IX > 5                                                 
106800       IF REQU-KDOUTMETH(IX) = WS-FAX                                     
106900       OR REQU-KDOUTMETH(IX) = WS-MAIL                                    
107000         MOVE YES TO WS-TEFAX-MAY-BE-SPECIFIED                            
107100       END-IF                                                             
107200       ADD 1 TO IX                                                        
107300     END-PERFORM                                                          
107400                                                                          
107500     MOVE 1 TO IX                                                         
107600     PERFORM UNTIL IX > 5                                                 
107700     OR RESP-IDMSG-ERROR > SPACE                                          
107800       IF REQU-TEFAX(IX) > SPACE AND NOT = ALL '+'                        
107900         IF WS-TEFAX-MAY-BE-SPECIFIED = YES                               
108000           CONTINUE                                                       
108100         ELSE                                                             
108200           MOVE ERR-TEFAX-NOT-ALLOWED TO RESP-IDMSG-ERROR                 
108300           MOVE 'TEFAX*' TO RESP-IDELMT-ERROR                             
108400           MOVE IX TO RESP-IDELMT-ERROR(10:1)                             
108500         END-IF                                                           
108600       ELSE                                                               
108700         IF REQU-TEFAX(IX) = ALL '+'                                      
108800           MOVE SPACE TO REQU-TEFAX(IX)                                   
108900         END-IF                                                           
109000       END-IF                                                             
109100       ADD 1 TO IX                                                        
109200     END-PERFORM                                                          
109300                                                                          
109400     .                                                                    
109500*** - IDMAIL-SENDER RELATION CONTROL                                      
109600 S04M-RELATION-CNTL-IDMAIL SECTION.                                       
109700                                                                          
109800     MOVE NOO TO WS-IDMAIL-MAY-BE-SPECIFIED                               
109900     MOVE 1 TO IX                                                         
110000     PERFORM UNTIL IX > 15                                                
110100       IF REQU-KDOUTMETH(IX) = WS-FAX                                     
110200       OR REQU-KDOUTMETH(IX) = WS-MAIL                                    
110300         MOVE YES TO WS-IDMAIL-MAY-BE-SPECIFIED                           
110400       END-IF                                                             
110500       ADD 1 TO IX                                                        
110600     END-PERFORM                                                          
110700                                                                          
110800     IF REQU-IDMAIL-SENDER > SPACE AND NOT = ALL '+'                      
110900       IF WS-IDMAIL-MAY-BE-SPECIFIED = YES                                
111000         MOVE REQU-IDMAIL-SENDER TO EMAD-IDMAIL                           
111100         CALL W009EMAD USING EMAD-W009EMAD                                
111200         IF EMAD-KDSVAR > SPACE                                           
111300           MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                     
111400           MOVE 'IDMAIL-SENDER' TO RESP-IDELMT-ERROR                      
111500         END-IF                                                           
111600       ELSE                                                               
111700         MOVE ERR-IDMAIL-NOT-ALLOWED TO RESP-IDMSG-ERROR                  
111800         MOVE 'IDMAIL-SENDER' TO RESP-IDELMT-ERROR                        
111900       END-IF                                                             
112000     ELSE                                                                 
112100       IF REQU-IDMAIL-SENDER = ALL '+'                                    
112200         MOVE SPACE TO REQU-IDMAIL-SENDER                                 
112300       END-IF                                                             
112400     END-IF                                                               
112500     .                                                                    
112600                                                                          
112700*** - FLACIF RELATION CONTROL                                             
112800 S04N-RELATION-CNTL-FLACIF SECTION.                                       
112900                                                                          
113000     MOVE 1 TO IX                                                         
113100     PERFORM UNTIL IX > 15                                                
113200     OR RESP-IDMSG-ERROR > SPACE                                          
113300       IF REQU-FLACIF(IX) = JA                                            
113400         IF (REQU-KDOUTMETH(IX) = WS-PRINT                                
113500          OR REQU-KDOUTMETH(IX) = WS-ONDEMAND)                            
113600         AND REQU-IDPFDEF(IX) NOT = SPACE                                 
113700         AND REQU-IDPFDEF(IX) NOT = ALL '+'                               
113800           CONTINUE                                                       
113900         ELSE                                                             
114000           MOVE ERR-INVALID-FIELD TO RESP-IDMSG-ERROR                     
114100           MOVE 'FLACIF*' TO RESP-IDELMT-ERROR                            
114200           MOVE IX TO RESP-IDELMT-ERROR(12:1)                             
114300         END-IF                                                           
114400       ELSE                                                               
114500         IF REQU-KDOUTMETH(IX) > SPACE                                    
114600           MOVE NOO TO REQU-FLACIF(IX)                                    
114700         ELSE                                                             
114800           MOVE SPACE TO REQU-FLACIF(IX)                                  
114900         END-IF                                                           
115000       END-IF                                                             
115100       ADD 1 TO IX                                                        
115200     END-PERFORM                                                          
115300     .                                                                    
115400*** - VALIDATE TENOTE                                                     
115500 S04P-VALIDATE-TENOTE SECTION.                                            
115600                                                                          
115700     IF REQU-TENOTE = ALL '+'                                             
115800       MOVE SPACE                   TO REQU-TENOTE                        
115900     END-IF                                                               
116000     .                                                                    
116100*** - MOVE FIELDS TO MAP FIELDS                                           
116200 S04L-MOVE-TO-MAP SECTION.                                                
116300                                                                          
116400     MOVE 1 TO IX                                                         
116500     PERFORM UNTIL IX > 15                                                
116600       EVALUATE IX                                                        
116700         WHEN 1                                                           
116800           MOVE REQU-KDOUTMETH(IX)  TO MAP-KDOUTMETH-1                    
116900           MOVE REQU-IDOUTDEST(IX)  TO MAP-IDOUTDEST-1                    
117000           MOVE REQU-KVCOPIES(IX)   TO MAP-KVCOPIES-1                     
117100           MOVE REQU-FLCARRCNTL(IX) TO MAP-FLCARRCNTL-1                   
117200           MOVE REQU-IDPFDEF(IX)    TO MAP-IDPFDEF-1                      
117300           MOVE REQU-IDFORMSNM(IX)  TO MAP-IDFORMSNM-1                    
117400           MOVE REQU-TEVCOMST(IX)   TO MAP-TEVCOMST-1                     
117500           MOVE REQU-IDVCINIT(IX)   TO MAP-IDVCINIT-1                     
117600           MOVE REQU-TEFAX(IX)      TO MAP-TEFAX-1                        
117700           MOVE REQU-FLACIF(IX)     TO MAP-FLACIF-1                       
117800         WHEN 2                                                           
117900           MOVE REQU-KDOUTMETH(IX)  TO MAP-KDOUTMETH-2                    
118000           MOVE REQU-IDOUTDEST(IX)  TO MAP-IDOUTDEST-2                    
118100           MOVE REQU-KVCOPIES(IX)   TO MAP-KVCOPIES-2                     
118200           MOVE REQU-FLCARRCNTL(IX) TO MAP-FLCARRCNTL-2                   
118300           MOVE REQU-IDPFDEF(IX)    TO MAP-IDPFDEF-2                      
118400           MOVE REQU-IDFORMSNM(IX)  TO MAP-IDFORMSNM-2                    
118500           MOVE REQU-TEVCOMST(IX)   TO MAP-TEVCOMST-2                     
118600           MOVE REQU-IDVCINIT(IX)   TO MAP-IDVCINIT-2                     
118700           MOVE REQU-TEFAX(IX)      TO MAP-TEFAX-2                        
118800           MOVE REQU-FLACIF(IX)     TO MAP-FLACIF-2                       
118900         WHEN 3                                                           
119000           MOVE REQU-KDOUTMETH(IX)  TO MAP-KDOUTMETH-3                    
119100           MOVE REQU-IDOUTDEST(IX)  TO MAP-IDOUTDEST-3                    
119200           MOVE REQU-KVCOPIES(IX)   TO MAP-KVCOPIES-3                     
119300           MOVE REQU-FLCARRCNTL(IX) TO MAP-FLCARRCNTL-3                   
119400           MOVE REQU-IDPFDEF(IX)    TO MAP-IDPFDEF-3                      
119500           MOVE REQU-IDFORMSNM(IX)  TO MAP-IDFORMSNM-3                    
119600           MOVE REQU-TEVCOMST(IX)   TO MAP-TEVCOMST-3                     
119700           MOVE REQU-IDVCINIT(IX)   TO MAP-IDVCINIT-3                     
119800           MOVE REQU-TEFAX(IX)      TO MAP-TEFAX-3                        
119900           MOVE REQU-FLACIF(IX)     TO MAP-FLACIF-3                       
120000         WHEN 4                                                           
120100           MOVE REQU-KDOUTMETH(IX)  TO MAP-KDOUTMETH-4                    
120200           MOVE REQU-IDOUTDEST(IX)  TO MAP-IDOUTDEST-4                    
120300           MOVE REQU-KVCOPIES(IX)   TO MAP-KVCOPIES-4                     
120400           MOVE REQU-FLCARRCNTL(IX) TO MAP-FLCARRCNTL-4                   
120500           MOVE REQU-IDPFDEF(IX)    TO MAP-IDPFDEF-4                      
120600           MOVE REQU-IDFORMSNM(IX)  TO MAP-IDFORMSNM-4                    
120700           MOVE REQU-TEVCOMST(IX)   TO MAP-TEVCOMST-4                     
120800           MOVE REQU-IDVCINIT(IX)   TO MAP-IDVCINIT-4                     
120900           MOVE REQU-TEFAX(IX)      TO MAP-TEFAX-4                        
121000           MOVE REQU-FLACIF(IX)     TO MAP-FLACIF-4                       
121100         WHEN 5                                                           
121200           MOVE REQU-KDOUTMETH(IX)  TO MAP-KDOUTMETH-5                    
121300           MOVE REQU-IDOUTDEST(IX)  TO MAP-IDOUTDEST-5                    
121400           MOVE REQU-KVCOPIES(IX)   TO MAP-KVCOPIES-5                     
121500           MOVE REQU-FLCARRCNTL(IX) TO MAP-FLCARRCNTL-5                   
121600           MOVE REQU-IDPFDEF(IX)    TO MAP-IDPFDEF-5                      
121700           MOVE REQU-IDFORMSNM(IX)  TO MAP-IDFORMSNM-5                    
121800           MOVE REQU-TEVCOMST(IX)   TO MAP-TEVCOMST-5                     
121900           MOVE REQU-IDVCINIT(IX)   TO MAP-IDVCINIT-5                     
122000           MOVE REQU-TEFAX(IX)      TO MAP-TEFAX-5                        
122100           MOVE REQU-FLACIF(IX)     TO MAP-FLACIF-5                       
122200         WHEN 6                                                           
122300           MOVE REQU-KDOUTMETH(IX)  TO MAP-KDOUTMETH-6                    
122400           MOVE REQU-IDOUTDEST(IX)  TO MAP-IDOUTDEST-6                    
122500           MOVE REQU-KVCOPIES(IX)   TO MAP-KVCOPIES-6                     
122600           MOVE REQU-FLCARRCNTL(IX) TO MAP-FLCARRCNTL-6                   
122700           MOVE REQU-IDPFDEF(IX)    TO MAP-IDPFDEF-6                      
122800           MOVE REQU-IDFORMSNM(IX)  TO MAP-IDFORMSNM-6                    
122900           MOVE REQU-TEVCOMST(IX)   TO MAP-TEVCOMST-6                     
123000           MOVE REQU-IDVCINIT(IX)   TO MAP-IDVCINIT-6                     
123100           MOVE REQU-FLACIF(IX)     TO MAP-FLACIF-6                       
123200         WHEN 7                                                           
123300           MOVE REQU-KDOUTMETH(IX)  TO MAP-KDOUTMETH-7                    
123400           MOVE REQU-IDOUTDEST(IX)  TO MAP-IDOUTDEST-7                    
123500           MOVE REQU-KVCOPIES(IX)   TO MAP-KVCOPIES-7                     
123600           MOVE REQU-FLCARRCNTL(IX) TO MAP-FLCARRCNTL-7                   
123700           MOVE REQU-IDPFDEF(IX)    TO MAP-IDPFDEF-7                      
123800           MOVE REQU-IDFORMSNM(IX)  TO MAP-IDFORMSNM-7                    
123900           MOVE REQU-TEVCOMST(IX)   TO MAP-TEVCOMST-7                     
124000           MOVE REQU-IDVCINIT(IX)   TO MAP-IDVCINIT-7                     
124100           MOVE REQU-FLACIF(IX)     TO MAP-FLACIF-7                       
124200         WHEN 8                                                           
124300           MOVE REQU-KDOUTMETH(IX)  TO MAP-KDOUTMETH-8                    
124400           MOVE REQU-IDOUTDEST(IX)  TO MAP-IDOUTDEST-8                    
124500           MOVE REQU-KVCOPIES(IX)   TO MAP-KVCOPIES-8                     
124600           MOVE REQU-FLCARRCNTL(IX) TO MAP-FLCARRCNTL-8                   
124700           MOVE REQU-IDPFDEF(IX)    TO MAP-IDPFDEF-8                      
124800           MOVE REQU-IDFORMSNM(IX)  TO MAP-IDFORMSNM-8                    
124900           MOVE REQU-TEVCOMST(IX)   TO MAP-TEVCOMST-8                     
125000           MOVE REQU-IDVCINIT(IX)   TO MAP-IDVCINIT-8                     
125100           MOVE REQU-FLACIF(IX)     TO MAP-FLACIF-8                       
125200         WHEN 9                                                           
125300           MOVE REQU-KDOUTMETH(IX)  TO MAP-KDOUTMETH-9                    
125400           MOVE REQU-IDOUTDEST(IX)  TO MAP-IDOUTDEST-9                    
125500           MOVE REQU-KVCOPIES(IX)   TO MAP-KVCOPIES-9                     
125600           MOVE REQU-FLCARRCNTL(IX) TO MAP-FLCARRCNTL-9                   
125700           MOVE REQU-IDPFDEF(IX)    TO MAP-IDPFDEF-9                      
125800           MOVE REQU-IDFORMSNM(IX)  TO MAP-IDFORMSNM-9                    
125900           MOVE REQU-TEVCOMST(IX)   TO MAP-TEVCOMST-9                     
126000           MOVE REQU-IDVCINIT(IX)   TO MAP-IDVCINIT-9                     
126100           MOVE REQU-FLACIF(IX)     TO MAP-FLACIF-9                       
126200         WHEN 10                                                          
126300           MOVE REQU-KDOUTMETH(IX)  TO MAP-KDOUTMETH-10                   
126400           MOVE REQU-IDOUTDEST(IX)  TO MAP-IDOUTDEST-10                   
126500           MOVE REQU-KVCOPIES(IX)   TO MAP-KVCOPIES-10                    
126600           MOVE REQU-FLCARRCNTL(IX) TO MAP-FLCARRCNTL-10                  
126700           MOVE REQU-IDPFDEF(IX)    TO MAP-IDPFDEF-10                     
126800           MOVE REQU-IDFORMSNM(IX)  TO MAP-IDFORMSNM-10                   
126900           MOVE REQU-TEVCOMST(IX)   TO MAP-TEVCOMST-10                    
127000           MOVE REQU-IDVCINIT(IX)   TO MAP-IDVCINIT-10                    
127100           MOVE REQU-FLACIF(IX)     TO MAP-FLACIF-10                      
127200         WHEN 11                                                          
127300           MOVE REQU-KDOUTMETH(IX)  TO MAP-KDOUTMETH-11                   
127400           MOVE REQU-IDOUTDEST(IX)  TO MAP-IDOUTDEST-11                   
127500           MOVE REQU-KVCOPIES(IX)   TO MAP-KVCOPIES-11                    
127600           MOVE REQU-FLCARRCNTL(IX) TO MAP-FLCARRCNTL-11                  
127700           MOVE REQU-IDPFDEF(IX)    TO MAP-IDPFDEF-11                     
127800           MOVE REQU-IDFORMSNM(IX)  TO MAP-IDFORMSNM-11                   
127900           MOVE REQU-TEVCOMST(IX)   TO MAP-TEVCOMST-11                    
128000           MOVE REQU-IDVCINIT(IX)   TO MAP-IDVCINIT-11                    
128100           MOVE REQU-FLACIF(IX)     TO MAP-FLACIF-11                      
128200         WHEN 12                                                          
128300           MOVE REQU-KDOUTMETH(IX)  TO MAP-KDOUTMETH-12                   
128400           MOVE REQU-IDOUTDEST(IX)  TO MAP-IDOUTDEST-12                   
128500           MOVE REQU-KVCOPIES(IX)   TO MAP-KVCOPIES-12                    
128600           MOVE REQU-FLCARRCNTL(IX) TO MAP-FLCARRCNTL-12                  
128700           MOVE REQU-IDPFDEF(IX)    TO MAP-IDPFDEF-12                     
128800           MOVE REQU-IDFORMSNM(IX)  TO MAP-IDFORMSNM-12                   
128900           MOVE REQU-TEVCOMST(IX)   TO MAP-TEVCOMST-12                    
129000           MOVE REQU-IDVCINIT(IX)   TO MAP-IDVCINIT-12                    
129100           MOVE REQU-FLACIF(IX)     TO MAP-FLACIF-12                      
129200         WHEN 13                                                          
129300           MOVE REQU-KDOUTMETH(IX)  TO MAP-KDOUTMETH-13                   
129400           MOVE REQU-IDOUTDEST(IX)  TO MAP-IDOUTDEST-13                   
129500           MOVE REQU-KVCOPIES(IX)   TO MAP-KVCOPIES-13                    
129600           MOVE REQU-FLCARRCNTL(IX) TO MAP-FLCARRCNTL-13                  
129700           MOVE REQU-IDPFDEF(IX)    TO MAP-IDPFDEF-13                     
129800           MOVE REQU-IDFORMSNM(IX)  TO MAP-IDFORMSNM-13                   
129900           MOVE REQU-TEVCOMST(IX)   TO MAP-TEVCOMST-13                    
130000           MOVE REQU-IDVCINIT(IX)   TO MAP-IDVCINIT-13                    
130100           MOVE REQU-FLACIF(IX)     TO MAP-FLACIF-13                      
130200         WHEN 14                                                          
130300           MOVE REQU-KDOUTMETH(IX)  TO MAP-KDOUTMETH-14                   
130400           MOVE REQU-IDOUTDEST(IX)  TO MAP-IDOUTDEST-14                   
130500           MOVE REQU-KVCOPIES(IX)   TO MAP-KVCOPIES-14                    
130600           MOVE REQU-FLCARRCNTL(IX) TO MAP-FLCARRCNTL-14                  
130700           MOVE REQU-IDPFDEF(IX)    TO MAP-IDPFDEF-14                     
130800           MOVE REQU-IDFORMSNM(IX)  TO MAP-IDFORMSNM-14                   
130900           MOVE REQU-TEVCOMST(IX)   TO MAP-TEVCOMST-14                    
131000           MOVE REQU-IDVCINIT(IX)   TO MAP-IDVCINIT-14                    
131100           MOVE REQU-FLACIF(IX)     TO MAP-FLACIF-14                      
131200         WHEN 15                                                          
131300           MOVE REQU-KDOUTMETH(IX)  TO MAP-KDOUTMETH-15                   
131400           MOVE REQU-IDOUTDEST(IX)  TO MAP-IDOUTDEST-15                   
131500           MOVE REQU-KVCOPIES(IX)   TO MAP-KVCOPIES-15                    
131600           MOVE REQU-FLCARRCNTL(IX) TO MAP-FLCARRCNTL-15                  
131700           MOVE REQU-IDPFDEF(IX)    TO MAP-IDPFDEF-15                     
131800           MOVE REQU-IDFORMSNM(IX)  TO MAP-IDFORMSNM-15                   
131900           MOVE REQU-TEVCOMST(IX)   TO MAP-TEVCOMST-15                    
132000           MOVE REQU-IDVCINIT(IX)   TO MAP-IDVCINIT-15                    
132100           MOVE REQU-FLACIF(IX)     TO MAP-FLACIF-15                      
132200       END-EVALUATE                                                       
132300       ADD 1 TO IX                                                        
132400     END-PERFORM                                                          
132500                                                                          
132600     MOVE REQU-KVDAGAR-RESEND TO MAP-KVDAGAR-RESEND                       
132700     COMPUTE MAP-TENOTE-L =                                               
132800       FUNCTION LENGTH (FUNCTION TRIM(REQU-TENOTE))                       
132900     MOVE FUNCTION TRIM(REQU-TENOTE)                                      
133000                              TO MAP-TENOTE-D                             
133100     .                                                                    
133200 S05-MOVE-SEARCH-TO-RESPOND SECTION.                                      
133300                                                                          
133400     MOVE DIRU-KDOUTMETH-1      TO RESP-KDOUTMETH(1)                      
133500     MOVE DIRU-IDOUTDEST-1      TO RESP-IDOUTDEST(1)                      
133600     MOVE DIRU-KVCOPIES-1       TO RESP-KVCOPIES(1)                       
133700     MOVE DIRU-FLCARRCNTL-1     TO RESP-FLCARRCNTL(1)                     
133800     MOVE DIRU-IDPFDEF-1        TO RESP-IDPFDEF(1)                        
133900     MOVE DIRU-IDFORMSNM-1      TO RESP-IDFORMSNM(1)                      
134000     MOVE DIRU-TEVCOMST-1       TO RESP-TEVCOMST(1)                       
134100     MOVE DIRU-IDVCINIT-1       TO RESP-IDVCINIT(1)                       
134200     MOVE DIRU-TEFAX-1          TO RESP-TEFAX(1)                          
134300     MOVE DIRU-FLACIF-1         TO RESP-FLACIF(1)                         
134400                                                                          
134500     MOVE DIRU-KDOUTMETH-2      TO RESP-KDOUTMETH(2)                      
134600     MOVE DIRU-IDOUTDEST-2      TO RESP-IDOUTDEST(2)                      
134700     MOVE DIRU-KVCOPIES-2       TO RESP-KVCOPIES(2)                       
134800     MOVE DIRU-FLCARRCNTL-2     TO RESP-FLCARRCNTL(2)                     
134900     MOVE DIRU-IDPFDEF-2        TO RESP-IDPFDEF(2)                        
135000     MOVE DIRU-IDFORMSNM-2      TO RESP-IDFORMSNM(2)                      
135100     MOVE DIRU-TEVCOMST-2       TO RESP-TEVCOMST(2)                       
135200     MOVE DIRU-IDVCINIT-2       TO RESP-IDVCINIT(2)                       
135300     MOVE DIRU-TEFAX-2          TO RESP-TEFAX(2)                          
135400     MOVE DIRU-FLACIF-2         TO RESP-FLACIF(2)                         
135500                                                                          
135600     MOVE DIRU-KDOUTMETH-3      TO RESP-KDOUTMETH(3)                      
135700     MOVE DIRU-IDOUTDEST-3      TO RESP-IDOUTDEST(3)                      
135800     MOVE DIRU-KVCOPIES-3       TO RESP-KVCOPIES(3)                       
135900     MOVE DIRU-FLCARRCNTL-3     TO RESP-FLCARRCNTL(3)                     
136000     MOVE DIRU-IDPFDEF-3        TO RESP-IDPFDEF(3)                        
136100     MOVE DIRU-IDFORMSNM-3      TO RESP-IDFORMSNM(3)                      
136200     MOVE DIRU-TEVCOMST-3       TO RESP-TEVCOMST(3)                       
136300     MOVE DIRU-IDVCINIT-3       TO RESP-IDVCINIT(3)                       
136400     MOVE DIRU-TEFAX-3          TO RESP-TEFAX(3)                          
136500     MOVE DIRU-FLACIF-3         TO RESP-FLACIF(3)                         
136600                                                                          
136700     MOVE DIRU-KDOUTMETH-4      TO RESP-KDOUTMETH(4)                      
136800     MOVE DIRU-IDOUTDEST-4      TO RESP-IDOUTDEST(4)                      
136900     MOVE DIRU-KVCOPIES-4       TO RESP-KVCOPIES(4)                       
137000     MOVE DIRU-FLCARRCNTL-4     TO RESP-FLCARRCNTL(4)                     
137100     MOVE DIRU-IDPFDEF-4        TO RESP-IDPFDEF(4)                        
137200     MOVE DIRU-IDFORMSNM-4      TO RESP-IDFORMSNM(4)                      
137300     MOVE DIRU-TEVCOMST-4       TO RESP-TEVCOMST(4)                       
137400     MOVE DIRU-IDVCINIT-4       TO RESP-IDVCINIT(4)                       
137500     MOVE DIRU-TEFAX-4          TO RESP-TEFAX(4)                          
137600     MOVE DIRU-FLACIF-4         TO RESP-FLACIF(4)                         
137700                                                                          
137800     MOVE DIRU-KDOUTMETH-5      TO RESP-KDOUTMETH(5)                      
137900     MOVE DIRU-IDOUTDEST-5      TO RESP-IDOUTDEST(5)                      
138000     MOVE DIRU-KVCOPIES-5       TO RESP-KVCOPIES(5)                       
138100     MOVE DIRU-FLCARRCNTL-5     TO RESP-FLCARRCNTL(5)                     
138200     MOVE DIRU-IDPFDEF-5        TO RESP-IDPFDEF(5)                        
138300     MOVE DIRU-IDFORMSNM-5      TO RESP-IDFORMSNM(5)                      
138400     MOVE DIRU-TEVCOMST-5       TO RESP-TEVCOMST(5)                       
138500     MOVE DIRU-IDVCINIT-5       TO RESP-IDVCINIT(5)                       
138600     MOVE DIRU-TEFAX-5          TO RESP-TEFAX(5)                          
138700     MOVE DIRU-FLACIF-5         TO RESP-FLACIF(5)                         
138800                                                                          
138900     MOVE DIRU-KDOUTMETH-6      TO RESP-KDOUTMETH(6)                      
139000     MOVE DIRU-IDOUTDEST-6      TO RESP-IDOUTDEST(6)                      
139100     MOVE DIRU-KVCOPIES-6       TO RESP-KVCOPIES(6)                       
139200     MOVE DIRU-FLCARRCNTL-6     TO RESP-FLCARRCNTL(6)                     
139300     MOVE DIRU-IDPFDEF-6        TO RESP-IDPFDEF(6)                        
139400     MOVE DIRU-IDFORMSNM-6      TO RESP-IDFORMSNM(6)                      
139500     MOVE DIRU-TEVCOMST-6       TO RESP-TEVCOMST(6)                       
139600     MOVE DIRU-IDVCINIT-6       TO RESP-IDVCINIT(6)                       
139700     MOVE DIRU-FLACIF-6         TO RESP-FLACIF(6)                         
139800                                                                          
139900     MOVE DIRU-KDOUTMETH-7      TO RESP-KDOUTMETH(7)                      
140000     MOVE DIRU-IDOUTDEST-7      TO RESP-IDOUTDEST(7)                      
140100     MOVE DIRU-KVCOPIES-7       TO RESP-KVCOPIES(7)                       
140200     MOVE DIRU-FLCARRCNTL-7     TO RESP-FLCARRCNTL(7)                     
140300     MOVE DIRU-IDPFDEF-7        TO RESP-IDPFDEF(7)                        
140400     MOVE DIRU-IDFORMSNM-7      TO RESP-IDFORMSNM(7)                      
140500     MOVE DIRU-TEVCOMST-7       TO RESP-TEVCOMST(7)                       
140600     MOVE DIRU-IDVCINIT-7       TO RESP-IDVCINIT(7)                       
140700     MOVE DIRU-FLACIF-7         TO RESP-FLACIF(7)                         
140800                                                                          
140900     MOVE DIRU-KDOUTMETH-8      TO RESP-KDOUTMETH(8)                      
141000     MOVE DIRU-IDOUTDEST-8      TO RESP-IDOUTDEST(8)                      
141100     MOVE DIRU-KVCOPIES-8       TO RESP-KVCOPIES(8)                       
141200     MOVE DIRU-FLCARRCNTL-8     TO RESP-FLCARRCNTL(8)                     
141300     MOVE DIRU-IDPFDEF-8        TO RESP-IDPFDEF(8)                        
141400     MOVE DIRU-IDFORMSNM-8      TO RESP-IDFORMSNM(8)                      
141500     MOVE DIRU-TEVCOMST-8       TO RESP-TEVCOMST(8)                       
141600     MOVE DIRU-IDVCINIT-8       TO RESP-IDVCINIT(8)                       
141700     MOVE DIRU-FLACIF-8         TO RESP-FLACIF(8)                         
141800                                                                          
141900     MOVE DIRU-KDOUTMETH-9      TO RESP-KDOUTMETH(9)                      
142000     MOVE DIRU-IDOUTDEST-9      TO RESP-IDOUTDEST(9)                      
142100     MOVE DIRU-KVCOPIES-9       TO RESP-KVCOPIES(9)                       
142200     MOVE DIRU-FLCARRCNTL-9     TO RESP-FLCARRCNTL(9)                     
142300     MOVE DIRU-IDPFDEF-9        TO RESP-IDPFDEF(9)                        
142400     MOVE DIRU-IDFORMSNM-9      TO RESP-IDFORMSNM(9)                      
142500     MOVE DIRU-TEVCOMST-9       TO RESP-TEVCOMST(9)                       
142600     MOVE DIRU-IDVCINIT-9       TO RESP-IDVCINIT(9)                       
142700     MOVE DIRU-FLACIF-9         TO RESP-FLACIF(9)                         
142800                                                                          
142900     MOVE DIRU-KDOUTMETH-10     TO RESP-KDOUTMETH(10)                     
143000     MOVE DIRU-IDOUTDEST-10     TO RESP-IDOUTDEST(10)                     
143100     MOVE DIRU-KVCOPIES-10      TO RESP-KVCOPIES(10)                      
143200     MOVE DIRU-FLCARRCNTL-10    TO RESP-FLCARRCNTL(10)                    
143300     MOVE DIRU-IDPFDEF-10       TO RESP-IDPFDEF(10)                       
143400     MOVE DIRU-IDFORMSNM-10     TO RESP-IDFORMSNM(10)                     
143500     MOVE DIRU-TEVCOMST-10      TO RESP-TEVCOMST(10)                      
143600     MOVE DIRU-IDVCINIT-10      TO RESP-IDVCINIT(10)                      
143700     MOVE DIRU-FLACIF-10        TO RESP-FLACIF(10)                        
143800                                                                          
143900     MOVE DIRU-KDOUTMETH-11     TO RESP-KDOUTMETH(11)                     
144000     MOVE DIRU-IDOUTDEST-11     TO RESP-IDOUTDEST(11)                     
144100     MOVE DIRU-KVCOPIES-11      TO RESP-KVCOPIES(11)                      
144200     MOVE DIRU-FLCARRCNTL-11    TO RESP-FLCARRCNTL(11)                    
144300     MOVE DIRU-IDPFDEF-11       TO RESP-IDPFDEF(11)                       
144400     MOVE DIRU-IDFORMSNM-11     TO RESP-IDFORMSNM(11)                     
144500     MOVE DIRU-TEVCOMST-11      TO RESP-TEVCOMST(11)                      
144600     MOVE DIRU-IDVCINIT-11      TO RESP-IDVCINIT(11)                      
144700     MOVE DIRU-FLACIF-11        TO RESP-FLACIF(11)                        
144800                                                                          
144900     MOVE DIRU-KDOUTMETH-12     TO RESP-KDOUTMETH(12)                     
145000     MOVE DIRU-IDOUTDEST-12     TO RESP-IDOUTDEST(12)                     
145100     MOVE DIRU-KVCOPIES-12      TO RESP-KVCOPIES(12)                      
145200     MOVE DIRU-FLCARRCNTL-12    TO RESP-FLCARRCNTL(12)                    
145300     MOVE DIRU-IDPFDEF-12       TO RESP-IDPFDEF(12)                       
145400     MOVE DIRU-IDFORMSNM-12     TO RESP-IDFORMSNM(12)                     
145500     MOVE DIRU-TEVCOMST-12      TO RESP-TEVCOMST(12)                      
145600     MOVE DIRU-IDVCINIT-12      TO RESP-IDVCINIT(12)                      
145700     MOVE DIRU-FLACIF-12        TO RESP-FLACIF(12)                        
145800                                                                          
145900     MOVE DIRU-KDOUTMETH-13     TO RESP-KDOUTMETH(13)                     
146000     MOVE DIRU-IDOUTDEST-13     TO RESP-IDOUTDEST(13)                     
146100     MOVE DIRU-KVCOPIES-13      TO RESP-KVCOPIES(13)                      
146200     MOVE DIRU-FLCARRCNTL-13    TO RESP-FLCARRCNTL(13)                    
146300     MOVE DIRU-IDPFDEF-13       TO RESP-IDPFDEF(13)                       
146400     MOVE DIRU-IDFORMSNM-13     TO RESP-IDFORMSNM(13)                     
146500     MOVE DIRU-TEVCOMST-13      TO RESP-TEVCOMST(13)                      
146600     MOVE DIRU-IDVCINIT-13      TO RESP-IDVCINIT(13)                      
146700     MOVE DIRU-FLACIF-13        TO RESP-FLACIF(13)                        
146800                                                                          
146900     MOVE DIRU-KDOUTMETH-14     TO RESP-KDOUTMETH(14)                     
147000     MOVE DIRU-IDOUTDEST-14     TO RESP-IDOUTDEST(14)                     
147100     MOVE DIRU-KVCOPIES-14      TO RESP-KVCOPIES(14)                      
147200     MOVE DIRU-FLCARRCNTL-14    TO RESP-FLCARRCNTL(14)                    
147300     MOVE DIRU-IDPFDEF-14       TO RESP-IDPFDEF(14)                       
147400     MOVE DIRU-IDFORMSNM-14     TO RESP-IDFORMSNM(14)                     
147500     MOVE DIRU-TEVCOMST-14      TO RESP-TEVCOMST(14)                      
147600     MOVE DIRU-IDVCINIT-14      TO RESP-IDVCINIT(14)                      
147700     MOVE DIRU-FLACIF-14        TO RESP-FLACIF(14)                        
147800                                                                          
147900     MOVE DIRU-KDOUTMETH-15     TO RESP-KDOUTMETH(15)                     
148000     MOVE DIRU-IDOUTDEST-15     TO RESP-IDOUTDEST(15)                     
148100     MOVE DIRU-KVCOPIES-15      TO RESP-KVCOPIES(15)                      
148200     MOVE DIRU-FLCARRCNTL-15    TO RESP-FLCARRCNTL(15)                    
148300     MOVE DIRU-IDPFDEF-15       TO RESP-IDPFDEF(15)                       
148400     MOVE DIRU-IDFORMSNM-15     TO RESP-IDFORMSNM(15)                     
148500     MOVE DIRU-TEVCOMST-15      TO RESP-TEVCOMST(15)                      
148600     MOVE DIRU-IDVCINIT-15      TO RESP-IDVCINIT(15)                      
148700     MOVE DIRU-FLACIF-15        TO RESP-FLACIF(15)                        
148800                                                                          
148900     MOVE DIRU-KVDAGAR-RESEND   TO RESP-KVDAGAR-RESEND                    
149000     MOVE DIRU-IDMAIL-SENDER    TO RESP-IDMAIL-SENDER                     
149100     MOVE DIRU-TIREGDAT         TO RESP-TIREGDAT                          
149200     MOVE DIRU-TIUPPDAT         TO RESP-TIUPPDAT                          
149300     MOVE DIRU-IDUSER           TO RESP-IDUSER                            
149400     MOVE DIRU-TENOTE-D         TO RESP-TENOTE                            
149500     .                                                                    
149600 S06-MOVE-UPD-INS-TO-RESPOND SECTION.                                     
149700                                                                          
149800     MOVE 1 TO IX                                                         
149900     PERFORM UNTIL IX > 15                                                
150000       MOVE REQU-KDOUTMETH(IX)  TO RESP-KDOUTMETH(IX)                     
150100       MOVE REQU-IDOUTDEST(IX)  TO RESP-IDOUTDEST(IX)                     
150200       MOVE REQU-KVCOPIES(IX)   TO RESP-KVCOPIES(IX)                      
150300       MOVE REQU-FLCARRCNTL(IX) TO RESP-FLCARRCNTL(IX)                    
150400       MOVE REQU-IDPFDEF(IX)    TO RESP-IDPFDEF(IX)                       
150500       MOVE REQU-IDFORMSNM(IX)  TO RESP-IDFORMSNM(IX)                     
150600       MOVE REQU-TEVCOMST(IX)   TO RESP-TEVCOMST(IX)                      
150700       MOVE REQU-IDVCINIT(IX)   TO RESP-IDVCINIT(IX)                      
150800       MOVE REQU-FLACIF(IX)     TO RESP-FLACIF(IX)                        
150900       ADD 1 TO IX                                                        
151000     END-PERFORM                                                          
151100                                                                          
151200     MOVE 1 TO IX                                                         
151300     PERFORM UNTIL IX > 5                                                 
151400       MOVE REQU-TEFAX(IX)      TO RESP-TEFAX(IX)                         
151500       ADD 1 TO IX                                                        
151600     END-PERFORM                                                          
151700                                                                          
151800     MOVE REQU-KVDAGAR-RESEND TO RESP-KVDAGAR-RESEND                      
151900     MOVE REQU-IDMAIL-SENDER  TO RESP-IDMAIL-SENDER                       
152000     MOVE MAP-TIREGDAT        TO RESP-TIREGDAT                            
152100     MOVE MAP-TIUPPDAT        TO RESP-TIUPPDAT                            
152200     MOVE REQU-IDUSER         TO RESP-IDUSER                              
152300     MOVE REQU-TENOTE         TO RESP-TENOTE                              
152400     .                                                                    
152500 S07-SCRATCH-RESPOND SECTION.                                             
152600                                                                          
152700     MOVE 1 TO IX                                                         
152800     PERFORM UNTIL IX > 15                                                
152900       MOVE SPACE TO RESP-KDOUTMETH(IX)                                   
153000       MOVE SPACE TO RESP-IDOUTDEST(IX)                                   
153100       MOVE SPACE TO RESP-KVCOPIES(IX)                                    
153200       MOVE SPACE TO RESP-FLCARRCNTL(IX)                                  
153300       MOVE SPACE TO RESP-IDPFDEF(IX)                                     
153400       MOVE SPACE TO RESP-IDFORMSNM(IX)                                   
153500       MOVE SPACE TO RESP-TEVCOMST(IX)                                    
153600       MOVE SPACE TO RESP-IDVCINIT(IX)                                    
153700       MOVE SPACE TO RESP-FLACIF(IX)                                      
153800       ADD 1 TO IX                                                        
153900     END-PERFORM                                                          
154000                                                                          
154100     MOVE 1 TO IX                                                         
154200     PERFORM UNTIL IX > 5                                                 
154300       MOVE SPACE TO RESP-TEFAX(IX)                                       
154400       ADD 1 TO IX                                                        
154500     END-PERFORM                                                          
154600                                                                          
154700     MOVE ZERO  TO RESP-KVDAGAR-RESEND                                    
154800     MOVE SPACE TO RESP-IDMAIL-SENDER                                     
154900     MOVE ZERO  TO RESP-TIREGDAT                                          
155000     MOVE ZERO  TO RESP-TIUPPDAT                                          
155100     MOVE SPACE TO RESP-IDUSER                                            
155200     MOVE SPACE TO RESP-TENOTE                                            
155300     .                                                                    
155400                                                                          
155500*    --- DB2 SECTIONS                                                     
155600 DB2-SELECT-TZ4DIRU-TAB SECTION.                                          
155700                                                                          
155800     MOVE 000100  TO GOOD-SQLCODECODES                                    
155900     EXEC SQL                                                             
156000          SELECT KDOUTMETH_1                                              
156100               , IDOUTDEST_1                                              
156200               , KVCOPIES_1                                               
156300               , FLCARRCNTL_1                                             
156400               , IDPFDEF_1                                                
156500               , IDFORMSNM_1                                              
156600               , TEVCOMST_1                                               
156700               , IDVCINIT_1                                               
156800               , TEFAX_1                                                  
156900               , FLACIF_1                                                 
157000               , KDOUTMETH_2                                              
157100               , IDOUTDEST_2                                              
157200               , KVCOPIES_2                                               
157300               , FLCARRCNTL_2                                             
157400               , IDPFDEF_2                                                
157500               , IDFORMSNM_2                                              
157600               , TEVCOMST_2                                               
157700               , IDVCINIT_2                                               
157800               , TEFAX_2                                                  
157900               , FLACIF_2                                                 
158000               , KDOUTMETH_3                                              
158100               , IDOUTDEST_3                                              
158200               , KVCOPIES_3                                               
158300               , FLCARRCNTL_3                                             
158400               , IDPFDEF_3                                                
158500               , IDFORMSNM_3                                              
158600               , TEVCOMST_3                                               
158700               , IDVCINIT_3                                               
158800               , TEFAX_3                                                  
158900               , FLACIF_3                                                 
159000               , KDOUTMETH_4                                              
159100               , IDOUTDEST_4                                              
159200               , KVCOPIES_4                                               
159300               , FLCARRCNTL_4                                             
159400               , IDPFDEF_4                                                
159500               , IDFORMSNM_4                                              
159600               , TEVCOMST_4                                               
159700               , IDVCINIT_4                                               
159800               , TEFAX_4                                                  
159900               , FLACIF_4                                                 
160000               , KDOUTMETH_5                                              
160100               , IDOUTDEST_5                                              
160200               , KVCOPIES_5                                               
160300               , FLCARRCNTL_5                                             
160400               , IDPFDEF_5                                                
160500               , IDFORMSNM_5                                              
160600               , TEVCOMST_5                                               
160700               , IDVCINIT_5                                               
160800               , TEFAX_5                                                  
160900               , FLACIF_5                                                 
161000               , KDOUTMETH_6                                              
161100               , IDOUTDEST_6                                              
161200               , KVCOPIES_6                                               
161300               , FLCARRCNTL_6                                             
161400               , IDPFDEF_6                                                
161500               , IDFORMSNM_6                                              
161600               , TEVCOMST_6                                               
161700               , IDVCINIT_6                                               
161800               , FLACIF_6                                                 
161900               , KDOUTMETH_7                                              
162000               , IDOUTDEST_7                                              
162100               , KVCOPIES_7                                               
162200               , FLCARRCNTL_7                                             
162300               , IDPFDEF_7                                                
162400               , IDFORMSNM_7                                              
162500               , TEVCOMST_7                                               
162600               , IDVCINIT_7                                               
162700               , FLACIF_7                                                 
162800               , KDOUTMETH_8                                              
162900               , IDOUTDEST_8                                              
163000               , KVCOPIES_8                                               
163100               , FLCARRCNTL_8                                             
163200               , IDPFDEF_8                                                
163300               , IDFORMSNM_8                                              
163400               , TEVCOMST_8                                               
163500               , IDVCINIT_8                                               
163600               , FLACIF_8                                                 
163700               , KDOUTMETH_9                                              
163800               , IDOUTDEST_9                                              
163900               , KVCOPIES_9                                               
164000               , FLCARRCNTL_9                                             
164100               , IDPFDEF_9                                                
164200               , IDFORMSNM_9                                              
164300               , TEVCOMST_9                                               
164400               , IDVCINIT_9                                               
164500               , FLACIF_9                                                 
164600               , KDOUTMETH_10                                             
164700               , IDOUTDEST_10                                             
164800               , KVCOPIES_10                                              
164900               , FLCARRCNTL_10                                            
165000               , IDPFDEF_10                                               
165100               , IDFORMSNM_10                                             
165200               , TEVCOMST_10                                              
165300               , IDVCINIT_10                                              
165400               , FLACIF_10                                                
165500               , KDOUTMETH_11                                             
165600               , IDOUTDEST_11                                             
165700               , KVCOPIES_11                                              
165800               , FLCARRCNTL_11                                            
165900               , IDPFDEF_11                                               
166000               , IDFORMSNM_11                                             
166100               , TEVCOMST_11                                              
166200               , IDVCINIT_11                                              
166300               , FLACIF_11                                                
166400               , KDOUTMETH_12                                             
166500               , IDOUTDEST_12                                             
166600               , KVCOPIES_12                                              
166700               , FLCARRCNTL_12                                            
166800               , IDPFDEF_12                                               
166900               , IDFORMSNM_12                                             
167000               , TEVCOMST_12                                              
167100               , IDVCINIT_12                                              
167200               , FLACIF_12                                                
167300               , KDOUTMETH_13                                             
167400               , IDOUTDEST_13                                             
167500               , KVCOPIES_13                                              
167600               , FLCARRCNTL_13                                            
167700               , IDPFDEF_13                                               
167800               , IDFORMSNM_13                                             
167900               , TEVCOMST_13                                              
168000               , IDVCINIT_13                                              
168100               , FLACIF_13                                                
168200               , KDOUTMETH_14                                             
168300               , IDOUTDEST_14                                             
168400               , KVCOPIES_14                                              
168500               , FLCARRCNTL_14                                            
168600               , IDPFDEF_14                                               
168700               , IDFORMSNM_14                                             
168800               , TEVCOMST_14                                              
168900               , IDVCINIT_14                                              
169000               , FLACIF_14                                                
169100               , KDOUTMETH_15                                             
169200               , IDOUTDEST_15                                             
169300               , KVCOPIES_15                                              
169400               , FLCARRCNTL_15                                            
169500               , IDPFDEF_15                                               
169600               , IDFORMSNM_15                                             
169700               , TEVCOMST_15                                              
169800               , IDVCINIT_15                                              
169900               , FLACIF_15                                                
170000               , KVDAGAR_RESEND                                           
170100               , IDMAIL_SENDER                                            
170200               , TIREGDAT                                                 
170300               , TIUPPDAT                                                 
170400               , IDUSER                                                   
170500               , TENOTE                                                   
170600                                                                          
170700          INTO  :DIRU-KDOUTMETH-1                                         
170800              , :DIRU-IDOUTDEST-1                                         
170900              , :DIRU-KVCOPIES-1                                          
171000              , :DIRU-FLCARRCNTL-1                                        
171100              , :DIRU-IDPFDEF-1                                           
171200              , :DIRU-IDFORMSNM-1                                         
171300              , :DIRU-TEVCOMST-1                                          
171400              , :DIRU-IDVCINIT-1                                          
171500              , :DIRU-TEFAX-1                                             
171600              , :DIRU-FLACIF-1                                            
171700              , :DIRU-KDOUTMETH-2                                         
171800              , :DIRU-IDOUTDEST-2                                         
171900              , :DIRU-KVCOPIES-2                                          
172000              , :DIRU-FLCARRCNTL-2                                        
172100              , :DIRU-IDPFDEF-2                                           
172200              , :DIRU-IDFORMSNM-2                                         
172300              , :DIRU-TEVCOMST-2                                          
172400              , :DIRU-IDVCINIT-2                                          
172500              , :DIRU-TEFAX-2                                             
172600              , :DIRU-FLACIF-2                                            
172700              , :DIRU-KDOUTMETH-3                                         
172800              , :DIRU-IDOUTDEST-3                                         
172900              , :DIRU-KVCOPIES-3                                          
173000              , :DIRU-FLCARRCNTL-3                                        
173100              , :DIRU-IDPFDEF-3                                           
173200              , :DIRU-IDFORMSNM-3                                         
173300              , :DIRU-TEVCOMST-3                                          
173400              , :DIRU-IDVCINIT-3                                          
173500              , :DIRU-TEFAX-3                                             
173600              , :DIRU-FLACIF-3                                            
173700              , :DIRU-KDOUTMETH-4                                         
173800              , :DIRU-IDOUTDEST-4                                         
173900              , :DIRU-KVCOPIES-4                                          
174000              , :DIRU-FLCARRCNTL-4                                        
174100              , :DIRU-IDPFDEF-4                                           
174200              , :DIRU-IDFORMSNM-4                                         
174300              , :DIRU-TEVCOMST-4                                          
174400              , :DIRU-IDVCINIT-4                                          
174500              , :DIRU-TEFAX-4                                             
174600              , :DIRU-FLACIF-4                                            
174700              , :DIRU-KDOUTMETH-5                                         
174800              , :DIRU-IDOUTDEST-5                                         
174900              , :DIRU-KVCOPIES-5                                          
175000              , :DIRU-FLCARRCNTL-5                                        
175100              , :DIRU-IDPFDEF-5                                           
175200              , :DIRU-IDFORMSNM-5                                         
175300              , :DIRU-TEVCOMST-5                                          
175400              , :DIRU-IDVCINIT-5                                          
175500              , :DIRU-TEFAX-5                                             
175600              , :DIRU-FLACIF-5                                            
175700              , :DIRU-KDOUTMETH-6                                         
175800              , :DIRU-IDOUTDEST-6                                         
175900              , :DIRU-KVCOPIES-6                                          
176000              , :DIRU-FLCARRCNTL-6                                        
176100              , :DIRU-IDPFDEF-6                                           
176200              , :DIRU-IDFORMSNM-6                                         
176300              , :DIRU-TEVCOMST-6                                          
176400              , :DIRU-IDVCINIT-6                                          
176500              , :DIRU-FLACIF-6                                            
176600              , :DIRU-KDOUTMETH-7                                         
176700              , :DIRU-IDOUTDEST-7                                         
176800              , :DIRU-KVCOPIES-7                                          
176900              , :DIRU-FLCARRCNTL-7                                        
177000              , :DIRU-IDPFDEF-7                                           
177100              , :DIRU-IDFORMSNM-7                                         
177200              , :DIRU-TEVCOMST-7                                          
177300              , :DIRU-IDVCINIT-7                                          
177400              , :DIRU-FLACIF-7                                            
177500              , :DIRU-KDOUTMETH-8                                         
177600              , :DIRU-IDOUTDEST-8                                         
177700              , :DIRU-KVCOPIES-8                                          
177800              , :DIRU-FLCARRCNTL-8                                        
177900              , :DIRU-IDPFDEF-8                                           
178000              , :DIRU-IDFORMSNM-8                                         
178100              , :DIRU-TEVCOMST-8                                          
178200              , :DIRU-IDVCINIT-8                                          
178300              , :DIRU-FLACIF-8                                            
178400              , :DIRU-KDOUTMETH-9                                         
178500              , :DIRU-IDOUTDEST-9                                         
178600              , :DIRU-KVCOPIES-9                                          
178700              , :DIRU-FLCARRCNTL-9                                        
178800              , :DIRU-IDPFDEF-9                                           
178900              , :DIRU-IDFORMSNM-9                                         
179000              , :DIRU-TEVCOMST-9                                          
179100              , :DIRU-IDVCINIT-9                                          
179200              , :DIRU-FLACIF-9                                            
179300              , :DIRU-KDOUTMETH-10                                        
179400              , :DIRU-IDOUTDEST-10                                        
179500              , :DIRU-KVCOPIES-10                                         
179600              , :DIRU-FLCARRCNTL-10                                       
179700              , :DIRU-IDPFDEF-10                                          
179800              , :DIRU-IDFORMSNM-10                                        
179900              , :DIRU-TEVCOMST-10                                         
180000              , :DIRU-IDVCINIT-10                                         
180100              , :DIRU-FLACIF-10                                           
180200              , :DIRU-KDOUTMETH-11                                        
180300              , :DIRU-IDOUTDEST-11                                        
180400              , :DIRU-KVCOPIES-11                                         
180500              , :DIRU-FLCARRCNTL-11                                       
180600              , :DIRU-IDPFDEF-11                                          
180700              , :DIRU-IDFORMSNM-11                                        
180800              , :DIRU-TEVCOMST-11                                         
180900              , :DIRU-IDVCINIT-11                                         
181000              , :DIRU-FLACIF-11                                           
181100              , :DIRU-KDOUTMETH-12                                        
181200              , :DIRU-IDOUTDEST-12                                        
181300              , :DIRU-KVCOPIES-12                                         
181400              , :DIRU-FLCARRCNTL-12                                       
181500              , :DIRU-IDPFDEF-12                                          
181600              , :DIRU-IDFORMSNM-12                                        
181700              , :DIRU-TEVCOMST-12                                         
181800              , :DIRU-IDVCINIT-12                                         
181900              , :DIRU-FLACIF-12                                           
182000              , :DIRU-KDOUTMETH-13                                        
182100              , :DIRU-IDOUTDEST-13                                        
182200              , :DIRU-KVCOPIES-13                                         
182300              , :DIRU-FLCARRCNTL-13                                       
182400              , :DIRU-IDPFDEF-13                                          
182500              , :DIRU-IDFORMSNM-13                                        
182600              , :DIRU-TEVCOMST-13                                         
182700              , :DIRU-IDVCINIT-13                                         
182800              , :DIRU-FLACIF-13                                           
182900              , :DIRU-KDOUTMETH-14                                        
183000              , :DIRU-IDOUTDEST-14                                        
183100              , :DIRU-KVCOPIES-14                                         
183200              , :DIRU-FLCARRCNTL-14                                       
183300              , :DIRU-IDPFDEF-14                                          
183400              , :DIRU-IDFORMSNM-14                                        
183500              , :DIRU-TEVCOMST-14                                         
183600              , :DIRU-IDVCINIT-14                                         
183700              , :DIRU-FLACIF-14                                           
183800              , :DIRU-KDOUTMETH-15                                        
183900              , :DIRU-IDOUTDEST-15                                        
184000              , :DIRU-KVCOPIES-15                                         
184100              , :DIRU-FLCARRCNTL-15                                       
184200              , :DIRU-IDPFDEF-15                                          
184300              , :DIRU-IDFORMSNM-15                                        
184400              , :DIRU-TEVCOMST-15                                         
184500              , :DIRU-IDVCINIT-15                                         
184600              , :DIRU-FLACIF-15                                           
184700              , :DIRU-KVDAGAR-RESEND                                      
184800              , :DIRU-IDMAIL-SENDER                                       
184900              , :DIRU-TIREGDAT                                            
185000              , :DIRU-TIUPPDAT                                            
185100              , :DIRU-IDUSER                                              
185200              , :DIRU-TENOTE                                              
185300                                                                          
185400          FROM   TZ4DIRU                                                  
185500                                                                          
185600          WHERE  IDOUTTYPE = :REQU-IDOUTTYPE-KEY                          
185700           AND   IDOUTREC_FROM = :REQU-IDOUTREC-FROM-KEY                  
185800           AND   IDOUTREC_TO = :REQU-IDOUTREC-TO-KEY                      
185900     END-EXEC                                                             
186000                                                                          
186100     MOVE SQLCODE TO SQLCODE-WS                                           
186200     PERFORM DB2-STATUS-CHECK                                             
186300     .                                                                    
186400 DB2-COUNT-TZ4DIRU-TAB SECTION.                                           
186500                                                                          
186600     MOVE 000100  TO GOOD-SQLCODECODES                                    
186700     EXEC SQL                                                             
186800          SELECT COUNT(*)                                                 
186900                                                                          
187000          INTO  :TZ4DIRU-COUNTER                                          
187100                                                                          
187200          FROM   TZ4DIRU                                                  
187300                                                                          
187400          WHERE  IDOUTTYPE     = :REQU-IDOUTTYPE-KEY                      
187500           AND   IDOUTREC_FROM = :REQU-IDOUTREC-FROM-KEY                  
187600           AND   IDOUTREC_TO   = :REQU-IDOUTREC-TO-KEY                    
187700     END-EXEC                                                             
187800                                                                          
187900     MOVE SQLCODE TO SQLCODE-WS                                           
188000     PERFORM DB2-STATUS-CHECK                                             
188100     .                                                                    
188200 DB2-UPDATE-TZ4DIRU-TAB  SECTION.                                         
188300                                                                          
188400     MOVE 000     TO GOOD-SQLCODECODES                                    
188500     EXEC SQL                                                             
188600         UPDATE TZ4DIRU                                                   
188700             SET   KDOUTMETH_1    = :MAP-KDOUTMETH-1                      
188800                 , KDOUTMETH_2    = :MAP-KDOUTMETH-2                      
188900                 , KDOUTMETH_3    = :MAP-KDOUTMETH-3                      
189000                 , KDOUTMETH_4    = :MAP-KDOUTMETH-4                      
189100                 , KDOUTMETH_5    = :MAP-KDOUTMETH-5                      
189200                 , KDOUTMETH_6    = :MAP-KDOUTMETH-6                      
189300                 , KDOUTMETH_7    = :MAP-KDOUTMETH-7                      
189400                 , KDOUTMETH_8    = :MAP-KDOUTMETH-8                      
189500                 , KDOUTMETH_9    = :MAP-KDOUTMETH-9                      
189600                 , KDOUTMETH_10   = :MAP-KDOUTMETH-10                     
189700                 , KDOUTMETH_11   = :MAP-KDOUTMETH-11                     
189800                 , KDOUTMETH_12   = :MAP-KDOUTMETH-12                     
189900                 , KDOUTMETH_13   = :MAP-KDOUTMETH-13                     
190000                 , KDOUTMETH_14   = :MAP-KDOUTMETH-14                     
190100                 , KDOUTMETH_15   = :MAP-KDOUTMETH-15                     
190200                 , IDOUTDEST_1    = :MAP-IDOUTDEST-1                      
190300                 , IDOUTDEST_2    = :MAP-IDOUTDEST-2                      
190400                 , IDOUTDEST_3    = :MAP-IDOUTDEST-3                      
190500                 , IDOUTDEST_4    = :MAP-IDOUTDEST-4                      
190600                 , IDOUTDEST_5    = :MAP-IDOUTDEST-5                      
190700                 , IDOUTDEST_6    = :MAP-IDOUTDEST-6                      
190800                 , IDOUTDEST_7    = :MAP-IDOUTDEST-7                      
190900                 , IDOUTDEST_8    = :MAP-IDOUTDEST-8                      
191000                 , IDOUTDEST_9    = :MAP-IDOUTDEST-9                      
191100                 , IDOUTDEST_10   = :MAP-IDOUTDEST-10                     
191200                 , IDOUTDEST_11   = :MAP-IDOUTDEST-11                     
191300                 , IDOUTDEST_12   = :MAP-IDOUTDEST-12                     
191400                 , IDOUTDEST_13   = :MAP-IDOUTDEST-13                     
191500                 , IDOUTDEST_14   = :MAP-IDOUTDEST-14                     
191600                 , IDOUTDEST_15   = :MAP-IDOUTDEST-15                     
191700                 , KVCOPIES_1     = :MAP-KVCOPIES-1                       
191800                 , KVCOPIES_2     = :MAP-KVCOPIES-2                       
191900                 , KVCOPIES_3     = :MAP-KVCOPIES-3                       
192000                 , KVCOPIES_4     = :MAP-KVCOPIES-4                       
192100                 , KVCOPIES_5     = :MAP-KVCOPIES-5                       
192200                 , KVCOPIES_6     = :MAP-KVCOPIES-6                       
192300                 , KVCOPIES_7     = :MAP-KVCOPIES-7                       
192400                 , KVCOPIES_8     = :MAP-KVCOPIES-8                       
192500                 , KVCOPIES_9     = :MAP-KVCOPIES-9                       
192600                 , KVCOPIES_10    = :MAP-KVCOPIES-10                      
192700                 , KVCOPIES_11    = :MAP-KVCOPIES-11                      
192800                 , KVCOPIES_12    = :MAP-KVCOPIES-12                      
192900                 , KVCOPIES_13    = :MAP-KVCOPIES-13                      
193000                 , KVCOPIES_14    = :MAP-KVCOPIES-14                      
193100                 , KVCOPIES_15    = :MAP-KVCOPIES-15                      
193200                 , FLCARRCNTL_1   = :MAP-FLCARRCNTL-1                     
193300                 , FLCARRCNTL_2   = :MAP-FLCARRCNTL-2                     
193400                 , FLCARRCNTL_3   = :MAP-FLCARRCNTL-3                     
193500                 , FLCARRCNTL_4   = :MAP-FLCARRCNTL-4                     
193600                 , FLCARRCNTL_5   = :MAP-FLCARRCNTL-5                     
193700                 , FLCARRCNTL_6   = :MAP-FLCARRCNTL-6                     
193800                 , FLCARRCNTL_7   = :MAP-FLCARRCNTL-7                     
193900                 , FLCARRCNTL_8   = :MAP-FLCARRCNTL-8                     
194000                 , FLCARRCNTL_9   = :MAP-FLCARRCNTL-9                     
194100                 , FLCARRCNTL_10  = :MAP-FLCARRCNTL-10                    
194200                 , FLCARRCNTL_11  = :MAP-FLCARRCNTL-11                    
194300                 , FLCARRCNTL_12  = :MAP-FLCARRCNTL-12                    
194400                 , FLCARRCNTL_13  = :MAP-FLCARRCNTL-13                    
194500                 , FLCARRCNTL_14  = :MAP-FLCARRCNTL-14                    
194600                 , FLCARRCNTL_15  = :MAP-FLCARRCNTL-15                    
194700                 , IDPFDEF_1      = :MAP-IDPFDEF-1                        
194800                 , IDPFDEF_2      = :MAP-IDPFDEF-2                        
194900                 , IDPFDEF_3      = :MAP-IDPFDEF-3                        
195000                 , IDPFDEF_4      = :MAP-IDPFDEF-4                        
195100                 , IDPFDEF_5      = :MAP-IDPFDEF-5                        
195200                 , IDPFDEF_6      = :MAP-IDPFDEF-6                        
195300                 , IDPFDEF_7      = :MAP-IDPFDEF-7                        
195400                 , IDPFDEF_8      = :MAP-IDPFDEF-8                        
195500                 , IDPFDEF_9      = :MAP-IDPFDEF-9                        
195600                 , IDPFDEF_10     = :MAP-IDPFDEF-10                       
195700                 , IDPFDEF_11     = :MAP-IDPFDEF-11                       
195800                 , IDPFDEF_12     = :MAP-IDPFDEF-12                       
195900                 , IDPFDEF_13     = :MAP-IDPFDEF-13                       
196000                 , IDPFDEF_14     = :MAP-IDPFDEF-14                       
196100                 , IDPFDEF_15     = :MAP-IDPFDEF-15                       
196200                 , IDFORMSNM_1    = :MAP-IDFORMSNM-1                      
196300                 , IDFORMSNM_2    = :MAP-IDFORMSNM-2                      
196400                 , IDFORMSNM_3    = :MAP-IDFORMSNM-3                      
196500                 , IDFORMSNM_4    = :MAP-IDFORMSNM-4                      
196600                 , IDFORMSNM_5    = :MAP-IDFORMSNM-5                      
196700                 , IDFORMSNM_6    = :MAP-IDFORMSNM-6                      
196800                 , IDFORMSNM_7    = :MAP-IDFORMSNM-7                      
196900                 , IDFORMSNM_8    = :MAP-IDFORMSNM-8                      
197000                 , IDFORMSNM_9    = :MAP-IDFORMSNM-9                      
197100                 , IDFORMSNM_10   = :MAP-IDFORMSNM-10                     
197200                 , IDFORMSNM_11   = :MAP-IDFORMSNM-11                     
197300                 , IDFORMSNM_12   = :MAP-IDFORMSNM-12                     
197400                 , IDFORMSNM_13   = :MAP-IDFORMSNM-13                     
197500                 , IDFORMSNM_14   = :MAP-IDFORMSNM-14                     
197600                 , IDFORMSNM_15   = :MAP-IDFORMSNM-15                     
197700                 , TEVCOMST_1     = :MAP-TEVCOMST-1                       
197800                 , TEVCOMST_2     = :MAP-TEVCOMST-2                       
197900                 , TEVCOMST_3     = :MAP-TEVCOMST-3                       
198000                 , TEVCOMST_4     = :MAP-TEVCOMST-4                       
198100                 , TEVCOMST_5     = :MAP-TEVCOMST-5                       
198200                 , TEVCOMST_6     = :MAP-TEVCOMST-6                       
198300                 , TEVCOMST_7     = :MAP-TEVCOMST-7                       
198400                 , TEVCOMST_8     = :MAP-TEVCOMST-8                       
198500                 , TEVCOMST_9     = :MAP-TEVCOMST-9                       
198600                 , TEVCOMST_10    = :MAP-TEVCOMST-10                      
198700                 , TEVCOMST_11    = :MAP-TEVCOMST-11                      
198800                 , TEVCOMST_12    = :MAP-TEVCOMST-12                      
198900                 , TEVCOMST_13    = :MAP-TEVCOMST-13                      
199000                 , TEVCOMST_14    = :MAP-TEVCOMST-14                      
199100                 , TEVCOMST_15    = :MAP-TEVCOMST-15                      
199200                 , IDVCINIT_1     = :MAP-IDVCINIT-1                       
199300                 , IDVCINIT_2     = :MAP-IDVCINIT-2                       
199400                 , IDVCINIT_3     = :MAP-IDVCINIT-3                       
199500                 , IDVCINIT_4     = :MAP-IDVCINIT-4                       
199600                 , IDVCINIT_5     = :MAP-IDVCINIT-5                       
199700                 , IDVCINIT_6     = :MAP-IDVCINIT-6                       
199800                 , IDVCINIT_7     = :MAP-IDVCINIT-7                       
199900                 , IDVCINIT_8     = :MAP-IDVCINIT-8                       
200000                 , IDVCINIT_9     = :MAP-IDVCINIT-9                       
200100                 , IDVCINIT_10    = :MAP-IDVCINIT-10                      
200200                 , IDVCINIT_11    = :MAP-IDVCINIT-11                      
200300                 , IDVCINIT_12    = :MAP-IDVCINIT-12                      
200400                 , IDVCINIT_13    = :MAP-IDVCINIT-13                      
200500                 , IDVCINIT_14    = :MAP-IDVCINIT-14                      
200600                 , IDVCINIT_15    = :MAP-IDVCINIT-15                      
200700                 , TEFAX_1        = :MAP-TEFAX-1                          
200800                 , TEFAX_2        = :MAP-TEFAX-2                          
200900                 , TEFAX_3        = :MAP-TEFAX-3                          
201000                 , TEFAX_4        = :MAP-TEFAX-4                          
201100                 , TEFAX_5        = :MAP-TEFAX-5                          
201200                 , FLACIF_1       = :MAP-FLACIF-1                         
201300                 , FLACIF_2       = :MAP-FLACIF-2                         
201400                 , FLACIF_3       = :MAP-FLACIF-3                         
201500                 , FLACIF_4       = :MAP-FLACIF-4                         
201600                 , FLACIF_5       = :MAP-FLACIF-5                         
201700                 , FLACIF_6       = :MAP-FLACIF-6                         
201800                 , FLACIF_7       = :MAP-FLACIF-7                         
201900                 , FLACIF_8       = :MAP-FLACIF-8                         
202000                 , FLACIF_9       = :MAP-FLACIF-9                         
202100                 , FLACIF_10      = :MAP-FLACIF-10                        
202200                 , FLACIF_11      = :MAP-FLACIF-11                        
202300                 , FLACIF_12      = :MAP-FLACIF-12                        
202400                 , FLACIF_13      = :MAP-FLACIF-13                        
202500                 , FLACIF_14      = :MAP-FLACIF-14                        
202600                 , FLACIF_15      = :MAP-FLACIF-15                        
202700                 , KVDAGAR_RESEND = :MAP-KVDAGAR-RESEND                   
202800                 , IDMAIL_SENDER  = :REQU-IDMAIL-SENDER                   
202900                 , TIUPPDAT       = :MAP-TIUPPDAT                         
203000                 , IDUSER         = :REQU-IDUSER                          
203100                 , TENOTE         = :MAP-TENOTE                           
203200                                                                          
203300         WHERE   IDOUTTYPE     = :REQU-IDOUTTYPE-KEY                      
203400          AND    IDOUTREC_FROM = :REQU-IDOUTREC-FROM-KEY                  
203500          AND    IDOUTREC_TO   = :REQU-IDOUTREC-TO-KEY                    
203600     END-EXEC                                                             
203700                                                                          
203800     MOVE SQLCODE TO SQLCODE-WS                                           
203900     PERFORM DB2-STATUS-CHECK                                             
204000     .                                                                    
204100 DB2-INSERT-TZ4DIRU-TAB  SECTION.                                         
204200                                                                          
204300     MOVE 000   TO GOOD-SQLCODECODES                                      
204400     EXEC SQL                                                             
204500         INSERT INTO TZ4DIRU                                              
204600            (IDOUTTYPE                                                    
204700            ,IDOUTREC_FROM                                                
204800            ,IDOUTREC_TO                                                  
204900            ,KDOUTMETH_1                                                  
205000            ,KDOUTMETH_2                                                  
205100            ,KDOUTMETH_3                                                  
205200            ,KDOUTMETH_4                                                  
205300            ,KDOUTMETH_5                                                  
205400            ,KDOUTMETH_6                                                  
205500            ,KDOUTMETH_7                                                  
205600            ,KDOUTMETH_8                                                  
205700            ,KDOUTMETH_9                                                  
205800            ,KDOUTMETH_10                                                 
205900            ,KDOUTMETH_11                                                 
206000            ,KDOUTMETH_12                                                 
206100            ,KDOUTMETH_13                                                 
206200            ,KDOUTMETH_14                                                 
206300            ,KDOUTMETH_15                                                 
206400            ,IDOUTDEST_1                                                  
206500            ,IDOUTDEST_2                                                  
206600            ,IDOUTDEST_3                                                  
206700            ,IDOUTDEST_4                                                  
206800            ,IDOUTDEST_5                                                  
206900            ,IDOUTDEST_6                                                  
207000            ,IDOUTDEST_7                                                  
207100            ,IDOUTDEST_8                                                  
207200            ,IDOUTDEST_9                                                  
207300            ,IDOUTDEST_10                                                 
207400            ,IDOUTDEST_11                                                 
207500            ,IDOUTDEST_12                                                 
207600            ,IDOUTDEST_13                                                 
207700            ,IDOUTDEST_14                                                 
207800            ,IDOUTDEST_15                                                 
207900            ,KVCOPIES_1                                                   
208000            ,KVCOPIES_2                                                   
208100            ,KVCOPIES_3                                                   
208200            ,KVCOPIES_4                                                   
208300            ,KVCOPIES_5                                                   
208400            ,KVCOPIES_6                                                   
208500            ,KVCOPIES_7                                                   
208600            ,KVCOPIES_8                                                   
208700            ,KVCOPIES_9                                                   
208800            ,KVCOPIES_10                                                  
208900            ,KVCOPIES_11                                                  
209000            ,KVCOPIES_12                                                  
209100            ,KVCOPIES_13                                                  
209200            ,KVCOPIES_14                                                  
209300            ,KVCOPIES_15                                                  
209400            ,FLCARRCNTL_1                                                 
209500            ,FLCARRCNTL_2                                                 
209600            ,FLCARRCNTL_3                                                 
209700            ,FLCARRCNTL_4                                                 
209800            ,FLCARRCNTL_5                                                 
209900            ,FLCARRCNTL_6                                                 
210000            ,FLCARRCNTL_7                                                 
210100            ,FLCARRCNTL_8                                                 
210200            ,FLCARRCNTL_9                                                 
210300            ,FLCARRCNTL_10                                                
210400            ,FLCARRCNTL_11                                                
210500            ,FLCARRCNTL_12                                                
210600            ,FLCARRCNTL_13                                                
210700            ,FLCARRCNTL_14                                                
210800            ,FLCARRCNTL_15                                                
210900            ,IDPFDEF_1                                                    
211000            ,IDPFDEF_2                                                    
211100            ,IDPFDEF_3                                                    
211200            ,IDPFDEF_4                                                    
211300            ,IDPFDEF_5                                                    
211400            ,IDPFDEF_6                                                    
211500            ,IDPFDEF_7                                                    
211600            ,IDPFDEF_8                                                    
211700            ,IDPFDEF_9                                                    
211800            ,IDPFDEF_10                                                   
211900            ,IDPFDEF_11                                                   
212000            ,IDPFDEF_12                                                   
212100            ,IDPFDEF_13                                                   
212200            ,IDPFDEF_14                                                   
212300            ,IDPFDEF_15                                                   
212400            ,IDFORMSNM_1                                                  
212500            ,IDFORMSNM_2                                                  
212600            ,IDFORMSNM_3                                                  
212700            ,IDFORMSNM_4                                                  
212800            ,IDFORMSNM_5                                                  
212900            ,IDFORMSNM_6                                                  
213000            ,IDFORMSNM_7                                                  
213100            ,IDFORMSNM_8                                                  
213200            ,IDFORMSNM_9                                                  
213300            ,IDFORMSNM_10                                                 
213400            ,IDFORMSNM_11                                                 
213500            ,IDFORMSNM_12                                                 
213600            ,IDFORMSNM_13                                                 
213700            ,IDFORMSNM_14                                                 
213800            ,IDFORMSNM_15                                                 
213900            ,TEVCOMST_1                                                   
214000            ,TEVCOMST_2                                                   
214100            ,TEVCOMST_3                                                   
214200            ,TEVCOMST_4                                                   
214300            ,TEVCOMST_5                                                   
214400            ,TEVCOMST_6                                                   
214500            ,TEVCOMST_7                                                   
214600            ,TEVCOMST_8                                                   
214700            ,TEVCOMST_9                                                   
214800            ,TEVCOMST_10                                                  
214900            ,TEVCOMST_11                                                  
215000            ,TEVCOMST_12                                                  
215100            ,TEVCOMST_13                                                  
215200            ,TEVCOMST_14                                                  
215300            ,TEVCOMST_15                                                  
215400            ,IDVCINIT_1                                                   
215500            ,IDVCINIT_2                                                   
215600            ,IDVCINIT_3                                                   
215700            ,IDVCINIT_4                                                   
215800            ,IDVCINIT_5                                                   
215900            ,IDVCINIT_6                                                   
216000            ,IDVCINIT_7                                                   
216100            ,IDVCINIT_8                                                   
216200            ,IDVCINIT_9                                                   
216300            ,IDVCINIT_10                                                  
216400            ,IDVCINIT_11                                                  
216500            ,IDVCINIT_12                                                  
216600            ,IDVCINIT_13                                                  
216700            ,IDVCINIT_14                                                  
216800            ,IDVCINIT_15                                                  
216900            ,TEFAX_1                                                      
217000            ,TEFAX_2                                                      
217100            ,TEFAX_3                                                      
217200            ,TEFAX_4                                                      
217300            ,TEFAX_5                                                      
217400            ,FLACIF_1                                                     
217500            ,FLACIF_2                                                     
217600            ,FLACIF_3                                                     
217700            ,FLACIF_4                                                     
217800            ,FLACIF_5                                                     
217900            ,FLACIF_6                                                     
218000            ,FLACIF_7                                                     
218100            ,FLACIF_8                                                     
218200            ,FLACIF_9                                                     
218300            ,FLACIF_10                                                    
218400            ,FLACIF_11                                                    
218500            ,FLACIF_12                                                    
218600            ,FLACIF_13                                                    
218700            ,FLACIF_14                                                    
218800            ,FLACIF_15                                                    
218900            ,KVDAGAR_RESEND                                               
219000            ,IDMAIL_SENDER                                                
219100            ,TIREGDAT                                                     
219200            ,TIUPPDAT                                                     
219300            ,IDUSER                                                       
219400            ,TENOTE)                                                      
219500         VALUES                                                           
219600            (:REQU-IDOUTTYPE-KEY                                          
219700            ,:REQU-IDOUTREC-FROM-KEY                                      
219800            ,:REQU-IDOUTREC-TO-KEY                                        
219900            ,:MAP-KDOUTMETH-1                                             
220000            ,:MAP-KDOUTMETH-2                                             
220100            ,:MAP-KDOUTMETH-3                                             
220200            ,:MAP-KDOUTMETH-4                                             
220300            ,:MAP-KDOUTMETH-5                                             
220400            ,:MAP-KDOUTMETH-6                                             
220500            ,:MAP-KDOUTMETH-7                                             
220600            ,:MAP-KDOUTMETH-8                                             
220700            ,:MAP-KDOUTMETH-9                                             
220800            ,:MAP-KDOUTMETH-10                                            
220900            ,:MAP-KDOUTMETH-11                                            
221000            ,:MAP-KDOUTMETH-12                                            
221100            ,:MAP-KDOUTMETH-13                                            
221200            ,:MAP-KDOUTMETH-14                                            
221300            ,:MAP-KDOUTMETH-15                                            
221400            ,:MAP-IDOUTDEST-1                                             
221500            ,:MAP-IDOUTDEST-2                                             
221600            ,:MAP-IDOUTDEST-3                                             
221700            ,:MAP-IDOUTDEST-4                                             
221800            ,:MAP-IDOUTDEST-5                                             
221900            ,:MAP-IDOUTDEST-6                                             
222000            ,:MAP-IDOUTDEST-7                                             
222100            ,:MAP-IDOUTDEST-8                                             
222200            ,:MAP-IDOUTDEST-9                                             
222300            ,:MAP-IDOUTDEST-10                                            
222400            ,:MAP-IDOUTDEST-11                                            
222500            ,:MAP-IDOUTDEST-12                                            
222600            ,:MAP-IDOUTDEST-13                                            
222700            ,:MAP-IDOUTDEST-14                                            
222800            ,:MAP-IDOUTDEST-15                                            
222900            ,:MAP-KVCOPIES-1                                              
223000            ,:MAP-KVCOPIES-2                                              
223100            ,:MAP-KVCOPIES-3                                              
223200            ,:MAP-KVCOPIES-4                                              
223300            ,:MAP-KVCOPIES-5                                              
223400            ,:MAP-KVCOPIES-6                                              
223500            ,:MAP-KVCOPIES-7                                              
223600            ,:MAP-KVCOPIES-8                                              
223700            ,:MAP-KVCOPIES-9                                              
223800            ,:MAP-KVCOPIES-10                                             
223900            ,:MAP-KVCOPIES-11                                             
224000            ,:MAP-KVCOPIES-12                                             
224100            ,:MAP-KVCOPIES-13                                             
224200            ,:MAP-KVCOPIES-14                                             
224300            ,:MAP-KVCOPIES-15                                             
224400            ,:MAP-FLCARRCNTL-1                                            
224500            ,:MAP-FLCARRCNTL-2                                            
224600            ,:MAP-FLCARRCNTL-3                                            
224700            ,:MAP-FLCARRCNTL-4                                            
224800            ,:MAP-FLCARRCNTL-5                                            
224900            ,:MAP-FLCARRCNTL-6                                            
225000            ,:MAP-FLCARRCNTL-7                                            
225100            ,:MAP-FLCARRCNTL-8                                            
225200            ,:MAP-FLCARRCNTL-9                                            
225300            ,:MAP-FLCARRCNTL-10                                           
225400            ,:MAP-FLCARRCNTL-11                                           
225500            ,:MAP-FLCARRCNTL-12                                           
225600            ,:MAP-FLCARRCNTL-13                                           
225700            ,:MAP-FLCARRCNTL-14                                           
225800            ,:MAP-FLCARRCNTL-15                                           
225900            ,:MAP-IDPFDEF-1                                               
226000            ,:MAP-IDPFDEF-2                                               
226100            ,:MAP-IDPFDEF-3                                               
226200            ,:MAP-IDPFDEF-4                                               
226300            ,:MAP-IDPFDEF-5                                               
226400            ,:MAP-IDPFDEF-6                                               
226500            ,:MAP-IDPFDEF-7                                               
226600            ,:MAP-IDPFDEF-8                                               
226700            ,:MAP-IDPFDEF-9                                               
226800            ,:MAP-IDPFDEF-10                                              
226900            ,:MAP-IDPFDEF-11                                              
227000            ,:MAP-IDPFDEF-12                                              
227100            ,:MAP-IDPFDEF-13                                              
227200            ,:MAP-IDPFDEF-14                                              
227300            ,:MAP-IDPFDEF-15                                              
227400            ,:MAP-IDFORMSNM-1                                             
227500            ,:MAP-IDFORMSNM-2                                             
227600            ,:MAP-IDFORMSNM-3                                             
227700            ,:MAP-IDFORMSNM-4                                             
227800            ,:MAP-IDFORMSNM-5                                             
227900            ,:MAP-IDFORMSNM-6                                             
228000            ,:MAP-IDFORMSNM-7                                             
228100            ,:MAP-IDFORMSNM-8                                             
228200            ,:MAP-IDFORMSNM-9                                             
228300            ,:MAP-IDFORMSNM-10                                            
228400            ,:MAP-IDFORMSNM-11                                            
228500            ,:MAP-IDFORMSNM-12                                            
228600            ,:MAP-IDFORMSNM-13                                            
228700            ,:MAP-IDFORMSNM-14                                            
228800            ,:MAP-IDFORMSNM-15                                            
228900            ,:MAP-TEVCOMST-1                                              
229000            ,:MAP-TEVCOMST-2                                              
229100            ,:MAP-TEVCOMST-3                                              
229200            ,:MAP-TEVCOMST-4                                              
229300            ,:MAP-TEVCOMST-5                                              
229400            ,:MAP-TEVCOMST-6                                              
229500            ,:MAP-TEVCOMST-7                                              
229600            ,:MAP-TEVCOMST-8                                              
229700            ,:MAP-TEVCOMST-9                                              
229800            ,:MAP-TEVCOMST-10                                             
229900            ,:MAP-TEVCOMST-11                                             
230000            ,:MAP-TEVCOMST-12                                             
230100            ,:MAP-TEVCOMST-13                                             
230200            ,:MAP-TEVCOMST-14                                             
230300            ,:MAP-TEVCOMST-15                                             
230400            ,:MAP-IDVCINIT-1                                              
230500            ,:MAP-IDVCINIT-2                                              
230600            ,:MAP-IDVCINIT-3                                              
230700            ,:MAP-IDVCINIT-4                                              
230800            ,:MAP-IDVCINIT-5                                              
230900            ,:MAP-IDVCINIT-6                                              
231000            ,:MAP-IDVCINIT-7                                              
231100            ,:MAP-IDVCINIT-8                                              
231200            ,:MAP-IDVCINIT-9                                              
231300            ,:MAP-IDVCINIT-10                                             
231400            ,:MAP-IDVCINIT-11                                             
231500            ,:MAP-IDVCINIT-12                                             
231600            ,:MAP-IDVCINIT-13                                             
231700            ,:MAP-IDVCINIT-14                                             
231800            ,:MAP-IDVCINIT-15                                             
231900            ,:MAP-TEFAX-1                                                 
232000            ,:MAP-TEFAX-2                                                 
232100            ,:MAP-TEFAX-3                                                 
232200            ,:MAP-TEFAX-4                                                 
232300            ,:MAP-TEFAX-5                                                 
232400            ,:MAP-FLACIF-1                                                
232500            ,:MAP-FLACIF-2                                                
232600            ,:MAP-FLACIF-3                                                
232700            ,:MAP-FLACIF-4                                                
232800            ,:MAP-FLACIF-5                                                
232900            ,:MAP-FLACIF-6                                                
233000            ,:MAP-FLACIF-7                                                
233100            ,:MAP-FLACIF-8                                                
233200            ,:MAP-FLACIF-9                                                
233300            ,:MAP-FLACIF-10                                               
233400            ,:MAP-FLACIF-11                                               
233500            ,:MAP-FLACIF-12                                               
233600            ,:MAP-FLACIF-13                                               
233700            ,:MAP-FLACIF-14                                               
233800            ,:MAP-FLACIF-15                                               
233900            ,:MAP-KVDAGAR-RESEND                                          
234000            ,:REQU-IDMAIL-SENDER                                          
234100            ,:MAP-TIREGDAT                                                
234200            ,:MAP-TIUPPDAT                                                
234300            ,:REQU-IDUSER                                                 
234400            ,:MAP-TENOTE)                                                 
234500     END-EXEC                                                             
234600                                                                          
234700     MOVE SQLCODE TO SQLCODE-WS                                           
234800     PERFORM DB2-STATUS-CHECK                                             
234900     .                                                                    
235000 DB2-DELETE-TZ4DIRU-TAB  SECTION.                                         
235100                                                                          
235200     MOVE 000   TO GOOD-SQLCODECODES                                      
235300                                                                          
235400     EXEC SQL                                                             
235500        DELETE FROM TZ4DIRU                                               
235600                                                                          
235700        WHERE IDOUTTYPE     = :REQU-IDOUTTYPE-KEY                         
235800         AND  IDOUTREC_FROM = :REQU-IDOUTREC-FROM-KEY                     
235900         AND  IDOUTREC_TO   = :REQU-IDOUTREC-TO-KEY                       
236000     END-EXEC                                                             
236100                                                                          
236200     MOVE SQLCODE TO SQLCODE-WS                                           
236300     PERFORM DB2-STATUS-CHECK                                             
236400     .                                                                    
236500 DB2-STATUS-CHECK  SECTION.                                               
236600                                                                          
236700     SET SQLCODE-IX TO 1                                                  
236800     SEARCH GOOD-SQLCODE                                                  
236900       AT END                                                             
237000          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
237100          DELIMITED BY SIZE INTO ERROR-TEXT                               
237200          CALL ABEND USING RKOD-ABEND-DB2                                 
237300       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
237400     END-SEARCH                                                           
237500     .                                                                    
