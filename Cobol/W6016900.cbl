000100*COMPOPT VPOSIX=YES                                                       
000200**********************************************************                
000300 ID DIVISION.                                                             
000400 PROGRAM-ID.     W6016900.                                                
000500 AUTHOR.         GÖRAN KJELLSON.                                          
000600 DATE-WRITTEN.   20/02/25.                                                
000700 DATE-COMPILED.                                                           
000800                                                                          
000900*    FUNCTION:                                                            
001000*        PART MEASUREMENTS, SHOW AND UPDATE                               
001100*                                                                         
001200*        THE PROGRAM READS     WDK2                                       
001300*        THE PROGRAM READS     WDD3                                       
001400*        THE PROGRAM UPDATES   WDK6                                       
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSACTION: W6T169                                              
001800*        MID:         W6I16901                                            
001900*                                                                         
002000*    OUTDATA.                                                             
002100*        MOD:         W6O16901                                            
002200                                                                          
002300                                                                          
002400 ENVIRONMENT DIVISION.                                                    
002500                                                                          
002600 DATA DIVISION.                                                           
002700 WORKING-STORAGE SECTION.                                                 
002800 77  IDPGM                       PIC X(08)   VALUE 'W6016900'.            
002900                                                                          
003000*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003100 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003200 77  CURRENT-SECTION             PIC X(16) VALUE SPACE.                   
003300 77  CURRENT-IMS-SECTION         PIC X(16) VALUE SPACE.                   
003400                                                                          
003500 77  CURRENT-DATE                PIC 9(6)  VALUE ZERO.                    
003600 77  CURRENT-TIME                PIC 9(8)  VALUE ZERO.                    
003700                                                                          
003800 77  YES                         PIC X     VALUE 'Y'.                     
003900 77  NOO                         PIC X     VALUE 'N'.                     
004000                                                                          
004100 77  LNG-P-TO-P-PREFIX           PIC S9(4) COMP SYNC VALUE +17.           
004200                                                                          
004300 77  WS-UPD-VOLUME               PIC X(1)  VALUE SPACE.                   
004400 77  WS-KDUVKNTO                 PIC X(1)  VALUE SPACE.                   
004500 01  WC-UPD-MAN                  PIC X(1)  VALUE '2'.                     
004600 01  WC-UPD-VKART                PIC X(1)  VALUE '4'.                     
004700                                                                          
004800 77  WS-VKART-NTO-CURR           PIC S9(9)       VALUE ZERO.              
004900 77  WS-VKART-BTO-CURR           PIC S9(9)       VALUE ZERO.              
005000 01  WS-VLARTNTO-CURR-X.                                                  
005100     03 WS-VLARTNTO-CURR         PIC S9(8)V9(1)  VALUE ZERO.              
005200                                                                          
005300 77  WS-VLARTNTO                 PIC S9(8)V9(1)  VALUE ZERO.              
005400                                                                          
005500 01  WS-VLARTNTO-DISP-NUM        PIC 9(8)V9(1).                           
005600 01  WS-VLARTNTO-DISP-XX  REDEFINES WS-VLARTNTO-DISP-NUM.                 
005700     05 WS-VLARTNTO-DISP-ALFA    PIC X(9).                                
005800*                                                                         
005900*01    -COPY WWDCKONS                                                     
006000                                                                          
006100*01    -COPY WWLNDKON                                                     
006200                                                                          
006300 77  INDATA-SW                   PIC X       VALUE 'Y'.                   
006400     88  INDATA-OK                           VALUE 'Y'.                   
006500     88  INDATA-WRONG                        VALUE 'N'.                   
006600                                                                          
006700 77  KEYS-SW                     PIC X       VALUE 'Y'.                   
006800     88  KEYS-OK                             VALUE 'Y'.                   
006900     88  KEYS-WRONG                          VALUE 'N'.                   
007000                                                                          
007100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007200     88  OWN-MID                             VALUE '6169'.                
007300     88  GOOD-MID                            VALUE '6161' '6162'          
007400                                                   '6163' '6164'          
007500                                                   '6165' '6166'          
007600                                                   '6167' '6168'          
007700                                                   '6169'.                
007800     88  HELP-MID                            VALUE '0551'.                
007900                                                                          
008000*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
008100 01  GENERAL-SUBPROGRAMS.                                                 
008200     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
008400     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
008500     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
008600     03  W488PRMA                PIC X(8)    VALUE 'W488PRMA'.            
008700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008900                                                                          
009000*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
009100*01 -COPY WMEDAREA                                                        
009200                                                                          
009300 01  MESSAGE-CODES.                                                       
009400     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
009500     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
009600     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
009700     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
009800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
009900     03  ERR-IDARTNR-SAKNAS      PIC X(3)    VALUE '017'.                 
010000                                                                          
010100 01  FILLER                      PIC X(16)  VALUE 'WWOMVAND '.            
010200*   -COPY WWOMVAND                                                        
010300                                                                          
010400*01 -COPY WDECAREA                                                        
010500                                                                          
010600*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
010700*                                                                         
010800 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
010900                                                                          
011000*01 -COPY WMSGINIT                                                        
011100                                                                          
011200*    --- PARAMETERS FOR SUB PROGRAM W488PRMA                              
011300*                                                                         
011400 01  FILLER                      PIC X(16)   VALUE 'W488PRMA'.            
011500                                                                          
011600*01 -COPY W488PRMA                                                        
011700                                                                          
011800*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
011900*                                                                         
012000 01  SAVE-AREA.                                                           
012100     03  SAVE-IDTRANS           PIC X(4)    VALUE '6169'.                 
012200                                                                          
012300*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
012400*                                                                         
012500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
012600                                                                          
012700*01  MID -COPY W6I16901                                                   
012800                                                                          
012900                                                                          
013000 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
013100                                                                          
013200*01  -COPY WMSGAREA                                                       
013300                                                                          
013400     03  MOD REDEFINES MSG-AREA.                                          
013500*      05  -COPY W6O16901                                                 
013600                                                                          
013700                                                                          
013800 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
013900                                                                          
014000*01  -COPY WMFSAREA                                                       
014100                                                                          
014200 01  FILLER                   PIC X(16)  VALUE 'KOM-MSG-IO-AREA '.        
014300 01  KOM-MSG-IO-AREA.                                                     
014400*03  -COPY WMSGKOM                                                        
014500                                                                          
014600 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-SW'.           
014700 01  P-TO-P-SW.                                                           
014800     02     P-TO-P-KVLL             PIC S9(4) COMP SYNC.                  
014900     02     P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.            
015000     02     P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.            
015100     02     P-TO-P-KDTRANS          PIC X(8).                             
015200     02     P-TO-P-IDTRANS          PIC X(4).                             
015300     02     P-TO-P-KDMFSFOR         PIC X(1).                             
015400     02     P-TO-P-DATA             PIC X(1000).                          
015500                                                                          
015600 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-SW2'.          
015700 01  P-TO-P-SW2.                                                          
015800     02     P-TO-P2-KVLL             PIC S9(4) COMP SYNC.                 
015900     02     P-TO-P2-KDZ1             PIC X(1)  VALUE LOW-VALUE.           
016000     02     P-TO-P2-KDZ2             PIC X(1)  VALUE LOW-VALUE.           
016100     02     P-TO-P2-KDTRANS          PIC X(8).                            
016200     02     P-TO-P2-IDTRANS          PIC X(4).                            
016300     02     P-TO-P2-KDMFSFOR         PIC X(1).                            
016400     02     MID -COPY W4I28901   -PRE P-TO-P2-                            
016500*****************************************************************         
016600                                                                          
016700                                                                          
016800 01      FILLER                  PIC X(24)   VALUE                        
016900                                 '619B-MID-W6I19B01'.                     
017000     SKIP2                                                                
017100     -COPY W6I19B01 -PRE 619B-                                            
017200                                                                          
017300*    --- WORK-AREAS FOR IMS-SECTIONS                                      
017400*                                                                         
017500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017600                                                                          
017700 01  KEYS-FOR-DLI.                                                        
017800     03  W-IDARTNR-X.                                                     
017900         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
018000     03  W-KDSEGKEY-X.                                                    
018100         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
018200     03  W-IDBENNR-X.                                                     
018300         05  W-IDBENNR           PIC S9(7)   VALUE ZERO COMP-3.           
018400     03  W-IDSKYLT-X.                                                     
018500         05  W-IDSKYLT           PIC X(3)    VALUE 'GB'.                  
018600     03  W-IDLAND-X.                                                      
018700         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
018800                                                                          
018900*    --- STATUS CODES FROM IMS                                            
019000 01  STATUS-WS                   PIC XX.                                  
019100     88  SEGMENT-FOUND                       VALUE '  '.                  
019200     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
019300     88  SEGMENT-MISSING                     VALUE 'GE'.                  
019400                                                                          
019500 01  GOOD-STATUSCODES.                                                    
019600     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019700                                                                          
019800 01  ALL-SSA.                                                             
019900     03 SSA1                     PIC X(64).                               
020000     03 SSA2                     PIC X(64).                               
020100                                                                          
020200*    --- IMS FUNCTION CODES                                               
020300*01  -COPY W0003                                                          
020400                                                                          
020500*    ---  DLI INPUT-OUTPUT AREA                                           
020600                                                                          
020700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK201'.                      
020800 01  DLI-IO-WDK201.                                                       
020900*    03  -COPY WDK201                                                     
021000                                                                          
021100                                                                          
021200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK211'.                      
021300 01  DLI-IO-WDK211.                                                       
021400*    03  -COPY WDK211                                                     
021500                                                                          
021600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK212'.                      
021700 01  DLI-IO-WDK212.                                                       
021800*    03  -COPY WDK212                                                     
021900                                                                          
022000                                                                          
022100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD301'.                      
022200 01  DLI-IO-WDD301.                                                       
022300*    03  -COPY WDD301                                                     
022400                                                                          
022500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
022600 01  DLI-IO-WDD311.                                                       
022700*    03  -COPY WDD311                                                     
022800                                                                          
022900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
023000 01  DLI-IO-WDK601.                                                       
023100*    03  -COPY WDK601                                                     
023200                                                                          
023300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
023400 01  DLI-IO-WDK611.                                                       
023500*    03  -COPY WDK611                                                     
023600                                                                          
023700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK712'.                      
023800 01  DLI-IO-WDK712.                                                       
023900*    03  -COPY WDK712                                                     
024000                                                                          
024100                                                                          
024200 LINKAGE SECTION.                                                         
024300*01  -COPY W0009  -PRE MSG-                                               
024400*01  -COPY W0009  -PRE DISP-                                              
024410*01  -COPY W0009  -PRE 4289-                                              
024500*01  -COPY W0009  -PRE SYNQ-                                              
024600 01  KOM-KOMA-PCB              PIC X.                                     
024700*01  -COPY W0008  -PRE WDP7-                                              
024800     05  FILLER                PIC X.                                     
024900                                                                          
025000*01  -COPY W0008  -PRE WDK2-                                              
025100     05  FILLER                PIC X.                                     
025200                                                                          
025300*01  -COPY W0008  -PRE WDD3-                                              
025400     05  FILLER                PIC X.                                     
025500                                                                          
025600*01  -COPY W0008  -PRE WDK6-                                              
025700     05  FILLER                PIC X.                                     
025800                                                                          
025900*01  -COPY W0008  -PRE WDK7-                                              
026000     05  FILLER                PIC X.                                     
026100                                                                          
026200 01  SYNQ-ATAB-PCB             PIC X.                                     
026300 01  SYNQ-WDK6-PCB             PIC X.                                     
026400 01  SYNQ-WDD3-PCB             PIC X.                                     
026500                                                                          
026600                                                                          
026700 PROCEDURE DIVISION  USING MSG-PCB  DISP-PCB 4289-PCB                     
026710                           SYNQ-PCB KOM-KOMA-PCB                          
026800                           WDP7-PCB WDK2-PCB WDD3-PCB                     
026900                           WDK6-PCB WDK7-PCB SYNQ-ATAB-PCB                
027000                           SYNQ-WDK6-PCB SYNQ-WDD3-PCB.                   
027100 MAIN SECTION.                                                            
027200     ENTRY 'DLITCBL' USING MSG-PCB  DISP-PCB 4289-PCB                     
027210                           SYNQ-PCB KOM-KOMA-PCB                          
027300                           WDP7-PCB WDK2-PCB WDD3-PCB                     
027400                           WDK6-PCB WDK7-PCB SYNQ-ATAB-PCB                
027500                           SYNQ-WDK6-PCB SYNQ-WDD3-PCB.                   
027600                                                                          
027700     PERFORM IMS-GET-MSG                                                  
027800     IF SEGMENT-FOUND                                                     
027900        PERFORM A-INIT                                                    
028000        PERFORM B-CHECK-KEYS                                              
028100        IF KEYS-OK                                                        
028200           IF MFS-UPDATE                                                  
028300              PERFORM G-CHECK-INPUT                                       
028400              IF INDATA-OK                                                
028500                 PERFORM H-UPDATE                                         
028600                 PERFORM F-READ-SHOW-INFO                                 
028700              END-IF                                                      
028800           ELSE                                                           
028900              PERFORM F-READ-SHOW-INFO                                    
029000           END-IF                                                         
029100        END-IF                                                            
029200        COMPUTE MSG-KVLL = LENGTH OF MOD-W6O16901 + 4                     
029300        PERFORM IMS-INSERT-MSG                                            
029400     END-IF                                                               
029500                                                                          
029600     MOVE ZERO TO RETURN-CODE                                             
029700     GOBACK                                                               
029800     .                                                                    
029900                                                                          
030000                                                                          
030100 A-INIT SECTION.                                                          
030200     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
030300                                                                          
030400     MOVE ALL '+'                       TO 619B-MID-W6I19B01              
030500                                                                          
030600     IF MSG-DOUBLE-TRANSACTIONS                                           
030700       MOVE MSG-INDATA-MINUS-2-TRANSACT TO MID-W6I16901                   
030800       MOVE MSG-IDTRANS-2               TO MFS-IDTRANS                    
030900       MOVE MSG-KDMFSFOR-2              TO MFS-KDMFSFOR                   
031000     ELSE                                                                 
031100       MOVE MSG-INDATA-MINUS-1-TRANSACT TO MID-W6I16901                   
031200       MOVE MSG-IDTRANS-1               TO MFS-IDTRANS                    
031300       MOVE MSG-KDMFSFOR-1              TO MFS-KDMFSFOR                   
031400     END-IF                                                               
031500                                                                          
031600     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
031700     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
031800     MOVE MFS-IDTRANS TO W-IDTRANS                                        
031900                                                                          
032000     MOVE LOW-VALUE       TO MSG-AREA                                     
032100     MOVE 'W6O169N1'      TO MFS-IDMOD                                    
032200     MOVE '6169'          TO MOD-IDTRANS                                  
032300     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
032400                                                                          
032500     IF OWN-MID OR HELP-MID                                               
032600       CONTINUE                                                           
032700     ELSE                                                                 
032800       MOVE SPACE TO MFS-KDTRTYP                                          
032900       MOVE '7'   TO MFS-IDPFK                                            
033000     END-IF                                                               
033100     ACCEPT CURRENT-DATE       FROM DATE                                  
033200     ACCEPT CURRENT-TIME       FROM TIME                                  
033300     .                                                                    
033400                                                                          
033500                                                                          
033600 B-CHECK-KEYS SECTION.                                                    
033700     MOVE 'B-CHECK-KEYS    ' TO CURRENT-SECTION                           
033800                                                                          
033900     MOVE ALL '+'            TO MSGI-WMSGINIT                             
034000     MOVE '001'              TO MSGI-KDCALL                               
034100     MOVE MSG-LTERM-NAME     TO MSGI-IDLTERM-USER                         
034200     MOVE MSG-SIGNON-USERID  TO MSGI-IDUSER                               
034300     MOVE '6169'             TO MSGI-IDTRANS                              
034400     IF GOOD-MID                                                          
034500         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
034600     END-IF                                                               
034700     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
034800     MOVE MSGI-SPAR-AREA     TO SAVE-AREA                                 
034900                                                                          
035000     MOVE 'GB'               TO MED-IDSKYLT                               
035100                                                                          
035200     MOVE YES                TO KEYS-SW                                   
035300                                                                          
035400                                                                          
035500*    -- CHECK OF IDARTNR                                                  
035600     MOVE MFS-ERASE-FIELD TO MOD-IDARTNR-IN                               
035700                                                                          
035800     IF MID-IDARTNR-IN NOT = ALL '+'                                      
035900       MOVE '7'           TO MFS-IDPFK                                    
036000       MOVE SPACE         TO MFS-KDTRTYP                                  
036100     END-IF                                                               
036200     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
036300     IF MSGI-IDARTNR NUMERIC                                              
036400       MOVE MSGI-IDARTNR  TO W-IDARTNR                                    
036500     ELSE                                                                 
036600       MOVE NOO TO KEYS-SW                                                
036700     END-IF                                                               
036800                                                                          
036900     IF GOOD-MID OR KEYS-OK                                               
037000       MOVE MSGI-IDARTNR    TO MOD-IDARTNR-UT                             
037100       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
037200     ELSE                                                                 
037300       MOVE MFS-ERASE-FIELD TO MOD-IDARTNR-UT                             
037400     END-IF                                                               
037500                                                                          
037600     IF KEYS-OK                                                           
037700       PERFORM IMS-GHU-WDK611                                             
037800       IF SEGMENT-MISSING                                                 
037900          MOVE NOO TO KEYS-SW                                             
038000       END-IF                                                             
038100     END-IF                                                               
038200                                                                          
038300     IF KEYS-WRONG                                                        
038400       IF SEGMENT-MISSING                                                 
038500          MOVE ERR-IDARTNR-SAKNAS TO MED-IDMFSFEL                         
038600       ELSE                                                               
038700          MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                              
038800       END-IF                                                             
038900       CALL WMEDKONV USING MED-WMEDAREA                                   
039000       MOVE MED-MFSFEL    TO MOD-TEMFSFEL                                 
039100       PERFORM MFS-ERASE-FIELD-IN                                         
039200       PERFORM MFS-ERASE-FIELD-OUT                                        
039300     END-IF                                                               
039400     .                                                                    
039500                                                                          
039600                                                                          
039700 F-READ-SHOW-INFO SECTION.                                                
039800     MOVE 'F-READ-SHOW-INFO' TO CURRENT-SECTION                           
039900                                                                          
040000     PERFORM MFS-FORM-ATTR                                                
040100     PERFORM MFS-ERASE-FIELD-IN                                           
040200                                                                          
040300     PERFORM FA-READ-WDD3                                                 
040400     PERFORM FB-READ-WDK2                                                 
040500     PERFORM FC-READ-WDK6                                                 
040600                                                                          
040700     IF SEGMENT-MISSING                                                   
040800        MOVE ERR-IDARTNR-SAKNAS TO MED-IDMFSFEL                           
040900        CALL WMEDKONV USING MED-WMEDAREA                                  
041000        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
041100        PERFORM MFS-ERASE-FIELD-OUT                                       
041200     END-IF                                                               
041300     .                                                                    
041400                                                                          
041500                                                                          
041600 FA-READ-WDD3 SECTION.                                                    
041700     MOVE 'FA-READ-WDD3    ' TO CURRENT-SECTION                           
041800                                                                          
041900     PERFORM IMS-GU-WDD301                                                
042000     IF SEGMENT-FOUND                                                     
042100        PERFORM IMS-GNP-WDD311                                            
042200        IF SEGMENT-FOUND                                                  
042300           MOVE TEXT-BEART TO MOD-BEART                                   
042400        END-IF                                                            
042500     END-IF                                                               
042600     .                                                                    
042700                                                                          
042800                                                                          
042900 FB-READ-WDK2 SECTION.                                                    
043000     MOVE 'FB-READ-WDK2    ' TO CURRENT-SECTION                           
043100                                                                          
043200     PERFORM IMS-GU-WDK201                                                
043300     IF SEGMENT-FOUND                                                     
043400        PERFORM IMS-GNP-WDK211                                            
043500        IF SEGMENT-FOUND                                                  
043600          MOVE MATD-KDVSOP       TO MOD-KDVSOP                            
043700          INSPECT MOD-KDVSOP REPLACING LEADING ZERO BY SPACE              
043800          MOVE MATD-KVHEIGHT-BTO TO MOD-KVHEIGHT-BTO                      
043900          INSPECT MOD-KVHEIGHT-BTO REPLACING LEADING ZERO BY SPACE        
044000          MOVE MATD-KVHEIGHT-NTO TO MOD-KVHEIGHT-NTO                      
044100          INSPECT MOD-KVHEIGHT-NTO REPLACING LEADING ZERO BY SPACE        
044200          MOVE MATD-KVLENGTH-BTO TO MOD-KVLENGTH-BTO                      
044300          INSPECT MOD-KVLENGTH-BTO REPLACING LEADING ZERO BY SPACE        
044400          MOVE MATD-KVLENGTH-NTO TO MOD-KVLENGTH-NTO                      
044500          INSPECT MOD-KVLENGTH-NTO REPLACING LEADING ZERO BY SPACE        
044600          MOVE MATD-KVWIDTH-BTO  TO MOD-KVWIDTH-BTO                       
044700          INSPECT MOD-KVWIDTH-BTO REPLACING LEADING ZERO BY SPACE         
044800          MOVE MATD-KVWIDTH-NTO  TO MOD-KVWIDTH-NTO                       
044900          INSPECT MOD-KVWIDTH-NTO REPLACING LEADING ZERO BY SPACE         
045000          MOVE MATD-TIUPPDAT     TO MOD-TIUPPDAT-MASK                     
045100          MOVE MATD-VKART-BTO    TO MOD-VKART-BTO                         
045200          INSPECT MOD-VKART-BTO REPLACING LEADING ZERO BY SPACE           
045300          MOVE MATD-VKART-NTO    TO MOD-VKART-NTO                         
045400          INSPECT MOD-VKART-NTO REPLACING LEADING ZERO BY SPACE           
045500          MOVE MATD-VLARTNTO     TO MOD-VLARTNTO                          
045600          INSPECT MOD-VLARTNTO REPLACING LEADING ZERO BY SPACE            
045700        END-IF                                                            
045800        PERFORM IMS-GNP-WDK212                                            
045900        IF SEGMENT-FOUND                                                  
046000          MOVE KDP-KVANTAL     TO MOD-KVANTAL-KDP                         
046100          INSPECT MOD-KVANTAL-KDP REPLACING LEADING ZERO BY SPACE         
046200          MOVE KDP-VKART-NTO   TO MOD-VKART-NTO-KDP                       
046300         INSPECT MOD-VKART-NTO-KDP REPLACING LEADING ZERO BY SPACE        
046400        END-IF                                                            
046500     END-IF                                                               
046600     .                                                                    
046700                                                                          
046800                                                                          
046900 FC-READ-WDK6 SECTION.                                                    
047000     MOVE 'FC-READ-WDK6    ' TO CURRENT-SECTION                           
047100                                                                          
047200     PERFORM IMS-GU-WDK601                                                
047300     IF SEGMENT-FOUND                                                     
047400                                                                          
047500       MOVE '-'                 TO MOD-STRECK                             
047600       MOVE ART-REKSIFFR        TO MOD-REKSIFFR                           
047700                                                                          
047800       PERFORM IMS-GHNP-WDK611                                            
047900       IF SEGMENT-FOUND                                                   
048000         MOVE CLAG-VKART-NTO   TO MOD-VKART-NTO-UT                        
048100         INSPECT MOD-VKART-NTO-UT REPLACING LEADING ZERO BY SPACE         
048200         MOVE CLAG-VKART       TO MOD-VKART-BTO-UT                        
048300         INSPECT MOD-VKART-BTO-UT REPLACING LEADING ZERO BY SPACE         
048400         MOVE CLAG-VLARTNTO    TO MOD-VLARTNTO-UT                         
048500         INSPECT MOD-VLARTNTO-UT REPLACING LEADING ZERO BY SPACE          
048600         MOVE CLAG-KDVSOP      TO MOD-KDVSOP-UT                           
048700         INSPECT MOD-KDVSOP-UT REPLACING LEADING ZERO BY SPACE            
048800         MOVE CLAG-IDUSER-VUPD TO MOD-IDUSER                              
048900         MOVE CLAG-TIUPPDAT-VUPD TO MOD-TIUPPDAT                          
049000       END-IF                                                             
049100     END-IF                                                               
049200     .                                                                    
049300                                                                          
049400                                                                          
049500 G-CHECK-INPUT SECTION.                                                   
049600     MOVE 'G-CHECK-INPUT   ' TO CURRENT-SECTION                           
049700                                                                          
049800     MOVE YES   TO INDATA-SW                                              
049900     MOVE SPACE TO WS-KDUVKNTO                                            
050000                                                                          
050100     IF MID-INPUT = ALL '+'                                               
050200       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
050300       CALL WMEDKONV USING MED-WMEDAREA                                   
050400       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
050500       PERFORM MFS-DONT-TOUCH-FIELD-IN                                    
050600       PERFORM MFS-DONT-TOUCH-FIELD-OUT                                   
050700       MOVE NOO                  TO INDATA-SW                             
050800     ELSE                                                                 
050900                                                                          
051000       PERFORM IMS-GHU-WDK611                                             
051100       IF SEGMENT-FOUND                                                   
051200                                                                          
051300        IF MID-VKART-NTO NOT = ALL '+'                                    
051400          IF MID-VKART-NTO NOT NUMERIC                                    
051500          OR MID-VKART-NTO = ZERO                                         
051600             MOVE MFS-NUM-FIELD-WRONG TO MOD-VKART-NTO-IN-ATTR            
051700             MOVE NOO                 TO INDATA-SW                        
051800          ELSE                                                            
051900             MOVE MFS-NUM-FIELD-OK    TO MOD-VKART-NTO-IN-ATTR            
052000             MOVE MID-VKART-NTO       TO WS-VKART-NTO-CURR                
052100             MOVE WC-UPD-MAN          TO WS-KDUVKNTO                      
052200          END-IF                                                          
052300        ELSE                                                              
052400          MOVE CLAG-VKART-NTO         TO WS-VKART-NTO-CURR                
052500        END-IF                                                            
052600                                                                          
052700        IF MID-VKART-BTO NOT = ALL '+'                                    
052800          IF MID-VKART-BTO NOT NUMERIC                                    
052900          OR MID-VKART-BTO = ZERO                                         
053000             MOVE MFS-NUM-FIELD-WRONG TO MOD-VKART-BTO-IN-ATTR            
053100             MOVE NOO                 TO INDATA-SW                        
053200          ELSE                                                            
053300             MOVE MFS-NUM-FIELD-OK    TO MOD-VKART-BTO-IN-ATTR            
053400             MOVE MID-VKART-BTO       TO WS-VKART-BTO-CURR                
053500             MOVE MID-VKART-BTO(2:7)  TO 619B-MID-VKART                   
053600          END-IF                                                          
053700        ELSE                                                              
053800          MOVE CLAG-VKART             TO WS-VKART-BTO-CURR                
053900        END-IF                                                            
054000                                                                          
054100        IF MID-VLARTNTO              NOT = ALL '+'                        
054200         INSPECT MID-VLARTNTO REPLACING LEADING SPACE BY ZERO             
054300         MOVE MID-VLARTNTO          TO DEC-IDFRIDATA                      
054400         MOVE  8                    TO DEC-KVHELTAL                       
054500         MOVE  1                    TO DEC-KVDECIMAL                      
054600         CALL WDECEDIT              USING DEC-WDECAREA                    
054700         IF DEC-KDSVAR-OK                                                 
054800           MOVE DEC-IDEDITDATA      TO WS-VLARTNTO                        
054900           MOVE MFS-NUM-FAELT-RAETT TO MOD-VLARTNTO-IN-ATTR               
055000         ELSE                                                             
055100           MOVE MFS-NUM-FAELT-FEL   TO MOD-VLARTNTO-IN-ATTR               
055200           MOVE NOO                 TO INDATA-SW                          
055300         END-IF                                                           
055400                                                                          
055500         IF INDATA-OK                                                     
055600           IF WS-VLARTNTO NOT NUMERIC OR                                  
055700              WS-VLARTNTO = ZERO                                          
055800             MOVE MFS-NUM-FAELT-FEL     TO MOD-VLARTNTO-IN-ATTR           
055900             MOVE NOO                   TO INDATA-SW                      
056000           ELSE                                                           
056100             IF MSGI-KDMATT           = 'U'                               
056200               COMPUTE WS-VLARTNTO ROUNDED =                              
056300                                     WS-VLARTNTO * CONV-IN3-TO-CM3        
056400               END-COMPUTE                                                
056500             END-IF                                                       
056600             MOVE WS-VLARTNTO           TO WS-VLARTNTO-DISP-NUM           
056700                                                                          
056800******* 99999999.9 = MAX-VÄRDE VOLYM (CM3)                                
056900             IF WS-VLARTNTO > 99999999.9                                  
057000               MOVE MFS-NUM-FAELT-FEL   TO MOD-VLARTNTO-IN-ATTR           
057100               MOVE NOO                 TO INDATA-SW                      
057200             END-IF                                                       
057300             IF INDATA-OK                                                 
057400               MOVE WS-VLARTNTO         TO WS-VLARTNTO-DISP-NUM           
057500               MOVE WS-VLARTNTO-DISP-ALFA                                 
057600                                        TO 619B-MID-VLARTNTO              
057700             END-IF                                                       
057800                                                                          
057900           END-IF                                                         
058000         END-IF                                                           
058100        ELSE                                                              
058200         MOVE MFS-NUM-FAELT-RAETT       TO MOD-VLARTNTO-IN-ATTR           
058300         MOVE CLAG-VLARTNTO             TO WS-VLARTNTO-CURR               
058400        END-IF                                                            
058500       END-IF                                                             
058600                                                                          
058700       IF INDATA-OK                                                       
058800         IF WS-VKART-NTO-CURR > WS-VKART-BTO-CURR                         
058900            IF MID-VKART-NTO NOT = ALL '+'                                
059000             MOVE MFS-NUM-FIELD-WRONG TO MOD-VKART-NTO-IN-ATTR            
059100             MOVE NOO                 TO INDATA-SW                        
059200            END-IF                                                        
059300            IF MID-VKART-BTO NOT = ALL '+'                                
059400             MOVE MFS-NUM-FIELD-WRONG TO MOD-VKART-BTO-IN-ATTR            
059500             MOVE NOO                 TO INDATA-SW                        
059600            END-IF                                                        
059700         END-IF                                                           
059800                                                                          
059900         IF MID-VKART-BTO NOT = ALL '+' AND                               
060000            MID-VKART-NTO = ALL '+'                                       
060100            IF CLAG-VKART-NTO = ZERO                                      
060200               MOVE MID-VKART-BTO     TO MID-VKART-NTO                    
060300               MOVE WC-UPD-VKART      TO WS-KDUVKNTO                      
060400            END-IF                                                        
060500         END-IF                                                           
060600       END-IF                                                             
060700                                                                          
060800       IF INDATA-WRONG                                                    
060900         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
061000         CALL WMEDKONV USING MED-WMEDAREA                                 
061100         MOVE MED-MFSFEL           TO MOD-TEMFSFEL                        
061200         PERFORM MFS-DONT-TOUCH-FIELD-OUT                                 
061300         PERFORM MFS-DONT-TOUCH-FIELD-IN                                  
061400       END-IF                                                             
061500     END-IF                                                               
061600     .                                                                    
061700                                                                          
061800                                                                          
061900 H-UPDATE SECTION.                                                        
062000     MOVE 'H-UPDATE        ' TO CURRENT-SECTION                           
062100                                                                          
062200     MOVE NOO TO WS-UPD-VOLUME                                            
062300     PERFORM IMS-GHU-WDK611                                               
062400     IF SEGMENT-FOUND                                                     
062500       IF WS-KDUVKNTO NOT = SPACE                                         
062600          MOVE WS-KDUVKNTO      TO CLAG-KDUVKNTO                          
062700       END-IF                                                             
062800                                                                          
062900       IF MID-VKART-NTO NOT = ALL '+'                                     
063000         MOVE MID-VKART-NTO TO  CLAG-VKART-NTO                            
063100                                MOD-VKART-NTO-UT                          
063200         MOVE MSG-SIGNON-USERID TO CLAG-IDUSER-VUPD                       
063300         MOVE CURRENT-DATE      TO CLAG-TIUPPDAT-VUPD                     
063400       ELSE                                                               
063500         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-VKART-NTO-IN-ATTR             
063600       END-IF                                                             
063700                                                                          
063800       IF MID-VKART-BTO NOT = ALL '+'                                     
063900         MOVE MID-VKART-BTO TO  CLAG-VKART                                
064000                                MOD-VKART-BTO-UT                          
064100         MOVE MSG-SIGNON-USERID TO CLAG-IDUSER-VUPD                       
064200         MOVE CURRENT-DATE      TO CLAG-TIUPPDAT-VUPD                     
064300       ELSE                                                               
064400         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-VKART-BTO-IN-ATTR             
064500       END-IF                                                             
064600                                                                          
064700       IF MID-VLARTNTO NOT = ALL '+'                                      
064800         IF CLAG-VLARTNTO = ZERO                                          
064900           MOVE 002             TO SYNQ-KDCALL                            
065000         ELSE                                                             
065100           MOVE 003             TO SYNQ-KDCALL                            
065200         END-IF                                                           
065300         MOVE WS-VLARTNTO       TO CLAG-VLARTNTO                          
065400                                   MOD-VLARTNTO-UT                        
065500         MOVE MSG-SIGNON-USERID TO CLAG-IDUSER-VUPD                       
065600         MOVE CURRENT-DATE      TO CLAG-TIUPPDAT-VUPD                     
065700         MOVE YES               TO WS-UPD-VOLUME                          
065800       ELSE                                                               
065900         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-VLARTNTO-IN-ATTR              
066000       END-IF                                                             
066100                                                                          
066200       IF MID-KDVSOP NOT = ALL '+'                                        
066300         MOVE MID-KDVSOP TO  CLAG-KDVSOP                                  
066400                             MOD-KDVSOP-UT                                
066500         MOVE MSG-SIGNON-USERID TO CLAG-IDUSER-VUPD                       
066600         MOVE CURRENT-DATE      TO CLAG-TIUPPDAT-VUPD                     
066700       ELSE                                                               
066800         MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDVSOP-IN-ATTR                
066900       END-IF                                                             
067000                                                                          
067100       PERFORM IMS-REPL-WDK611                                            
067200       IF WS-UPD-VOLUME = YES                                             
067300         MOVE W-IDARTNR         TO SYNQ-IDARTNR                           
067400         CALL W488PRMA USING  SYNQ-W488PRMA                               
067500         SYNQ-ATAB-PCB SYNQ-WDK6-PCB SYNQ-WDD3-PCB                        
067600       END-IF                                                             
067700                                                                          
067800       PERFORM HA-CHECK-WDK7-UPDATE                                       
067900       IF 619B-MID-VKART    NOT = ALL '+'                                 
068000       OR 619B-MID-VLARTNTO NOT = ALL '+'                                 
068100          PERFORM HB-STARTA-DISPATCHEN                                    
068200       END-IF                                                             
068210                                                                          
068220       IF MID-VKART-BTO NOT = ALL '+'                                     
068230       OR MID-VLARTNTO NOT = ALL '+'                                      
068240        PERFORM HC-STARTA-W40289                                          
068250       END-IF                                                             
068300                                                                          
068400       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
068500       CALL WMEDKONV USING MED-WMEDAREA                                   
068600       MOVE MED-MFSINF      TO MOD-TEMFSINF                               
068700       PERFORM MFS-FORM-ATTR                                              
068800       PERFORM MFS-ERASE-FIELD-IN                                         
068900     END-IF                                                               
069000     .                                                                    
069100                                                                          
069200                                                                          
069300 HA-CHECK-WDK7-UPDATE SECTION.                                            
069400     MOVE 'HA-CHECK-WDK7-UP' TO CURRENT-SECTION                           
069500                                                                          
069600     IF CLAG-IDDC-REF(1:1) = '7' OR '4'                                   
069700        IF CLAG-IDDC-REF(1:1) = '7'                                       
069800           MOVE WC-LAND-CN     TO W-IDLAND                                
069900        ELSE                                                              
070000           IF CLAG-IDDC-REF(1:1) = '4'                                    
070100              MOVE WC-LAND-US     TO W-IDLAND                             
070200           END-IF                                                         
070300        END-IF                                                            
070400        PERFORM IMS-GHU-WDK712                                            
070500        IF SEGMENT-FOUND                                                  
070600           IF MID-VKART-BTO NOT = ALL '+'                                 
070700              MOVE WS-VKART-BTO-CURR    TO LART-VKART                     
070800              MOVE NOO                  TO LART-FLMSKUPD                  
070900           END-IF                                                         
071000           IF MID-VLARTNTO NOT = ALL '+'                                  
071100              MOVE WS-VLARTNTO-DISP-NUM TO LART-VLARTNTO                  
071200              MOVE NOO                  TO LART-FLMSKUPD                  
071300           END-IF                                                         
071400           PERFORM IMS-REPL-WDK712                                        
071500        END-IF                                                            
071600     END-IF                                                               
071700     .                                                                    
071800                                                                          
071900                                                                          
072000 HB-STARTA-DISPATCHEN SECTION.                                            
072100     MOVE 'EA-STARTA-DISPAT' TO CURRENT-SECTION                           
072200                                                                          
072300     MOVE SPACE                TO MSG-KOM-WMSGKOM                         
072400     COMPUTE MSG-KOM-KVLL = LENGTH OF MSG-KOM-WMSGKOM                     
072500     MOVE LOW-VALUE            TO MSG-KOM-KDZ1                            
072600     MOVE LOW-VALUE            TO MSG-KOM-KDZ2                            
072700     MOVE SPACE                TO MSG-KOM-KDTRANS                         
072800     MOVE 'W6I19B01'           TO MSG-KOM-IDCPYTXT                        
072900     MOVE 'INLEV   '           TO MSG-KOM-IDSNDNOD                        
073000     MOVE 'W6016900'           TO MSG-KOM-IDSNDJOB                        
073100     MOVE CURRENT-DATE         TO MSG-KOM-TIREGDAT                        
073200     MOVE CURRENT-TIME         TO MSG-KOM-TIKLOCK                         
073300     MOVE SPACE                TO MSG-KOM-IDMFSMED                        
073400                                                                          
073500     MOVE W-IDARTNR            TO 619B-MID-IDARTNR                        
073600     IF CLAG-IDDC-REF NOT = SPACE                                         
073700        MOVE CLAG-IDDC-REF     TO 619B-MID-IDDC                           
073800     ELSE                                                                 
073900        MOVE WC-CDC-SE         TO 619B-MID-IDDC                           
074000     END-IF                                                               
074100     COMPUTE P-TO-P-KVLL       =  LNG-P-TO-P-PREFIX + 87                  
074200     MOVE 'W6T19BX '           TO P-TO-P-KDTRANS                          
074300     MOVE '6169'               TO P-TO-P-IDTRANS                          
074400     MOVE MFS-KDMFSFOR         TO P-TO-P-KDMFSFOR                         
074500     MOVE 619B-MID-W6I19B01    TO P-TO-P-DATA                             
074600                                                                          
074700     CALL W006KOM USING MSG-PCB                                           
074800                        DISP-PCB                                          
074900                        KOM-KOMA-PCB                                      
075000                        MSG-KOM-WMSGKOM                                   
075100                        P-TO-P-SW                                         
075200     .                                                                    
075210 HC-STARTA-W40289 SECTION.                                                
075220                                                                          
075230     MOVE W-IDARTNR            TO P-TO-P2-MID-IDARTNR-IN                  
075271     MOVE WS-VKART-BTO-CURR    TO P-TO-P2-MID-VKART-IN                    
075272     IF MID-VLARTNTO NOT = ALL '+'                                        
075273      MOVE WS-VLARTNTO          TO P-TO-P2-MID-VLARTNTO-IN                
075274     ELSE                                                                 
075275      MOVE WS-VLARTNTO-CURR     TO P-TO-P2-MID-VLARTNTO-IN                
075276     END-IF                                                               
075280     MOVE ZERO                 TO P-TO-P2-MID-IDDISTR-IN                  
075290                                  P-TO-P2-MID-IDKUNDNR-IN                 
075291                                  P-TO-P2-MID-IDORDNR5-IN                 
075292                                  P-TO-P2-MID-IDORDER-IN                  
075293                                  P-TO-P2-MID-IDLOPNR-IN                  
075294                                  P-TO-P2-MID-IDDC-IN                     
075295                                  P-TO-P2-MID-ADLAGOMR-IN                 
075296                                  P-TO-P2-MID-ADGANG-IN                   
075297                                  P-TO-P2-MID-ADPLATS-IN                  
075298                                                                          
075299     COMPUTE P-TO-P2-KVLL   =  LENGTH OF P-TO-P2-MID-W4I28901 + 25        
075300     END-COMPUTE                                                          
075301                                                                          
075302     MOVE 'W4T289X '           TO P-TO-P2-KDTRANS                         
075303     MOVE '6169'               TO P-TO-P2-IDTRANS                         
075304     MOVE '2'                  TO P-TO-P2-KDMFSFOR                        
075305                                                                          
075306     PERFORM IMS-PURG-4289                                                
075307     .                                                                    
075308     EJECT                                                                
075310 MFS-ERASE-FIELD-OUT SECTION.                                             
075400                                                                          
075500*    --- ALLA UTDATA-FÄLT                                                 
075600     MOVE MFS-ERASE-FIELD TO MOD-STRECK                                   
075700                             MOD-REKSIFFR                                 
075800                             MOD-BEART                                    
075900                             MOD-VKART-NTO                                
076000                             MOD-VKART-BTO                                
076100                             MOD-VLARTNTO                                 
076200                             MOD-KVLENGTH-NTO                             
076300                             MOD-KVWIDTH-NTO                              
076400                             MOD-KVHEIGHT-NTO                             
076500                             MOD-KVLENGTH-BTO                             
076600                             MOD-KVWIDTH-BTO                              
076700                             MOD-KVHEIGHT-BTO                             
076800                             MOD-KDVSOP                                   
076900                             MOD-VKART-NTO-UT                             
077000                             MOD-VKART-BTO-UT                             
077100                             MOD-VLARTNTO-UT                              
077200                             MOD-KDVSOP-UT                                
077300                             MOD-KVANTAL-KDP                              
077400                             MOD-IDUSER                                   
077500                             MOD-TIUPPDAT                                 
077600                             MOD-TIUPPDAT-MASK                            
077700     .                                                                    
077800                                                                          
077900 MFS-ERASE-FIELD-IN SECTION.                                              
078000                                                                          
078100*    --- ALLA INDATA-FÄLT                                                 
078200     MOVE MFS-ERASE-FIELD TO MOD-VKART-NTO-IN                             
078300                             MOD-VKART-BTO-IN                             
078400                             MOD-VLARTNTO-IN                              
078500                             MOD-KDVSOP-IN                                
078600     .                                                                    
078700                                                                          
078800                                                                          
078900 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
079000                                                                          
079100*    --- ALLA UTDATA-FÄLT                                                 
079200     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-STRECK                            
079300                                    MOD-REKSIFFR                          
079400                                    MOD-BEART                             
079500                                    MOD-VKART-NTO                         
079600                                    MOD-VKART-BTO                         
079700                                    MOD-VLARTNTO                          
079800                                    MOD-KVLENGTH-NTO                      
079900                                    MOD-KVWIDTH-NTO                       
080000                                    MOD-KVHEIGHT-NTO                      
080100                                    MOD-KVLENGTH-BTO                      
080200                                    MOD-KVWIDTH-BTO                       
080300                                    MOD-KVHEIGHT-BTO                      
080400                                    MOD-KDVSOP                            
080500                                    MOD-VKART-NTO-UT                      
080600                                    MOD-VKART-BTO-UT                      
080700                                    MOD-VLARTNTO-UT                       
080800                                    MOD-KDVSOP-UT                         
080900                                    MOD-VKART-NTO-KDP                     
081000                                    MOD-KVANTAL-KDP                       
081100                                    MOD-IDUSER                            
081200                                    MOD-TIUPPDAT                          
081300                                    MOD-TIUPPDAT-MASK                     
081400     .                                                                    
081500                                                                          
081600 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
081700                                                                          
081800*    --- ALLA INDATA-FÄLT                                                 
081900     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-VKART-NTO-IN                      
082000                                    MOD-VKART-BTO-IN                      
082100                                    MOD-VLARTNTO-IN                       
082200                                    MOD-KDVSOP-IN                         
082300     .                                                                    
082400                                                                          
082500                                                                          
082600 MFS-FORM-ATTR SECTION.                                                   
082700                                                                          
082800*    --- ALL INDATA-FIELDS                                                
082900     MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-VKART-NTO-IN                     
083000                                     MOD-VKART-BTO-IN                     
083100                                     MOD-VLARTNTO-IN                      
083200                                     MOD-KDVSOP-IN                        
083300     .                                                                    
083400                                                                          
083500 MFS-READ-IN-AGAIN SECTION.                                               
083600                                                                          
083700*    --- ALL INDATA-FIELDS                                                
083800     MOVE MFS-ADD-READ-FIELD TO MOD-VKART-NTO-IN-ATTR                     
083900                                MOD-VKART-BTO-IN-ATTR                     
084000                                MOD-VLARTNTO-IN-ATTR                      
084100                                MOD-KDVSOP-IN-ATTR                        
084200     .                                                                    
084300                                                                          
084400                                                                          
084500* --- IMS SECTIONS ---                                                    
084600                                                                          
084700 IMS-GET-MSG SECTION.                                                     
084800                                                                          
084900     MOVE '  QC' TO GOOD-STATUSCODES                                      
085000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
085100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
085200     PERFORM IMS-STATUSCHECK                                              
085300     .                                                                    
085400                                                                          
085500 IMS-INSERT-MSG SECTION.                                                  
085600                                                                          
085700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
085800     MOVE SPACE TO GOOD-STATUSCODES                                       
085900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
086000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
086100     PERFORM IMS-STATUSCHECK                                              
086200     .                                                                    
086300                                                                          
086400                                                                          
086500 IMS-GU-WDK201 SECTION.                                                   
086600     MOVE 'IMS-GU-WDK201   ' TO CURRENT-IMS-SECTION                       
086700                                                                          
086800     MOVE SPACE               TO ALL-SSA                                  
086900     STRING 'WDK201  (IDARTNR  =' W-IDARTNR-X ')'                         
087000          DELIMITED BY SIZE INTO SSA1                                     
087100     MOVE '  GE'              TO GOOD-STATUSCODES                         
087200     CALL CBLTDLI USING GU WDK2-PCB DLI-IO-WDK201 SSA1                    
087300     MOVE WDK2-STATUS-CODE    TO STATUS-WS                                
087400     PERFORM IMS-STATUSCHECK                                              
087500     .                                                                    
087600                                                                          
087700                                                                          
087800 IMS-GNP-WDK211 SECTION.                                                  
087900     MOVE 'IMS-GNP-WDK211  ' TO CURRENT-IMS-SECTION                       
088000                                                                          
088100     MOVE SPACE               TO ALL-SSA                                  
088200     STRING 'WDK211  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
088300          DELIMITED BY SIZE INTO SSA1                                     
088400     MOVE '  GE'              TO GOOD-STATUSCODES                         
088500     CALL CBLTDLI USING GNP WDK2-PCB DLI-IO-WDK211 SSA1                   
088600     MOVE WDK2-STATUS-CODE    TO STATUS-WS                                
088700     PERFORM IMS-STATUSCHECK                                              
088800     .                                                                    
088900                                                                          
089000                                                                          
089100 IMS-GNP-WDK212 SECTION.                                                  
089200     MOVE 'IMS-GNP-WDK212  ' TO CURRENT-IMS-SECTION                       
089300                                                                          
089400     MOVE SPACE               TO ALL-SSA                                  
089500     STRING 'WDK212  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
089600          DELIMITED BY SIZE INTO SSA1                                     
089700     MOVE '  GE'              TO GOOD-STATUSCODES                         
089800     CALL CBLTDLI USING GNP WDK2-PCB DLI-IO-WDK212 SSA1                   
089900     MOVE WDK2-STATUS-CODE    TO STATUS-WS                                
090000     PERFORM IMS-STATUSCHECK                                              
090100     .                                                                    
090200                                                                          
090300                                                                          
090400 IMS-GU-WDD301 SECTION.                                                   
090500     MOVE 'IMS-GU-WDD301   ' TO CURRENT-IMS-SECTION                       
090600                                                                          
090700     MOVE SPACE               TO ALL-SSA                                  
090800     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
090900          DELIMITED BY SIZE INTO SSA1                                     
091000     MOVE '  GE'              TO GOOD-STATUSCODES                         
091100     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD301 SSA1                    
091200     MOVE WDD3-STATUS-CODE    TO STATUS-WS                                
091300     PERFORM IMS-STATUSCHECK                                              
091400     .                                                                    
091500                                                                          
091600                                                                          
091700 IMS-GNP-WDD311 SECTION.                                                  
091800     MOVE 'IMS-GNP-WDD311  ' TO CURRENT-IMS-SECTION                       
091900                                                                          
092000     MOVE SPACE               TO ALL-SSA                                  
092100     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
092200          DELIMITED BY SIZE INTO SSA1                                     
092300     MOVE '  GE'              TO GOOD-STATUSCODES                         
092400     CALL CBLTDLI USING GNP WDD3-PCB DLI-IO-WDD311 SSA1                   
092500     MOVE WDD3-STATUS-CODE    TO STATUS-WS                                
092600     PERFORM IMS-STATUSCHECK                                              
092700     .                                                                    
092800                                                                          
092900                                                                          
093000 IMS-GU-WDK601 SECTION.                                                   
093100     MOVE 'IMS-GU-WDK601   ' TO CURRENT-IMS-SECTION                       
093200                                                                          
093300     MOVE SPACE               TO ALL-SSA                                  
093400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
093500          DELIMITED BY SIZE INTO SSA1                                     
093600     MOVE '  GE'              TO GOOD-STATUSCODES                         
093700     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1                    
093800     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
093900     PERFORM IMS-STATUSCHECK                                              
094000     .                                                                    
094100                                                                          
094200 IMS-GHNP-WDK611 SECTION.                                                 
094300     MOVE 'IMS-GHNP-WDK611 ' TO CURRENT-IMS-SECTION                       
094400                                                                          
094500     MOVE SPACE               TO ALL-SSA                                  
094600     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
094700          DELIMITED BY SIZE INTO SSA1                                     
094800     MOVE '  GE'              TO GOOD-STATUSCODES                         
094900     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK611 SSA1                  
095000     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
095100     PERFORM IMS-STATUSCHECK                                              
095200     .                                                                    
095300                                                                          
095400 IMS-GHU-WDK611 SECTION.                                                  
095500     MOVE 'IMS-GHU-WDK611  ' TO CURRENT-IMS-SECTION                       
095600                                                                          
095700     MOVE SPACE               TO ALL-SSA                                  
095800     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
095900          DELIMITED BY SIZE INTO SSA1                                     
096000     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
096100          DELIMITED BY SIZE INTO SSA2                                     
096200     MOVE '  GE'              TO GOOD-STATUSCODES                         
096300     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
096400     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
096500     PERFORM IMS-STATUSCHECK                                              
096600     .                                                                    
096700                                                                          
096800 IMS-REPL-WDK611 SECTION.                                                 
096900     MOVE 'IMS-REPL-WDK611 ' TO CURRENT-IMS-SECTION                       
097000                                                                          
097100     MOVE SPACE               TO ALL-SSA                                  
097200     MOVE '  '                TO GOOD-STATUSCODES                         
097300     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
097400     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
097500     PERFORM IMS-STATUSCHECK                                              
097600     .                                                                    
097700                                                                          
097800                                                                          
097900 IMS-GHU-WDK712 SECTION.                                                  
098000     MOVE 'IMS-GHU-WDK712  ' TO CURRENT-IMS-SECTION                       
098100                                                                          
098200     MOVE SPACE               TO ALL-SSA                                  
098300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
098400          DELIMITED BY SIZE INTO SSA1                                     
098500     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
098600          DELIMITED BY SIZE INTO SSA2                                     
098700     MOVE '  GE'              TO GOOD-STATUSCODES                         
098800     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2              
098900     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
099000     PERFORM IMS-STATUSCHECK                                              
099100     .                                                                    
099200                                                                          
099300                                                                          
099400 IMS-REPL-WDK712 SECTION.                                                 
099500     MOVE 'IMS-REPL-WDK712 ' TO CURRENT-IMS-SECTION                       
099600                                                                          
099700     MOVE SPACE              TO ALL-SSA                                   
099800     MOVE '  '               TO GOOD-STATUSCODES                          
099900     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK712                       
100000     MOVE WDK7-STATUS-CODE   TO STATUS-WS                                 
100100     PERFORM IMS-STATUSCHECK                                              
100200     .                                                                    
100210 IMS-PURG-4289 SECTION.                                                   
100220     MOVE LOW-VALUE TO P-TO-P2-KDZ1 P-TO-P2-KDZ2                          
100230     MOVE SPACE TO GOOD-STATUSCODES                                       
100240     CALL  CBLTDLI  USING PURG 4289-PCB P-TO-P-SW2                        
100250     MOVE 4289-STATUS-CODE TO STATUS-WS                                   
100260     PERFORM IMS-STATUSCHECK                                              
100270     .                                                                    
100500 IMS-STATUSCHECK SECTION.                                                 
100600                                                                          
100700     SET STATUS-IX TO 1                                                   
100800     SEARCH GOOD-STATUS                                                   
100900       AT END                                                             
101000         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
101100         DELIMITED BY SIZE INTO ERROR-TEXT                                
101200         CALL FELLOG                                                      
101300       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
101400         CONTINUE                                                         
101500     END-SEARCH                                                           
101600     .                                                                    
