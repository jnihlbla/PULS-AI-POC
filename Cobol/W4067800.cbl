000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4067800.                                                
000300 AUTHOR.         KARANDE DIGAMBAR.                                        
000400 DATE-WRITTEN.   02/09/12.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        THIS PROGRAM IS STARTED BY 4677 SCREEN TO UPDATE THE             
000900*        DETAILS IN WDE211 SEGMENT.                                       
001000*                                                                         
001100*        THE PROGRAM UPDATES   WDE2                                       
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSACTION: W4T678                                              
001500*        MID:         W4I67801                                            
001600*                                                                         
001700*    OUTDATA.                                                             
001800*        MOD:         W4O67801                                            
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200                                                                          
002300 DATA DIVISION.                                                           
002400     EJECT                                                                
002500 WORKING-STORAGE SECTION.                                                 
002600 77  IDPGM                       PIC X(08)   VALUE 'W4067800'.            
002700                                                                          
002800 77  IDLAND-INDX                 PIC S9(4)   VALUE +0   COMP SYNC.        
002900 77  MAX-IDLAND-INDX             PIC S9(4)   VALUE +23  COMP SYNC.        
003000 77  MSG-KVLL-TILL-WHELP         PIC S9(4)  VALUE +85   COMP SYNC.        
003100*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003200 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
003300                                                                          
003400 77  YES                         PIC X       VALUE 'J'.                   
003500 77  NOO                         PIC X       VALUE 'N'.                   
003600                                                                          
003700 77  WS-CDC-11                   PIC X(2)    VALUE '11'.                  
003800                                                                          
003900 77  W-IDDC                      PIC X(02)   VALUE SPACE.                 
004000 77  W-IDDISTR                   PIC 9(04)   VALUE ZERO COMP-3.           
004100 77  W-FLFARLIG                  PIC X(01)   VALUE SPACE.                 
004200 77  W-IDLANDX2                  PIC X(2)    VALUE SPACE.                 
004300 77  W-9KOMPL                    PIC  9(7)   VALUE 9999999.               
004400 77  W-DCS-IDPARTNR-EXP          PIC  X(9)   VALUE SPACE.                 
004500                                                                          
004600 01  WS-TID-X.                                                            
004700     03 WS-TISKPTID                PIC 9(6)       VALUE ZERO.             
004800     03 WS-TISKPTID-GRP            REDEFINES WS-TISKPTID.                 
004900       05 WS-TISKPTID-HHMM         PIC 9(4).                              
005000       05 WS-TISKPTID-SS           PIC 9(2).                              
005100                                                                          
005200 01  W-IDDCTEXT-MSGI.                                                     
005300     03  FILLER                  PIC X(5)    VALUE 'WIDDC'.               
005400     03  W-IDDC-MSGI             PIC X(2).                                
005500                                                                          
005600 77  FLSAMFAK-SW                 PIC X       VALUE 'N'.                   
005700     88  FLSAMFAK                            VALUE 'J'.                   
005800                                                                          
005900 77  WDE211-ISRT-SW              PIC X       VALUE 'N'.                   
006000     88  WDE211-ISRT                         VALUE 'J'.                   
006100                                                                          
006200*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
006300                                                                          
006400 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006500     88  INDATA-OK                           VALUE 'J'.                   
006600     88  INDATA-WRONG                        VALUE 'N'.                   
006700                                                                          
006800 77  KEYS-SW                     PIC X       VALUE 'J'.                   
006900     88  KEYS-OK                             VALUE 'J'.                   
007000     88  KEYS-WRONG                          VALUE 'N'.                   
007100                                                                          
007200 77  UPDATE-SAVE-SW              PIC X       VALUE 'N'.                   
007300     88  UPDATE-SAVE                         VALUE 'J'.                   
007400                                                                          
007500 77  INPUT-GIVEN-SW              PIC X       VALUE 'N'.                   
007600     88  INPUT-GIVEN                         VALUE 'J'.                   
007700                                                                          
007800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007900     88  OWN-MID                             VALUE '4678'.                
008000     88  GOOD-MID                            VALUE '4677'                 
008100                                                   '4678'.                
008200     88  4677-MID                            VALUE '4677'.                
008300     88  HELP-MID                            VALUE '0551'.                
008400     EJECT                                                                
008500                                                                          
008600                                                                          
008700 01  TEST-IDDISTR               PIC S9(05)   VALUE ZERO  COMP-3.          
008800*01  FILLER  -COPY  WWDIST79  -RED  TEST-IDDISTR                          
008900*01  FILLER  -COPY  WWDIST35  -RED  TEST-IDDISTR                          
009000*01  FILLER  -COPY  WWDIST07  -RED  TEST-IDDISTR                          
009100                                                                          
009200*01  -COPY  WWDCKONS                                                      
009300                                                                          
009400*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
009500 01  GENERAL-SUBPROGRAMS.                                                 
009600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
009700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009800     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
009900     03  W476SHNO                PIC X(8)    VALUE 'W476SHNO'.            
010000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010300     EJECT                                                                
010400*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
010500*01 -COPY WMEDAREA                                                        
010600     SKIP3                                                                
010700 01  FILLER.                                                              
010800   03  FELMEDD-AREA.                                                      
010900     05  FELMEDD-ENGLISH.                                                 
011000       10  FILLER                PIC X(40)                                
011100           VALUE '622 4678 WRONG PICTURE SELECTED         '.              
011200                                                                          
011300 01  MESSAGE-CODES.                                                       
011400     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
011500     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
011600     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
011700     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
011800     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
011900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
012000     03  ERR-START-FROM-FIRST    PIC X(3)    VALUE '754'.                 
012100     EJECT                                                                
012200*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
012300*                                                                         
012400 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
012500     SKIP3                                                                
012600*01 -COPY WMSGINIT                                                        
012700                                                                          
012800 01  FILLER                      PIC X(16)   VALUE 'DC-WMSGINIT'.         
012900     SKIP3                                                                
013000*01 -COPY WMSGINIT        -PRE   DC-                                      
013100                                                                          
013200*    --- PARAMETERS FOR SUB PROGRAM WDECEDIT                              
013300*                                                                         
013400 01  FILLER                      PIC X(16)   VALUE 'WDECEDIT'.            
013500*01 -COPY WDECAREA                                                        
013600     EJECT                                                                
013700*    --- AREA  FOR W476SHNO ---                                           
013800 01  FILLER                      PIC X(16)   VALUE 'W476SHNO'.            
013900*01 -COPY W476SHNO                                                        
014000                                                                          
014100*    --- AREA  FOR WDATKONV ---                                           
014200 01  FILLER                      PIC X(16)   VALUE 'WDATAREA'.            
014300*01   -COPY WDATAREA                                                      
014400                                                                          
014500*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
014600*                                                                         
014700 01  SAVE-AREA.                                                           
014800     03  SAVE-IDTRANS            PIC X(4)    VALUE '4678'.                
014900     03  SAVE-4677-4678.                                                  
015000         05  SAVE-IDDISTR-RETURN     PIC S9(5).                           
015100         05  SAVE-IDKUNDNR-RETURN    PIC S9(7).                           
015200         05  SAVE-KDFAKTYP-RETURN    PIC X(01).                           
015300         05  SAVE-IDKUNDRF-RETURN    PIC X(10).                           
015400         05  SAVE-IDPRODNR-RETURN    PIC S9(07).                          
015500         05  SAVE-IDKOLLI-RETURN     PIC S9(05).                          
015600         05  SAVE-IDDISTR-4678       PIC S9(5).                           
015700         05  SAVE-IDKUNDNR-4678      PIC S9(7).                           
015800         05  SAVE-FLSAMFAK           PIC X(01)   VALUE SPACE.             
015900         05  SAVE-IDSHIPM            PIC  9(7).                           
016000         05  SAVE-FLFARLIG           PIC X(01).                           
016100         05  SAVE-FLAVSLUTA          PIC X(01).                           
016200         05  SAVE-REL-PROFORMA       PIC X(01)   VALUE 'N'.               
016300             88  REL-PROFORMA                    VALUE 'J'.               
016400                                                                          
016500     03  SAVE-PRFRAKT            PIC S9(7)V9(2)      COMP-3.              
016600     03  SAVE-PRFOERS            PIC S9(7)V9(2)      COMP-3.              
016700     03  SAVE-PRKURS-MAN         PIC S9(6)V9(5)      COMP-3.              
016800     03  SAVE-PRLEGKST           PIC S9(7)V9(2)      COMP-3.              
016900     03  SAVE-RELEGKST           PIC S9(2)V9(1)      COMP-3.              
017000     03  SAVE-PREMBHNT           PIC S9(7)V9(2)      COMP-3.              
017100     03  SAVE-REEMBHNT           PIC S9(2)V9(1)      COMP-3.              
017200     03  SAVE-PRAVDRAG           PIC S9(7)V9(2)      COMP-3.              
017300     03  SAVE-REAVDRAG           PIC S9(2)V9(1)      COMP-3.              
017400     03  SAVE-REFOERS            PIC S9(2)V9(3)      COMP-3.              
017500     03  SAVE-REOVKOFF           PIC S9(2)V9(1)      COMP-3.              
017600                                                                          
017700*                                                                         
017800     EJECT                                                                
017900*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
018000*                                                                         
018100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
018200     SKIP3                                                                
018300*01  MID -COPY W4I67801                                                   
018400     EJECT                                                                
018500 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
018600     SKIP3                                                                
018700*01  -COPY WMSGAREA                                                       
018800     EJECT                                                                
018900*   TO RETURN TO MAIN MENU                                                
019000*    03  FILLER  -COPY W0O50401  -PRE MOD0504-  -RED MSG-AREA.            
019100     EJECT                                                                
019200     03  MOD REDEFINES MSG-AREA.                                          
019300*      05  -COPY W4O67801                                                 
019400     EJECT                                                                
019500 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
019600     SKIP3                                                                
019700*01  -COPY WMFSAREA                                                       
019800     EJECT                                                                
019900 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-AREA'.         
020000 01  P-TO-P-SW.                                                           
020100                                                                          
020200     03  P-TO-P-KVLL                PIC S9(4)           COMP SYNC.        
020300     03  P-TO-P-KDZ1                PIC X(1)  VALUE LOW-VALUE.            
020400     03  P-TO-P-KDZ2                PIC X(1)  VALUE LOW-VALUE.            
020500     03  P-TO-P-KDTRANS             PIC X(8).                             
020600     03  P-TO-P-IDTRANS             PIC X(4).                             
020700     03  P-TO-P-KDMFSFOR            PIC X(1).                             
020800     03  P-TO-P-DATA.                                                     
020900*        05 -COPY W4I67701   -PRE PTOP-                                   
021000*                                                                         
021100*    --- WORK-AREAS FOR IMS-SECTIONS                                      
021200*                                                                         
021300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
021400     SKIP3                                                                
021500 01  KEYS-TO-DLI.                                                         
021600     03  W-IDSHIPM-X.                                                     
021700         05  W-IDSHIPM           PIC  9(07)  VALUE ZERO.                  
021800                                                                          
021900     03  W-WDE211KY-X.                                                    
022000         05  W-WDE211-IDDISTR    PIC S9(05)  VALUE ZERO COMP-3.           
022100         05  W-WDE211-IDKUNDNR   PIC S9(07)  VALUE ZERO COMP-3.           
022200                                                                          
022300     03  W-WDE211KY-MIN.                                                  
022400         05  W-WDE211-IDDISTR-MIN                                         
022500                                 PIC S9(05)  VALUE ZERO COMP-3.           
022600         05  FILLER              PIC  X(4)   VALUE LOW-VALUES.            
022700                                                                          
022800     03  W-WDE211KY-MAX.                                                  
022900         05  W-WDE211-IDDISTR-MAX                                         
023000                                 PIC S9(05)  VALUE ZERO COMP-3.           
023100         05  FILLER              PIC  X(4)   VALUE HIGH-VALUES.           
023200                                                                          
023300     03  W-KDSEGKEY-X.                                                    
023400         05  W-KDSEGKEY          PIC X(01)   VALUE '1'.                   
023500                                                                          
023600                                                                          
023700     03  W-4479-X.                                                        
023800         05  W-IDHTR             PIC X(4)    VALUE '4479'.                
023900         05  W-IDDC-4479         PIC X(2)    VALUE SPACE.                 
024000         05  W-IDTRPTNR          PIC S9(3)   VALUE ZERO   COMP-3.         
025000         05  W-IDLBBET           PIC X(12)   VALUE SPACE.                 
025100         05  FILLER              PIC X(10)   VALUE LOW-VALUE.             
025200                                                                          
025300     03  W-4482-MIN.                                                      
025400         05  W-4482-IDDISTR-MIN  PIC S9(05)  COMP-3.                      
025500         05  W-4482-IDKUNDNR-MIN PIC S9(07)  COMP-3.                      
025600         05  FILLER              PIC  X(18)  VALUE LOW-VALUE.             
025700                                                                          
025800     03  W-4482-MAX.                                                      
025900         05  W-4482-IDDISTR-MAX  PIC S9(05)  COMP-3.                      
026000         05  W-4482-IDKUNDNR-MAX PIC S9(07)  COMP-3.                      
026100         05  FILLER              PIC  X(18)  VALUE HIGH-VALUE.            
026200                                                                          
026300     03  W-4495-X.                                                        
026400         05  W-IDHTR             PIC X(4)    VALUE '4495'.                
026500         05  W-IDDC-4495         PIC X(2)    VALUE SPACE.                 
026600         05  W-IDTRPTNR-4495     PIC S9(3)   VALUE ZERO   COMP-3.         
026700         05  W-IDLBBET-4495      PIC X(12)   VALUE SPACE.                 
026800         05  FILLER              PIC X(10)   VALUE LOW-VALUE.             
026900                                                                          
027000     03  W-IDDC-B6-X.                                                     
027100         05 W-IDDC-B6                  PIC X(2).                          
027200                                                                          
027300     SKIP2                                                                
027400*    --- STATUS-KOD FRÅN IMS                                              
027500 01  STATUS-WS                   PIC XX.                                  
027600     88  SEGMENT-FOUND                       VALUE '  '.                  
027700     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
027800     88  SEGMENT-MISSING                     VALUE 'GE'.                  
027900     SKIP2                                                                
028000 01  GOOD-STATUSCODES.                                                    
028100     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
028200     SKIP3                                                                
028300 01  SSA1                        PIC X(128).                              
028400 01  SSA2                        PIC X(128).                              
028500 01  SSA3                        PIC X(128).                              
028600     EJECT                                                                
028700*    --- IMS FUNCTION CODES                                               
028800*01  -COPY W0003                                                          
028900     EJECT                                                                
029000*    ---  DLI INPUT-OUTPUT AREA                                           
029100                                                                          
029200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE201'.                      
029300 01  DLI-IO-WDE201.                                                       
029400*    03  -COPY WDE201                                                     
029500                                                                          
029600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE211'.                      
029700 01  DLI-IO-WDE211.                                                       
029800*    03  -COPY WDE211                                                     
029900                                                                          
030000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4482'.                    
031000 01  DLI-IO-WDGX4482.                                                     
031100*    03  -COPY WDGX4482                                                   
031200                                                                          
031300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4496'.                    
031400 01  DLI-IO-WDGX4496.                                                     
031500*    03  -COPY WDGX4496                                                   
031600                                                                          
031700 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
031800 01   DLI-IO-AREA-B601.                                                   
031900*     03  -COPY WDB601                                                    
032000                                                                          
032100     EJECT                                                                
032200 LINKAGE SECTION.                                                         
032300*01  -COPY W0009   -PRE MSG-                                              
032400*01  -COPY W0009   -PRE ALT-                                              
032500*01  -COPY W0008   -PRE WDP7-                                             
032600     05  FILLER                  PIC X.                                   
032700                                                                          
032800*01  -COPY W0008   -PRE 4479-                                             
032900     05  FILLER                  PIC X.                                   
033000*01  -COPY W0008   -PRE 4495-                                             
033100     05  FILLER                  PIC X.                                   
033200                                                                          
033300*01  -COPY W0008   -PRE WDE2-                                             
033400     05  FILLER                  PIC X.                                   
033500*01  -COPY W0008   -PRE WDB6-                                             
033600     05  FILLER                  PIC X.                                   
033700 01  SHNO-4517-PCB               PIC X.                                   
033800     EJECT                                                                
033900 PROCEDURE DIVISION  USING MSG-PCB  ALT-PCB   WDP7-PCB                    
034000                           4479-PCB 4495-PCB                              
035000                           WDE2-PCB WDB6-PCB                              
035100                           SHNO-4517-PCB.                                 
035200                                                                          
035300 MAIN SECTION.                                                            
035400     ENTRY 'DLITCBL' USING MSG-PCB  ALT-PCB   WDP7-PCB                    
035500                           4479-PCB 4495-PCB                              
035600                           WDE2-PCB WDB6-PCB                              
035700                           SHNO-4517-PCB.                                 
035800                                                                          
035900     PERFORM IMS-GET-MSG                                                  
036000     IF SEGMENT-FOUND                                                     
036100       PERFORM A-INIT                                                     
036200       IF GOOD-MID                                                        
036300         PERFORM B-CHECK-KEYS                                             
036400       END-IF                                                             
036500       IF KEYS-OK                                                         
036600         IF MFS-UPDATE                                                    
036700           PERFORM G-CHECK-INPUT                                          
036800           IF INDATA-OK                                                   
036900             PERFORM H-UPDATE                                             
037000           END-IF                                                         
037100         ELSE                                                             
037200           IF MFS-RETURN                                                  
037300             PERFORM I-PF3-RETURN                                         
037400           ELSE                                                           
037500             IF MFS-FIRST                                                 
037600               PERFORM C-FIRST-PAGE                                       
037700             ELSE                                                         
037800               PERFORM E-SAME-PAGE                                        
037900             END-IF                                                       
038000           END-IF                                                         
038100         END-IF                                                           
038200         IF DIST79-DEALER-PRICE OR                                        
038400            DIST79-ECOM-PRICE                                             
038500           PERFORM J-CLOSE-FIELDS                                         
038600         END-IF                                                           
038700         IF INDATA-OK AND NOT MFS-RETURN                                  
038800           PERFORM F-READ-SHOW-INFO                                       
038900         END-IF                                                           
039000       END-IF                                                             
039100*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
039200*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
039300       IF UPDATE-SAVE                                                     
039400         MOVE '002'       TO MSGI-KDCALL                                  
039500         MOVE '4678'      TO SAVE-IDTRANS                                 
039600         MOVE SAVE-AREA   TO MSGI-SPAR-AREA                               
039700         CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                       
039800       END-IF                                                             
039900       IF INDATA-OK AND MFS-RETURN                                        
040000         PERFORM IMS-ISRT-MSG-ALT                                         
040100       ELSE                                                               
040200         IF GOOD-MID                                                      
040300           COMPUTE MSG-KVLL = LENGTH OF MOD-W4O67801 + 4                  
040400           PERFORM IMS-INSERT-MSG                                         
040500         END-IF                                                           
040600       END-IF                                                             
040700     END-IF                                                               
040800                                                                          
040900     MOVE ZERO TO RETURN-CODE                                             
041000     GOBACK                                                               
041100     .                                                                    
041200     EJECT                                                                
041300 A-INIT SECTION.                                                          
041400                                                                          
041500     IF MSG-DOUBLE-TRANSACTIONS                                           
041600       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W4I67801                 
041700       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
041800       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
041900     ELSE                                                                 
042000       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W4I67801                  
042100       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
042200       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
042300     END-IF                                                               
042400                                                                          
042500     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
042600     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
042700     MOVE MFS-IDTRANS TO W-IDTRANS                                        
042800                                                                          
042900     MOVE LOW-VALUE TO MSG-AREA                                           
043000     MOVE 'W4O678N1' TO MFS-IDMOD                                         
043100     MOVE '4678' TO MOD-IDTRANS                                           
043200     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
043300                                                                          
043400     IF OWN-MID                                                           
043500       CONTINUE                                                           
043600     ELSE                                                                 
043700       MOVE SPACE TO MFS-KDTRTYP                                          
043800       MOVE '7' TO MFS-IDPFK                                              
043900     END-IF                                                               
044000                                                                          
044100     IF NOT GOOD-MID                                                      
044200       MOVE NOO  TO KEYS-SW                                               
044300                    INDATA-SW                                             
044400       PERFORM S10-WRONG-PICTURE-MESSAGE                                  
044500     END-IF                                                               
044600     .                                                                    
044700     EJECT                                                                
044800 B-CHECK-KEYS SECTION.                                                    
044900                                                                          
045000     MOVE ALL '+'           TO MSGI-WMSGINIT                              
045100     MOVE '001'             TO MSGI-KDCALL                                
045200     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
045300     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
045400     MOVE '4678'            TO MSGI-IDTRANS                               
045500     IF GOOD-MID                                                          
045600       MOVE MID-IDTRPTNR-IN TO MSGI-IDTRPTNR                              
045700       MOVE MID-IDLBBET-IN  TO MSGI-IDLBBET                               
045800       MOVE MID-IDDC-IN     TO MSGI-IDDC-KEY                              
045900     END-IF                                                               
046000                                                                          
046100     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
046200                                                                          
046300     IF 4677-MID                                                          
046400       MOVE MID-IDTRPTNR-IN TO MSGI-IDTRPTNR                              
046500       MOVE MID-IDLBBET-IN  TO MSGI-IDLBBET                               
046600       MOVE MID-IDDC-IN     TO MSGI-IDDC-KEY                              
046700     END-IF                                                               
046800                                                                          
046900     IF GOOD-MID                                                          
047000       IF MID-IDTRPTNR-IN NOT  = ALL '+'                                  
047100         MOVE '7'           TO MFS-IDPFK                                  
047200         MOVE SPACE         TO MFS-KDTRTYP                                
047300       END-IF                                                             
047400       IF MID-IDLBBET-IN NOT   = ALL '+'                                  
047500         MOVE '7'           TO MFS-IDPFK                                  
047600         MOVE SPACE         TO MFS-KDTRTYP                                
047700       END-IF                                                             
047800       IF MID-IDDC-IN NOT      = ALL '+'                                  
047900         MOVE '7'           TO MFS-IDPFK                                  
048000         MOVE SPACE         TO MFS-KDTRTYP                                
048100       END-IF                                                             
048200       IF MID-FLFARLIG-IN NOT  = ALL '+'                                  
048300         MOVE '7'           TO MFS-IDPFK                                  
048400         MOVE SPACE         TO MFS-KDTRTYP                                
048500       END-IF                                                             
048600     END-IF                                                               
048700                                                                          
048800     MOVE MSGI-SPAR-AREA    TO SAVE-AREA                                  
048900                                                                          
049000     IF MFS-FIRST                                                         
049100       MOVE SPACE           TO SAVE-FLAVSLUTA                             
049200                               SAVE-REL-PROFORMA                          
049300       MOVE YES             TO UPDATE-SAVE-SW                             
049400     END-IF                                                               
049500                                                                          
049600*    - LANGUAGE TO BE USED BY MEDKONV                                     
049700     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
049800                                                                          
049900     MOVE YES TO KEYS-SW                                                  
050000                                                                          
050100     MOVE MFS-ERASE-FIELD   TO MOD-IDTRPTNR-IN                            
050200                               MOD-IDLBBET-IN                             
050300                               MOD-FLFARLIG-IN                            
050400                               MOD-IDDC-IN                                
050500                                                                          
050600     IF MSGI-IDTRPTNR NUMERIC                                             
050700        IF MSGI-IDTRPTNR > ZERO                                           
050800           MOVE MSGI-IDTRPTNR   TO  W-IDTRPTNR                            
050900                                    W-IDTRPTNR-4495                       
051000        ELSE                                                              
051100          MOVE NOO     TO KEYS-SW                                         
051200        END-IF                                                            
051300     ELSE                                                                 
051400        MOVE NOO       TO KEYS-SW                                         
051500     END-IF                                                               
051600                                                                          
051700     IF MSGI-IDLBBET = ALL '+'                                            
051800        MOVE NOO           TO KEYS-SW                                     
051900     ELSE                                                                 
052000       IF MSGI-IDLBBET > SPACE                                            
052100          MOVE MSGI-IDLBBET  TO W-IDLBBET                                 
052200                                W-IDLBBET-4495                            
052300       ELSE                                                               
052400          MOVE NOO           TO KEYS-SW                                   
052500       END-IF                                                             
052600     END-IF                                                               
052700                                                                          
052800     IF MID-FLFARLIG-IN NOT = ALL '+'                                     
052900       MOVE MID-FLFARLIG-IN    TO W-FLFARLIG                              
053000     ELSE                                                                 
053100       IF SAVE-FLFARLIG > SPACE                                           
053200         MOVE SAVE-FLFARLIG    TO W-FLFARLIG                              
053300       END-IF                                                             
053400     END-IF                                                               
053500                                                                          
053600     IF W-FLFARLIG NOT = SAVE-FLFARLIG                                    
053700       MOVE W-FLFARLIG     TO SAVE-FLFARLIG                               
053800       MOVE YES            TO UPDATE-SAVE-SW                              
053900     END-IF                                                               
054000                                                                          
054100     MOVE SAVE-IDDISTR-4678    TO TEST-IDDISTR                            
054200     IF DIST35-NONVCC-NONVCC-REFILL                                       
054300       MOVE WC-CDC-SE          TO W-IDDC-B6                               
054400       PERFORM IMS-GU-WDB601                                              
054500       MOVE DCS-IDPARTNR       TO W-DCS-IDPARTNR-EXP                      
054600     END-IF                                                               
054700                                                                          
054800     MOVE MSGI-IDDC-KEY    TO W-IDDC-B6                                   
054900     PERFORM IMS-GU-WDB601                                                
055000     IF DCS-KDDC NOT = SPACE                                              
055100       MOVE MSGI-IDDC-KEY  TO W-IDDC                                      
055200                              W-IDDC-4479                                 
055300                              W-IDDC-4495                                 
055400       IF DCS-DDC                                                         
055500         MOVE WS-CDC-11        TO W-IDDC-MSGI                             
055600       ELSE                                                               
055700         MOVE MSGI-IDDC-KEY    TO W-IDDC-MSGI                             
055800       END-IF                                                             
055900                                                                          
056000       MOVE ALL '+'            TO DC-MSGI-WMSGINIT                        
056100       MOVE '001'              TO DC-MSGI-KDCALL                          
056200       MOVE W-IDDCTEXT-MSGI    TO DC-MSGI-IDUSER                          
056300       MOVE '4678'             TO DC-MSGI-IDTRANS                         
056400       MOVE MSG-LTERM-NAME     TO DC-MSGI-IDLTERM-USER                    
056500       CALL W005INIT        USING DC-MSGI-WMSGINIT WDP7-PCB               
056600                                                                          
056700     ELSE                                                                 
056800       MOVE NOO                TO KEYS-SW                                 
056900     END-IF                                                               
057000                                                                          
057100     MOVE SAVE-FLSAMFAK        TO FLSAMFAK-SW                             
057200     MOVE SAVE-IDSHIPM         TO W-IDSHIPM                               
057300                                                                          
057400     IF GOOD-MID OR KEYS-OK                                               
057500       MOVE MSGI-IDTRPTNR       TO MOD-IDTRPTNR-UT                        
057600       MOVE MSGI-IDLBBET        TO MOD-IDLBBET-UT                         
057700       MOVE MSGI-IDDC-KEY       TO MOD-IDDC-UT                            
057800       MOVE SAVE-FLFARLIG       TO MOD-FLFARLIG-UT                        
057900       MOVE SAVE-IDDISTR-4678   TO MOD-IDDISTR                            
058000                                   W-IDDISTR                              
058100                                   TEST-IDDISTR                           
058200                                   W-4482-IDDISTR-MIN                     
058300                                   W-4482-IDDISTR-MAX                     
058400                                   W-WDE211-IDDISTR                       
058500                                   W-WDE211-IDDISTR-MIN                   
058600                                   W-WDE211-IDDISTR-MAX                   
058700       MOVE SAVE-IDKUNDNR-4678  TO MOD-IDKUNDNR                           
058800                                   W-WDE211-IDKUNDNR                      
058900     ELSE                                                                 
059000       PERFORM MFS-ERASE-FIELD-OUT                                        
059100     END-IF                                                               
059200                                                                          
059300     IF KEYS-WRONG                                                        
059400       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
059500       CALL WMEDKONV USING MED-WMEDAREA                                   
059600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
059700       PERFORM MFS-ERASE-FIELD-IN                                         
059800       PERFORM MFS-ERASE-FIELD-OUT                                        
059900     END-IF                                                               
060000     .                                                                    
060100     EJECT                                                                
060200 C-FIRST-PAGE SECTION.                                                    
060300                                                                          
060400     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
060500     CALL WMEDKONV USING MED-WMEDAREA                                     
060600     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
060700                                                                          
060800     PERFORM MFS-ERASE-FIELD-IN                                           
060900     .                                                                    
061000     EJECT                                                                
061100 E-SAME-PAGE SECTION.                                                     
061200                                                                          
061300     IF OWN-MID OR HELP-MID                                               
061400       IF MID-W4I67801 = ALL '+'                                          
061500         PERFORM MFS-ERASE-FIELD-IN                                       
061600       ELSE                                                               
061700         MOVE NOO            TO INDATA-SW                                 
061800         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
061900         CALL WMEDKONV USING MED-WMEDAREA                                 
062000         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
062100         PERFORM MFS-DONT-TOUCH-FIELD-OUT                                 
062200         PERFORM EA-MID-INDATA-TO-MOD                                     
062300       END-IF                                                             
062400     ELSE                                                                 
062500       PERFORM MFS-ERASE-FIELD-IN                                         
062600     END-IF                                                               
062700     .                                                                    
062800     EJECT                                                                
062900 EA-MID-INDATA-TO-MOD SECTION.                                            
063000                                                                          
063100     IF MID-FLSEPINV NOT = ALL '+'                                        
063200       MOVE MFS-ADD-READ-FIELD       TO MOD-FLSEPINV-ATTR                 
063300     END-IF                                                               
063400                                                                          
063500     IF MID-PRFRAKT NOT = ALL '+'                                         
063600       MOVE MFS-ADD-READ-FIELD       TO MOD-PRFRAKT-ATTR                  
063700     END-IF                                                               
063800                                                                          
063900     IF MID-PRFOERS NOT = ALL '+'                                         
064000       MOVE MFS-ADD-READ-FIELD       TO MOD-PRFOERS-ATTR                  
064100     END-IF                                                               
064200                                                                          
064300     IF MID-REFOERS NOT = ALL '+'                                         
064400       MOVE MFS-ADD-READ-FIELD       TO MOD-REFOERS-ATTR                  
064500     END-IF                                                               
064600                                                                          
064700     IF MID-REOVKOFF NOT = ALL '+'                                        
064800       MOVE MFS-ADD-READ-FIELD       TO MOD-REOVKOFF-ATTR                 
064900     END-IF                                                               
065000                                                                          
065100     IF MID-PRLEGKST NOT = ALL '+'                                        
065200       MOVE MFS-ADD-READ-FIELD       TO MOD-PRLEGKST-ATTR                 
065300     END-IF                                                               
065400                                                                          
065500     IF MID-RELEGKST NOT = ALL '+'                                        
065600       MOVE MFS-ADD-READ-FIELD       TO MOD-RELEGKST-ATTR                 
065700     END-IF                                                               
065800                                                                          
065900     IF MID-PREMBHNT NOT = ALL '+'                                        
066000       MOVE MFS-ADD-READ-FIELD       TO MOD-PREMBHNT-ATTR                 
066100     END-IF                                                               
066200                                                                          
066300     IF MID-REEMBHNT NOT = ALL '+'                                        
066400       MOVE MFS-ADD-READ-FIELD       TO MOD-REEMBHNT-ATTR                 
066500     END-IF                                                               
066600                                                                          
066700     IF MID-PRAVDRAG NOT = ALL '+'                                        
066800       MOVE MFS-ADD-READ-FIELD       TO MOD-PRAVDRAG-ATTR                 
066900     END-IF                                                               
067000                                                                          
067100     IF MID-REAVDRAG NOT = ALL '+'                                        
067200       MOVE MFS-ADD-READ-FIELD       TO MOD-REAVDRAG-ATTR                 
067300     END-IF                                                               
067400                                                                          
067500     .                                                                    
067600     EJECT                                                                
067700 F-READ-SHOW-INFO SECTION.                                                
067800                                                                          
067900     PERFORM IMS-GU-WDGX4496                                              
068000     IF SEGMENT-FOUND                                                     
068100        MOVE 4496-VKORDBTO-LASTB TO MOD-VKORDBTO                          
068200        MOVE 4496-VLORDBTO-LASTB TO MOD-VLORDBTO                          
068300        MOVE 4496-SUORDV-LASTB   TO MOD-SUORDV                            
068400     END-IF                                                               
068500                                                                          
068600     MOVE W-IDDISTR         TO TEST-IDDISTR                               
068700     IF DIST79-DEALER-PRICE OR                                            
068900        DIST79-ECOM-PRICE                                                 
069000        MOVE SPACE                  TO MOD-CURRENCY-MC                    
069100                                       MOD-KDVALISO-MC                    
069200     ELSE                                                                 
069300       IF DIST35-NONVCC-NONVCC-REFILL                                     
069400       OR DCS-USA                                                         
069500       OR DCS-LAND-NON-VCC-OWNED                                          
069600         MOVE 'Currency   '         TO MOD-CURRENCY-MC                    
069700         MOVE DCS-KDVALISO          TO MOD-KDVALISO-MC                    
069800       ELSE                                                               
069900         MOVE 'Currency   '         TO MOD-CURRENCY-MC                    
070000         MOVE 'SEK'                 TO MOD-KDVALISO-MC                    
070100       END-IF                                                             
070200     END-IF                                                               
070300                                                                          
070400     IF W-IDSHIPM NUMERIC AND W-IDSHIPM > ZERO                            
070500       PERFORM FA-READ-BASICDATA                                          
070600                                                                          
070700       IF SEGMENT-MISSING                                                 
070800         PERFORM MFS-ERASE-FIELD-LINE                                     
070900       ELSE                                                               
071000         IF DIST79-DEALER-PRICE OR                                        
071200            DIST79-ECOM-PRICE                                             
071300                                                                          
071400           MOVE MFS-ERASE-FIELD TO MOD-PRFRAKT                            
071500                                   MOD-PRFOERS                            
071600                                   MOD-REFOERS                            
071700                                   MOD-REOVKOFF                           
071800                                   MOD-PRLEGKST                           
071900                                   MOD-RELEGKST                           
072000                                   MOD-PREMBHNT                           
072100                                   MOD-REEMBHNT                           
072200                                   MOD-PRAVDRAG                           
072300                                   MOD-REAVDRAG                           
072400         ELSE                                                             
072500           MOVE BGMT-PRFRAKT    TO MOD-PRFRAKT                            
072600           MOVE BGMT-PRFOERS    TO MOD-PRFOERS                            
072700           MOVE BGMT-REFOERS    TO MOD-REFOERS                            
072800           MOVE BGMT-REOVKOFF   TO MOD-REOVKOFF                           
072900           MOVE BGMT-PRLEGKST   TO MOD-PRLEGKST                           
073000           MOVE BGMT-RELEGKST   TO MOD-RELEGKST                           
073100           MOVE BGMT-PREMBHNT   TO MOD-PREMBHNT                           
073200           MOVE BGMT-REEMBHNT   TO MOD-REEMBHNT                           
073300           MOVE BGMT-PRAVDRAG   TO MOD-PRAVDRAG                           
073400           MOVE BGMT-REAVDRAG   TO MOD-REAVDRAG                           
073500         END-IF                                                           
073600                                                                          
073700         IF BGMT-FLSEPINV = 'J'                                           
073800           MOVE 'Y'             TO MOD-FLSEPINV                           
073900         ELSE                                                             
074000           MOVE BGMT-FLSEPINV   TO MOD-FLSEPINV                           
074100         END-IF                                                           
074200       END-IF                                                             
074300     END-IF                                                               
074400     .                                                                    
074500     EJECT                                                                
074600 FA-READ-BASICDATA SECTION.                                               
074700                                                                          
074800     IF SAVE-FLSAMFAK = YES                                               
074900       PERFORM IMS-GHU-WDE211-DIST                                        
075000     ELSE                                                                 
075100       PERFORM IMS-GHU-WDE211                                             
075200     END-IF                                                               
075300     .                                                                    
075400     EJECT                                                                
075500                                                                          
075600 G-CHECK-INPUT SECTION.                                                   
075700                                                                          
075800     MOVE NOO TO INPUT-GIVEN-SW                                           
075900     MOVE YES TO INDATA-SW                                                
076000                                                                          
076100     MOVE ZERO TO   SAVE-PRKURS-MAN                                       
076200                    SAVE-PRFRAKT                                          
076300                    SAVE-PRLEGKST                                         
076400                    SAVE-RELEGKST                                         
076500                    SAVE-PREMBHNT                                         
076600                    SAVE-REEMBHNT                                         
076700                    SAVE-PRFOERS                                          
076800                    SAVE-REFOERS                                          
076900                    SAVE-REOVKOFF                                         
077000                    SAVE-PRAVDRAG                                         
077100                    SAVE-REAVDRAG                                         
077200     EJECT                                                                
077300     IF MID-W4I67801 = ALL '+'                                            
077400       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
077500       CALL WMEDKONV USING MED-WMEDAREA                                   
077600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
077700       PERFORM MFS-DONT-TOUCH-FIELD-IN                                    
077800       PERFORM MFS-DONT-TOUCH-FIELD-OUT                                   
077900       MOVE NOO TO INDATA-SW                                              
078000     ELSE                                                                 
078100       PERFORM S05-CHECK-SPACE-INPUT                                      
078200       IF NOT DIST79-DEALER-PRICE AND                                     
078400          NOT DIST79-ECOM-PRICE                                           
078500         MOVE W-IDDISTR TO  TEST-IDDISTR                                  
078600         IF DIST35-RETUR              OR                                  
078700            DIST35-CDC-NL-REFILL      OR                                  
078800            DIST35-CDC-GB-REFILL      OR                                  
078900            DIST35-CDC-GB-3A-REFILL   OR                                  
079000            DIST35-CDC-ES-REFILL      OR                                  
079100            DIST35-CDC-IT-REFILL      OR                                  
079200            DIST35-CDC-AT-REFILL      OR                                  
079300            DIST35-REFILL-JP          OR                                  
079400            DIST35-REFILL-NA          OR                                  
079500            DIST35-REFILL-NA-JAP      OR                                  
079600            DIST35-REFILL-CN          OR                                  
079700            DIST35-CDC-IN-REFILL      OR                                  
079800            DIST35-CDC-KR-REFILL      OR                                  
079900            DIST35-CDC-AE-REFILL      OR                                  
080000            DIST35-CDC-TR-REFILL      OR                                  
080100            DIST35-CDC-MY-REFILL      OR                                  
080200            DIST35-CDC-TH-REFILL      OR                                  
080300            DIST35-CDC-TW-REFILL      OR                                  
080400            DIST35-CDC-MX-REFILL      OR                                  
080500            DIST35-CDC-BR-REFILL      OR                                  
080510            DIST35-CDC-ZA-REFILL      OR                                  
080600            DIST07-NA-CUSTOMERS       OR                                  
080700            DIST07-KINA               OR                                  
080800            DIST07-KINA-RET-DISCR     OR                                  
080900            DIST07-INDIEN             OR                                  
081000            DIST07-INDIEN-RET-DISCR   OR                                  
081100            DIST07-KOREA              OR                                  
081200            DIST07-KOREA-RET-DISCR    OR                                  
081300            DIST07-TURKEY             OR                                  
081400            DIST07-TURKEY-RET-DISCR   OR                                  
081500            DIST07-MALAYSIA           OR                                  
081600            DIST07-MALAYSIA-RET-DISCR OR                                  
081700            DIST07-THAILAND           OR                                  
081800            DIST07-THAILAND-RET-DISCR OR                                  
081900            DIST07-TAIWAN             OR                                  
082000            DIST07-TAIWAN-RET-DISCR   OR                                  
082100            DIST07-MEXICO             OR                                  
082200            DIST07-MEXICO-RET-DISCR   OR                                  
082201            DIST07-BRAZIL             OR                                  
082202            DIST07-BRAZIL-RET-DISCR   OR                                  
082210            DIST07-S-AFRICA           OR                                  
082220            DIST07-S-AFRICA-RET-DISCR                                     
082300                                                                          
082400           IF MID-PRFRAKT NOT = ALL '+'                                   
082500             IF (DIST35-REFILL                                            
082600                 AND NOT (DIST35-REFILL-NA OR                             
082700                          DIST35-REFILL-CN OR                             
082800                          DIST35-CDC-IN-REFILL OR                         
082900                          DIST35-CDC-KR-REFILL OR                         
083000                          DIST35-CDC-AE-REFILL OR                         
083100                          DIST35-CDC-TR-REFILL OR                         
083200                          DIST35-CDC-MY-REFILL OR                         
083300                          DIST35-CDC-TH-REFILL OR                         
083400                          DIST35-CDC-TW-REFILL OR                         
083500                          DIST35-CDC-MX-REFILL OR                         
083600                          DIST35-CDC-BR-REFILL OR                         
083610                          DIST35-CDC-ZA-REFILL))                          
083700                                      OR                                  
083800                 DIST35-RETUR         OR                                  
083900                 DIST35-REFILL-NA-JAP                                     
084000               MOVE MFS-NUM-FIELD-WRONG TO MOD-PRFRAKT-ATTR               
084100               MOVE NOO TO INDATA-SW                                      
084200             END-IF                                                       
084300           END-IF                                                         
084400                                                                          
084500           IF MID-PRLEGKST NOT = ALL '+'                                  
084600             MOVE MFS-NUM-FIELD-WRONG TO MOD-PRLEGKST-ATTR                
084700             MOVE NOO TO INDATA-SW                                        
084800           END-IF                                                         
084900                                                                          
085000           IF MID-PREMBHNT NOT = ALL '+'                                  
085100             MOVE MFS-NUM-FIELD-WRONG TO MOD-PREMBHNT-ATTR                
085200             MOVE NOO TO INDATA-SW                                        
085300           END-IF                                                         
085400                                                                          
085500           IF MID-PRFOERS  NOT = ALL '+'                                  
085600             MOVE MFS-NUM-FIELD-WRONG TO MOD-PRFOERS-ATTR                 
085700             MOVE NOO TO INDATA-SW                                        
085800           END-IF                                                         
085900                                                                          
086000           IF MID-PRAVDRAG NOT = ALL '+'                                  
086100             MOVE MFS-NUM-FIELD-WRONG TO MOD-PRAVDRAG-ATTR                
086200             MOVE NOO TO INDATA-SW                                        
086300           END-IF                                                         
086400                                                                          
086500           IF MID-RELEGKST NOT = ALL '+'                                  
086600             MOVE MFS-NUM-FIELD-WRONG TO MOD-RELEGKST-ATTR                
086700             MOVE NOO TO INDATA-SW                                        
086800           END-IF                                                         
086900                                                                          
087000           IF MID-REEMBHNT NOT = ALL '+'                                  
087100             IF (DIST35-REFILL AND NOT (DIST35-REFILL-NA OR               
087200                                        DIST35-REFILL-CN OR               
087300                                        DIST35-CDC-IN-REFILL OR           
087400                                        DIST35-CDC-KR-REFILL OR           
087500                                        DIST35-CDC-AE-REFILL OR           
087600                                        DIST35-CDC-TR-REFILL OR           
087700                                        DIST35-CDC-MY-REFILL OR           
087800                                        DIST35-CDC-TH-REFILL OR           
087900                                        DIST35-CDC-TW-REFILL OR           
088000                                        DIST35-CDC-MX-REFILL OR           
088100                                        DIST35-CDC-BR-REFILL OR           
088110                                        DIST35-CDC-ZA-REFILL))            
088200                               OR                                         
088300                DIST35-RETUR   OR                                         
088400                DIST35-REFILL-NA-JAP                                      
088500               MOVE MFS-NUM-FIELD-WRONG TO MOD-REEMBHNT-ATTR              
088600               MOVE NOO TO INDATA-SW                                      
088700             END-IF                                                       
088800           END-IF                                                         
088900                                                                          
089000           IF MID-REFOERS  NOT = ALL '+'                                  
089100             MOVE MFS-NUM-FIELD-WRONG TO MOD-REFOERS-ATTR                 
089200             MOVE NOO TO INDATA-SW                                        
089300           END-IF                                                         
089400                                                                          
089500           IF MID-REAVDRAG NOT = ALL '+'                                  
089600             MOVE MFS-NUM-FIELD-WRONG TO MOD-REAVDRAG-ATTR                
089700             MOVE NOO TO INDATA-SW                                        
089800           END-IF                                                         
089900                                                                          
090000           IF MID-REOVKOFF NOT = ALL '+'                                  
090100             MOVE MFS-NUM-FIELD-WRONG TO MOD-REOVKOFF-ATTR                
090200             MOVE NOO TO INDATA-SW                                        
090300           END-IF                                                         
090400         END-IF                                                           
090500                                                                          
090600         IF MID-PRFRAKT NOT = ALL '+'                                     
090700           MOVE MID-PRFRAKT     TO DEC-IDFRIDATA                          
090800           MOVE 7 TO DEC-KVHELTAL                                         
090900           MOVE 2 TO DEC-KVDECIMAL                                        
091000           PERFORM S01-HANDLE-ALPHA-TO-NUM                                
091100                                                                          
091200           IF DEC-KDSVAR-OK                                               
091300             MOVE DEC-IDEDITDATA TO SAVE-PRFRAKT                          
091400             MOVE MFS-NUM-FIELD-OK                                        
091500                                 TO MOD-PRFRAKT-ATTR                      
091600             MOVE YES TO INPUT-GIVEN-SW                                   
091700           ELSE                                                           
091800             MOVE MFS-NUM-FIELD-WRONG                                     
091900                                 TO MOD-PRFRAKT-ATTR                      
092000             MOVE NOO TO INDATA-SW                                        
092100           END-IF                                                         
092200         END-IF                                                           
092300                                                                          
092400         IF MID-LEGKST NOT = ALL '+'                                      
092500           IF MID-PRLEGKST NOT = ALL '+' AND                              
092600              MID-RELEGKST NOT = ALL '+'                                  
092700             MOVE MFS-NUM-FIELD-WRONG TO MOD-PRLEGKST-ATTR                
092800                                         MOD-RELEGKST-ATTR                
092900             MOVE NOO TO INDATA-SW                                        
093000           ELSE                                                           
093100             IF MID-RELEGKST NOT = ALL '+'                                
093200               MOVE MID-RELEGKST    TO DEC-IDFRIDATA                      
093300               MOVE 2 TO DEC-KVHELTAL                                     
093400               MOVE 1 TO DEC-KVDECIMAL                                    
093500               PERFORM S01-HANDLE-ALPHA-TO-NUM                            
093600                                                                          
093700               IF DEC-KDSVAR-OK                                           
093800                 MOVE DEC-IDEDITDATA TO SAVE-RELEGKST                     
093900                 MOVE MFS-NUM-FIELD-OK                                    
094000                                     TO MOD-RELEGKST-ATTR                 
094100                 MOVE YES TO INPUT-GIVEN-SW                               
094200               ELSE                                                       
094300                 MOVE MFS-NUM-FIELD-WRONG                                 
094400                                     TO MOD-RELEGKST-ATTR                 
094500                 MOVE NOO TO INDATA-SW                                    
094600               END-IF                                                     
094700             END-IF                                                       
094800                                                                          
094900             IF MID-PRLEGKST NOT = ALL '+'                                
095000               MOVE MID-PRLEGKST    TO DEC-IDFRIDATA                      
095100               MOVE 7 TO DEC-KVHELTAL                                     
095200               MOVE 2 TO DEC-KVDECIMAL                                    
095300               PERFORM S01-HANDLE-ALPHA-TO-NUM                            
095400                                                                          
095500               IF DEC-KDSVAR-OK                                           
095600                 MOVE DEC-IDEDITDATA TO SAVE-PRLEGKST                     
095700                 MOVE MFS-NUM-FIELD-OK                                    
095800                                     TO MOD-PRLEGKST-ATTR                 
095900                 MOVE YES TO INPUT-GIVEN-SW                               
096000               ELSE                                                       
096100                 MOVE MFS-NUM-FIELD-WRONG                                 
096200                                     TO MOD-PRLEGKST-ATTR                 
096300                 MOVE NOO TO INDATA-SW                                    
096400               END-IF                                                     
096500             END-IF                                                       
096600           END-IF                                                         
096700         END-IF                                                           
096800                                                                          
096900         IF MID-EMBHNT NOT = ALL '+'                                      
097000           IF MID-PREMBHNT NOT = ALL '+'  AND                             
097100              MID-REEMBHNT NOT = ALL '+'                                  
097200             MOVE MFS-NUM-FIELD-WRONG TO MOD-PREMBHNT-ATTR                
097300                                         MOD-REEMBHNT-ATTR                
097400             MOVE NOO TO INDATA-SW                                        
097500           ELSE                                                           
097600             IF MID-REEMBHNT NOT = ALL '+'                                
097700               MOVE MID-REEMBHNT    TO DEC-IDFRIDATA                      
097800               MOVE 2 TO DEC-KVHELTAL                                     
097900               MOVE 1 TO DEC-KVDECIMAL                                    
098000               PERFORM S01-HANDLE-ALPHA-TO-NUM                            
098100                                                                          
098200               IF DEC-KDSVAR-OK                                           
098300                 MOVE DEC-IDEDITDATA TO SAVE-REEMBHNT                     
098400                 MOVE MFS-NUM-FIELD-OK                                    
098500                                     TO MOD-REEMBHNT-ATTR                 
098600                 MOVE YES TO INPUT-GIVEN-SW                               
098700               ELSE                                                       
098800                 MOVE MFS-NUM-FIELD-WRONG                                 
098900                                     TO MOD-REEMBHNT-ATTR                 
099000                 MOVE NOO TO INDATA-SW                                    
099100               END-IF                                                     
099200             END-IF                                                       
099300                                                                          
099400             IF MID-PREMBHNT NOT = ALL '+'                                
099500               MOVE MID-PREMBHNT    TO DEC-IDFRIDATA                      
099600               MOVE 7 TO DEC-KVHELTAL                                     
099700               MOVE 2 TO DEC-KVDECIMAL                                    
099800               PERFORM S01-HANDLE-ALPHA-TO-NUM                            
099900                                                                          
100000               IF DEC-KDSVAR-OK                                           
100100                 MOVE DEC-IDEDITDATA TO SAVE-PREMBHNT                     
100200                 MOVE MFS-NUM-FIELD-OK                                    
100300                                     TO MOD-PREMBHNT-ATTR                 
100400                 MOVE YES TO INPUT-GIVEN-SW                               
100500               ELSE                                                       
100600                 MOVE MFS-NUM-FIELD-WRONG                                 
100700                                     TO MOD-PREMBHNT-ATTR                 
100800                 MOVE NOO TO INDATA-SW                                    
100900               END-IF                                                     
101000             END-IF                                                       
101100           END-IF                                                         
101200         END-IF                                                           
101300                                                                          
101400         IF MID-FOERS NOT = ALL '+'                                       
101500           IF MID-PRFOERS NOT = ALL '+'  AND                              
101600              MID-REFOERS-OVKOFF NOT = ALL '+'                            
101700             IF MID-PRFOERS NOT = ALL '+'                                 
101800               MOVE MFS-NUM-FIELD-WRONG TO MOD-PRFOERS-ATTR               
101900             END-IF                                                       
102000             IF MID-REFOERS NOT = ALL '+'                                 
102100               MOVE MFS-NUM-FIELD-WRONG TO MOD-REFOERS-ATTR               
102200             END-IF                                                       
102300             IF MID-REOVKOFF NOT = ALL '+'                                
102400               MOVE MFS-NUM-FIELD-WRONG TO MOD-REOVKOFF-ATTR              
102500             END-IF                                                       
102600             MOVE NOO TO INDATA-SW                                        
102700           ELSE                                                           
102800             IF MID-PRFOERS NOT = ALL '+'                                 
102900               MOVE MID-PRFOERS     TO DEC-IDFRIDATA                      
103000               MOVE 7 TO DEC-KVHELTAL                                     
103100               MOVE 2 TO DEC-KVDECIMAL                                    
103200               PERFORM S01-HANDLE-ALPHA-TO-NUM                            
103300                                                                          
103400               IF DEC-KDSVAR-OK                                           
103500                 MOVE DEC-IDEDITDATA TO SAVE-PRFOERS                      
103600                 MOVE MFS-NUM-FIELD-OK                                    
103700                                     TO MOD-PRFOERS-ATTR                  
103800                 MOVE YES TO INPUT-GIVEN-SW                               
103900               ELSE                                                       
104000                 MOVE MFS-NUM-FIELD-WRONG                                 
104100                                     TO MOD-PRFOERS-ATTR                  
104200                 MOVE NOO TO INDATA-SW                                    
104300               END-IF                                                     
104400             END-IF                                                       
104500                                                                          
104600             IF MID-REFOERS-OVKOFF NOT = ALL '+'                          
104700               IF MID-REFOERS = ALL '+'                                   
104800                 MOVE MFS-NUM-FIELD-WRONG TO MOD-REFOERS-ATTR             
104900                                             MOD-REOVKOFF-ATTR            
105000                 MOVE NOO TO INDATA-SW                                    
105100               ELSE                                                       
105200                 MOVE MID-REFOERS     TO DEC-IDFRIDATA                    
105300                 MOVE 2 TO DEC-KVHELTAL                                   
105400                 MOVE 3 TO DEC-KVDECIMAL                                  
105500                 PERFORM S01-HANDLE-ALPHA-TO-NUM                          
105600                                                                          
105700                 IF DEC-KDSVAR-OK                                         
105800                   MOVE DEC-IDEDITDATA TO SAVE-REFOERS                    
105900                   MOVE MFS-NUM-FIELD-OK                                  
106000                                        TO MOD-REFOERS-ATTR               
106100                   MOVE YES TO INPUT-GIVEN-SW                             
106200                 ELSE                                                     
106300                   MOVE MFS-NUM-FIELD-WRONG                               
106400                                        TO MOD-REFOERS-ATTR               
106500                   MOVE NOO TO INDATA-SW                                  
106600                 END-IF                                                   
106700               END-IF                                                     
106800                                                                          
106900               IF MID-REOVKOFF NOT = ALL '+'                              
107000                 MOVE MID-REOVKOFF    TO DEC-IDFRIDATA                    
107100                 MOVE 2 TO DEC-KVHELTAL                                   
107200                 MOVE 1 TO DEC-KVDECIMAL                                  
107300                 PERFORM S01-HANDLE-ALPHA-TO-NUM                          
107400                                                                          
107500                 IF DEC-KDSVAR-OK                                         
107600                   MOVE DEC-IDEDITDATA TO SAVE-REOVKOFF                   
107700                   MOVE MFS-NUM-FIELD-OK                                  
107800                                       TO MOD-REOVKOFF-ATTR               
107900                   MOVE YES TO INPUT-GIVEN-SW                             
108000                 ELSE                                                     
108100                   MOVE MFS-NUM-FIELD-WRONG                               
108200                                       TO MOD-REOVKOFF-ATTR               
108300                   MOVE NOO TO INDATA-SW                                  
108400                 END-IF                                                   
108500               ELSE                                                       
108600                 MOVE NOO TO INDATA-SW                                    
108700                 MOVE MFS-NUM-FIELD-WRONG TO MOD-REOVKOFF-ATTR            
108800               END-IF                                                     
108900             END-IF                                                       
109000           END-IF                                                         
109100         END-IF                                                           
109200                                                                          
109300         IF MID-AVDRAG         NOT = ALL '+'                              
109400           MOVE YES TO INPUT-GIVEN-SW                                     
109500           IF MID-REAVDRAG NOT = ALL '+'    AND                           
109600              MID-PRAVDRAG NOT = ALL '+'                                  
109700             MOVE MFS-NUM-FIELD-WRONG  TO MOD-REAVDRAG-ATTR               
109800                                          MOD-PRAVDRAG-ATTR               
109900             MOVE NOO TO INDATA-SW                                        
110000           ELSE                                                           
110100             IF MID-REAVDRAG NOT = ALL '+'                                
110200               MOVE MID-REAVDRAG      TO DEC-IDFRIDATA                    
110300               MOVE 2 TO DEC-KVHELTAL                                     
110400               MOVE 1 TO DEC-KVDECIMAL                                    
110500               PERFORM S01-HANDLE-ALPHA-TO-NUM                            
110600                                                                          
110700               IF DEC-KDSVAR-OK                                           
110800                 MOVE DEC-IDEDITDATA  TO SAVE-REAVDRAG                    
110900                 MOVE MFS-NUM-FIELD-OK                                    
111000                                      TO MOD-REAVDRAG-ATTR                
111100                 MOVE YES             TO INPUT-GIVEN-SW                   
111200               ELSE                                                       
111300                 MOVE MFS-NUM-FIELD-WRONG                                 
111400                                      TO MOD-REAVDRAG-ATTR                
111500                 MOVE NOO             TO INDATA-SW                        
111600               END-IF                                                     
111700             END-IF                                                       
111800                                                                          
111900             IF MID-PRAVDRAG NOT = ALL '+'                                
112000               MOVE MID-PRAVDRAG      TO DEC-IDFRIDATA                    
112100               MOVE 7 TO DEC-KVHELTAL                                     
112200               MOVE 2 TO DEC-KVDECIMAL                                    
112300               PERFORM S01-HANDLE-ALPHA-TO-NUM                            
112400                                                                          
112500               IF DEC-KDSVAR-OK                                           
112600                 MOVE DEC-IDEDITDATA  TO SAVE-PRAVDRAG                    
112700                 MOVE MFS-NUM-FIELD-OK                                    
112800                                      TO MOD-PRAVDRAG-ATTR                
112900                 MOVE YES             TO INPUT-GIVEN-SW                   
113000               ELSE                                                       
113100                 MOVE MFS-NUM-FIELD-WRONG                                 
113200                                      TO MOD-PRAVDRAG-ATTR                
113300                 MOVE NOO             TO INDATA-SW                        
113400               END-IF                                                     
113500             END-IF                                                       
113600           END-IF                                                         
113700         END-IF                                                           
113800       END-IF                                                             
113900                                                                          
114000       IF MID-FLSEPINV NOT = ALL '+'                                      
114100         IF MID-FLSEPINV  = 'J' OR 'Y'                                    
114200           MOVE YES TO INPUT-GIVEN-SW                                     
114300           MOVE MFS-ALPHA-FIELD-OK    TO MOD-FLSEPINV-ATTR                
114400         ELSE                                                             
114500           MOVE MFS-ALPHA-FIELD-WRONG TO MOD-FLSEPINV-ATTR                
114600           MOVE NOO                   TO INDATA-SW                        
114700         END-IF                                                           
114800       END-IF                                                             
114900                                                                          
115000       IF INDATA-WRONG                                                    
115100         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
115200         CALL WMEDKONV USING MED-WMEDAREA                                 
115300         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
115400         PERFORM MFS-DONT-TOUCH-FIELD-OUT                                 
115500         PERFORM MFS-DONT-TOUCH-FIELD-IN                                  
115600       END-IF                                                             
115700     END-IF                                                               
115800     .                                                                    
115900     EJECT                                                                
116000 H-UPDATE SECTION.                                                        
116100                                                                          
116200     MOVE YES    TO WDE211-ISRT-SW                                        
116300     PERFORM S04-DC-LAND                                                  
116400                                                                          
116500     IF INPUT-GIVEN                                                       
116600** READ FIRST 4482 SEGMENT FOR IDDISTR                                    
116700       MOVE SAVE-IDDISTR-4678                                             
116800                         TO W-4482-IDDISTR-MIN                            
116900                            W-4482-IDDISTR-MAX                            
117000                                                                          
117100       IF FLSAMFAK                                                        
117200         MOVE ZERO       TO W-4482-IDKUNDNR-MIN                           
117300         MOVE 9999999    TO W-4482-IDKUNDNR-MAX                           
117400       ELSE                                                               
117500         MOVE SAVE-IDKUNDNR-4678                                          
117600                         TO W-4482-IDKUNDNR-MIN                           
117700                            W-4482-IDKUNDNR-MAX                           
117800       END-IF                                                             
117900                                                                          
118000       PERFORM IMS-GU-WDGX4482                                            
118100       IF SEGMENT-FOUND                                                   
118200         MOVE 4482-IDKUNDNR TO W-WDE211-IDKUNDNR                          
118300         IF SAVE-IDSHIPM  > ZERO                                          
118400                                                                          
118500           PERFORM IMS-GHU-WDE201                                         
118600           PERFORM IMS-GHNP-WDE211                                        
118700           PERFORM S02-TEST-WDE211                                        
118800           PERFORM HA-UPDATE-WDE211                                       
118900         ELSE                                                             
119000           PERFORM S03-CREATE-IDSHIPM                                     
119100           PERFORM HA-UPDATE-WDE211                                       
119200         END-IF                                                           
119300                                                                          
119400         IF FLSAMFAK                                                      
119500           PERFORM IMS-GNP-WDGX4482                                       
119600                                                                          
119700           PERFORM UNTIL  SEGMENT-MISSING                                 
119800                                                                          
119900             MOVE 4482-IDKUNDNR TO W-WDE211-IDKUNDNR                      
120000                                                                          
120100             PERFORM IMS-GHU-WDE211                                       
120200             PERFORM S02-TEST-WDE211                                      
120300             PERFORM HA-UPDATE-WDE211                                     
120400                                                                          
120500             PERFORM IMS-GNP-WDGX4482                                     
120600           END-PERFORM                                                    
120700         END-IF                                                           
120800       END-IF                                                             
120900                                                                          
121000       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
121100       CALL WMEDKONV USING MED-WMEDAREA                                   
121200       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
121300       PERFORM MFS-FORM-ATTR                                              
121400       PERFORM MFS-ERASE-FIELD-IN                                         
121500                                                                          
121600     END-IF                                                               
121700                                                                          
121800     .                                                                    
121900     EJECT                                                                
122000                                                                          
122100 HA-UPDATE-WDE211  SECTION.                                               
122200                                                                          
122300     IF MID-FLSEPINV  = 'Y' OR 'J'                                        
122400       MOVE 'J'          TO BGMT-FLSEPINV                                 
122500     ELSE                                                                 
122600       IF MID-FLSEPINV = SPACE                                            
122700         MOVE SPACE      TO BGMT-FLSEPINV                                 
122800       END-IF                                                             
122900     END-IF                                                               
123000                                                                          
123100     IF NOT DIST79-DEALER-PRICE AND                                       
123300        NOT DIST79-ECOM-PRICE                                             
123400       IF MID-PRFRAKT NOT = ALL '+'                                       
123500         MOVE SAVE-PRFRAKT TO BGMT-PRFRAKT                                
123600       END-IF                                                             
123700                                                                          
123800       IF MID-FOERS NOT = ALL '+'                                         
123900         IF MID-PRFOERS NOT = ALL '+'                                     
124000           MOVE SAVE-PRFOERS TO BGMT-PRFOERS                              
124100           MOVE ZERO TO BGMT-REFOERS                                      
124200                        BGMT-REOVKOFF                                     
124300         ELSE                                                             
124400           MOVE SAVE-REFOERS  TO BGMT-REFOERS                             
124500           MOVE SAVE-REOVKOFF TO BGMT-REOVKOFF                            
124600           MOVE ZERO TO BGMT-PRFOERS                                      
124700         END-IF                                                           
124800       END-IF                                                             
124900                                                                          
125000       IF MID-LEGKST NOT = ALL '+'                                        
125100         IF MID-PRLEGKST NOT = ALL '+'                                    
125200           MOVE SAVE-PRLEGKST TO BGMT-PRLEGKST                            
125300           MOVE ZERO          TO BGMT-RELEGKST                            
125400         END-IF                                                           
125500                                                                          
125600         IF MID-RELEGKST NOT = ALL '+'                                    
125700           MOVE SAVE-RELEGKST TO BGMT-RELEGKST                            
125800           MOVE ZERO          TO BGMT-PRLEGKST                            
125900         END-IF                                                           
126000       END-IF                                                             
126100                                                                          
126200       IF MID-EMBHNT NOT = ALL '+'                                        
126300         IF MID-PREMBHNT NOT = ALL '+'                                    
126400           MOVE SAVE-PREMBHNT TO BGMT-PREMBHNT                            
126500           MOVE ZERO TO BGMT-REEMBHNT                                     
126600         ELSE                                                             
126700           MOVE SAVE-REEMBHNT TO BGMT-REEMBHNT                            
126800           MOVE ZERO          TO BGMT-PREMBHNT                            
126900         END-IF                                                           
127000       END-IF                                                             
127100                                                                          
127200       IF MID-AVDRAG NOT = ALL '+'                                        
127300         IF MID-PRAVDRAG NOT = ALL '+'                                    
127400           MOVE SAVE-PRAVDRAG TO BGMT-PRAVDRAG                            
127500         END-IF                                                           
127600                                                                          
127700         IF MID-REAVDRAG NOT = ALL '+'                                    
127800           MOVE SAVE-REAVDRAG TO BGMT-REAVDRAG                            
127900         END-IF                                                           
128000       END-IF                                                             
128100     END-IF                                                               
128200                                                                          
128300     IF WDE211-ISRT                                                       
128400       PERFORM IMS-ISRT-WDE211                                            
128500     ELSE                                                                 
128600       PERFORM IMS-REPL-WDE211                                            
128700     END-IF                                                               
128800     .                                                                    
128900     EJECT                                                                
129000                                                                          
129100 I-PF3-RETURN SECTION.                                                    
129200                                                                          
129300     COMPUTE P-TO-P-KVLL = LENGTH OF PTOP-MID-W4I67701 + 17               
129400     MOVE ALL '+'              TO PTOP-MID-W4I67701                       
129500     MOVE LOW-VALUE            TO P-TO-P-KDZ1                             
129600     MOVE LOW-VALUE            TO P-TO-P-KDZ2                             
129700     MOVE 'W4T677  '           TO P-TO-P-KDTRANS                          
129800     MOVE '4678'               TO P-TO-P-IDTRANS                          
129900     MOVE MFS-KDMFSFOR         TO P-TO-P-KDMFSFOR                         
130000                                                                          
130100     .                                                                    
130200     EJECT                                                                
130300                                                                          
130400 J-CLOSE-FIELDS SECTION.                                                  
130500                                                                          
130600     MOVE MFS-CLOSE-FIELD TO MOD-PRFRAKT-ATTR                             
130700                             MOD-PRFOERS-ATTR                             
130800                             MOD-REFOERS-ATTR                             
130900                             MOD-REOVKOFF-ATTR                            
131000                             MOD-PRLEGKST-ATTR                            
131100                             MOD-RELEGKST-ATTR                            
131200                             MOD-PREMBHNT-ATTR                            
131300                             MOD-REEMBHNT-ATTR                            
131400                             MOD-PRAVDRAG-ATTR                            
131500                             MOD-REAVDRAG-ATTR                            
131600                                                                          
131700     MOVE MFS-ERASE-FIELD TO MOD-PRFRAKT                                  
131800                             MOD-PRFOERS                                  
131900                             MOD-REFOERS                                  
132000                             MOD-REOVKOFF                                 
132100                             MOD-PRLEGKST                                 
132200                             MOD-RELEGKST                                 
132300                             MOD-PREMBHNT                                 
132400                             MOD-REEMBHNT                                 
132500                             MOD-PRAVDRAG                                 
132600                             MOD-REAVDRAG                                 
132700     .                                                                    
132800     EJECT                                                                
132900                                                                          
133000 S01-HANDLE-ALPHA-TO-NUM SECTION.                                         
133100                                                                          
133200     CALL WDECEDIT USING DEC-IDFRIDATA                                    
133300                         DEC-IDEDITDATA                                   
133400                         DEC-KVHELTAL                                     
133500                         DEC-KVDECIMAL                                    
133600                         DEC-KDSVAR                                       
133700     .                                                                    
133800     SKIP2                                                                
133900                                                                          
134000 S02-TEST-WDE211         SECTION.                                         
134100                                                                          
134200     MOVE YES    TO WDE211-ISRT-SW                                        
134300                                                                          
134400     IF SEGMENT-FOUND                                                     
134500       MOVE NOO TO WDE211-ISRT-SW                                         
134600     ELSE                                                                 
134700       MOVE 4482-IDDISTR  TO BGMT-IDDISTR                                 
134800       MOVE 4482-IDKUNDNR TO BGMT-IDKUNDNR                                
134900                                                                          
135000       MOVE SPACE    TO BGMT-IDPARTNR                                     
135100                        BGMT-FLSEPINV                                     
135200       MOVE 'N'      TO BGMT-FLCOD                                        
135300                                                                          
135400       MOVE ZERO     TO BGMT-KDLEVVIL                                     
135500                        BGMT-PRAVDRAG                                     
135600                        BGMT-PREMBHNT                                     
135700                        BGMT-PRFOERS                                      
135800                        BGMT-PRFRAKT                                      
135900                        BGMT-PRLEGKST                                     
136000                        BGMT-REAVDRAG                                     
136100                        BGMT-REEMBHNT                                     
136200                        BGMT-REFOERS                                      
136300                        BGMT-RELEGKST                                     
136400                        BGMT-REOVKOFF                                     
136500     END-IF                                                               
136600                                                                          
136700     .                                                                    
136800     SKIP2                                                                
136900                                                                          
137000 S03-CREATE-IDSHIPM  SECTION.                                             
137100                                                                          
137200     CALL W476SHNO         USING SHNO-W476SHNO SHNO-4517-PCB              
137300                                                                          
137400     MOVE SHNO-IDSHIPM        TO W-IDSHIPM                                
137500                                 SAVE-IDSHIPM                             
137600     MOVE YES                 TO UPDATE-SAVE-SW                           
137700                                                                          
137800** BUILD AND ISRT WDE201                                                  
137900     MOVE W-IDSHIPM      TO BILL-IDSHIPM                                  
138000     MOVE W-IDDC         TO BILL-IDDC                                     
138100     MOVE W-IDLANDX2     TO BILL-IDLANDX3-SEND                            
138200     MOVE ZERO           TO BILL-IDLEVNR                                  
138300     MOVE 'PROF'         TO BILL-KDFINDOC                                 
138400     MOVE DC-MSGI-TILOKDAT  TO BILL-TISKEPPN                              
138500     MOVE DC-MSGI-TILOKTID  TO WS-TISKPTID-HHMM                           
138600     MOVE FUNCTION CURRENT-DATE (13:2) TO                                 
138700                      WS-TISKPTID-SS                                      
138800     MOVE WS-TISKPTID          TO BILL-TISKPTID                           
138900     MOVE SPACE                TO BILL-IDDC-EXP                           
139000     PERFORM IMS-ISRT-WDE201                                              
139100                                                                          
139200** BUILD WDE211                                                           
139300                                                                          
139400     MOVE 4482-IDDISTR  TO BGMT-IDDISTR                                   
139500     MOVE 4482-IDKUNDNR TO BGMT-IDKUNDNR                                  
139600                                                                          
139700     MOVE SPACE    TO BGMT-IDPARTNR                                       
139800                      BGMT-FLSEPINV                                       
139900     MOVE 'N'      TO BGMT-FLCOD                                          
140000                                                                          
140100     MOVE ZERO     TO BGMT-KDLEVVIL                                       
140200                      BGMT-PRAVDRAG                                       
140300                      BGMT-PREMBHNT                                       
140400                      BGMT-PRFOERS                                        
140500                      BGMT-PRFRAKT                                        
140600                      BGMT-PRLEGKST                                       
140700                      BGMT-REAVDRAG                                       
140800                      BGMT-REEMBHNT                                       
140900                      BGMT-REFOERS                                        
141000                      BGMT-RELEGKST                                       
141100                      BGMT-REOVKOFF                                       
141200                                                                          
141300     MOVE YES    TO WDE211-ISRT-SW                                        
141400                                                                          
141500     .                                                                    
141600     EJECT                                                                
141700                                                                          
141800 S04-DC-LAND  SECTION.                                                    
141900                                                                          
142000     IF W-IDDC NOT = W-IDDC-B6                                            
142100        MOVE W-IDDC TO W-IDDC-B6                                          
142200        PERFORM IMS-GU-WDB601                                             
142300     END-IF                                                               
142400     MOVE DCS-IDLANDX2 TO W-IDLANDX2                                      
142500     .                                                                    
142600     EJECT                                                                
142700                                                                          
142800 S05-CHECK-SPACE-INPUT SECTION.                                           
142900                                                                          
143000** SPACE INPUT WILL BE IGNORED                                            
143100                                                                          
143200     IF MID-PRLEGKST = SPACE                                              
143300       MOVE ALL '+'  TO MID-PRLEGKST                                      
143400     END-IF                                                               
143500     IF MID-PREMBHNT = SPACE                                              
143600       MOVE ALL '+'  TO MID-PREMBHNT                                      
143700     END-IF                                                               
143800     IF MID-REEMBHNT = SPACE                                              
143900       MOVE ALL '+'  TO MID-REEMBHNT                                      
144000     END-IF                                                               
144100     IF MID-PRFOERS  = SPACE                                              
144200       MOVE ALL '+'  TO MID-PRFOERS                                       
144300     END-IF                                                               
144400     IF MID-REFOERS  = SPACE                                              
144500       MOVE ALL '+'  TO MID-REFOERS                                       
144600     END-IF                                                               
144700     IF MID-REOVKOFF = SPACE                                              
144800       MOVE ALL '+'  TO MID-REOVKOFF                                      
144900     END-IF                                                               
145000     IF MID-PRAVDRAG = SPACE                                              
145100       MOVE ALL '+'  TO MID-PRAVDRAG                                      
145200     END-IF                                                               
145300     IF MID-REAVDRAG = SPACE                                              
145400       MOVE ALL '+'  TO MID-REAVDRAG                                      
145500     END-IF                                                               
145600     .                                                                    
145700     EJECT                                                                
145800                                                                          
145900                                                                          
146000 S10-WRONG-PICTURE-MESSAGE SECTION.                                       
146100     SKIP2                                                                
146200* *****************************************************                   
146300*                                                     *                   
146400* GIVE WRONG PICTURE MESSAGE FROM WHELP               *                   
146500*                                                     *                   
146600* *****************************************************                   
146700     SKIP2                                                                
146800     MOVE 'W0O50401'          TO MFS-IDMOD                                
146900     MOVE MFS-ERASE-FIELD     TO MOD0504-IDTRANS                          
147000     MOVE FELMEDD-ENGLISH     TO MOD0504-TEMFSINF                         
147100     MOVE MSG-KVLL-TILL-WHELP TO MSG-KVLL                                 
147200     PERFORM IMS-INSERT-MSG                                               
147300     .                                                                    
147400     EJECT                                                                
147500                                                                          
147600 MFS-ERASE-FIELD-OUT SECTION.                                             
147700                                                                          
147800*    --- ALLA UTDATA-FÄLT                                                 
147900     MOVE MFS-ERASE-FIELD TO MOD-IDTRPTNR-UT                              
148000                             MOD-IDLBBET-UT                               
148100                             MOD-FLFARLIG-UT                              
148200                             MOD-IDDC-UT                                  
148300     .                                                                    
148400     SKIP3                                                                
148500 MFS-ERASE-FIELD-LINE SECTION.                                            
148600                                                                          
148700     MOVE MFS-ERASE-FIELD TO MOD-FLSEPINV                                 
148800                             MOD-PRFRAKT                                  
148900                             MOD-PRFOERS                                  
149000                             MOD-REFOERS                                  
149100                             MOD-REOVKOFF                                 
149200                             MOD-PRLEGKST                                 
149300                             MOD-RELEGKST                                 
149400                             MOD-PREMBHNT                                 
149500                             MOD-REEMBHNT                                 
149600                             MOD-PRAVDRAG                                 
149700                             MOD-REAVDRAG                                 
149800     .                                                                    
149900     SKIP3                                                                
150000                                                                          
150100 MFS-ERASE-FIELD-IN SECTION.                                              
150200                                                                          
150300*    --- ALLA INDATA-FÄLT                                                 
150400     MOVE MFS-ERASE-FIELD TO MOD-IDTRPTNR-IN                              
150500                             MOD-IDLBBET-IN                               
150600                             MOD-FLFARLIG-IN                              
150700                             MOD-IDDC-IN                                  
150800     .                                                                    
150900     EJECT                                                                
151000 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
151100                                                                          
151200*    --- ALLA UTDATA-FÄLT                                                 
151300     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDTRPTNR-UT                       
151400                                    MOD-IDLBBET-UT                        
151500                                    MOD-FLFARLIG-UT                       
151600                                    MOD-IDDC-UT                           
151700                                    MOD-IDDISTR                           
151800                                    MOD-IDKUNDNR                          
151900                                    MOD-FLSEPINV                          
152000                                                                          
152100     IF NOT DIST79-DEALER-PRICE AND                                       
152300        NOT DIST79-ECOM-PRICE                                             
152400       MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-PRFRAKT                         
152500                                      MOD-PRFOERS                         
152600                                      MOD-REFOERS                         
152700                                      MOD-REOVKOFF                        
152800                                      MOD-PRLEGKST                        
152900                                      MOD-RELEGKST                        
153000                                      MOD-PREMBHNT                        
153100                                      MOD-REEMBHNT                        
153200                                      MOD-PRAVDRAG                        
153300                                      MOD-REAVDRAG                        
153400     END-IF                                                               
153500                                                                          
153600     .                                                                    
153700     SKIP3                                                                
153800 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
153900                                                                          
154000*    --- ALLA INDATA-FÄLT                                                 
154100     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDTRPTNR-IN                       
154200                                    MOD-IDLBBET-IN                        
154300                                    MOD-FLFARLIG-IN                       
154400                                    MOD-IDDC-IN                           
154500     .                                                                    
154600     EJECT                                                                
154700 MFS-FORM-ATTR SECTION.                                                   
154800                                                                          
154900*    --- ALL INDATA-FIELDS                                                
155000     MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-FLSEPINV-ATTR                    
155100                                                                          
155200     IF NOT DIST79-DEALER-PRICE AND                                       
155400        NOT DIST79-ECOM-PRICE                                             
155500       MOVE MFS-FORMAT-DEFAULT-ATTR                                       
155600                                  TO MOD-PRFRAKT-ATTR                     
155700                                     MOD-PRFOERS-ATTR                     
155800                                     MOD-REFOERS-ATTR                     
155900                                     MOD-REOVKOFF-ATTR                    
156000                                     MOD-PRLEGKST-ATTR                    
156100                                     MOD-RELEGKST-ATTR                    
156200                                     MOD-PREMBHNT-ATTR                    
156300                                     MOD-REEMBHNT-ATTR                    
156400                                     MOD-PRAVDRAG-ATTR                    
156500                                     MOD-REAVDRAG-ATTR                    
156600     END-IF                                                               
156700     .                                                                    
156800     SKIP2                                                                
156900     EJECT                                                                
157000* --- IMS SECTIONS ---                                                    
157100     SKIP3                                                                
157200 IMS-GET-MSG SECTION.                                                     
157300                                                                          
157400     MOVE '  QC' TO GOOD-STATUSCODES                                      
157500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
157600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
157700     PERFORM IMS-STATUSCHECK                                              
157800     .                                                                    
157900     SKIP3                                                                
158000 IMS-INSERT-MSG SECTION.                                                  
158100                                                                          
158200*    IF MSGI-IDLAND-SPR = 'SE'                                            
158300*      MOVE '0' TO MFS-KDHUVOMR                                           
158400*    END-IF                                                               
158500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
158600     MOVE SPACE TO GOOD-STATUSCODES                                       
158700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
158800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
158900     PERFORM IMS-STATUSCHECK                                              
159000     .                                                                    
159100     EJECT                                                                
159200 IMS-ISRT-MSG-ALT SECTION.                                                
159300                                                                          
159400     MOVE SPACE TO GOOD-STATUSCODES                                       
159500     CALL CBLTDLI USING ISRT ALT-PCB P-TO-P-SW                            
159600     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
159700     PERFORM IMS-STATUSCHECK                                              
159800     .                                                                    
159900     SKIP3                                                                
160000 IMS-GHU-WDE201 SECTION.                                                  
160100                                                                          
160200     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
160300          DELIMITED BY SIZE INTO SSA1                                     
160400     MOVE '  ' TO GOOD-STATUSCODES                                        
160500     CALL CBLTDLI USING GHU WDE2-PCB DLI-IO-WDE201 SSA1                   
160600     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
160700     PERFORM IMS-STATUSCHECK                                              
160800     .                                                                    
160900     SKIP3                                                                
161000 IMS-GHNP-WDE211 SECTION.                                                 
161100                                                                          
161200     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
161300          DELIMITED BY SIZE INTO SSA1                                     
161400     STRING 'WDE211  (WDE211KY =' W-WDE211KY-X ')'                        
161500          DELIMITED BY SIZE INTO SSA2                                     
161600     MOVE '  GE' TO GOOD-STATUSCODES                                      
161700     CALL CBLTDLI USING GHNP WDE2-PCB DLI-IO-WDE211 SSA1 SSA2             
161800     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
161900     PERFORM IMS-STATUSCHECK                                              
162000     .                                                                    
162100     SKIP3                                                                
162200 IMS-GHU-WDE211 SECTION.                                                  
162300                                                                          
162400     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
162500          DELIMITED BY SIZE INTO SSA1                                     
162600     STRING 'WDE211  (WDE211KY =' W-WDE211KY-X ')'                        
162700          DELIMITED BY SIZE INTO SSA2                                     
162800     MOVE '  GE' TO GOOD-STATUSCODES                                      
162900     CALL CBLTDLI USING GHU WDE2-PCB DLI-IO-WDE211 SSA1 SSA2              
163000     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
163100     PERFORM IMS-STATUSCHECK                                              
163200     .                                                                    
163300     SKIP3                                                                
163400 IMS-GHU-WDE211-DIST SECTION.                                             
163500                                                                          
163600     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
163700          DELIMITED BY SIZE INTO SSA1                                     
163800     STRING 'WDE211  (WDE211KY>=' W-WDE211KY-MIN                          
163900                    '&WDE211KY<=' W-WDE211KY-MAX ')'                      
164000          DELIMITED BY SIZE INTO SSA2                                     
164100     MOVE '  GE' TO GOOD-STATUSCODES                                      
164200     CALL CBLTDLI USING GHU WDE2-PCB DLI-IO-WDE211 SSA1 SSA2              
164300     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
164400     PERFORM IMS-STATUSCHECK                                              
164500     .                                                                    
164600     SKIP3                                                                
164700 IMS-ISRT-WDE201 SECTION.                                                 
164800                                                                          
164900     MOVE 'WDE201  ' TO SSA1                                              
165000     MOVE '  ' TO GOOD-STATUSCODES                                        
165100     CALL CBLTDLI USING ISRT WDE2-PCB DLI-IO-WDE201 SSA1                  
165200     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
165300     PERFORM IMS-STATUSCHECK                                              
165400     .                                                                    
165500     EJECT                                                                
165600                                                                          
165700 IMS-ISRT-WDE211 SECTION.                                                 
165800                                                                          
165900     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
166000          DELIMITED BY SIZE INTO SSA1                                     
166100     MOVE 'WDE211 ' TO SSA2                                               
166200     MOVE '  II' TO GOOD-STATUSCODES                                      
166300     CALL CBLTDLI USING ISRT WDE2-PCB DLI-IO-WDE211 SSA1 SSA2             
166400     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
166500     PERFORM IMS-STATUSCHECK                                              
166600     .                                                                    
166700     SKIP3                                                                
166800                                                                          
166900 IMS-REPL-WDE211 SECTION.                                                 
167000                                                                          
167100     MOVE '  ' TO GOOD-STATUSCODES                                        
167200     CALL CBLTDLI USING REPL WDE2-PCB DLI-IO-WDE211                       
167300     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
167400     PERFORM IMS-STATUSCHECK                                              
167500     .                                                                    
167600     EJECT                                                                
167700 IMS-GU-WDGX4482   SECTION.                                               
167800                                                                          
167900     STRING 'WDR401  *P(WDGXKEY  =' W-4479-X ')'                          
168000                      DELIMITED BY SIZE INTO SSA1                         
168100     STRING 'WDGX4482(WDGXKEY >=' W-4482-MIN                              
168200                    '&WDGXKEY <=' W-4482-MAX  ')'                         
168300                      DELIMITED BY SIZE INTO SSA2                         
168400     MOVE '  GE' TO GOOD-STATUSCODES                                      
168500     CALL CBLTDLI USING GU 4479-PCB DLI-IO-WDGX4482 SSA1 SSA2             
168600     MOVE 4479-STATUS-CODE TO STATUS-WS                                   
168700     PERFORM IMS-STATUSCHECK                                              
168800     .                                                                    
168900     SKIP2                                                                
169000 IMS-GNP-WDGX4482   SECTION.                                              
169100                                                                          
169200     STRING 'WDR401  (WDGXKEY  =' W-4479-X ')'                            
169300                      DELIMITED BY SIZE INTO SSA1                         
169400     STRING 'WDGX4482(WDGXKEY >=' W-4482-MIN                              
169500                    '&WDGXKEY <=' W-4482-MAX  ')'                         
169600                      DELIMITED BY SIZE INTO SSA2                         
169700     MOVE '  GE' TO GOOD-STATUSCODES                                      
169800     CALL CBLTDLI USING GNP 4479-PCB DLI-IO-WDGX4482 SSA1 SSA2            
169900     MOVE 4479-STATUS-CODE TO STATUS-WS                                   
170000     PERFORM IMS-STATUSCHECK                                              
170100     .                                                                    
170200     SKIP2                                                                
170300                                                                          
170400 IMS-GU-WDGX4496   SECTION.                                               
170500                                                                          
170600     STRING 'WDR401  *P(WDGXKEY  =' W-4495-X ')'                          
170700                      DELIMITED BY SIZE INTO SSA1                         
170800     MOVE   'WDGX4496 '                   TO SSA2                         
170900     MOVE '  GE' TO GOOD-STATUSCODES                                      
171000     CALL CBLTDLI USING GU 4495-PCB DLI-IO-WDGX4496 SSA1 SSA2             
171100     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
171200     PERFORM IMS-STATUSCHECK                                              
171300     .                                                                    
171400 IMS-GU-WDB601    SECTION.                                                
171500     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
171600          DELIMITED BY SIZE INTO SSA1                                     
171700     MOVE '  GE' TO GOOD-STATUSCODES                                      
171800     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
171900     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
172000     PERFORM IMS-STATUSCHECK                                              
172100     IF SEGMENT-MISSING                                                   
172200         MOVE SPACE TO DCS-KDDC                                           
172300                       DCS-IDLANDX2                                       
172400     END-IF                                                               
172500     .                                                                    
172600 IMS-STATUSCHECK SECTION.                                                 
172700                                                                          
172800     SET STATUS-IX TO 1                                                   
172900     SEARCH GOOD-STATUS                                                   
173000       AT END                                                             
173100         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
173200         DELIMITED BY SIZE INTO ERROR-TEXT                                
173300         CALL FELLOG                                                      
173400       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
173500         CONTINUE                                                         
173600     END-SEARCH                                                           
173700     .                                                                    
