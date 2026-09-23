000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4067600.                                                
000300 AUTHOR.         KARANDE DIGAMBAR.                                        
000400 DATE-WRITTEN.   02/09/03.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        THIS PROGRAM IS STARTED BY 4675 SCREEN TO UPDATE THE             
001000*        DETAILS IN WDE211 AND WDE122 SEGMENT.                            
001100*                                                                         
001200*        THE PROGRAM UPDATES   WDE1                                       
001300*        THE PROGRAM UPDATES   WDE2                                       
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSACTION: W4T676                                              
001700*        MID:         W4I67601                                            
001800*                                                                         
001900*    OUTDATA.                                                             
002000*        MOD:         W4O67601                                            
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400                                                                          
002500 DATA DIVISION.                                                           
002600     EJECT                                                                
002700 WORKING-STORAGE SECTION.                                                 
002800 77  IDPGM                       PIC X(08)   VALUE 'W4067600'.            
002900                                                                          
003000 77  IDLAND-INDX                 PIC S9(4)   VALUE +0   COMP SYNC.        
003100 77  MAX-IDLAND-INDX             PIC S9(4)   VALUE +23  COMP SYNC.        
003200*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003300 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
003400                                                                          
003500 77  YES                         PIC X       VALUE 'J'.                   
003600 77  NOO                         PIC X       VALUE 'N'.                   
003700                                                                          
003800 77  WS-CDC-11                   PIC X(2)    VALUE '11'.                  
003900                                                                          
004000 77  W-IDDC                      PIC X(02)   VALUE SPACE.                 
004100 77  W-IDDISTR                   PIC 9(04)   VALUE ZERO COMP-3.           
004200 77  W-FLFARLIG                  PIC X(01)   VALUE SPACE.                 
004300 77  W-IDLANDX2                  PIC X(2)    VALUE SPACE.                 
004400 77  W-KDVALUTA                  PIC 9(2)    VALUE ZERO.                  
004500 77  W-9KOMPL                    PIC  9(7)   VALUE 9999999.               
004600 77  W-DCS-IDPARTNR-EXP          PIC  X(9)   VALUE SPACE.                 
004700 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
004800 77  WS-KDVALISO-HUV             PIC X(3)    VALUE 'SEK'.                 
004900 77  WS-BESLULEV                 PIC 9(5)   VALUE ZERO.                   
005000                                                                          
005100*01  -COPY WWDC99                                                         
005200*01  -COPY WWDCKONS                                                       
005300                                                                          
005400 01  WS-TID-X.                                                            
005500     03 WS-TISKPTID                PIC 9(6)       VALUE ZERO.             
005600     03 WS-TISKPTID-GRP            REDEFINES WS-TISKPTID.                 
005700       05 WS-TISKPTID-HHMM         PIC 9(4).                              
005800       05 WS-TISKPTID-SS           PIC 9(2).                              
005900                                                                          
006000 01  W-IDDCTEXT-MSGI.                                                     
006100     03  FILLER                  PIC X(5)   VALUE 'WIDDC'.                
006200     03  W-IDDC-MSGI             PIC X(2).                                
006300                                                                          
006400 77  FLSAMFAK-SW                 PIC X       VALUE 'N'.                   
006500     88  FLSAMFAK                            VALUE 'J'.                   
006600                                                                          
006700 77  WDE122-ISRT-SW              PIC X       VALUE 'N'.                   
006800     88  WDE122-ISRT                         VALUE 'J'.                   
006900                                                                          
007000 77  WDE211-ISRT-SW              PIC X       VALUE 'N'.                   
007100     88  WDE211-ISRT                         VALUE 'J'.                   
007200                                                                          
007300 77  PRFRAKT-IFYLLES-SW          PIC X       VALUE 'J'.                   
007400     88  PRFRAKT-OBLIGATORISK                VALUE 'J'.                   
007500     88  PRFRAKT-ROER-EJ                     VALUE 'N'.                   
007600                                                                          
007700*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
007800                                                                          
007900 77  MSG-KVLL-TILL-WHELP         PIC S9(4)  VALUE +85   COMP SYNC.        
008000                                                                          
008100 77  INDATA-SW                   PIC X       VALUE 'J'.                   
008200     88  INDATA-OK                           VALUE 'J'.                   
008300     88  INDATA-WRONG                        VALUE 'N'.                   
008400                                                                          
008500 77  KEYS-SW                     PIC X       VALUE 'J'.                   
008600     88  KEYS-OK                             VALUE 'J'.                   
008700     88  KEYS-WRONG                          VALUE 'N'.                   
008800                                                                          
008900 77  UPDATE-SAVE-SW              PIC X       VALUE 'N'.                   
009000     88  UPDATE-SAVE                         VALUE 'J'.                   
009100                                                                          
009200 77  INPUT-111-SW                PIC X       VALUE 'N'.                   
009300     88  INPUT-111                           VALUE 'J'.                   
009400                                                                          
009500 77  INPUT-122-SW                PIC X       VALUE 'N'.                   
009600     88  INPUT-122                           VALUE 'J'.                   
009700                                                                          
009800 77  INPUT-211-SW                PIC X       VALUE 'N'.                   
009900     88  INPUT-211                           VALUE 'J'.                   
010000                                                                          
010100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
010200     88  OWN-MID                             VALUE '4676'.                
010300     88  GOOD-MID                            VALUE '4675'                 
010400                                                   '4676'.                
010500     88  4675-MID                            VALUE '4675'.                
010600     88  HELP-MID                            VALUE '0551'.                
010700     EJECT                                                                
010800                                                                          
010900*    --- PARAMETRAR TILL W930VAL                                          
011000*01 -COPY W930VAL                                                         
011100     EJECT                                                                
011200 01  TEST-IDDISTR               PIC S9(05)   VALUE ZERO  COMP-3.          
011300*01  FILLER  -COPY  WWDIST79  -RED  TEST-IDDISTR                          
011400*01  FILLER  -COPY  WWDIST35  -RED  TEST-IDDISTR                          
011500*01  FILLER  -COPY  WWDIST07  -RED  TEST-IDDISTR                          
011600*01  FILLER  -COPY  WWDIST92  -RED  TEST-IDDISTR                          
011700                                                                          
011800*   --- SUBPROGRAMS AND PARAMETER AREAS                                   
011900 01  GENERAL-SUBPROGRAMS.                                                 
012000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
012100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
012200     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
012300     03  W476SHNO                PIC X(8)    VALUE 'W476SHNO'.            
012400     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
012500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012700     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
012800     EJECT                                                                
012900*    --- PARAMETERS FOR SUBPROGRAM W510CURR                               
013000*01 -COPY W510CURR                                                        
013100*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
013200*01 -COPY WMEDAREA                                                        
013300     SKIP3                                                                
013400 01  FILLER.                                                              
013500   03  FELMEDD-AREA.                                                      
013600     05  FELMEDD-ENGLISH.                                                 
013700       10  FILLER                PIC X(40)                                
013800           VALUE '622 4676 WRONG PICTURE SELECTED         '.              
013900                                                                          
014000 01  MESSAGE-CODES.                                                       
014100     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
014200     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
014300     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
014400     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
014500     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
014600     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
014700     03  ERR-START-FROM-FIRST    PIC X(3)    VALUE '754'.                 
014800     03  ERR-INFO-MISSING        PIC X(3)    VALUE '760'.                 
014900     EJECT                                                                
015000*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
015100*                                                                         
015200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
015300     SKIP3                                                                
015400*01 -COPY WMSGINIT                                                        
015500                                                                          
015600 01  FILLER                      PIC X(16)   VALUE 'DC-WMSGINIT'.         
015700     SKIP3                                                                
015800*01 -COPY WMSGINIT     -PRE DC-                                           
015900                                                                          
016000*    --- PARAMETERS FOR SUB PROGRAM WDECEDIT                              
016100*                                                                         
016200 01  FILLER                      PIC X(16)   VALUE 'WDECEDIT'.            
016300*01 -COPY WDECAREA                                                        
016400     EJECT                                                                
016500*    --- AREA  FOR W476SHNO ---                                           
016600 01  FILLER                      PIC X(16)   VALUE 'W476SHNO'.            
016700*01 -COPY W476SHNO                                                        
016800                                                                          
016900*    --- AREA  FOR WDATKONV ---                                           
017000 01  FILLER                      PIC X(16)   VALUE 'WDATAREA'.            
017100*01   -COPY WDATAREA                                                      
017200                                                                          
017300*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
017400*                                                                         
017500 01  SAVE-AREA.                                                           
017600     03  SAVE-IDTRANS            PIC X(4)    VALUE '4676'.                
017700     03  SAVE-4675-4676.                                                  
017800         05  SAVE-IDDISTR-RETURN     PIC S9(5).                           
017900         05  SAVE-IDKUNDNR-RETURN    PIC S9(7).                           
018000         05  SAVE-KDFAKTYP-RETURN    PIC X(01).                           
018100         05  SAVE-IDKUNDRF-RETURN    PIC X(10).                           
018200         05  SAVE-IDPRODNR-RETURN    PIC S9(07).                          
018300         05  SAVE-IDKOLLI-RETURN     PIC S9(05).                          
018400         05  SAVE-IDDISTR-4676       PIC S9(5).                           
018500         05  SAVE-IDKUNDNR-4676      PIC S9(7).                           
018600         05  SAVE-FLSAMFAK           PIC X(01)   VALUE SPACE.             
018700         05  SAVE-IDSHIPM            PIC  9(7).                           
018800         05  SAVE-FLFARLIG           PIC X(01).                           
018900         05  SAVE-FLSKRIV-NU         PIC X(01).                           
019000         05  SAVE-FLAVSLUTA          PIC X(01).                           
019100         05  SAVE-REL-TRANSPORT      PIC X(01)   VALUE 'N'.               
019200             88  REL-TRANSPORT                   VALUE 'J'.               
019300                                                                          
019400     03  SAVE-PRFRAKT            PIC S9(7)V9(2)      COMP-3.              
019500     03  SAVE-PRFOERS            PIC S9(7)V9(2)      COMP-3.              
019600     03  SAVE-PRKURS-MAN         PIC S9(6)V9(5)      COMP-3.              
019700     03  SAVE-PRLEGKST           PIC S9(7)V9(2)      COMP-3.              
019800     03  SAVE-RELEGKST           PIC S9(2)V9(1)      COMP-3.              
019900     03  SAVE-PREMBHNT           PIC S9(7)V9(2)      COMP-3.              
020000     03  SAVE-REEMBHNT           PIC S9(2)V9(1)      COMP-3.              
020100     03  SAVE-PRAVDRAG           PIC S9(7)V9(2)      COMP-3.              
020200     03  SAVE-REAVDRAG           PIC S9(2)V9(1)      COMP-3.              
020300     03  SAVE-REFOERS            PIC S9(2)V9(3)      COMP-3.              
020400     03  SAVE-REOVKOFF           PIC S9(2)V9(1)      COMP-3.              
020500                                                                          
020600*                                                                         
020700     EJECT                                                                
020800*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
020900*                                                                         
021000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
021100     SKIP3                                                                
021200*01  MID -COPY W4I67601                                                   
021300     EJECT                                                                
021400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
021500     SKIP3                                                                
021600*01  -COPY WMSGAREA                                                       
021700     EJECT                                                                
021800*   TO RETURN TO MAIN MENU                                                
021900*    03  FILLER  -COPY W0O50401  -PRE MOD0504-  -RED MSG-AREA.            
022000     EJECT                                                                
022100     03  MOD REDEFINES MSG-AREA.                                          
022200*      05  -COPY W4O67601                                                 
022300     EJECT                                                                
022400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
022500     SKIP3                                                                
022600*01  -COPY WMFSAREA                                                       
022700     EJECT                                                                
022800 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-AREA'.         
022900 01  P-TO-P-SW.                                                           
023000                                                                          
023100     03  P-TO-P-KVLL                PIC S9(4)           COMP SYNC.        
023200     03  P-TO-P-KDZ1                PIC X(1)  VALUE LOW-VALUE.            
023300     03  P-TO-P-KDZ2                PIC X(1)  VALUE LOW-VALUE.            
023400     03  P-TO-P-KDTRANS             PIC X(8).                             
023500     03  P-TO-P-IDTRANS             PIC X(4).                             
023600     03  P-TO-P-KDMFSFOR            PIC X(1).                             
023700     03  P-TO-P-DATA.                                                     
023800*        05 -COPY W4I67501   -PRE PTOP-                                   
023900*                                                                         
024000*    --- WORK-AREAS FOR IMS-SECTIONS                                      
024100*                                                                         
024200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
024300     SKIP3                                                                
024400 01  KEYS-TO-DLI.                                                         
024500     03  W-IDSHIPM-X.                                                     
024600         05  W-IDSHIPM           PIC  9(07)  VALUE ZERO.                  
024700                                                                          
024800     03  W-WDE211KY-X.                                                    
024900         05  W-WDE211-IDDISTR    PIC S9(05)  VALUE ZERO COMP-3.           
025000         05  W-WDE211-IDKUNDNR   PIC S9(07)  VALUE ZERO COMP-3.           
025100                                                                          
025200     03  W-WDE211KY-MIN.                                                  
025300         05  W-WDE211-IDDISTR-MIN                                         
025400                                 PIC S9(05)  VALUE ZERO COMP-3.           
025500         05  FILLER              PIC  X(4)   VALUE LOW-VALUES.            
025600                                                                          
025700     03  W-WDE211KY-MAX.                                                  
025800         05  W-WDE211-IDDISTR-MAX                                         
025900                                 PIC S9(05)  VALUE ZERO COMP-3.           
026000         05  FILLER              PIC  X(4)   VALUE HIGH-VALUES.           
026100                                                                          
026200     03  W-WDE111KY-X.                                                    
026300         05  W-WDE111-IDDISTR    PIC S9(05)  VALUE ZERO COMP-3.           
026400         05  W-WDE111-IDKUNDNR   PIC S9(07)  VALUE ZERO COMP-3.           
026500                                                                          
026600     03  W-WDE111KY-MIN.                                                  
026700         05  W-WDE111-IDDISTR-MIN                                         
026800                                 PIC S9(05)  VALUE ZERO COMP-3.           
026900         05  FILLER              PIC  X(4)   VALUE LOW-VALUES.            
027000                                                                          
027100     03  W-WDE111KY-MAX.                                                  
027200         05  W-WDE111-IDDISTR-MAX                                         
027300                                 PIC S9(05)  VALUE ZERO COMP-3.           
027400         05  FILLER              PIC  X(4)   VALUE HIGH-VALUES.           
027500                                                                          
027600     03  W-KDSEGKEY-X.                                                    
027700         05  W-KDSEGKEY          PIC X(01)   VALUE '1'.                   
027800                                                                          
027900     03  W-4495-X.                                                        
028000         05  W-IDHTR             PIC X(4)    VALUE '4495'.                
028100         05  W-IDDC-4495         PIC X(2)    VALUE SPACE.                 
028200         05  W-IDTRPTNR          PIC S9(3)   VALUE ZERO   COMP-3.         
028300         05  W-IDLBBET           PIC X(12)   VALUE SPACE.                 
028400         05  FILLER              PIC X(10)   VALUE LOW-VALUE.             
028500                                                                          
028600     03  W-4498-MIN.                                                      
028700         05  W-4498-IDDISTR-MIN  PIC S9(05)  COMP-3.                      
028800         05  W-4498-IDKUNDNR-MIN PIC S9(07)  COMP-3.                      
028900         05  FILLER              PIC  X(18)  VALUE LOW-VALUE.             
029000                                                                          
029100     03  W-4498-MAX.                                                      
029200         05  W-4498-IDDISTR-MAX  PIC S9(05)  COMP-3.                      
029300         05  W-4498-IDKUNDNR-MAX PIC S9(07)  COMP-3.                      
029400         05  FILLER              PIC  X(18)  VALUE HIGH-VALUE.            
029500                                                                          
029600     03  W-IDDC-B6-X.                                                     
029700         05 W-IDDC-B6                  PIC X(2).                          
029800                                                                          
029900     SKIP2                                                                
030000*    --- STATUS-KOD FRÅN IMS                                              
030100 01  STATUS-WS                   PIC XX.                                  
030200     88  SEGMENT-FOUND                       VALUE '  '.                  
030300     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
030400     88  SEGMENT-MISSING                     VALUE 'GE'.                  
030500     SKIP2                                                                
030600 01  GOOD-STATUSCODES.                                                    
030700     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
030800     SKIP3                                                                
030900 01  SSA1                        PIC X(128).                              
031000 01  SSA2                        PIC X(128).                              
031100 01  SSA3                        PIC X(128).                              
031200     EJECT                                                                
031300*    --- IMS FUNCTION CODES                                               
031400*01  -COPY W0003                                                          
031500     EJECT                                                                
031600*    ---  DLI INPUT-OUTPUT AREA                                           
031700                                                                          
031800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE101'.                      
031900 01  DLI-IO-WDE101.                                                       
032000*    03  -COPY WDE101                                                     
032100                                                                          
032200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE111'.                      
032300 01  DLI-IO-WDE111.                                                       
032400*    03  -COPY WDE111                                                     
032500                                                                          
032600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE122'.                      
032700 01  DLI-IO-WDE122.                                                       
032800*    03  -COPY WDE122                                                     
032900                                                                          
033000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE201'.                      
033100 01  DLI-IO-WDE201.                                                       
033200*    03  -COPY WDE201                                                     
033300                                                                          
033400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE211'.                      
033500 01  DLI-IO-WDE211.                                                       
033600*    03  -COPY WDE211                                                     
033700                                                                          
033800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4496'.                    
033900 01  DLI-IO-WDGX4496.                                                     
034000*    03  -COPY WDGX4496                                                   
034100                                                                          
034200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4498'.                    
034300 01  DLI-IO-WDGX4498.                                                     
034400*    03  -COPY WDGX4498                                                   
034500                                                                          
034600 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
034700 01   DLI-IO-AREA-B601.                                                   
034800*     03  -COPY WDB601                                                    
034900                                                                          
035000     EJECT                                                                
035100 LINKAGE SECTION.                                                         
035200*01  -COPY W0009   -PRE MSG-                                              
035300*01  -COPY W0009   -PRE ALT-                                              
035400*01  -COPY W0008   -PRE WDP7-                                             
035500     05  FILLER                  PIC X.                                   
035600                                                                          
035700*01  -COPY W0008   -PRE 4495-                                             
035800     05  FILLER                  PIC X.                                   
035900                                                                          
036000*01  -COPY W0008  -PRE WDE1-                                              
036100     05  FILLER                  PIC X.                                   
036200                                                                          
036300*01  -COPY W0008  -PRE WDE2-                                              
036400     05  FILLER                  PIC X.                                   
036500*01  -COPY W0008  -PRE WDG2-                                              
036600     05  FILLER                  PIC X.                                   
036700*01  -COPY W0008  -PRE WDB6-                                              
036800     05  FILLER                  PIC X.                                   
036900 01  SHNO-4517-PCB               PIC X.                                   
037000     EJECT                                                                
037100 PROCEDURE DIVISION  USING MSG-PCB  ALT-PCB  WDP7-PCB  4495-PCB           
037200                           WDE1-PCB WDE2-PCB WDG2-PCB                     
037300                           WDB6-PCB                                       
037400                           SHNO-4517-PCB.                                 
037500                                                                          
037600 MAIN SECTION.                                                            
037700     ENTRY 'DLITCBL' USING MSG-PCB  ALT-PCB  WDP7-PCB  4495-PCB           
037800                           WDE1-PCB WDE2-PCB WDB6-PCB                     
037900                           SHNO-4517-PCB.                                 
038000                                                                          
038100     PERFORM IMS-GET-MSG                                                  
038200     IF SEGMENT-FOUND                                                     
038300       PERFORM A-INIT                                                     
038400       IF GOOD-MID                                                        
038500         PERFORM B-CHECK-KEYS                                             
038600       END-IF                                                             
038700       IF KEYS-OK                                                         
038800         IF MFS-UPDATE                                                    
038900           PERFORM G-CHECK-INPUT                                          
039000           IF INDATA-OK                                                   
039100             PERFORM H-UPDATE                                             
039200           END-IF                                                         
039300         ELSE                                                             
039400           IF MFS-RETURN                                                  
039500             PERFORM I-PF3-RETURN                                         
039600           ELSE                                                           
039700             IF MFS-FIRST                                                 
039800               PERFORM C-FIRST-PAGE                                       
039900             ELSE                                                         
040000               PERFORM E-SAME-PAGE                                        
040100             END-IF                                                       
040200           END-IF                                                         
040300         END-IF                                                           
040400         IF DIST79-ECOM-PRICE                                             
040500           PERFORM J-CLOSE-FIELDS                                         
040600         END-IF                                                           
040700         IF INDATA-OK AND NOT MFS-RETURN                                  
040800           PERFORM F-READ-SHOW-INFO                                       
040900         END-IF                                                           
041000       END-IF                                                             
041100*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
041200*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
041300       IF UPDATE-SAVE                                                     
041400         MOVE '002'       TO MSGI-KDCALL                                  
041500         MOVE '4676'      TO SAVE-IDTRANS                                 
041600         MOVE SAVE-AREA   TO MSGI-SPAR-AREA                               
041700         CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                       
041800       END-IF                                                             
041900       IF INDATA-OK AND MFS-RETURN                                        
042000         PERFORM IMS-ISRT-MSG-ALT                                         
042100       ELSE                                                               
042200         IF GOOD-MID                                                      
042300           COMPUTE MSG-KVLL = LENGTH OF MOD-W4O67601 + 4                  
042400           PERFORM IMS-INSERT-MSG                                         
042500         END-IF                                                           
042600       END-IF                                                             
042700     END-IF                                                               
042800                                                                          
042900     MOVE ZERO TO RETURN-CODE                                             
043000     GOBACK                                                               
043100     .                                                                    
043200     EJECT                                                                
043300 A-INIT SECTION.                                                          
043400                                                                          
043500     IF MSG-DOUBLE-TRANSACTIONS                                           
043600       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W4I67601                 
043700       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
043800       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
043900     ELSE                                                                 
044000       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W4I67601                  
044100       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
044200       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
044300     END-IF                                                               
044400                                                                          
044500     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
044600     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
044700     MOVE MFS-IDTRANS TO W-IDTRANS                                        
044800                                                                          
044900     MOVE LOW-VALUE TO MSG-AREA                                           
045000     MOVE 'W4O676N1' TO MFS-IDMOD                                         
045100     MOVE '4676' TO MOD-IDTRANS                                           
045200     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
045300                                                                          
045400     IF OWN-MID                                                           
045500       CONTINUE                                                           
045600     ELSE                                                                 
045700       MOVE SPACE TO MFS-KDTRTYP                                          
045800       MOVE '7' TO MFS-IDPFK                                              
045900     END-IF                                                               
046000                                                                          
046100     IF NOT GOOD-MID                                                      
046200       MOVE NOO  TO KEYS-SW                                               
046300                    INDATA-SW                                             
046400       PERFORM S10-WRONG-PICTURE-MESSAGE                                  
046500     END-IF                                                               
046600                                                                          
046700     MOVE FUNCTION CURRENT-DATE (3:2) TO W-DATE-AAMM(1:2)                 
046800     MOVE FUNCTION CURRENT-DATE (5:2) TO W-DATE-AAMM(3:2)                 
046900                                                                          
047000     .                                                                    
047100     EJECT                                                                
047200 B-CHECK-KEYS SECTION.                                                    
047300                                                                          
047400     MOVE ALL '+'           TO MSGI-WMSGINIT                              
047500     MOVE '001'             TO MSGI-KDCALL                                
047600     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
047700     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
047800     MOVE '4676'            TO MSGI-IDTRANS                               
047900     IF GOOD-MID                                                          
048000       MOVE MID-IDTRPTNR-IN TO MSGI-IDTRPTNR                              
048100       MOVE MID-IDLBBET-IN  TO MSGI-IDLBBET                               
048200       MOVE MID-IDDC-IN     TO MSGI-IDDC-KEY                              
048300     END-IF                                                               
048400                                                                          
048500     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
048600                                                                          
048700     IF 4675-MID                                                          
048800       MOVE MID-IDTRPTNR-IN TO MSGI-IDTRPTNR                              
048900       MOVE MID-IDLBBET-IN  TO MSGI-IDLBBET                               
049000       MOVE MID-IDDC-IN     TO MSGI-IDDC-KEY                              
049100     END-IF                                                               
049200                                                                          
049300     IF OWN-MID                                                           
049400       MOVE MID-FLFRAKT-UPD TO MOD-FLFRAKT-UPD                            
049500     ELSE                                                                 
049600       MOVE NOO              TO MOD-FLFRAKT-UPD                           
049700     END-IF                                                               
049800                                                                          
049900     IF GOOD-MID                                                          
050000       IF MID-IDTRPTNR-IN NOT  = ALL '+'                                  
050100         MOVE '7'           TO MFS-IDPFK                                  
050200         MOVE SPACE         TO MFS-KDTRTYP                                
050300       END-IF                                                             
050400       IF MID-IDLBBET-IN NOT   = ALL '+'                                  
050500         MOVE '7'           TO MFS-IDPFK                                  
050600         MOVE SPACE         TO MFS-KDTRTYP                                
050700       END-IF                                                             
050800       IF MID-IDDC-IN NOT      = ALL '+'                                  
050900         MOVE '7'           TO MFS-IDPFK                                  
051000         MOVE SPACE         TO MFS-KDTRTYP                                
051100       END-IF                                                             
051200       IF MID-FLFARLIG-IN NOT  = ALL '+'                                  
051300         MOVE '7'           TO MFS-IDPFK                                  
051400         MOVE SPACE         TO MFS-KDTRTYP                                
051500       END-IF                                                             
051600     END-IF                                                               
051700                                                                          
051800     MOVE MSGI-SPAR-AREA    TO SAVE-AREA                                  
051900                                                                          
052000     IF MFS-FIRST                                                         
052100       MOVE SPACE           TO SAVE-FLAVSLUTA                             
052200                               SAVE-REL-TRANSPORT                         
052300       MOVE YES             TO UPDATE-SAVE-SW                             
052400     END-IF                                                               
052500                                                                          
052600                                                                          
052700*    - LANGUAGE TO BE USED BY MEDKONV                                     
052800     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
052900                                                                          
053000     MOVE YES TO KEYS-SW                                                  
053100                                                                          
053200     MOVE MFS-ERASE-FIELD   TO MOD-IDTRPTNR-IN                            
053300                               MOD-IDLBBET-IN                             
053400                               MOD-FLFARLIG-IN                            
053500                               MOD-IDDC-IN                                
053600                                                                          
053700     IF MSGI-IDTRPTNR NUMERIC                                             
053800        IF MSGI-IDTRPTNR > ZERO                                           
053900           MOVE MSGI-IDTRPTNR   TO  W-IDTRPTNR                            
054000        ELSE                                                              
054100          MOVE NOO     TO KEYS-SW                                         
054200        END-IF                                                            
054300     ELSE                                                                 
054400        MOVE NOO       TO KEYS-SW                                         
054500     END-IF                                                               
054600                                                                          
054700     IF MSGI-IDLBBET = ALL '+'                                            
054800        MOVE NOO           TO KEYS-SW                                     
054900     ELSE                                                                 
055000       IF MSGI-IDLBBET > SPACE                                            
055100          MOVE MSGI-IDLBBET  TO W-IDLBBET                                 
055200       ELSE                                                               
055300          MOVE NOO           TO KEYS-SW                                   
055400       END-IF                                                             
055500     END-IF                                                               
055600                                                                          
055700     IF MID-FLFARLIG-IN NOT = ALL '+'                                     
055800       MOVE MID-FLFARLIG-IN    TO W-FLFARLIG                              
055900     ELSE                                                                 
056000       IF SAVE-FLFARLIG > SPACE                                           
056100         MOVE SAVE-FLFARLIG    TO W-FLFARLIG                              
056200       END-IF                                                             
056300     END-IF                                                               
056400                                                                          
056500     IF W-FLFARLIG NOT = SAVE-FLFARLIG                                    
056600       MOVE W-FLFARLIG     TO SAVE-FLFARLIG                               
056700       MOVE YES            TO UPDATE-SAVE-SW                              
056800     END-IF                                                               
056900                                                                          
057000     MOVE SAVE-IDDISTR-4676    TO TEST-IDDISTR                            
057100     IF DIST35-NONVCC-NONVCC-REFILL                                       
057200       MOVE WC-CDC-SE          TO W-IDDC-B6                               
057300       PERFORM IMS-GU-WDB601                                              
057400       MOVE DCS-IDPARTNR       TO W-DCS-IDPARTNR-EXP                      
057500     END-IF                                                               
057600                                                                          
057700     MOVE MSGI-IDDC-KEY        TO W-IDDC-B6                               
057800     PERFORM IMS-GU-WDB601                                                
057900     IF DCS-KDDC NOT = SPACE                                              
058000       MOVE MSGI-IDDC-KEY  TO W-IDDC                                      
058100                              W-IDDC-4495                                 
058200       IF DCS-DDC                                                         
058300         MOVE WS-CDC-11        TO W-IDDC-MSGI                             
058400       ELSE                                                               
058500         MOVE MSGI-IDDC-KEY    TO W-IDDC-MSGI                             
058600       END-IF                                                             
058700                                                                          
058800       MOVE ALL '+'            TO DC-MSGI-WMSGINIT                        
058900       MOVE '001'              TO DC-MSGI-KDCALL                          
059000       MOVE W-IDDCTEXT-MSGI    TO DC-MSGI-IDUSER                          
059100       MOVE '4676'             TO DC-MSGI-IDTRANS                         
059200       MOVE MSG-LTERM-NAME     TO DC-MSGI-IDLTERM-USER                    
059300       CALL W005INIT        USING DC-MSGI-WMSGINIT WDP7-PCB               
059400                                                                          
059500     ELSE                                                                 
059600       MOVE NOO                TO KEYS-SW                                 
059700     END-IF                                                               
059800                                                                          
059900     MOVE SAVE-FLSAMFAK        TO FLSAMFAK-SW                             
060000     MOVE SAVE-IDSHIPM         TO W-IDSHIPM                               
060100                                                                          
060200     IF GOOD-MID OR KEYS-OK                                               
060300       MOVE MSGI-IDTRPTNR       TO MOD-IDTRPTNR-UT                        
060400       MOVE MSGI-IDLBBET        TO MOD-IDLBBET-UT                         
060500       MOVE MSGI-IDDC-KEY       TO MOD-IDDC-UT                            
060600       MOVE SAVE-FLFARLIG       TO MOD-FLFARLIG-UT                        
060700       MOVE SAVE-IDDISTR-4676   TO MOD-IDDISTR                            
060800                                   W-IDDISTR                              
060900                                   TEST-IDDISTR                           
061000                                   W-4498-IDDISTR-MIN                     
061100                                   W-4498-IDDISTR-MAX                     
061200                                   W-WDE211-IDDISTR                       
061300                                   W-WDE211-IDDISTR-MIN                   
061400                                   W-WDE211-IDDISTR-MAX                   
061500                                   W-WDE111-IDDISTR                       
061600                                   W-WDE111-IDDISTR-MIN                   
061700                                   W-WDE111-IDDISTR-MAX                   
061800       MOVE SAVE-IDKUNDNR-4676  TO MOD-IDKUNDNR                           
061900                                   W-WDE211-IDKUNDNR                      
062000                                   W-WDE111-IDKUNDNR                      
062100     ELSE                                                                 
062200       PERFORM MFS-ERASE-FIELD-OUT                                        
062300     END-IF                                                               
062400                                                                          
062500     IF KEYS-WRONG                                                        
062600       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
062700       CALL WMEDKONV USING MED-WMEDAREA                                   
062800       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
062900       PERFORM MFS-ERASE-FIELD-IN                                         
063000       PERFORM MFS-ERASE-FIELD-OUT                                        
063100     END-IF                                                               
063200     .                                                                    
063300     EJECT                                                                
063400 C-FIRST-PAGE SECTION.                                                    
063500                                                                          
063600     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
063700     CALL WMEDKONV USING MED-WMEDAREA                                     
063800     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
063900                                                                          
064000     PERFORM MFS-ERASE-FIELD-IN                                           
064100     .                                                                    
064200     EJECT                                                                
064300 E-SAME-PAGE SECTION.                                                     
064400                                                                          
064500     IF OWN-MID OR HELP-MID                                               
064600       IF MID-W4I67601 = ALL '+'                                          
064700         PERFORM MFS-ERASE-FIELD-IN                                       
064800       ELSE                                                               
064900         MOVE NOO            TO INDATA-SW                                 
065000         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
065100         CALL WMEDKONV USING MED-WMEDAREA                                 
065200         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
065300         PERFORM MFS-DONT-TOUCH-FIELD-OUT                                 
065400         PERFORM EA-MID-INDATA-TO-MOD                                     
065500       END-IF                                                             
065600     ELSE                                                                 
065700       PERFORM MFS-ERASE-FIELD-IN                                         
065800     END-IF                                                               
065900     .                                                                    
066000     EJECT                                                                
066100 EA-MID-INDATA-TO-MOD SECTION.                                            
066200                                                                          
066300     IF MID-FLSEPINV NOT = ALL '+'                                        
066400       MOVE MFS-ADD-READ-FIELD       TO MOD-FLSEPINV-ATTR                 
066500     END-IF                                                               
066600                                                                          
066700     IF MID-PRFRAKT NOT = ALL '+'                                         
066800       MOVE MFS-ADD-READ-FIELD       TO MOD-PRFRAKT-ATTR                  
066900     END-IF                                                               
067000                                                                          
067100     IF MID-PRFOERS NOT = ALL '+'                                         
067200       MOVE MFS-ADD-READ-FIELD       TO MOD-PRFOERS-ATTR                  
067300     END-IF                                                               
067400                                                                          
067500     IF MID-REFOERS NOT = ALL '+'                                         
067600       MOVE MFS-ADD-READ-FIELD       TO MOD-REFOERS-ATTR                  
067700     END-IF                                                               
067800                                                                          
067900     IF MID-REOVKOFF NOT = ALL '+'                                        
068000       MOVE MFS-ADD-READ-FIELD       TO MOD-REOVKOFF-ATTR                 
068100     END-IF                                                               
068200                                                                          
068300     IF MID-PRLEGKST NOT = ALL '+'                                        
068400       MOVE MFS-ADD-READ-FIELD       TO MOD-PRLEGKST-ATTR                 
068500     END-IF                                                               
068600                                                                          
068700     IF MID-RELEGKST NOT = ALL '+'                                        
068800       MOVE MFS-ADD-READ-FIELD       TO MOD-RELEGKST-ATTR                 
068900     END-IF                                                               
069000                                                                          
069100     IF MID-PREMBHNT NOT = ALL '+'                                        
069200       MOVE MFS-ADD-READ-FIELD       TO MOD-PREMBHNT-ATTR                 
069300     END-IF                                                               
069400                                                                          
069500     IF MID-REEMBHNT NOT = ALL '+'                                        
069600       MOVE MFS-ADD-READ-FIELD       TO MOD-REEMBHNT-ATTR                 
069700     END-IF                                                               
069800                                                                          
069900     IF MID-PRAVDRAG NOT = ALL '+'                                        
070000       MOVE MFS-ADD-READ-FIELD       TO MOD-PRAVDRAG-ATTR                 
070100     END-IF                                                               
070200                                                                          
070300     IF MID-REAVDRAG NOT = ALL '+'                                        
070400       MOVE MFS-ADD-READ-FIELD       TO MOD-REAVDRAG-ATTR                 
070500     END-IF                                                               
070600                                                                          
070700     IF MID-FLCREDL  NOT = ALL '+'                                        
070800       MOVE MFS-ADD-READ-FIELD       TO MOD-FLCREDL-ATTR                  
070900     END-IF                                                               
071000                                                                          
071100     IF MID-KDVALUTA-MAN NOT = ALL '+'                                    
071200       MOVE MFS-ADD-READ-FIELD       TO MOD-KDVALUTA-MAN-ATTR             
071300     END-IF                                                               
071400                                                                          
071500     IF MID-PRKURS-MAN NOT = ALL '+'                                      
071600       MOVE MFS-ADD-READ-FIELD       TO MOD-PRKURS-MAN-ATTR               
071700     END-IF                                                               
071800                                                                          
071900     IF MID-IDSIGILL NOT = ALL '+'                                        
072000       MOVE MFS-ADD-READ-FIELD       TO MOD-IDSIGILL-ATTR                 
072100     END-IF                                                               
072200                                                                          
072300     IF MID-TISKEPPN-MAN NOT = ALL '+'                                    
072400       MOVE MFS-ADD-READ-FIELD       TO MOD-TISKEPPN-MAN-ATTR             
072500     END-IF                                                               
072600                                                                          
072700     IF MID-IDLC NOT = ALL '+'                                            
072800       MOVE MFS-ADD-READ-FIELD       TO MOD-IDLC-ATTR                     
072900     END-IF                                                               
073000                                                                          
073100     IF MID-IDLICENS NOT = ALL '+'                                        
073200       MOVE MFS-ADD-READ-FIELD       TO MOD-IDLICENS-ATTR                 
073300     END-IF                                                               
073400                                                                          
073500     IF MID-IDBOKN NOT = ALL '+'                                          
073600       MOVE MFS-ADD-READ-FIELD       TO MOD-IDBOKN-ATTR                   
073700     END-IF                                                               
073800                                                                          
073900     IF MID-IDVCERT NOT = ALL '+'                                         
074000       MOVE MFS-ADD-READ-FIELD       TO MOD-IDVCERT-ATTR                  
074100     END-IF                                                               
074200                                                                          
074300     IF MID-BESLULEV NOT = ALL '+'                                        
074400       MOVE MFS-ADD-READ-FIELD       TO MOD-BESLULEV-ATTR                 
074500     END-IF                                                               
074600                                                                          
074700     IF MID-KDLEVVIL NOT = ALL '+'                                        
074800       MOVE MFS-ADD-READ-FIELD       TO MOD-KDLEVVIL-ATTR                 
074900     END-IF                                                               
075000                                                                          
075100     .                                                                    
075200     EJECT                                                                
075300 F-READ-SHOW-INFO SECTION.                                                
075400                                                                          
075500     PERFORM IMS-GU-WDGX4496                                              
075600     IF SEGMENT-FOUND                                                     
075700        MOVE 4496-VKORDBTO-LASTB TO MOD-VKORDBTO                          
075800        MOVE 4496-VLORDBTO-LASTB TO MOD-VLORDBTO                          
075900        MOVE 4496-SUORDV-LASTB   TO MOD-SUORDV                            
076000     END-IF                                                               
076100                                                                          
076200     MOVE W-IDDISTR         TO TEST-IDDISTR                               
076300     IF DIST79-ECOM-PRICE                                                 
076400        MOVE SPACE                  TO MOD-CURRENCY-MC                    
076500                                       MOD-KDVALISO-MC                    
076600     ELSE                                                                 
076700       IF DIST35-NONVCC-NONVCC-REFILL                                     
076800       OR DCS-USA                                                         
076900       OR DCS-LAND-NON-VCC-OWNED                                          
077000         MOVE 'Currency   '         TO MOD-CURRENCY-MC                    
077100         MOVE DCS-KDVALISO          TO MOD-KDVALISO-MC                    
077200       ELSE                                                               
077300          MOVE 'Currency   '        TO MOD-CURRENCY-MC                    
077400          MOVE 'SEK'                TO MOD-KDVALISO-MC                    
077500       END-IF                                                             
077600     END-IF                                                               
077700                                                                          
077800     IF W-IDSHIPM NUMERIC AND W-IDSHIPM > ZERO                            
077900       PERFORM FA-READ-BASICDATA                                          
078000                                                                          
078100       IF SEGMENT-MISSING                                                 
078200         PERFORM MFS-ERASE-FIELD-LINE                                     
078300       ELSE                                                               
078400         IF DIST79-ECOM-PRICE                                             
078500                                                                          
078600           MOVE MFS-ERASE-FIELD TO MOD-PRFRAKT                            
078700                                   MOD-PRFOERS                            
078800                                   MOD-REFOERS                            
078900                                   MOD-REOVKOFF                           
079000                                   MOD-PRLEGKST                           
079100                                   MOD-RELEGKST                           
079200                                   MOD-PREMBHNT                           
079300                                   MOD-REEMBHNT                           
079400                                   MOD-PRAVDRAG                           
079500                                   MOD-REAVDRAG                           
079600                                   MOD-FLCREDL                            
079700                                   MOD-KDVALISO-MAN                       
079800                                   MOD-PRKURS-MAN                         
079900         ELSE                                                             
080000                                                                          
080100*        TILL-PRFRAKT är (alltid?) = 0 här. Därför nedanstående           
080200*        för att kunna kolla om PRFRAKT inmatad i bild.                   
080300*        Aktivt inmatad PRFRAKT = 0 är godkänd.                           
080400                                                                          
080500           IF TILL-PRFRAKT > ZERO                                         
080600             MOVE YES           TO MOD-FLFRAKT-UPD                        
080700           END-IF                                                         
080800                                                                          
080900           MOVE TILL-PRFRAKT    TO MOD-PRFRAKT                            
081000           MOVE TILL-PRFOERS    TO MOD-PRFOERS                            
081100           MOVE TILL-REFOERS    TO MOD-REFOERS                            
081200           MOVE TILL-REOVKOFF   TO MOD-REOVKOFF                           
081300           MOVE TILL-PRLEGKST   TO MOD-PRLEGKST                           
081400           MOVE TILL-RELEGKST   TO MOD-RELEGKST                           
081500           MOVE TILL-PREMBHNT   TO MOD-PREMBHNT                           
081600           MOVE TILL-REEMBHNT   TO MOD-REEMBHNT                           
081700           MOVE TILL-PRAVDRAG   TO MOD-PRAVDRAG                           
081800           MOVE TILL-REAVDRAG   TO MOD-REAVDRAG                           
081900           MOVE TILL-KDVALISO-MAN                                         
082000                                TO MOD-KDVALISO-MAN                       
082100           MOVE TILL-PRKURS-MAN TO MOD-PRKURS-MAN                         
082200           MOVE MFS-ERASE-FIELD TO MOD-FLCREDL                            
082300         END-IF                                                           
082500         MOVE TILL-IDSIGILL     TO MOD-IDSIGILL                           
082600         MOVE TILL-TISKEPPN-MAN TO MOD-TISKEPPN-MAN                       
082700         MOVE TILL-IDLC         TO MOD-IDLC                               
082800         MOVE TILL-IDLICENS     TO MOD-IDLICENS                           
082900         MOVE TILL-IDBOKN       TO MOD-IDBOKN                             
083000         MOVE TILL-IDVCERT      TO MOD-IDVCERT                            
083100         MOVE TILL-BESLULEV     TO MOD-BESLULEV                           
083110         INSPECT MOD-BESLULEV REPLACING LEADING ZERO  BY SPACE            
083200         MOVE MFS-ERASE-FIELD   TO MOD-KDVALUTA-MAN                       
083300       END-IF                                                             
083400                                                                          
083500       IF SAVE-FLSAMFAK = YES                                             
083600         PERFORM IMS-GHU-WDE211-DIST                                      
083700       ELSE                                                               
083800         PERFORM IMS-GHU-WDE211                                           
083900       END-IF                                                             
084000                                                                          
084100       IF SEGMENT-FOUND                                                   
084200         IF BGMT-FLSEPINV = 'J'                                           
084300           MOVE 'Y'             TO MOD-FLSEPINV                           
084400         ELSE                                                             
084500           MOVE BGMT-FLSEPINV   TO MOD-FLSEPINV                           
084600         END-IF                                                           
084700         IF BGMT-KDLEVVIL > ZERO                                          
084800           MOVE BGMT-KDLEVVIL   TO MOD-KDLEVVIL                           
084900         ELSE                                                             
085000           MOVE MFS-ERASE-FIELD TO MOD-KDLEVVIL                           
085100         END-IF                                                           
085200       ELSE                                                               
085300         MOVE MFS-ERASE-FIELD   TO MOD-FLSEPINV                           
085400                                   MOD-KDLEVVIL                           
085500       END-IF                                                             
085600                                                                          
085700     END-IF                                                               
085800     .                                                                    
085900     EJECT                                                                
086000 FA-READ-BASICDATA SECTION.                                               
086100                                                                          
086200     IF SAVE-FLSAMFAK = YES                                               
086300       PERFORM IMS-GHU-WDE122-DIST                                        
086400     ELSE                                                                 
086500       PERFORM IMS-GHU-WDE122                                             
086600     END-IF                                                               
086700     .                                                                    
086800     EJECT                                                                
086900                                                                          
087000 G-CHECK-INPUT SECTION.                                                   
087100                                                                          
087200     MOVE NOO TO INPUT-122-SW                                             
087300     MOVE NOO TO INPUT-211-SW                                             
087400     MOVE YES TO INDATA-SW                                                
087500                                                                          
087600     MOVE ZERO TO   SAVE-PRKURS-MAN                                       
087700                    SAVE-PRFRAKT                                          
087800                    SAVE-PRLEGKST                                         
087900                    SAVE-RELEGKST                                         
088000                    SAVE-PREMBHNT                                         
088100                    SAVE-REEMBHNT                                         
088200                    SAVE-PRFOERS                                          
088300                    SAVE-REFOERS                                          
088400                    SAVE-REOVKOFF                                         
088500                    SAVE-PRAVDRAG                                         
088600                    SAVE-REAVDRAG                                         
088700     EJECT                                                                
088800     IF MID-W4I67601 = ALL '+'                                            
088900       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
089000       CALL WMEDKONV USING MED-WMEDAREA                                   
089100       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
089200       PERFORM MFS-DONT-TOUCH-FIELD-IN                                    
089300       PERFORM MFS-DONT-TOUCH-FIELD-OUT                                   
089400       MOVE NOO TO INDATA-SW                                              
089500     ELSE                                                                 
089600       PERFORM S07-CHECK-SPACE-INPUT                                      
089700       IF NOT DIST79-DEALER-PRICE AND                                     
089800          NOT DIST79-ECOM-PRICE                                           
089900         IF MID-FLCREDL NOT = '+'                                         
090000           IF MID-FLCREDL = 'J' OR 'Y'                                    
090100             MOVE YES TO INPUT-122-SW                                     
090200             MOVE MFS-ALPHA-FIELD-OK    TO MOD-FLCREDL-ATTR               
090300           ELSE                                                           
090400             MOVE MFS-ALPHA-FIELD-WRONG TO MOD-FLCREDL-ATTR               
090500             MOVE NOO TO INDATA-SW                                        
090600           END-IF                                                         
090700         END-IF                                                           
090800                                                                          
090900         IF MID-VALUTA  NOT = ALL '+'                                     
091000           MOVE YES TO INPUT-122-SW                                       
091100           IF MID-KDVALUTA-MAN  = ALL '+' OR                              
091200              MID-PRKURS-MAN    = ALL '+' OR                              
091300              NOT (MID-FLCREDL  = 'J' OR 'Y')                             
091400             MOVE MFS-NUM-FIELD-WRONG TO MOD-KDVALUTA-MAN-ATTR            
091500                                         MOD-PRKURS-MAN-ATTR              
091600             MOVE NOO TO INDATA-SW                                        
091700           ELSE                                                           
091800             IF MID-KDVALUTA-MAN NOT NUMERIC                              
091900               MOVE NOO TO INDATA-SW                                      
092000               MOVE MFS-NUM-FIELD-WRONG TO MOD-KDVALUTA-MAN-ATTR          
092100             ELSE                                                         
092200               MOVE MID-KDVALUTA-MAN      TO W-KDVALUTA                   
092300                                                                          
092400               MOVE '   '                 TO CURR-KDVALISO-ROW            
092500               MOVE +1                    TO TAB-IX                       
092600               PERFORM UNTIL TAB-IX > TAB-IX-MAX                          
092700                 IF W-KDVALUTA = TAB-KDVALUTA(TAB-IX)                     
092800                   MOVE TAB-KDVALISO(TAB-IX ) TO CURR-KDVALISO-ROW        
092900                   MOVE TAB-IX-MAX        TO TAB-IX                       
093000                 END-IF                                                   
093100                 ADD +1                   TO TAB-IX                       
093200               END-PERFORM                                                
093300                                                                          
093400               IF CURR-KDVALISO-ROW NOT = '   '                           
093500                 MOVE W-DATE-AAMM         TO CURR-TIAAMM                  
093600                 MOVE WS-KDVALISO-HUV     TO CURR-KDVALISO-HUV            
093700                 MOVE 'M'                 TO CURR-KDVALTYP                
093800                                                                          
093900                 CALL W510CURR USING CURR-W510CURR WDG2-PCB               
094000                 IF CURR-KDSVAR = ' '                                     
094100                   MOVE MFS-NUM-FIELD-OK  TO MOD-KDVALUTA-MAN-ATTR        
094200                 ELSE                                                     
094300                   MOVE MFS-NUM-FIELD-WRONG                               
094400                                          TO MOD-KDVALUTA-MAN-ATTR        
094500                   MOVE NOO TO INDATA-SW                                  
094600                 END-IF                                                   
094700               ELSE                                                       
094800                 MOVE MFS-NUM-FIELD-WRONG TO MOD-KDVALUTA-MAN-ATTR        
094900                 MOVE NOO TO INDATA-SW                                    
095000               END-IF                                                     
095100             END-IF                                                       
095200** CHECK ON PRKURS                                                        
095300             MOVE MID-PRKURS-MAN  TO DEC-IDFRIDATA                        
095400             MOVE 6 TO DEC-KVHELTAL                                       
095500             MOVE 4 TO DEC-KVDECIMAL                                      
095600             PERFORM S01-HANDLE-ALPHA-TO-NUM                              
095700                                                                          
095800             IF DEC-KDSVAR-OK                                             
095900               MOVE DEC-IDEDITDATA TO SAVE-PRKURS-MAN                     
096000               MOVE MFS-NUM-FIELD-OK                                      
096100                                   TO MOD-PRKURS-MAN-ATTR                 
096200             ELSE                                                         
096300               MOVE MFS-NUM-FIELD-WRONG                                   
096400                                   TO MOD-PRKURS-MAN-ATTR                 
096500               MOVE NOO TO INDATA-SW                                      
096600             END-IF                                                       
096700           END-IF                                                         
096800         END-IF                                                           
096900                                                                          
097000         MOVE W-IDDISTR TO  TEST-IDDISTR                                  
097100         IF DIST35-RETUR              OR                                  
097200            DIST35-CDC-NL-REFILL      OR                                  
097300            DIST35-CDC-GB-REFILL      OR                                  
097400            DIST35-CDC-GB-3A-REFILL   OR                                  
097500            DIST35-CDC-ES-REFILL      OR                                  
097600            DIST35-CDC-IT-REFILL      OR                                  
097700            DIST35-CDC-AT-REFILL      OR                                  
097800            DIST35-REFILL-JP          OR                                  
097900            DIST35-REFILL-NA          OR                                  
098000            DIST35-REFILL-NA-JAP      OR                                  
098100            DIST35-REFILL-CN          OR                                  
098200            DIST35-CDC-IN-REFILL      OR                                  
098300            DIST35-CDC-KR-REFILL      OR                                  
098400            DIST35-CDC-AE-REFILL      OR                                  
098500            DIST35-CDC-TR-REFILL      OR                                  
098600            DIST35-CDC-MY-REFILL      OR                                  
098700            DIST35-CDC-TH-REFILL      OR                                  
098800            DIST35-CDC-TW-REFILL      OR                                  
098900            DIST35-CDC-MX-REFILL      OR                                  
099000            DIST35-CDC-BR-REFILL      OR                                  
099100            DIST35-CDC-ZA-REFILL      OR                                  
099200            DIST07-NA-CUSTOMERS       OR                                  
099300            DIST07-NA-CUSTOMERS       OR                                  
099400            DIST07-KINA-RET-DISCR     OR                                  
099500            DIST07-INDIEN-RET-DISCR   OR                                  
099600            DIST07-KOREA-RET-DISCR    OR                                  
099700            DIST07-TURKEY-RET-DISCR   OR                                  
099800            DIST07-MALAYSIA-RET-DISCR OR                                  
099900            DIST07-THAILAND-RET-DISCR OR                                  
100000            DIST07-TAIWAN-RET-DISCR   OR                                  
100100            DIST07-MEXICO-RET-DISCR   OR                                  
100110            DIST07-BRAZIL-RET-DISCR   OR                                  
100200            DIST07-S-AFRICA-RET-DISCR                                     
100300**                                                                        
100400**---DC 11--> Kina, Indien, Korea (VOR)                                   
100500**---skall kunna fylla i frakt oh försäkring                              
100600**---enligt orderkontoret 18/12'19                                        
100700**          DIST07-KINA               OR                                  
100800**          DIST07-INDIEN             OR                                  
100900**          DIST07-KOREA              OR                                  
101000**          DIST07-TURKEY             OR                                  
101100                                                                          
101200                                                                          
101300           IF (DIST35-REFILL         AND NOT                              
101400               DIST35-REFILL-NA      AND NOT                              
101500               DIST35-REFILL-CN      AND NOT                              
101600               DIST35-CDC-IN-REFILL  AND NOT                              
101700               DIST35-CDC-KR-REFILL  AND NOT                              
101800               DIST35-CDC-AE-REFILL  AND NOT                              
101900               DIST35-CDC-TR-REFILL  AND NOT                              
102000               DIST35-CDC-MY-REFILL  AND NOT                              
102100               DIST35-CDC-TH-REFILL  AND NOT                              
102200               DIST35-CDC-TW-REFILL  AND NOT                              
102300               DIST35-CDC-MX-REFILL  AND NOT                              
102400               DIST35-CDC-BR-REFILL  AND NOT                              
102500               DIST35-CDC-ZA-REFILL)                                      
102600                                     OR                                   
102700               DIST35-RETUR          OR                                   
102800               DIST35-REFILL-NA-JAP                                       
102900                                                                          
103000* Frakt är oftast obl att fylla i, men för vissa dist otillåtet!          
103100             MOVE NOO                 TO PRFRAKT-IFYLLES-SW               
103200                                                                          
103300             IF MID-PRFRAKT NOT = ALL '+'                                 
103400               MOVE MFS-NUM-FIELD-WRONG TO MOD-PRFRAKT-ATTR               
103500               MOVE NOO TO INDATA-SW                                      
103600             END-IF                                                       
103700           END-IF                                                         
103800*DIST35-CDC-ZA-REFILL will not added for legal fees                       
103900           IF MID-PRLEGKST NOT = ALL '+'                                  
104000             IF DIST35-CDC-AE-REFILL  OR                                  
104100                DIST35-CDC-TR-REFILL                                      
104200               CONTINUE                                                   
104300             ELSE                                                         
104400               MOVE MFS-NUM-FIELD-WRONG TO MOD-PRLEGKST-ATTR              
104500               MOVE NOO TO INDATA-SW                                      
104600             END-IF                                                       
104700           END-IF                                                         
104800                                                                          
104900           IF MID-PREMBHNT NOT = ALL '+'                                  
105000             MOVE MFS-NUM-FIELD-WRONG TO MOD-PREMBHNT-ATTR                
105100             MOVE NOO TO INDATA-SW                                        
105200           END-IF                                                         
105300                                                                          
105400           IF MID-PRFOERS  NOT = ALL '+'        AND NOT                   
105500              DIST35-REFILL-CN                  AND NOT                   
105600              DIST35-CDC-IN-REFILL              AND NOT                   
105700              DIST35-CDC-KR-REFILL              AND NOT                   
105800              DIST35-CDC-AE-REFILL              AND NOT                   
105900              DIST35-CDC-TR-REFILL              AND NOT                   
106000              DIST35-CDC-MY-REFILL              AND NOT                   
106100              DIST35-CDC-TH-REFILL              AND NOT                   
106200              DIST35-CDC-TW-REFILL              AND NOT                   
106300              DIST35-CDC-MX-REFILL              AND NOT                   
106400              DIST35-CDC-BR-REFILL              AND NOT                   
106500              DIST35-CDC-ZA-REFILL                                        
106600                                                                          
106700*             DIST07-KINA                       AND NOT                   
106800*             DIST07-INDIEN                     AND NOT                   
106900*             DIST07-KOREA                      AND NOT                   
107000*             DIST07-TURKEY                                               
107100               MOVE MFS-NUM-FIELD-WRONG TO MOD-PRFOERS-ATTR               
107200               MOVE NOO TO INDATA-SW                                      
107300                                                                          
107400           END-IF                                                         
107500                                                                          
107600           IF MID-PRAVDRAG NOT = ALL '+'                                  
107700             MOVE MFS-NUM-FIELD-WRONG TO MOD-PRAVDRAG-ATTR                
107800             MOVE NOO TO INDATA-SW                                        
107900           END-IF                                                         
108000                                                                          
108100           IF MID-RELEGKST NOT = ALL '+'                                  
108200             MOVE MFS-NUM-FIELD-WRONG TO MOD-RELEGKST-ATTR                
108300             MOVE NOO TO INDATA-SW                                        
108400           END-IF                                                         
108500                                                                          
108600           IF MID-REEMBHNT NOT = ALL '+'                                  
108700             IF (DIST35-REFILL AND NOT (DIST35-REFILL-NA     OR           
108800                                        DIST35-REFILL-CN     OR           
108900                                        DIST35-CDC-IN-REFILL OR           
109000                                        DIST35-CDC-KR-REFILL OR           
109100                                        DIST35-CDC-AE-REFILL OR           
109200                                        DIST35-CDC-TR-REFILL OR           
109300                                        DIST35-CDC-MY-REFILL OR           
109400                                        DIST35-CDC-TH-REFILL OR           
109500                                        DIST35-CDC-TW-REFILL OR           
109600                                        DIST35-CDC-MX-REFILL OR           
109700                                        DIST35-CDC-BR-REFILL OR           
109800                                        DIST35-CDC-ZA-REFILL))            
109900                               OR                                         
110000                 DIST35-RETUR  OR                                         
110100                 DIST35-REFILL-NA-JAP                                     
110200               MOVE MFS-NUM-FIELD-WRONG TO MOD-REEMBHNT-ATTR              
110300               MOVE NOO TO INDATA-SW                                      
110400             END-IF                                                       
110500           END-IF                                                         
110600                                                                          
110700           IF MID-REFOERS  NOT = ALL '+'      AND NOT                     
110800             DIST35-REFILL-CN                 AND NOT                     
110900             DIST35-CDC-IN-REFILL             AND NOT                     
111000             DIST35-CDC-KR-REFILL             AND NOT                     
111100             DIST35-CDC-AE-REFILL             AND NOT                     
111200             DIST35-CDC-TR-REFILL             AND NOT                     
111300             DIST35-CDC-MY-REFILL             AND NOT                     
111400             DIST35-CDC-TH-REFILL             AND NOT                     
111500             DIST35-CDC-TW-REFILL             AND NOT                     
111600             DIST35-CDC-MX-REFILL             AND NOT                     
111700             DIST35-CDC-BR-REFILL             AND NOT                     
111800             DIST35-CDC-ZA-REFILL                                         
111900                                                                          
112000*            DIST07-KINA                      AND NOT                     
112100*            DIST07-INDIEN                    AND NOT                     
112200*            DIST07-KOREA                     AND NOT                     
112300*            DIST07-TURKEY                                                
112400                                                                          
112500             MOVE MFS-NUM-FIELD-WRONG TO MOD-REFOERS-ATTR                 
112600             MOVE NOO TO INDATA-SW                                        
112700           END-IF                                                         
112800                                                                          
112900           IF MID-REAVDRAG NOT = ALL '+'                                  
113000             MOVE MFS-NUM-FIELD-WRONG TO MOD-REAVDRAG-ATTR                
113100             MOVE NOO TO INDATA-SW                                        
113200           END-IF                                                         
113300                                                                          
113400           IF MID-REOVKOFF NOT = ALL '+'  AND NOT                         
113500              (DIST35-REFILL-CN OR DIST35-CDC-IN-REFILL OR                
113600                                   DIST35-CDC-KR-REFILL OR                
113700                                   DIST35-CDC-AE-REFILL OR                
113800                                   DIST35-CDC-TR-REFILL OR                
113900                                   DIST35-CDC-MY-REFILL OR                
114000                                   DIST35-CDC-TH-REFILL OR                
114100                                   DIST35-CDC-TW-REFILL OR                
114200                                   DIST35-CDC-MX-REFILL OR                
114300                                   DIST35-CDC-BR-REFILL OR                
114400                                   DIST35-CDC-ZA-REFILL)                  
114500               MOVE MFS-NUM-FIELD-WRONG TO MOD-REOVKOFF-ATTR              
114600               MOVE NOO TO INDATA-SW                                      
114700           END-IF                                                         
114800         END-IF                                                           
114900                                                                          
115000         IF MID-PRFRAKT NOT = ALL '+'                                     
115100           MOVE MID-PRFRAKT     TO DEC-IDFRIDATA                          
115200           MOVE 7 TO DEC-KVHELTAL                                         
115300           MOVE 2 TO DEC-KVDECIMAL                                        
115400           PERFORM S01-HANDLE-ALPHA-TO-NUM                                
115500                                                                          
115600           IF DEC-KDSVAR-OK                                               
115700             MOVE DEC-IDEDITDATA TO SAVE-PRFRAKT                          
115800             MOVE MFS-NUM-FIELD-OK                                        
115900                                 TO MOD-PRFRAKT-ATTR                      
116000             MOVE YES TO INPUT-122-SW                                     
116100             MOVE YES TO INPUT-211-SW                                     
116200           ELSE                                                           
116300             MOVE MFS-NUM-FIELD-WRONG                                     
116400                                 TO MOD-PRFRAKT-ATTR                      
116500             MOVE NOO TO INDATA-SW                                        
116600           END-IF                                                         
116700         ELSE                                                             
116800           IF PRFRAKT-OBLIGATORISK                                        
116900             MOVE MFS-NUM-FIELD-WRONG                                     
117000                                 TO MOD-PRFRAKT-ATTR                      
117100             MOVE NOO TO INDATA-SW                                        
117200           END-IF                                                         
117300         END-IF                                                           
117400                                                                          
117500         IF MID-LEGKST NOT = ALL '+'                                      
117600           IF MID-PRLEGKST NOT = ALL '+' AND                              
117700              MID-RELEGKST NOT = ALL '+'                                  
117800             MOVE MFS-NUM-FIELD-WRONG TO MOD-PRLEGKST-ATTR                
117900                                         MOD-RELEGKST-ATTR                
118000             MOVE NOO TO INDATA-SW                                        
118100           ELSE                                                           
118200             IF MID-RELEGKST NOT = ALL '+'                                
118300               MOVE MID-RELEGKST    TO DEC-IDFRIDATA                      
118400               MOVE 2 TO DEC-KVHELTAL                                     
118500               MOVE 1 TO DEC-KVDECIMAL                                    
118600               PERFORM S01-HANDLE-ALPHA-TO-NUM                            
118700                                                                          
118800               IF DEC-KDSVAR-OK                                           
118900                 MOVE DEC-IDEDITDATA TO SAVE-RELEGKST                     
119000                 MOVE MFS-NUM-FIELD-OK                                    
119100                                     TO MOD-RELEGKST-ATTR                 
119200                 MOVE YES TO INPUT-122-SW                                 
119300                 MOVE YES TO INPUT-211-SW                                 
119400               ELSE                                                       
119500                 MOVE MFS-NUM-FIELD-WRONG                                 
119600                                     TO MOD-RELEGKST-ATTR                 
119700                 MOVE NOO TO INDATA-SW                                    
119800               END-IF                                                     
119900             END-IF                                                       
120000                                                                          
120100             IF MID-PRLEGKST NOT = ALL '+'                                
120200               MOVE MID-PRLEGKST    TO DEC-IDFRIDATA                      
120300               MOVE 7 TO DEC-KVHELTAL                                     
120400               MOVE 2 TO DEC-KVDECIMAL                                    
120500               PERFORM S01-HANDLE-ALPHA-TO-NUM                            
120600                                                                          
120700               IF DEC-KDSVAR-OK                                           
120800                 MOVE DEC-IDEDITDATA TO SAVE-PRLEGKST                     
120900                 MOVE MFS-NUM-FIELD-OK                                    
121000                                     TO MOD-PRLEGKST-ATTR                 
121100                 MOVE YES TO INPUT-122-SW                                 
121200                 MOVE YES TO INPUT-211-SW                                 
121300               ELSE                                                       
121400                 MOVE MFS-NUM-FIELD-WRONG                                 
121500                                     TO MOD-PRLEGKST-ATTR                 
121600                 MOVE NOO TO INDATA-SW                                    
121700               END-IF                                                     
121800             END-IF                                                       
121900           END-IF                                                         
122000         END-IF                                                           
122100                                                                          
122200         IF MID-EMBHNT NOT = ALL '+'                                      
122300           IF MID-PREMBHNT NOT = ALL '+'  AND                             
122400              MID-REEMBHNT NOT = ALL '+'                                  
122500             MOVE MFS-NUM-FIELD-WRONG TO MOD-PREMBHNT-ATTR                
122600                                         MOD-REEMBHNT-ATTR                
122700             MOVE NOO TO INDATA-SW                                        
122800           ELSE                                                           
122900             IF MID-REEMBHNT NOT = ALL '+'                                
123000               MOVE MID-REEMBHNT    TO DEC-IDFRIDATA                      
123100               MOVE 2 TO DEC-KVHELTAL                                     
123200               MOVE 1 TO DEC-KVDECIMAL                                    
123300               PERFORM S01-HANDLE-ALPHA-TO-NUM                            
123400                                                                          
123500               IF DEC-KDSVAR-OK                                           
123600                 MOVE DEC-IDEDITDATA TO SAVE-REEMBHNT                     
123700                 MOVE MFS-NUM-FIELD-OK                                    
123800                                     TO MOD-REEMBHNT-ATTR                 
123900                 MOVE YES TO INPUT-122-SW                                 
124000                 MOVE YES TO INPUT-211-SW                                 
124100               ELSE                                                       
124200                 MOVE MFS-NUM-FIELD-WRONG                                 
124300                                     TO MOD-REEMBHNT-ATTR                 
124400                 MOVE NOO TO INDATA-SW                                    
124500               END-IF                                                     
124600             END-IF                                                       
124700                                                                          
124800             IF MID-PREMBHNT NOT = ALL '+'                                
124900               MOVE MID-PREMBHNT    TO DEC-IDFRIDATA                      
125000               MOVE 7 TO DEC-KVHELTAL                                     
125100               MOVE 2 TO DEC-KVDECIMAL                                    
125200               PERFORM S01-HANDLE-ALPHA-TO-NUM                            
125300                                                                          
125400               IF DEC-KDSVAR-OK                                           
125500                 MOVE DEC-IDEDITDATA TO SAVE-PREMBHNT                     
125600                 MOVE MFS-NUM-FIELD-OK                                    
125700                                     TO MOD-PREMBHNT-ATTR                 
125800                 MOVE YES TO INPUT-122-SW                                 
125900                 MOVE YES TO INPUT-211-SW                                 
126000               ELSE                                                       
126100                 MOVE MFS-NUM-FIELD-WRONG                                 
126200                                     TO MOD-PREMBHNT-ATTR                 
126300                 MOVE NOO TO INDATA-SW                                    
126400               END-IF                                                     
126500             END-IF                                                       
126600           END-IF                                                         
126700         END-IF                                                           
126800                                                                          
126900         IF MID-FOERS NOT = ALL '+'                                       
127000           IF MID-PRFOERS NOT = ALL '+'  AND                              
127100              MID-REFOERS-OVKOFF NOT = ALL '+'                            
127200             IF MID-PRFOERS NOT = ALL '+'                                 
127300               MOVE MFS-NUM-FIELD-WRONG TO MOD-PRFOERS-ATTR               
127400             END-IF                                                       
127500             IF MID-REFOERS NOT = ALL '+'                                 
127600               MOVE MFS-NUM-FIELD-WRONG TO MOD-REFOERS-ATTR               
127700             END-IF                                                       
127800             IF MID-REOVKOFF NOT = ALL '+'                                
127900               MOVE MFS-NUM-FIELD-WRONG TO MOD-REOVKOFF-ATTR              
128000             END-IF                                                       
128100             MOVE NOO TO INDATA-SW                                        
128200           ELSE                                                           
128300             IF MID-PRFOERS NOT = ALL '+'                                 
128400               MOVE MID-PRFOERS     TO DEC-IDFRIDATA                      
128500               MOVE 7 TO DEC-KVHELTAL                                     
128600               MOVE 2 TO DEC-KVDECIMAL                                    
128700               PERFORM S01-HANDLE-ALPHA-TO-NUM                            
128800                                                                          
128900               IF DEC-KDSVAR-OK                                           
129000                 MOVE DEC-IDEDITDATA TO SAVE-PRFOERS                      
129100                 MOVE MFS-NUM-FIELD-OK                                    
129200                                     TO MOD-PRFOERS-ATTR                  
129300                 MOVE YES TO INPUT-122-SW                                 
129400                 MOVE YES TO INPUT-211-SW                                 
129500               ELSE                                                       
129600                 MOVE MFS-NUM-FIELD-WRONG                                 
129700                                     TO MOD-PRFOERS-ATTR                  
129800                 MOVE NOO TO INDATA-SW                                    
129900               END-IF                                                     
130000             END-IF                                                       
130100                                                                          
130200             IF MID-REFOERS-OVKOFF NOT = ALL '+'                          
130300               IF MID-REFOERS = ALL '+'                                   
130400                 MOVE MFS-NUM-FIELD-WRONG TO MOD-REFOERS-ATTR             
130500                                             MOD-REOVKOFF-ATTR            
130600                 MOVE NOO TO INDATA-SW                                    
130700               ELSE                                                       
130800                 MOVE MID-REFOERS     TO DEC-IDFRIDATA                    
130900                 MOVE 2 TO DEC-KVHELTAL                                   
131000                 MOVE 3 TO DEC-KVDECIMAL                                  
131100                 PERFORM S01-HANDLE-ALPHA-TO-NUM                          
131200                                                                          
131300                 IF DEC-KDSVAR-OK                                         
131400                   MOVE DEC-IDEDITDATA TO SAVE-REFOERS                    
131500                   MOVE MFS-NUM-FIELD-OK                                  
131600                                        TO MOD-REFOERS-ATTR               
131700                   MOVE YES TO INPUT-122-SW                               
131800                   MOVE YES TO INPUT-211-SW                               
131900                 ELSE                                                     
132000                   MOVE MFS-NUM-FIELD-WRONG                               
132100                                        TO MOD-REFOERS-ATTR               
132200                   MOVE NOO TO INDATA-SW                                  
132300                 END-IF                                                   
132400               END-IF                                                     
132500                                                                          
132600               IF MID-REOVKOFF NOT = ALL '+'                              
132700                 MOVE MID-REOVKOFF    TO DEC-IDFRIDATA                    
132800                 MOVE 2 TO DEC-KVHELTAL                                   
132900                 MOVE 1 TO DEC-KVDECIMAL                                  
133000                 PERFORM S01-HANDLE-ALPHA-TO-NUM                          
133100                                                                          
133200                 IF DEC-KDSVAR-OK                                         
133300                   MOVE DEC-IDEDITDATA TO SAVE-REOVKOFF                   
133400                   MOVE MFS-NUM-FIELD-OK                                  
133500                                       TO MOD-REOVKOFF-ATTR               
133600                   MOVE YES TO INPUT-122-SW                               
133700                   MOVE YES TO INPUT-211-SW                               
133800                 ELSE                                                     
133900                   MOVE MFS-NUM-FIELD-WRONG                               
134000                                       TO MOD-REOVKOFF-ATTR               
134100                   MOVE NOO TO INDATA-SW                                  
134200                 END-IF                                                   
134300               ELSE                                                       
134400                 MOVE NOO TO INDATA-SW                                    
134500                 MOVE MFS-NUM-FIELD-WRONG TO MOD-REOVKOFF-ATTR            
134600               END-IF                                                     
134700             END-IF                                                       
134800           END-IF                                                         
134900         END-IF                                                           
135000                                                                          
135100         IF MID-AVDRAG         NOT = ALL '+'                              
135200           MOVE YES TO INPUT-122-SW                                       
135300           MOVE YES TO INPUT-211-SW                                       
135400           IF MID-REAVDRAG NOT = ALL '+'    AND                           
135500              MID-PRAVDRAG NOT = ALL '+'                                  
135600             MOVE MFS-NUM-FIELD-WRONG  TO MOD-REAVDRAG-ATTR               
135700                                          MOD-PRAVDRAG-ATTR               
135800             MOVE NOO TO INDATA-SW                                        
135900           ELSE                                                           
136000             IF MID-REAVDRAG NOT = ALL '+'                                
136100               MOVE MID-REAVDRAG      TO DEC-IDFRIDATA                    
136200               MOVE 2 TO DEC-KVHELTAL                                     
136300               MOVE 1 TO DEC-KVDECIMAL                                    
136400               PERFORM S01-HANDLE-ALPHA-TO-NUM                            
136500                                                                          
136600               IF DEC-KDSVAR-OK                                           
136700                 MOVE DEC-IDEDITDATA  TO SAVE-REAVDRAG                    
136800                 MOVE MFS-NUM-FIELD-OK                                    
136900                                      TO MOD-REAVDRAG-ATTR                
137000                 MOVE YES             TO INPUT-122-SW                     
137100                 MOVE YES             TO INPUT-211-SW                     
137200               ELSE                                                       
137300                 MOVE MFS-NUM-FIELD-WRONG                                 
137400                                      TO MOD-REAVDRAG-ATTR                
137500                 MOVE NOO             TO INDATA-SW                        
137600               END-IF                                                     
137700             END-IF                                                       
137800                                                                          
137900             IF MID-PRAVDRAG NOT = ALL '+'                                
138000               MOVE MID-PRAVDRAG      TO DEC-IDFRIDATA                    
138100               MOVE 7 TO DEC-KVHELTAL                                     
138200               MOVE 2 TO DEC-KVDECIMAL                                    
138300               PERFORM S01-HANDLE-ALPHA-TO-NUM                            
138400                                                                          
138500               IF DEC-KDSVAR-OK                                           
138600                 MOVE DEC-IDEDITDATA  TO SAVE-PRAVDRAG                    
138700                 MOVE MFS-NUM-FIELD-OK                                    
138800                                      TO MOD-PRAVDRAG-ATTR                
138900                 MOVE YES             TO INPUT-122-SW                     
139000                 MOVE YES             TO INPUT-211-SW                     
139100               ELSE                                                       
139200                 MOVE MFS-NUM-FIELD-WRONG                                 
139300                                      TO MOD-PRAVDRAG-ATTR                
139400                 MOVE NOO             TO INDATA-SW                        
139500               END-IF                                                     
139600             END-IF                                                       
139700           END-IF                                                         
139800         END-IF                                                           
139900       END-IF                                                             
140000                                                                          
140100       IF MID-FLSEPINV NOT = ALL '+'                                      
140200         IF MID-FLSEPINV  = 'J' OR 'Y'                                    
140300           MOVE YES TO INPUT-211-SW                                       
140400           MOVE MFS-ALPHA-FIELD-OK    TO MOD-FLSEPINV-ATTR                
140500         ELSE                                                             
140600           IF MID-FLSEPINV = SPACE                                        
140700             MOVE YES TO INPUT-211-SW                                     
140800             MOVE MFS-ALPHA-FIELD-OK    TO MOD-FLSEPINV-ATTR              
140900           ELSE                                                           
141000             MOVE MFS-ALPHA-FIELD-WRONG TO MOD-FLSEPINV-ATTR              
141100             MOVE NOO                   TO INDATA-SW                      
141200           END-IF                                                         
141300         END-IF                                                           
141400       END-IF                                                             
141500                                                                          
141600       IF MID-IDSIGILL NOT = ALL '+'                                      
141700         MOVE YES TO INPUT-122-SW                                         
141800         MOVE MFS-ALPHA-FIELD-OK TO MOD-IDSIGILL-ATTR                     
141900       END-IF                                                             
142000                                                                          
142100       IF MID-TISKEPPN-MAN NOT = ALL '+'                                  
142200         IF MID-TISKEPPN-MAN NOT NUMERIC                                  
142300           MOVE MFS-NUM-FIELD-WRONG TO MOD-TISKEPPN-MAN-ATTR              
142400           MOVE NOO                 TO INDATA-SW                          
142500         ELSE                                                             
142600           PERFORM S06-CHECK-DATE                                         
142700           IF DAT-KDSVAR-OK                                               
142800             MOVE MFS-NUM-FIELD-OK  TO MOD-TISKEPPN-MAN-ATTR              
142900             MOVE YES TO INPUT-122-SW                                     
143000           ELSE                                                           
143100             MOVE MFS-NUM-FIELD-WRONG                                     
143200                                    TO MOD-TISKEPPN-MAN-ATTR              
143300             MOVE NOO               TO INDATA-SW                          
143400           END-IF                                                         
143500         END-IF                                                           
143600       END-IF                                                             
143700                                                                          
143800       IF MID-IDLC NOT = ALL '+'                                          
143900         MOVE MFS-ALPHA-FIELD-OK TO MOD-IDLC-ATTR                         
144000         MOVE YES TO INPUT-122-SW                                         
144100       END-IF                                                             
144200                                                                          
144300       IF MID-IDLICENS NOT = ALL '+'                                      
144400         MOVE MFS-ALPHA-FIELD-OK TO MOD-IDLICENS-ATTR                     
144500         MOVE YES TO INPUT-122-SW                                         
144600       END-IF                                                             
144700                                                                          
144800       IF MID-IDBOKN NOT = ALL '+'                                        
144900         MOVE MFS-ALPHA-FIELD-OK TO MOD-IDBOKN-ATTR                       
145000         MOVE YES TO INPUT-122-SW                                         
145100       END-IF                                                             
145200                                                                          
145300       IF MID-IDVCERT NOT = ALL '+'                                       
145400         MOVE YES TO INPUT-122-SW                                         
145500         MOVE MFS-ALPHA-FIELD-OK TO MOD-IDVCERT-ATTR                      
145600       END-IF                                                             
145700                                                                          
145800       IF MID-BESLULEV NOT = ALL '+'                                      
145900         IF FUNCTION TRIM(MID-BESLULEV) IS NUMERIC                        
146000           MOVE FUNCTION TRIM(MID-BESLULEV)                               
146010                                      TO WS-BESLULEV                      
146100           IF WS-BESLULEV >= 1 AND WS-BESLULEV <= 99999                   
146200             MOVE YES                 TO INPUT-122-SW                     
146300             MOVE MFS-ALPHA-FIELD-OK  TO MOD-BESLULEV-ATTR                
146400           ELSE                                                           
146500             MOVE MFS-NUM-FIELD-WRONG TO MOD-BESLULEV-ATTR                
146600             MOVE NOO                 TO INDATA-SW                        
146700           END-IF                                                         
146800         ELSE                                                             
146900           MOVE MFS-NUM-FIELD-WRONG   TO MOD-BESLULEV-ATTR                
147000                                                                          
147100           MOVE NOO                   TO INDATA-SW                        
147200         END-IF                                                           
147300       END-IF                                                             
147400                                                                          
147500       IF MID-KDLEVVIL NOT = ALL '+'                                      
147600         IF MID-KDLEVVIL NUMERIC                                          
147700           MOVE YES TO INPUT-111-SW                                       
147800                       INPUT-211-SW                                       
147900           MOVE MFS-NUM-FIELD-OK TO MOD-KDLEVVIL-ATTR                     
148000         ELSE                                                             
148100           MOVE MFS-NUM-FIELD-WRONG                                       
148200                                 TO MOD-KDLEVVIL-ATTR                     
148300           MOVE NOO              TO INDATA-SW                             
148400         END-IF                                                           
148500       END-IF                                                             
148600                                                                          
148700       IF INDATA-WRONG                                                    
148800         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
148900         CALL WMEDKONV USING MED-WMEDAREA                                 
149000         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
149100         PERFORM MFS-DONT-TOUCH-FIELD-OUT                                 
149200         PERFORM MFS-DONT-TOUCH-FIELD-IN                                  
149300       END-IF                                                             
149400     END-IF                                                               
149500     .                                                                    
149600     EJECT                                                                
149700 H-UPDATE SECTION.                                                        
149800                                                                          
149900     MOVE YES    TO WDE122-ISRT-SW                                        
150000                    WDE211-ISRT-SW                                        
150100     PERFORM S05-DC-LAND                                                  
150200                                                                          
150300     IF INPUT-122 OR INPUT-211 OR INPUT-111                               
150400** READ FIRST 4498 SEGMENT FOR IDDISTR                                    
150500       MOVE SAVE-IDDISTR-4676                                             
150600                         TO W-4498-IDDISTR-MIN                            
150700                            W-4498-IDDISTR-MAX                            
150800                                                                          
150900       IF FLSAMFAK                                                        
151000         MOVE ZERO       TO W-4498-IDKUNDNR-MIN                           
151100         MOVE 9999999    TO W-4498-IDKUNDNR-MAX                           
151200       ELSE                                                               
151300         MOVE SAVE-IDKUNDNR-4676                                          
151400                         TO W-4498-IDKUNDNR-MIN                           
151500                            W-4498-IDKUNDNR-MAX                           
151600       END-IF                                                             
151700                                                                          
151800       PERFORM IMS-GU-WDGX4498                                            
151900       IF SEGMENT-FOUND                                                   
152000         MOVE 4498-IDKUNDNR TO W-WDE111-IDKUNDNR                          
152100                               W-WDE211-IDKUNDNR                          
152200         IF FLSAMFAK                                                      
152300            MOVE ZERO       TO W-WDE211-IDKUNDNR                          
152400         END-IF                                                           
152500         IF SAVE-IDSHIPM  > ZERO                                          
152600           PERFORM S02-TEST-WDE122                                        
152700           IF INPUT-122                                                   
152800             PERFORM HA-UPDATE-WDE122                                     
152900           END-IF                                                         
153000                                                                          
153100           PERFORM IMS-GHU-WDE201                                         
153200           PERFORM IMS-GHNP-WDE211                                        
153300           PERFORM S03-TEST-WDE211                                        
153400           IF INPUT-211                                                   
153500             PERFORM HB-UPDATE-WDE211                                     
153600           END-IF                                                         
153700                                                                          
153800         ELSE                                                             
153900           PERFORM S04-CREATE-IDSHIPM                                     
154000           IF INPUT-122                                                   
154100             PERFORM HA-UPDATE-WDE122                                     
154200           END-IF                                                         
154300                                                                          
154400           IF INPUT-211                                                   
154500             PERFORM HB-UPDATE-WDE211                                     
154600           END-IF                                                         
154700         END-IF                                                           
154800                                                                          
154900       END-IF                                                             
155000                                                                          
155100       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
155200       CALL WMEDKONV USING MED-WMEDAREA                                   
155300       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
155400       PERFORM MFS-FORM-ATTR                                              
155500       PERFORM MFS-ERASE-FIELD-IN                                         
155600                                                                          
155700     END-IF                                                               
155800                                                                          
155900     .                                                                    
156000     EJECT                                                                
156100                                                                          
156200 HA-UPDATE-WDE122        SECTION.                                         
156300                                                                          
156400     IF NOT DIST79-DEALER-PRICE AND                                       
156500        NOT DIST79-ECOM-PRICE                                             
156600       IF MID-PRFRAKT NOT = ALL '+'                                       
156700         MOVE SAVE-PRFRAKT TO TILL-PRFRAKT                                
156800         MOVE YES          TO MOD-FLFRAKT-UPD                             
156900       END-IF                                                             
157000                                                                          
157100       IF MID-FOERS NOT = ALL '+'                                         
157200         IF MID-PRFOERS NOT = ALL '+'                                     
157300           MOVE SAVE-PRFOERS TO TILL-PRFOERS                              
157400           MOVE ZERO TO TILL-REFOERS                                      
157500                        TILL-REOVKOFF                                     
157600         ELSE                                                             
157700           MOVE SAVE-REFOERS  TO TILL-REFOERS                             
157800           MOVE SAVE-REOVKOFF TO TILL-REOVKOFF                            
157900           MOVE ZERO TO TILL-PRFOERS                                      
158000         END-IF                                                           
158100       END-IF                                                             
158200                                                                          
158300       IF MID-LEGKST NOT = ALL '+'                                        
158400         IF MID-PRLEGKST NOT = ALL '+'                                    
158500           MOVE SAVE-PRLEGKST TO TILL-PRLEGKST                            
158600           MOVE ZERO          TO TILL-RELEGKST                            
158700         END-IF                                                           
158800                                                                          
158900         IF MID-RELEGKST NOT = ALL '+'                                    
159000           MOVE SAVE-RELEGKST TO TILL-RELEGKST                            
159100           MOVE ZERO          TO TILL-PRLEGKST                            
159200         END-IF                                                           
159300       END-IF                                                             
159400                                                                          
159500       IF MID-EMBHNT NOT = ALL '+'                                        
159600         IF MID-PREMBHNT NOT = ALL '+'                                    
159700           MOVE SAVE-PREMBHNT TO TILL-PREMBHNT                            
159800           MOVE ZERO          TO TILL-REEMBHNT                            
159900         ELSE                                                             
160000           MOVE SAVE-REEMBHNT TO TILL-REEMBHNT                            
160100           MOVE ZERO          TO TILL-PREMBHNT                            
160200         END-IF                                                           
160300       END-IF                                                             
160400                                                                          
160500       IF MID-AVDRAG NOT = ALL '+'                                        
160600         IF MID-PRAVDRAG NOT = ALL '+'                                    
160700           MOVE SAVE-PRAVDRAG TO TILL-PRAVDRAG                            
160800           MOVE ZERO          TO TILL-REAVDRAG                            
160900         END-IF                                                           
161000                                                                          
161100         IF MID-REAVDRAG NOT = ALL '+'                                    
161200           MOVE SAVE-REAVDRAG TO TILL-REAVDRAG                            
161300           MOVE ZERO          TO TILL-PRAVDRAG                            
161400         END-IF                                                           
161500       END-IF                                                             
161600                                                                          
161700       IF MID-VALUTA NOT = ALL '+'                                        
161800         MOVE   CURR-KDVALISO-ROW TO TILL-KDVALISO-MAN                    
161900         MOVE   SAVE-PRKURS-MAN   TO TILL-PRKURS-MAN                      
162000       END-IF                                                             
162100     END-IF                                                               
162200                                                                          
162300     IF MID-IDSIGILL NOT = ALL '+'                                        
162400       MOVE MID-IDSIGILL TO TILL-IDSIGILL                                 
162410       MOVE SPACE TO TILL-FILLER                                          
162500     END-IF                                                               
162600                                                                          
162700     IF MID-TISKEPPN-MAN NOT = ALL '+'                                    
162800       MOVE MID-TISKEPPN-MAN TO TILL-TISKEPPN-MAN                         
162900     END-IF                                                               
163000                                                                          
163100     IF MID-IDLC NOT = ALL '+'                                            
163200       MOVE MID-IDLC TO TILL-IDLC                                         
163300     END-IF                                                               
163400                                                                          
163500     IF MID-IDLICENS NOT = ALL '+'                                        
163600       MOVE MID-IDLICENS TO TILL-IDLICENS                                 
163700     END-IF                                                               
163800                                                                          
163900     IF MID-IDBOKN NOT = ALL '+'                                          
164000       MOVE MID-IDBOKN TO TILL-IDBOKN                                     
164100     END-IF                                                               
164200                                                                          
164300     IF MID-IDVCERT NOT = ALL '+'                                         
164400       MOVE MID-IDVCERT TO TILL-IDVCERT                                   
164500     END-IF                                                               
164600                                                                          
164700     IF MID-BESLULEV NOT = ALL '+'                                        
164800       MOVE WS-BESLULEV TO TILL-BESLULEV                                  
164810       INSPECT TILL-BESLULEV REPLACING LEADING ZERO BY SPACE              
164820       MOVE SPACE       TO TILL-FILLER1                                   
164900     END-IF                                                               
165000                                                                          
165100     IF WDE122-ISRT                                                       
165200       PERFORM IMS-ISRT-WDE122                                            
165300     ELSE                                                                 
165400       PERFORM IMS-REPL-WDE122                                            
165500     END-IF                                                               
165600                                                                          
165700     .                                                                    
165800     EJECT                                                                
165900 HB-UPDATE-WDE211  SECTION.                                               
166000                                                                          
166100     MOVE W-IDDISTR TO  TEST-IDDISTR                                      
166200     IF MID-FLSEPINV  = 'Y' OR 'J'                                        
166300       MOVE 'J'          TO BGMT-FLSEPINV                                 
166400     ELSE                                                                 
166500       IF MID-FLSEPINV = SPACE                                            
166600         MOVE SPACE      TO BGMT-FLSEPINV                                 
166700       END-IF                                                             
166800     END-IF                                                               
166900     IF DIST92-ITALY                                                      
167000       MOVE 'J'          TO BGMT-FLSEPINV                                 
167100     END-IF                                                               
167200                                                                          
167300     IF MID-KDLEVVIL NOT = ALL '+'                                        
167400       MOVE MID-KDLEVVIL TO BGMT-KDLEVVIL                                 
167500     END-IF                                                               
167600                                                                          
167700     IF NOT DIST79-DEALER-PRICE AND                                       
167800        NOT DIST79-ECOM-PRICE                                             
167900       IF MID-PRFRAKT NOT = ALL '+'                                       
168000         MOVE SAVE-PRFRAKT TO BGMT-PRFRAKT                                
168100       END-IF                                                             
168200                                                                          
168300       IF MID-FOERS NOT = ALL '+'                                         
168400         IF MID-PRFOERS NOT = ALL '+'                                     
168500           MOVE SAVE-PRFOERS TO BGMT-PRFOERS                              
168600           MOVE ZERO TO BGMT-REFOERS                                      
168700                        BGMT-REOVKOFF                                     
168800         ELSE                                                             
168900           MOVE SAVE-REFOERS  TO BGMT-REFOERS                             
169000           MOVE SAVE-REOVKOFF TO BGMT-REOVKOFF                            
169100           MOVE ZERO TO BGMT-PRFOERS                                      
169200         END-IF                                                           
169300       END-IF                                                             
169400                                                                          
169500       IF MID-LEGKST NOT = ALL '+'                                        
169600         IF MID-PRLEGKST NOT = ALL '+'                                    
169700           MOVE SAVE-PRLEGKST TO BGMT-PRLEGKST                            
169800           MOVE ZERO          TO BGMT-RELEGKST                            
169900         END-IF                                                           
170000                                                                          
170100         IF MID-RELEGKST NOT = ALL '+'                                    
170200           MOVE SAVE-RELEGKST TO BGMT-RELEGKST                            
170300           MOVE ZERO          TO BGMT-PRLEGKST                            
170400         END-IF                                                           
170500       END-IF                                                             
170600                                                                          
170700       IF MID-EMBHNT NOT = ALL '+'                                        
170800         IF MID-PREMBHNT NOT = ALL '+'                                    
170900           MOVE SAVE-PREMBHNT TO BGMT-PREMBHNT                            
171000           MOVE ZERO TO BGMT-REEMBHNT                                     
171100         ELSE                                                             
171200           MOVE SAVE-REEMBHNT TO BGMT-REEMBHNT                            
171300           MOVE ZERO          TO BGMT-PREMBHNT                            
171400         END-IF                                                           
171500       END-IF                                                             
171600                                                                          
171700       IF MID-AVDRAG NOT = ALL '+'                                        
171800         IF MID-PRAVDRAG NOT = ALL '+'                                    
171900           MOVE SAVE-PRAVDRAG TO BGMT-PRAVDRAG                            
172000         END-IF                                                           
172100                                                                          
172200         IF MID-REAVDRAG NOT = ALL '+'                                    
172300           MOVE SAVE-REAVDRAG TO BGMT-REAVDRAG                            
172400         END-IF                                                           
172500       END-IF                                                             
172600     END-IF                                                               
172700                                                                          
172800     IF WDE211-ISRT                                                       
172900       PERFORM IMS-ISRT-WDE211                                            
173000     ELSE                                                                 
173100       PERFORM IMS-REPL-WDE211                                            
173200     END-IF                                                               
173300     .                                                                    
173400     EJECT                                                                
173500                                                                          
173600 I-PF3-RETURN SECTION.                                                    
173700                                                                          
173800*    om det är obligatoriskt att ange frakt, ska man inte                 
173900*    kunna undgå ange frakt genom att trycka pf3                          
174000*    (ska vara samma villkor som i G-CHECK-INPUT)                         
174100                                                                          
174200     IF NOT DIST79-DEALER-PRICE AND                                       
174300        NOT DIST79-ECOM-PRICE                                             
174400                                                                          
174500       MOVE W-IDDISTR TO    TEST-IDDISTR                                  
174600       IF DIST35-RETUR                OR                                  
174700          DIST35-CDC-NL-REFILL        OR                                  
174800          DIST35-CDC-GB-REFILL        OR                                  
174900          DIST35-CDC-GB-3A-REFILL     OR                                  
175000          DIST35-CDC-ES-REFILL        OR                                  
175100          DIST35-CDC-IT-REFILL        OR                                  
175200          DIST35-CDC-AT-REFILL        OR                                  
175300          DIST35-REFILL-JP            OR                                  
175400          DIST35-REFILL-NA            OR                                  
175500          DIST35-REFILL-NA-JAP        OR                                  
175600          DIST35-REFILL-CN            OR                                  
175700          DIST35-CDC-IN-REFILL        OR                                  
175800          DIST35-CDC-KR-REFILL        OR                                  
175900          DIST35-CDC-AE-REFILL        OR                                  
176000          DIST35-CDC-TR-REFILL        OR                                  
176100          DIST35-CDC-MY-REFILL        OR                                  
176200          DIST35-CDC-TH-REFILL        OR                                  
176300          DIST35-CDC-TW-REFILL        OR                                  
176400          DIST35-CDC-MX-REFILL        OR                                  
176500          DIST35-CDC-BR-REFILL        OR                                  
176600          DIST35-CDC-ZA-REFILL        OR                                  
176700          DIST07-NA-CUSTOMERS         OR                                  
176800          DIST07-KINA-RET-DISCR       OR                                  
176900          DIST07-INDIEN-RET-DISCR     OR                                  
177000          DIST07-KOREA-RET-DISCR      OR                                  
177100          DIST07-TURKEY-RET-DISCR     OR                                  
177200          DIST07-MALAYSIA-RET-DISCR   OR                                  
177300          DIST07-THAILAND-RET-DISCR   OR                                  
177400          DIST07-TAIWAN-RET-DISCR     OR                                  
177500          DIST07-MEXICO-RET-DISCR     OR                                  
177510          DIST07-BRAZIL-RET-DISCR     OR                                  
177600          DIST07-S-AFRICA-RET-DISCR                                       
177700                                                                          
177800**                                                                        
177900**---DC 11--> Kina, Indien, Korea (VOR)                                   
178000**---skall kunna fylla i frakt oh försäkring                              
178100**---enligt orderkontoret 18/12'19                                        
178200**          DIST07-KINA               OR                                  
178300**          DIST07-INDIEN             OR                                  
178400**          DIST07-KOREA              OR                                  
178500**          DIST07-TURKEY             OR                                  
178600                                                                          
178700                                                                          
178800         IF (DIST35-REFILL           AND NOT                              
178900             DIST35-REFILL-NA        AND NOT                              
179000             DIST35-REFILL-CN        AND NOT                              
179100             DIST35-CDC-IN-REFILL    AND NOT                              
179200             DIST35-CDC-KR-REFILL    AND NOT                              
179300             DIST35-CDC-AE-REFILL    AND NOT                              
179400             DIST35-CDC-TR-REFILL    AND NOT                              
179500             DIST35-CDC-MY-REFILL    AND NOT                              
179600             DIST35-CDC-TH-REFILL    AND NOT                              
179700             DIST35-CDC-TW-REFILL    AND NOT                              
179800             DIST35-CDC-MX-REFILL    AND NOT                              
179900             DIST35-CDC-BR-REFILL    AND NOT                              
180000             DIST35-CDC-ZA-REFILL)                                        
180100                                     OR                                   
180200             DIST35-RETUR            OR                                   
180300             DIST35-REFILL-NA-JAP                                         
180400           MOVE NOO                   TO PRFRAKT-IFYLLES-SW               
180500         END-IF                                                           
180600       END-IF                                                             
180700     END-IF                                                               
180800                                                                          
180900     IF PRFRAKT-OBLIGATORISK AND MOD-FLFRAKT-UPD NOT = YES                
181000       MOVE NOO                TO INDATA-SW                               
181100                                                                          
181200       IF MED-IDSKYLT = 'S  '                                             
181300         MOVE 'Fyll i Fraktkostnad' TO  MOD-TEMFSFEL                      
181400       ELSE                                                               
181500         MOVE 'Enter Freight Cost' TO  MOD-TEMFSFEL                       
181600       END-IF                                                             
181700                                                                          
181800       PERFORM MFS-DONT-TOUCH-FIELD-OUT                                   
181900       PERFORM MFS-DONT-TOUCH-FIELD-IN                                    
182000     ELSE                                                                 
182100                                                                          
182200** 4675 TAKES DATA FROM SAVE AREA                                         
182300       COMPUTE P-TO-P-KVLL = LENGTH OF PTOP-MID-W4I67501 + 17             
182400       MOVE ALL '+'            TO PTOP-MID-W4I67501                       
182500       MOVE LOW-VALUE          TO P-TO-P-KDZ1                             
182600       MOVE LOW-VALUE          TO P-TO-P-KDZ2                             
182700       MOVE 'W4T675  '         TO P-TO-P-KDTRANS                          
182800       MOVE '4676'             TO P-TO-P-IDTRANS                          
182900       MOVE MFS-KDMFSFOR       TO P-TO-P-KDMFSFOR                         
183000     END-IF                                                               
183100                                                                          
183200     .                                                                    
183300     EJECT                                                                
183400                                                                          
183500 J-CLOSE-FIELDS SECTION.                                                  
183600                                                                          
183700     MOVE MFS-CLOSE-FIELD TO MOD-PRFRAKT-ATTR                             
183800                             MOD-PRFOERS-ATTR                             
183900                             MOD-REFOERS-ATTR                             
184000                             MOD-REOVKOFF-ATTR                            
184100                             MOD-PRLEGKST-ATTR                            
184200                             MOD-RELEGKST-ATTR                            
184300                             MOD-PREMBHNT-ATTR                            
184400                             MOD-REEMBHNT-ATTR                            
184500                             MOD-PRAVDRAG-ATTR                            
184600                             MOD-REAVDRAG-ATTR                            
184700                             MOD-FLCREDL-ATTR                             
184800                             MOD-KDVALUTA-MAN-ATTR                        
184900                             MOD-PRKURS-MAN-ATTR                          
185000                                                                          
185100     MOVE MFS-ERASE-FIELD TO MOD-PRFRAKT                                  
185200                             MOD-PRFOERS                                  
185300                             MOD-REFOERS                                  
185400                             MOD-REOVKOFF                                 
185500                             MOD-PRLEGKST                                 
185600                             MOD-RELEGKST                                 
185700                             MOD-PREMBHNT                                 
185800                             MOD-REEMBHNT                                 
185900                             MOD-PRAVDRAG                                 
186000                             MOD-REAVDRAG                                 
186100                             MOD-FLCREDL                                  
186200                             MOD-KDVALISO-MAN                             
186300                             MOD-PRKURS-MAN                               
186400     .                                                                    
186500     EJECT                                                                
186600                                                                          
186700 S01-HANDLE-ALPHA-TO-NUM SECTION.                                         
186800                                                                          
186900     CALL WDECEDIT USING DEC-IDFRIDATA                                    
187000                         DEC-IDEDITDATA                                   
187100                         DEC-KVHELTAL                                     
187200                         DEC-KVDECIMAL                                    
187300                         DEC-KDSVAR                                       
187400     .                                                                    
187500     SKIP2                                                                
187600                                                                          
187700 S02-TEST-WDE122         SECTION.                                         
187800                                                                          
187900     MOVE YES    TO WDE122-ISRT-SW                                        
188000     PERFORM IMS-GHU-WDE101                                               
188100     PERFORM IMS-GHNP-WDE111                                              
188200     IF SEGMENT-FOUND                                                     
188300       IF INPUT-111                                                       
188400         IF MID-KDLEVVIL NOT = ALL '+'                                    
188500           MOVE MID-KDLEVVIL  TO SGMT-KDLEVVIL                            
188600           PERFORM IMS-REPL-WDE111                                        
188700         END-IF                                                           
188800       END-IF                                                             
188900       PERFORM IMS-GHNP-WDE122                                            
189000     ELSE                                                                 
189100       MOVE  4498-IDDISTR     TO  SGMT-IDDISTR                            
189200       MOVE  4498-IDKUNDNR    TO  SGMT-IDKUNDNR                           
189300       MOVE  'N'              TO  SGMT-FLCOD                              
189400       MOVE  W-IDDC           TO  SGMT-IDDC                               
189500       MOVE  SPACE            TO  SGMT-IDPARTNR                           
189600       MOVE  ZERO             TO  SGMT-KDFKBIL                            
189700                                  SGMT-KDFORSKN                           
189800       IF INPUT-111                                                       
189900         IF MID-KDLEVVIL NOT = ALL '+'                                    
190000           MOVE MID-KDLEVVIL  TO  SGMT-KDLEVVIL                           
190100         ELSE                                                             
190200           MOVE  -1           TO  SGMT-KDLEVVIL                           
190300         END-IF                                                           
190400       ELSE                                                               
190500         MOVE  -1             TO  SGMT-KDLEVVIL                           
190600       END-IF                                                             
190700                                                                          
190800       MOVE   9               TO  SGMT-KDORDKL-MAX                        
190900       MOVE  SPACE            TO  SGMT-KDVALISO                           
191000       MOVE  ZERO             TO  SGMT-PRKURS                             
191100       COMPUTE SGMT-TISKEPPN-9KOMPL                                       
191200                              =   W-9KOMPL - SHIP-TISKEPPN                
191300       PERFORM IMS-ISRT-WDE111                                            
191400       PERFORM IMS-GHU-WDE122                                             
191500     END-IF                                                               
191600                                                                          
191700     IF SEGMENT-FOUND                                                     
191800       MOVE NOO TO WDE122-ISRT-SW                                         
191900     ELSE                                                                 
192000       MOVE 1   TO TILL-KDSEGKEY                                          
192100                                                                          
192200       MOVE SPACE TO TILL-IDSIGILL                                        
192300                     TILL-BESLULEV                                        
192400                     TILL-IDBOKN                                          
192500                     TILL-IDLC                                            
192600                     TILL-IDLICENS                                        
192700                     TILL-IDVCERT                                         
192800                     TILL-KDVALISO-MAN                                    
192900                                                                          
193000       MOVE ZERO TO  TILL-PRFRAKT                                         
193100                     TILL-PRFOERS                                         
193200                     TILL-PRKURS-MAN                                      
193300                     TILL-PRLEGKST                                        
193400                     TILL-RELEGKST                                        
193500                     TILL-PREMBHNT                                        
193600                     TILL-REEMBHNT                                        
193700                     TILL-PRAVDRAG                                        
193800                     TILL-REAVDRAG                                        
193900                     TILL-REFOERS                                         
194000                     TILL-REOVKOFF                                        
194100                     TILL-TISKEPPN-MAN                                    
194200     END-IF                                                               
194300     .                                                                    
194400     SKIP2                                                                
194500                                                                          
194600 S03-TEST-WDE211         SECTION.                                         
194700                                                                          
194800     MOVE YES    TO WDE211-ISRT-SW                                        
194900                                                                          
195000     IF SEGMENT-FOUND                                                     
195100       MOVE NOO TO WDE211-ISRT-SW                                         
195200     ELSE                                                                 
195300       MOVE 4498-IDDISTR  TO BGMT-IDDISTR                                 
195400       MOVE 4498-IDKUNDNR TO BGMT-IDKUNDNR                                
195500                                                                          
195600       MOVE SPACE    TO BGMT-IDPARTNR                                     
195700                        BGMT-FLSEPINV                                     
195800       MOVE 'N'      TO BGMT-FLCOD                                        
195900                                                                          
196000       MOVE -1       TO BGMT-KDLEVVIL                                     
196100       MOVE ZERO     TO BGMT-PRAVDRAG                                     
196200                        BGMT-PREMBHNT                                     
196300                        BGMT-PRFOERS                                      
196400                        BGMT-PRFRAKT                                      
196500                        BGMT-PRLEGKST                                     
196600                        BGMT-REAVDRAG                                     
196700                        BGMT-REEMBHNT                                     
196800                        BGMT-REFOERS                                      
196900                        BGMT-RELEGKST                                     
197000                        BGMT-REOVKOFF                                     
197100     END-IF                                                               
197200                                                                          
197300     .                                                                    
197400     SKIP2                                                                
197500                                                                          
197600 S04-CREATE-IDSHIPM  SECTION.                                             
197700                                                                          
197800     CALL W476SHNO         USING SHNO-W476SHNO SHNO-4517-PCB              
197900                                                                          
198000     MOVE SHNO-IDSHIPM        TO W-IDSHIPM                                
198100                                 SAVE-IDSHIPM                             
198200     MOVE YES                 TO UPDATE-SAVE-SW                           
198300                                                                          
198400** CREATE WDE101                                                          
198500                                                                          
198600     MOVE W-IDSHIPM           TO SHIP-IDSHIPM                             
198700     MOVE W-IDTRPTNR          TO SHIP-IDTRPTNR                            
198800     MOVE W-IDLBBET           TO SHIP-IDLBBET                             
198900     MOVE W-IDDC              TO SHIP-IDDC                                
199000     MOVE W-IDLANDX2          TO SHIP-IDLANDX3-SEND                       
199100     MOVE SHIP-IDDC           TO WS-IDDC                                  
199200     IF NDC-CN OR LDC-CN                                                  
199300       IF 4498-KDFAKTYP = 'R' or 'G'                                      
199400         MOVE 'INV'           TO SHIP-KDFINDOC                            
199500       ELSE                                                               
199600         IF 4498-KDFAKTYP = 'N' OR 'K'                                    
199700           MOVE 'INT'         TO SHIP-KDFINDOC                            
199800         END-IF                                                           
199900       END-IF                                                             
200000     ELSE                                                                 
200100       IF 4498-KDFAKTYP = 'N' OR 'K'                                      
200200         MOVE 'INT'           TO SHIP-KDFINDOC                            
200300       ELSE                                                               
200400         MOVE 'INV'           TO SHIP-KDFINDOC                            
200500       END-IF                                                             
200600     END-IF                                                               
200700     MOVE DC-MSGI-TILOKDAT    TO SHIP-TISKEPPN                            
200800     MOVE DC-MSGI-TILOKTID    TO WS-TISKPTID-HHMM                         
200900     MOVE FUNCTION CURRENT-DATE(13:2) TO WS-TISKPTID-SS                   
201000     MOVE WS-TISKPTID         TO SHIP-TISKPTID                            
201100     MOVE ZERO                TO SHIP-KVANTEX                             
201200                                 SHIP-SUNTO-TOT                           
201300                                 SHIP-PRKURS-BET                          
201400     MOVE SAVE-FLSKRIV-NU     TO SHIP-FLSKRIV-NU                          
201500** AFTER ALL UPDATE AND RELEASE TRANSPORT SHIP-KDKLAR CHANGES             
201600** TO 'N' IN W4067500 PROGRAM                                             
201700     MOVE 'A'                 TO SHIP-KDKLAR                              
201800     MOVE SPACE               TO SHIP-IDDC-EXP                            
201900                                 SHIP-BELEVVIL                            
202000                                 SHIP-IDSYSTEM                            
202100                                 SHIP-KDVALISO-BET                        
202200     MOVE '0'                 TO SHIP-KDFAKSTA-EXP                        
202300                                                                          
202400     MOVE NOO                 TO SHIP-FLFARLIG                            
202500     MOVE SPACES              TO SHIP-KDVALISO-EXP                        
202600     MOVE ZEROES              TO SHIP-SUORDV-EXP                          
202700                                 SHIP-SUORDV-FAKT                         
202800                                 SHIP-VKORDBTO-FAKT                       
202900                                 SHIP-VLORDBTO-FAKT                       
203000                                                                          
203100     PERFORM IMS-ISRT-WDE101                                              
203200                                                                          
203300** CREATE WDE111                                                          
203400                                                                          
203500     MOVE  4498-IDDISTR     TO  SGMT-IDDISTR                              
203600     MOVE  4498-IDKUNDNR    TO  SGMT-IDKUNDNR                             
203700     MOVE  'N'              TO  SGMT-FLCOD                                
203800     MOVE  W-IDDC           TO  SGMT-IDDC                                 
203900     MOVE  SPACE            TO  SGMT-IDPARTNR                             
204000     MOVE  ZERO             TO  SGMT-KDFKBIL                              
204100                                SGMT-KDFORSKN                             
204200     IF INPUT-111                                                         
204300       IF MID-KDLEVVIL NOT = ALL '+'                                      
204400         MOVE MID-KDLEVVIL  TO  SGMT-KDLEVVIL                             
204500       ELSE                                                               
204600         MOVE -1            TO  SGMT-KDLEVVIL                             
204700       END-IF                                                             
204800     ELSE                                                                 
204900       MOVE  -1             TO  SGMT-KDLEVVIL                             
205000     END-IF                                                               
205100     MOVE   9               TO  SGMT-KDORDKL-MAX                          
205200     MOVE  SPACE            TO  SGMT-KDVALISO                             
205300     MOVE  ZERO             TO  SGMT-PRKURS                               
205400     COMPUTE SGMT-TISKEPPN-9KOMPL                                         
205500                            =   W-9KOMPL - SHIP-TISKEPPN                  
205600     PERFORM IMS-ISRT-WDE111                                              
205700                                                                          
205800** BUILD WDE122                                                           
205900                                                                          
206000     MOVE YES    TO WDE122-ISRT-SW                                        
206100     MOVE 1   TO TILL-KDSEGKEY                                            
206200                                                                          
206300     MOVE SPACE TO TILL-IDSIGILL                                          
206400                   TILL-BESLULEV                                          
206500                   TILL-IDBOKN                                            
206600                   TILL-IDLC                                              
206700                   TILL-IDLICENS                                          
206800                   TILL-IDVCERT                                           
206900                   TILL-KDVALISO-MAN                                      
207000                                                                          
207100     MOVE ZERO TO  TILL-PRFRAKT                                           
207200                   TILL-PRFOERS                                           
207300                   TILL-PRKURS-MAN                                        
207400                   TILL-PRLEGKST                                          
207500                   TILL-RELEGKST                                          
207600                   TILL-PREMBHNT                                          
207700                   TILL-REEMBHNT                                          
207800                   TILL-PRAVDRAG                                          
207900                   TILL-REAVDRAG                                          
208000                   TILL-REFOERS                                           
208100                   TILL-REOVKOFF                                          
208200                   TILL-TISKEPPN-MAN                                      
208300                                                                          
208400** BUILD AND ISRT WDE201                                                  
208500     MOVE W-IDSHIPM      TO BILL-IDSHIPM                                  
208600     MOVE W-IDDC         TO BILL-IDDC                                     
208700     MOVE W-IDLANDX2     TO BILL-IDLANDX3-SEND                            
208800     MOVE ZERO           TO BILL-IDLEVNR                                  
208900     MOVE BILL-IDDC      TO WS-IDDC                                       
209000     IF NDC-CN OR LDC-CN                                                  
209100       IF 4498-KDFAKTYP = 'R' OR 'G'                                      
209200         MOVE 'INV'      TO BILL-KDFINDOC                                 
209300       ELSE                                                               
209400         IF 4498-KDFAKTYP = 'N' OR 'K'                                    
209500           MOVE 'INT'      TO BILL-KDFINDOC                               
209600         END-IF                                                           
209700       END-IF                                                             
209800     ELSE                                                                 
209900       IF 4498-KDFAKTYP = 'N' OR 'K'                                      
210000         MOVE 'INT'      TO BILL-KDFINDOC                                 
210100       ELSE                                                               
210200         MOVE 'INV'      TO BILL-KDFINDOC                                 
210300       END-IF                                                             
210400     END-IF                                                               
210500     MOVE DC-MSGI-TILOKDAT  TO BILL-TISKEPPN                              
210600     MOVE WS-TISKPTID       TO BILL-TISKPTID                              
210700** AFTER ALL UPDATE AND RELEASE TRANSPORT BILL-IDDC-EXP                   
210800** IS UPDATED IN W4067500 PROGRAM                                         
210900     MOVE SPACE             TO BILL-IDDC-EXP                              
211000     PERFORM IMS-ISRT-WDE201                                              
211100                                                                          
211200** BUILD WDE211                                                           
211300     MOVE 4498-IDDISTR  TO BGMT-IDDISTR                                   
211400     IF FLSAMFAK                                                          
211500       MOVE ZERO          TO BGMT-IDKUNDNR                                
211600     ELSE                                                                 
211700       MOVE 4498-IDKUNDNR TO BGMT-IDKUNDNR                                
211800     END-IF                                                               
211900                                                                          
212000     MOVE SPACE    TO BGMT-IDPARTNR                                       
212100                      BGMT-FLSEPINV                                       
212200     MOVE 'N'      TO BGMT-FLCOD                                          
212300                                                                          
212400     MOVE -1       TO BGMT-KDLEVVIL                                       
212500     MOVE ZERO     TO BGMT-PRAVDRAG                                       
212600                      BGMT-PREMBHNT                                       
212700                      BGMT-PRFOERS                                        
212800                      BGMT-PRFRAKT                                        
212900                      BGMT-PRLEGKST                                       
213000                      BGMT-REAVDRAG                                       
213100                      BGMT-REEMBHNT                                       
213200                      BGMT-REFOERS                                        
213300                      BGMT-RELEGKST                                       
213400                      BGMT-REOVKOFF                                       
213500                                                                          
213600     MOVE YES    TO WDE211-ISRT-SW                                        
213700                                                                          
213800     .                                                                    
213900     EJECT                                                                
214000                                                                          
214100 S05-DC-LAND  SECTION.                                                    
214200                                                                          
214300     IF W-IDDC NOT = W-IDDC-B6                                            
214400        MOVE W-IDDC TO W-IDDC-B6                                          
214500        PERFORM IMS-GU-WDB601                                             
214600     END-IF                                                               
214700     MOVE DCS-IDLANDX2 TO W-IDLANDX2                                      
214800     .                                                                    
214900     EJECT                                                                
215000                                                                          
215100 S06-CHECK-DATE SECTION.                                                  
215200                                                                          
215300     MOVE 'AAMMDD'             TO DAT-KDDATFORM                           
215400     MOVE MID-TISKEPPN-MAN     TO DAT-I-TIDATUM                           
215500                                                                          
215600     CALL WDATKONV          USING DAT-KDDATFORM                           
215700                                  DAT-I-TIDATUM                           
215800                                  DAT-O-TIDATUM                           
215900                                  DAT-KDSVAR                              
216000     .                                                                    
216100     EJECT                                                                
216200                                                                          
216300 S07-CHECK-SPACE-INPUT SECTION.                                           
216400                                                                          
216500** SPACE INPUT WILL BE IGNORED                                            
216600                                                                          
216700     IF MID-KDVALUTA-MAN = SPACE                                          
216800       MOVE ALL '+'  TO MID-KDVALUTA-MAN                                  
216900     END-IF                                                               
217000     IF MID-PRKURS-MAN = SPACE                                            
217100       MOVE ALL '+'  TO MID-PRKURS-MAN                                    
217200     END-IF                                                               
217300     IF MID-PRLEGKST = SPACE                                              
217400       MOVE ALL '+'  TO MID-PRLEGKST                                      
217500     END-IF                                                               
217600     IF MID-PREMBHNT = SPACE                                              
217700       MOVE ALL '+'  TO MID-PREMBHNT                                      
217800     END-IF                                                               
217900     IF MID-REEMBHNT = SPACE                                              
218000       MOVE ALL '+'  TO MID-REEMBHNT                                      
218100     END-IF                                                               
218200     IF MID-PRFOERS  = SPACE                                              
218300       MOVE ALL '+'  TO MID-PRFOERS                                       
218400     END-IF                                                               
218500     IF MID-REFOERS  = SPACE                                              
218600       MOVE ALL '+'  TO MID-REFOERS                                       
218700     END-IF                                                               
218800     IF MID-REOVKOFF = SPACE                                              
218900       MOVE ALL '+'  TO MID-REOVKOFF                                      
219000     END-IF                                                               
219100     IF MID-PRAVDRAG = SPACE                                              
219200       MOVE ALL '+'  TO MID-PRAVDRAG                                      
219300     END-IF                                                               
219400     IF MID-REAVDRAG = SPACE                                              
219500       MOVE ALL '+'  TO MID-REAVDRAG                                      
219600     END-IF                                                               
219700     .                                                                    
219800     EJECT                                                                
219900                                                                          
220000 S10-WRONG-PICTURE-MESSAGE SECTION.                                       
220100     SKIP2                                                                
220200* *****************************************************                   
220300*                                                     *                   
220400* GIVE WRONG PICTURE MESSAGE FROM WHELP               *                   
220500*                                                     *                   
220600* *****************************************************                   
220700     SKIP2                                                                
220800     MOVE 'W0O50401'          TO MFS-IDMOD                                
220900     MOVE MFS-ERASE-FIELD     TO MOD0504-IDTRANS                          
221000     MOVE FELMEDD-ENGLISH     TO MOD0504-TEMFSINF                         
221100     MOVE MSG-KVLL-TILL-WHELP TO MSG-KVLL                                 
221200     PERFORM IMS-INSERT-MSG                                               
221300     .                                                                    
221400     EJECT                                                                
221500     EJECT                                                                
221600 MFS-ERASE-FIELD-OUT SECTION.                                             
221700                                                                          
221800*    --- ALLA UTDATA-FÄLT                                                 
221900     MOVE MFS-ERASE-FIELD TO MOD-IDTRPTNR-UT                              
222000                             MOD-IDLBBET-UT                               
222100                             MOD-FLFARLIG-UT                              
222200                             MOD-IDDC-UT                                  
222300     .                                                                    
222400     SKIP3                                                                
222500 MFS-ERASE-FIELD-LINE SECTION.                                            
222600                                                                          
222700     MOVE MFS-ERASE-FIELD TO MOD-FLSEPINV                                 
222800                             MOD-PRFRAKT                                  
222900                             MOD-PRFOERS                                  
223000                             MOD-REFOERS                                  
223100                             MOD-REOVKOFF                                 
223200                             MOD-PRLEGKST                                 
223300                             MOD-RELEGKST                                 
223400                             MOD-PREMBHNT                                 
223500                             MOD-REEMBHNT                                 
223600                             MOD-PRAVDRAG                                 
223700                             MOD-REAVDRAG                                 
223800                             MOD-FLCREDL                                  
223900                             MOD-KDVALISO-MAN                             
224000                             MOD-PRKURS-MAN                               
224100                             MOD-IDSIGILL                                 
224200                             MOD-TISKEPPN-MAN                             
224300                             MOD-IDLC                                     
224400                             MOD-IDLICENS                                 
224500                             MOD-IDBOKN                                   
224600                             MOD-IDVCERT                                  
224700                             MOD-BESLULEV                                 
224800                             MOD-KDLEVVIL                                 
224900                             MOD-VKORDBTO                                 
225000                             MOD-VLORDBTO                                 
225100                             MOD-SUORDV                                   
225200     .                                                                    
225300     SKIP3                                                                
225400                                                                          
225500 MFS-ERASE-FIELD-IN SECTION.                                              
225600                                                                          
225700*    --- ALLA INDATA-FÄLT                                                 
225800     MOVE MFS-ERASE-FIELD TO MOD-IDTRPTNR-IN                              
225900                             MOD-IDLBBET-IN                               
226000                             MOD-FLFARLIG-IN                              
226100                             MOD-IDDC-IN                                  
226200     .                                                                    
226300     EJECT                                                                
226400 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
226500                                                                          
226600*    --- ALLA UTDATA-FÄLT                                                 
226700     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDTRPTNR-UT                       
226800                                    MOD-IDLBBET-UT                        
226900                                    MOD-FLFARLIG-UT                       
227000                                    MOD-IDDC-UT                           
227100                                    MOD-IDDISTR                           
227200                                    MOD-IDKUNDNR                          
227300                                    MOD-FLSEPINV                          
227400                                    MOD-IDSIGILL                          
227500                                    MOD-TISKEPPN-MAN                      
227600                                    MOD-IDLC                              
227700                                    MOD-IDLICENS                          
227800                                    MOD-IDBOKN                            
227900                                    MOD-IDVCERT                           
228000                                    MOD-BESLULEV                          
228100                                    MOD-KDLEVVIL                          
228200                                                                          
228300     IF NOT DIST79-DEALER-PRICE AND                                       
228400        NOT DIST79-ECOM-PRICE                                             
228500       MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-PRFRAKT                         
228600                                      MOD-PRFOERS                         
228700                                      MOD-REFOERS                         
228800                                      MOD-REOVKOFF                        
228900                                      MOD-PRLEGKST                        
229000                                      MOD-RELEGKST                        
229100                                      MOD-PREMBHNT                        
229200                                      MOD-REEMBHNT                        
229300                                      MOD-PRAVDRAG                        
229400                                      MOD-REAVDRAG                        
229500                                      MOD-FLCREDL                         
229600                                      MOD-KDVALUTA-MAN                    
229700                                      MOD-KDVALISO-MAN                    
229800                                      MOD-PRKURS-MAN                      
229900     END-IF                                                               
230000                                                                          
230100     .                                                                    
230200     SKIP3                                                                
230300 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
230400                                                                          
230500*    --- ALLA INDATA-FÄLT                                                 
230600     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDTRPTNR-IN                       
230700                                    MOD-IDLBBET-IN                        
230800                                    MOD-FLFARLIG-IN                       
230900                                    MOD-IDDC-IN                           
231000     .                                                                    
231100     EJECT                                                                
231200 MFS-FORM-ATTR SECTION.                                                   
231300                                                                          
231400*    --- ALL INDATA-FIELDS                                                
231500     MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-FLSEPINV-ATTR                    
231600                                     MOD-IDSIGILL-ATTR                    
231700                                     MOD-TISKEPPN-MAN-ATTR                
231800                                     MOD-IDLC-ATTR                        
231900                                     MOD-IDLICENS-ATTR                    
232000                                     MOD-IDBOKN-ATTR                      
232100                                     MOD-IDVCERT-ATTR                     
232200                                     MOD-BESLULEV-ATTR                    
232300                                     MOD-KDLEVVIL-ATTR                    
232400                                                                          
232500     IF NOT DIST79-DEALER-PRICE AND                                       
232600        NOT DIST79-ECOM-PRICE                                             
232700       MOVE MFS-FORMAT-DEFAULT-ATTR                                       
232800                                  TO MOD-PRFRAKT-ATTR                     
232900                                     MOD-PRFOERS-ATTR                     
233000                                     MOD-REFOERS-ATTR                     
233100                                     MOD-REOVKOFF-ATTR                    
233200                                     MOD-PRLEGKST-ATTR                    
233300                                     MOD-RELEGKST-ATTR                    
233400                                     MOD-PREMBHNT-ATTR                    
233500                                     MOD-REEMBHNT-ATTR                    
233600                                     MOD-PRAVDRAG-ATTR                    
233700                                     MOD-REAVDRAG-ATTR                    
233800                                     MOD-FLCREDL-ATTR                     
233900                                     MOD-KDVALUTA-MAN-ATTR                
234000                                     MOD-PRKURS-MAN-ATTR                  
234100                                     MOD-FLFRAKT-UPD-ATTR                 
234200     END-IF                                                               
234300     .                                                                    
234400     SKIP2                                                                
234500* --- IMS SECTIONS ---                                                    
234600     SKIP3                                                                
234700 IMS-GET-MSG SECTION.                                                     
234800                                                                          
234900     MOVE '  QC' TO GOOD-STATUSCODES                                      
235000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
235100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
235200     PERFORM IMS-STATUSCHECK                                              
235300     .                                                                    
235400     SKIP3                                                                
235500 IMS-INSERT-MSG SECTION.                                                  
235600                                                                          
235700*    IF MSGI-IDLAND-SPR = 'SE'                                            
235800*      MOVE '0' TO MFS-KDHUVOMR                                           
235900*    END-IF                                                               
236000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
236100     MOVE SPACE TO GOOD-STATUSCODES                                       
236200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
236300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
236400     PERFORM IMS-STATUSCHECK                                              
236500     .                                                                    
236600     EJECT                                                                
236700 IMS-ISRT-MSG-ALT SECTION.                                                
236800                                                                          
236900     MOVE SPACE TO GOOD-STATUSCODES                                       
237000     CALL CBLTDLI USING ISRT ALT-PCB P-TO-P-SW                            
237100     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
237200     PERFORM IMS-STATUSCHECK                                              
237300     .                                                                    
237400     SKIP3                                                                
237500 IMS-GHU-WDE101 SECTION.                                                  
237600                                                                          
237700     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
237800          DELIMITED BY SIZE INTO SSA1                                     
237900     MOVE '  ' TO GOOD-STATUSCODES                                        
238000     CALL CBLTDLI USING GHU WDE1-PCB DLI-IO-WDE101 SSA1                   
238100     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
238200     PERFORM IMS-STATUSCHECK                                              
238300     .                                                                    
238400     SKIP3                                                                
238500 IMS-GHNP-WDE111 SECTION.                                                 
238600                                                                          
238700     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
238800          DELIMITED BY SIZE INTO SSA1                                     
238900     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
239000          DELIMITED BY SIZE INTO SSA2                                     
239100     MOVE '  GE' TO GOOD-STATUSCODES                                      
239200     CALL CBLTDLI USING GHNP WDE1-PCB DLI-IO-WDE111 SSA1 SSA2             
239300     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
239400     PERFORM IMS-STATUSCHECK                                              
239500     .                                                                    
239600     SKIP3                                                                
239700 IMS-REPL-WDE111 SECTION.                                                 
239800                                                                          
239900     MOVE '  ' TO GOOD-STATUSCODES                                        
240000     CALL CBLTDLI USING REPL WDE1-PCB DLI-IO-WDE111                       
240100     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
240200     PERFORM IMS-STATUSCHECK                                              
240300     .                                                                    
240400     EJECT                                                                
240500                                                                          
240600 IMS-GHNP-WDE122 SECTION.                                                 
240700                                                                          
240800     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
240900          DELIMITED BY SIZE INTO SSA1                                     
241000     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
241100          DELIMITED BY SIZE INTO SSA2                                     
241200     STRING 'WDE122  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
241300          DELIMITED BY SIZE INTO SSA3                                     
241400     MOVE '  GE' TO GOOD-STATUSCODES                                      
241500     CALL CBLTDLI USING GHNP WDE1-PCB DLI-IO-WDE122 SSA1 SSA2 SSA3        
241600     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
241700     PERFORM IMS-STATUSCHECK                                              
241800     .                                                                    
241900     SKIP3                                                                
242000 IMS-GHU-WDE122 SECTION.                                                  
242100                                                                          
242200     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
242300          DELIMITED BY SIZE INTO SSA1                                     
242400     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
242500          DELIMITED BY SIZE INTO SSA2                                     
242600     STRING 'WDE122  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
242700          DELIMITED BY SIZE INTO SSA3                                     
242800     MOVE '  GE' TO GOOD-STATUSCODES                                      
242900     CALL CBLTDLI USING GHU WDE1-PCB DLI-IO-WDE122 SSA1 SSA2 SSA3         
243000     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
243100     PERFORM IMS-STATUSCHECK                                              
243200     .                                                                    
243300     SKIP3                                                                
243400 IMS-GHU-WDE122-DIST SECTION.                                             
243500                                                                          
243600     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
243700          DELIMITED BY SIZE INTO SSA1                                     
243800     STRING 'WDE111  (WDE111KY>=' W-WDE111KY-MIN                          
243900                    '&WDE111KY<=' W-WDE111KY-MAX ')'                      
244000          DELIMITED BY SIZE INTO SSA2                                     
244100     STRING 'WDE122  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
244200          DELIMITED BY SIZE INTO SSA3                                     
244300     MOVE '  GE' TO GOOD-STATUSCODES                                      
244400     CALL CBLTDLI USING GHU WDE1-PCB DLI-IO-WDE122 SSA1 SSA2 SSA3         
244500     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
244600     PERFORM IMS-STATUSCHECK                                              
244700     .                                                                    
244800     EJECT                                                                
244900 IMS-GHU-WDE201 SECTION.                                                  
245000                                                                          
245100     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
245200          DELIMITED BY SIZE INTO SSA1                                     
245300     MOVE '  ' TO GOOD-STATUSCODES                                        
245400     CALL CBLTDLI USING GHU WDE2-PCB DLI-IO-WDE201 SSA1                   
245500     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
245600     PERFORM IMS-STATUSCHECK                                              
245700     .                                                                    
245800     SKIP3                                                                
245900 IMS-GHNP-WDE211 SECTION.                                                 
246000                                                                          
246100     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
246200          DELIMITED BY SIZE INTO SSA1                                     
246300     STRING 'WDE211  (WDE211KY =' W-WDE211KY-X ')'                        
246400          DELIMITED BY SIZE INTO SSA2                                     
246500     MOVE '  GE' TO GOOD-STATUSCODES                                      
246600     CALL CBLTDLI USING GHNP WDE2-PCB DLI-IO-WDE211 SSA1 SSA2             
246700     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
246800     PERFORM IMS-STATUSCHECK                                              
246900     .                                                                    
247000     SKIP3                                                                
247100 IMS-GHU-WDE211 SECTION.                                                  
247200                                                                          
247300     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
247400          DELIMITED BY SIZE INTO SSA1                                     
247500     STRING 'WDE211  (WDE211KY =' W-WDE211KY-X ')'                        
247600          DELIMITED BY SIZE INTO SSA2                                     
247700     MOVE '  GE' TO GOOD-STATUSCODES                                      
247800     CALL CBLTDLI USING GHU WDE2-PCB DLI-IO-WDE211 SSA1 SSA2              
247900     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
248000     PERFORM IMS-STATUSCHECK                                              
248100     .                                                                    
248200     SKIP3                                                                
248300 IMS-GHU-WDE211-DIST SECTION.                                             
248400                                                                          
248500     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
248600          DELIMITED BY SIZE INTO SSA1                                     
248700     STRING 'WDE211  (WDE211KY>=' W-WDE211KY-MIN                          
248800                    '&WDE211KY<=' W-WDE211KY-MAX ')'                      
248900          DELIMITED BY SIZE INTO SSA2                                     
249000     MOVE '  GE' TO GOOD-STATUSCODES                                      
249100     CALL CBLTDLI USING GHU WDE2-PCB DLI-IO-WDE211 SSA1 SSA2              
249200     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
249300     PERFORM IMS-STATUSCHECK                                              
249400     .                                                                    
249500     SKIP3                                                                
249600 IMS-ISRT-WDE101 SECTION.                                                 
249700                                                                          
249800     MOVE 'WDE101  ' TO SSA1                                              
249900     MOVE '  ' TO GOOD-STATUSCODES                                        
250000     CALL CBLTDLI USING ISRT WDE1-PCB DLI-IO-WDE101 SSA1                  
250100     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
250200     PERFORM IMS-STATUSCHECK                                              
250300     .                                                                    
250400     EJECT                                                                
250500 IMS-ISRT-WDE111 SECTION.                                                 
250600                                                                          
250700     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
250800          DELIMITED BY SIZE INTO SSA1                                     
250900     MOVE 'WDE111  ' TO SSA2                                              
251000     MOVE '  ' TO GOOD-STATUSCODES                                        
251100     CALL CBLTDLI USING ISRT WDE1-PCB DLI-IO-WDE111 SSA1 SSA2             
251200     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
251300     PERFORM IMS-STATUSCHECK                                              
251400     .                                                                    
251500     EJECT                                                                
251600 IMS-ISRT-WDE122 SECTION.                                                 
251700                                                                          
251800     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
251900          DELIMITED BY SIZE INTO SSA1                                     
252000     STRING 'WDE111  (WDE111KY =' W-WDE111KY-X ')'                        
252100          DELIMITED BY SIZE INTO SSA2                                     
252200     MOVE 'WDE122 ' TO SSA3                                               
252300     MOVE '  II' TO GOOD-STATUSCODES                                      
252400     CALL CBLTDLI USING ISRT WDE1-PCB DLI-IO-WDE122 SSA1 SSA2 SSA3        
252500     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
252600     PERFORM IMS-STATUSCHECK                                              
252700     .                                                                    
252800     SKIP3                                                                
252900 IMS-REPL-WDE122 SECTION.                                                 
253000                                                                          
253100     MOVE '  ' TO GOOD-STATUSCODES                                        
253200     CALL CBLTDLI USING REPL WDE1-PCB DLI-IO-WDE122                       
253300     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
253400     PERFORM IMS-STATUSCHECK                                              
253500     .                                                                    
253600     EJECT                                                                
253700                                                                          
253800 IMS-ISRT-WDE201 SECTION.                                                 
253900                                                                          
254000     MOVE 'WDE201  ' TO SSA1                                              
254100     MOVE '  ' TO GOOD-STATUSCODES                                        
254200     CALL CBLTDLI USING ISRT WDE2-PCB DLI-IO-WDE201 SSA1                  
254300     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
254400     PERFORM IMS-STATUSCHECK                                              
254500     .                                                                    
254600     EJECT                                                                
254700                                                                          
254800 IMS-ISRT-WDE211 SECTION.                                                 
254900                                                                          
255000     STRING 'WDE201  (IDSHIPM  =' W-IDSHIPM-X ')'                         
255100          DELIMITED BY SIZE INTO SSA1                                     
255200     MOVE 'WDE211 ' TO SSA2                                               
255300     MOVE '  II' TO GOOD-STATUSCODES                                      
255400     CALL CBLTDLI USING ISRT WDE2-PCB DLI-IO-WDE211 SSA1 SSA2             
255500     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
255600     PERFORM IMS-STATUSCHECK                                              
255700     .                                                                    
255800     SKIP3                                                                
255900                                                                          
256000 IMS-REPL-WDE211 SECTION.                                                 
256100                                                                          
256200     MOVE '  ' TO GOOD-STATUSCODES                                        
256300     CALL CBLTDLI USING REPL WDE2-PCB DLI-IO-WDE211                       
256400     MOVE WDE2-STATUS-CODE TO STATUS-WS                                   
256500     PERFORM IMS-STATUSCHECK                                              
256600     .                                                                    
256700     EJECT                                                                
256800 IMS-GU-WDGX4496   SECTION.                                               
256900                                                                          
257000     STRING 'WDR401  *P(WDGXKEY  =' W-4495-X ')'                          
257100                      DELIMITED BY SIZE INTO SSA1                         
257200     MOVE   'WDGX4496 '                   TO SSA2                         
257300     MOVE '  GE' TO GOOD-STATUSCODES                                      
257400     CALL CBLTDLI USING GU 4495-PCB DLI-IO-WDGX4496 SSA1 SSA2             
257500     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
257600     PERFORM IMS-STATUSCHECK                                              
257700     .                                                                    
257800                                                                          
257900 IMS-GU-WDGX4498   SECTION.                                               
258000                                                                          
258100     STRING 'WDR401  *P(WDGXKEY  =' W-4495-X ')'                          
258200                      DELIMITED BY SIZE INTO SSA1                         
258300     STRING 'WDGX4498(WDGXKEY >=' W-4498-MIN                              
258400                    '&WDGXKEY <=' W-4498-MAX  ')'                         
258500                      DELIMITED BY SIZE INTO SSA2                         
258600     MOVE '  GE' TO GOOD-STATUSCODES                                      
258700     CALL CBLTDLI USING GU 4495-PCB DLI-IO-WDGX4498 SSA1 SSA2             
258800     MOVE 4495-STATUS-CODE TO STATUS-WS                                   
258900     PERFORM IMS-STATUSCHECK                                              
259000     .                                                                    
259100     SKIP2                                                                
259200 IMS-GU-WDB601    SECTION.                                                
259300     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
259400          DELIMITED BY SIZE INTO SSA1                                     
259500     MOVE '  GE' TO GOOD-STATUSCODES                                      
259600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
259700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
259800     PERFORM IMS-STATUSCHECK                                              
259900     IF SEGMENT-MISSING                                                   
260000         MOVE SPACE TO DCS-KDDC                                           
260100                       DCS-IDLANDX2                                       
260200     END-IF                                                               
260300     .                                                                    
260400 IMS-STATUSCHECK SECTION.                                                 
260500                                                                          
260600     SET STATUS-IX TO 1                                                   
260700     SEARCH GOOD-STATUS                                                   
260800       AT END                                                             
260900         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
261000         DELIMITED BY SIZE INTO ERROR-TEXT                                
261100         CALL FELLOG                                                      
261200       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
261300         CONTINUE                                                         
261400     END-SEARCH                                                           
261500     .                                                                    
