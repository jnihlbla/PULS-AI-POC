000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6121F00.                                                
000300 AUTHOR.         BODIL LINDAHL.                                           
000400 DATE-WRITTEN.   JANUARI 2007.                                            
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        LÄSER FIL W6121F (AVVIKELSE VID REFILL INLEVERANS)               
001000*        SKAPADE UNDER VECKAN OCH SKAPAR LISTA.                           
001100*                                                                         
001200*    ABENDCODES:                                                          
001300*        U1000 - D&P ERROR                                                
001400*                                                                         
001500                                                                          
001600     SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300*          --- INFIL                                                      
002400     SELECT W6121E                     ASSIGN TO W6121FD1.                
002500     SKIP2                                                                
002600*          --- INFIL                                                      
002700     SELECT W6121F                     ASSIGN TO W6121FD2.                
002800     SKIP2                                                                
002900 DATA DIVISION.                                                           
003000     SKIP3                                                                
003100 FILE SECTION.                                                            
003200     SKIP3                                                                
003300 FD  W6121E                                                               
003400     RECORDING       F                                                    
003500     BLOCK CONTAINS  0.                                                   
003600*01  -COPY W6121E    -L.                                                  
003700     SKIP3                                                                
003800 FD  W6121F                                                               
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100*01  -COPY W61247    -L.                                                  
004200     SKIP3                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400                                                                          
004500 77  IDPGM                       PIC X(8)    VALUE 'W6121F00'.            
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800 77  IX                          PIC S9(5)   VALUE ZERO COMP-3.           
004900 77  IX-MAX                      PIC S9(5)   VALUE +1000 COMP-3.          
005000 77  IX7                         PIC S9(5)   VALUE ZERO COMP-3.           
005100 77  IX7-MAX                      PIC S9(5)  VALUE +1000 COMP-3.          
005200 77  SPAR-IDDC-REC               PIC X(2)    VALUE SPACE.                 
005300 77  SPAR-IDDC-SEND              PIC X(2)    VALUE SPACE.                 
005400 77  WS2-IDDC-SEND               PIC X(2)    VALUE SPACE.                 
005500 77  WS2-IDDC-REC                PIC X(2)    VALUE SPACE.                 
005600 77  SPAR-IDFAKT                 PIC 9(7)    VALUE ZERO.                  
005700 77  SPAR-IDKUNDRF               PIC X(10)   VALUE SPACE.                 
005800 77  SPAR-IDKUNDNR               PIC 9(7)    VALUE ZERO.                  
005900 77  SPAR-IDKOLLI                PIC 9(5)    VALUE ZERO.                  
006000 77  SPAR-KDSORT1                PIC 9(1)    VALUE ZERO.                  
006100 77  WS-IDDC-REC                 PIC X(2)    VALUE SPACE.                 
006200 77  WS-IDDC-SEND                PIC X(2)    VALUE SPACE.                 
006300 77  TEST-SEND                   PIC X(2)    VALUE SPACE.                 
006400 77  TEST-REC                    PIC X(2)    VALUE SPACE.                 
006500 77  TEST-ANTAL                  PIC 9(5)    VALUE ZERO.                  
006600 77  SW-TRAFF                    PIC X       VALUE SPACE.                 
006700 77  SW-OK                       PIC X       VALUE SPACE.                 
006800 77  SW-TOT                      PIC X       VALUE 'N'.                   
006900 77  INDX                        PIC S9(3)   VALUE +000 COMP SYNC.        
007000 77  KDRC-DISPLAY                PIC Z(5).                                
007100                                                                          
007200 01  TAB-DC-TABELL.                                                       
007300     03  TAB-DC-RAD OCCURS 1000.                                          
007400         05 TAB-IDDC-REC         PIC  X(2) VALUE SPACE.                   
007500         05 TAB-IDDC-SEND        PIC  X(2) VALUE SPACE.                   
007600         05 TAB-ANTAL            PIC  9(5) VALUE ZERO.                    
007700                                                                          
007800 01  TAB-DC-KONTROLL.                                                     
007900     03  TAB-KONTR-RAD OCCURS 1000.                                       
008000         05 TAB-KONTR-REC        PIC  X(2) VALUE SPACE.                   
008100         05 TAB-KONTR-SEND       PIC  X(2) VALUE SPACE.                   
008200         05 TAB-KONTR-ANTAL      PIC  9(5) VALUE ZERO.                    
014900                                                                          
015700 77  WS-ANTAL-OVERLEV            PIC 9(5)  VALUE ZERO.                    
015800 77  WS-ANTAL-OVERLEV-DC         PIC 9(5)  VALUE ZERO.                    
015900 77  WS-ANTAL-UNDERLEV           PIC 9(5)  VALUE ZERO.                    
016000 77  WS-ANTAL-UNDERLEV-00        PIC 9(5)  VALUE ZERO.                    
016100 77  WS-ANTAL-UNDERLEV-XX        PIC 9(5)  VALUE ZERO.                    
016200 77  WS-ANTAL-UNDERLEV-DC        PIC 9(5)  VALUE ZERO.                    
016300 77  WS-ANTAL-UNDERLEV-DC-00     PIC 9(5)  VALUE ZERO.                    
016400 77  WS-ANTAL-UNDERLEV-DC-XX     PIC 9(5)  VALUE ZERO.                    
016500 77  WS-ANTAL-DAM                PIC 9(5)  VALUE ZERO.                    
016600 77  WS-ANTAL-DAM-00             PIC 9(5)  VALUE ZERO.                    
016700 77  WS-ANTAL-DAM-XX             PIC 9(5)  VALUE ZERO.                    
016800 77  WS-ANTAL-DAM-DC             PIC 9(5)  VALUE ZERO.                    
016900 77  WS-ANTAL-DAM-DC-00          PIC 9(5)  VALUE ZERO.                    
017000 77  WS-ANTAL-DAM-DC-XX          PIC 9(5)  VALUE ZERO.                    
017100 77  WS-ANTAL-LOST               PIC 9(5)  VALUE ZERO.                    
017200 77  WS-ANTAL-LOST-DC            PIC 9(5)  VALUE ZERO.                    
017300 77  WS-ANTAL-LOST-KOLLI         PIC 9(5)  VALUE ZERO.                    
017400 77  WS-ANTAL-LOST-KOLLI-DC      PIC 9(5)  VALUE ZERO.                    
017500 77  WS-ANTAL-FOUND              PIC 9(5)  VALUE ZERO.                    
017600 77  WS-ANTAL-FOUND-DC           PIC 9(5)  VALUE ZERO.                    
017700 77  WS-ANTAL-FOUND-KOLLI        PIC 9(5)  VALUE ZERO.                    
017800 77  WS-ANTAL-FOUND-KOLLI-DC     PIC 9(5)  VALUE ZERO.                    
017900 77  WS-ANTAL-NY                 PIC 9(5)  VALUE ZERO.                    
018000 77  WS-ANTAL-NY-DC              PIC 9(5)  VALUE ZERO.                    
018100 77  WS-ANTAL                    PIC 9(7)  VALUE ZERO.                    
018200 77  WS-ANTAL-DC                 PIC 9(7)  VALUE ZERO.                    
018300 77  WS-SUMMA-ERR                PIC 9(7)  VALUE ZERO.                    
018400 77  WS-SUMMA-ERR-DC             PIC 9(7)  VALUE ZERO.                    
018500 77  WS-SUMMA-ERR-KOLLI          PIC 9(7)  VALUE ZERO.                    
018600 77  WS-SUMMA-ERR-KOLLI-DC       PIC 9(7)  VALUE ZERO.                    
018700 77  WS-SUMMA-TOT                PIC 9(7)  VALUE ZERO.                    
018800 77  WS-SUMMA-TOT-DC             PIC 9(7)  VALUE ZERO.                    
018900 77  WS-SUMMA-BIN                PIC 9(7)  VALUE ZERO.                    
019000 77  WS-SUMMA-BIN-DC             PIC 9(7)  VALUE ZERO.                    
019100 77  WS-SUMMA-TOT-BIN            PIC S9(7) VALUE ZERO.                    
019200 77  WS-SUMMA-TOT-BIN-DC         PIC S9(7) VALUE ZERO.                    
019300 77  WS-SUMMA-ANTMOT             PIC 9(7)  VALUE ZERO.                    
019400 77  WS-SUMMA-ANTMOT-KOLLI       PIC 9(7)  VALUE ZERO.                    
019500 77  WS-SUMMA-ANTMOT-DC          PIC 9(7)  VALUE ZERO.                    
019600 77  WS-SUMMA-ANTMOT-KOLLI-DC    PIC 9(7)  VALUE ZERO.                    
019700 77  WS-CORR-PROC                PIC S9(3)V9(1) VALUE ZERO COMP-3.        
019800 77  WS-FEL-PROC                 PIC S9(3)V9(1) VALUE ZERO COMP-3.        
019900                                                                          
020000 77  W6121F-EOF-SW               PIC X       VALUE 'N'.                   
020100     88  END-OF-W6121F                       VALUE 'Y'.                   
020200                                                                          
020300 77  W6121E-EOF-SW               PIC X       VALUE 'N'.                   
020400     88  END-OF-W6121E                       VALUE 'Y'.                   
020500     EJECT                                                                
020600*01  -COPY WDATAREA                                                       
020700     EJECT                                                                
020800 01  HDR-AREA.                                                            
020900*    03  -COPY WZ01REQU                                                   
021000*    03  -COPY WZ04HDR                                                    
021100     EJECT                                                                
021200 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
021300 01  SEND-AREA.                                                           
021400*    03  -COPY WZ01SEND                                                   
021500     EJECT                                                                
021600 01  SEND-RAD-STYRTECKEN.                                                 
021700     03  STYRTECKEN-RAD          PIC X.                                   
021800     03  SEND-RAD                PIC X(200)  VALUE SPACE.                 
021900*    --- CONTROL CHARACTERS                                               
022000 01  WS-SKIP1                    PIC X       VALUE ' '.                   
022100 01  WS-SKIP2                    PIC X       VALUE '0'.                   
022200 01  WS-SKIP3                    PIC X       VALUE '-'.                   
022300 01  WS-PAGESKIP                 PIC X       VALUE '1'.                   
022400     EJECT                                                                
022500                                                                          
022600 01  FILLER                      PIC X(16)   VALUE 'BOLIST'.              
022700                                                                          
022800     EJECT                                                                
022900*    --- LISTLAYOUT                                                       
023000 01  LISTA.                                                               
023100     03  RUBRIK-1.                                                        
023200         05  FILLER     PIC X       VALUE SPACE.                          
023300         05  FILLER     PIC X(15)   VALUE 'VCCS W6121F-001'.              
023400         05  FILLER     PIC X(3)    VALUE SPACE.                          
023500         05  FILLER     PIC X(47)   VALUE                                 
023600            'FOLLOW UP OF THE REFILL DELIVERIES TO THE DC'.               
023700         05  FILLER     PIC X       VALUE SPACE.                          
023800         05  FILLER     PIC X(8)    VALUE 'FOR WEEK'.                     
023900         05  FILLER     PIC X       VALUE SPACE.                          
024000         05  RUB1-AAVV  PIC X(4)    VALUE ZERO.                           
024100                                                                          
024200     03  RUBRIK-2.                                                        
024300         05  FILLER           PIC X(29)   VALUE SPACE.                    
024400         05  FILLER           PIC X(5)    VALUE 'TOT.'.                   
024500         05  FILLER           PIC X(2)    VALUE SPACE.                    
024600         05  FILLER           PIC X(4)    VALUE 'TOT.'.                   
024700         05  FILLER           PIC X(2)    VALUE SPACE.                    
024800         05  FILLER           PIC X(5)    VALUE 'NO.OF'.                  
024900         05  FILLER           PIC X(1)    VALUE SPACE.                    
025000         05  FILLER           PIC X(7)    VALUE 'CORRECT'.                
025100         05  FILLER           PIC X(1)    VALUE SPACE.                    
025200         05  FILLER           PIC X(5)    VALUE 'NO.OF'.                  
025300         05  FILLER           PIC X(4)    VALUE SPACE.                    
025400         05  FILLER           PIC X(7)    VALUE 'CORRECT'.                
025500         05  FILLER           PIC X(28)   VALUE SPACE.                    
025600         05  FILLER           PIC X(5)    VALUE 'EXTRA'.                  
025700         05  FILLER           PIC X(2)    VALUE SPACE.                    
025800         05  FILLER           PIC X(4)    VALUE 'LOST'.                   
025900         05  FILLER           PIC X(8)    VALUE SPACE.                    
026000         05  FILLER           PIC X(5)    VALUE 'FOUND'.                  
026100     03  RUBRIK-3.                                                        
026200         05  FILLER           PIC X(4)    VALUE SPACE.                    
026300         05  RUB3-DC-FROM     PIC X(4)    VALUE 'FROM'.                   
026400         05  FILLER           PIC X(2)    VALUE SPACE.                    
026500         05  FILLER           PIC X(2)    VALUE 'TO'.                     
026600         05  FILLER           PIC X(2)    VALUE SPACE.                    
026700         05  RUB3-DC-REC      PIC X(2)    VALUE 'DC'.                     
026800         05  RUB3-DC          PIC X(13)   VALUE SPACE.                    
026900************************                                                  
027000         05  FILLER           PIC X(4)    VALUE 'ADV.'.                   
027100         05  FILLER           PIC X(3)    VALUE SPACE.                    
027200         05  FILLER           PIC X(4)    VALUE 'BIN.'.                   
027300         05  FILLER           PIC X(4)    VALUE SPACE.                    
027400         05  FILLER           PIC X(4)    VALUE 'ERR.'.                   
027500         05  FILLER           PIC X(6)    VALUE SPACE.                    
027600         05  FILLER           PIC X       VALUE '%'.                      
027700         05  FILLER           PIC X(1)    VALUE SPACE.                    
027800         05  FILLER           PIC X(8)    VALUE 'ERR.LINE'.               
027900         05  FILLER           PIC X(1)    VALUE SPACE.                    
028000         05  FILLER           PIC X(7)    VALUE '% LINE'.                 
028100         05  FILLER           PIC X(2)    VALUE SPACE.                    
028200         05  FILLER           PIC X(10)   VALUE 'SHORT/ZERO'.             
028300         05  FILLER           PIC X       VALUE SPACE.                    
028400         05  FILLER           PIC X(5)    VALUE 'OVER.'.                  
028500         05  FILLER           PIC X       VALUE SPACE.                    
028600         05  FILLER           PIC X(8)    VALUE 'DAM/ZERO'.               
028700         05  FILLER           PIC X       VALUE SPACE.                    
028800         05  FILLER           PIC X(4)    VALUE 'PART'.                   
028900         05  FILLER           PIC X(3)    VALUE SPACE.                    
029000         05  FILLER           PIC X(10)   VALUE 'CASE/LINE'.              
029100         05  FILLER           PIC X(2)    VALUE SPACE.                    
029200         05  FILLER           PIC X(10)   VALUE 'CASE/LINE'.              
029300     03  RAD.                                                             
029400         05  FILLER           PIC X(4)    VALUE SPACE.                    
029500         05  RAD-IDDC-SEND    PIC X(2)    VALUE SPACE.                    
029600         05  FILLER           PIC X(4)    VALUE SPACE.                    
029700         05  RAD-IDDC-REC     PIC X(2)    VALUE SPACE.                    
029800         05  FILLER           PIC X(2)    VALUE SPACE.                    
029900         05  RAD-IDDC         PIC X(12)   VALUE SPACE.                    
030000****     05  FILLER           PIC X(3)    VALUE SPACE.                    
030100*************                                                             
030200         05  RAD-TOT-REC      PIC ZZZZZZ9.                                
030300         05  FILLER           PIC X(1)    VALUE SPACE.                    
030400         05  RAD-TOT-BINNED   PIC ZZZZZZ9.                                
030500         05  FILLER           PIC X(1)    VALUE SPACE.                    
030600         05  RAD-ERROR        PIC ZZZ9.                                   
030700         05  FILLER           PIC X(3)    VALUE SPACE.                    
030800         05  RAD-CORRECT      PIC ZZ9V,9   VALUE ZERO.                    
030900         05  FILLER           PIC X(2)    VALUE SPACE.                    
031000         05  RAD-ERROR-LINE   PIC ZZZ9.                                   
031100         05  FILLER           PIC X(6)    VALUE SPACE.                    
031200         05  RAD-CORRECT-LINE  PIC ZZ9V,9   VALUE ZERO.                   
031300         05  FILLER           PIC X(4)    VALUE SPACE.                    
031400         05  RAD-SHORTAGE     PIC ZZZ9.                                   
031500         05  FILLER           PIC X       VALUE '/'.                      
031600         05  RAD-SHORTAGE-00  PIC ZZZ9.                                   
031700         05  FILLER           PIC X       VALUE SPACE.                    
031800         05  RAD-OVERAGE      PIC ZZZ9.                                   
031900         05  FILLER           PIC X       VALUE SPACE.                    
032000         05  RAD-DAM          PIC ZZZ9.                                   
032100         05  FILLER           PIC X       VALUE '/'.                      
032200         05  RAD-DAM-00       PIC ZZZ9.                                   
032300         05  FILLER           PIC X(2)    VALUE SPACE.                    
032400         05  RAD-EXTRA        PIC ZZZ9.                                   
032500         05  FILLER           PIC X(3)    VALUE SPACE.                    
032600         05  RAD-LOST-KOLLI   PIC Z9.                                     
032700         05  FILLER           PIC X       VALUE '/'.                      
032800         05  RAD-LOST-LINE    PIC ZZ9.                                    
032900         05  FILLER           PIC X(6)    VALUE SPACE.                    
033000         05  RAD-FOUND-KOLLI  PIC Z9.                                     
033100         05  FILLER           PIC X       VALUE '/'.                      
033200         05  RAD-FOUND-LINE   PIC ZZ9.                                    
033300     03  TOT-RAD.                                                         
033400         05  FILLER               PIC X(26)   VALUE SPACE.                
033500         05  RAD-TOT-REC-DC       PIC ZZZZZZ9.                            
033600         05  FILLER               PIC X(1)    VALUE SPACE.                
033700         05  RAD-TOT-BINNED-DC    PIC ZZZZZZ9.                            
033800         05  FILLER               PIC X(1)    VALUE SPACE.                
033900         05  RAD-ERROR-DC         PIC ZZZ9.                               
034000         05  FILLER               PIC X(3)    VALUE SPACE.                
034100         05  RAD-CORRECT-DC       PIC ZZ9V,9   VALUE ZERO.                
034200         05  FILLER               PIC X(2)    VALUE SPACE.                
034300         05  RAD-ERROR-LINE-DC    PIC ZZZ9.                               
034400         05  FILLER               PIC X(6)    VALUE SPACE.                
034500         05  RAD-CORRECT-LINE-DC  PIC ZZ9V,9   VALUE ZERO.                
034600         05  FILLER               PIC X(4)    VALUE SPACE.                
034700         05  RAD-SHORTAGE-DC      PIC ZZZ9.                               
034800         05  FILLER               PIC X       VALUE '/'.                  
034900         05  RAD-SHORTAGE-DC-00   PIC ZZZ9.                               
035000         05  FILLER               PIC X       VALUE SPACE.                
035100         05  RAD-OVERAGE-DC       PIC ZZZ9.                               
035200         05  FILLER               PIC X       VALUE SPACE.                
035300         05  RAD-DAM-DC           PIC ZZZ9.                               
035400         05  FILLER               PIC X       VALUE '/'.                  
035500         05  RAD-DAM-DC-00        PIC ZZZ9.                               
035600         05  FILLER               PIC X(2)    VALUE SPACE.                
035700         05  RAD-EXTRA-DC         PIC ZZZ9.                               
035800         05  FILLER               PIC X(3)    VALUE SPACE.                
035900         05  RAD-LOST-KOLLI-DC    PIC Z9.                                 
036000         05  FILLER               PIC X       VALUE '/'.                  
036100         05  RAD-LOST-LINE-DC     PIC ZZ9.                                
036200         05  FILLER               PIC X(6)    VALUE SPACE.                
036300         05  RAD-FOUND-KOLLI-DC   PIC Z9.                                 
036400         05  FILLER               PIC X       VALUE '/'.                  
036500         05  RAD-FOUND-LINE-DC    PIC ZZ9.                                
036600                                                                          
036700                                                                          
036800 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
036900 01  FILLER REDEFINES DAGENS-DATUM.                                       
037000     03  DAGENS-AA               PIC 9(2).                                
037100     03  DAGENS-MM               PIC 9(2).                                
037200     03  DAGENS-DD               PIC 9(2).                                
037300     EJECT                                                                
037400 01  GENERAL-SUBPROGRAMS.                                                 
037500*                                                                         
037600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
037700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
037800     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
037900     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
038000     03  WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.            
038010     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
038020     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
038100     SKIP2                                                                
038200*    --- PARAMETERS TO ABEND                                              
038300                                                                          
038400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
038500 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
038600 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
038700     SKIP2                                                                
038800 01  ERRTEXT.                                                             
038900     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
039000     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
039100     EJECT                                                                
039200*                                                                         
039201*    --- PARAMETRAR TILL POSTSUM                                          
039202*                                                                         
039203*01  -COPY W0005   -PRE  POSTSUM-                                         
039204     EJECT                                                                
039205 01  IN-AREA-START               PIC X(24)   VALUE                        
039206                                 'IN-AREA-START  '.                       
039207     SKIP2                                                                
039208*01  AREA -COPY W61247      -PRE IN-                                      
039209     EJECT                                                                
039210                                                                          
039211 01  IN-W6121E2-AREA-START       PIC X(24)   VALUE                        
039212                                 'IN-W6121E-AREA-START  '.                
039213     SKIP2                                                                
039214*01  AREA -COPY W6121E      -PRE IN-W6121E-                               
039215     EJECT                                                                
039216                                                                          
039217 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
039220     SKIP3                                                                
039230 01  KEYS-FOR-DLI.                                                        
039240     03  W-IDDC-X.                                                        
039250         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
039260                                                                          
039294*    --- STATUS-KOD FRÅN IMS                                              
039295 01  STATUS-WS                   PIC XX.                                  
039296     88  SEGMENT-FOUND                       VALUE '  '.                  
039298     88  SEGMENT-MISSING                     VALUE 'GE'.                  
039299     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
039300     SKIP2                                                                
039301 01  GOOD-STATUSCODES.                                                    
039302     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
039303     SKIP3                                                                
039304 01  SSA1                        PIC X(64).                               
039305 01  SSA2                        PIC X(64).                               
039306*    --- IMS FUNCTION CODES                                               
039307*01  -COPY W0003                                                          
039308*    ---  DLI INPUT-OUTPUT AREA                                           
039309 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
039310 01  DLI-IO-WDB601.                                                       
039311*    03  -COPY WDB601                                                     
039312*                                                                         
040900 LINKAGE SECTION.                                                         
041000                                                                          
041100*01  -COPY W0009            -PRE MSG-                                     
041200                                                                          
041300*01  -COPY W0009            -PRE DISTRDOC-                                
041400                                                                          
041410*01  -COPY W0008  -PRE WDB6-                                              
041420     05  FILLER                  PIC X.                                   
041430                                                                          
041500 PROCEDURE DIVISION  USING MSG-PCB DISTRDOC-PCB WDB6-PCB.                 
041600 MAIN SECTION.                                                            
041700     ENTRY 'DLITCBL' USING MSG-PCB DISTRDOC-PCB WDB6-PCB.                 
041800                                                                          
041900     PERFORM A-INIT                                                       
042000                                                                          
042100     PERFORM S04-LAS-W6121E                                               
042200     MOVE +1 TO IX                                                        
042300     MOVE ZERO TO WS-ANTAL                                                
042400     PERFORM UNTIL IN-W6121E-IDDC-SEND > SPACE OR END-OF-W6121E           
042500        PERFORM S04-LAS-W6121E                                            
042600     END-PERFORM                                                          
042700                                                                          
042800     MOVE IN-W6121E-IDDC-REC TO SPAR-IDDC-REC                             
042900     MOVE IN-W6121E-IDDC-SEND TO SPAR-IDDC-SEND                           
043000     PERFORM UNTIL END-OF-W6121E                                          
043100        IF IN-W6121E-IDDC-REC = SPAR-IDDC-REC AND                         
043200           IN-W6121E-IDDC-SEND = SPAR-IDDC-SEND                           
043300           ADD +1 TO WS-ANTAL                                             
043400        ELSE                                                              
043500           MOVE SPAR-IDDC-REC TO TAB-IDDC-REC(IX)                         
043600           MOVE SPAR-IDDC-SEND TO TAB-IDDC-SEND(IX)                       
043700           MOVE WS-ANTAL TO TAB-ANTAL(IX)                                 
043800           MOVE IN-W6121E-IDDC-REC TO SPAR-IDDC-REC                       
043900           MOVE IN-W6121E-IDDC-SEND TO SPAR-IDDC-SEND                     
044000           MOVE +1 TO WS-ANTAL                                            
044100           ADD +1 TO IX                                                   
044200        END-IF                                                            
044300        PERFORM S04-LAS-W6121E                                            
044400     END-PERFORM                                                          
044500     MOVE IX TO IX7-MAX                                                   
044600     MOVE SPAR-IDDC-REC TO TAB-IDDC-REC(IX)                               
044700     MOVE SPAR-IDDC-SEND TO TAB-IDDC-SEND(IX)                             
044800     MOVE WS-ANTAL TO TAB-ANTAL(IX)                                       
044900                                                                          
045000     PERFORM S90-SEND-OPEN                                                
045100     PERFORM S90-PUT-DAP-START                                            
045200     PERFORM C-PRINT-HEAD                                                 
045300                                                                          
045400     PERFORM S01-LAS-W6121F                                               
045500     PERFORM UNTIL (IN-IDDC-SEND NOT = SPACE)                             
045600       PERFORM S01-LAS-W6121F                                             
045700     END-PERFORM                                                          
045800                                                                          
045900     MOVE +1 TO IX7                                                       
046000     PERFORM UNTIL (END-OF-W6121F) AND (IX7 > IX7-MAX)                    
046100        IF IN-IDDC-SEND = TAB-IDDC-SEND(IX7)                              
046200           IF IN-IDDC-REC = TAB-IDDC-REC(IX7)                             
046300              MOVE IN-IDDC-REC TO SPAR-IDDC-REC                           
046400                                  WS-IDDC-REC                             
046500              MOVE IN-IDDC-SEND TO SPAR-IDDC-SEND                         
046600                                  WS-IDDC-SEND                            
046700              PERFORM UNTIL END-OF-W6121F OR                              
046800                 (SPAR-IDDC-REC NOT = IN-IDDC-REC OR                      
046900                   SPAR-IDDC-SEND NOT = IN-IDDC-SEND)                     
047000                      PERFORM D-RAD-DATA                                  
047100                      PERFORM S01-LAS-W6121F                              
047200              END-PERFORM                                                 
047300              PERFORM E-SKAPA-UTRAD                                       
047400              PERFORM F-SPARA-LAGERVARDE                                  
047500              PERFORM S02-NOLLSTALL                                       
047600              ADD +1 TO IX7                                               
047700              IF TAB-IDDC-SEND(IX7) = SPAR-IDDC-SEND                      
047800                 CONTINUE                                                 
047900              ELSE                                                        
048000                  PERFORM G-SKAPA-TOTRAD                                  
048100                  PERFORM S06-NOLLSTALL                                   
048200                  MOVE SPACE TO SEND-RAD                                  
048300                  PERFORM S90-PUT-DOC-LINE                                
048400                  MOVE NEJ TO SW-TOT                                      
048500              END-IF                                                      
048600           ELSE                                                           
048700              IF NOT END-OF-W6121F                                        
048800**************INGA AVVIKELSER                                             
048900                 IF IN-IDDC-REC > TAB-IDDC-REC(IX7)                       
049000                    MOVE TAB-IDDC-SEND(IX7) TO SPAR-IDDC-SEND             
049100                    MOVE TAB-IDDC-REC(IX7) TO SPAR-IDDC-REC               
049200                    MOVE TAB-ANTAL(IX7) TO WS-SUMMA-BIN                   
049300                    PERFORM E-SKAPA-UTRAD                                 
049400                    PERFORM F-SPARA-LAGERVARDE                            
049500                    PERFORM S02-NOLLSTALL                                 
049600                    ADD +1 TO IX7                                         
049700                    IF TAB-IDDC-SEND (IX7) = SPAR-IDDC-SEND               
049800                       CONTINUE                                           
049900                    ELSE                                                  
050000                       PERFORM G-SKAPA-TOTRAD                             
050100                       PERFORM S06-NOLLSTALL                              
050200                       MOVE SPACE TO SEND-RAD                             
050300                       PERFORM S90-PUT-DOC-LINE                           
050400                       MOVE NEJ TO SW-TOT                                 
050500                    END-IF                                                
050600                 ELSE                                                     
050700*****************INGEN R32                                                
050800                    IF IN-IDDC-REC < TAB-IDDC-REC(IX7)                    
050900                      MOVE IN-IDDC-SEND TO SPAR-IDDC-SEND                 
051000                      MOVE IN-IDDC-REC TO SPAR-IDDC-REC                   
051100                      MOVE ZERO TO WS-SUMMA-BIN                           
051200                      PERFORM D-RAD-DATA                                  
051300                      PERFORM E-SKAPA-UTRAD                               
051400                      PERFORM F-SPARA-LAGERVARDE                          
051500                      PERFORM S02-NOLLSTALL                               
051600                      MOVE JA TO SW-TOT                                   
051700                      PERFORM S01-LAS-W6121F                              
051800                      IF IN-IDDC-SEND NOT = TAB-IDDC-SEND(IX7)            
051900                         PERFORM G-SKAPA-TOTRAD                           
052000                         PERFORM S06-NOLLSTALL                            
052100                         MOVE SPACE TO SEND-RAD                           
052200                         PERFORM S90-PUT-DOC-LINE                         
052300                         MOVE NEJ TO SW-TOT                               
052400                      END-IF                                              
052500                    END-IF                                                
052600                 END-IF                                                   
052700              ELSE                                                        
052800**************EOF INFILEN                                                 
052900                 IF (TAB-IDDC-REC(IX7) > IN-IDDC-REC)                     
053000                    MOVE TAB-IDDC-SEND(IX7) TO SPAR-IDDC-SEND             
053100                    MOVE TAB-IDDC-REC(IX7) TO SPAR-IDDC-REC               
053200                    MOVE TAB-ANTAL(IX7) TO WS-SUMMA-BIN                   
053300                    PERFORM E-SKAPA-UTRAD                                 
053400                    PERFORM F-SPARA-LAGERVARDE                            
053500                    PERFORM S02-NOLLSTALL                                 
053600                    MOVE JA TO SW-TOT                                     
053700                    ADD +1 TO IX7                                         
053800                    IF TAB-IDDC-SEND(IX7) = SPAR-IDDC-SEND                
053900                       CONTINUE                                           
054000                    ELSE                                                  
054100                       PERFORM G-SKAPA-TOTRAD                             
054200                       PERFORM S06-NOLLSTALL                              
054300                       MOVE SPACE TO SEND-RAD                             
054400                       PERFORM S90-PUT-DOC-LINE                           
054500                       MOVE NEJ TO SW-TOT                                 
054600                    END-IF                                                
054700                 END-IF                                                   
054800              END-IF                                                      
054900           END-IF                                                         
055000        ELSE                                                              
055100           IF NOT END-OF-W6121F                                           
055200              IF SW-TOT = JA                                              
055300                  PERFORM G-SKAPA-TOTRAD                                  
055400                  PERFORM S06-NOLLSTALL                                   
055500                  MOVE SPACE TO SEND-RAD                                  
055600                  PERFORM S90-PUT-DOC-LINE                                
055700                  MOVE NEJ TO SW-TOT                                      
055800              END-IF                                                      
055900              IF IN-IDDC-SEND > TAB-IDDC-SEND(IX7)                        
056000                  PERFORM H-EJ-AVVIKELSE                                  
056100                  MOVE TAB-IDDC-SEND(IX7) TO WS2-IDDC-SEND                
056200                  ADD +1 TO IX7                                           
056300                  IF TAB-IDDC-SEND(IX7) = WS2-IDDC-SEND                   
056400                     CONTINUE                                             
056500                  ELSE                                                    
056600                     IF IN-IDDC-SEND = WS2-IDDC-SEND                      
056700                        CONTINUE                                          
056800                     ELSE                                                 
056900                        PERFORM G-SKAPA-TOTRAD                            
057000                        PERFORM S06-NOLLSTALL                             
057100                        MOVE SPACE TO SEND-RAD                            
057200                        PERFORM S90-PUT-DOC-LINE                          
057300                        MOVE NEJ TO SW-TOT                                
057400                     END-IF                                               
057500                  END-IF                                                  
057600              ELSE                                                        
057700**************INGEN 32                                                    
057800                IF SW-TOT = JA                                            
057900                   PERFORM G-SKAPA-TOTRAD                                 
058000                   PERFORM S06-NOLLSTALL                                  
058100                   MOVE SPACE TO SEND-RAD                                 
058200                   PERFORM S90-PUT-DOC-LINE                               
058300                   MOVE NEJ TO SW-TOT                                     
058400                END-IF                                                    
058500                MOVE IN-IDDC-SEND TO SPAR-IDDC-SEND                       
058600                MOVE IN-IDDC-REC TO SPAR-IDDC-REC                         
058700                MOVE ZERO TO WS-SUMMA-BIN                                 
058800                PERFORM D-RAD-DATA                                        
058900                PERFORM E-SKAPA-UTRAD                                     
059000                PERFORM F-SPARA-LAGERVARDE                                
059100                PERFORM S02-NOLLSTALL                                     
059200                MOVE JA TO SW-TOT                                         
059300                PERFORM S01-LAS-W6121F                                    
059400                IF IN-IDDC-SEND = SPAR-IDDC-SEND                          
059500                   CONTINUE                                               
059600                ELSE                                                      
059700                   PERFORM G-SKAPA-TOTRAD                                 
059800                   PERFORM S06-NOLLSTALL                                  
059900                   MOVE SPACE TO SEND-RAD                                 
060000                   PERFORM S90-PUT-DOC-LINE                               
060100                   MOVE NEJ TO SW-TOT                                     
060200                END-IF                                                    
060300              END-IF                                                      
060400           ELSE                                                           
060500********   EOF AV INFIL                                                   
060600              IF (TAB-IDDC-SEND(IX7) > IN-IDDC-SEND)                      
060700                 IF RAD-IDDC-SEND = TAB-IDDC-SEND(IX7)                    
060800                    MOVE TAB-IDDC-SEND(IX7) TO SPAR-IDDC-SEND             
060900                    MOVE TAB-IDDC-REC(IX7) TO SPAR-IDDC-REC               
061000                    MOVE TAB-ANTAL(IX7) TO WS-SUMMA-BIN                   
061100                    PERFORM E-SKAPA-UTRAD                                 
061200                    PERFORM F-SPARA-LAGERVARDE                            
061300                    PERFORM S02-NOLLSTALL                                 
061400                    MOVE JA TO SW-TOT                                     
061500                    ADD +1 TO IX7                                         
061600                    IF TAB-IDDC-SEND(IX7) = SPAR-IDDC-SEND                
061700                       CONTINUE                                           
061800                    ELSE                                                  
061900                       PERFORM G-SKAPA-TOTRAD                             
062000                       PERFORM S06-NOLLSTALL                              
062100                       MOVE SPACE TO SEND-RAD                             
062200                       PERFORM S90-PUT-DOC-LINE                           
062300                       MOVE NEJ TO SW-TOT                                 
062400                    END-IF                                                
062500                 ELSE                                                     
062600                    PERFORM H-EJ-AVVIKELSE                                
062700                    ADD +1 TO IX7                                         
062800                    IF TAB-IDDC-SEND(IX7) = SPAR-IDDC-SEND                
062900                       CONTINUE                                           
063000                    ELSE                                                  
063100                       PERFORM G-SKAPA-TOTRAD                             
063200                       PERFORM S06-NOLLSTALL                              
063300                       MOVE SPACE TO SEND-RAD                             
063400                       PERFORM S90-PUT-DOC-LINE                           
063500                       MOVE NEJ TO SW-TOT                                 
063600                    END-IF                                                
063700                 END-IF                                                   
063800              ELSE                                                        
063900                 IF SW-TOT = JA                                           
064000                    PERFORM G-SKAPA-TOTRAD                                
064100                    PERFORM S06-NOLLSTALL                                 
064200                    MOVE SPACE TO SEND-RAD                                
064300                    PERFORM S90-PUT-DOC-LINE                              
064400                    MOVE NEJ TO SW-TOT                                    
064500                 END-IF                                                   
064600                 ADD +1 TO IX7                                            
064700              END-IF                                                      
064800           END-IF                                                         
064900        END-IF                                                            
065000     END-PERFORM                                                          
065100                                                                          
065200     IF SW-TOT = JA                                                       
065300        PERFORM G-SKAPA-TOTRAD                                            
065400        PERFORM S06-NOLLSTALL                                             
065500        MOVE SPACE TO SEND-RAD                                            
065600        PERFORM S90-PUT-DOC-LINE                                          
065700     END-IF                                                               
065800                                                                          
065900     PERFORM S90-SEND-CLOSE                                               
066000                                                                          
066100     PERFORM Z-FINIT                                                      
066200                                                                          
066300     MOVE ZERO TO RETURN-CODE                                             
066400     GOBACK                                                               
066500     .                                                                    
066600     EJECT                                                                
066700 A-INIT SECTION.                                                          
066800                                                                          
066900     OPEN INPUT W6121F                                                    
067000                W6121E                                                    
067100     ACCEPT DAGENS-DATUM FROM DATE                                        
067200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
067300                                                                          
067400     MOVE 'IDAG  ' TO DAT-KDDATFORM                                       
067500     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
067600                     DAT-O-TIDATUM DAT-KDSVAR                             
067700     IF DAT-KDSVAR-OK                                                     
067800       MOVE DAT-TIAAVV-GRP TO RUB1-AAVV                                   
067900     END-IF                                                               
068000                                                                          
068100     MOVE NEJ TO SW-TOT                                                   
068200     PERFORM S02-NOLLSTALL                                                
068300     PERFORM S06-NOLLSTALL                                                
068400     PERFORM S05-NOLLSTALL-TAB                                            
068500     .                                                                    
068600     EJECT                                                                
068700 C-PRINT-HEAD SECTION.                                                    
068800                                                                          
068900     MOVE SPACE                      TO SEND-RAD                          
069000     PERFORM S90-PUT-DOC-LINE                                             
069100     MOVE RUBRIK-1                   TO SEND-RAD                          
069200     PERFORM S90-PUT-DOC-LINE                                             
069300                                                                          
069400     MOVE SPACE                      TO SEND-RAD                          
069500     PERFORM S90-PUT-DOC-LINE                                             
069600                                                                          
069700     MOVE SPACE                      TO SEND-RAD                          
069800     MOVE RUBRIK-2                   TO SEND-RAD                          
069900     PERFORM S90-PUT-DOC-LINE                                             
070000                                                                          
070100     MOVE SPACE                      TO SEND-RAD                          
070200     MOVE RUBRIK-3                   TO SEND-RAD                          
070300     PERFORM S90-PUT-DOC-LINE                                             
070400                                                                          
070500     MOVE SPACE                      TO SEND-RAD                          
070600     PERFORM S90-PUT-DOC-LINE                                             
070700     .                                                                    
070800     EJECT                                                                
070900 D-RAD-DATA SECTION.                                                      
071000                                                                          
071100     IF IN-AVVIKELSETYP = 'ÖVERLEV.'                                      
071200        ADD +1 TO WS-ANTAL-OVERLEV                                        
071300        PERFORM S03-SPARA-SORT                                            
071400     ELSE                                                                 
071500       IF IN-AVVIKELSETYP = 'UNDERLEV.'                                   
071600          ADD +1 TO WS-ANTAL-UNDERLEV                                     
071700          IF IN-KVAVIS = IN-KVANTAL                                       
071800             ADD +1 TO WS-ANTAL-UNDERLEV-00                               
071900          ELSE                                                            
072000             ADD +1 TO WS-ANTAL-UNDERLEV-XX                               
072100          END-IF                                                          
072200          PERFORM S03-SPARA-SORT                                          
072300       ELSE                                                               
072400          IF IN-AVVIKELSETYP = 'DAM'                                      
072500             ADD +1 TO WS-ANTAL-DAM                                       
072600             IF IN-KVAVIS = IN-KVANTAL                                    
072700                ADD +1 TO WS-ANTAL-DAM-00                                 
072800             ELSE                                                         
072900                ADD +1 TO WS-ANTAL-DAM-XX                                 
073000             END-IF                                                       
073100             PERFORM S03-SPARA-SORT                                       
073200          ELSE                                                            
073300            IF IN-AVVIKELSETYP = 'NY'                                     
073400               ADD +1 TO WS-ANTAL-NY                                      
073500               PERFORM S03-SPARA-SORT                                     
073600            END-IF                                                        
073700          END-IF                                                          
073800       END-IF                                                             
073900     END-IF                                                               
074000                                                                          
074100     IF IN-AVVIKELSETYP = 'LOST'                                          
074200        IF SPAR-IDFAKT    = IN-IDFAKT                                     
074300        AND SPAR-IDKUNDRF = IN-IDKUNDRF                                   
074400        AND SPAR-IDKUNDNR = IN-IDKUNDNR                                   
074500        AND SPAR-IDKOLLI  = IN-IDKOLLI                                    
074600        AND SPAR-KDSORT1  = IN-KDSORT1                                    
074700           ADD +1 TO WS-ANTAL-LOST                                        
074800           PERFORM S03-SPARA-SORT                                         
074900        ELSE                                                              
075000           ADD +1 TO WS-ANTAL-LOST                                        
075100           ADD +1 TO WS-ANTAL-LOST-KOLLI                                  
075200***        MOVE ZERO TO WS-ANTAL-LOST                                     
075300           PERFORM S03-SPARA-SORT                                         
075400        END-IF                                                            
075500     ELSE                                                                 
075600        IF IN-AVVIKELSETYP = 'FOUND'                                      
075700         IF SPAR-IDFAKT    = IN-IDFAKT                                    
075800            AND SPAR-IDKUNDRF = IN-IDKUNDRF                               
075900            AND SPAR-IDKUNDNR = IN-IDKUNDNR                               
076000            AND SPAR-IDKOLLI  = IN-IDKOLLI                                
076100            AND SPAR-KDSORT1  = IN-KDSORT1                                
076200              ADD +1 TO WS-ANTAL-FOUND                                    
076300              PERFORM S03-SPARA-SORT                                      
076400          ELSE                                                            
076500              ADD +1 TO WS-ANTAL-FOUND                                    
076600              ADD +1 TO WS-ANTAL-FOUND-KOLLI                              
076700***           MOVE ZERO TO WS-ANTAL-FOUND                                 
076800              PERFORM S03-SPARA-SORT                                      
076900          END-IF                                                          
077000        END-IF                                                            
077100     END-IF                                                               
077200     .                                                                    
077300     EJECT                                                                
077400 E-SKAPA-UTRAD SECTION.                                                   
077500                                                                          
077600     MOVE SPAR-IDDC-REC         TO RAD-IDDC-REC                           
077610                                   W-IDDC                                 
077700     MOVE SPAR-IDDC-SEND        TO RAD-IDDC-SEND                          
077800                                                                          
078710     PERFORM IMS-GU-WDB601                                                
078720     IF SEGMENT-FOUND                                                     
078730       MOVE DCS-ADCITY IN DCS-ADPOST-PNRORT                               
078740                                TO RAD-IDDC                               
078750     END-IF                                                               
078760                                                                          
078800     MOVE WS-ANTAL-OVERLEV      TO RAD-OVERAGE                            
078900**   MOVE WS-ANTAL-UNDERLEV     TO RAD-SHORTAGE                           
079000     MOVE WS-ANTAL-UNDERLEV-XX  TO RAD-SHORTAGE                           
079100     MOVE WS-ANTAL-UNDERLEV-00  TO RAD-SHORTAGE-00                        
079200**   MOVE WS-ANTAL-DAM          TO RAD-DAM                                
079300     MOVE WS-ANTAL-DAM-XX       TO RAD-DAM                                
079400     MOVE WS-ANTAL-DAM-00       TO RAD-DAM-00                             
079500     MOVE WS-ANTAL-LOST         TO RAD-LOST-LINE                          
079600     MOVE WS-ANTAL-LOST-KOLLI   TO RAD-LOST-KOLLI                         
079700     MOVE WS-ANTAL-FOUND        TO RAD-FOUND-LINE                         
079800     MOVE WS-ANTAL-FOUND-KOLLI  TO RAD-FOUND-KOLLI                        
079900     MOVE WS-ANTAL-NY           TO RAD-EXTRA                              
080000                                                                          
080100     COMPUTE WS-SUMMA-ERR-KOLLI = WS-ANTAL-OVERLEV                        
080200        + WS-ANTAL-UNDERLEV                                               
080300        + WS-ANTAL-DAM + WS-ANTAL-LOST-KOLLI                              
080400        + WS-ANTAL-FOUND-KOLLI + WS-ANTAL-NY                              
080500                                                                          
080600     MOVE WS-SUMMA-ERR-KOLLI TO RAD-ERROR                                 
080700                                                                          
080800     COMPUTE WS-SUMMA-ERR = WS-ANTAL-OVERLEV + WS-ANTAL-UNDERLEV          
080900        + WS-ANTAL-DAM + WS-ANTAL-LOST +                                  
081000        WS-ANTAL-FOUND + WS-ANTAL-NY                                      
081100                                                                          
081200     MOVE WS-SUMMA-ERR TO RAD-ERROR-LINE                                  
081300                                                                          
081400     COMPUTE WS-SUMMA-TOT = WS-ANTAL-OVERLEV + WS-ANTAL-UNDERLEV          
081500        + WS-ANTAL-DAM + WS-ANTAL-LOST +                                  
081600        WS-ANTAL-FOUND + WS-ANTAL-NY                                      
081700                                                                          
081800     MOVE +1 TO IX                                                        
081900     MOVE ZERO TO WS-SUMMA-BIN                                            
082000     PERFORM UNTIL IX > IX-MAX                                            
082100       IF (TAB-IDDC-REC(IX) = SPAR-IDDC-REC                               
082200       AND TAB-IDDC-SEND(IX)= SPAR-IDDC-SEND)                             
082300          MOVE TAB-ANTAL(IX) TO WS-SUMMA-BIN                              
082400          ADD IX-MAX TO IX                                                
082500       ELSE                                                               
082600          ADD +1 TO IX                                                    
082700       END-IF                                                             
082800     END-PERFORM                                                          
082900                                                                          
083000     COMPUTE WS-SUMMA-TOT-BIN = WS-SUMMA-BIN                              
083100      - WS-ANTAL-UNDERLEV-00  - WS-ANTAL-DAM-00  - WS-ANTAL-LOST          
083200************                                                              
083300     IF WS-SUMMA-TOT-BIN < ZERO                                           
083400        MOVE ZERO TO WS-SUMMA-TOT-BIN                                     
083500     END-IF                                                               
083600     MOVE WS-SUMMA-TOT-BIN TO RAD-TOT-BINNED                              
083700************                                                              
083800                                                                          
083900     COMPUTE WS-SUMMA-ANTMOT = WS-SUMMA-BIN - WS-ANTAL-NY                 
084000                  - WS-ANTAL-FOUND                                        
084100     MOVE WS-SUMMA-ANTMOT TO RAD-TOT-REC                                  
084200                                                                          
084300     IF WS-SUMMA-ANTMOT = ZERO                                            
084400        MOVE ZERO TO RAD-CORRECT-LINE                                     
084500     ELSE                                                                 
084600       COMPUTE WS-FEL-PROC = WS-SUMMA-ERR * 100 / WS-SUMMA-ANTMOT         
084700       COMPUTE WS-CORR-PROC = 100 - WS-FEL-PROC                           
084800       MOVE WS-CORR-PROC TO RAD-CORRECT-LINE                              
084900       IF WS-CORR-PROC < 0                                                
085000          MOVE ZERO TO RAD-CORRECT-LINE                                   
085100       END-IF                                                             
085200     END-IF                                                               
085300                                                                          
085400     COMPUTE WS-SUMMA-ANTMOT-KOLLI = WS-SUMMA-BIN - WS-ANTAL-NY           
085500                  - WS-ANTAL-FOUND                                        
085600                                                                          
085700     IF WS-SUMMA-ANTMOT-KOLLI = ZERO                                      
085800        MOVE ZERO TO RAD-CORRECT                                          
085900     ELSE                                                                 
086000       COMPUTE WS-FEL-PROC = WS-SUMMA-ERR-KOLLI * 100 /                   
086100              WS-SUMMA-ANTMOT-KOLLI                                       
086200       COMPUTE WS-CORR-PROC = 100 - WS-FEL-PROC                           
086300       MOVE WS-CORR-PROC TO RAD-CORRECT                                   
086400       IF WS-CORR-PROC < 0                                                
086500          MOVE ZERO TO RAD-CORRECT                                        
086600       END-IF                                                             
086700     END-IF                                                               
086800                                                                          
086900     MOVE SPACE TO SEND-RAD                                               
087000     MOVE RAD TO SEND-RAD                                                 
087100     PERFORM S90-PUT-DOC-LINE                                             
087200     .                                                                    
087300     EJECT                                                                
087400 F-SPARA-LAGERVARDE SECTION.                                              
087500                                                                          
087600     ADD WS-ANTAL-OVERLEV       TO WS-ANTAL-OVERLEV-DC                    
087700     ADD WS-ANTAL-UNDERLEV      TO WS-ANTAL-UNDERLEV-DC                   
087800     ADD WS-ANTAL-UNDERLEV-00   TO WS-ANTAL-UNDERLEV-DC-00                
087900     ADD WS-ANTAL-UNDERLEV-XX   TO WS-ANTAL-UNDERLEV-DC-XX                
088000     ADD WS-ANTAL-DAM           TO WS-ANTAL-DAM-DC                        
088100     ADD WS-ANTAL-DAM-00        TO WS-ANTAL-DAM-DC-00                     
088200     ADD WS-ANTAL-DAM-XX        TO WS-ANTAL-DAM-DC-XX                     
088300     ADD WS-ANTAL-LOST-KOLLI    TO WS-ANTAL-LOST-KOLLI-DC                 
088400     ADD WS-ANTAL-FOUND-KOLLI   TO WS-ANTAL-FOUND-KOLLI-DC                
088500     ADD WS-ANTAL-NY            TO WS-ANTAL-NY-DC                         
088600     ADD WS-ANTAL-LOST          TO WS-ANTAL-LOST-DC                       
088700     ADD WS-ANTAL-FOUND         TO WS-ANTAL-FOUND-DC                      
088800     ADD WS-SUMMA-BIN           TO WS-SUMMA-BIN-DC                        
088900     ADD WS-SUMMA-ANTMOT        TO WS-SUMMA-ANTMOT-DC                     
089000     ADD WS-SUMMA-ERR           TO WS-SUMMA-ERR-DC                        
089100                                                                          
089200     .                                                                    
089300     EJECT                                                                
089400 G-SKAPA-TOTRAD SECTION.                                                  
089500                                                                          
089600     MOVE WS-ANTAL-OVERLEV-DC      TO RAD-OVERAGE-DC                      
089700**   MOVE WS-ANTAL-UNDERLEV-DC     TO RAD-SHORTAGE-DC                     
089800     MOVE WS-ANTAL-UNDERLEV-DC-XX  TO RAD-SHORTAGE-DC                     
089900     MOVE WS-ANTAL-UNDERLEV-DC-00  TO RAD-SHORTAGE-DC-00                  
090000**   MOVE WS-ANTAL-DAM-DC          TO RAD-DAM-DC                          
090100     MOVE WS-ANTAL-DAM-DC-XX       TO RAD-DAM-DC                          
090200     MOVE WS-ANTAL-DAM-DC-00       TO RAD-DAM-DC-00                       
090300     MOVE WS-ANTAL-LOST-KOLLI-DC   TO RAD-LOST-KOLLI-DC                   
090400     MOVE WS-ANTAL-FOUND-KOLLI-DC  TO RAD-FOUND-KOLLI-DC                  
090500     MOVE WS-ANTAL-NY-DC           TO RAD-EXTRA-DC                        
090600     MOVE WS-ANTAL-LOST-DC         TO RAD-LOST-LINE-DC                    
090700     MOVE WS-ANTAL-FOUND-DC        TO RAD-FOUND-LINE-DC                   
090800     MOVE WS-SUMMA-ANTMOT-DC       TO RAD-TOT-REC-DC                      
090900                                                                          
091000     COMPUTE WS-SUMMA-ERR-KOLLI-DC = WS-ANTAL-OVERLEV-DC                  
091100        + WS-ANTAL-UNDERLEV-DC                                            
091200        + WS-ANTAL-DAM-DC + WS-ANTAL-LOST-KOLLI-DC                        
091300        + WS-ANTAL-FOUND-KOLLI-DC + WS-ANTAL-NY-DC                        
091400                                                                          
091500     MOVE WS-SUMMA-ERR-KOLLI-DC TO RAD-ERROR-DC                           
091600                                                                          
091700     COMPUTE WS-SUMMA-ERR-DC = WS-ANTAL-OVERLEV-DC                        
091800        + WS-ANTAL-UNDERLEV-DC                                            
091900        + WS-ANTAL-DAM-DC + WS-ANTAL-LOST-DC  +                           
092000        WS-ANTAL-FOUND-DC + WS-ANTAL-NY-DC                                
092100                                                                          
092200     MOVE WS-SUMMA-ERR-DC TO RAD-ERROR-LINE-DC                            
092300                                                                          
092400     COMPUTE WS-SUMMA-TOT = WS-ANTAL-OVERLEV + WS-ANTAL-UNDERLEV          
092500        + WS-ANTAL-DAM + WS-ANTAL-LOST +                                  
092600        WS-ANTAL-FOUND + WS-ANTAL-NY                                      
092700                                                                          
092800     COMPUTE WS-SUMMA-TOT-BIN-DC = WS-SUMMA-BIN-DC                        
092900                     - WS-ANTAL-UNDERLEV-DC-00                            
093000                     - WS-ANTAL-DAM-DC-00 - WS-ANTAL-LOST-DC              
093100                                                                          
093200************                                                              
093300     IF WS-SUMMA-TOT-BIN-DC < ZERO                                        
093400        MOVE ZERO TO WS-SUMMA-TOT-BIN-DC                                  
093500     END-IF                                                               
093600     MOVE WS-SUMMA-TOT-BIN-DC TO RAD-TOT-BINNED-DC                        
093700************                                                              
093800                                                                          
093900***  COMPUTE WS-SUMMA-ANTMOT-DC = WS-SUMMA-BIN-DC - WS-ANTAL-NY-DC        
094000***               - WS-ANTAL-FOUND-KOLLI-DC                               
094100***               - WS-ANTAL-FOUND-DC                                     
094200                                                                          
094300     IF WS-SUMMA-ANTMOT-DC = ZERO                                         
094400        MOVE ZERO TO RAD-CORRECT-LINE-DC                                  
094500     ELSE                                                                 
094600       COMPUTE WS-FEL-PROC = WS-SUMMA-ERR-DC * 100 /                      
094700        WS-SUMMA-ANTMOT-DC                                                
094800       COMPUTE WS-CORR-PROC = 100 - WS-FEL-PROC                           
094900       MOVE WS-CORR-PROC TO RAD-CORRECT-LINE-DC                           
095000       IF WS-CORR-PROC < 0                                                
095100          MOVE ZERO TO RAD-CORRECT-LINE-DC                                
095200       END-IF                                                             
095300     END-IF                                                               
095400                                                                          
095500     COMPUTE WS-SUMMA-ANTMOT-DC = WS-SUMMA-BIN-DC - WS-ANTAL-NY-DC        
095600                  - WS-ANTAL-FOUND-DC                                     
095700                                                                          
095800     IF WS-SUMMA-ANTMOT-DC = ZERO                                         
095900        MOVE ZERO TO RAD-CORRECT-DC                                       
096000     ELSE                                                                 
096100       COMPUTE WS-FEL-PROC = WS-SUMMA-ERR-KOLLI-DC * 100 /                
096200        WS-SUMMA-ANTMOT-DC                                                
096300       COMPUTE WS-CORR-PROC = 100 - WS-FEL-PROC                           
096400       MOVE WS-CORR-PROC TO RAD-CORRECT-DC                                
096500       IF WS-CORR-PROC < 0                                                
096600          MOVE ZERO TO RAD-CORRECT-DC                                     
096700       END-IF                                                             
096800     END-IF                                                               
096900                                                                          
097000     MOVE SPACE TO SEND-RAD                                               
097100     MOVE TOT-RAD TO SEND-RAD                                             
097200     PERFORM S90-PUT-DOC-LINE                                             
097300     .                                                                    
097400     EJECT                                                                
097500 H-EJ-AVVIKELSE SECTION.                                                  
097600                                                                          
097700     MOVE TAB-IDDC-SEND(IX7) TO SPAR-IDDC-SEND                            
097800     MOVE TAB-IDDC-REC(IX7) TO SPAR-IDDC-REC                              
097900     MOVE TAB-ANTAL(IX7) TO WS-SUMMA-BIN                                  
098000     PERFORM E-SKAPA-UTRAD                                                
098100     PERFORM F-SPARA-LAGERVARDE                                           
098200     PERFORM S02-NOLLSTALL                                                
098300     .                                                                    
098400     EJECT                                                                
098500 Z-FINIT SECTION.                                                         
098600                                                                          
098700     CLOSE W6121F                                                         
098800           W6121E                                                         
098900     MOVE 'S' TO POSTSUM-OPKOD                                            
099000     CALL POSTSUM USING POSTSUM-PARM                                      
099100     .                                                                    
099200     EJECT                                                                
099300 S01-LAS-W6121F SECTION.                                                  
099400                                                                          
099500     READ W6121F INTO IN-AREA                                             
099600     AT END                                                               
099700        MOVE LOW-VALUE TO IN-AREA                                         
099800        SET END-OF-W6121F TO TRUE                                         
099900     NOT AT END                                                           
100000        MOVE 'W6121F'   TO POSTSUM-FDNAMN                                 
100100        MOVE 'W6121FD2' TO POSTSUM-DDNAMN2                                
100200        MOVE SPACE      TO POSTSUM-TRANSTYP                               
100300        CALL POSTSUM USING POSTSUM-PARM                                   
100400     END-READ                                                             
100500     .                                                                    
100600     EJECT                                                                
100700 S02-NOLLSTALL SECTION.                                                   
100800                                                                          
100900     MOVE ZERO TO WS-ANTAL-OVERLEV                                        
101000                  WS-ANTAL-UNDERLEV                                       
101100                  WS-ANTAL-UNDERLEV-00                                    
101200                  WS-ANTAL-UNDERLEV-XX                                    
101300                  WS-ANTAL-DAM                                            
101400                  WS-ANTAL-DAM-00                                         
101500                  WS-ANTAL-DAM-XX                                         
101600                  WS-ANTAL-LOST                                           
101700                  WS-ANTAL-LOST-KOLLI                                     
101800                  WS-ANTAL-FOUND                                          
101900                  WS-ANTAL-FOUND-KOLLI                                    
102000                  WS-ANTAL-NY                                             
102100                  WS-ANTAL                                                
102200                  WS-SUMMA-ERR                                            
102300                  WS-SUMMA-ERR-KOLLI                                      
102400                  WS-SUMMA-TOT                                            
102500                  WS-SUMMA-BIN                                            
102600                  WS-SUMMA-TOT-BIN                                        
102700                  WS-SUMMA-ANTMOT                                         
102800     .                                                                    
102900     EJECT                                                                
103000 S03-SPARA-SORT SECTION.                                                  
103100                                                                          
103200     MOVE IN-IDFAKT   TO SPAR-IDFAKT                                      
103300     MOVE IN-IDKUNDRF TO SPAR-IDKUNDRF                                    
103400     MOVE IN-IDKUNDNR TO SPAR-IDKUNDNR                                    
103500     MOVE IN-IDKOLLI  TO SPAR-IDKOLLI                                     
103600     MOVE IN-KDSORT1  TO SPAR-KDSORT1                                     
103700     .                                                                    
103800     EJECT                                                                
103900 S04-LAS-W6121E SECTION.                                                  
104000                                                                          
104100     READ W6121E INTO IN-W6121E-AREA                                      
104200     AT END                                                               
104300        MOVE HIGH-VALUE TO IN-W6121E-AREA                                 
104400        SET END-OF-W6121E TO TRUE                                         
104500     NOT AT END                                                           
104600        MOVE 'W6121E'   TO POSTSUM-FDNAMN                                 
104700        MOVE 'W6121FD1' TO POSTSUM-DDNAMN2                                
104800        MOVE SPACE      TO POSTSUM-TRANSTYP                               
104900        CALL POSTSUM USING POSTSUM-PARM                                   
105000     END-READ                                                             
105100     .                                                                    
105200     EJECT                                                                
105300 S05-NOLLSTALL-TAB SECTION.                                               
105400                                                                          
105500     MOVE +1 TO IX                                                        
105600     PERFORM UNTIL IX > IX-MAX                                            
105700        MOVE HIGH-VALUE TO TAB-IDDC-REC(IX)                               
105800                           TAB-IDDC-SEND(IX)                              
105900        MOVE ZERO  TO TAB-ANTAL(IX)                                       
106000        ADD +1 TO IX                                                      
106100     END-PERFORM                                                          
106200                                                                          
106300     MOVE +1 TO IX                                                        
106400     PERFORM UNTIL IX > IX-MAX                                            
106500        MOVE SPACE TO TAB-KONTR-REC(IX)                                   
106600                      TAB-KONTR-SEND(IX)                                  
106700        MOVE ZERO  TO TAB-KONTR-ANTAL(IX)                                 
106800        ADD +1 TO IX                                                      
106900     END-PERFORM                                                          
107000     .                                                                    
107100     EJECT                                                                
107200 S06-NOLLSTALL SECTION.                                                   
107300                                                                          
107400     MOVE ZERO TO WS-ANTAL-OVERLEV-DC                                     
107500                  WS-ANTAL-UNDERLEV-DC                                    
107600                  WS-ANTAL-UNDERLEV-DC-00                                 
107700                  WS-ANTAL-UNDERLEV-DC-XX                                 
107800                  WS-ANTAL-DAM-DC                                         
107900                  WS-ANTAL-DAM-DC-00                                      
108000                  WS-ANTAL-DAM-DC-XX                                      
108100                  WS-ANTAL-LOST-DC                                        
108200                  WS-ANTAL-LOST-KOLLI-DC                                  
108300                  WS-ANTAL-FOUND-DC                                       
108400                  WS-ANTAL-FOUND-KOLLI-DC                                 
108500                  WS-ANTAL-NY-DC                                          
108600                  WS-ANTAL-DC                                             
108700                  WS-SUMMA-ERR-DC                                         
108800                  WS-SUMMA-ERR-KOLLI-DC                                   
108900                  WS-SUMMA-TOT-DC                                         
109000                  WS-SUMMA-BIN-DC                                         
109100                  WS-SUMMA-TOT-BIN-DC                                     
109200                  WS-SUMMA-ANTMOT-DC                                      
109300     .                                                                    
109400     EJECT                                                                
109500 S90-SEND-OPEN SECTION.                                                   
109600     MOVE 'OPEN'                        TO SEND-KDFUNC                    
109700     MOVE 'CARPARTS.DAP.DISTRDOC'       TO SEND-ADDISPABS                 
109800     CALL WZ01SEND USING SEND-CONTROL-AREA                                
109900                         SEND-OPEN-AREA                                   
110000     IF SEND-KDRC > 0                                                     
110100       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
110200       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
110300       DELIMITED BY SIZE INTO ERRTEXT                                     
110400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
110500     END-IF                                                               
110600     .                                                                    
110700     EJECT                                                                
110800 S90-PUT-DAP-START SECTION.                                               
110900                                                                          
111000     MOVE 1                       TO REQU-IDMSGVER                        
111100     MOVE 'R'                     TO REQU-KDPGMACT                        
111200     MOVE IDPGM                   TO REQU-IDUSER                          
111300     MOVE 'W6121F-001'            TO HDR-IDOUTTYPE                        
111400     MOVE SPACE                   TO HDR-IDOUTREC                         
111500                                     HDR-IDLIST                           
111600     MOVE 'W6121F'                TO HDR-IDOUTREC                         
111700                                     HDR-IDLIST                           
111800     MOVE 'PUT'                   TO SEND-KDFUNC                          
111900     MOVE LENGTH OF HDR-AREA      TO SEND-KVDLEN                          
112000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
112100                         SEND-KVDLEN                                      
112200                         HDR-AREA                                         
112300     IF SEND-KDRC > ZERO                                                  
112400       MOVE SEND-KDRC             TO KDRC-DISPLAY                         
112500       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
112600       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
112700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
112800     END-IF                                                               
112900     .                                                                    
113000     EJECT                                                                
113100 S90-PUT-DOC-LINE SECTION.                                                
113200                                                                          
113300     MOVE 'PUT'                           TO SEND-KDFUNC                  
113400*                     -- UTAN STYRTECKEN:                                 
113500     MOVE LENGTH OF SEND-RAD              TO SEND-KVDLEN                  
113600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
113700                         SEND-KVDLEN                                      
113800*                     -- UTAN STYRTECKEN:                                 
113900                         SEND-RAD                                         
114000     IF SEND-KDRC > ZERO                                                  
114100       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
114200       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
114300       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
114400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
114500     END-IF                                                               
114600     .                                                                    
114700     EJECT                                                                
114800 S90-SEND-CLOSE SECTION.                                                  
114900                                                                          
115000     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
115100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
115200                                                                          
115300     IF SEND-KDRC > 0                                                     
115400       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
115500       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
115600       DELIMITED BY SIZE INTO ERRTEXT                                     
115700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
115800     END-IF                                                               
115900     .                                                                    
116000     EJECT                                                                
116100 IMS-GU-WDB601    SECTION.                                                
116200     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
116300          DELIMITED BY SIZE INTO SSA1                                     
116400     MOVE '  GE' TO GOOD-STATUSCODES                                      
116500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
116600     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
116700     PERFORM IMS-STATUSKONTROLL                                           
116800     .                                                                    
116900     EJECT                                                                
117000                                                                          
117100 IMS-STATUSKONTROLL SECTION.                                              
117200     SET STATUS-IX TO 1                                                   
117300     SEARCH GOOD-STATUS                                                   
117400       AT END                                                             
117500         CALL FELLOG                                                      
117600       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
117700         CONTINUE                                                         
117800     END-SEARCH                                                           
117900     .                                                                    
118000     EJECT                                                                
