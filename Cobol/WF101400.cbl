000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WF101400.                                                
000300 AUTHOR.         BO HAMMARIN.                                             
000400 DATE-WRITTEN.   FEBRUARI 2002.                                           
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        THE PGM                                                          
000900*        - READS FILE WITH NEW/CHANGED/DELETED ACCOUNTING RECORDS         
001000*        - READS FILE WITH NEW/CHANGED/DELETED COST CENTER RECORDS        
001100*        - READS FILE WITH NEW/CHANGED/DELETED ORDERNO RECORDS            
001200*        - READS FILE WITH NEW/CHANGED         CUSTOMER RECORDS           
001300*        - READS FILE WITH NEW/CHANGED/DELETED VAT CODE RECORDS           
001400*        - SENDS ACCOUNT-/COST CTR-/ORDERNO-/CUSTOMER/VAT DATA TO         
001500*          . PULS BY USING WZ01SEND (CARPARTS.PULS.RECFINDOC)             
001600*                                                                         
001700*                                                                         
001800                                                                          
001900 ENVIRONMENT DIVISION.                                                    
002000                                                                          
002100 INPUT-OUTPUT SECTION.                                                    
002200 FILE-CONTROL.                                                            
002300*          --- ACCOUNT-RECORDS                                            
002400     SELECT WF1014                     ASSIGN TO WF1014D1.                
002500                                                                          
002600*          --- COSTCTR-RECORDS                                            
002700     SELECT WF1015                     ASSIGN TO WF1014D2.                
002800                                                                          
002900*          --- ORDERNO-RECORDS                                            
003000     SELECT WF1016                     ASSIGN TO WF1014D3.                
003100                                                                          
003200*          --- CUSTOMER-RECORDS                                           
003300     SELECT WF1018                     ASSIGN TO WF1014D4.                
003400                                                                          
003500*          --- VAT CODE-RECORDS                                           
003600     SELECT WF1017                     ASSIGN TO WF1014D5.                
003700     EJECT                                                                
003800                                                                          
003900 DATA DIVISION.                                                           
004000                                                                          
004100 FILE SECTION.                                                            
004200 FD  WF1014                                                               
004300     RECORDING       F                                                    
004400     BLOCK CONTAINS  0.                                                   
004500                                                                          
004600*01  -COPY WF10M10     -L.                                                
004700     EJECT                                                                
004800                                                                          
004900 FD  WF1015                                                               
005000     RECORDING       F                                                    
005100     BLOCK CONTAINS  0.                                                   
005200                                                                          
005300*01  -COPY WF10M11     -L.                                                
005400     EJECT                                                                
005500                                                                          
005600 FD  WF1016                                                               
005700     RECORDING       F                                                    
005800     BLOCK CONTAINS  0.                                                   
005900                                                                          
006000*01  -COPY WF10M13     -L.                                                
006100     EJECT                                                                
006200                                                                          
006300 FD  WF1018                                                               
006400     RECORDING       F                                                    
006500     BLOCK CONTAINS  0.                                                   
006600                                                                          
006700*01  -COPY WF10CUS2    -L.                                                
006800     EJECT                                                                
006900                                                                          
007000 FD  WF1017                                                               
007100     RECORDING       F                                                    
007200     BLOCK CONTAINS  0.                                                   
007300                                                                          
007400*01  -COPY WF10M17     -L.                                                
007500     EJECT                                                                
007600                                                                          
007700 WORKING-STORAGE SECTION.                                                 
007800 77  IDPGM                       PIC X(8)    VALUE 'WF101400'.            
007900 77  WF1014-EOF-SW               PIC X       VALUE 'N'.                   
008000     88  END-OF-WF1014                       VALUE 'J'.                   
008100 77  WF1015-EOF-SW               PIC X       VALUE 'N'.                   
008200     88  END-OF-WF1015                       VALUE 'J'.                   
008300 77  WF1016-EOF-SW               PIC X       VALUE 'N'.                   
008400     88  END-OF-WF1016                       VALUE 'J'.                   
008500 77  WF1018-EOF-SW               PIC X       VALUE 'N'.                   
008600     88  END-OF-WF1018                       VALUE 'J'.                   
008700 77  WF1017-EOF-SW               PIC X       VALUE 'N'.                   
008800     88  END-OF-WF1017                       VALUE 'J'.                   
008900                                                                          
009000 01  ERRTEXT.                                                             
009100     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
009200     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
009300 01  KDRC-DISPLAY                PIC Z(5).                                
009400     EJECT                                                                
009500                                                                          
009600 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
009700 01  FILLER REDEFINES TODAYS-DATE.                                        
009800     03  TODAYS-DATE-YEAR        PIC 9(2).                                
009900     03  TODAYS-DATE-MONTH       PIC 9(2).                                
010000     03  TODAYS-DATE-DAY         PIC 9(2).                                
010100     EJECT                                                                
010200                                                                          
010300 01  GENERAL-SUBPROGRAMS.                                                 
010400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
010500     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
010600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
010700     EJECT                                                                
010800                                                                          
010900*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
011000                                                                          
011100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
011200 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
011300 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
011400     EJECT                                                                
011500*    --- PARAMETRAR TILL POSTSUM                                          
011600*                                                                         
011700*01  -COPY W0005   -PRE  POSTSUM-                                         
011800     EJECT                                                                
011900                                                                          
012000*    --- AREOR FÖR KOMMUNIKATION                                          
012100 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
012200*01  -COPY WZ01SEND                                                       
012300     EJECT                                                                
012400                                                                          
012500 01  UT10-AREA-START             PIC X(24)   VALUE                        
012600                                             'UT10-AREA-START'.           
012700 01  UT10-AREA.                                                           
012800     03  UT10-IDPTYP             PIC X(3)    VALUE 'M10'.                 
012900*    03  -COPY WF10M10          -PRE UT10-                                
013000*                                                                         
013100     EJECT                                                                
013200                                                                          
013300 01  UT11-AREA-START             PIC X(24)   VALUE                        
013400                                             'UT11-AREA-START'.           
013500 01  UT11-AREA.                                                           
013600     03  UT11-IDPTYP             PIC X(3)    VALUE 'M11'.                 
013700*    03  -COPY WF10M11          -PRE UT11-                                
013800*                                                                         
013900     EJECT                                                                
014000                                                                          
014100 01  UT13-AREA-START             PIC X(24)   VALUE                        
014200                                             'UT13-AREA-START'.           
014300 01  UT13-AREA.                                                           
014400     03  UT13-IDPTYP             PIC X(3)    VALUE 'M13'.                 
014500*    03  -COPY WF10M13          -PRE UT13-                                
014600*                                                                         
014700     EJECT                                                                
014800                                                                          
014900 01  UT00-AREA-START             PIC X(24)   VALUE                        
015000                                             'UT00-AREA-START'.           
015100 01  UT00-AREA.                                                           
015200     03  UT00-IDPTYP             PIC X(3)    VALUE 'M00'.                 
015300*    03  -COPY WF10CUS2         -PRE UT00-                                
015400*                                                                         
015500     EJECT                                                                
015600 01  UT17-AREA-START             PIC X(24)   VALUE                        
015610                                             'UT17-AREA-START'.           
015620 01  UT17-AREA.                                                           
015630     03  UT17-IDPTYP             PIC X(3)    VALUE 'M17'.                 
015640*    03  -COPY WF10M17          -PRE UT17-                                
015650*                                                                         
015660     EJECT                                                                
015700                                                                          
015800 LINKAGE SECTION.                                                         
015900*01  -COPY W0009   -PRE MSG-                                              
016000     EJECT                                                                
016100                                                                          
016200 PROCEDURE DIVISION  USING MSG-PCB.                                       
016300                                                                          
016400 MAIN SECTION.                                                            
016500     ENTRY 'DLITCBL' USING MSG-PCB.                                       
016600                                                                          
016700     PERFORM A-INIT                                                       
016800                                                                          
016900     PERFORM B-DAILY-FINANCE                                              
017000                                                                          
017100     PERFORM Z-FINIT                                                      
017200                                                                          
017300     MOVE ZERO TO RETURN-CODE                                             
017400     GOBACK                                                               
017500     .                                                                    
017600     EJECT                                                                
017700                                                                          
017800 A-INIT SECTION.                                                          
017900     OPEN INPUT WF1014                                                    
018000                WF1015                                                    
018100                WF1016                                                    
018200                WF1018                                                    
018210                WF1017                                                    
018300                                                                          
018400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
018500     .                                                                    
018600     EJECT                                                                
018700                                                                          
018800 B-DAILY-FINANCE SECTION.                                                 
018900     PERFORM S11-FINANCE-OPEN                                             
019000                                                                          
019100     PERFORM S01-READ-WF1014                                              
019200     PERFORM UNTIL END-OF-WF1014                                          
019300       PERFORM S51-ACCOUNT-PUT                                            
019400       PERFORM S01-READ-WF1014                                            
019500     END-PERFORM                                                          
019600                                                                          
019700     PERFORM S02-READ-WF1015                                              
019800     PERFORM UNTIL END-OF-WF1015                                          
019900       PERFORM S52-COSTCTR-PUT                                            
020000       PERFORM S02-READ-WF1015                                            
020100     END-PERFORM                                                          
020200                                                                          
020300     PERFORM S03-READ-WF1016                                              
020400     PERFORM UNTIL END-OF-WF1016                                          
020500       PERFORM S53-ORDERNO-PUT                                            
020600       PERFORM S03-READ-WF1016                                            
020700     END-PERFORM                                                          
020800                                                                          
020900     PERFORM S05-READ-WF1018                                              
021000     PERFORM UNTIL END-OF-WF1018                                          
021100       PERFORM S54-CUSTOMER-PUT                                           
021200       PERFORM S05-READ-WF1018                                            
021300     END-PERFORM                                                          
021400                                                                          
021410     PERFORM S06-READ-WF1017                                              
021420     PERFORM UNTIL END-OF-WF1017                                          
021430       PERFORM S55-VAT-PUT                                                
021440       PERFORM S06-READ-WF1017                                            
021450     END-PERFORM                                                          
021460                                                                          
021500     PERFORM S12-FINANCE-CLOSE                                            
021600     .                                                                    
021700     EJECT                                                                
021800                                                                          
021900 Z-FINIT SECTION.                                                         
022000     CLOSE WF1014                                                         
022100     CLOSE WF1015                                                         
022200     CLOSE WF1016                                                         
022300     CLOSE WF1018                                                         
022310     CLOSE WF1017                                                         
022400                                                                          
022500     MOVE 'S' TO POSTSUM-OPKOD                                            
022600     CALL POSTSUM USING POSTSUM-PARM                                      
022700     .                                                                    
022800     EJECT                                                                
022900                                                                          
023000 S01-READ-WF1014  SECTION.                                                
023100     READ WF1014          INTO UT10-WF10M10                               
023200     AT END                                                               
023300        MOVE HIGH-VALUE   TO   UT10-WF10M10                               
023400        SET END-OF-WF1014 TO   TRUE                                       
023500     NOT AT END                                                           
023600        MOVE 'WF1014'     TO POSTSUM-FDNAMN                               
023700        MOVE 'WF1014D1'   TO POSTSUM-DDNAMN2                              
023800        MOVE 'M10'        TO POSTSUM-TRANSTYP                             
023900        CALL POSTSUM USING POSTSUM-PARM                                   
024000     END-READ                                                             
024100     .                                                                    
024200                                                                          
024300 S02-READ-WF1015  SECTION.                                                
024400     READ WF1015          INTO UT11-WF10M11                               
024500     AT END                                                               
024600        MOVE HIGH-VALUE   TO   UT11-WF10M11                               
024700        SET END-OF-WF1015 TO   TRUE                                       
024800     NOT AT END                                                           
024900        MOVE 'WF1015'     TO POSTSUM-FDNAMN                               
025000        MOVE 'WF1014D2'   TO POSTSUM-DDNAMN2                              
025100        MOVE 'M11'        TO POSTSUM-TRANSTYP                             
025200        CALL POSTSUM USING POSTSUM-PARM                                   
025300     END-READ                                                             
025400     .                                                                    
025500                                                                          
025600 S03-READ-WF1016  SECTION.                                                
025700     READ WF1016          INTO UT13-WF10M13                               
025800     AT END                                                               
025900        MOVE HIGH-VALUE   TO   UT13-WF10M13                               
026000        SET END-OF-WF1016 TO   TRUE                                       
026100     NOT AT END                                                           
026200        MOVE 'WF1016'     TO POSTSUM-FDNAMN                               
026300        MOVE 'WF1014D3'   TO POSTSUM-DDNAMN2                              
026400        MOVE 'M13'        TO POSTSUM-TRANSTYP                             
026500        CALL POSTSUM USING POSTSUM-PARM                                   
026600     END-READ                                                             
026700     .                                                                    
026800                                                                          
026900 S05-READ-WF1018  SECTION.                                                
027000     READ WF1018          INTO UT00-WF10CUST                              
027100     AT END                                                               
027200        MOVE HIGH-VALUE   TO   UT00-WF10CUST                              
027300        SET END-OF-WF1018 TO   TRUE                                       
027400     NOT AT END                                                           
027500        MOVE 'WF1018'     TO POSTSUM-FDNAMN                               
027600        MOVE 'WF1014D4'   TO POSTSUM-DDNAMN2                              
027700        MOVE 'CUST'       TO POSTSUM-TRANSTYP                             
027800        CALL POSTSUM USING POSTSUM-PARM                                   
027900     END-READ                                                             
028000     .                                                                    
028100     EJECT                                                                
028200                                                                          
028210 S06-READ-WF1017  SECTION.                                                
028220     READ WF1017          INTO UT17-WF10M17                               
028230     AT END                                                               
028240        MOVE HIGH-VALUE   TO   UT17-WF10M17                               
028250        SET END-OF-WF1017 TO   TRUE                                       
028260     NOT AT END                                                           
028270        MOVE 'WF1017'     TO POSTSUM-FDNAMN                               
028280        MOVE 'WF1014D5'   TO POSTSUM-DDNAMN2                              
028290        MOVE 'M17'        TO POSTSUM-TRANSTYP                             
028291        CALL POSTSUM USING POSTSUM-PARM                                   
028292     END-READ                                                             
028293     .                                                                    
028294                                                                          
028300 S11-FINANCE-OPEN SECTION.                                                
028400     MOVE 'CARPARTS.PULS.RECFINDOC'       TO SEND-ADDISPABS               
028500     MOVE 'OPEN'                          TO SEND-KDFUNC                  
028600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
028700                         SEND-OPEN-AREA                                   
028800     IF SEND-KDRC > ZERO                                                  
028900       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
029000       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
029100       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
029200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
029300     END-IF                                                               
029400     .                                                                    
029500     EJECT                                                                
029600                                                                          
029700 S12-FINANCE-CLOSE SECTION.                                               
029800     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
029900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
030000     .                                                                    
030100     EJECT                                                                
030200                                                                          
030300 S51-ACCOUNT-PUT SECTION.                                                 
030400     MOVE 'PUT'                           TO SEND-KDFUNC                  
030500     MOVE LENGTH OF UT10-AREA             TO SEND-KVDLEN                  
030600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
030700                         SEND-KVDLEN                                      
030800                         UT10-AREA                                        
030900     IF SEND-KDRC > 1                                                     
031000       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
031100       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
031200       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
031300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
031400     ELSE                                                                 
031500       MOVE 'WF1014 '  TO POSTSUM-FDNAMN                                  
031600       MOVE 'SEND ACC' TO POSTSUM-DDNAMN2                                 
031700       MOVE 'M10'      TO POSTSUM-TRANSTYP                                
031800       CALL POSTSUM USING POSTSUM-PARM                                    
031900     END-IF                                                               
032000     .                                                                    
032100     EJECT                                                                
032200                                                                          
032300 S52-COSTCTR-PUT SECTION.                                                 
032400     MOVE 'PUT'                           TO SEND-KDFUNC                  
032500     MOVE LENGTH OF UT11-AREA             TO SEND-KVDLEN                  
032600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
032700                         SEND-KVDLEN                                      
032800                         UT11-AREA                                        
032900     IF SEND-KDRC > 1                                                     
033000       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
033100       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
033200       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
033300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
033400     ELSE                                                                 
033500       MOVE 'WF1015 '  TO POSTSUM-FDNAMN                                  
033600       MOVE 'SEND COS' TO POSTSUM-DDNAMN2                                 
033700       MOVE 'M11'      TO POSTSUM-TRANSTYP                                
033800       CALL POSTSUM USING POSTSUM-PARM                                    
033900     END-IF                                                               
034000     .                                                                    
034100     EJECT                                                                
034200                                                                          
034300 S53-ORDERNO-PUT SECTION.                                                 
034400     MOVE 'PUT'                           TO SEND-KDFUNC                  
034500     MOVE LENGTH OF UT13-AREA             TO SEND-KVDLEN                  
034600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
034700                         SEND-KVDLEN                                      
034800                         UT13-AREA                                        
034900     IF SEND-KDRC > 1                                                     
035000       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
035100       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
035200       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
035300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
035400     ELSE                                                                 
035500       MOVE 'WF1016 '  TO POSTSUM-FDNAMN                                  
035600       MOVE 'SEND ORD' TO POSTSUM-DDNAMN2                                 
035700       MOVE 'M13'      TO POSTSUM-TRANSTYP                                
035800       CALL POSTSUM USING POSTSUM-PARM                                    
035900     END-IF                                                               
036000     .                                                                    
036100     EJECT                                                                
036200                                                                          
036300 S54-CUSTOMER-PUT SECTION.                                                
036400     MOVE 'PUT'                           TO SEND-KDFUNC                  
036500     MOVE LENGTH OF UT00-AREA             TO SEND-KVDLEN                  
036600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
036700                         SEND-KVDLEN                                      
036800                         UT00-AREA                                        
036900     IF SEND-KDRC > 1                                                     
037000       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
037100       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
037200       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
037300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
037400     ELSE                                                                 
037500       MOVE 'WF1018 '  TO POSTSUM-FDNAMN                                  
037600       MOVE 'SEND CUS' TO POSTSUM-DDNAMN2                                 
037700       MOVE 'CUST'     TO POSTSUM-TRANSTYP                                
037800       CALL POSTSUM USING POSTSUM-PARM                                    
037900     END-IF                                                               
038000     .                                                                    
038010                                                                          
038100 S55-VAT-PUT SECTION.                                                     
038200     MOVE 'PUT'                           TO SEND-KDFUNC                  
038300     MOVE LENGTH OF UT17-AREA             TO SEND-KVDLEN                  
038400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
038500                         SEND-KVDLEN                                      
038600                         UT17-AREA                                        
038700     IF SEND-KDRC > 1                                                     
038800       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
038900       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
039000       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
039100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
039200     ELSE                                                                 
039300       MOVE 'WF1017 '  TO POSTSUM-FDNAMN                                  
039400       MOVE 'SEND VAT' TO POSTSUM-DDNAMN2                                 
039500       MOVE 'M17'      TO POSTSUM-TRANSTYP                                
039600       CALL POSTSUM USING POSTSUM-PARM                                    
039700     END-IF                                                               
039800     .                                                                    
039900     EJECT                                                                
040000                                                                          
