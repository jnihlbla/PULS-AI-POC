000100 ID DIVISION.                                                             
000201 PROGRAM-ID.     W6W11500.                                                
000301 AUTHOR.         ANDRE KJELL.                                             
000401 DATE-WRITTEN.   11/12/06.                                                
000501 DATE-COMPILED.                                                           
000601                                                                          
000701*    NAME:       CARPARTS.NDC.ACTIVATEADVICENOTE                          
000801*                                                                         
000901*    FUNCTION:                                                            
001001*        THIS IS A DRIVER PGM FOR TRANSACTION W6W115T                     
001101*                                                                         
001201*        IT TAKES CARE OF TECHNICAL DETAILS RELATED BEING CALLED          
001301*        VIA IMS-CONNECT AND CALLS SUBPROGRAM W6011510 WHICH              
001401*        CONTAINS ALL BUSINESS LOGIC FOR THIS TRANSACTION.                
001501*                                                                         
001601*        A SIMILAR DRIVER PROGRAM FOR INVOCATIONS FROM A 3270 TER-        
001701*        MINAL EXISTS - W6011500 (TRANSACTION W6T115)                     
001801*                                                                         
001901*    INDATA.                                                              
002001*        TRANSACTION: W6W115T                                             
002101*        REQUEST:     W60115I1                                            
002201*                                                                         
002301*    OUTDATA.                                                             
002401*        RESPONSE:    W60115O1                                            
002501                                                                          
002601     SKIP3                                                                
002701 ENVIRONMENT DIVISION.                                                    
002801     SKIP2                                                                
002901 INPUT-OUTPUT SECTION.                                                    
003001                                                                          
003101 FILE-CONTROL.                                                            
003201     EJECT                                                                
003301 DATA DIVISION.                                                           
003401     SKIP3                                                                
003501 FILE SECTION.                                                            
003601     EJECT                                                                
003701 WORKING-STORAGE SECTION.                                                 
003801 77  IDPGM                       PIC X(08)   VALUE 'W6W11500'.            
003901                                                                          
004001*    --- WOHK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004101 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004201 77  KDRC-DISPLAY                PIC Z(5).                                
004301                                                                          
004401 77  YES                         PIC X       VALUE 'J'.                   
004501 77  NOO                         PIC X       VALUE 'N'.                   
004601 77  MAX-KVRADER                 PIC S9(4)   VALUE +500 COMP.             
004701                                                                          
004801 77  KEYS-SW                     PIC X       VALUE 'J'.                   
004901     88  KEYS-OK                             VALUE 'J'.                   
005001     88  KEYS-WRONG                          VALUE 'N'.                   
005101     EJECT                                                                
005201*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
005301 01  GENERAL-SUBPROGRAMS.                                                 
005401     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005501     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
005601     03  W6011510                PIC X(8)    VALUE 'W6011510'.            
005701     SKIP3                                                                
005801*    --- PARAMETERS TO ABEND                                              
005901                                                                          
006001 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006101 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006201 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006301     EJECT                                                                
006401*                                                                         
006501 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
006601     SKIP3                                                                
006701*01  -COPY WZ01SUB                                                        
006801     EJECT                                                                
006901 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
007001     SKIP3                                                                
007101 01  REQU-AREA.                                                           
007201*    03  -COPY WZ01REQU                                                   
007301*    03  -COPY W60115I1                                                   
007401     EJECT                                                                
007501 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
007601     SKIP3                                                                
007701 01  RESP-AREA.                                                           
007801*    03  -COPY WZ01RESP                                                   
007901*    03  -COPY W60115O1                                                   
008001     EJECT                                                                
008101 LINKAGE SECTION.                                                         
008201                                                                          
008301 01  MSG-PCB                     PIC X.                                   
008401                                                                          
008701 01  DISP-PCB                    PIC X.                                   
008801                                                                          
008901 01  STAT-PCB                    PIC X.                                   
009001                                                                          
009101 01  AR-PCB                      PIC X.                                   
009201                                                                          
009301 01  FR-PCB                      PIC X.                                   
009401                                                                          
009501 01  ILIST-PCB                   PIC X.                                   
009502                                                                          
009503 01  SYNQ-PCB                    PIC X.                                   
009601                                                                          
009701 01  ARTC-PCB                    PIC X.                                   
009801                                                                          
009901 01  WDB6-PCB                    PIC X.                                   
010001                                                                          
010100 01  INLA1-PCB                   PIC X.                                   
010200                                                                          
010300 01  INLA2-PCB                   PIC X.                                   
010400                                                                          
010500 01  INLB-PCB                    PIC X.                                   
010600                                                                          
010700 01  INLA3-PCB                   PIC X.                                   
010800                                                                          
010900 01  INLA-D-PCB                  PIC X.                                   
011000                                                                          
011100 01  W6D1-I-PCB                  PIC X.                                   
011200                                                                          
011300 01  PLAA-PCB                    PIC X.                                   
011400                                                                          
011500 01  LASA-PCB                    PIC X.                                   
011600                                                                          
011700 01  LOPA-PCB                    PIC X.                                   
011800                                                                          
011900 01  ARTS-PCB                    PIC X.                                   
012000                                                                          
012100 01  WDJ1-PCB                    PIC X.                                   
012200                                                                          
012300 01  WDJ2-PCB                    PIC X.                                   
012400                                                                          
012500 01  INLA4-PCB                   PIC X.                                   
012601                                                                          
012701 01  WDK9-PCB                    PIC X.                                   
012801                                                                          
012901 01  WDL2-PCB                    PIC X.                                   
013001                                                                          
013101 01  WDD9-PCB                    PIC X.                                   
013201                                                                          
013301 01  WDD8-PCB                    PIC X.                                   
013400*    PCB'ER FÖR SUBPGM                                                    
013500                                                                          
013600 01 STYR-HANB-PCB                PIC X.                                   
013700                                                                          
013800 01 STYR-PLAA-PCB                PIC X.                                   
013900                                                                          
014000 01 PRIO-INLA-PCB                PIC X.                                   
014100                                                                          
014200 01 PRIO-ARTC-PCB                PIC X.                                   
014300                                                                          
014400 01 PRIO-ARTM-PCB                PIC X.                                   
014500                                                                          
014600 01 PRIO-ORDQ-PCB                PIC X.                                   
014700                                                                          
014800 01 PRIO-ARTS-PCB                PIC X.                                   
014900                                                                          
015000 01 PRIO-KVAI-PCB                PIC X.                                   
015100                                                                          
015200 01 PRIO-INLI1-PCB               PIC X.                                   
015300                                                                          
015400 01 PRIO-KVAE-PCB                PIC X.                                   
015500                                                                          
015600 01 KOM-KOMA-PCB                 PIC X.                                   
015700                                                                          
015800 01 KVAL-ARTC-PCB                PIC X.                                   
015900                                                                          
016000 01 KVAL-KVAH1-PCB               PIC X.                                   
016100                                                                          
016200 01 KVAL-KVAH2-PCB               PIC X.                                   
016300                                                                          
016400 01 KVAL-KVAG-PCB                PIC X.                                   
016500                                                                          
016600 01 KVAL-LEVA-PCB                PIC X.                                   
016700                                                                          
016800 01 KVAL-UPFA-PCB                PIC X.                                   
016900                                                                          
017000 01 KVAL-PROA-PCB                PIC X.                                   
017100                                                                          
017200 01 KVAL-XXLA-PCB                PIC X.                                   
017300                                                                          
017400 01 KVAL-KODA-PCB                PIC X.                                   
017500                                                                          
017600 01 BEHOV-WDK6-PCB               PIC X.                                   
017700                                                                          
017800 01 BEHOV-WDK7-PCB               PIC X.                                   
017900                                                                          
018000 01 BEHOV-WDR2-PCB               PIC X.                                   
018100                                                                          
018200 01 BEHOV-WDE3-PCB               PIC X.                                   
018300                                                                          
018400 01 BEHOV-WDK6-2-PCB             PIC X.                                   
018500                                                                          
018600 01 BEHOV-WDK7-2-PCB             PIC X.                                   
018700                                                                          
018800 01 BEHOV-WDL6-PCB               PIC X.                                   
018900                                                                          
019000 01 BEHOV-WDD7A-PCB              PIC X.                                   
019100                                                                          
019200 01 BEHOV-WDB6-PCB               PIC X.                                   
019301                                                                          
019401 01 W22222-WDK6-PCB              PIC X.                                   
019501                                                                          
019601 01 W22222-WDK7-PCB              PIC X.                                   
019701                                                                          
019801 01 W22222-ARTM-PCB              PIC X.                                   
019901                                                                          
020001 01 W22222-2501-PCB              PIC X.                                   
020101                                                                          
020201 01 W22222-WDB6-PCB              PIC X.                                   
020301                                                                          
020401 01 W22222-WDD7-PCB              PIC X.                                   
020501                                                                          
020601 01 W22222-WDK7E-PCB             PIC X.                                   
020701                                                                          
020801 01 W6D2-PCB                     PIC X.                                   
020802                                                                          
020803 01 SYNQ-ATAB-PCB                PIC X.                                   
020804                                                                          
020805 01 WDQ3-PCB                     PIC X.                                   
020900                                                                          
021000     EJECT                                                                
021100                                                                          
021201 PROCEDURE DIVISION  USING MSG-PCB DISP-PCB STAT-PCB                      
021300                           AR-PCB FR-PCB ILIST-PCB SYNQ-PCB               
021400                           ARTC-PCB WDB6-PCB                              
021500                           INLA1-PCB INLA2-PCB INLB-PCB INLA3-PCB         
021600                           INLA-D-PCB W6D1-I-PCB                          
021700                           PLAA-PCB LASA-PCB LOPA-PCB ARTS-PCB            
021800                           WDJ1-PCB WDJ2-PCB INLA4-PCB                    
021901                           WDK9-PCB WDL2-PCB WDD9-PCB WDD8-PCB            
022000                           STYR-HANB-PCB STYR-PLAA-PCB                    
022100                           PRIO-INLA-PCB PRIO-ARTC-PCB                    
022200                           PRIO-ARTM-PCB PRIO-ORDQ-PCB                    
022300                           PRIO-ARTS-PCB PRIO-KVAI-PCB                    
022400                           PRIO-INLI1-PCB                                 
022500                           PRIO-KVAE-PCB                                  
022600                           KOM-KOMA-PCB                                   
022700                           KVAL-ARTC-PCB  KVAL-KVAH1-PCB                  
022800                           KVAL-KVAH2-PCB KVAL-KVAG-PCB                   
022900                           KVAL-LEVA-PCB  KVAL-UPFA-PCB                   
023000                           KVAL-PROA-PCB  KVAL-XXLA-PCB                   
023100                           KVAL-KODA-PCB                                  
023200                           BEHOV-WDK6-PCB BEHOV-WDK7-PCB                  
023300                           BEHOV-WDR2-PCB BEHOV-WDE3-PCB                  
023400                           BEHOV-WDK6-2-PCB BEHOV-WDK7-2-PCB              
023500                           BEHOV-WDL6-PCB BEHOV-WDD7A-PCB                 
023600                           BEHOV-WDB6-PCB                                 
023701                           W22222-WDK6-PCB W22222-WDK7-PCB                
023801                           W22222-ARTM-PCB W22222-2501-PCB                
023901                           W22222-WDB6-PCB W22222-WDD7-PCB                
024001                           W22222-WDK7E-PCB W6D2-PCB                      
024002                           SYNQ-ATAB-PCB WDQ3-PCB.                        
024100                                                                          
024201     ENTRY 'DLITCBL' USING MSG-PCB DISP-PCB STAT-PCB                      
024300                           AR-PCB FR-PCB ILIST-PCB SYNQ-PCB               
024400                           ARTC-PCB  WDB6-PCB                             
024500                           INLA1-PCB INLA2-PCB INLB-PCB INLA3-PCB         
024600                           INLA-D-PCB W6D1-I-PCB                          
024700                           PLAA-PCB LASA-PCB LOPA-PCB ARTS-PCB            
024800                           WDJ1-PCB WDJ2-PCB INLA4-PCB                    
024901                           WDK9-PCB WDL2-PCB WDD9-PCB WDD8-PCB            
025000                           STYR-HANB-PCB STYR-PLAA-PCB                    
025100                           PRIO-INLA-PCB PRIO-ARTC-PCB                    
025200                           PRIO-ARTM-PCB PRIO-ORDQ-PCB                    
025300                           PRIO-ARTS-PCB PRIO-KVAI-PCB                    
025400                           PRIO-INLI1-PCB                                 
025500                           PRIO-KVAE-PCB                                  
025600                           KOM-KOMA-PCB                                   
025700                           KVAL-ARTC-PCB  KVAL-KVAH1-PCB                  
025800                           KVAL-KVAH2-PCB KVAL-KVAG-PCB                   
025900                           KVAL-LEVA-PCB  KVAL-UPFA-PCB                   
026000                           KVAL-PROA-PCB  KVAL-XXLA-PCB                   
026100                           KVAL-KODA-PCB                                  
026200                           BEHOV-WDK6-PCB BEHOV-WDK7-PCB                  
026300                           BEHOV-WDR2-PCB BEHOV-WDE3-PCB                  
026400                           BEHOV-WDK6-2-PCB BEHOV-WDK7-2-PCB              
026500                           BEHOV-WDL6-PCB BEHOV-WDD7A-PCB                 
026600                           BEHOV-WDB6-PCB                                 
026701                           W22222-WDK6-PCB W22222-WDK7-PCB                
026801                           W22222-ARTM-PCB W22222-2501-PCB                
026901                           W22222-WDB6-PCB W22222-WDD7-PCB                
027001                           W22222-WDK7E-PCB W6D2-PCB                      
027002                           SYNQ-ATAB-PCB WDQ3-PCB.                        
027100                                                                          
027200     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
027300     IF SUB-KDRC = 0                                                      
027400       PERFORM A-INIT                                                     
027500                                                                          
027600       CALL W6011510 USING REQU-AREA   RESP-AREA    MAX-KVRADER           
027701                           MSG-PCB DISP-PCB STAT-PCB                      
027800                           AR-PCB FR-PCB ILIST-PCB SYNQ-PCB               
027900                           ARTC-PCB  WDB6-PCB                             
028000                           INLA1-PCB INLA2-PCB INLB-PCB INLA3-PCB         
028100                           INLA-D-PCB W6D1-I-PCB                          
028200                           PLAA-PCB LASA-PCB LOPA-PCB ARTS-PCB            
028300                           WDJ1-PCB WDJ2-PCB INLA4-PCB                    
028401                           WDK9-PCB WDL2-PCB WDD9-PCB WDD8-PCB            
028500                           STYR-HANB-PCB STYR-PLAA-PCB                    
028600                           PRIO-INLA-PCB PRIO-ARTC-PCB                    
028700                           PRIO-ARTM-PCB PRIO-ORDQ-PCB                    
028800                           PRIO-ARTS-PCB PRIO-KVAI-PCB                    
028900                           PRIO-INLI1-PCB                                 
029000                           PRIO-KVAE-PCB                                  
029100                           KOM-KOMA-PCB                                   
029200                           KVAL-ARTC-PCB  KVAL-KVAH1-PCB                  
029300                           KVAL-KVAH2-PCB KVAL-KVAG-PCB                   
029400                           KVAL-LEVA-PCB  KVAL-UPFA-PCB                   
029500                           KVAL-PROA-PCB  KVAL-XXLA-PCB                   
029600                           KVAL-KODA-PCB                                  
029700                           BEHOV-WDK6-PCB BEHOV-WDK7-PCB                  
029800                           BEHOV-WDR2-PCB BEHOV-WDE3-PCB                  
029900                           BEHOV-WDK6-2-PCB BEHOV-WDK7-2-PCB              
030000                           BEHOV-WDL6-PCB BEHOV-WDD7A-PCB                 
030100                           BEHOV-WDB6-PCB                                 
030201                           W22222-WDK6-PCB W22222-WDK7-PCB                
030301                           W22222-ARTM-PCB W22222-2501-PCB                
030401                           W22222-WDB6-PCB W22222-WDD7-PCB                
030501                           W22222-WDK7E-PCB W6D2-PCB                      
030502                           SYNQ-ATAB-PCB WDQ3-PCB                         
030600                                                                          
030700       PERFORM S02-RETURN-RESPONSE                                        
030800     END-IF                                                               
030900                                                                          
031000     PERFORM Z-FINIT                                                      
031100     MOVE ZERO TO RETURN-CODE                                             
031200     GOBACK                                                               
031300     .                                                                    
031400     EJECT                                                                
031500 A-INIT SECTION.                                                          
031600     CONTINUE                                                             
031700     .                                                                    
031800     EJECT                                                                
031900 Z-FINIT SECTION.                                                         
032000     CONTINUE                                                             
032100     .                                                                    
032200     EJECT                                                                
032300*    --- DISPATCHER SECTIONS                                              
032400 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
032500                                                                          
032600     MOVE 'GETARG'               TO SUB-KDFUNC                            
032700     MOVE 'CARPARTS.NDC.ACTIVATEADVICENOTE'      TO SUB-ADDISPABS         
032800     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
032900                                                                          
033000     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
033100                                                                          
033200     IF SUB-KDRC > 0                                                      
033300       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
033400       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
033500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
033600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
033700     END-IF                                                               
033800     .                                                                    
033900     SKIP3                                                                
034000 S02-RETURN-RESPONSE SECTION.                                             
034100                                                                          
034200     MOVE 'RETURN'                   TO SUB-KDFUNC                        
034300     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
034400                                                                          
034500     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
034600                                                                          
034700     IF SUB-KDRC > 0                                                      
034800       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
034900       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
035000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
035100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
035200     END-IF                                                               
035300     .                                                                    
035400     EJECT                                                                
