000100 ID DIVISION.                                                             
000200                                                                          
000301 PROGRAM-ID.     WXTR2400.                                                
000401 AUTHOR.         UMESH JAIN                                               
000501 DATE-WRITTEN.   21/03/04                                                 
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000901*        READ W01184 AND CONVERT IT INTO EDITABLE FORMAT                  
001001*        FOR WXTR FILE.                                                   
001201*        LDC         W011.LDC.WXTR24                                      
001301*        SDC         W011.SDC.WXTR24                                      
001401*        NDC         W011.NDC.WXTR24                                      
001700*                                                                         
001800*    ABENDKODER:                                                          
001900*        U0016 -  . . . .                                                 
002000*        U1000 -  . . . .                                                 
003000*                                                                         
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400     SKIP2                                                                
003500 INPUT-OUTPUT SECTION.                                                    
003600                                                                          
003700 FILE-CONTROL.                                                            
003800     SKIP2                                                                
004001     SELECT XDC-W01184                 ASSIGN TO WXTR24D1.                
004100                                                                          
004301     SELECT LDC-W01184X                ASSIGN TO WXTR24D2.                
004400                                                                          
004601     SELECT SDC-W01184X                ASSIGN TO WXTR24D3.                
004700                                                                          
004901     SELECT NDC-W01184X                ASSIGN TO WXTR24D4.                
005000                                                                          
005500 DATA DIVISION.                                                           
005600     SKIP2                                                                
005700 FILE SECTION.                                                            
005800     SKIP3                                                                
005901 FD  XDC-W01184                                                           
006000     RECORDING       F                                                    
006100     BLOCK CONTAINS  0.                                                   
006200                                                                          
006302*01  POST -COPY W01184                                                    
006400                                                                          
006501 FD  LDC-W01184X                                                          
006600     RECORDING       F                                                    
006700     BLOCK CONTAINS  0.                                                   
006800                                                                          
006901*01  POST -COPY W01184X -PRE  LDC- -L.                                    
007000                                                                          
007101 FD  SDC-W01184X                                                          
007200     RECORDING       F                                                    
007300     BLOCK CONTAINS  0.                                                   
007400                                                                          
007501*01  POST -COPY W01184X -PRE  SDC- -L.                                    
007600                                                                          
007700     EJECT                                                                
007801 FD  NDC-W01184X                                                          
007900     RECORDING       F                                                    
008000     BLOCK CONTAINS  0.                                                   
008100                                                                          
008201*01  POST -COPY W01184X -PRE  NDC- -L.                                    
008300                                                                          
009100 WORKING-STORAGE SECTION.                                                 
009200*    -- CHECKED BY WY2000                                                 
009301 77  IDPGM                       PIC X(8)    VALUE 'WXTR2400'.            
009400 77  JA                          PIC X       VALUE 'J'.                   
009500 77  NEJ                         PIC X       VALUE 'N'.                   
009600                                                                          
009701 77  W01184-EOF-SW               PIC X       VALUE 'N'.                   
009801     88  END-OF-W01184                       VALUE 'J'.                   
009901                                                                          
010000 01  ARBETSAREOR.                                                         
010100     03 IX                       PIC 9(2)    VALUE ZERO.                  
010300                                                                          
010400     EJECT                                                                
010500*      --- VALID IDDC CODES                                               
010600*                                                                         
010700*01    -COPY WWDC99                                                       
010800       EJECT                                                              
011102 01  W01184X-XDC-AREA.                                                    
011202     03  -COPY W01184X                                                    
011302     EJECT                                                                
011303 01  W01184X-LDC-AREA.                                                    
011304     03  -COPY W01184X -PRE  LDC-                                         
011305     EJECT                                                                
011306 01  W01184X-SDC-AREA.                                                    
011307     03  -COPY W01184X -PRE  SDC-                                         
011308     EJECT                                                                
011309 01  W01184X-NDC-AREA.                                                    
011310     03  -COPY W01184X -PRE  NDC-                                         
011320     EJECT                                                                
012000 01  DYNAMISKA-SUBPROGRAM.                                                
012100*                                                                         
012200     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
012300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
012600     SKIP2                                                                
012700*    --- PARAMETRAR TILL ABEND                                            
012800                                                                          
012900 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
013000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
013100     SKIP2                                                                
013200 01  FELTEXT.                                                             
013300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
013400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
013500     EJECT                                                                
013600*    --- PARAMETRAR TILL POSTSUM                                          
013700*                                                                         
013800*01  -COPY W0005   -PRE  POSTSUM-                                         
013900     EJECT                                                                
016300*    --- STATUS-KOD FRÅN IMS                                              
020500                                                                          
021801 PROCEDURE DIVISION.                                                      
021901 MAIN SECTION.                                                            
022001                                                                          
022101     PERFORM A-INIT                                                       
022202     PERFORM S01-READ-W01184                                              
022301     PERFORM UNTIL END-OF-W01184                                          
022401       PERFORM B-MOVE-W01184                                              
022402       PERFORM S01-READ-W01184                                            
022801     END-PERFORM                                                          
022901                                                                          
023001     PERFORM Z-FINIT                                                      
023101                                                                          
023201     MOVE ZERO TO RETURN-CODE                                             
023301     GOBACK                                                               
024000                                                                          
026700     .                                                                    
026800     EJECT                                                                
026900                                                                          
027000 A-INIT SECTION.                                                          
027101     OPEN INPUT  XDC-W01184                                               
027201     OPEN OUTPUT LDC-W01184X                                              
027301                 SDC-W01184X                                              
027401                 NDC-W01184X                                              
027600                                                                          
027902     INITIALIZE SLAG-W01184X                                              
028000                                                                          
029700     .                                                                    
029800     EJECT                                                                
029900                                                                          
030000 B-MOVE-W01184     SECTION.                                               
030600                                                                          
030902     MOVE CORR SLAG-W01184    TO SLAG-W01184X                             
031000                                                                          
031100     MOVE 1 TO IX                                                         
031200     PERFORM UNTIL IX > 12                                                
031300        MOVE SLAG-RESEASON IN SLAG-W01184(IX) TO                          
031402             SLAG-RESEASON IN SLAG-W01184X(IX)                            
031500        ADD 1 TO IX                                                       
031600     END-PERFORM                                                          
031700                                                                          
031802     MOVE 1 TO IX                                                         
031902     PERFORM UNTIL IX > 12                                                
032002        MOVE SLAG-RESEASON-PLAN     IN SLAG-W01184(IX) TO                 
032102             SLAG-RESEASON-PLAN     IN SLAG-W01184X(IX)                   
032202        ADD 1 TO IX                                                       
032302     END-PERFORM                                                          
032402*                                                                         
032802     MOVE 1 TO IX                                                         
032902     PERFORM UNTIL IX > 5                                                 
033002        MOVE SLAG-TILEVDAG     IN SLAG-W01184(IX) TO                      
033102             SLAG-TILEVDAG     IN SLAG-W01184X(IX)                        
033202        ADD 1 TO IX                                                       
033302     END-PERFORM                                                          
033402*                                                                         
033500     MOVE SLAG-IDDC IN SLAG-W01184     TO WS-IDDC                         
033602*                                                                         
033702     IF LDC                                                               
033802       MOVE SLAG-W01184X     TO LDC-SLAG-W01184X                          
033902       PERFORM S10-SKRIV-LDC-W01184X                                      
034002     ELSE                                                                 
034102       IF SDC                                                             
034202         MOVE SLAG-W01184X   TO SDC-SLAG-W01184X                          
034302         PERFORM S11-SKRIV-SDC-W01184X                                    
034402       ELSE                                                               
034502         IF NDC                                                           
034602           MOVE SLAG-W01184X TO NDC-SLAG-W01184X                          
034702           PERFORM S12-SKRIV-NDC-W01184X                                  
034802         END-IF                                                           
034902       END-IF                                                             
035002     END-IF                                                               
035102     .                                                                    
036000     EJECT                                                                
043700                                                                          
043800 Z-FINIT SECTION.                                                         
043902     CLOSE XDC-W01184                                                     
043903           LDC-W01184X                                                    
044002           SDC-W01184X                                                    
044102           NDC-W01184X                                                    
044500     MOVE 'S' TO POSTSUM-OPKOD                                            
044600     CALL POSTSUM USING POSTSUM-PARM                                      
044700     .                                                                    
044800     EJECT                                                                
044900                                                                          
045000 S01-READ-W01184 SECTION.                                                 
045100     SKIP2                                                                
045200     READ XDC-W01184 RECORD INTO W01184X-XDC-AREA                         
045300     AT END                                                               
045400        MOVE JA              TO W01184-EOF-SW                             
045500     END-READ                                                             
045600     .                                                                    
045700     EJECT                                                                
045800                                                                          
063702 S10-SKRIV-LDC-W01184X SECTION.                                           
063802     WRITE LDC-POST FROM W01184X-LDC-AREA                                 
063900                                                                          
064001     MOVE 'WXTR24' TO POSTSUM-FDNAMN                                      
064101     MOVE 'WXTR24D1' TO POSTSUM-DDNAMN2                                   
064200     CALL POSTSUM USING POSTSUM-PARM                                      
064300     .                                                                    
064400     EJECT                                                                
064503 S11-SKRIV-SDC-W01184X SECTION.                                           
064602     WRITE SDC-POST FROM W01184X-SDC-AREA                                 
064702                                                                          
064802     MOVE 'WXTR24' TO POSTSUM-FDNAMN                                      
064903     MOVE 'WXTR24D2' TO POSTSUM-DDNAMN2                                   
065002     CALL POSTSUM USING POSTSUM-PARM                                      
065102     .                                                                    
065202     EJECT                                                                
065303 S12-SKRIV-NDC-W01184X SECTION.                                           
065403     WRITE NDC-POST FROM W01184X-NDC-AREA                                 
065503                                                                          
065603     MOVE 'WXTR24' TO POSTSUM-FDNAMN                                      
065703     MOVE 'WXTR24D3' TO POSTSUM-DDNAMN2                                   
065803     CALL POSTSUM USING POSTSUM-PARM                                      
065903     .                                                                    
066003     EJECT                                                                
066103                                                                          
067003                                                                          
