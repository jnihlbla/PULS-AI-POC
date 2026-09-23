000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W0118600.                                                
000400 AUTHOR.         STEFAN KIHLBERG.                                         
000500 DATE-WRITTEN.   94/10/26.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄSER WDL7 MED SB                                                
001000*        SKAPAR FILER MED SAMTLIGA DATAELEMENT FRÅN WDL711                
001100*                                                   WDL411                
001200*        FÖR FIL FÖR VART DC                                              
001300*        LDC         W011.LDC.W01186                                      
001400*        SDC         W011.SDC.W01186                                      
001500*        NDC         W011.NDC.W01186                                      
001600*                                                                         
001700*        PROGRAMMET LASER WDL7 + WDL4                            0        
001800*                                                                         
001900*    ABENDKODER:                                                          
002000*        U0016 -  . . . .                                                 
003000*        U1000 -  . . . .                                                 
004000*                                                                         
005000                                                                          
006000     SKIP3                                                                
007000 ENVIRONMENT DIVISION.                                                    
007100     SKIP2                                                                
007200 INPUT-OUTPUT SECTION.                                                    
007300                                                                          
007400 FILE-CONTROL.                                                            
007500     SKIP2                                                                
007600*  --- SAMTLIGA DATAELEMENT FRÅN WDL711+L411, IDDC = LDC                  
007700     SELECT LDC-W01186                ASSIGN TO W01186D1.                 
007800                                                                          
008000*  --- SAMTLIGA DATAELEMENT FRÅN WDL711+L411, IDDC = SDC                  
008100     SELECT SDC-W01186                ASSIGN TO W01186D2.                 
008200                                                                          
008400*  --- SAMTLIGA DATAELEMENT FRÅN WDL711+L411, IDDC = NDC                  
008500     SELECT NDC-W01186                ASSIGN TO W01186D3.                 
008600                                                                          
008700     EJECT                                                                
008800 DATA DIVISION.                                                           
008900     SKIP2                                                                
009000 FILE SECTION.                                                            
009100     SKIP3                                                                
009200 FD  LDC-W01186                                                           
010000     RECORDING       F                                                    
020000     BLOCK CONTAINS  0.                                                   
021000                                                                          
021100*01  POST -COPY W01186 -PRE  LDC-W01186-  -L.                             
021200                                                                          
021300 FD  SDC-W01186                                                           
021400     RECORDING       F                                                    
021500     BLOCK CONTAINS  0.                                                   
021600                                                                          
021700*01  POST -COPY W01186 -PRE  SDC-W01186-  -L.                             
021800                                                                          
021900 FD  NDC-W01186                                                           
022000     RECORDING       F                                                    
022100     BLOCK CONTAINS  0.                                                   
022200                                                                          
022300*01  POST -COPY W01186 -PRE  NDC-W01186-  -L.                             
022400                                                                          
022500 WORKING-STORAGE SECTION.                                                 
022600*    -- CHECKED BY WY2000                                                 
022700 77  IDPGM                       PIC X(8)    VALUE 'W0118600'.            
022800 77  JA                          PIC X       VALUE 'J'.                   
022900 77  NEJ                         PIC X       VALUE 'N'.                   
023000                                                                          
024000 01  ARBETSAREOR.                                                         
025000     03 WS-SPAR-IDARTNR          PIC S9(9)   VALUE ZERO COMP-3.           
026000     03 IX                       PIC S9(9)   VALUE ZERO COMP-3.           
027000     03 IX2                      PIC S9(9)   VALUE ZERO COMP-3.           
028000                                                                          
029000     03 FELRAK                   PIC S9(9)   VALUE ZERO COMP-3.           
030000                                                                          
040000     EJECT                                                                
041000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
042000 01  FILLER REDEFINES DAGENS-DATUM.                                       
043000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
044000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
045000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
046000     EJECT                                                                
046100*      --- VALID IDDC CODES                                               
046200*                                                                         
046300*01    -COPY WWDC99                                                       
046400       EJECT                                                              
046500 01  DYNAMISKA-SUBPROGRAM.                                                
046600*                                                                         
046700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
046800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
046900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
047000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
047100     SKIP2                                                                
047200*    --- PARAMETRAR TILL ABEND                                            
047300                                                                          
047400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
047500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
047600     SKIP2                                                                
047700 01  FELTEXT.                                                             
047800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
047900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
048000     EJECT                                                                
048100*    --- PARAMETRAR TILL POSTSUM                                          
048200*                                                                         
048300*01  -COPY W0005   -PRE  POSTSUM-                                         
048400     EJECT                                                                
048500 01  W01186-AREA-START           PIC X(24)   VALUE                        
048600                                 'W01186-AREA-START  '.                   
048700     SKIP2                                                                
048800                                                                          
048900 01  W01186-AREA.                                                         
049000     03  DC-UPPGIFTER.                                                    
049100         05  -COPY W01186                                                 
049200     EJECT                                                                
049300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
049400*                                                                         
049500     EJECT                                                                
049600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
049700     SKIP3                                                                
049800 01  NYCKLAR-TILL-DLI.                                                    
049900     03  W-IDARTNR-X.                                                     
050000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
050100     03  W-IDDC-X.                                                        
050200         05  W-IDDC              PIC  X(2)   VALUE SPACE.                 
050300     SKIP2                                                                
050400*    --- STATUS-KOD FRÅN IMS                                              
050500 01  STATUS-WS                   PIC XX.                                  
050600     88  SEGMENT-FINNS                       VALUE '  '.                  
050700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
050800     88  SEGMENT-SLUT                        VALUE 'GB'.                  
050900     SKIP2                                                                
051000 01  GODK-STATUSKODER.                                                    
051100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
051200     SKIP3                                                                
051300 01  SSA1                        PIC X(64).                               
051400 01  SSA2                        PIC X(64).                               
051500     EJECT                                                                
051600*    --- IMS FUNKTIONSKODER                                               
051700*01  -COPY W0003                                                          
051800     EJECT                                                                
051900*    ---  DLI INPUT-OUTPUT AREA                                           
052000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
052100     SKIP3                                                                
052200 01  DLI-IO-AREA.                                                         
052300     03  IO-AREA                 PIC X(2300) VALUE SPACE.                 
052400     SKIP3                                                                
052500     03  WDL701   REDEFINES IO-AREA.                                      
052600*        05  -COPY WDL701                                                 
052700     SKIP3                                                                
052800     03  WDL711   REDEFINES IO-AREA.                                      
052900*        05  -COPY WDL711                                                 
053000     EJECT                                                                
053100 01  DLI-IO-L411.                                                         
053400*    03  -COPY WDL411                                                     
053500     EJECT                                                                
053600 LINKAGE SECTION.                                                         
053700                                                                          
053800     EJECT                                                                
053900*01  -COPY W0008  -PRE WDL7-                                              
054000     05  FILLER                  PIC X.                                   
054100     EJECT                                                                
054200*01  -COPY W0008  -PRE WDL4-                                              
054300     05  FILLER                  PIC X.                                   
054400     EJECT                                                                
054500 PROCEDURE DIVISION  USING WDL7-PCB WDL4-PCB.                             
054600     ENTRY 'DLITCBL' USING WDL7-PCB WDL4-PCB.                             
054700                                                                          
054800     PERFORM A-INIT                                                       
054900     PERFORM IMS-GET-WDL7                                                 
055000     PERFORM UNTIL SEGMENT-SLUT                                           
055100        EVALUATE WDL7-SEG-NAME-FB                                         
055200           WHEN 'WDL701  '                                                
055300              MOVE ART-IDARTNR TO WS-SPAR-IDARTNR                         
055400                                  W-IDARTNR                               
055500           WHEN 'WDL711  '                                                
055602              MOVE DC-IDDC IN DC-WDL711 TO W-IDDC                         
055700              PERFORM IMS-GU-WDL411                                       
055800              IF SEGMENT-SAKNAS                                           
055900******** IF PART/DC FROM WDL7 IS MISSING IN WDL4 THEN L411-AREA           
056000******** MUST BE ZEROED BCZ SOME ITEMS ARE USED IN COMPUTES BELOW         
056100                INITIALIZE OIHD-WDL411                                    
056200              END-IF                                                      
056300              PERFORM B-BEHANDLA-FLYTTA-SKRIV-SDC                         
056400         END-EVALUATE                                                     
056500         PERFORM IMS-GET-WDL7                                             
056600     END-PERFORM                                                          
056700     PERFORM Z-FINIT                                                      
056800                                                                          
056900     DISPLAY 'ANTAL FELPOSTER = ' FELRAK                                  
057000                                                                          
057100     MOVE ZERO TO RETURN-CODE                                             
057200     GOBACK                                                               
057300     .                                                                    
057400     EJECT                                                                
057500                                                                          
057600 A-INIT SECTION.                                                          
057700     OPEN OUTPUT LDC-W01186                                               
057800                 SDC-W01186                                               
057900                 NDC-W01186                                               
058000                                                                          
058100     ACCEPT DAGENS-DATUM  FROM DATE                                       
058200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
058300     .                                                                    
058400     EJECT                                                                
058500                                                                          
058600 B-BEHANDLA-FLYTTA-SKRIV-SDC SECTION.                                     
058700     INITIALIZE DC-UPPGIFTER                                              
058800     MOVE WS-SPAR-IDARTNR     TO DC-IDARTNR                               
059000                                                                          
060000     MOVE CORR DC-WDL711      TO DC-W01186                                
061000     PERFORM BA-FLYTTA-RESTEN                                             
061100     PERFORM BB-SKRIV                                                     
061200     .                                                                    
061300     EJECT                                                                
061400                                                                          
061500 BA-FLYTTA-RESTEN SECTION.                                                
061600     MOVE +1                  TO IX                                       
061700     PERFORM UNTIL IX > +53                                               
061800        MOVE DC-KVOI-RULL IN DC-WDL711(IX) TO                             
061900                      DC-KVOI-RULL IN DC-W01186(IX)                       
062000        MOVE DC-KVOI-CDC-RULL IN DC-WDL711(IX) TO                         
063000                         DC-KVOI-CDC-RULL IN DC-W01186(IX)                
063100        MOVE DC-KVOI-REF-RULL IN DC-WDL711(IX) TO                         
063200                         DC-KVOI-REF-RULL IN DC-W01186(IX)                
063300        MOVE DC-KVOT-RULL IN DC-WDL711(IX) TO                             
063400                         DC-KVOT-RULL IN DC-W01186(IX)                    
063500        MOVE DC-KVOT-CDC-RULL IN DC-WDL711(IX) TO                         
063600                         DC-KVOT-CDC-RULL IN DC-W01186(IX)                
063700        MOVE DC-KVOT-REF-RULL IN DC-WDL711(IX) TO                         
063800                         DC-KVOT-REF-RULL IN DC-W01186(IX)                
063900        ADD +1 TO IX                                                      
064000     END-PERFORM                                                          
064100                                                                          
064200     MOVE +1                  TO IX                                       
064300     PERFORM UNTIL IX > +5                                                
064400        MOVE DC-TIVV IN DC-WDL711(IX) TO DC-TIVV IN DC-W01186(IX)         
064500        MOVE DC-KVOI-INNEV IN DC-WDL711(IX)     TO                        
064600                         DC-KVOI-INNEV IN DC-W01186(IX)                   
064700        MOVE DC-KVOI-CDC-INNEV IN DC-WDL711(IX) TO                        
064800                         DC-KVOI-CDC-INNEV IN DC-W01186(IX)               
064900        MOVE DC-KVOI-REF-INNEV IN DC-WDL711(IX) TO                        
065000                         DC-KVOI-REF-INNEV IN DC-W01186(IX)               
065100        MOVE DC-KVOT-INNEV IN DC-WDL711(IX) TO                            
065200                         DC-KVOT-INNEV IN DC-W01186(IX)                   
065300        MOVE DC-KVOT-CDC-INNEV IN DC-WDL711(IX) TO                        
065400                         DC-KVOT-CDC-INNEV IN DC-W01186(IX)               
065500        MOVE DC-KVOT-REF-INNEV IN DC-WDL711(IX) TO                        
065600                         DC-KVOT-REF-INNEV IN DC-W01186(IX)               
065610        MOVE DC-KVOI-PP-INNEV IN DC-WDL711(IX)  TO                        
065620                         DC-KVOI-PP-INNEV IN DC-W01186(IX)                
065630        MOVE DC-KVOT-PP-INNEV IN DC-WDL711(IX)  TO                        
065640                         DC-KVOT-PP-INNEV IN DC-W01186(IX)                
065700        ADD +1 TO IX                                                      
065800     END-PERFORM                                                          
065900                                                                          
066000     MOVE +1                  TO IX                                       
066100     PERFORM UNTIL IX > +2                                                
066200        MOVE OIHD-KVOT-FOREG(IX)     TO DC-KVOT-FOREG (IX)                
066400        MOVE OIHD-KVOT-CDC-FOREG(IX) TO DC-KVOT-CDC-FOREG(IX)             
066500        MOVE OIHD-KVOT-REF-FOREG(IX) TO DC-KVOT-REF-FOREG(IX)             
066600        ADD +1 TO IX                                                      
066700     END-PERFORM                                                          
066800                                                                          
066900     MOVE +1 TO IX                                                        
067000                IX2                                                       
067100     PERFORM UNTIL IX > 5                                                 
067200        PERFORM UNTIL IX2 > 12                                            
067300           MOVE OIHD-KVOI(IX, IX2)    TO DC-KVOI ( IX, IX2)               
067500           MOVE OIHD-KVVIPER(IX, IX2) TO DC-KVVIPER(IX, IX2)              
067700           ADD +1 TO IX2                                                  
067800        END-PERFORM                                                       
067900     MOVE +1 TO IX2                                                       
068000     ADD +1 TO IX                                                         
068100     END-PERFORM                                                          
068200     .                                                                    
068300     EJECT                                                                
068400                                                                          
068500 BB-SKRIV SECTION.                                                        
068600     MOVE DC-IDDC IN DC-WDL711 TO WS-IDDC                                 
068700     EVALUATE TRUE                                                        
068800        WHEN LDC                                                          
068900           PERFORM S01-SKRIV-LDC-W01186                                   
069000        WHEN SDC                                                          
069100           PERFORM S02-SKRIV-SDC-W01186                                   
069200        WHEN NDC                                                          
069300           PERFORM S03-SKRIV-NDC-W01186                                   
069400        WHEN OTHER                                                        
069500           CONTINUE                                                       
069600     END-EVALUATE                                                         
069700     .                                                                    
069800     EJECT                                                                
069900                                                                          
070000 Z-FINIT SECTION.                                                         
071000     CLOSE LDC-W01186                                                     
072000           SDC-W01186                                                     
073000           NDC-W01186                                                     
074000     SKIP2                                                                
075000     MOVE 'S' TO POSTSUM-OPKOD                                            
076000     CALL POSTSUM USING POSTSUM-PARM                                      
077000     .                                                                    
078000     EJECT                                                                
079000                                                                          
080000                                                                          
081000 S01-SKRIV-LDC-W01186 SECTION.                                            
082000     WRITE LDC-W01186-POST FROM W01186-AREA                               
082100                                                                          
082200     MOVE 'W01186' TO POSTSUM-FDNAMN                                      
082300     MOVE 'W01186D1' TO POSTSUM-DDNAMN2                                   
082400     CALL POSTSUM USING POSTSUM-PARM                                      
082500     .                                                                    
082600     EJECT                                                                
082700                                                                          
082800 S02-SKRIV-SDC-W01186 SECTION.                                            
082900     WRITE SDC-W01186-POST FROM W01186-AREA                               
083000                                                                          
083100     MOVE 'W01186' TO POSTSUM-FDNAMN                                      
083200     MOVE 'W01186D2' TO POSTSUM-DDNAMN2                                   
083300     CALL POSTSUM USING POSTSUM-PARM                                      
083400     .                                                                    
083500     EJECT                                                                
083600                                                                          
083700 S03-SKRIV-NDC-W01186 SECTION.                                            
083800     WRITE NDC-W01186-POST FROM W01186-AREA                               
083900                                                                          
084000     MOVE 'W01186' TO POSTSUM-FDNAMN                                      
085000     MOVE 'W01186D3' TO POSTSUM-DDNAMN2                                   
086000     CALL POSTSUM USING POSTSUM-PARM                                      
086100     .                                                                    
086200     EJECT                                                                
086300                                                                          
086400* --- IMS SEKTIONER ---                                                   
086500     SKIP3                                                                
086600                                                                          
086700 IMS-GET-WDL7   SECTION.                                                  
086800     CALL CBLTDLI USING GN WDL7-PCB DLI-IO-AREA                           
086900     MOVE WDL7-STATUS-CODE TO STATUS-WS                                   
087000     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
088000     PERFORM IMS-STATUSKONTROLL                                           
089000     .                                                                    
090000     SKIP3                                                                
090100 IMS-GU-WDL411 SECTION.                                                   
090500     STRING 'WDL401  (IDARTNR  =' W-IDARTNR-X ')'                         
090600          DELIMITED BY SIZE INTO SSA1                                     
090700     STRING 'WDL411  (IDDC     =' W-IDDC-X ')'                            
090800          DELIMITED BY SIZE INTO SSA2                                     
090901     MOVE '  GE'              TO GODK-STATUSKODER                         
091000     CALL CBLTDLI USING GU WDL4-PCB DLI-IO-L411 SSA1 SSA2                 
091100     MOVE WDL4-STATUS-CODE    TO STATUS-WS                                
091201     PERFORM IMS-STATUSKONTROLL                                           
091300     .                                                                    
100000                                                                          
110000 IMS-STATUSKONTROLL SECTION.                                              
120000     SET STATUS-IX TO 1                                                   
121000     SEARCH GODK-STATUS                                                   
122000       AT END                                                             
123000         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
124000         DISPLAY FELTEXT                                                  
125000         CALL FELLOG                                                      
126000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
127000         CONTINUE                                                         
127100     END-SEARCH                                                           
127200     .                                                                    
