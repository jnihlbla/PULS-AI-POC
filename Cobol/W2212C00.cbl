000100 ID DIVISION.                                                             
000205 PROGRAM-ID.     W2212C00.                                                
000305 DATE-WRITTEN.   MARS 2014.                                               
000400 AUTHOR          GÖRAN KJELLSON                                           
000500     REMARKS.                                                             
000600*    FUNKTION.   RENSAR WDG3 (HTYP 2257) FRÅN ALLA WDGX2260               
000700*                DÄR DAAVROP-TOM < DAGENS-AAAAVV                          
000800*                                                                         
001100 ENVIRONMENT DIVISION.                                                    
001200 INPUT-OUTPUT SECTION.                                                    
001300 FILE-CONTROL.                                                            
001400                                                                          
001502 DATA DIVISION.                                                           
001702 FILE SECTION.                                                            
001800                                                                          
003200 WORKING-STORAGE SECTION.                                                 
003400                                                                          
003500*    -- CHECKED BY WY2000                                                 
003605 77  PROGRAM-NAMN            PIC X(8)  VALUE 'W2212C00'.                  
003700 77  CURRENT-SECTION         PIC X(16) VALUE SPACE.                       
003800 77  CURRENT-IMS-SECTION     PIC X(16) VALUE SPACE.                       
004000 77  MSG-IO-AREA-LENGTH      PIC S9(9) VALUE +32 COMP SYNC.               
004100 77  MSG-IO-AREA             PIC X(32) VALUE SPACE.                       
004200 77  CHKP-AREA-1-LENGTH      PIC S9(9) VALUE +32 COMP SYNC.               
004300 77  CHKP-AREA-1             PIC X(32) VALUE SPACE.                       
004800 77  WS-CHKP-RAKNARE         PIC S9(3) COMP-3 VALUE ZERO.                 
004900 77  WS-CHKP-MAX             PIC S9(3) COMP-3 VALUE +1.                   
005000*77  WS-CHKP-MAX             PIC S9(3) COMP-3 VALUE +100.                 
009500                                                                          
009603 01  DAGENS-DATUM            PIC 9(6)  VALUE 200000.                      
009703 01  FILLER REDEFINES DAGENS-DATUM.                                       
009803     03 FILLER               PIC 9(2).                                    
009903     03 DAGENS-AAR           PIC 9(2).                                    
010003     03 DAGENS-VECKA         PIC 9(2).                                    
010103                                                                          
010902                                                                          
011002 01  DYNAMISK-SUBMODUL.                                                   
011103     03  WDATKONV            PIC X(8) VALUE  'WDATKONV'.                  
011203     03  FELLOG              PIC X(8) VALUE  'FELLOG  '.                  
011403     03  CBLTDLI             PIC X(8) VALUE  'CBLTDLI '.                  
011603                                                                          
011704*    ---  LÄNKAREA TILL WDATKONV                                          
011804 01  FILLER                      PIC X(16)   VALUE 'WDATAREA'.            
011904                                                                          
012004*01 -COPY WDATAREA                                                        
012104                                                                          
012204                                                                          
013003******************************************************************        
013103*    I M S  W S                                                  *        
013203******************************************************************        
013303                                                                          
013403 01   FILLER                 PIC X(16)   VALUE ALL 'I M S  W S'.          
013503                                                                          
014000 01  W-WDG3KEY-X.                                                         
014100     03  W-IDHTYP            PIC X(4)     VALUE '2257'.                   
014300     03  W-LOW-VALUE         PIC X(26)    VALUE LOW-VALUE.                
014400                                                                          
014410 01  W-IDLEVNR-X.                                                         
014420     03  W-IDLEVNR           PIC X(5)     VALUE SPACE.                    
014440                                                                          
014500 01  W-DAAVROP-TOM-X.                                                     
014600     03  W-DAAVROP-TOM       PIC 9(6)     VALUE ZERO.                     
014700                                                                          
014800 01  GODK-STATUSKODER.                                                    
014900     03  GODK-STATUS  OCCURS 5  INDEXED BY STATUS-IX                      
015000                             PIC X(2).                                    
015100                                                                          
015200 01  STATUS-WS               PIC X(2).                                    
015400     88  SEGMENT-SAKNAS                  VALUE 'GE'.                      
015600     88  IMS-EJ-OK                       VALUE 'XD'.                      
015700                                                                          
015800 01  ALL-SSA.                                                             
015900     03 SSA1                             PIC X(64).                       
016000     03 SSA2                             PIC X(64).                       
016100                                                                          
016200                                                                          
016300*    -COPY W0003.                                                         
016407                                                                          
016500******************************************************************        
016600*    DLI-IO-AREAOR                                               *        
016700******************************************************************        
016800                                                                          
016900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDG301  '.                    
017000 01  DLI-IO-WDG301.                                                       
017100*    03  -COPY WDG301                                                     
017200                                                                          
017300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2258'.                    
017400 01  DLI-IO-WDGX2258.                                                     
017500*    03  -COPY WDGX2258                                                   
017600                                                                          
017700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2260'.                    
017800 01  DLI-IO-WDGX2260.                                                     
017900*    03  -COPY WDGX2260                                                   
018000                                                                          
018400                                                                          
020100 LINKAGE SECTION.                                                         
020200                                                                          
020300*01  -COPY W0008      -PRE MSG-.                                          
020400         05    FILLER        PIC X.                                       
020500                                                                          
020600*01  -COPY W0008      -PRE WDG3-.                                         
020700         05    FILLER        PIC X(30).                                   
020800         05    KFB-IDLEVNR   PIC X(5).                                    
022000                                                                          
022100                                                                          
022200 PROCEDURE DIVISION  USING MSG-PCB WDG3-PCB.                              
022400     ENTRY 'DLITCBL' USING MSG-PCB WDG3-PCB.                              
022600                                                                          
022700     PERFORM A-INIT                                                       
022800                                                                          
022900     PERFORM IMS-GU-WDG301                                                
023000                                                                          
023103     MOVE DAGENS-DATUM         TO W-DAAVROP-TOM                           
023200     PERFORM IMS-GHNP-WDGX2260                                            
023300     PERFORM UNTIL SEGMENT-SAKNAS                                         
023400        PERFORM IMS-DLET-WDGX2260                                         
023410        MOVE KFB-IDLEVNR TO W-IDLEVNR                                     
023500        PERFORM IMS-GNP-WDGX2260-LEV                                      
023501        IF SEGMENT-SAKNAS                                                 
023503           PERFORM IMS-GHNP-WDGX2258                                      
023505           PERFORM IMS-DLET-WDGX2258                                      
023506        END-IF                                                            
023507                                                                          
023508******************************************                                
023509* VI KÖR UTAN CHECKPOINT TILLSVIDARE     *                                
023510* NÄR WS-CHKP-RAKNARE BLIR STÖRRE ÄN 500 *                                
023511* ÄR DET DAGS ATT TÄNKA OM               *                                
023512******************************************                                
023520*       IF WS-CHKP-RAKNARE >= WS-CHKP-MAX                                 
023603*          MOVE +0            TO WS-CHKP-RAKNARE                          
023703*          PERFORM IMS-CHECKPOINT                                         
023803*       END-IF                                                            
023804******************************************                                
023805                                                                          
023900        PERFORM IMS-GHNP-WDGX2260                                         
025200     END-PERFORM                                                          
025300     DISPLAY '***** WS-CHKP-RAKNARE ' WS-CHKP-RAKNARE                     
025500                                                                          
025600     MOVE ZERO                 TO RETURN-CODE                             
025700     GOBACK                                                               
025800     .                                                                    
025900                                                                          
026000                                                                          
026100 A-INIT      SECTION.                                                     
026200     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
026300                                                                          
027000*    PERFORM IMS-RESTART                                                  
028003                                                                          
028103     MOVE 'IDAG  '    TO DAT-KDDATFORM                                    
028203     CALL WDATKONV USING DAT-KDDATFORM                                    
028303                         DAT-I-TIDATUM                                    
028403                         DAT-O-TIDATUM                                    
028503                         DAT-KDSVAR                                       
028603                                                                          
028703     MOVE DAT-TIAA    TO DAGENS-AAR                                       
028803     MOVE DAT-TIVV    TO DAGENS-VECKA                                     
028900     .                                                                    
029000                                                                          
051500                                                                          
054300* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
054400*                                                           *             
054500*   I M S - S E K T I O N E R                               *             
054600*                                                           *             
054700* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
054800                                                                          
054900 IMS-GU-WDG301 SECTION.                                                   
055000     MOVE 'IMS-GU-WDG301   ' TO CURRENT-IMS-SECTION                       
055100                                                                          
055200     MOVE SPACE                  TO ALL-SSA                               
055300     STRING 'WDG301  (WDG3KEY  =' W-WDG3KEY-X ')'                         
055400             DELIMITED BY SIZE INTO SSA1                                  
055500     MOVE '  '                   TO GODK-STATUSKODER                      
055606     CALL CBLTDLI USING GU WDG3-PCB DLI-IO-WDG301 SSA1                    
055700     MOVE WDG3-STATUS-CODE       TO STATUS-WS                             
055800     PERFORM IMS-STATUSKONTROLL                                           
055900     .                                                                    
056000                                                                          
056100                                                                          
056202 IMS-GHNP-WDGX2258 SECTION.                                               
056302     MOVE 'GHNP-WDGX2260   ' TO CURRENT-IMS-SECTION                       
056400                                                                          
056500     MOVE SPACE                  TO ALL-SSA                               
056607     STRING 'WDGX2258*F(IDLEVNRS =' W-IDLEVNR-X ')'                       
056700             DELIMITED BY SIZE INTO SSA1                                  
056806     MOVE '    '                 TO GODK-STATUSKODER                      
056900     CALL CBLTDLI USING GHNP WDG3-PCB DLI-IO-WDGX2258 SSA1                
057000     MOVE WDG3-STATUS-CODE       TO STATUS-WS                             
057100     PERFORM IMS-STATUSKONTROLL                                           
057200     .                                                                    
057300                                                                          
057400                                                                          
057401 IMS-DLET-WDGX2258 SECTION.                                               
057402     MOVE 'DLET-WDGX2258   ' TO CURRENT-IMS-SECTION                       
057403                                                                          
057404     MOVE SPACE                  TO ALL-SSA                               
057405     MOVE '  '               TO GODK-STATUSKODER                          
057406     CALL CBLTDLI USING DLET WDG3-PCB DLI-IO-WDGX2258                     
057407     MOVE WDG3-STATUS-CODE   TO STATUS-WS                                 
057408     PERFORM IMS-STATUSKONTROLL                                           
057409                                                                          
057410     ADD +1                   TO WS-CHKP-RAKNARE                          
057411     .                                                                    
057412                                                                          
057413 IMS-GHNP-WDGX2260 SECTION.                                               
057420     MOVE 'GHNP-WDGX2260   ' TO CURRENT-IMS-SECTION                       
057430                                                                          
057431     MOVE SPACE                  TO ALL-SSA                               
057440     STRING 'WDGX2260(DAAVROPT <' W-DAAVROP-TOM ')'                       
057450             DELIMITED BY SIZE INTO SSA1                                  
057460     MOVE '  GE'                 TO GODK-STATUSKODER                      
057470     CALL CBLTDLI USING GHNP WDG3-PCB DLI-IO-WDGX2260 SSA1                
057480     MOVE WDG3-STATUS-CODE       TO STATUS-WS                             
057490     PERFORM IMS-STATUSKONTROLL                                           
057491     .                                                                    
057492                                                                          
057493                                                                          
057500 IMS-GNP-WDGX2260-LEV SECTION.                                            
057600     MOVE 'GNP-WDGX2260-LEV' TO CURRENT-IMS-SECTION                       
057700                                                                          
057701     MOVE SPACE                  TO ALL-SSA                               
057710     STRING 'WDGX2258(IDLEVNRS =' W-IDLEVNR-X ')'                         
057720             DELIMITED BY SIZE INTO SSA1                                  
057800     MOVE 'WDGX2260*F'           TO SSA2                                  
058000     MOVE '  GE'                 TO GODK-STATUSKODER                      
058100     CALL CBLTDLI USING GHNP WDG3-PCB DLI-IO-WDGX2260 SSA1 SSA2           
058200     MOVE WDG3-STATUS-CODE       TO STATUS-WS                             
058300     PERFORM IMS-STATUSKONTROLL                                           
058400     .                                                                    
058500                                                                          
058510                                                                          
058600 IMS-DLET-WDGX2260 SECTION.                                               
058700     MOVE 'DLET-WDGX2260   ' TO CURRENT-IMS-SECTION                       
058800                                                                          
058900     MOVE SPACE                  TO ALL-SSA                               
059000     MOVE '  '               TO GODK-STATUSKODER                          
059100     CALL CBLTDLI USING DLET WDG3-PCB DLI-IO-WDGX2260                     
059200     MOVE WDG3-STATUS-CODE   TO STATUS-WS                                 
059300     PERFORM IMS-STATUSKONTROLL                                           
059400                                                                          
059500     ADD +1                   TO WS-CHKP-RAKNARE                          
059600     .                                                                    
059700                                                                          
066400*IMS-RESTART  SECTION.                                                    
066500*                                                                         
066600*    MOVE SPACE TO MSG-IO-AREA                                            
066700*    MOVE '  '  TO GODK-STATUSKODER                                       
066800*    CALL CBLTDLI USING XRST MSG-PCB                                      
066900*                         MSG-IO-AREA-LENGTH MSG-IO-AREA                  
067000*                         CHKP-AREA-1-LENGTH CHKP-AREA-1                  
067100*    MOVE MSG-STATUS-CODE TO STATUS-WS                                    
067200*    PERFORM IMS-STATUSKONTROLL                                           
067300*                                                                         
067400*    IF IMS-EJ-OK                                                         
067500*      DISPLAY 'IMS-KONTROLLREGION EJ TILLGÄNGLIG'                        
067600*      CALL FELLOG                                                        
067700*    END-IF                                                               
067800*    .                                                                    
067900                                                                          
068000                                                                          
071100*IMS-CHECKPOINT SECTION.                                                  
071200*                                                                         
071300*    MOVE PROGRAM-NAMN TO MSG-IO-AREA                                     
071400*    MOVE '  XD'       TO GODK-STATUSKODER                                
071500*    CALL CBLTDLI USING CHKP MSG-PCB                                      
071600*                         MSG-IO-AREA-LENGTH MSG-IO-AREA                  
071700*                         CHKP-AREA-1-LENGTH CHKP-AREA-1                  
071800*    MOVE MSG-STATUS-CODE TO STATUS-WS                                    
071900*    PERFORM IMS-STATUSKONTROLL                                           
072000*    IF IMS-EJ-OK                                                         
072100*      DISPLAY 'IMS-KONTROLLREGION EJ TILLGÄNGLIG'                        
072200*      CALL FELLOG                                                        
072300*    END-IF                                                               
072400*    .                                                                    
072507                                                                          
072600                                                                          
072700 IMS-STATUSKONTROLL SECTION.                                              
072800                                                                          
072907                                                                          
073000     SET STATUS-IX TO 1                                                   
073100     SEARCH GODK-STATUS AT END CALL FELLOG                                
073200     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
073300     CONTINUE                                                             
074000     .                                                                    
