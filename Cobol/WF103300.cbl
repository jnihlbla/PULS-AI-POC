001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     WF103300.                                                
001400 AUTHOR.         BHAT ARCHANA.                                            
001500 DATE-WRITTEN.   20/09/14.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001800                                                                          
001900*    FUNCTION:                                                            
002000*        THIS PROGRAM SENDS  YEARLY RATES TO PULS THRU W93003T            
002100*        TRANSACTION.                                                     
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003201     SKIP2                                                                
003202*          --- YEARLY RATES                                               
003210     SELECT WF1032                     ASSIGN TO WF1033D1.                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP3                                                                
003700 FILE SECTION.                                                            
003801     SKIP3                                                                
003802 FD  WF1032                                                               
003803     RECORDING       F                                                    
003804     BLOCK CONTAINS  0.                                                   
003805                                                                          
003810*01  -COPY WF1032      -L.                                                
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100                                                                          
004200 77  IDPGM                       PIC X(8)    VALUE 'WF103300'.            
004300 77  YES                         PIC X       VALUE 'J'.                   
004400 77  NOO                         PIC X       VALUE 'N'.                   
004410 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
004500     SKIP2                                                                
004600 01  ERROR-TEXT.                                                          
004700     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
004800     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
004900 01  KDRC-DISPLAY                PIC Z(5).                                
005001                                                                          
005002 77  WF1032-EOF-SW               PIC X       VALUE 'N'.                   
005010     88  END-OF-WF1032                       VALUE 'Y'.                   
005300     EJECT                                                                
006000 01  GENERAL-SUBPROGRAMS.                                                 
006100*                                                                         
006200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006300     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006410     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006420     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
006501     EJECT                                                                
006502*    --- PARAMETRAR TILL POSTSUM                                          
006503*                                                                         
006510*01  -COPY W0005   -PRE  POSTSUM-                                         
006801     EJECT                                                                
006802*    --- AREOR FOR COMMUNIKATION                                          
006803 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
006804*01  -COPY WZ01SEND                                                       
006805     EJECT                                                                
006806 01  IN01-AREA-START               PIC X(24)   VALUE                      
006807                                          'IN01-AREA-START'.              
006808     SKIP2                                                                
006809                                                                          
006810*01  AREA -COPY WF1032     -PRE IN01-                                     
006900*                                                                         
010000 LINKAGE SECTION.                                                         
010100                                                                          
010200*01  -COPY W0009   -PRE MSG-                                              
010600     EJECT                                                                
010701 PROCEDURE DIVISION  USING MSG-PCB.                                       
010702 MAIN SECTION.                                                            
010710     ENTRY 'DLITCBL' USING MSG-PCB.                                       
010800                                                                          
011000     SKIP2                                                                
011100     PERFORM A-INIT                                                       
011210     PERFORM S01-READ-WF1032                                              
011212     IF NOT END-OF-WF1032                                                 
011220       PERFORM S11-CURRENCY-OPEN                                          
011221       PERFORM B-PROCESS                                                  
011222     END-IF                                                               
011223                                                                          
011224     PERFORM S13-CURRENCY-CLOSE                                           
012300                                                                          
012400     PERFORM Z-FINIT                                                      
012500                                                                          
012600     MOVE ZERO TO RETURN-CODE                                             
012700     GOBACK                                                               
012800     .                                                                    
012900     EJECT                                                                
013000 A-INIT SECTION.                                                          
013100     SKIP2                                                                
013201                                                                          
013210     OPEN INPUT WF1032                                                    
013700                                                                          
013810     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014100     .                                                                    
014200     EJECT                                                                
014201 B-PROCESS SECTION.                                                       
014202                                                                          
014210     PERFORM UNTIL END-OF-WF1032                                          
014247       PERFORM S12-CURRENCY-PUT                                           
014248       PERFORM S01-READ-WF1032                                            
014265     END-PERFORM                                                          
014266     .                                                                    
014270     EJECT                                                                
014300 Z-FINIT SECTION.                                                         
014400                                                                          
014501                                                                          
014510     CLOSE WF1032                                                         
014701     SKIP2                                                                
014702     MOVE 'S' TO POSTSUM-OPKOD                                            
014710     CALL POSTSUM USING POSTSUM-PARM                                      
014900     .                                                                    
015001     EJECT                                                                
015002 S01-READ-WF1032  SECTION.                                                
015003     SKIP2                                                                
015004     READ WF1032 INTO IN01-AREA                                           
015005     AT END                                                               
015007        SET END-OF-WF1032 TO TRUE                                         
015008                                                                          
015009     NOT AT END                                                           
015010        MOVE 'WF1032' TO POSTSUM-FDNAMN                                   
015011        MOVE 'WF1033D1' TO POSTSUM-DDNAMN2                                
015012        MOVE SPACES    TO POSTSUM-TRANSTYP                                
015013        CALL POSTSUM USING POSTSUM-PARM                                   
015014     END-READ                                                             
015020     .                                                                    
015300     EJECT                                                                
017100 S11-CURRENCY-OPEN SECTION.                                               
017200     MOVE 'CARPARTS.PULS.YEARCURRENCY'    TO SEND-ADDISPABS               
017300     MOVE 'OPEN'                          TO SEND-KDFUNC                  
017400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
017500                         SEND-OPEN-AREA                                   
017600     IF SEND-KDRC > ZERO                                                  
017700       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
017800       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
017900       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
018000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
018100     END-IF                                                               
018200     .                                                                    
018300                                                                          
019000 S12-CURRENCY-PUT SECTION.                                                
019100     MOVE 'PUT'                           TO SEND-KDFUNC                  
019200     MOVE LENGTH OF IN01-AREA             TO SEND-KVDLEN                  
019300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
019400                         SEND-KVDLEN                                      
019500                         IN01-AREA                                        
019600     IF SEND-KDRC > 1                                                     
019700       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
019800       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
019900       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
020000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
020100     END-IF                                                               
020200     .                                                                    
020300 S13-CURRENCY-CLOSE SECTION.                                              
020400     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
020500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
020600     .                                                                    
020700     EJECT                                                                
020800                                                                          
