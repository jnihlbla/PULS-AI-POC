010001 ID DIVISION.                                                             
020001 PROGRAM-ID.     WZ112100.                                                
030001 AUTHOR.         ANDRÉ KJELL.                                             
040001 DATE-WRITTEN.   16/12/14.                                                
050001 DATE-COMPILED.                                                           
060001                                                                          
070001*                                                                         
080001*    FUNCTION:                                                            
090001*        PROGRAM FOR PREPARING INPUT PARAMETERS TO MQFT FROM              
100001*        PARAMETERS IN WZ01ATAB.                                          
110001*        THIS PGM PREPARES FOR RECEIVING DATA (Q2F)                       
120001*                                                                         
130001*    ABENDCODES:                                                          
140001*        U0016 -  IF ADRESS IS MISSING IN WZ01ATAB                        
150001*                                                                         
160001                                                                          
170001     SKIP3                                                                
180001 ENVIRONMENT DIVISION.                                                    
190001     SKIP2                                                                
200001 INPUT-OUTPUT SECTION.                                                    
210001                                                                          
220001 FILE-CONTROL.                                                            
230001     SKIP2                                                                
240001*          --- PARAMETERS TO MQFT                                         
250001     SELECT WZ1121                     ASSIGN TO WZ1121D1.                
260001     EJECT                                                                
270001 DATA DIVISION.                                                           
280001     SKIP3                                                                
290001 FILE SECTION.                                                            
300001     SKIP3                                                                
310001 FD  WZ1121                                                               
320001     RECORDING       F                                                    
330001     BLOCK CONTAINS  0.                                                   
340001                                                                          
350001 01  MQFT-RECORD                 PIC X(80).                               
360001     EJECT                                                                
370001 WORKING-STORAGE SECTION.                                                 
380001                                                                          
390002 77  IDPGM                       PIC X(8)    VALUE 'WZ112100'.            
400001 77  YES                         PIC X       VALUE 'J'.                   
410001 77  NOO                         PIC X       VALUE 'N'.                   
420001     EJECT                                                                
430001                                                                          
440001 77  OK-SWITCH                   PIC X       VALUE 'J'.                   
450001   88 ALL-OK                                 VALUE 'J'.                   
460001   88 SOME-ERROR                             VALUE 'N'.                   
470001                                                                          
471001 01  SMALL-LETTERS              PIC X(31)  VALUE                          
472001     'abcdefghijklmnopqrstuvwxyzåäöüé'.                                   
473001 01  CAPS-LETTERS               PIC X(31)  VALUE                          
474001     'ABCDEFGHIJKLMNOPQRSTUVWXYZÅÄÖÜÉ'.                                   
475001                                                                          
476001     EJECT                                                                
477001 01  GENERAL-SUBPROGRAMS.                                                 
478001*                                                                         
479001     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
480001     03  WZ01ATAB                PIC X(8)    VALUE 'WZ01ATAB'.            
490001     SKIP2                                                                
500001*    --- PARAMETERS TO ABEND                                              
510001                                                                          
520001 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
530001 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
540001 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
550001     SKIP2                                                                
560001 01  ERROR-TEXT.                                                          
570001     03  FILLER                  PIC X(12)  VALUE 'ERROR-TEXT: '.         
580001     03  ERROR-TEXT-STR          PIC X(80)  VALUE SPACE.                  
590001     EJECT                                                                
600001*    -- PARAMETERS TO WZ01ATAB                                            
610001                                                                          
620001*01  -COPY WZ01ATAB                                                       
630001     EJECT                                                                
640001 01  MQFT-AREA-START             PIC X(24)   VALUE                        
650001                                 'MQFT-AREA-START  '.                     
660001 01  MQFT-AREA                   PIC X(80).                               
670001                                                                          
680001                                                                          
690001 01  MQ-QMGR                     PIC X(48) VALUE SPACE.                   
700001 01  MQ-QNAME                    PIC X(48) VALUE SPACE.                   
701001 01  MQ-INTEGRATION-ID           PIC X(100) VALUE SPACE.                  
702001 01  MQ-CONTRACT-ID              PIC X(100) VALUE SPACE.                  
703001                                                                          
704001     EJECT                                                                
705001 LINKAGE SECTION.                                                         
706001                                                                          
707001 01     PARM-AREA.                                                        
708001   03   PARM-LENGTH      PIC S9(4) BINARY.                                
709001   03   PARM-RTENV       PIC X(4).                                        
710001   03   PARM-ADDISPABS   PIC X(50).                                       
711001                                                                          
712001     EJECT                                                                
713001 PROCEDURE DIVISION USING PARM-AREA.                                      
714001 MAIN SECTION.                                                            
715001                                                                          
716001     PERFORM A-INIT                                                       
717001     IF ALL-OK                                                            
718001       PERFORM B-WRITE-MQFT-PARAMETERS                                    
719001     END-IF                                                               
720001     PERFORM Z-FINIT                                                      
730001                                                                          
740001     MOVE ZERO TO RETURN-CODE                                             
750001     GOBACK                                                               
760001     .                                                                    
770001     EJECT                                                                
780001 A-INIT SECTION.                                                          
790001                                                                          
800002     OPEN OUTPUT WZ1121                                                   
810001                                                                          
820001     IF PARM-LENGTH > 4                                                   
830001       MOVE PARM-ADDISPABS(1:PARM-LENGTH - 4)                             
840001         TO ATAB-ADDISPABS                                                
850001     ELSE                                                                 
860001       MOVE SPACE TO ATAB-ADDISPABS                                       
870001     END-IF                                                               
880001     INSPECT ATAB-ADDISPABS CONVERTING                                    
890001             SMALL-LETTERS TO CAPS-LETTERS                                
900001     CALL WZ01ATAB USING ATAB-WZ01ATAB                                    
910001                                                                          
920001     IF RETURN-CODE > ZERO                                                
930001*      -- ABSTRACT ADDRESS NOT FOUND IN ATAB                              
940001       SET SOME-ERROR TO TRUE                                             
941001       STRING ATAB-ADDISPABS DELIMITED BY SPACE                           
942001              ' NOT FOUND IN WZ01ATAB' DELIMITED BY SIZE                  
943001         INTO ERROR-TEXT-STR                                              
944001     ELSE                                                                 
945001       PERFORM AA-EXTRACT-MQ-PARAMETERS                                   
946001     END-IF                                                               
947001     .                                                                    
948001                                                                          
949001     EJECT                                                                
950001 AA-EXTRACT-MQ-PARAMETERS SECTION.                                        
951001                                                                          
952001*    -- QMGR NAME IS NO LONGER FETCHED FROM ATAB                          
953001*    -- INSTEAD IT IS DETERMINED FROM ENVIRONMENT SPECIFIED               
954001*    -- ON EXEC-CARD PARM                                                 
955001                                                                          
956001     IF PARM-RTENV  = 'QASE'                                              
957001       MOVE 'MC0P'   TO MQ-QMGR                                           
958001     ELSE                                                                 
959001       IF PARM-RTENV = 'ACPT'                                             
960001         MOVE 'MC0Q' TO MQ-QMGR                                           
960101       ELSE                                                               
960201         MOVE 'MC0T' TO MQ-QMGR                                           
960301       END-IF                                                             
960401     END-IF                                                               
960501                                                                          
960601*    -- QUEUE NAME                                                        
960602                                                                          
960701     MOVE ZERO TO TALLY                                                   
960801     INSPECT ATAB-ADDISPINT-RECV TALLYING TALLY                           
960901             FOR CHARACTERS BEFORE INITIAL 'LQ:'                          
961001     MOVE SPACE TO MQ-QNAME                                               
961101     IF TALLY < LENGTH OF ATAB-ADDISPINT-RECV                             
961201       UNSTRING ATAB-ADDISPINT-RECV(TALLY + 4:)                           
961301       DELIMITED BY ';'                                                   
961401       INTO MQ-QNAME                                                      
961501     ELSE                                                                 
961601       SET SOME-ERROR TO TRUE                                             
961701       MOVE 'LOCAL QUEUE NAME I MISSING IN WZ01ATAB'                      
961801         TO ERROR-TEXT-STR                                                
961901     END-IF                                                               
962002     .                                                                    
962102                                                                          
962202     EJECT                                                                
964201 B-WRITE-MQFT-PARAMETERS SECTION.                                         
964301                                                                          
964302     DISPLAY 'MQFT parameters:'                                           
964303                                                                          
964401     MOVE '-W Q2F'  TO MQFT-AREA                                          
964501     WRITE MQFT-RECORD FROM MQFT-AREA                                     
964502     DISPLAY MQFT-AREA                                                    
964601                                                                          
964701     MOVE SPACE TO MQFT-AREA                                              
964801     STRING '-M ' MQ-QMGR                                                 
964901       DELIMITED BY SIZE INTO MQFT-AREA                                   
965001     WRITE MQFT-RECORD FROM MQFT-AREA                                     
965002     DISPLAY MQFT-AREA                                                    
965101                                                                          
965201     MOVE SPACE TO MQFT-AREA                                              
965301     STRING '-Q ' MQ-QNAME                                                
965401       DELIMITED BY SIZE INTO MQFT-AREA                                   
965501     WRITE MQFT-RECORD FROM MQFT-AREA                                     
965502     DISPLAY MQFT-AREA                                                    
965601                                                                          
965701     MOVE '-T NONE' TO MQFT-AREA                                          
965801     WRITE MQFT-RECORD FROM MQFT-AREA                                     
965802     DISPLAY MQFT-AREA                                                    
965901                                                                          
966001     MOVE '-R '     TO MQFT-AREA                                          
967001     WRITE MQFT-RECORD FROM MQFT-AREA                                     
967002     DISPLAY MQFT-AREA                                                    
967101                                                                          
967201     MOVE '-G '     TO MQFT-AREA                                          
967301     WRITE MQFT-RECORD FROM MQFT-AREA                                     
967302     DISPLAY MQFT-AREA                                                    
969201     .                                                                    
969301                                                                          
969401     EJECT                                                                
969501 Z-FINIT SECTION.                                                         
969601                                                                          
969701     CLOSE WZ1121                                                         
969801                                                                          
969901     IF SOME-ERROR                                                        
970001       DISPLAY 'WZ1121 ' ERROR-TEXT                                       
980001       CALL ABEND USING RKOD-ABEND-NO-DUMP                                
990001     END-IF                                                               
990002     .                                                                    
