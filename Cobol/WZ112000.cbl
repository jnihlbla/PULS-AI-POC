000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WZ112000.                                                
000300 AUTHOR.         ANDRÉ KJELL.                                             
000400 DATE-WRITTEN.   16/12/14.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        PROGRAM FOR PREPARING INPUT PARAMETERS TO MQFT FROM              
001000*        PARAMETERS IN WZ01ATAB                                           
001100*        THIS PGM PREPARES FOR SENDING DATA (F2Q)                         
001200*                                                                         
001300*    ABENDCODES:                                                          
001400*        U0016 -  IF ADRESS IS MISSING IN WZ01ATAB                        
001500*                                                                         
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- PARAMETERS TO MQFT                                         
002500     SELECT WZ1120                     ASSIGN TO WZ1120D1.                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP3                                                                
002900 FILE SECTION.                                                            
003000     SKIP3                                                                
003100 FD  WZ1120                                                               
003200     RECORDING       F                                                    
003300     BLOCK CONTAINS  0.                                                   
003400                                                                          
003500 01  MQFT-RECORD                 PIC X(80).                               
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800                                                                          
003900 77  IDPGM                       PIC X(8)    VALUE 'WZ112000'.            
004000 77  YES                         PIC X       VALUE 'J'.                   
004100 77  NOO                         PIC X       VALUE 'N'.                   
004200     EJECT                                                                
004300                                                                          
004400 77  OK-SWITCH                   PIC X       VALUE 'J'.                   
004500   88 ALL-OK                                 VALUE 'J'.                   
004600   88 SOME-ERROR                             VALUE 'N'.                   
004700                                                                          
004710 01  SMALL-LETTERS              PIC X(31)  VALUE                          
004720     'abcdefghijklmnopqrstuvwxyzåäöüé'.                                   
004730 01  CAPS-LETTERS               PIC X(31)  VALUE                          
004740     'ABCDEFGHIJKLMNOPQRSTUVWXYZÅÄÖÜÉ'.                                   
004750                                                                          
004760     EJECT                                                                
004770 01  GENERAL-SUBPROGRAMS.                                                 
004780*                                                                         
004790     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
004800     03  WZ01ATAB                PIC X(8)    VALUE 'WZ01ATAB'.            
004900     SKIP2                                                                
005000*    --- PARAMETERS TO ABEND                                              
005100                                                                          
005200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005300 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
005400 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
005500     SKIP2                                                                
005600 01  ERROR-TEXT.                                                          
005700     03  FILLER                  PIC X(12)  VALUE 'ERROR-TEXT: '.         
005800     03  ERROR-TEXT-STR          PIC X(80)  VALUE SPACE.                  
005900     EJECT                                                                
006000*    -- PARAMETERS TO WZ01ATAB                                            
006100                                                                          
006200*01  -COPY WZ01ATAB                                                       
006300     EJECT                                                                
006400 01  MQFT-AREA-START             PIC X(24)   VALUE                        
006500                                 'MQFT-AREA-START  '.                     
006600 01  MQFT-AREA                   PIC X(80).                               
006700                                                                          
006800                                                                          
006900 01  MQ-QMGR                     PIC X(48) VALUE SPACE.                   
007000 01  MQ-QNAME                    PIC X(48) VALUE SPACE.                   
007010 01  MQ-INTEGRATION-ID           PIC X(100) VALUE SPACE.                  
007020 01  MQ-CONTRACT-ID              PIC X(100) VALUE SPACE.                  
007030                                                                          
007040     EJECT                                                                
007050 LINKAGE SECTION.                                                         
007060                                                                          
007070 01     PARM-AREA.                                                        
007080   03   PARM-LENGTH      PIC S9(4) BINARY.                                
007090   03   PARM-RTENV       PIC X(4).                                        
007100   03   PARM-ADDISPABS   PIC X(50).                                       
007110                                                                          
007120     EJECT                                                                
007130 PROCEDURE DIVISION USING PARM-AREA.                                      
007140 MAIN SECTION.                                                            
007150                                                                          
007160     PERFORM A-INIT                                                       
007170     IF ALL-OK                                                            
007180       PERFORM B-WRITE-MQFT-PARAMETERS                                    
007190     END-IF                                                               
007200     PERFORM Z-FINIT                                                      
007300                                                                          
007400     MOVE ZERO TO RETURN-CODE                                             
007500     GOBACK                                                               
007600     .                                                                    
007700     EJECT                                                                
007800 A-INIT SECTION.                                                          
007900                                                                          
008000     OPEN OUTPUT WZ1120                                                   
008100                                                                          
008200     IF PARM-LENGTH > 4                                                   
008300       MOVE PARM-ADDISPABS(1:PARM-LENGTH - 4)                             
008400         TO ATAB-ADDISPABS                                                
008500     ELSE                                                                 
008600       MOVE SPACE TO ATAB-ADDISPABS                                       
008700     END-IF                                                               
008800     INSPECT ATAB-ADDISPABS CONVERTING                                    
008900             SMALL-LETTERS TO CAPS-LETTERS                                
009000     CALL WZ01ATAB USING ATAB-WZ01ATAB                                    
009100                                                                          
009200     IF RETURN-CODE > ZERO                                                
009300*      -- ABSTRACT ADDRESS NOT FOUND IN ATAB                              
009400       SET SOME-ERROR TO TRUE                                             
009410       STRING ATAB-ADDISPABS DELIMITED BY SPACE                           
009420              ' NOT FOUND IN WZ01ATAB' DELIMITED BY SIZE                  
009430         INTO ERROR-TEXT-STR                                              
009440     ELSE                                                                 
009450       PERFORM AA-EXTRACT-MQ-PARAMETERS                                   
009460     END-IF                                                               
009470     .                                                                    
009480                                                                          
009490     EJECT                                                                
009500 AA-EXTRACT-MQ-PARAMETERS SECTION.                                        
009510                                                                          
009520*    -- QMGR NAME IS NO LONGER FETCHED FROM ATAB                          
009530*    -- INSTEAD IT IS DETERMINED FROM ENVIRONMENT SPECIFIED               
009540*    -- ON EXEC-CARD PARM                                                 
009550                                                                          
009560     IF PARM-RTENV  = 'QASE'                                              
009570       MOVE 'MC0P'   TO MQ-QMGR                                           
009580     ELSE                                                                 
009590       IF PARM-RTENV = 'ACPT'                                             
009600         MOVE 'MC0Q' TO MQ-QMGR                                           
009601       ELSE                                                               
009602         MOVE 'MC0T' TO MQ-QMGR                                           
009603       END-IF                                                             
009604     END-IF                                                               
009605                                                                          
009606*    -- QUEUE NAME                                                        
009607     MOVE ZERO TO TALLY                                                   
009608     INSPECT ATAB-ADDISPINT TALLYING TALLY                                
009609             FOR CHARACTERS BEFORE INITIAL 'RQ:'                          
009610     MOVE SPACE TO MQ-QNAME                                               
009611     IF TALLY < LENGTH OF ATAB-ADDISPINT                                  
009612       UNSTRING ATAB-ADDISPINT(TALLY + 4:)                                
009613       DELIMITED BY ';'                                                   
009614       INTO MQ-QNAME                                                      
009615     ELSE                                                                 
009616       SET SOME-ERROR TO TRUE                                             
009617       MOVE 'ALIAS/REMOTE QUEUE NAME I MISSING IN WZ01ATAB'               
009618         TO ERROR-TEXT-STR                                                
009619     END-IF                                                               
009620                                                                          
009621*    -- META DATA: INTEGRATION ID                                         
009622     MOVE ZERO TO TALLY                                                   
009623     INSPECT ATAB-ADDISPMETA TALLYING TALLY                               
009624             FOR CHARACTERS BEFORE INITIAL 'I:'                           
009625     MOVE SPACE TO MQ-INTEGRATION-ID                                      
009626     IF TALLY < LENGTH OF ATAB-ADDISPMETA                                 
009627       UNSTRING ATAB-ADDISPMETA(TALLY + 3:)  DELIMITED BY ';'             
009628       INTO MQ-INTEGRATION-ID                                             
009629     END-IF                                                               
009630                                                                          
009631*    -- META DATA: CONTRACT ID                                            
009632     MOVE ZERO TO TALLY                                                   
009633     INSPECT ATAB-ADDISPMETA TALLYING TALLY                               
009634             FOR CHARACTERS BEFORE INITIAL 'C:'                           
009635     MOVE SPACE TO MQ-CONTRACT-ID                                         
009636     IF TALLY < LENGTH OF ATAB-ADDISPMETA                                 
009637       UNSTRING ATAB-ADDISPMETA(TALLY + 3:)  DELIMITED BY ';'             
009638       INTO MQ-CONTRACT-ID                                                
009639     END-IF                                                               
009640     .                                                                    
009641                                                                          
009642 B-WRITE-MQFT-PARAMETERS SECTION.                                         
009643                                                                          
009644     DISPLAY 'MQFT parameters:'                                           
009645                                                                          
009646     MOVE '-W F2Q'  TO MQFT-AREA                                          
009647     WRITE MQFT-RECORD FROM MQFT-AREA                                     
009648     DISPLAY MQFT-AREA                                                    
009649                                                                          
009650     MOVE SPACE TO MQFT-AREA                                              
009651     STRING '-M ' MQ-QMGR                                                 
009652       DELIMITED BY SIZE INTO MQFT-AREA                                   
009653     WRITE MQFT-RECORD FROM MQFT-AREA                                     
009654     DISPLAY MQFT-AREA                                                    
009655                                                                          
009656     MOVE SPACE TO MQFT-AREA                                              
009657     STRING '-Q ' MQ-QNAME                                                
009658       DELIMITED BY SIZE INTO MQFT-AREA                                   
009659     WRITE MQFT-RECORD FROM MQFT-AREA                                     
009660     DISPLAY MQFT-AREA                                                    
009661                                                                          
009662     MOVE '-T NONE' TO MQFT-AREA                                          
009663     WRITE MQFT-RECORD FROM MQFT-AREA                                     
009664     DISPLAY MQFT-AREA                                                    
009665                                                                          
009666     MOVE '-R '     TO MQFT-AREA                                          
009667     WRITE MQFT-RECORD FROM MQFT-AREA                                     
009668     DISPLAY MQFT-AREA                                                    
009669                                                                          
009670     MOVE '-G '     TO MQFT-AREA                                          
009671     WRITE MQFT-RECORD FROM MQFT-AREA                                     
009672     DISPLAY MQFT-AREA                                                    
009673                                                                          
009674     IF MQ-INTEGRATION-ID NOT = SPACE                                     
009675       MOVE SPACE TO MQFT-AREA                                            
009676       STRING '-XPARM  "IntegrationId=' DELIMITED BY SIZE                 
009677               MQ-INTEGRATION-ID DELIMITED BY SPACE                       
009678               '"' DELIMITED BY SIZE                                      
009679       INTO MQFT-AREA                                                     
009680       WRITE MQFT-RECORD FROM MQFT-AREA                                   
009681       DISPLAY MQFT-AREA                                                  
009682     END-IF                                                               
009683                                                                          
009684     IF MQ-CONTRACT-ID NOT = SPACE                                        
009685       MOVE SPACE TO MQFT-AREA                                            
009686       STRING '-XPARM  "ContractId=' DELIMITED BY SIZE                    
009687               MQ-CONTRACT-ID DELIMITED BY SPACE                          
009688               '"' DELIMITED BY SIZE                                      
009689       INTO MQFT-AREA                                                     
009690       WRITE MQFT-RECORD FROM MQFT-AREA                                   
009691       DISPLAY MQFT-AREA                                                  
009692     END-IF                                                               
009693     .                                                                    
009694                                                                          
009695     EJECT                                                                
009696 Z-FINIT SECTION.                                                         
009697                                                                          
009698     CLOSE WZ1120                                                         
009699                                                                          
009700     IF SOME-ERROR                                                        
009800       DISPLAY 'WZ1120 ' ERROR-TEXT                                       
009900       CALL ABEND USING RKOD-ABEND-NO-DUMP                                
009910     END-IF                                                               
009920     .                                                                    
