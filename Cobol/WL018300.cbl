000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL018300.                                                
000300 AUTHOR.         KARANDE DIGAMBAR.                                        
000400 DATE-WRITTEN.   02/09/19.                                                
000500*AUTHOR.         BERT ANDERSSON.                                          
000600*DATE-WRITTEN.   MAJ   2005.                                              
000700                                                                          
000800                                                                          
000900     REMARKS.                                                             
001000* WL018300 PROGRAM IS A REPLICA OF W4062100 PROGRAM                       
001100* AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS.                                
001200*                                                                         
001300*    NAMN:       CARPARTS.LDC.SHIPPINGDOCEXT                              
001400*                                                                         
001500*    FUNCTION:                                                            
001600*        IT SHOWS THE SHIPMENT INFORMATION FROM WDE1. THIS SCREEN         
001700*        STARTS ONLY FROM THE 4622.                                       
001800*                                                                         
001900*        THE PROGRAM READS     WDE1                                       
002000*                                                                         
002100*    INDATA.                                                              
002200*        TRANSACTION: WL0183                                              
002300*        REQUEST:     WL0183I1                                            
002400*                                                                         
002500*    OUTDATA.                                                             
002600*        RESPONS:     WL0183O1                                            
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000                                                                          
003100 DATA DIVISION.                                                           
003200     EJECT                                                                
003300 WORKING-STORAGE SECTION.                                                 
003400 77  IDPGM                       PIC X(08)   VALUE 'WL018300'.            
003500                                                                          
003600*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003700 77  FILLER                      PIC X(08)   VALUE 'AAAAAAAA'.            
003800 77  PGM-POS                     PIC X(32)   VALUE SPACE.                 
003900 77  KDRC-DISPLAY                PIC Z(5)   VALUE ZERO.                   
004000 77  ERROR-TEXT                  PIC X(64)   VALUE SPACE.                 
004100                                                                          
004200 77  YES                         PIC X       VALUE 'J'.                   
004300 77  NOO                         PIC X       VALUE 'N'.                   
004400                                                                          
004500*    --- INDEX FOR SCROLL LINES                                           
004600 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004700 77  MAX-INDX-500                PIC S9(4)  VALUE +500  COMP SYNC.        
004800*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
004900                                                                          
005200 77  KEYS-SW                     PIC X       VALUE 'J'.                   
005300     88  KEYS-OK                             VALUE 'J'.                   
005400     88  KEYS-WRONG                          VALUE 'N'.                   
006000     EJECT                                                                
006100*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006200 01  GENERAL-SUBPROGRAMS.                                                 
006500     03  CBLTDLI                 PIC X(8) VALUE 'CBLTDLI '.               
006600     03  FELLOG                  PIC X(8) VALUE 'FELLOG  '.               
006700     03  ABEND                   PIC X(8) VALUE 'ABEND   '.               
006800     03  WZ01SUB                 PIC X(8) VALUE 'WZ01SUB '.               
006900     SKIP2                                                                
007000*    --- PARAMETERS TO ABEND                                              
007100                                                                          
007200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007300 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007400 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007500     SKIP2                                                                
008400                                                                          
008500 01    FILLER                    PIC X(08) VALUE 'MESSAGE:'.              
008600 01  MESSAGE-CODES.                                                       
009700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '043'.                 
010500     SKIP3                                                                
010600*                                                                         
010700     EJECT                                                                
012500******************************************************************        
012600*                                                                         
012700 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
012800     SKIP3                                                                
012900*01  -COPY WZ01SUB                                                        
013000     SKIP3                                                                
013100 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
013200     SKIP3                                                                
013300 01  REQU-AREA.                                                           
013400*    03  -COPY WZ01REQU                                                   
013500*    03  -COPY WL0183I1                                                   
013600     SKIP3                                                                
013700 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
013800     SKIP3                                                                
013900 01  RESP-AREA.                                                           
014000*    03  -COPY WZ01RESP                                                   
014100*    03  -COPY WL0183O1                                                   
014200******************************************************************        
014300     SKIP3                                                                
014400*    --- WORK-AREAS FOR IMS-SECTIONS                                      
014500*                                                                         
014600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014700     SKIP3                                                                
014800 01  KEYS-TO-DLI.                                                         
014900                                                                          
015000     03  W-IDSHIPM-X.                                                     
015100         05  W-IDSHIPM           PIC 9(7)    VALUE ZERO.                  
015200                                                                          
015300     03  W-WDE111KY-X.                                                    
015400         05  W-WDE111-IDDISTR    PIC S9(5)   VALUE ZERO  COMP-3.          
015500         05  W-WDE111-IDKUNDNR   PIC S9(7)   VALUE ZERO  COMP-3.          
015600                                                                          
015700     03  W-WDE121KY-X.                                                    
015800         05  W-WDE121-IDPRODNR   PIC S9(7)   VALUE ZERO  COMP-3.          
015900         05  W-WDE121-IDKOLLI    PIC S9(5)   VALUE ZERO  COMP-3.          
016000                                                                          
016100     SKIP2                                                                
016200*    --- STATUS-KOD FRÅN IMS                                              
016300 01  STATUS-WS                   PIC XX.                                  
016400     88  SEGMENT-FOUND                       VALUE '  '.                  
016500     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
016600     88  SEGMENT-MISSING                     VALUE 'GE'.                  
016700     SKIP2                                                                
016800 01  GOOD-STATUSCODES.                                                    
016900     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017000     SKIP3                                                                
017100 01  SSA1                        PIC X(64).                               
017200 01  SSA2                        PIC X(64).                               
017300 01  SSA3                        PIC X(64).                               
017400     SKIP2                                                                
017500*    --- IMS FUNCTION CODES                                               
017600*01  -COPY W0003                                                          
017700     SKIP2                                                                
017800*    ---  DLI INPUT-OUTPUT AREA                                           
017900                                                                          
018000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE101'.                      
018100 01  DLI-IO-WDE101.                                                       
018200*    03  -COPY WDE101                                                     
018300     SKIP2                                                                
018400 01  FILLER         PIC X(25) VALUE 'DLI-IO-WDE111-WDE121'.               
018500 01  DLI-IO-WDE111-121.                                                   
018600     03  DLI-IO-WDE111.                                                   
018700*        05  -COPY WDE111                                                 
018800     03  DLI-IO-WDE121.                                                   
018900*        05  -COPY WDE121                                                 
019000     SKIP2                                                                
019100 LINKAGE SECTION.                                                         
019200*01  -COPY W0009   -PRE MSG-                                              
019300                                                                          
019400*01  -COPY W0008  -PRE WDE1-                                              
019500     05  FILLER                  PIC X.                                   
019600     SKIP2                                                                
019700 PROCEDURE DIVISION  USING MSG-PCB WDE1-PCB.                              
019800 MAIN SECTION.                                                            
019900     ENTRY 'DLITCBL' USING MSG-PCB WDE1-PCB.                              
020000     SKIP2                                                                
020100     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
020200     IF SUB-KDRC = 0                                                      
020300       IF REQU-KDPGMACT = 'S'                                             
020400                                                                          
020500         PERFORM A-INIT                                                   
020600         PERFORM B-CHECK-KEYS                                             
020700         IF KEYS-OK                                                       
021300           PERFORM F-READ-SHOW-INFO                                       
021400         END-IF                                                           
021500                                                                          
021600       END-IF                                                             
021700       PERFORM S02-RETURN-RESPONSE                                        
021800     END-IF                                                               
021900                                                                          
022000     MOVE ZERO TO RETURN-CODE                                             
022100     GOBACK                                                               
022200     .                                                                    
022300     EJECT                                                                
022400 A-INIT SECTION.                                                          
022500     MOVE 'STA A-INIT        ' TO PGM-POS                                 
022600                                                                          
022700     MOVE ALL '+'              TO RESP-AREA                               
022800     MOVE 001                  TO RESP-IDMSGVER                           
022900     MOVE SPACE                TO RESP-IDMSG-ERROR                        
023000                                  RESP-IDMSG-INFO                         
023100                                  RESP-IDELMT-ERROR                       
023200                                                                          
023600     MOVE 'END A-INIT        ' TO PGM-POS                                 
023700     .                                                                    
023800     EJECT                                                                
023900 B-CHECK-KEYS SECTION.                                                    
024000     MOVE 'STA B-CHECK-KEYS  ' TO PGM-POS                                 
024500                                                                          
024600     MOVE YES TO KEYS-SW                                                  
024900                                                                          
025000*    -- CHECK OF IDSHIPM                                                  
025600                                                                          
025700     IF REQU-IDSHIPM-KEY NUMERIC                                          
025800       MOVE REQU-IDSHIPM-KEY TO W-IDSHIPM                                 
025900     ELSE                                                                 
026000       MOVE NOO TO KEYS-SW                                                
026100       MOVE ZERO         TO W-IDSHIPM                                     
026200     END-IF                                                               
026300                                                                          
026400     IF KEYS-OK                                                           
026500       IF REQU-IDSHIPM-KEY NUMERIC                                        
026600         MOVE REQU-IDSHIPM-KEY TO RESP-IDSHIPM-KEY                        
026700       ELSE                                                               
026800         MOVE ZERO            TO RESP-IDSHIPM-KEY                         
026900       END-IF                                                             
027200     END-IF                                                               
027400     IF KEYS-WRONG                                                        
027500       MOVE 'IDSHIPM'            TO RESP-IDELMT-ERROR                     
027600       MOVE ERR-WRONG-KEY        TO RESP-IDMSG-ERROR                      
028000     END-IF                                                               
028100     .                                                                    
028200     EJECT                                                                
030900 F-READ-SHOW-INFO SECTION.                                                
031000                                                                          
031100     PERFORM FA-READ-BASICDATA                                            
031200                                                                          
031300     IF SEGMENT-MISSING                                                   
031500        MOVE 'IDSHIPM'           TO RESP-IDELMT-ERROR                     
031600        MOVE '023'               TO RESP-IDMSG-ERROR                      
032300     ELSE                                                                 
032400       MOVE +1 TO INDX                                                    
033600                                                                          
033700       PERFORM UNTIL INDX > MAX-INDX-500                                  
033800         IF SEGMENT-FOUND                                                 
033900           MOVE SGMT-IDDISTR    TO RESP-IDDISTR (INDX)                    
034000           MOVE SGMT-IDKUNDNR   TO RESP-IDKUNDNR (INDX)                   
034100           MOVE SKOLLI-IDORDNR7 TO RESP-IDORDNR5 (INDX)                   
034200           MOVE SKOLLI-IDKOLLI  TO RESP-IDKOLLI (INDX)                    
034210           MOVE INDX            TO RESP-KVRADER                           
034300           PERFORM IMS-GNP-WDE111-121                                     
034900         END-IF                                                           
035000         ADD 1 TO INDX                                                    
035010                                                                          
035100       END-PERFORM                                                        
035200                                                                          
037100                                                                          
037200     END-IF                                                               
037300     .                                                                    
037400     EJECT                                                                
037500 FA-READ-BASICDATA SECTION.                                               
037600                                                                          
037700     PERFORM IMS-GU-WDE101                                                
037800                                                                          
037900     IF SEGMENT-FOUND                                                     
038300         PERFORM IMS-GNP-WDE111-121                                       
038500     END-IF                                                               
038600     .                                                                    
038700     EJECT                                                                
038800                                                                          
038900 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
039000     MOVE 'STA S01-FETCH-REQUEST          ' TO PGM-POS                    
039100                                                                          
039200     MOVE 'GETARG'               TO SUB-KDFUNC                            
039300     MOVE 'CARPARTS.LDC.UNREPORTEDLINES'     TO SUB-ADDISPABS             
039400     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
039500                                                                          
039600     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
039700                                                                          
039800     IF SUB-KDRC > 0                                                      
039900       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
040000       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
040100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
040200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
040300     END-IF                                                               
040400     MOVE 'END S01-FETCH-REQUEST          ' TO PGM-POS                    
040500     .                                                                    
040600     SKIP3                                                                
040700 S02-RETURN-RESPONSE SECTION.                                             
040800     MOVE 'STA S02-RETURN-RESPONSE        ' TO PGM-POS                    
040900                                                                          
041000     MOVE 'RETURN'                   TO SUB-KDFUNC                        
041100     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
041200                                                                          
041300     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
041400                                                                          
041500     IF SUB-KDRC > 0                                                      
041600       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
041700       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
041800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
041900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
042000     END-IF                                                               
042100     MOVE 'END S02-RETURN-RESPONSE        ' TO PGM-POS                    
042200     .                                                                    
042300     EJECT                                                                
042400* --- IMS SECTIONS ---                                                    
042500     SKIP3                                                                
042600 IMS-GU-WDE101 SECTION.                                                   
042700                                                                          
042800     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
042900          DELIMITED BY SIZE INTO SSA1                                     
043000     MOVE '  GE' TO GOOD-STATUSCODES                                      
043100     CALL CBLTDLI USING GU WDE1-PCB DLI-IO-WDE101 SSA1                    
043200     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
043300     PERFORM IMS-STATUSCHECK                                              
043400     .                                                                    
043500     EJECT                                                                
043600 IMS-GNP-WDE111-121 SECTION.                                              
043700                                                                          
043800     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
043900          DELIMITED BY SIZE INTO SSA1                                     
044000     MOVE   'WDE111  *D'  TO SSA2                                         
044100     MOVE   'WDE121  *D'  TO SSA3                                         
044200     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
044300     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE111-121                    
044400                                             SSA1 SSA2 SSA3               
044500     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
044600     PERFORM IMS-STATUSCHECK                                              
044700     .                                                                    
044800     EJECT                                                                
046400 IMS-STATUSCHECK SECTION.                                                 
046500                                                                          
046600     SET STATUS-IX TO 1                                                   
046700     SEARCH GOOD-STATUS                                                   
046800       AT END                                                             
046900         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
047000         DELIMITED BY SIZE INTO ERROR-TEXT                                
047100         CALL FELLOG                                                      
047200       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
047300         CONTINUE                                                         
047400     END-SEARCH                                                           
047500     .                                                                    
