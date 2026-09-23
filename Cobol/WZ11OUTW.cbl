000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WZ11OUTW.                                                
000300 AUTHOR.         ANDRE KJELL.                                             
000400 DATE-WRITTEN.   02/12/20.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        THIS PROGRAM HANDLES OUTPUT WHEN D&P IS TRIGGERED                
000810*        FROM AN MPP THAT WAS CALLED FROM THE WEB.                        
000820*        THE DATA RECEIVED VIA THE PUT CALL IS NOT USED OR                
000830*        PROCESSED BY THIS PROGRAM. ONLY THE INFORMATION PASSED           
000900*        IN THE OPEN CALL IS USED, AND RETURNED TO                        
001000*        THE WEB APPLICATION IN THE CLOSE CALL.                           
001100*        THE WEB APPLICATION THEN CALLS THE WZ0414 PROGRAM WHICH          
001101*        USES THIS INFORMATION TO RETRIEVE THE ACTUAL DATA                
001200                                                                          
001300     EJECT                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500     SKIP3                                                                
001600 CONFIGURATION SECTION.                                                   
001700 SPECIAL-NAMES.                                                           
001800     CLASS ALPHANUM-UPPER-AND-SPACE                                       
001900           IS ' ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'.                    
002000                                                                          
002100     EJECT                                                                
002200 DATA DIVISION.                                                           
002300     SKIP3                                                                
002400 WORKING-STORAGE SECTION.                                                 
002500 77  IDPGM                       PIC X(08)   VALUE 'WZ11OUTW'.            
002600                                                                          
002700*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
002800 77  ERROR-TEXT-START            PIC X(8)  VALUE 'ERROR:  '.              
002900 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003000 77  KDRC-DISPLAY                PIC Z(5).                                
003001*    --- WORK FIELD USED WHEN MOVING FROM S9(9) - 9(8) - X(8)             
003010 77  W-TIKLOCK                   PIC 9(8).                                
003100                                                                          
003200 77  YES                         PIC X       VALUE 'J'.                   
003300 77  NOO                         PIC X       VALUE 'N'.                   
003400                                                                          
004200     EJECT                                                                
004300*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
004400 01  GENERAL-SUBPROGRAMS.                                                 
004500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
004600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
004700     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
005100                                                                          
005200*    --- PARAMETERS TO ABEND                                              
005300 77  RCODE-ABEND-NO-DUMP         PIC S9(4)   COMP VALUE +16.              
005400 77  RCODE-ABEND-WITH-DUMP       PIC S9(4)   COMP VALUE +1000.            
005500                                                                          
006100*    --- PARAMETERS TO WZ01SUB                                            
006200 01  -COPY WZ01SUB                                                        
006210     EJECT                                                                
006300                                                                          
006310 01  FILLER                      PIC X(16)   VALUE 'REQUEST-AREA'.        
006320                                                                          
006321*    -- AREA TO STORE THE ARGUMENT FROM THE GETARG CALL.                  
006322*    -- IT CORRESPONDS TO THE FIRST HEADER RECORD SENT TO THE             
006323*    -- D&P WZ0420 MAIN PROGRAM AND IS NOT RELEVANT FOR THIS              
006324*    -- PROGRAM, BUT THE COPYTEXTS BELOW ARE USED TO GET THE              
006325*    -- CORRECT LENGTH.                                                   
006330 01  REQU-AREA.                                                           
006340*    03  -COPY WZ01REQU                                                   
006350*    03  -COPY WZ04HDR                                                    
006351                                                                          
006360     EJECT                                                                
006370 01  FILLER                      PIC X(16)  VALUE 'RESPONSE-AREA'.        
006380                                                                          
006800 01  RESP-AREA.                                                           
006900*    03  -COPY WZ01RESP                                                   
007000*    03  -COPY WZ0420O1                                                   
007100     EJECT                                                                
011100 LINKAGE SECTION.                                                         
011200*01  -COPY  WZ11OUTW                                                      
011300     EJECT                                                                
011600 PROCEDURE DIVISION  USING OUTW-WZ11OUT.                                  
011700 MAIN SECTION.                                                            
011800                                                                          
011900     PERFORM A-INIT                                                       
012000                                                                          
012100     EVALUATE OUTW-KDFUNC                                                 
012200       WHEN 'OPEN'   PERFORM B-OPEN                                       
012210*      -- THE DATA RECEIVED VIA THE PUT CALL IS NOT USED!                 
012300       WHEN 'PUT'    CONTINUE                                             
012400       WHEN 'CLOSE'  PERFORM C-CLOSE                                      
012500     END-EVALUATE                                                         
012600                                                                          
012700     MOVE OUTW-KDRC TO RETURN-CODE                                        
012800     GOBACK                                                               
012900     .                                                                    
013000     EJECT                                                                
013100 A-INIT SECTION.                                                          
013200                                                                          
013300     IF OUTW-IDCALL NOT = 1                                               
013400       MOVE OUTW-IDCALL TO KDRC-DISPLAY                                   
013500       STRING 'WZ11OUTW INVALID IDCALL VALUE: '                           
013600              KDRC-DISPLAY                                                
013700          DELIMITED BY SIZE                                               
013800          INTO ERROR-TEXT                                                 
013900       CALL ABEND USING RCODE-ABEND-WITH-DUMP                             
014000     END-IF                                                               
014100                                                                          
014800*    -- SO FAR, SO GOOD                                                   
014900     MOVE ZERO TO OUTW-KDRC                                               
015000     .                                                                    
015100     EJECT                                                                
015200 B-OPEN   SECTION.                                                        
015300                                                                          
015402*    -- SAVE OUTPUT ID INFORMATION FOR THE CLOSE CALL                     
016200     MOVE OUTW-IDOUTTYPE TO RESP-IDOUTTYPE-KEY                            
016300     MOVE OUTW-IDOUTREC  TO RESP-IDOUTREC-KEY                             
016310     MOVE OUTW-IDLIST    TO RESP-IDLIST-KEY                               
016320     MOVE OUTW-TIREGDAT  TO RESP-TIREGDAT-KEY                             
016321*    -- MOVE IN TWO STEPS SO 10TH OF SECONDS WILL NOT BE LOST             
016330     MOVE OUTW-TIKLOCK   TO W-TIKLOCK                                     
016331     MOVE W-TIKLOCK      TO RESP-TIKLOCK-KEY                              
016340     MOVE OUTW-IDLOPNR   TO RESP-IDLOPNR-KEY                              
016400                                                                          
032400     .                                                                    
032500     EJECT                                                                
034700 C-CLOSE             SECTION.                                             
034800                                                                          
034801*    -- WZ01SUB MUST FIRST BE CALLED WITH FUNCTION 'OPEN'                 
034802*    -- TO MAKE IT POSSIBLE TO USE THE 'RETURN' CALL.                     
034806*    -- THE ABSTRACT ADDRESS IS A DUMMY ENTRY IN THE                      
034807*    -- ADDRESS TABLE CREATED PARTICULARLY FOR THIS PROGRAM               
034808                                                                          
034809     MOVE 'OPEN'                 TO SUB-KDFUNC                            
034810     MOVE 'CARPARTS.DAP.WEB'     TO SUB-ADDISPABS                         
034811     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
034812                                                                          
034813     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
034814                                                                          
034815     IF SUB-KDRC > 0                                                      
034816       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
034817       STRING 'WZ11OUTW/WZ01SUB OPEN ERROR RC=' KDRC-DISPLAY              
034818       DELIMITED BY SIZE INTO ERROR-TEXT                                  
034819       CALL ABEND USING RCODE-ABEND-WITH-DUMP                             
034820     END-IF                                                               
034821                                                                          
034822*    -- RETURN THE OUTPUT ID TO THE WEB APPLICATION                       
034823                                                                          
034824     MOVE 001                 TO RESP-IDMSGVER                            
034825     MOVE 'PRI'               TO RESP-IDMSG-INFO                          
034826     MOVE SPACE               TO RESP-IDMSG-ERROR                         
034827     MOVE SPACE               TO RESP-IDELMT-ERROR                        
034828                                                                          
034829     MOVE 'RETURN'            TO SUB-KDFUNC                               
034830     MOVE LENGTH OF RESP-AREA TO SUB-KVDLEN                               
034840                                                                          
034850     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
034860                                                                          
034870     IF SUB-KDRC > 0                                                      
034880       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
034890       STRING 'WZ11OUTW/WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY            
034891       DELIMITED BY SIZE INTO ERROR-TEXT                                  
034892       CALL ABEND USING RCODE-ABEND-WITH-DUMP                             
034893     END-IF                                                               
034894                                                                          
044100     .                                                                    
