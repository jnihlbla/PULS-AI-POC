000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WZ11OUTF.                                                
000300 AUTHOR.         ANDRE KJELL.                                             
000400 DATE-WRITTEN.   02/09/02.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        THIS PROGRAM HANDLES OUTPUT TO FAX                               
000900*        IT USES THE GENERAL MAIL MODULE WZ11OUTM                         
001000*        TO DO MOST OF THE JOB.                                           
001100                                                                          
001200     EJECT                                                                
001300 DATA DIVISION.                                                           
001400     SKIP3                                                                
001500 WORKING-STORAGE SECTION.                                                 
001600 77  IDPGM                     PIC X(08)   VALUE 'WZ11OUTF'.              
001700 77  ERROR-TEXT                PIC X(80).                                 
001800 77  KDRC-DISPLAY              PIC Z(5).                                  
001900                                                                          
002000 01  INDX                      PIC S9(4)   BINARY.                        
002100 01  INDX2                     PIC S9(4)   BINARY.                        
002200                                                                          
002300 01  RCODE-ABEND-NO-DUMP       PIC S9(4)   BINARY  VALUE +16.             
002400 01  RCODE-ABEND-WITH-DUMP     PIC S9(4)   BINARY  VALUE +1000.           
002500                                                                          
002600 01  SUBPROGRAMS.                                                         
002700     03  WZ11OUTM              PIC X(8)    VALUE 'WZ11OUTM'.              
002800     03  ABEND                 PIC X(8)    VALUE 'ABEND   '.              
002900     EJECT                                                                
003000 01  OUTM-AREA-START           PIC X(16)   VALUE                          
003100                               'OUTM-AREA-START '.                        
003200*01  -COPY  WZ11OUTM                                                      
003300     EJECT                                                                
003400 LINKAGE SECTION.                                                         
003500*01  -COPY  WZ11OUTF                                                      
003600     EJECT                                                                
003700 PROCEDURE DIVISION  USING OUTF-WZ11OUT.                                  
003800 MAIN SECTION.                                                            
003900                                                                          
004000                                                                          
004100     PERFORM A-INIT                                                       
004200     EVALUATE OUTF-KDFUNC                                                 
004300       WHEN 'OPEN'   PERFORM B-OPEN-CHANNEL                               
004400       WHEN 'PUT'    PERFORM C-WRITE-ONE-RECORD                           
004500       WHEN 'CLOSE'  PERFORM D-CLOSE-CHANNEL                              
004600     END-EVALUATE                                                         
004700                                                                          
004800     GOBACK                                                               
004900     .                                                                    
005000     EJECT                                                                
005100 A-INIT SECTION.                                                          
005200                                                                          
005300     IF OUTF-IDCALL < 1 OR > 15                                           
005400       MOVE OUTF-IDCALL TO KDRC-DISPLAY                                   
005500       STRING 'WZ11OUTF INVALID IDCALL VALUE: '                           
005600              KDRC-DISPLAY                                                
005700          DELIMITED BY SIZE                                               
005800          INTO ERROR-TEXT                                                 
005900       CALL ABEND USING RCODE-ABEND-WITH-DUMP                             
006000     END-IF                                                               
006100                                                                          
006200*    -- INITIALIZE GENERAL ARGUMENTS TO THE MAIL MODULE (OUTM)            
006300     MOVE OUTF-IDCALL         TO OUTM-IDCALL                              
006400     MOVE OUTF-KDFUNC         TO OUTM-KDFUNC                              
006500                                                                          
006600*    -- SO FAR, SO GOOD                                                   
006700     MOVE ZERO TO OUTF-KDRC                                               
006800     .                                                                    
006900                                                                          
007000     EJECT                                                                
007100 B-OPEN-CHANNEL SECTION.                                                  
007200                                                                          
007300*    -- SEND THIS INFO, TO USE IN MAIL TEXT WHEN THERE                    
007400*    -- THERE IS A PROBLEM IN THE DISTRIBUTION                            
007500     MOVE OUTF-IDOUTTYPE      TO OUTM-IDOUTTYPE                           
007600     MOVE OUTF-IDOUTREC       TO OUTM-IDOUTREC                            
007700     MOVE OUTF-IDLIST         TO OUTM-IDLIST                              
007800     MOVE OUTF-TIREGDAT       TO OUTM-TIREGDAT                            
007900     MOVE OUTF-TIKLOCK        TO OUTM-TIKLOCK                             
008000                                                                          
008100*    -- INITIALIZE OPEN ARGUMENTS TO THE MAIL MODULE (OUTM)               
008200     MOVE SPACE               TO OUTM-IDOUTDEST                           
008300     STRING OUTF-IDOUTDEST    DELIMITED BY SPACE                          
008400           '@fax.volvo.com'   DELIMITED BY SIZE                           
008500                              INTO OUTM-IDOUTDEST                         
008600     MOVE OUTF-IDPFDEF        TO OUTM-IDPFDEF                             
008700     MOVE OUTF-FLCARRCNTL     TO OUTM-FLCARRCNTL                          
008800                                                                          
008900*    -- USE FIRST FAX INFO LINE AS TITLE AND SHIFT THE REST               
009000*    -- UP ONE STEP                                                       
009100     MOVE OUTF-TEFAX(1)       TO  OUTM-IDMAILTTL                          
009200     MOVE 1 TO INDX                                                       
009300     PERFORM 4 TIMES                                                      
009400       COMPUTE INDX2 = INDX + 1                                           
009500       MOVE OUTF-TEFAX(INDX2) TO  OUTM-TEFAX(INDX)                        
009600       ADD 1 TO INDX                                                      
009700     END-PERFORM                                                          
009800     MOVE SPACE               TO  OUTM-TEFAX(5)                           
009900                                                                          
010000     MOVE OUTF-IDMAIL-SENDER  TO OUTM-IDMAIL-SENDER                       
010100                                                                          
010200*    -- LET OUTM DO THE JOB                                               
010300     CALL WZ11OUTM  USING OUTM-WZ11OUT                                    
010400     .                                                                    
010500     EJECT                                                                
010600 C-WRITE-ONE-RECORD  SECTION.                                             
010700                                                                          
010800*    When the record is meta data, dont process it. It's not              
010900*    needed here.                                                         
011000     IF OUTF-TEOUTDATA (1:5) = '¤META' OR                                 
011100        OUTF-TEOUTDATA (2:5) = '¤META'                                    
011200       CONTINUE                                                           
011300     ELSE                                                                 
011400*    -- INITIALIZE PUT ARGUMENTS TO THE MAILT MODULE (OUTM)               
011500       MOVE OUTF-PUT-PARAMETERS TO OUTM-PUT-PARAMETERS                    
011600                                                                          
011700*    -- LET OUTM DO THE JOB                                               
011800       CALL WZ11OUTM USING OUTM-WZ11OUT                                   
011900     END-IF                                                               
012000     .                                                                    
012100     EJECT                                                                
012200 D-CLOSE-CHANNEL     SECTION.                                             
012300                                                                          
012400*    -- LET OUTN DO THE JOB                                               
012500     CALL WZ11OUTM  USING OUTM-WZ11OUT                                    
012600     .                                                                    
