000100 ID DIVISION.                                                             
000200* WY2000                                                                  
000300 PROGRAM-ID.     WZ11OUTV.                                                
000400 AUTHOR.         ANDRE KJELL.                                             
000500 DATE-WRITTEN.   02/12/12.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNCTION:                                                            
000900*        THIS PROGRAM HANDLES DISTRIBUTION VIA VCOM                       
001000*        1-15 TRANSMISSIONS IN PARALLELL CAN BE HANDLED.                  
001100                                                                          
001200     EJECT                                                                
001300 DATA DIVISION.                                                           
001400     SKIP3                                                                
001500 WORKING-STORAGE SECTION.                                                 
001600 77  IDPGM                     PIC X(08)   VALUE 'WZ11OUTV'.              
001700                                                                          
001800 01  SMALL-LETTERS              PIC X(31)  VALUE                          
001900     'abcdefghijklmnopqrstuvwxyzÂ‰ˆ¸È'.                                   
002000 01  CAPS-LETTERS               PIC X(31)  VALUE                          
002100     'ABCDEFGHIJKLMNOPQRSTUVWXYZ≈ƒ÷‹…'.                                   
002200                                                                          
002300*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND                
002400 77  ERROR-TEXT-START          PIC X(8)  VALUE 'ERROR:  '.                
002500 77  ERROR-TEXT                PIC X(80) VALUE SPACE.                     
002600 77  KDRC-DISPLAY              PIC -(4)9.                                 
002700                                                                          
002800 01  SUBPROGRAMS.                                                         
002900     03 ABEND                  PIC X(8)    VALUE 'ABEND   '.              
003000     03 DSCONS                 PIC X(8)    VALUE 'DSCONS  '.              
003100     03 DSSEND                 PIC X(8)    VALUE 'DSSEND  '.              
003200     03 DSRLSE                 PIC X(8)    VALUE 'DSRLSE  '.              
003300                                                                          
003400*    -- PARAMETERS TO ABEND                                               
003500 77  RCODE-ABEND-NO-DUMP       PIC S9(4)   BINARY VALUE +16.              
003600 77  RCODE-ABEND-WITH-DUMP     PIC S9(4)   BINARY VALUE +1000.            
003700                                                                          
003800     EJECT                                                                
003900 01  VCOM-AREA-START           PIC X(16)   VALUE                          
004000                               'VCOM-AREA-START '.                        
004100*01  -COPY W0028 -PRE VCOM-                                               
004200                                                                          
004300     EJECT                                                                
004400*    -- SAVED DATA FOR EACH OUTPUT CHANNEL                                
004500 01  W-CHANNEL-TAB.                                                       
004600   03  FILLER        OCCURS 15.                                           
004700     05  W-CHANNEL-DISTID      PIC X(34).                                 
004800     EJECT                                                                
004900 LINKAGE SECTION.                                                         
005000*01  -COPY  WZ11OUTV                                                      
005100     EJECT                                                                
005200 PROCEDURE DIVISION  USING OUTV-WZ11OUT.                                  
005300 MAIN SECTION.                                                            
005400                                                                          
005500     PERFORM A-INIT                                                       
005600     EVALUATE OUTV-KDFUNC                                                 
005700       WHEN 'OPEN'   PERFORM B-OPEN-CHANNEL                               
005800       WHEN 'PUT'    PERFORM C-SEND-ONE-RECORD                            
005900       WHEN 'CLOSE'  PERFORM D-CLOSE-CHANNEL                              
006000     END-EVALUATE                                                         
006100                                                                          
006200     MOVE OUTV-KDRC TO RETURN-CODE                                        
006300     GOBACK                                                               
006400     .                                                                    
006500     EJECT                                                                
006600 A-INIT SECTION.                                                          
006700                                                                          
006800     IF OUTV-IDCALL < 1 OR > 15                                           
006900       MOVE OUTV-IDCALL TO KDRC-DISPLAY                                   
007000       STRING 'WZ11OUTV INVALID IDCALL VALUE: '                           
007100              KDRC-DISPLAY                                                
007200          DELIMITED BY SIZE                                               
007300          INTO ERROR-TEXT                                                 
007400       CALL ABEND USING RCODE-ABEND-WITH-DUMP                             
007500     END-IF                                                               
007600                                                                          
007700*    -- FETCH THE VCOM DISTRIBUTION ID SAVED AT OPEN TIME,                
007800*    -- AND USE IT IN LATER CALLS                                         
007900     IF OUTV-KDFUNC NOT = 'OPEN'                                          
008000       MOVE W-CHANNEL-DISTID(OUTV-IDCALL) TO VCOM-DISTID                  
008100     END-IF                                                               
008200                                                                          
008300*    -- SO FAR, SO GOOD                                                   
008400     MOVE ZERO TO OUTV-KDRC                                               
008500     .                                                                    
008600     EJECT                                                                
008700 B-OPEN-CHANNEL SECTION.                                                  
008800                                                                          
008900     INSPECT OUTV-IDOUTDEST CONVERTING                                    
009000             SMALL-LETTERS TO CAPS-LETTERS                                
009100     MOVE OUTV-IDOUTDEST TO VCOM-PARTNER                                  
009200                                                                          
009300     INSPECT OUTV-TEVCOMST CONVERTING                                     
009400            SMALL-LETTERS TO CAPS-LETTERS                                 
009500     MOVE OUTV-TEVCOMST  TO VCOM-SENDERTAG                                
009600                                                                          
009700     INSPECT OUTV-IDVCINIT CONVERTING                                     
009800            SMALL-LETTERS TO CAPS-LETTERS                                 
009900     MOVE OUTV-IDVCINIT  TO VCOM-INITIATOR                                
010000                                                                          
010100     MOVE SPACE          TO VCOM-RECEIPT-PARTNER                          
010200     MOVE ZERO           TO VCOM-RECEIPT-RC                               
010300     MOVE ZERO           TO VCOM-RECEIPT-LENGTH                           
010400     MOVE SPACE          TO VCOM-RECEIPT-DATA                             
010500     MOVE 'N'            TO VCOM-PRIO                                     
010600                                                                          
010700     CALL DSCONS USING VCOM-RC                                            
010800                       VCOM-DISTID                                        
010900                       VCOM-SECUR                                         
011000                       VCOM-TIMEOUT                                       
011100                       VCOM-SENDERTAG                                     
011200                       VCOM-PARTNER                                       
011300                       VCOM-RECEIPT                                       
011400                       VCOM-PRIO                                          
011500                       VCOM-INITIATOR                                     
011600                                                                          
011700     IF VCOM-RC NOT = ZERO                                                
011800       MOVE VCOM-RC TO KDRC-DISPLAY                                       
011900       STRING 'WZ11OUTV RC FROM DSCONS: ' KDRC-DISPLAY                    
012000              ' ' VCOM-DISTID                                             
012100              ' ' VCOM-PARTNER                                            
012200              ' ' VCOM-SENDERTAG                                          
012300          DELIMITED BY SIZE                                               
012400          INTO ERROR-TEXT                                                 
012500       DISPLAY ERROR-TEXT                                                 
012600       MOVE 8 TO OUTV-KDRC                                                
012700       STRING 'OPEN RC ' KDRC-DISPLAY '      '                            
012800          DELIMITED BY SIZE INTO OUTV-TEOUTDATA                           
012900     END-IF                                                               
013000                                                                          
013100     MOVE VCOM-DISTID TO W-CHANNEL-DISTID(OUTV-IDCALL)                    
013200     .                                                                    
013300     EJECT                                                                
013400 C-SEND-ONE-RECORD  SECTION.                                              
013500                                                                          
013600*    When the record is meta data, dont process it. It's not              
013700*    needed here.                                                         
013800     IF OUTV-TEOUTDATA (1:5) = '§META' OR                                 
013900        OUTV-TEOUTDATA (2:5) = '§META'                                    
014000       CONTINUE                                                           
014100     ELSE                                                                 
014200       MOVE OUTV-TEOUTDATA-L     TO VCOM-ACTLENGTH                        
014300       MOVE OUTV-TEOUTDATA(1:VCOM-ACTLENGTH)                              
014400                                 TO VCOM-DATA                             
014500                                                                          
014600       CALL DSSEND USING VCOM-RC                                          
014700                         VCOM-DISTID                                      
014800                         VCOM-ACTLENGTH                                   
014900                         VCOM-DATA                                        
015000                                                                          
015100       IF VCOM-RC NOT = ZERO                                              
015200         MOVE VCOM-RC            TO KDRC-DISPLAY                          
015300         STRING 'WZ11OUTV RC FROM DSSEND: ' KDRC-DISPLAY                  
015400                ' ' VCOM-DISTID                                           
015500                ' ' VCOM-PARTNER                                          
015600                ' ' VCOM-SENDERTAG                                        
015700            DELIMITED BY SIZE                                             
015800                               INTO ERROR-TEXT                            
015900         DISPLAY ERROR-TEXT                                               
016000         STRING 'SEND RC ' KDRC-DISPLAY '      '                          
016100             DELIMITED BY SIZE INTO OUTV-TEOUTDATA                        
016200         MOVE 8                  TO OUTV-KDRC                             
016300**       CALL ABEND USING RCODE-ABEND-WITH-DUMP                           
016400       END-IF                                                             
016500     END-IF                                                               
016600     .                                                                    
016700     EJECT                                                                
016800 D-CLOSE-CHANNEL     SECTION.                                             
016900                                                                          
017000     MOVE ZERO                        TO VCOM-RC                          
017100     MOVE +1                          TO VCOM-RVALUE                      
017200                                                                          
017300     CALL DSRLSE USING VCOM-RC                                            
017400                       VCOM-DISTID                                        
017500                       VCOM-RVALUE                                        
017600                                                                          
017700     IF VCOM-RC NOT = ZERO                                                
017800       MOVE VCOM-RC TO KDRC-DISPLAY                                       
017900       STRING 'WZ11OUTV RC FROM DSRLSE: ' KDRC-DISPLAY                    
018000              ' ' VCOM-DISTID                                             
018100              ' ' VCOM-PARTNER                                            
018200              ' ' VCOM-SENDERTAG                                          
018300          DELIMITED BY SIZE                                               
018400          INTO ERROR-TEXT                                                 
018500       DISPLAY ERROR-TEXT                                                 
018600**     CALL ABEND USING RCODE-ABEND-WITH-DUMP                             
018700     END-IF                                                               
018800     .                                                                    
