000100*COMPOPT AMODE=ANY                                                        
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.                     W0161300.                                
000400 ENVIRONMENT DIVISION.                                                    
000500 CONFIGURATION SECTION.                                                   
000600 SPECIAL-NAMES.                                                           
000700         C01 IS NYSIDA.                                                   
000800 INPUT-OUTPUT SECTION.                                                    
000900 FILE-CONTROL.                                                            
001000         SELECT W016JOB-REG ASSIGN UT-S-W01613D1.                         
001100         SELECT W016INT-REG ASSIGN UT-S-W01613D2.                         
001200         SELECT W016PRM-REG ASSIGN UT-S-W01613D3.                         
001300 DATA DIVISION.                                                           
001400 FILE SECTION.                                                            
001500                                                                          
001600                                                                          
001700 FD      W016JOB-REG         BLOCK 0                                      
001800                             RECORDING F                                  
001900                             LABEL RECORD STANDARD.                       
002000 01      W016JOB-REC         PIC X(80).                                   
002100                                                                          
002200                                                                          
002300 FD      W016INT-REG         BLOCK 0                                      
002400                             RECORDING F                                  
002500                             LABEL RECORD STANDARD.                       
002600 01      W016INT-REC         PIC X(80).                                   
002700                                                                          
002800                                                                          
002900 FD      W016PRM-REG         BLOCK 0                                      
003000                             RECORDING F                                  
003100                             LABEL RECORD STANDARD.                       
003200 01      W016PRM-REC         PIC X(80).                                   
003300                                                                          
003400                                                                          
003500                                                                          
003600 EJECT                                                                    
003700 WORKING-STORAGE SECTION.                                                 
003800                                                                          
003900*    -- CHECKED BY WY2000                                                 
004000 01      W.                                                               
004100  02     FILLER              PIC X(10)   VALUE 'W O R K I '.              
004200  02     FILLER              PIC X(10)   VALUE 'N G   S T '.              
004300  02     FILLER              PIC X(10)   VALUE 'O R A G E '.              
004400  02     FILLER              PIC X(10)   VALUE '1234567890'.              
004500  02     W-PROGNAMN          PIC X(8)    VALUE 'W0161300'.                
004600  02     W-RETURN-CODE       PIC S9(4)   VALUE +0      COMP.              
004700  02     W-VILKEN-SECTION    PIC X(20)   VALUE ' '.                       
004800  02     W-REDIG-RC          PIC 9(4)    VALUE ZERO.                      
004900  02     W-LRECL             PIC 9(4)    VALUE 0       COMP SYNC.         
005000  02     W-CRTIME.                                                        
005100   03    W-DATE              PIC X(6)    VALUE ' '.                       
005200   03    W-TIME              PIC X(8)    VALUE ' '.                       
005300  02     W-SOPROUTINE        PIC X(8)    VALUE ' '.                       
005400  02     W-SOPPARM           PIC X(30)   VALUE ' '.                       
005500  02     W-SOPTEXT           PIC X(20)   VALUE ' '.                       
005600  02     W-RED-RETCODE       PIC ---9.                                    
005700  02     W-RED-MSGCODE       PIC ---9.                                    
005800                                                                          
005900  02     W-SENDERTAG.                                                     
006000   03    W-CREATE-DSN        PIC X(20).                                   
006100                                                                          
006200  02     W-DSN.                                                           
006300   03    W-DSN-1             PIC X(20).                                   
006400   03    W-DSN-2             PIC X(20).                                   
006500   03    W-DSN-3             PIC X(20).                                   
006600   03    W-DSN-4             PIC X(20).                                   
006700   03    W-DSN-5             PIC X(20).                                   
006800   03    W-DSN-6             PIC X(20).                                   
006900   03    W-DSN-7             PIC X(20).                                   
007000  02     W-VAR.                                                           
007100   03    W-VAR-1             PIC S9(3)                COMP-3.             
007200   03    W-VAR-2             PIC S9(3)                COMP-3.             
007300   03    W-VAR-3             PIC S9(3)                COMP-3.             
007400   03    W-VAR-4             PIC S9(3)                COMP-3.             
007500   03    W-VAR-5             PIC S9(3)                COMP-3.             
007600   03    W-VAR-6             PIC S9(3)                COMP-3.             
007700   03    W-VAR-7             PIC S9(3)                COMP-3.             
007800   03    W-TALLY             PIC S9(3)                COMP-3.             
007900                                                                          
008000  02     W-W016RCV-GEN       PIC X(44)   VALUE ' '.                       
008100  02     W-WLOCATE-GEN       PIC 9(4)    VALUE ZERO.                      
008200*                                                                         
008300 01      SW.                                                              
008400  02     SW-CORRECT-DSNAME   PIC X.                                       
008500*                                                                         
008600 01      I.                                                               
008700  02     JCLMEMO-IX          PIC S9(9)   VALUE +0      COMP.              
008800  02     JCLMEMO-IXANT       PIC S9(9)   VALUE +0      COMP.              
008900  02     JCLMEMO-IXMAX       PIC S9(9)   VALUE +30     COMP.              
009000  02     JCLSUBM-IX          PIC S9(9)   VALUE +0      COMP.              
009100  02     JCLSUBM-IXANT       PIC S9(9)   VALUE +0      COMP.              
009200  02     JCLSUBM-IXMAX       PIC S9(9)   VALUE +9      COMP.              
009300 EJECT                                                                    
009400 01      DYNAMISKA-SUBPGM.                                                
009500  02     ABEND               PIC X(8)    VALUE 'ABEND   '.                
009600  02     DSCONR              PIC X(8)    VALUE 'DSCONR  '.                
009700  02     DSRECV              PIC X(8)    VALUE 'DSRECV  '.                
009800  02     DSRLSE              PIC X(8)    VALUE 'DSRLSE  '.                
009900  02     WLOCATE             PIC X(8)    VALUE 'WLOCATE '.                
010000  02     WLISTDSI            PIC X(8)    VALUE 'WLISTDSI'.                
010100  02     WDYNALC             PIC X(8)    VALUE 'WDYNALC '.                
010200  02     WFILWRT             PIC X(8)    VALUE 'WFILWRT '.                
010300  02     W980SOP             PIC X(8)    VALUE 'W980SOP '.                
010400*                                                                         
010500 01      PARMWS.                                                          
010600  02     PARM-EXPEDITER.                                                  
010700   03    PARM-EXP-SYS-PREFIX PIC X(4)    VALUE ' '.                       
010800   03    FILLER              PIC X(4)    VALUE ' '.                       
010900  02     PARM-GRP            PIC X(4)    VALUE ' '.                       
011000  02     PARM-RLSE-RVALUE    PIC X(2)    VALUE ' '.                       
011100*                                                                         
011200 01      DYN-PARM.                                                        
011300  02     DYN-INIT            PIC X       VALUE 'I'.                       
011400  02     DYN-OPEN            PIC X       VALUE 'O'.                       
011500  02     DYN-WRITE           PIC X       VALUE ' '.                       
011600  02     DYN-CLOSE           PIC X       VALUE 'C'.                       
011700  02     DYN-RCVFILE         PIC S9(9)                 COMP SYNC.         
011800  02     DYN-DATA            PIC X(4096).                                 
011900  02     DYN-WRITE-RECFM     PIC X       VALUE 'V'.                       
012000  02     DYN-WRITE-LRECL     PIC S9(4)   VALUE +0      COMP SYNC.         
012100*                                                                         
012200 01      EOF.                                                             
012300  02     EOF-W016PRM         PIC X       VALUE 'N'.                       
012400  02     EOF-W016JOB         PIC X       VALUE 'N'.                       
012500*                                                                         
012600 01      WLOCATE-PARM.                                                    
012700  02     WLOCATE-INDSN       PIC X(44)   VALUE ' '.                       
012800  02     WLOCATE-UTDSN       PIC X(44)   VALUE ' '.                       
012900*                                                                         
013000 01      WLISTDSI-PARM.                                                   
013100  02     WLISTDSI-INDSN      PIC X(44).                                   
013200  02     WLISTDSI-UTDSN      PIC X(44)   VALUE ' '.                       
013300  02     WLISTDSI-CREYYYY    PIC X(4)    VALUE ' '.                       
013400  02     WLISTDSI-CREDDD     PIC X(3)    VALUE ' '.                       
013500  02     WLISTDSI-EXPYYYY    PIC X(4)    VALUE ' '.                       
013600  02     WLISTDSI-EXPDDD     PIC X(3)    VALUE ' '.                       
013700  02     WLISTDSI-REFYYYY    PIC X(4)    VALUE ' '.                       
013800  02     WLISTDSI-REFDDD     PIC X(3)    VALUE ' '.                       
013900  02     WLISTDSI-RC         PIC S9(9)   VALUE ZERO    COMP.              
014000*                                                                         
014100* --- PARAMETRAR TILL W980SOP                                             
014200 01      W980SOP-AREA-START  PIC X(24)   VALUE                            
014300                                         'W980SOP-AREA-START'.            
014400*01  -COPY WSOPAREA                                                       
014500*                                                                         
014600 01      ABEND-PARM.                                                      
014700  02     ABEND-CODE          PIC S9(4)   VALUE +0      COMP SYNC.         
014800*                                                                         
014900  02     ABEND-RUBRIK.                                                    
015000   03    FILLER              PIC X(16)   VALUE '--------->     *'.        
015100   03    FILLER              PIC X(16)   VALUE '*** A B E N D **'.        
015200   03    FILLER              PIC X(16)   VALUE '**     <--------'.        
015300*                                                                         
015400  02     ABEND-RAD.                                                       
015500   03    FILLER              PIC X(16)   VALUE ' DUMPNUMMER---->'.        
015600   03    ABEND-DUMPNR        PIC X(2)    VALUE ' '.                       
015700   03    FILLER              PIC X(160)  VALUE ' '.                       
015800 EJECT                                                                    
015900*                                                                         
016000 01      W016JOB.                                                         
016100  02     W016JOB-STEG        PIC X(10).                                   
016200  02     FILLER              REDEFINES W016JOB-STEG.                      
016300   03    FILLER              PIC X(9).                                    
016400   03    W016JOB-POS10       PIC X.                                       
016500  02     FILLER              PIC X(70).                                   
016600*                                                                         
016700 01      W016INT.                                                         
016800  02     FILLER              PIC X(80).                                   
016900*                                                                         
017000 01      W016REN.                                                         
017100  02     FILLER              PIC X(80).                                   
017200*                                                                         
017300 01      LOGG.                                                            
017400  02     LOGG-W016PRM        PIC S9(9)   VALUE +0         COMP.           
017500  02     LOGG-W016RCV        PIC S9(9)   VALUE +0         COMP.           
017600  02     LOGG-W016JOB        PIC S9(9)   VALUE +0         COMP.           
017700  02     LOGG-W016INT        PIC S9(9)   VALUE +0         COMP.           
017800  02     LOGG-W016REN        PIC S9(9)   VALUE +0         COMP.           
017900  02     LOGG-RCVFILE        PIC S9(9)   VALUE +0         COMP.           
018000  02     LOGG-W016DYN        PIC S9(9)   VALUE +0         COMP.           
018100 EJECT                                                                    
018200         -COPY WDYNAREA                                                   
018300 EJECT                                                                    
018400         -COPY W01613P                                                    
018500*                                                                         
018600 EJECT                                                                    
018700 01      JCLMEMO.                                                         
018800  02     JCLMEMO-TAB.                                                     
018900   03    FILLER              PIC X(15)   VALUE '//MEMO        E'.         
019000   03    FILLER              PIC X(15)   VALUE 'XEC      PGM=SM'.         
019100   03    FILLER              PIC X(50)   VALUE 'TPAPIX         '.         
019200   03    FILLER              PIC X(15)   VALUE '//APIPARM     D'.         
019300   03    FILLER              PIC X(15)   VALUE 'D      DSN=F1ST'.         
019400   03    FILLER              PIC X(15)   VALUE '&VX..MEMOAPI.PA'.         
019500   03    FILLER              PIC X(35)   VALUE 'RMLIB,DISP=SHR '.         
019600   03    FILLER              PIC X(15)   VALUE '//APIFILE     D'.         
019700   03    FILLER              PIC X(65)   VALUE 'D        *     '.         
019800   03    FILLER              PIC X(80)   VALUE ')SEND          '.         
019900   03    FILLER              PIC X(7)    VALUE ' TITLE '.                 
020000   03    JCLMEMO-TITLE       PIC X(14)   VALUE 'VCOM          '.          
020100   03    FILLER              PIC X(59)   VALUE ' '.                       
020200   03    FILLER              PIC X(80)   VALUE ' '.                       
020300   03    FILLER              PIC X(80)   VALUE ' '.                       
020400   03    FILLER              PIC X(80)   VALUE ' '.                       
020500   03    FILLER              PIC X(80)   VALUE ' '.                       
020600   03    FILLER              PIC X(80)   VALUE ' '.                       
020700   03    FILLER              PIC X(80)   VALUE ' '.                       
020800   03    FILLER              PIC X(80)   VALUE ' '.                       
020900   03    FILLER              PIC X(80)   VALUE ' '.                       
021000   03    FILLER              PIC X(80)   VALUE ' '.                       
021100   03    FILLER              PIC X(80)   VALUE ' '.                       
021200   03    FILLER              PIC X(80)   VALUE ' '.                       
021300   03    FILLER              PIC X(80)   VALUE ' '.                       
021400   03    FILLER              PIC X(80)   VALUE ' '.                       
021500   03    FILLER              PIC X(80)   VALUE ' '.                       
021600   03    FILLER              PIC X(80)   VALUE ' '.                       
021700   03    FILLER              PIC X(80)   VALUE ' '.                       
021800   03    FILLER              PIC X(80)   VALUE ' '.                       
021900   03    FILLER              PIC X(80)   VALUE ' '.                       
022000   03    FILLER              PIC X(80)   VALUE ' '.                       
022100   03    FILLER              PIC X(80)   VALUE ' '.                       
022200   03    FILLER              PIC X(80)   VALUE ' '.                       
022300   03    FILLER              PIC X(80)   VALUE ' '.                       
022400   03    FILLER              PIC X(80)   VALUE ' '.                       
022500   03    FILLER              PIC X(80)   VALUE ' '.                       
022600   03    FILLER              PIC X(80)   VALUE ' '.                       
022700  02     FILLER              REDEFINES JCLMEMO-TAB.                       
022800   03    JCLMEMO-KORT        PIC X(80)   OCCURS 30.                       
022900*                                                                         
023000 01      JCLSUBM.                                                         
023100  02     JCLSUBM-TAB.                                                     
023200   03    FILLER              PIC X(15)   VALUE '//IEBGENER    E'.         
023300   03    FILLER              PIC X(15)   VALUE 'XEC      PGM=IE'.         
023400   03    FILLER              PIC X(50)   VALUE 'BGENER         '.         
023500   03    FILLER              PIC X(80)   VALUE '//SYSUT1      D'.         
023600   03    FILLER              PIC X(15)   VALUE '//SYSUT2      D'.         
023700   03    FILLER              PIC X(15)   VALUE 'D        SYSOUT'.         
023800   03    FILLER              PIC X(50)   VALUE '=(A,INTRDR)    '.         
023900   03    FILLER              PIC X(15)   VALUE '//SYSPRINT    D'.         
024000   03    FILLER              PIC X(65)   VALUE 'D        DUMMY '.         
024100   03    FILLER              PIC X(15)   VALUE '//SYSIN       D'.         
024200   03    FILLER              PIC X(65)   VALUE 'D        DUMMY '.         
024300   03    FILLER              PIC X(80)   VALUE '//*            '.         
024400   03    FILLER              PIC X(15)   VALUE '//IEBGENRC    E'.         
024500   03    FILLER              PIC X(15)   VALUE 'XEC      VRCABE'.         
024600   03    FILLER              PIC X(15)   VALUE 'ND,COND=(0,GE,I'.         
024700   03    FILLER              PIC X(35)   VALUE 'EBGENER)       '.         
024800   03    FILLER              PIC X(80)   VALUE '//*            '.         
024900   03    FILLER              PIC X(80)   VALUE '//*            '.         
025000  02     FILLER              REDEFINES JCLSUBM-TAB.                       
025100   03    JCLSUBM-KORT        PIC X(80)   OCCURS 9.                        
025200*                                                                         
025300 EJECT                                                                    
025400 01      MEMOTX.                                                          
025500  02     MEMOTX-RAD1.                                                     
025600   03    FILLER              PIC X(15)   VALUE '     VCOM has d'.         
025700   03    FILLER              PIC X(15)   VALUE 'elivered a file'.         
025800   03    FILLER              PIC X(15)   VALUE ' and VCOM recei'.         
025900   03    FILLER              PIC X(20)   VALUE 've pgm was not '.         
026000  02     MEMOTX-RAD2.                                                     
026100   03    FILLER              PIC X(15)   VALUE '     able to de'.         
026200   03    FILLER              PIC X(15)   VALUE 'termine wich fi'.         
026300   03    FILLER              PIC X(15)   VALUE 'nal dataset to '.         
026400   03    FILLER              PIC X(20)   VALUE 'write to !     '.         
026500  02     MEMOTX-RAD3.                                                     
026600   03    FILLER              PIC X(15)   VALUE '     Therefore,'.         
026700   03    FILLER              PIC X(15)   VALUE ' rename must be'.         
026800   03    FILLER              PIC X(15)   VALUE ' done to the pr'.         
026900   03    FILLER              PIC X(15)   VALUE 'oper (final dat'.         
027000   03    FILLER              PIC X(5)    VALUE 'aset)'.                   
027100  02     MEMOTX-RAD4.                                                     
027200   03    FILLER              PIC X(15)   VALUE '     The follow'.         
027300   03    FILLER              PIC X(15)   VALUE 'ing statements '.         
027400   03    FILLER              PIC X(15)   VALUE 'will help you t'.         
027500   03    FILLER              PIC X(20)   VALUE 'o rename       '.         
027600  02     MEMOTX-RAD5.                                                     
027700   03    FILLER              PIC X(65)   VALUE ' '.                       
027800  02     MEMOTX-RAD6.                                                     
027900   03    FILLER              PIC X(15)   VALUE '     Received s'.         
028000   03    FILLER              PIC X(15)   VALUE 'endertag   --> '.         
028100   03    MEMOTX-SENDERTAG    PIC X(35)   VALUE '               '.         
028200  02     MEMOTX-RAD7.                                                     
028300   03    FILLER              PIC X(15)   VALUE '     File to be'.         
028400   03    FILLER              PIC X(15)   VALUE ' renamed   --> '.         
028500   03    MEMOTX-W016RCV-GEN  PIC X(35)   VALUE '               '.         
028600  02     MEMOTX-RAD8.                                                     
028700   03    FILLER              PIC X(15)   VALUE '     VCOM distr'.         
028800   03    FILLER              PIC X(15)   VALUE 'ibution id --> '.         
028900   03    MEMOTX-DISTID       PIC X(35)   VALUE '               '.         
029000  02     MEMOTX-RAD9.                                                     
029100   03    FILLER              PIC X(15)   VALUE '     VCOM exped'.         
029200   03    FILLER              PIC X(15)   VALUE 'iter       --> '.         
029300   03    MEMOTX-EXPEDITER    PIC X(35)   VALUE '               '.         
029400  02     MEMOTX-RAD10.                                                    
029500   03    FILLER              PIC X(65)   VALUE ' '.                       
029600  02     MEMOTX-RAD11.                                                    
029700   03    FILLER              PIC X(15)   VALUE '               '.         
029800   03    FILLER              PIC X(15)   VALUE '               '.         
029900   03    FILLER              PIC X(15)   VALUE '               '.         
030000   03    FILLER              PIC X(15)   VALUE '               '.         
030100   03    FILLER              PIC X(5)    VALUE '     '.                   
030200                                                                          
030300                                                                          
030400 01      memook.                                                          
030500  02     MEMOOK-RAD1.                                                     
030600   03    FILLER              PIC X(15)   VALUE '     VCOM has d'.         
030700   03    FILLER              PIC X(15)   VALUE 'elivered a file'.         
030800   03    FILLER              PIC X(15)   VALUE ' to the dataset'.         
030900   03    FILLER              PIC X(20)   VALUE ':              '.         
031000  02     MEMOOK-RAD2.                                                     
031100   03    FILLER              PIC X(15)   VALUE '               '.         
031200   03    FILLER              PIC X(15)   VALUE '               '.         
031300   03    FILLER              PIC X(15)   VALUE '               '.         
031400   03    FILLER              PIC X(20)   VALUE '               '.         
031500  02     MEMOOK-RAD3.                                                     
031600   03    FILLER              PIC X(15)   VALUE '     File deliv'.         
031700   03    FILLER              PIC X(15)   VALUE 'ered       --> '.         
031800   03    MEMOOK-W016RCV-GEN  PIC X(35)   VALUE '               '.         
031900  02     MEMOOK-RAD4.                                                     
032000   03    FILLER              PIC X(15)   VALUE '     VCOM distr'.         
032100   03    FILLER              PIC X(15)   VALUE 'ibution id --> '.         
032200   03    MEMOOK-DISTID       PIC X(35)   VALUE '               '.         
032300  02     MEMOOK-RAD5.                                                     
032400   03    FILLER              PIC X(15)   VALUE '     VCOM exped'.         
032500   03    FILLER              PIC X(15)   VALUE 'iter       --> '.         
032600   03    MEMOOK-EXPEDITER    PIC X(35)   VALUE '               '.         
032700  02     MEMOOK-RAD6.                                                     
032800   03    FILLER              PIC X(65)   VALUE ' '.                       
032900  02     MEMOOK-RAD7.                                                     
033000   03    FILLER              PIC X(65)   VALUE ' '.                       
033100  02     MEMOOK-RAD8.                                                     
033200   03    FILLER              PIC X(65)   VALUE ' '.                       
033300  02     MEMOOK-RAD9.                                                     
033400   03    FILLER              PIC X(65)   VALUE ' '.                       
033500 EJECT                                                                    
033600         -COPY W01613F                                                    
033700 EJECT                                                                    
033800 LINKAGE SECTION.                                                         
033900*                                                                         
034000 01      PARM.                                                            
034100  02     PARM-LGD            PIC S9(4)                    COMP.           
034200  02     PARM-VARDE          PIC X(100).                                  
034300 EJECT                                                                    
034400 PROCEDURE DIVISION USING PARM.                                           
034500                                                                          
034600         PERFORM A-INIT                                                   
034700                                                                          
034800         PERFORM B-STARTA-VCOM                                            
034900                                                                          
035000         IF VCOM-RC                   = ZERO                              
035100           PERFORM C-PREPARERA-RCVFILE                                    
035200           PERFORM D-HAMTA-FRAN-VCOM                                      
035300           PERFORM UNTIL VCOM-RC      > ZERO                              
035400             IF CONR-RECTYPE          = 'R'                               
035500              STRING 'DETTA PGM BEHANDLAR ENDAST DATA FRÅN VCOM'          
035600                      ' OCH CONR-RECTYPE ÄR EJ "D"'                       
035700                       DELIMITED BY SIZE   INTO ABEND-RAD                 
035800               PERFORM S99-ABEND-UTAN-DUMP                                
035900             ELSE                                                         
036000               PERFORM S14-WRITE-RCVFILE                                  
036100             END-IF                                                       
036200             PERFORM D-HAMTA-FRAN-VCOM                                    
036300           END-PERFORM                                                    
036400           PERFORM E-AVSLUTA-VCOM                                         
036500           IF SW-CORRECT-DSNAME   NOT = 'N'                               
036600             PERFORM F-SUBMIT-JCL-WRITE-MEMO                              
036700           ELSE                                                           
036800             PERFORM G-WRITE-FILEPROBL-MEMO                               
036900           END-IF                                                         
037000         END-IF                                                           
037100                                                                          
037200         PERFORM Z-FINIT                                                  
037300                                                                          
037400         MOVE W-RETURN-CODE  TO RETURN-CODE                               
037500         GOBACK                                                           
037600         .                                                                
037700 EJECT                                                                    
037800 A-INIT SECTION.                                                          
037900         MOVE 'A-INIT'                TO W-VILKEN-SECTION                 
038000         DISPLAY 'S T A R T'                                              
038100         ACCEPT W-DATE                FROM DATE                           
038200         ACCEPT W-TIME                FROM TIME                           
038300         MOVE +0                      TO W-RETURN-CODE                    
038400                                                                          
038500         UNSTRING PARM-VARDE          DELIMITED BY ',' INTO               
038600                                      PARM-EXPEDITER                      
038700                                      PARM-GRP                            
038800                                      PARM-RLSE-RVALUE                    
038900                                                                          
039000         DISPLAY 'PARM-EXPEDITER      -> ' PARM-EXPEDITER                 
039100         DISPLAY 'PARM-GRP            -> ' PARM-GRP                       
039200         DISPLAY 'PARM-RLSE-RVALUE    -> ' PARM-RLSE-RVALUE               
039300                                                                          
039400         PERFORM S11-INIT-RCVFILE                                         
039500                                                                          
039600         OPEN INPUT  W016JOB-REG                                          
039700         OPEN OUTPUT W016INT-REG                                          
039800         OPEN INPUT  W016PRM-REG                                          
039900         .                                                                
040000 EJECT                                                                    
040100 B-STARTA-VCOM SECTION.                                                   
040200         MOVE 'B-STARTA-VCOM'         TO W-VILKEN-SECTION                 
040300                                                                          
040400         MOVE +0                      TO VCOM-RC                          
040500         MOVE PARM-EXPEDITER          TO CONR-EXPEDITER                   
040600         IF PARM-EXPEDITER        NOT > ' '                               
040700           MOVE 'OKÄND'               TO PARM-EXPEDITER                   
040800         END-IF                                                           
040900         DISPLAY 'EXPEDITER           -> ' PARM-EXPEDITER                 
041000                                                                          
041100         CALL DSCONR                  USING VCOM-RC                       
041200                                            VCOM-DISTID                   
041300                                            CONR-SECUR                    
041400                                            CONR-TIMEOUT                  
041500                                            CONR-SENDERTAG                
041600                                            CONR-EXPEDITER                
041700                                            CONR-RECTYPE                  
041800                                                                          
041900         IF  VCOM-RC              NOT = ZERO                              
042000         AND VCOM-RC              NOT = 41                                
042100           MOVE VCOM-RC               TO W-REDIG-RC                       
042200           STRING ' CALL DSCONR GAV RETURKOD -> '                         
042300                  W-REDIG-RC                                              
042400                  DELIMITED BY SIZE   INTO ABEND-RAD                      
042500           PERFORM S99-ABEND-UTAN-DUMP                                    
042600         END-IF                                                           
042700                                                                          
042800         DISPLAY 'VCOM-DISTID         -> ' VCOM-DISTID                    
042900         DISPLAY 'CONR-SENDERTAG      -> ' CONR-SENDERTAG                 
043000         DISPLAY 'CONR-EXPEDITER      -> ' CONR-EXPEDITER                 
043100                                                                          
043200         IF  VCOM-RC              NOT = ZERO                              
043300           DISPLAY 'NOTHING TO RECEIVE  -> ' CONR-EXPEDITER               
043400           DISPLAY '  VCOM-RC           -> ' VCOM-RC                      
043500           MOVE +6                    TO W-RETURN-CODE                    
043600         END-IF                                                           
043700         .                                                                
043800 EJECT                                                                    
043900 C-PREPARERA-RCVFILE SECTION.                                             
044000         MOVE 'C-PREPARERA-RCVFILE'   TO W-VILKEN-SECTION                 
044100         MOVE CONR-SENDERTAG          TO W-SENDERTAG                      
044200         MOVE 'Y'                     TO SW-CORRECT-DSNAME                
044300                                                                          
044400         MOVE +0                      TO W-TALLY                          
044500         INSPECT W-SENDERTAG          TALLYING                            
044600                                      W-TALLY                             
044700                                      FOR ALL '.'                         
044800         IF W-TALLY               NOT > 0                                 
044900           MOVE 'N'                   TO SW-CORRECT-DSNAME                
045000           DISPLAY 'TALLY               -> ' W-TALLY                      
045100         END-IF                                                           
045200                                                                          
045300         MOVE +0                      TO W-VAR-1                          
045400         MOVE +0                      TO W-VAR-2                          
045500         MOVE +0                      TO W-VAR-3                          
045600         MOVE +0                      TO W-VAR-4                          
045700         MOVE +0                      TO W-VAR-5                          
045800         MOVE +0                      TO W-VAR-6                          
045900         MOVE +0                      TO W-VAR-7                          
046000         UNSTRING W-SENDERTAG         DELIMITED BY '.' OR ' '             
046100                                      INTO                                
046200                                      W-DSN-1 COUNT IN W-VAR-1            
046300                                      W-DSN-2 COUNT IN W-VAR-2            
046400                                      W-DSN-3 COUNT IN W-VAR-3            
046500                                      W-DSN-4 COUNT IN W-VAR-4            
046600                                      W-DSN-5 COUNT IN W-VAR-5            
046700                                      W-DSN-6 COUNT IN W-VAR-6            
046800                                      W-DSN-7 COUNT IN W-VAR-7            
046900                                                                          
047000         IF W-VAR-1                   > 8                                 
047100         OR (W-DSN-1 (1:1)        NOT = 'R'                               
047200         AND W-DSN-1 (1:1)        NOT = 'W')                              
047300           MOVE 'N'                   TO SW-CORRECT-DSNAME                
047400           DISPLAY 'W-DSN-1             -> ' W-DSN-1                      
047500         END-IF                                                           
047600         IF W-VAR-2                   > 8                                 
047700         OR W-DSN-2 (1:1)         NOT ALPHABETIC                          
047800           MOVE 'N'                   TO SW-CORRECT-DSNAME                
047900           DISPLAY 'W-DSN-2             -> ' W-DSN-2                      
048000         END-IF                                                           
048100         IF W-DSN-3                   > ' '                               
048200           IF W-VAR-3                 > 8                                 
048300           OR W-DSN-3 (1:1)       NOT ALPHABETIC                          
048400             MOVE 'N'                 TO SW-CORRECT-DSNAME                
048500             DISPLAY 'W-DSN-3             -> ' W-DSN-3                    
048600           END-IF                                                         
048700         END-IF                                                           
048800         IF W-DSN-4                   > ' '                               
048900           IF W-VAR-4                 > 8                                 
049000           OR W-DSN-4 (1:1)       NOT ALPHABETIC                          
049100             MOVE 'N'                 TO SW-CORRECT-DSNAME                
049200             DISPLAY 'W-DSN-4             -> ' W-DSN-4                    
049300           END-IF                                                         
049400         END-IF                                                           
049500         IF W-DSN-5                   > ' '                               
049600           IF W-VAR-5                 > 8                                 
049700           OR W-DSN-5 (1:1)       NOT ALPHABETIC                          
049800             MOVE 'N'                 TO SW-CORRECT-DSNAME                
049900             DISPLAY 'W-DSN-5             -> ' W-DSN-5                    
050000           END-IF                                                         
050100         END-IF                                                           
050200         IF W-DSN-6                   > ' '                               
050300           IF W-VAR-6                 > 8                                 
050400           OR W-DSN-6 (1:1)       NOT ALPHABETIC                          
050500             MOVE 'N'                 TO SW-CORRECT-DSNAME                
050600             DISPLAY 'W-DSN-6             -> ' W-DSN-6                    
050700           END-IF                                                         
050800         END-IF                                                           
050900         IF W-DSN-7                   > ' '                               
051000           IF W-VAR-7                 > 8                                 
051100           OR W-DSN-7 (1:1)       NOT ALPHABETIC                          
051200             MOVE 'N'                 TO SW-CORRECT-DSNAME                
051300             DISPLAY 'W-DSN-7             -> ' W-DSN-7                    
051400           END-IF                                                         
051500         END-IF                                                           
051600                                                                          
051700         PERFORM CA-ALLOC-OPEN-RCVFILE                                    
051800         .                                                                
051900 EJECT                                                                    
052000 CA-ALLOC-OPEN-RCVFILE SECTION.                                           
052100         MOVE 'CA-ALLOC-OPEN-RCVFILE' TO W-VILKEN-SECTION                 
052200         IF SW-CORRECT-DSNAME         = 'N'                               
052300           EVALUATE PARM-GRP                                              
052400             WHEN 'QASE'                                                  
052500               MOVE 'WIN.QASE.VCOMRCV' TO DYN-DSNAME                      
052600             WHEN 'PROD'                                                  
052700               MOVE 'WIN.PROD.VCOMRCV' TO DYN-DSNAME                      
052800             WHEN 'XDEV'                                                  
052900               MOVE 'W.XTRAIN.VCOMRCV' TO DYN-DSNAME                      
053000             WHEN OTHER                                                   
053100               MOVE 'W.TESTIN.VCOMRCV' TO DYN-DSNAME                      
053200           END-EVALUATE                                                   
053300                                                                          
053400           MOVE DYN-DSNAME            TO WLOCATE-INDSN                    
053500           MOVE ' '                   TO WLOCATE-UTDSN                    
053600           CALL WLOCATE               USING WLOCATE-INDSN                 
053700                                            WLOCATE-UTDSN                 
053800           MOVE WLOCATE-UTDSN         TO W-W016RCV-GEN                    
053900           DISPLAY 'W-W016RCV-GEN       -> ' W-W016RCV-GEN                
054000         ELSE                                                             
054100*          -- USE SENDER TAG AS FILE NAME BY DEFAULT                      
054200           MOVE CONR-SENDERTAG        TO WLOCATE-INDSN                    
054300*          -- CHANGE THE FILE NAME IN SOME SITUATIONS:                    
054400           IF PARM-GRP = 'QASE' AND W-DSN-2 = 'PROD'                      
054500*            -- CHANGE TO QASE IN PROD                                    
054600             MOVE SPACE TO WLOCATE-INDSN                                  
054700             STRING W-DSN-1   DELIMITED BY SPACE                          
054800                    '.QASE.'  DELIMITED BY SIZE                           
054900                    W-DSN-3   DELIMITED BY SPACE                          
055000               INTO WLOCATE-INDSN                                         
055100           END-IF                                                         
055200*          -- CHANGE SENDERTAGS WITH QASE TO PROD,                        
055300*          -- SINCE LOOKUP TABLE ONLY CONTAINS PROD                       
055400           IF W-DSN-2 = 'QASE'                                            
055500             MOVE SPACE TO CONR-SENDERTAG                                 
055600             STRING W-DSN-1   DELIMITED BY SPACE                          
055700                    '.PROD.'  DELIMITED BY SIZE                           
055800                    W-DSN-3   DELIMITED BY SPACE                          
055900               INTO CONR-SENDERTAG                                        
056000           END-IF                                                         
056100           IF W-DSN-2 = 'TESTIN' OR 'TEST'                                
056200*            -- CHANGE TO CORRECT ENV IN TEST                             
056300             MOVE SPACE TO WLOCATE-INDSN                                  
056400             STRING W-DSN-1   DELIMITED BY SPACE                          
056500                    '.'       DELIMITED BY SIZE                           
056600                    PARM-GRP  DELIMITED BY SPACE                          
056700                    'IN.'     DELIMITED BY SIZE                           
056800                    W-DSN-3   DELIMITED BY SPACE                          
056900               INTO WLOCATE-INDSN                                         
057000           END-IF                                                         
057100                                                                          
057200           MOVE ' '                   TO WLOCATE-UTDSN                    
057300           CALL WLOCATE               USING WLOCATE-INDSN                 
057400                                            WLOCATE-UTDSN                 
057500           MOVE WLOCATE-UTDSN         TO W-W016RCV-GEN                    
057600           DISPLAY 'W-W016RCV-GEN       -> ' W-W016RCV-GEN                
057700                                                                          
057800*          MOVE CONR-SENDERTAG        TO WLISTDSI-INDSN                   
057900           MOVE WLOCATE-INDSN         TO WLISTDSI-INDSN                   
058000           MOVE ' '                   TO WLISTDSI-UTDSN                   
058100                                         WLISTDSI-CREYYYY                 
058200                                         WLISTDSI-CREDDD                  
058300                                         WLISTDSI-EXPYYYY                 
058400                                         WLISTDSI-EXPDDD                  
058500                                         WLISTDSI-REFYYYY                 
058600                                         WLISTDSI-REFDDD                  
058700           MOVE ZERO                  TO WLISTDSI-RC                      
058800           CALL WLISTDSI              USING WLISTDSI-INDSN                
058900                                            WLISTDSI-UTDSN                
059000                                            WLISTDSI-CREYYYY              
059100                                            WLISTDSI-CREDDD               
059200                                            WLISTDSI-EXPYYYY              
059300                                            WLISTDSI-EXPDDD               
059400                                            WLISTDSI-REFYYYY              
059500                                            WLISTDSI-REFDDD               
059600                                            WLISTDSI-RC                   
059700           IF WLISTDSI-RC             > 8                                 
059800             STRING ' CALL WLISTDSI GAV RETURKOD -> '                     
059900                    W-REDIG-RC                                            
060000                    DELIMITED BY SIZE   INTO ABEND-RAD                    
060100             PERFORM S99-ABEND-UTAN-DUMP                                  
060200           END-IF                                                         
060300*          MOVE CONR-SENDERTAG        TO DYN-DSNAME                       
060400           MOVE WLOCATE-INDSN         TO DYN-DSNAME                       
060500         END-IF                                                           
060600                                                                          
060700         DISPLAY 'DYN-DSNAME          -> ' DYN-DSNAME                     
060800                                                                          
060900         PERFORM S12-ALLOC-RCVFILE                                        
061000         PERFORM S13-OPEN-RCVFILE                                         
061100         .                                                                
061200 EJECT                                                                    
061300 D-HAMTA-FRAN-VCOM SECTION.                                               
061400         MOVE 'D-HAMTA-FRAN-VCOM'     TO W-VILKEN-SECTION                 
061500         MOVE +0                      TO VCOM-RC                          
061600                                                                          
061700         CALL DSRECV                  USING VCOM-RC                       
061800                                            VCOM-DISTID                   
061900                                            RECV-MAXLTH                   
062000                                            RECV-ACTLTH                   
062100                                            RECV-DATA                     
062200                                                                          
062300         IF VCOM-RC               NOT = ZERO                              
062400         AND VCOM-RC              NOT = 45                                
062500           MOVE VCOM-RC               TO W-REDIG-RC                       
062600           STRING ' CALL DSRECV GAV RETURKOD -> '                         
062700                  W-REDIG-RC                                              
062800                  DELIMITED BY SIZE   INTO ABEND-RAD                      
062900           PERFORM S99-ABEND-UTAN-DUMP                                    
063000         END-IF                                                           
063100         .                                                                
063200 EJECT                                                                    
063300 E-AVSLUTA-VCOM SECTION.                                                  
063400         MOVE 'E-AVSLUTA-VCOM'        TO W-VILKEN-SECTION                 
063500                                                                          
063600         MOVE +0                      TO RLSE-RVALUE                      
063700                                                                          
063800         IF PARM-RLSE-RVALUE          = '-1'                              
063900           MOVE -1                    TO RLSE-RVALUE                      
064000         END-IF                                                           
064100         IF PARM-RLSE-RVALUE          = '-2'                              
064200           MOVE -2                    TO RLSE-RVALUE                      
064300         END-IF                                                           
064400                                                                          
064500         CALL DSRLSE                  USING VCOM-RC                       
064600                                            VCOM-DISTID                   
064700                                            RLSE-RVALUE                   
064800                                                                          
064900         IF VCOM-RC               NOT = ZERO                              
065000           MOVE VCOM-RC               TO W-REDIG-RC                       
065100           STRING ' CALL DSRLSE GAV RETURKOD -> '                         
065200                  W-REDIG-RC                                              
065300                  DELIMITED BY SIZE   INTO ABEND-RAD                      
065400           PERFORM S99-ABEND-UTAN-DUMP                                    
065500         END-IF                                                           
065600         .                                                                
065700 EJECT                                                                    
065800 F-SUBMIT-JCL-WRITE-MEMO SECTION.                                         
065900         MOVE 'F-SUBMIT-JCL-WRITE-MEMO'     TO W-VILKEN-SECTION           
066000         display w-vilken-section                                         
066100                                                                          
066200         MOVE 'N'                     TO EOF-W016PRM                      
066300         PERFORM S01-LAS-W016PRM                                          
066400         PERFORM UNTIL EOF-W016PRM    = 'J'                               
066500         OR CONR-SENDERTAG            = W01613P-SENDERTAG                 
066600           PERFORM S01-LAS-W016PRM                                        
066700         END-PERFORM                                                      
066800                                                                          
066900                                                                          
067000         IF W01613P-SENDERTAG         = CONR-SENDERTAG                    
067100           IF  W01613P-SUBJCL         > ' '                               
067200             MOVE +1                  TO W-RETURN-CODE                    
067300             IF W01613P-MEMO          > ' '                               
067400               MOVE +2                TO W-RETURN-CODE                    
067500             END-IF                                                       
067600           ELSE                                                           
067700             IF W01613P-MEMO          > ' '                               
067800               MOVE +3                TO W-RETURN-CODE                    
067900             END-IF                                                       
068000           END-IF                                                         
068100         ELSE                                                             
068200           MOVE +4                    TO W-RETURN-CODE                    
068300           STRING 'No match in rec'                                       
068400                  'eiving table, a'                                       
068500                  'nd no subject t'                                       
068600                  'o further treat'                                       
068700                  'ment '                                                 
068800                                      DELIMITED BY SIZE                   
068900                                    INTO MEMOOK-RAD6                      
069000         END-IF                                                           
069100                                                                          
069200         IF W-RETURN-CODE             = +1                                
069300         AND W01613P-SUBJCL (1:3) NOT = 'SOP'                             
069400           PERFORM X01-HAMTA-JOBKORT                                      
069500         END-IF                                                           
069600                                                                          
069700         IF W-RETURN-CODE             > +1                                
069800           PERFORM X01-HAMTA-JOBKORT                                      
069900         END-IF                                                           
070000                                                                          
070100         IF W-RETURN-CODE             = +1                                
070200         OR W-RETURN-CODE             = +2                                
070300           IF W01613P-SUBJCL (1:3) NOT = 'SOP'                            
070400             STRING '//SYSUT1      DD        DSN='                        
070500                                      DELIMITED BY SIZE                   
070600                    W01613P-SUBJCL    DELIMITED BY ' '                    
070700                    ',DISP=SHR'       DELIMITED BY SIZE                   
070800                                      INTO JCLSUBM-KORT (2)               
070900             MOVE +1                  TO JCLSUBM-IX                       
071000             PERFORM UNTIL JCLSUBM-IX > JCLSUBM-IXMAX                     
071100               MOVE JCLSUBM-KORT (JCLSUBM-IX) TO W016INT                  
071200               DISPLAY JCLSUBM-KORT (JCLSUBM-IX)                          
071300               PERFORM S04-SKRIV-W016INT                                  
071400               ADD +1                     TO JCLSUBM-IX                   
071500             END-PERFORM                                                  
071600           ELSE                                                           
071700             PERFORM FA-PREPARERA-SOP                                     
071800           END-IF                                                         
071900         END-IF                                                           
072000                                                                          
072100         IF W-RETURN-CODE             = +2                                
072200         OR W-RETURN-CODE             = +3                                
072300         OR W-RETURN-CODE             = +4                                
072400           PERFORM FB-PREPARERA-MEMO                                      
072500         END-IF                                                           
072600         .                                                                
072700 EJECT                                                                    
072800 FA-PREPARERA-SOP section.                                                
072900         MOVE 'FA-PREPARERA-SOP'      TO W-VILKEN-SECTION                 
073000                                                                          
073100         MOVE SPACE                   TO SOP-DDPREFIX                     
073200                                         SOP-SYMBOLIC-VARIABLES           
073300                                                                          
073400         UNSTRING W01613P-SUBJCL      DELIMITED BY '=' OR ' ' INTO        
073500                                      W-SOPTEXT                           
073600                                      W-SOPROUTINE                        
073700                                      W-SOPPARM                           
073800                                                                          
073900         STRING 'VSNDTAG('            DELIMITED BY SIZE                   
074000*               W01613P-SENDERTAG     DELIMITED BY ' '                    
074100                W-W016RCV-GEN         DELIMITED BY ' '                    
074200                ')'                   DELIMITED BY SIZE                   
074300                                    INTO                                  
074400                                      SOP-SYMBOLIC-VARIABLES              
074500                                                                          
074600         IF W-SOPPARM                 > ' '                               
074700           STRING SOP-SYMBOLIC-VARIABLES DELIMITED BY ' '                 
074800                  ' '                 DELIMITED BY SIZE                   
074900                  W-SOPPARM           DELIMITED BY ' '                    
075000                                    INTO                                  
075100                                      SOP-SYMBOLIC-VARIABLES              
075200         END-IF                                                           
075300                                                                          
075400         MOVE W-SOPROUTINE            TO SOP-PROC-NAME                    
075500         MOVE 'O'                     TO SOP-SOPFUNC                      
075600         MOVE ZERO                    TO SOP-ACTPASS-DATE                 
075700                                                                          
075800         DISPLAY 'ORDER AV SOP-ROUTINE-> ' SOP-PROC-NAME                  
075900         DISPLAY 'PARAMETER TO SOP    -> ' SOP-SYMBOLIC-VARIABLES         
076000                                                                          
076100         CALL W980SOP                 USING SOP-PARM-AREA                 
076200                                                                          
076300         MOVE SOP-RETCODE             TO W-RED-RETCODE                    
076400         MOVE SOP-MSGCODE             TO W-RED-MSGCODE                    
076500                                                                          
076600****************************************** Om MEMO ej begärt (+2)         
076700*                                          men SOP ger rc > 0             
076800*                                          tvingar vi ut ett MEMO         
076900*                                          m h a 'on error' def.          
077000*                                                                         
077100         IF SOP-RETCODE               > 0                                 
077200           IF W-RETURN-CODE           = +1                                
077300             MOVE +2                  TO W-RETURN-CODE                    
077400             PERFORM UNTIL EOF-W016PRM = 'J'                              
077500               PERFORM S01-LAS-W016PRM                                    
077600             END-PERFORM                                                  
077700           END-IF                                                         
077800         END-IF                                                           
077900                                                                          
078000         IF W-RETURN-CODE             = +2                                
078100           STRING '     Routine '     DELIMITED BY SIZE                   
078200                  SOP-PROC-NAME       DELIMITED BY ' '                    
078300                  ' ordered with '    DELIMITED BY SIZE                   
078400                  'symbolic variables:' DELIMITED BY SIZE                 
078500                                    INTO MEMOOK-RAD6                      
078600           STRING '       '           DELIMITED BY SIZE                   
078700                  SOP-SYMBOLIC-VARIABLES DELIMITED BY '     '             
078800                                    INTO MEMOOK-RAD7                      
078900           STRING '     Returcode '   DELIMITED BY SIZE                   
079000                  W-RED-RETCODE       DELIMITED BY SIZE                   
079100                  '  Message code '   DELIMITED BY SIZE                   
079200                  W-RED-MSGCODE       DELIMITED BY SIZE                   
079300                                    INTO MEMOOK-RAD8                      
079400         END-IF                                                           
079500                                                                          
079600         .                                                                
079700 EJECT                                                                    
079800 FB-PREPARERA-MEMO SECTION.                                               
079900         MOVE 'FB-PREPARERA-MEMO'     TO W-VILKEN-SECTION                 
080000                                                                          
080100         MOVE 'VCOM arrived  '        TO JCLMEMO-TITLE                    
080200         MOVE +6                      TO JCLMEMO-IX                       
080300                                         JCLMEMO-IXANT                    
080400                                                                          
080500         STRING ' DEST '              DELIMITED BY SIZE                   
080600                W01613P-MEMO          DELIMITED BY SIZE                   
080700                                      INTO                                
080800                                      JCLMEMO-KORT (JCLMEMO-IX)           
080900         ADD +1                       TO JCLMEMO-IX                       
081000         ADD +1                       TO JCLMEMO-IXANT                    
081100                                                                          
081200         MOVE ' OPTION FORCE'         TO JCLMEMO-KORT (JCLMEMO-IX)        
081300         ADD +1                       TO JCLMEMO-IX                       
081400         ADD +1                       TO JCLMEMO-IXANT                    
081500         MOVE ' MEMO'                 TO JCLMEMO-KORT (JCLMEMO-IX)        
081600         ADD +1                       TO JCLMEMO-IX                       
081700         ADD +1                       TO JCLMEMO-IXANT                    
081800                                                                          
081900         MOVE W-W016RCV-GEN           TO MEMOOK-W016RCV-GEN               
082000         MOVE VCOM-DISTID             TO MEMOOK-DISTID                    
082100         MOVE CONR-EXPEDITER          TO MEMOOK-EXPEDITER                 
082200         MOVE MEMOOK-RAD1             TO JCLMEMO-KORT (JCLMEMO-IX)        
082300         ADD +1                       TO JCLMEMO-IX                       
082400         ADD +1                       TO JCLMEMO-IXANT                    
082500         MOVE MEMOOK-RAD2             TO JCLMEMO-KORT (JCLMEMO-IX)        
082600         ADD +1                       TO JCLMEMO-IX                       
082700         ADD +1                       TO JCLMEMO-IXANT                    
082800         MOVE MEMOOK-RAD3             TO JCLMEMO-KORT (JCLMEMO-IX)        
082900         ADD +1                       TO JCLMEMO-IX                       
083000         ADD +1                       TO JCLMEMO-IXANT                    
083100         MOVE MEMOOK-RAD4             TO JCLMEMO-KORT (JCLMEMO-IX)        
083200         ADD +1                       TO JCLMEMO-IX                       
083300         ADD +1                       TO JCLMEMO-IXANT                    
083400         MOVE MEMOOK-RAD5             TO JCLMEMO-KORT (JCLMEMO-IX)        
083500         ADD +1                       TO JCLMEMO-IX                       
083600         ADD +1                       TO JCLMEMO-IXANT                    
083700         MOVE MEMOOK-RAD6             TO JCLMEMO-KORT (JCLMEMO-IX)        
083800         ADD +1                       TO JCLMEMO-IX                       
083900         ADD +1                       TO JCLMEMO-IXANT                    
084000         MOVE MEMOOK-RAD7             TO JCLMEMO-KORT (JCLMEMO-IX)        
084100         ADD +1                       TO JCLMEMO-IX                       
084200         ADD +1                       TO JCLMEMO-IXANT                    
084300         MOVE MEMOOK-RAD8             TO JCLMEMO-KORT (JCLMEMO-IX)        
084400         ADD +1                       TO JCLMEMO-IX                       
084500         ADD +1                       TO JCLMEMO-IXANT                    
084600         MOVE MEMOOK-RAD9             TO JCLMEMO-KORT (JCLMEMO-IX)        
084700         ADD +1                       TO JCLMEMO-IX                       
084800         ADD +1                       TO JCLMEMO-IXANT                    
084900                                                                          
085000         MOVE +1                      TO JCLMEMO-IX                       
085100         PERFORM UNTIL JCLMEMO-IX     > JCLMEMO-IXANT                     
085200           MOVE JCLMEMO-KORT (JCLMEMO-IX) TO W016INT                      
085300           DISPLAY JCLMEMO-KORT (JCLMEMO-IX)                              
085400           PERFORM S04-SKRIV-W016INT                                      
085500           ADD +1                     TO JCLMEMO-IX                       
085600         END-PERFORM                                                      
085700         .                                                                
085800 EJECT                                                                    
085900 G-WRITE-FILEPROBL-MEMO SECTION.                                          
086000         MOVE 'G-WRITE-ERROR-MEMO'    TO W-VILKEN-SECTION                 
086100         DISPLAY W-VILKEN-SECTION                                         
086200                                                                          
086300         MOVE 'N'                     TO EOF-W016PRM                      
086400         PERFORM S01-LAS-W016PRM                                          
086500         IF EOF-W016PRM               = 'J'                               
086600* CCID 10277114 -> NEW MAIL DESTINATION                                   
086600           MOVE 'WSYST@VOLVOCARS.COM' TO W01613P-MEMO                     
086700         END-IF                                                           
086800                                                                          
086900         PERFORM UNTIL EOF-W016PRM    = 'J'                               
087000           PERFORM S01-LAS-W016PRM                                        
087100         END-PERFORM                                                      
087200                                                                          
087300         MOVE +5                      TO W-RETURN-CODE                    
087400                                                                          
087500         MOVE 'VCOM fileprobl'        TO JCLMEMO-TITLE                    
087600         MOVE +6                      TO JCLMEMO-IX                       
087700                                         JCLMEMO-IXANT                    
087800         IF W01613P-MEMO-ERROR        > ' '                               
087900           STRING ' DEST '            DELIMITED BY SIZE                   
088000                  W01613P-MEMO-ERROR  DELIMITED BY SIZE                   
088100                                      INTO                                
088200                                      JCLMEMO-KORT (JCLMEMO-IX)           
088300           ADD +1                     TO JCLMEMO-IX                       
088400           ADD +1                     TO JCLMEMO-IXANT                    
088500         END-IF                                                           
088600                                                                          
088700         MOVE ' OPTION FORCE'         TO JCLMEMO-KORT (JCLMEMO-IX)        
088800         ADD +1                       TO JCLMEMO-IX                       
088900         ADD +1                       TO JCLMEMO-IXANT                    
089000         MOVE ' MEMO'                 TO JCLMEMO-KORT (JCLMEMO-IX)        
089100         ADD +1                       TO JCLMEMO-IX                       
089200         ADD +1                       TO JCLMEMO-IXANT                    
089300                                                                          
089400         MOVE W-W016RCV-GEN           TO MEMOTX-W016RCV-GEN               
089500         MOVE VCOM-DISTID             TO MEMOTX-DISTID                    
089600         MOVE CONR-EXPEDITER          TO MEMOTX-EXPEDITER                 
089700         MOVE CONR-SENDERTAG          TO MEMOTX-SENDERTAG                 
089800         MOVE MEMOTX-RAD1             TO JCLMEMO-KORT (JCLMEMO-IX)        
089900         ADD +1                       TO JCLMEMO-IX                       
090000         ADD +1                       TO JCLMEMO-IXANT                    
090100         MOVE MEMOTX-RAD2             TO JCLMEMO-KORT (JCLMEMO-IX)        
090200         ADD +1                       TO JCLMEMO-IX                       
090300         ADD +1                       TO JCLMEMO-IXANT                    
090400         MOVE MEMOTX-RAD3             TO JCLMEMO-KORT (JCLMEMO-IX)        
090500         ADD +1                       TO JCLMEMO-IX                       
090600         ADD +1                       TO JCLMEMO-IXANT                    
090700         MOVE MEMOTX-RAD4             TO JCLMEMO-KORT (JCLMEMO-IX)        
090800         ADD +1                       TO JCLMEMO-IX                       
090900         ADD +1                       TO JCLMEMO-IXANT                    
091000         MOVE MEMOTX-RAD5             TO JCLMEMO-KORT (JCLMEMO-IX)        
091100         ADD +1                       TO JCLMEMO-IX                       
091200         ADD +1                       TO JCLMEMO-IXANT                    
091300         MOVE MEMOTX-RAD6             TO JCLMEMO-KORT (JCLMEMO-IX)        
091400         ADD +1                       TO JCLMEMO-IX                       
091500         ADD +1                       TO JCLMEMO-IXANT                    
091600         MOVE MEMOTX-RAD7             TO JCLMEMO-KORT (JCLMEMO-IX)        
091700         ADD +1                       TO JCLMEMO-IX                       
091800         ADD +1                       TO JCLMEMO-IXANT                    
091900         MOVE MEMOTX-RAD8             TO JCLMEMO-KORT (JCLMEMO-IX)        
092000         ADD +1                       TO JCLMEMO-IX                       
092100         ADD +1                       TO JCLMEMO-IXANT                    
092200         MOVE MEMOTX-RAD9             TO JCLMEMO-KORT (JCLMEMO-IX)        
092300         ADD +1                       TO JCLMEMO-IX                       
092400         ADD +1                       TO JCLMEMO-IXANT                    
092500         MOVE MEMOTX-RAD10            TO JCLMEMO-KORT (JCLMEMO-IX)        
092600         ADD +1                       TO JCLMEMO-IX                       
092700         ADD +1                       TO JCLMEMO-IXANT                    
092800         MOVE MEMOTX-RAD11            TO JCLMEMO-KORT (JCLMEMO-IX)        
092900         ADD +1                       TO JCLMEMO-IX                       
093000         ADD +1                       TO JCLMEMO-IXANT                    
093100                                                                          
093200         MOVE +1                      TO JCLMEMO-IX                       
093300         PERFORM X01-HAMTA-JOBKORT                                        
093400         PERFORM UNTIL JCLMEMO-IX     > JCLMEMO-IXANT                     
093500           MOVE JCLMEMO-KORT (JCLMEMO-IX) TO W016INT                      
093600           DISPLAY JCLMEMO-KORT (JCLMEMO-IX)                              
093700           PERFORM S04-SKRIV-W016INT                                      
093800           ADD +1                     TO JCLMEMO-IX                       
093900         END-PERFORM                                                      
094000         .                                                                
094100 EJECT                                                                    
094200 Z-FINIT SECTION.                                                         
094300         MOVE 'Z-FINIT'               TO W-VILKEN-SECTION                 
094400         PERFORM S15-CLOSE-RCVFILE                                        
094500         DISPLAY ' '                                                      
094600         DISPLAY 'W016JOB JOBKORT     -> ' LOGG-W016JOB                   
094700         DISPLAY 'W016INT INTRDR      -> ' LOGG-W016INT                   
094800         DISPLAY 'W016DYN VCOMFIL DYN -> ' LOGG-W016DYN                   
094900         CLOSE W016JOB-REG                                                
095000         CLOSE W016INT-REG                                                
095100         CLOSE W016PRM-REG                                                
095200         DISPLAY 'S T O P P'                                              
095300         .                                                                
095400 EJECT                                                                    
095500 X01-HAMTA-JOBKORT SECTION.                                               
095600         MOVE 'X01-HAMTA-JOBKORT'     TO W-VILKEN-SECTION                 
095700                                                                          
095800         IF EOF-W016JOB            NOT = 'J'                              
095900           PERFORM S03-LAS-W016JOB                                        
096000           STRING '//'                DELIMITED BY SIZE                   
096100                  CONR-EXPEDITER      DELIMITED BY SIZE                   
096200                                      INTO W016JOB-STEG                   
096300           MOVE 'M'                   TO W016JOB-POS10                    
096400                                                                          
096500           PERFORM UNTIL EOF-W016JOB  = 'J'                               
096600             MOVE W016JOB             TO W016INT                          
096700             PERFORM S04-SKRIV-W016INT                                    
096800             PERFORM S03-LAS-W016JOB                                      
096900           END-PERFORM                                                    
097000         END-IF                                                           
097100         .                                                                
097200 S01-LAS-W016PRM SECTION.                                                 
097300         MOVE 'S01-LAS-W016PRM'       TO W-VILKEN-SECTION                 
097400         READ W016PRM-REG             INTO W01613P                        
097500           END MOVE 'J'               TO EOF-W016PRM                      
097600         END-READ                                                         
097700         IF EOF-W016PRM           NOT = 'J'                               
097800           ADD +1                     TO LOGG-W016PRM                     
097900         END-IF                                                           
098000         .                                                                
098100 S03-LAS-W016JOB SECTION.                                                 
098200         MOVE 'S03-LAS-W016JOB'       TO W-VILKEN-SECTION                 
098300         READ W016JOB-REG             INTO W016JOB                        
098400           END MOVE 'J'               TO EOF-W016JOB                      
098500         END-READ                                                         
098600         IF EOF-W016JOB           NOT = 'J'                               
098700           ADD +1                     TO LOGG-W016JOB                     
098800         END-IF                                                           
098900         .                                                                
099000 S04-SKRIV-W016INT SECTION.                                               
099100         MOVE 'S04-SKRIV-W016INT'     TO W-VILKEN-SECTION                 
099200         WRITE W016INT-REC            FROM W016INT                        
099300         ADD +1                       TO LOGG-W016INT                     
099400         MOVE ' '                     TO W016INT                          
099500         .                                                                
099600 S11-INIT-RCVFILE SECTION.                                                
099700                                                                          
099800* --- INITIERA DCB FÖR RCVFILE = SELECT-SATS SAMT FD I COBOL              
099900                                                                          
100000     CALL WFILWRT                     USING DYN-RCVFILE                   
100100                                            DYN-INIT                      
100200                                                                          
100300     IF DYN-INIT                      = 'F'                               
100400       MOVE 'KAN EJ INITIERA DCB MED WFILWRT' TO ABEND-RAD                
100500       PERFORM S99-ABEND-UTAN-DUMP                                        
100600     ELSE                                                                 
100700       DISPLAY 'WFILWRT INIT OK RC  -> ' DYN-INIT                         
100800     END-IF                                                               
100900     .                                                                    
101000     EJECT                                                                
101100 S12-ALLOC-RCVFILE SECTION.                                               
101200     MOVE 'S12-ALLOC-RCVFILE'         TO W-VILKEN-SECTION                 
101300                                                                          
101400     MOVE 'C'                         TO  DYN-FREE                        
101500     MOVE '+1'                        TO  DYN-GENMBR                      
101600     MOVE 'VB'                        TO  DYN-RECFM                       
101700     MOVE 'N'                         TO  DYN-DISP1                       
101800     MOVE 'C'                         TO  DYN-DISP2                       
101900     MOVE 'C'                         TO  DYN-DISP3                       
102000     MOVE 'D'                         TO  DYN-DISP3                       
102100     MOVE 'PSEB'                      TO  DYN-DATACLASS                   
102200     MOVE 'NOBACKUP'                  TO  DYN-MGMCLASS                    
102300     MOVE +4092                       TO  DYN-LRECL                       
102400     MOVE +4100                       TO  DYN-BLKSIZE                     
102500     MOVE 'R'                         TO  DYN-RLSE                        
102600                                                                          
102700     CALL WDYNALC                     USING DYN-RCVFILE                   
102800                                            DYN-AREA                      
102900                                                                          
103000     IF DYN-KDSVAR-FEL                                                    
103100       DISPLAY 'KAN EJ ALLOKERA FILE-> ' DYN-DSNAME DYN-GENMBR            
103200       DISPLAY 'FÖRSÖKER UTAN GEN   -> ' DYN-DSNAME                       
103300       MOVE '  '                      TO DYN-GENMBR                       
103400       IF WLISTDSI-RC                 = 0                                 
103500         MOVE 'O'                     TO DYN-DISP1                        
103600         MOVE 'K'                     TO DYN-DISP2                        
103700         MOVE 'K'                     TO DYN-DISP3                        
103800         DISPLAY 'REPLACE GÖRS FÖR    -> ' DYN-DSNAME                     
103900       END-IF                                                             
104000                                                                          
104100       CALL WDYNALC                   USING DYN-RCVFILE                   
104200                                            DYN-AREA                      
104300       IF DYN-KDSVAR-FEL                                                  
104400         PERFORM S99-ABEND-UTAN-DUMP                                      
104500       END-IF                                                             
104600     END-IF                                                               
104700     .                                                                    
104800 S13-OPEN-RCVFILE SECTION.                                                
104900     MOVE 'S13-OPEN-RCVFILE'          TO W-VILKEN-SECTION                 
105000                                                                          
105100     CALL WFILWRT                     USING DYN-RCVFILE                   
105200                                            DYN-OPEN                      
105300                                                                          
105400     IF DYN-OPEN                      = 'F'                               
105500       STRING 'KAN EJ ÖPPNA FILE     --> '                                
105600              DYN-DSNAME                                                  
105700              DYN-GENMBR              DELIMITED BY SIZE                   
105800                                      INTO ABEND-RAD                      
105900       PERFORM S99-ABEND-UTAN-DUMP                                        
106000     END-IF                                                               
106100     .                                                                    
106200 S14-WRITE-RCVFILE SECTION.                                               
106300                                                                          
106400     MOVE RECV-ACTLTH                 TO DYN-WRITE-LRECL                  
106500                                                                          
106600     CALL WFILWRT                     USING DYN-RCVFILE                   
106700                                            DYN-WRITE                     
106800                                            DYN-WRITE-RECFM               
106900                                            DYN-WRITE-LRECL               
107000                                            RECV-DATA                     
107100                                                                          
107200     IF DYN-WRITE                     = 'F'                               
107300       STRING 'KAN EJ SKRIVA PÅ FILE --> '                                
107400              DYN-DSNAME                                                  
107500              DYN-GENMBR              DELIMITED BY SIZE                   
107600                                      INTO ABEND-RAD                      
107700       PERFORM S99-ABEND-UTAN-DUMP                                        
107800     END-IF                                                               
107900     ADD +1                           TO LOGG-W016DYN                     
108000     .                                                                    
108100 S15-CLOSE-RCVFILE SECTION.                                               
108200     CALL WFILWRT                     USING DYN-RCVFILE                   
108300                                            DYN-CLOSE                     
108400     IF DYN-CLOSE                     = 'F'                               
108500       STRING 'KAN EJ STÄNGA FILE    --> '                                
108600              DYN-DSNAME                                                  
108700              DYN-GENMBR              DELIMITED BY SIZE                   
108800                                      INTO ABEND-RAD                      
108900       PERFORM S99-ABEND-UTAN-DUMP                                        
109000     END-IF                                                               
109100     .                                                                    
109200 EJECT                                                                    
109300 S99-ABEND-UTAN-DUMP SECTION.                                             
109400         DISPLAY W-PROGNAMN ABEND-RUBRIK UPON CONSOLE                     
109500         DISPLAY ' '                  UPON CONSOLE                        
109600         DISPLAY W-PROGNAMN ABEND-RAD UPON CONSOLE                        
109700         DISPLAY ' '                  UPON CONSOLE                        
109800         MOVE +32                     TO ABEND-CODE                       
109900         CALL ABEND USING ABEND-CODE                                      
110000         .                                                                
