001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     WF210800.                                                
001400 AUTHOR.         HAMMARIN BO.                                             
001500 DATE-WRITTEN.   NOV 2003.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001800                                                                          
001900*    FUNCTION:                                                            
002000*        THE PGM                                                          
002100*        - READS FILE WITH CUSTOMS DATA RECORDS                           
002200*        - SENDS CUSTOMS DATA RECORDS FOR VCCS TO:                        
002210*          . PULS BY USING WZ01SEND (CARPARTS.PULS.RECEIVEC)              
002500*                                                                         
002800 ENVIRONMENT DIVISION.                                                    
002900                                                                          
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003302*          --- CUSTOMS DATA RECORDS                                       
003310     SELECT WF2018                     ASSIGN TO WF2108D1.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700                                                                          
003800 FILE SECTION.                                                            
003902 FD  WF2018                                                               
003903     RECORDING       F                                                    
003904     BLOCK CONTAINS  0.                                                   
003905                                                                          
003910*01  -COPY WF2018      -L.                                                
004000     EJECT                                                                
004010                                                                          
004200 WORKING-STORAGE SECTION.                                                 
004300 77  IDPGM                       PIC X(8)    VALUE 'WF210800'.            
004501 77  WS-IDLEGSEL-VCCS            PIC X(4)    VALUE 'VCCS'.                
004502 77  WS-IDPTYP                   PIC X(3)    VALUE 'CUS'.                 
004503 77  WS-ADRESS                   PIC X(50)                                
004504                                  VALUE 'CARPARTS.PULS.RECEIVEC'.         
004505                                                                          
004510 77  WF2018-EOF-SW               PIC X       VALUE 'N'.                   
004520     88  END-OF-WF2018                       VALUE 'J'.                   
004600                                                                          
004700 01  ERRTEXT.                                                             
004800     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
004900     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
005000 01  KDRC-DISPLAY                PIC Z(5).                                
005400     EJECT                                                                
006010                                                                          
006100 01  GENERAL-SUBPROGRAMS.                                                 
006400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006401     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
006402     EJECT                                                                
006410                                                                          
006420*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
006430                                                                          
006440 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006450 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006460 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006601     EJECT                                                                
006603                                                                          
006604*    --- AREOR FÖR KOMMUNIKATION                                          
006609 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
006610*01  -COPY WZ01SEND                                                       
006620     EJECT                                                                
006630                                                                          
006902 01  IN-AREA-START            PIC X(24)   VALUE 'IN-AREA-START'.          
006910 01  IN-AREA.                                                             
006920*    03  -COPY WF2018                                                     
009000     EJECT                                                                
009010                                                                          
009100 01  CUSUT-AREA-START            PIC X(24)   VALUE                        
009200                                               'CUSUT-AREA-START'.        
009300 01  CUSUT-AREA.                                                          
009310*    03  -COPY WZ01REQU         -PRE CUSUT-                               
009311*    03  -COPY WF2108I1         -PRE CUSUT-                               
009320     EJECT                                                                
009330                                                                          
010100 LINKAGE SECTION.                                                         
010300*01  -COPY W0009   -PRE MSG-                                              
010310                                                                          
010400*01  -COPY W0009   -PRE RECEIVEC-                                         
010700     EJECT                                                                
010800                                                                          
010801 PROCEDURE DIVISION  USING MSG-PCB RECEIVEC-PCB.                          
010802 MAIN SECTION.                                                            
010803                                                                          
010810     ENTRY 'DLITCBL' USING MSG-PCB RECEIVEC-PCB.                          
010900                                                                          
011200     PERFORM A-INIT                                                       
011202     PERFORM B-EXECUTE                                                    
012510     PERFORM Z-FINIT                                                      
012600                                                                          
012700     MOVE ZERO TO RETURN-CODE                                             
012800     GOBACK                                                               
012900     .                                                                    
013000                                                                          
013100 A-INIT SECTION.                                                          
013310     OPEN INPUT WF2018                                                    
014200     .                                                                    
014300                                                                          
014370 B-EXECUTE SECTION.                                                       
014372     PERFORM S01-READ-WF2018                                              
014373                                                                          
014374     IF END-OF-WF2018                                                     
014375       CONTINUE                                                           
014380     ELSE                                                                 
014382       PERFORM S90-SEND-OPEN                                              
014383                                                                          
014392       PERFORM UNTIL END-OF-WF2018                                        
014393         IF CUS-IDLEGSEL = WS-IDLEGSEL-VCCS                               
014394           PERFORM BA-MOVE-TO-OUTPUT                                      
014395           PERFORM S91-SEND-PUT                                           
014398         END-IF                                                           
014399         PERFORM S01-READ-WF2018                                          
014400       END-PERFORM                                                        
014410                                                                          
014415       PERFORM S99-SEND-CLOSE                                             
014416     END-IF                                                               
014417     .                                                                    
014418                                                                          
014419 BA-MOVE-TO-OUTPUT SECTION.                                               
014420     MOVE 1                   TO CUSUT-REQU-IDMSGVER                      
014421     MOVE SPACE               TO CUSUT-REQU-KDPGMACT                      
014422     MOVE 'WF210800'          TO CUSUT-REQU-IDUSER                        
014423                                                                          
014424     MOVE CUS-DAEXDAT         TO CUSUT-DAEXDAT                            
014425     COMPUTE CUS-TIEXTID = CUS-TIEXTID / 10                               
014426     END-COMPUTE                                                          
014427     MOVE CUS-TIEXTID         TO CUSUT-TIEXTID                            
014428     MOVE WS-IDPTYP           TO CUSUT-IDPTYP                             
014429     MOVE CUS-IDLANDX3-SEND   TO CUSUT-IDLANDX3-SEND                      
014430     MOVE CUS-IDLANDX3-BET    TO CUSUT-IDLANDX3-BET                       
014434     MOVE CUS-IDPARTNR        TO CUSUT-IDPARTNR                           
014435     MOVE CUS-KDFINDOC        TO CUSUT-KDFINDOC                           
014436     MOVE CUS-DAFINDOC        TO CUSUT-DAFINDOC                           
014437     MOVE CUS-IDFINDOC        TO CUSUT-IDFINDOC                           
014438     MOVE CUS-IDEXCUST-1      TO CUSUT-IDEXCUST-1                         
014440     MOVE CUS-IDARTNR-FINANCE TO CUSUT-IDARTNR-FINANCE                    
014441     MOVE CUS-BEART           TO CUSUT-BEART                              
014446     MOVE CUS-KVLEVART        TO CUSUT-KVLEVART                           
014460     .                                                                    
014470                                                                          
014500 Z-FINIT SECTION.                                                         
014610     CLOSE WF2018                                                         
015000     .                                                                    
015100                                                                          
015103 S01-READ-WF2018  SECTION.                                                
015105     READ WF2018 INTO IN-AREA                                             
015106       AT END                                                             
015108         SET END-OF-WF2018 TO TRUE                                        
015115     END-READ                                                             
015120     .                                                                    
015130                                                                          
015456 S90-SEND-OPEN SECTION.                                                   
015458     MOVE WS-ADRESS                       TO SEND-ADDISPABS               
015459     MOVE 'OPEN'                          TO SEND-KDFUNC                  
015460     CALL WZ01SEND USING SEND-CONTROL-AREA                                
015461                         SEND-OPEN-AREA                                   
015462     IF SEND-KDRC > ZERO                                                  
015463       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
015464       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
015465       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
015466       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
015467     END-IF                                                               
015468     .                                                                    
015479                                                                          
015480 S91-SEND-PUT SECTION.                                                    
015481     MOVE 'PUT'                           TO SEND-KDFUNC                  
015482     MOVE LENGTH OF CUSUT-AREA            TO SEND-KVDLEN                  
015483     CALL WZ01SEND USING SEND-CONTROL-AREA                                
015484                         SEND-KVDLEN                                      
015485                         CUSUT-AREA                                       
015486     IF SEND-KDRC > ZERO                                                  
015487       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
015488       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
015489       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
015490       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
015491     END-IF                                                               
015492     .                                                                    
015494                                                                          
015495 S99-SEND-CLOSE SECTION.                                                  
015496     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
015497     CALL WZ01SEND USING SEND-CONTROL-AREA                                
015498     .                                                                    
