001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     WF210300.                                                
001400 AUTHOR.         BERNT LUNDH.                                             
001500 DATE-WRITTEN.   2002-04-30.                                              
001600 DATE-COMPILED.                                                           
001700                                                                          
001800                                                                          
001900*    FUNCTION:                                                            
002000*        THE PGM                                                          
002100*        - READS FILE WITH V.A.T. DATA RECORDS                            
002200*        - SENDS V.A.T. DATA RECORDS FOR VCCS TO:                         
002210*          . PULS BY USING WZ01SEND (CARPARTS.PULS.RECEIVEV)              
002500*                                                                         
002800 ENVIRONMENT DIVISION.                                                    
002900                                                                          
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003302*          --- V.A.T. DATA RECORDS                                        
003310     SELECT WF2013                     ASSIGN TO WF2103D1.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700                                                                          
003800 FILE SECTION.                                                            
003902 FD  WF2013                                                               
003903     RECORDING       F                                                    
003904     BLOCK CONTAINS  0.                                                   
003905                                                                          
003910*01  -COPY WF2013      -L.                                                
004000     EJECT                                                                
004010                                                                          
004200 WORKING-STORAGE SECTION.                                                 
004300 77  IDPGM                       PIC X(8)    VALUE 'WF210300'.            
004501 77  WS-IDLEGSEL-VCCS            PIC X(4)    VALUE 'VCCS'.                
004502 77  WS-IDPTYP                   PIC X(3)    VALUE 'VAT'.                 
004503 77  WS-ADRESS                   PIC X(50)                                
004504                                  VALUE 'CARPARTS.PULS.RECEIVEV'.         
004510                                                                          
004511 77  WF2013-EOF-SW               PIC X       VALUE 'N'.                   
004520     88  END-OF-WF2013                       VALUE 'J'.                   
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
006920*    03  -COPY WF2013                                                     
009000     EJECT                                                                
009010                                                                          
009100 01  VATUT-AREA-START            PIC X(24)   VALUE                        
009200                                               'VATUT-AREA-START'.        
009300 01  VATUT-AREA.                                                          
009310*    03  -COPY WZ01REQU         -PRE VATUT-                               
009311*    03  -COPY WF2103I1         -PRE VATUT-                               
009320     EJECT                                                                
009330                                                                          
010100 LINKAGE SECTION.                                                         
010300*01  -COPY W0009   -PRE MSG-                                              
010310                                                                          
010400*01  -COPY W0009   -PRE RECEIVEV-                                         
010700     EJECT                                                                
010800                                                                          
010801 PROCEDURE DIVISION  USING MSG-PCB RECEIVEV-PCB.                          
010802 MAIN SECTION.                                                            
010803                                                                          
010810     ENTRY 'DLITCBL' USING MSG-PCB RECEIVEV-PCB.                          
010900                                                                          
011200     PERFORM A-INIT                                                       
011202     PERFORM B-EXECUTE                                                    
012510     PERFORM Z-FINIT                                                      
012600                                                                          
012700     MOVE ZERO TO RETURN-CODE                                             
012800     GOBACK                                                               
012900     .                                                                    
013100 A-INIT SECTION.                                                          
013200                                                                          
013310     OPEN INPUT WF2013                                                    
014200     .                                                                    
014370 B-EXECUTE SECTION.                                                       
014371                                                                          
014372     PERFORM S01-READ-WF2013                                              
014373                                                                          
014374     IF END-OF-WF2013                                                     
014375       CONTINUE                                                           
014380     ELSE                                                                 
014382       PERFORM S90-SEND-OPEN                                              
014383                                                                          
014392       PERFORM UNTIL END-OF-WF2013                                        
014393         IF VAT-IDLEGSEL = WS-IDLEGSEL-VCCS                               
014394           PERFORM BA-MOVE-TO-OUTPUT                                      
014395           PERFORM S91-SEND-PUT                                           
014398         END-IF                                                           
014399         PERFORM S01-READ-WF2013                                          
014400       END-PERFORM                                                        
014410                                                                          
014415       PERFORM S99-SEND-CLOSE                                             
014416     END-IF                                                               
014417     .                                                                    
014418 BA-MOVE-TO-OUTPUT SECTION.                                               
014419                                                                          
014420     MOVE 1                    TO VATUT-REQU-IDMSGVER                     
014421     MOVE SPACE                TO VATUT-REQU-KDPGMACT                     
014422     MOVE 'WF210300'           TO VATUT-REQU-IDUSER                       
014423                                                                          
014424     MOVE VAT-DAEXDAT          TO VATUT-DAEXDAT                           
014425     COMPUTE VAT-TIEXTID = VAT-TIEXTID / 10                               
014426     END-COMPUTE                                                          
014427     MOVE VAT-TIEXTID          TO VATUT-TIEXTID                           
014428     MOVE WS-IDPTYP            TO VATUT-IDPTYP                            
014429     MOVE VAT-IDLANDX3-SEND    TO VATUT-IDLANDX3-SEND                     
014430     MOVE VAT-IDLANDX3-BET     TO VATUT-IDLANDX3-BET                      
014431     MOVE VAT-KDVALISO         TO VATUT-KDVALISO                          
014432     MOVE VAT-PRKURS           TO VATUT-PRKURS                            
014433     MOVE VAT-IDVAT-LEG        TO VATUT-IDVAT-LEG                         
014439     MOVE VAT-IDVAT-RESP       TO VATUT-IDVAT-RESP                        
014441     MOVE VAT-IDVAT-BET        TO VATUT-IDVAT-BET                         
014442     MOVE VAT-IDPARTNR         TO VATUT-IDPARTNR                          
014443     MOVE VAT-KDFINDOC         TO VATUT-KDFINDOC                          
014444     MOVE VAT-DAFINDOC         TO VATUT-DAFINDOC                          
014445     MOVE VAT-IDFINDOC         TO VATUT-IDFINDOC                          
014446     MOVE VAT-SUNTO-TOT        TO VATUT-SUNTO-TOT                         
014447     MOVE VAT-SUVAT-BILLIT-TOT TO VATUT-SUVAT-BILLIT-TOT                  
014448     MOVE VAT-IDEXCUST-1       TO VATUT-IDEXCUST-1                        
014449     MOVE VAT-IDEXCUST-2       TO VATUT-IDEXCUST-2                        
014450     .                                                                    
014500 Z-FINIT SECTION.                                                         
014600                                                                          
014610     CLOSE WF2013                                                         
015000     .                                                                    
015103 S01-READ-WF2013  SECTION.                                                
015104                                                                          
015105     READ WF2013 INTO IN-AREA                                             
015106       AT END                                                             
015108         SET END-OF-WF2013 TO TRUE                                        
015115     END-READ                                                             
015120     .                                                                    
015456 S90-SEND-OPEN SECTION.                                                   
015457                                                                          
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
015478 S91-SEND-PUT SECTION.                                                    
015479                                                                          
015480     MOVE 'PUT'                           TO SEND-KDFUNC                  
015481     MOVE LENGTH OF VATUT-AREA            TO SEND-KVDLEN                  
015482     CALL WZ01SEND USING SEND-CONTROL-AREA                                
015483                         SEND-KVDLEN                                      
015484                         VATUT-AREA                                       
015485     IF SEND-KDRC > ZERO                                                  
015486       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
015487       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
015488       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
015489       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
015490     END-IF                                                               
015491     .                                                                    
015492 S99-SEND-CLOSE SECTION.                                                  
015493                                                                          
015494     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
015495     CALL WZ01SEND USING SEND-CONTROL-AREA                                
015496     .                                                                    
