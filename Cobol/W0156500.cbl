000100 ID DIVISION.                                                             
000200 PROGRAM-ID.      W0156500.                                               
000300 AUTHOR.          RICHARD THÖRNGREN                                       
000400 DATE-WRITTEN.    JULI 1990.                                              
000410 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION.                                                            
000900*    GENERELLT PGM FÖR TESTDATABASLADDNINGAR.                             
001000                                                                          
001100 ENVIRONMENT DIVISION.                                                    
001200 INPUT-OUTPUT SECTION.                                                    
001300 FILE-CONTROL.                                                            
001400                                                                          
001500     SELECT INDATA           ASSIGN TO      W01565D1.                     
001600                                                                          
001700 DATA DIVISION.                                                           
001800 FILE SECTION.                                                            
001900                                                                          
002000 FD  INDATA                                                               
002100     LABEL RECORD STANDARD                                                
002200     RECORDING V                                                          
002300     BLOCK CONTAINS 0                                                     
002400     RECORD VARYING FROM 1 TO 8188.                                       
002500 01  IN-POST.                                                             
002600   03  IN-KDSEGM.                                                         
002610     05  FILLER                  PIC X(4).                                
002620     05  IN-KDSEGMNR             PIC X(2).                                
002630     05  FILLER                  PIC X(2).                                
002700   03  IN-KEY                    PIC X(38).                               
002800   03  IN-SEGM                   PIC X(8142).                             
002900     EJECT                                                                
003000 WORKING-STORAGE SECTION.                                                 
003100                                                                          
003101*    -- CHECKED BY WY2000                                                 
003102                                                                          
003200 77  IDPGM                       PIC X(8)    VALUE 'W0156500'.            
003300 77  JA                          PIC X       VALUE 'J'.                   
003400 77  NEJ                         PIC X       VALUE 'N'.                   
003500 77  EOF-INPOST                  PIC X       VALUE 'N'.                   
003600 77  W-INANTAL                   PIC S9(7)   VALUE +0   COMP-3.           
003700 77  W-UTANTAL                   PIC S9(7)   VALUE +0   COMP-3.           
003710 77  W-CHKP-RAEKNARE             PIC S9(4)   VALUE +0    COMP-3.          
003730 77  CHKP-ID                     PIC X(8)    VALUE 'W0156500'.            
003740 77  MSG-IO-AREA-LENGTH          PIC S9(9)   VALUE +32  COMP SYNC.        
003750 77  MSG-IO-AREA                 PIC X(32)   VALUE SPACE.                 
003760 77  CHKP-AREA-1-LENGTH          PIC S9(9)   VALUE +32  COMP SYNC.        
003770 77  CHKP-AREA-1                 PIC X(32)   VALUE SPACE.                 
003800     SKIP3                                                                
003900 01  INAREA.                                                              
004000   03  IN-SEG-NAME               PIC X(8).                                
004100   03  IN-SEGKEY                 PIC X(38).                               
004200   03  IN-SEGMENT                PIC X(8142).                             
004300     SKIP3                                                                
004400 01  DYNAMISKA-SUBPROGRAM.                                                
004500   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
004600   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
004700     EJECT                                                                
004800 01  FILLER                      PIC X(8)   VALUE 'IMS-WS  '.             
004900     SKIP3                                                                
005000 01  STATUS-WS                   PIC XX.                                  
005100     88  IMS-EJ-OK                          VALUE 'XD'.                   
005110     88  SEGMENT-FINNS                      VALUE '  '.                   
005200     88  SEGMENT-OK                         VALUE '  ' 'GE'               
005300                                                  'II' 'IX'               
005400                                                  'NI'.                   
005500     SKIP3                                                                
005600 01  GODK-STATUSKODER.                                                    
005700   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
005800     SKIP3                                                                
005900 01  SSA1                        PIC X(64).                               
006000     SKIP3                                                                
006100 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA'.          
006200     SKIP3                                                                
006300 01  DLI-IO-AREA                 PIC X(8142).                             
006400     EJECT                                                                
006500*01      -COPY W0003.                                                     
006700     EJECT                                                                
006800 LINKAGE SECTION.                                                         
007100*    -COPY W0009  -PRE MSG-                                               
007110                                                                          
007200*    -COPY W0008  -PRE DB1-                                               
007300    05  FILLER                   PIC X(1).                                
011000     EJECT                                                                
011100 PROCEDURE DIVISION  USING MSG-PCB DB1-PCB.                               
011400 MAIN SECTION.                                                            
011410     ENTRY 'DLITCBL' USING MSG-PCB DB1-PCB.                               
011700                                                                          
011800     PERFORM A-INIT                                                       
011810     PERFORM IMS-RESTART                                                  
011900     PERFORM B-LAES-POST                                                  
012100     PERFORM UNTIL EOF-INPOST = JA                                        
012200       MOVE IN-SEG-NAME TO SSA1                                           
012300       PERFORM IMS-ISRT-DB1                                               
012400       IF SEGMENT-FINNS                                                   
012410         ADD +1 TO W-UTANTAL                                              
012420       ELSE                                                               
012500         DISPLAY W-INANTAL                                                
012510         DISPLAY IN-SEG-NAME                                              
012600         ' '     IN-SEGKEY                                                
012700       END-IF                                                             
012800       PERFORM B-LAES-POST                                                
012802       IF IN-KDSEGMNR = '01'                                              
012810         ADD +1 TO W-CHKP-RAEKNARE                                        
012820         IF W-CHKP-RAEKNARE > +999                                        
012830           PERFORM IMS-CHECKPOINT                                         
012840           MOVE +0 TO W-CHKP-RAEKNARE                                     
012850         END-IF                                                           
012860       END-IF                                                             
012900     END-PERFORM                                                          
013000     PERFORM Z-FINIT                                                      
013100                                                                          
013200     MOVE ZERO TO RETURN-CODE                                             
013300     GOBACK                                                               
013400     .                                                                    
013500     EJECT                                                                
013600 A-INIT SECTION.                                                          
013700                                                                          
013800     OPEN INPUT INDATA                                                    
013900     .                                                                    
014000     SKIP3                                                                
014100 B-LAES-POST SECTION.                                                     
014200                                                                          
014300     READ INDATA INTO INAREA                                              
014400       AT END                                                             
014500         MOVE JA TO EOF-INPOST                                            
014600       NOT AT END                                                         
014700         ADD +1 TO W-INANTAL                                              
014800     END-READ                                                             
014900     .                                                                    
015700     SKIP3                                                                
015800 Z-FINIT      SECTION.                                                    
015900                                                                          
016000     CLOSE INDATA                                                         
016100     DISPLAY ' IN = ' W-INANTAL                                           
016110     DISPLAY ' UT = ' W-UTANTAL                                           
016200     .                                                                    
016300     EJECT                                                                
016400*                            ---- IMS SEKTIONER                           
016410                                                                          
016420 IMS-RESTART SECTION.                                                     
016430     SKIP2                                                                
016440     MOVE SPACE TO MSG-IO-AREA                                            
016450     MOVE '  ' TO GODK-STATUSKODER                                        
016460     CALL CBLTDLI USING XRST MSG-PCB                                      
016470                        MSG-IO-AREA-LENGTH MSG-IO-AREA                    
016480                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
016490     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
016491     PERFORM IMS-STATUSKONTROLL                                           
016492     .                                                                    
016493     SKIP2                                                                
016494 IMS-CHECKPOINT SECTION.                                                  
016495     SKIP2                                                                
016496     MOVE CHKP-ID TO MSG-IO-AREA                                          
016497     MOVE '  XD' TO GODK-STATUSKODER                                      
016498     CALL CBLTDLI USING CHKP MSG-PCB                                      
016499                        MSG-IO-AREA-LENGTH MSG-IO-AREA                    
016500                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
016501     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
016502     PERFORM IMS-STATUSKONTROLL                                           
016503     IF IMS-EJ-OK                                                         
016504       DISPLAY 'IMS-KONTROLLREGION EJ TILLGÄNGLIG'                        
016505       CALL FELLOG                                                        
016506     END-IF                                                               
016507     .                                                                    
016508     SKIP2                                                                
016510 IMS-ISRT-DB1 SECTION.                                                    
016600                                                                          
016700     MOVE '  GENIIIIX' TO GODK-STATUSKODER                                
016800     CALL CBLTDLI USING ISRT DB1-PCB IN-SEGMENT SSA1                      
016900     MOVE DB1-STATUS-CODE TO STATUS-WS                                    
017000     PERFORM IMS-STATUSKONTROLL                                           
017100     .                                                                    
024700     SKIP2                                                                
024800 IMS-STATUSKONTROLL SECTION.                                              
024900                                                                          
025000     SET STATUS-IX TO 1                                                   
025100     SEARCH GODK-STATUS                                                   
025200       AT END                                                             
025210         CALL FELLOG                                                      
025300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
025310         CONTINUE                                                         
025400     END-SEARCH                                                           
025500     .                                                                    
