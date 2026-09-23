001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     WF210700.                                                
001400 AUTHOR.         ANDERS HENRIKSSON.                                       
001500 DATE-WRITTEN.   2003-06-02.                                              
001600 DATE-COMPILED.                                                           
001700                                                                          
001900*    FUNCTION:                                                            
002000*    THE PGM                                                              
002100*    - READS FILE WITH FEEDBACK DATA RECORDS                              
002200*    - SENDS FEEDBACK DATA RECORDS FOR VCCS TO:                           
002210*      WEBSHOP (VSS)     BY WZ01SEND (CARPARTS.VSS.FB)                    
002500                                                                          
002800 ENVIRONMENT DIVISION.                                                    
002900                                                                          
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003302*          --- FEEDBACK DATA RECORDS                                      
003310     SELECT WF2017                     ASSIGN TO WF2107D1.                
003520*                                                                         
003530     SELECT WF2107                     ASSIGN TO WF2107D2.                
003540     EJECT                                                                
003550                                                                          
003600 DATA DIVISION.                                                           
003700                                                                          
003800 FILE SECTION.                                                            
003902 FD  WF2017                                                               
003903     RECORDING       F                                                    
003904     BLOCK CONTAINS  0.                                                   
003905                                                                          
003910*01  -COPY WF2017      -L.                                                
004001                                                                          
004002*                                                                         
004003 FD  WF2107                                                               
004004     RECORDING       F                                                    
004005     BLOCK CONTAINS  0.                                                   
004006 01  WF2107-POST.                                                         
004007*    03  -COPY WF10VSS     -L.                                            
004008*    03  -COPY WF2107      -L.                                            
004009     EJECT                                                                
004010                                                                          
004200 WORKING-STORAGE SECTION.                                                 
004300*** - CONSTANTS                                                           
004310 77  IDPGM                       PIC X(8)    VALUE 'WF210700'.            
004501 77  WS-IDLEGSEL-VCCS            PIC X(4)    VALUE 'VCCS'.                
004502 77  WS-IDSYSTEM-REC             PIC X(4)    VALUE 'VSS'.                 
004503 77  WS-ADRESS-VSS               PIC X(50)                                
004504                                VALUE 'CARPARTS.VSS.FB'.                  
004521                                                                          
004522 77  WF2017-EOF-SW               PIC X       VALUE 'N'.                   
004530     88  END-OF-WF2017                       VALUE 'J'.                   
004600                                                                          
004601 01  WS-IDLOPNR.                                                          
004603     03  WS-KVDAGAR              PIC 9(3)    VALUE ZERO.                  
004604     03  WS-TIME                 PIC 9(8)    VALUE ZERO.                  
004605 01  WS-TIMESTAMP.                                                        
004606     03  WS-YEAR                 PIC X(4)    VALUE SPACE.                 
004607     03  FILLER                  PIC X       VALUE '-'.                   
004608     03  WS-MONTH                PIC X(2)    VALUE SPACE.                 
004609     03  FILLER                  PIC X       VALUE '-'.                   
004610     03  WS-DAY                  PIC X(2)    VALUE SPACE.                 
004611     03  FILLER                  PIC X       VALUE SPACE.                 
004612     03  WS-HOUR                 PIC X(2)    VALUE SPACE.                 
004613     03  FILLER                  PIC X       VALUE ':'.                   
004614     03  WS-MINUTE               PIC X(2)    VALUE SPACE.                 
004615     03  FILLER                  PIC X       VALUE ':'.                   
004616     03  WS-SECOND               PIC X(2)    VALUE SPACE.                 
004617     03  FILLER                  PIC X       VALUE '.'.                   
004618     03  WS-DECIMAL              PIC X(2)    VALUE SPACE.                 
004619     03  FILLER                  PIC X(4)    VALUE '0000'.                
004620 01  WS-BILLIT                   PIC X(6)    VALUE 'BILINV'.              
004621 01  WS-CURRENT-DATE             PIC X(8)    VALUE SPACE.                 
004622 01  WS-FIRST-DAY-OF-YEAR.                                                
004623     03  WS-NEW-YEAR             PIC X(4)    VALUE SPACE.                 
004624     03  FILLER                  PIC X(4)    VALUE '0101'.                
004625     EJECT                                                                
004626                                                                          
004627 01  VSS-SEND-IDCOM              PIC S9(9)   COMP VALUE +0.               
004628 01  WS-DAFINDOC                 PIC S9(8)   COMP VALUE +0.               
004629 01  WS-IDFINDOC                 PIC S9(9)   COMP VALUE +0.               
004630 01  WS-IDREF                    PIC X(15)   VALUE SPACE.                 
004640                                                                          
004700 01  ERRTEXT.                                                             
004800     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
004900     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
005000 01  KDRC-DISPLAY                PIC Z(5).                                
005400     EJECT                                                                
006010                                                                          
006100 01  GENERAL-SUBPROGRAMS.                                                 
006400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006401     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
006402     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
006403     EJECT                                                                
006410                                                                          
006420*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
006430                                                                          
006440 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006450 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006460 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006601     EJECT                                                                
006603                                                                          
006604*    --- AREOR FÖR KOMMUNIKATION                                          
006609 01  FILLER                   PIC X(16)   VALUE 'SEND-CONTROL'.           
006610*01  -COPY WZ01SEND                                                       
006620     EJECT                                                                
006640                                                                          
006670*    -COPY WZ20DAYS                                                       
006680     EJECT                                                                
006690                                                                          
006902 01  IN-AREA-START            PIC X(24)   VALUE 'IN-AREA-START'.          
006910 01  IN-AREA.                                                             
006920*    03  -COPY WF2017                                                     
009000     EJECT                                                                
009010                                                                          
009100 01  VSSUT-AREA-START             PIC X(24)   VALUE                       
009200                                              'VSSUT-AREA-START'.         
009300 01  VSSUT-AREA.                                                          
009302*    03  -COPY WF10VSS    -PRE VSSUT-                                     
009310*    03  -COPY WF2107     -PRE VSSUT-                                     
009320     EJECT                                                                
009340                                                                          
010100 LINKAGE SECTION.                                                         
010300*01  -COPY W0009   -PRE MSG-                                              
010700     EJECT                                                                
010800                                                                          
010801 PROCEDURE DIVISION  USING MSG-PCB.                                       
010802 MAIN SECTION.                                                            
010810     ENTRY 'DLITCBL' USING MSG-PCB.                                       
010900                                                                          
011200     PERFORM A-INIT                                                       
011202     PERFORM B-EXECUTE                                                    
012510     PERFORM Z-FINIT                                                      
012600                                                                          
012700     MOVE ZERO TO RETURN-CODE                                             
012800     GOBACK                                                               
012900     .                                                                    
012910     EJECT                                                                
013000                                                                          
013100 A-INIT SECTION.                                                          
013200     MOVE FUNCTION CURRENT-DATE (1:4)  TO WS-YEAR                         
013210     MOVE FUNCTION CURRENT-DATE (1:4)  TO WS-NEW-YEAR                     
013220     MOVE FUNCTION CURRENT-DATE (5:2)  TO WS-MONTH                        
013230     MOVE FUNCTION CURRENT-DATE (7:2)  TO WS-DAY                          
013240     MOVE FUNCTION CURRENT-DATE (9:2)  TO WS-HOUR                         
013250     MOVE FUNCTION CURRENT-DATE (11:2) TO WS-MINUTE                       
013260     MOVE FUNCTION CURRENT-DATE (13:2) TO WS-SECOND                       
013270     MOVE FUNCTION CURRENT-DATE (15:6) TO WS-DECIMAL                      
013280     MOVE FUNCTION CURRENT-DATE (1:8)  TO WS-CURRENT-DATE                 
013281                                                                          
013282     MOVE FUNCTION CURRENT-DATE (9:8)  TO WS-TIME                         
013290                                                                          
013291     MOVE WS-CURRENT-DATE (1:8)        TO DAYS-TIDATE2                    
013292     MOVE 'YYYYMMDD'                   TO DAYS-KDDATFMT2                  
013293     MOVE ZERO                         TO DAYS-KVDAYS                     
013294     MOVE SPACE                        TO DAYS-IDCALEND                   
013295     MOVE WS-FIRST-DAY-OF-YEAR         TO DAYS-TIDATE1                    
013296     MOVE 'YYYYMMDD'                   TO DAYS-KDDATFMT1                  
013297     CALL WZ20DAYS USING                                                  
013298          DAYS-WZ20DAYS                                                   
013299     IF DAYS-KDRC = ZERO                                                  
013300       MOVE DAYS-KVDAYS                TO WS-KVDAGAR                      
013303     END-IF                                                               
013304                                                                          
013306     MOVE WS-IDLOPNR   TO VSSUT-IDLOPNR                                   
013307     MOVE WS-TIMESTAMP TO VSSUT-TIMESTAMP                                 
013308     MOVE WS-BILLIT    TO VSSUT-BILLIT-TEXT                               
013309                                                                          
013320     OPEN INPUT  WF2017                                                   
013330     OPEN OUTPUT WF2107                                                   
014200     .                                                                    
014300     EJECT                                                                
014310                                                                          
014370 B-EXECUTE SECTION.                                                       
014372     PERFORM S01-READ-WF2017                                              
014375                                                                          
014376     IF END-OF-WF2017                                                     
014377       CONTINUE                                                           
014380     ELSE                                                                 
014392       PERFORM UNTIL END-OF-WF2017                                        
014394         IF FEED-IDLEGSEL         = WS-IDLEGSEL-VCCS                      
014399           IF FEED-IDSYSTEM-REC   = WS-IDSYSTEM-REC                       
014403             IF FEED-IDREF    NOT = WS-IDREF OR                           
014404                FEED-IDFINDOC NOT = WS-IDFINDOC                           
014405               PERFORM BA-HANDLE-VSS-FEEDBACK                             
014406               MOVE FEED-IDREF    TO WS-IDREF                             
014407               MOVE FEED-IDFINDOC TO WS-IDFINDOC                          
014408             END-IF                                                       
014409           END-IF                                                         
014410         END-IF                                                           
014411         PERFORM S01-READ-WF2017                                          
014420       END-PERFORM                                                        
014438     END-IF                                                               
014439     .                                                                    
014440                                                                          
014441 BA-HANDLE-VSS-FEEDBACK SECTION.                                          
014443     IF VSS-SEND-IDCOM = ZERO                                             
014444       PERFORM S70-OPEN-VSS                                               
014445       MOVE SEND-IDCOM TO VSS-SEND-IDCOM                                  
014446     END-IF                                                               
014447                                                                          
014459     MOVE FEED-IDREF            TO VSSUT-FB-IDREF                         
014460     MOVE FEED-IDFINDOC         TO VSSUT-FB-IDFINDOC                      
014503                                                                          
014504     PERFORM S80-PUT-VSS                                                  
014505     WRITE WF2107-POST          FROM VSSUT-AREA                           
014507     .                                                                    
014508     EJECT                                                                
014509                                                                          
014633 Z-FINIT SECTION.                                                         
014634     CLOSE WF2017                                                         
014635           WF2107                                                         
014636                                                                          
014637     IF VSS-SEND-IDCOM > ZERO                                             
014638       PERFORM S90-CLOSE-VSS                                              
014640     END-IF                                                               
015000     .                                                                    
015100     EJECT                                                                
015101                                                                          
015103 S01-READ-WF2017  SECTION.                                                
015105     READ WF2017 INTO IN-AREA                                             
015106       AT END                                                             
015108         SET END-OF-WF2017 TO TRUE                                        
015115     END-READ                                                             
015120     .                                                                    
015130     EJECT                                                                
015140                                                                          
015456 S70-OPEN-VSS SECTION.                                                    
015459     MOVE WS-ADRESS-VSS                   TO SEND-ADDISPABS               
015460     MOVE 'OPEN'                          TO SEND-KDFUNC                  
015461     CALL WZ01SEND USING SEND-CONTROL-AREA                                
015462                         SEND-OPEN-AREA                                   
015463     IF SEND-KDRC > ZERO                                                  
015464       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
015465       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
015466       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
015467       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
015468     END-IF                                                               
015469     .                                                                    
015470                                                                          
015535 S80-PUT-VSS SECTION.                                                     
015538     MOVE 'PUT'                           TO SEND-KDFUNC                  
015539     MOVE VSS-SEND-IDCOM                  TO SEND-IDCOM                   
015540     MOVE LENGTH OF VSSUT-AREA            TO SEND-KVDLEN                  
015541     CALL WZ01SEND USING SEND-CONTROL-AREA                                
015542                         SEND-KVDLEN                                      
015543                         VSSUT-AREA                                       
015544     IF SEND-KDRC > ZERO                                                  
015545       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
015546       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
015547       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
015548       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
015549     END-IF                                                               
015550     .                                                                    
015560                                                                          
015626 S90-CLOSE-VSS SECTION.                                                   
015629     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
015630     MOVE VSS-SEND-IDCOM                  TO SEND-IDCOM                   
015640     CALL WZ01SEND USING SEND-CONTROL-AREA                                
015700     .                                                                    
