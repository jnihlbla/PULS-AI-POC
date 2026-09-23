001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     WF211000.                                                
001400 AUTHOR.         BERNT LUNDH.                                             
001500 DATE-WRITTEN.   OCTOBER 2002.                                            
001600 DATE-COMPILED.                                                           
001700                                                                          
001900*    FUNCTION:                                                            
002000*    THE PGM                                                              
002010*    - READS PRM-DATA FROM SYSIN                                          
002100*    - READS FILE WITH FEEDBACK DATA RECORDS                              
002200*    - SENDS FEEDBACK DATA RECORDS FOR VCCS TO:                           
002233*      . NETHERLANDS                                                      
002234*        BY USING WZ01SEND (CARPARTS.VIPS.FBNL)                           
002235*      DEPENDING ON SYMBOLIC PRM                                          
002500                                                                          
002800 ENVIRONMENT DIVISION.                                                    
002900                                                                          
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003300*          --- SYSIN FROM JCL                                             
003301     SELECT INDATA                     ASSIGN TO SYSIN.                   
003302                                                                          
003303*          --- FEEDBACK DATA RECORDS                                      
003310     SELECT WF2017                     ASSIGN TO WF2110D1.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700                                                                          
003800 FILE SECTION.                                                            
003900 FD  INDATA                                                               
003901     LABEL RECORD STANDARD                                                
003902     RECORDING  F                                                         
003903     BLOCK CONTAINS 0.                                                    
003904 01  INPOST                      PIC X(80).                               
003905                                                                          
003906 FD  WF2017                                                               
003907     RECORDING       F                                                    
003908     BLOCK CONTAINS  0.                                                   
003909                                                                          
003910*01  -COPY WF2017      -L.                                                
004000     EJECT                                                                
004010                                                                          
004200 WORKING-STORAGE SECTION.                                                 
004300*** - CONSTANTS                                                           
004310 77  IDPGM                       PIC X(8)    VALUE 'WF211000'.            
004311 77  SYSIN-EOF                   PIC X       VALUE 'N'.                   
004320 77  YES                         PIC X       VALUE 'J'.                   
004330 77  NOO                         PIC X       VALUE 'N'.                   
004501 77  WS-IDLEGSEL-VCCS            PIC X(4)    VALUE 'VCCS'.                
004516 77  WS-VIPS                     PIC X(4)    VALUE 'VIPS'.                
004522 77  OK-SW                       PIC X       VALUE 'N'.                   
004523 77  FIRST-TIME-SW               PIC X       VALUE 'J'.                   
004540 77  WF2017-EOF-SW               PIC X       VALUE 'N'.                   
004550     88  END-OF-WF2017                       VALUE 'J'.                   
004560     EJECT                                                                
004600                                                                          
004610 01  WS-TABELL.                                                           
004620     03 WS-COUNTRY-VALUES.                                                
004630       05  FILLER PIC X(2)  VALUE 'NL'.                                   
004640       05  FILLER PIC X(50) VALUE 'CARPARTS.VIPS.FBNL'.                   
004650       05  FILLER PIC X(2)  VALUE SPACE.                                  
004660       05  FILLER PIC X(50) VALUE SPACE.                                  
004670                                                                          
004680     03 WS-COUNTRY-IND REDEFINES WS-COUNTRY-VALUES OCCURS 2               
004690                       INDEXED BY IX.                                     
004691       05 TAB-IDLAND  PIC X(2).                                           
004692       05 TAB-ADDRESS PIC X(50).                                          
004693     EJECT                                                                
004694                                                                          
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
006640                                                                          
006650 01  FILLER                      PIC X(10)   VALUE 'SYSIN '.              
006660 01  INAREA.                                                              
006670     03  PRM-IDLAND              PIC X(2).                                
006680     03  FILLER                  PIC X(78).                               
006690     EJECT                                                                
006700                                                                          
006902 01  IN-AREA-START            PIC X(24)   VALUE 'IN-AREA-START'.          
006910 01  IN-AREA.                                                             
006920*    03  -COPY WF2017                                                     
009000     EJECT                                                                
009010                                                                          
009329 01  VIPSUT-AREA-START            PIC X(24)   VALUE                       
009330                                              'VIPSUT-AREA-START'.        
009331 01  VIPSUT-AREA.                                                         
009332*    03  -COPY WZ01REQU         -PRE VIPSUT-                              
009333*    03  -COPY WF2110           -PRE VIPSUT-                              
009334     EJECT                                                                
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
013100 A-INIT SECTION.                                                          
013200     OPEN INPUT INDATA                                                    
013300     READ INDATA NEXT RECORD INTO INAREA                                  
013301       AT END MOVE YES              TO SYSIN-EOF                          
013302     END-READ                                                             
013303     CLOSE INDATA                                                         
013304     DISPLAY PRM-IDLAND                                                   
013305                                                                          
013306     MOVE 1                TO VIPSUT-REQU-IDMSGVER                        
013307     MOVE SPACE            TO VIPSUT-REQU-KDPGMACT                        
013308     MOVE IDPGM            TO VIPSUT-REQU-IDUSER                          
013309                                                                          
013310     OPEN INPUT WF2017                                                    
014200     .                                                                    
014300     EJECT                                                                
014310                                                                          
014370 B-EXECUTE SECTION.                                                       
014372     PERFORM S01-READ-WF2017                                              
014374                                                                          
014375     IF END-OF-WF2017                                                     
014377       CONTINUE                                                           
014380     ELSE                                                                 
014392       PERFORM UNTIL END-OF-WF2017                                        
014394         IF FEED-IDLEGSEL = WS-IDLEGSEL-VCCS                              
014403           IF FEED-IDSYSTEM-REC = WS-VIPS                                 
014404             PERFORM BA-HANDLE-VIPS                                       
014405           END-IF                                                         
014408         END-IF                                                           
014409         PERFORM S01-READ-WF2017                                          
014410       END-PERFORM                                                        
014438     END-IF                                                               
014439     .                                                                    
014440                                                                          
014524 BA-HANDLE-VIPS SECTION.                                                  
014526     MOVE FEED-IDPARTNR         TO VIPSUT-IDPARTNR                        
014527     MOVE FEED-KDVALISO         TO VIPSUT-KDVALISO                        
014528     MOVE FEED-PRKURS           TO VIPSUT-PRKURS                          
014529     MOVE FEED-DAFINDOC         TO VIPSUT-DAFINDOC                        
014530     MOVE FEED-IDFINDOC         TO VIPSUT-IDFINDOC                        
014531     MOVE FEED-KDBETALV         TO VIPSUT-KDBETALV                        
014532     MOVE FEED-SUNTO-TOT        TO VIPSUT-SUNTO-TOT                       
014533     MOVE FEED-SUBTO-TOT        TO VIPSUT-SUBTO-TOT                       
014534     MOVE FEED-SUVAT-BILLIT-TOT TO VIPSUT-SUVAT-BILLIT-TOT                
014535     MOVE FEED-IDEXCUST(2)      TO VIPSUT-IDEXCUST                        
014536     MOVE FEED-DAREFDAT         TO VIPSUT-DAREFDAT                        
014537     MOVE FEED-IDREF            TO VIPSUT-IDREF                           
014538     MOVE FEED-IDREFRAD         TO VIPSUT-IDREFRAD                        
014539     MOVE FEED-KDVAT            TO VIPSUT-KDVAT                           
014540     MOVE FEED-IDARTNR-FINANCE  TO VIPSUT-IDARTNR-FINANCE                 
014541     MOVE FEED-KVLEVART         TO VIPSUT-KVLEVART                        
014542     MOVE FEED-PRARTBTO         TO VIPSUT-PRARTBTO                        
014543     MOVE FEED-PRARTNTO         TO VIPSUT-PRARTNTO                        
014544     MOVE FEED-SUNTO            TO VIPSUT-SUNTO                           
014545     MOVE FEED-SUBTO            TO VIPSUT-SUBTO                           
014546     MOVE FEED-SUVAT-BILLIT     TO VIPSUT-SUVAT-BILLIT                    
014547                                                                          
014548     IF FEED-IDLANDX3-SEND = PRM-IDLAND                                   
014549       IF FIRST-TIME-SW = YES                                             
014550         MOVE NOO        TO FIRST-TIME-SW                                 
014551         PERFORM S72-OPEN-VIPS                                            
014553       END-IF                                                             
014554       PERFORM S82-PUT-VIPS                                               
014555     END-IF                                                               
014578     .                                                                    
014580     EJECT                                                                
014581                                                                          
014590 Z-FINIT SECTION.                                                         
014600     IF FIRST-TIME-SW = NOO                                               
014616       PERFORM S92-CLOSE-VIPS                                             
014617     END-IF                                                               
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
015483 S72-OPEN-VIPS SECTION.                                                   
015486     MOVE NOO                TO OK-SW                                     
015487     SET IX TO 1                                                          
015488     SEARCH WS-COUNTRY-IND                                                
015489       AT END                                                             
015490         MOVE NOO                TO OK-SW                                 
015491       WHEN TAB-IDLAND(IX) = PRM-IDLAND                                   
015492         MOVE YES            TO   OK-SW                                   
015493     END-SEARCH                                                           
015494*    PERFORM UNTIL OK-SW = YES OR                                         
015495*                  TAB-IDLAND(IX) = SPACE                                 
015496*      IF TAB-IDLAND(IX) = PRM-IDLAND                                     
015497*        MOVE YES            TO   OK-SW                                   
015498*      END-IF                                                             
015499*    END-PERFORM                                                          
015500                                                                          
015501     IF OK-SW = YES                                                       
015502       MOVE TAB-ADDRESS(IX)                 TO SEND-ADDISPABS             
015503       MOVE 'OPEN'                          TO SEND-KDFUNC                
015504                                                                          
015505       CALL WZ01SEND USING SEND-CONTROL-AREA                              
015506                           SEND-OPEN-AREA                                 
015507                                                                          
015508       IF SEND-KDRC > ZERO                                                
015509         MOVE SEND-KDRC                     TO KDRC-DISPLAY               
015510         STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                    
015511         DELIMITED BY SIZE INTO ERRTEXT-STR                               
015512         CALL ABEND USING RKOD-ABEND-WITH-DUMP                            
015513       END-IF                                                             
015514     ELSE                                                                 
015515       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
015516     END-IF                                                               
015517     .                                                                    
015520                                                                          
015566 S82-PUT-VIPS SECTION.                                                    
015568     MOVE 'PUT'                           TO SEND-KDFUNC                  
015570     MOVE LENGTH OF VIPSUT-AREA           TO SEND-KVDLEN                  
015571                                                                          
015572     CALL WZ01SEND USING SEND-CONTROL-AREA                                
015573                         SEND-KVDLEN                                      
015574                         VIPSUT-AREA                                      
015575                                                                          
015576     IF SEND-KDRC > ZERO                                                  
015577       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
015578       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
015579       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
015580       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
015581     END-IF                                                               
015582     .                                                                    
015590                                                                          
016400 S92-CLOSE-VIPS SECTION.                                                  
016600     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
016710                                                                          
016800     CALL WZ01SEND USING SEND-CONTROL-AREA                                
016900     .                                                                    
