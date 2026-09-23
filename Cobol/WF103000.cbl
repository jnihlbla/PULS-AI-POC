001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     WF103000.                                                
001400 AUTHOR.         BO HAMMARIN.                                             
001500 DATE-WRITTEN.   OCTOBER 2002.                                            
001600 DATE-COMPILED.                                                           
001800                                                                          
001900*    FUNCTION:                                                            
002000*        THE PGM                                                          
002100*        - READS PRM-DATA FROM SYSIN                                      
002130*        - READS FILE WITH NEW/CHANGED         CUSTOMER RECORDS           
002160*        - SENDS THE FOLLOWING CUSTOMER DATA TO VIPS:                     
002200*          . NETHERLANDS                                                  
002300*            BY USING WZ01SEND (CARPARTS.VIPS.RECCUSTNL)                  
002400*          DEPENDING ON SYMBOLIC PRM                                      
002500*                                                                         
002600                                                                          
002800 ENVIRONMENT DIVISION.                                                    
002900                                                                          
003000 INPUT-OUTPUT SECTION.                                                    
003200 FILE-CONTROL.                                                            
003300*          --- SYSIN FROM JCL                                             
003400     SELECT INDATA                     ASSIGN TO SYSIN.                   
003401                                                                          
003410*          --- CUSTOMER-RECORDS                                           
003420     SELECT WF1018                     ASSIGN TO WF1030D1.                
003500     EJECT                                                                
003510                                                                          
003600 DATA DIVISION.                                                           
003700                                                                          
003800 FILE SECTION.                                                            
003900 FD  INDATA                                                               
004000     LABEL RECORD STANDARD                                                
004100     RECORDING  F                                                         
004110     BLOCK CONTAINS 0.                                                    
004120 01  INPOST                      PIC X(80).                               
004121                                                                          
004122 FD  WF1018                                                               
004123     RECORDING       F                                                    
004124     BLOCK CONTAINS  0.                                                   
004125                                                                          
004126*01  -COPY WF10CUS2    -L.                                                
004127     EJECT                                                                
004130                                                                          
004200 WORKING-STORAGE SECTION.                                                 
004300 77  IDPGM                       PIC X(8)    VALUE 'WF103000'.            
004590 77  SYSIN-EOF                   PIC X       VALUE 'N'.                   
004591 77  OK-SW                       PIC X       VALUE 'N'.                   
004592 77  WF1018-EOF-SW               PIC X       VALUE 'N'.                   
004593     88  END-OF-WF1018                       VALUE 'J'.                   
004594 77  FIRST-TIME-SW               PIC X       VALUE 'J'.                   
004595     88  FIRST-TIME                          VALUE 'J'.                   
004596     EJECT                                                                
004600                                                                          
004651 01  WS-TABELL.                                                           
004652     03 WS-COUNTRY-VALUES.                                                
004653       05  FILLER PIC X(2)  VALUE 'NL'.                                   
004655       05  FILLER PIC X(50) VALUE 'CARPARTS.VIPS.RECCUSTNL'.              
004656       05  FILLER PIC X(2)  VALUE SPACE.                                  
004658       05  FILLER PIC X(50) VALUE SPACE.                                  
004659                                                                          
004660     03 WS-COUNTRY-IND REDEFINES WS-COUNTRY-VALUES OCCURS 2               
004661                       INDEXED BY IX.                                     
004662       05 TAB-IDLAND  PIC X(2).                                           
004664       05 TAB-ADDRESS PIC X(50).                                          
004665     EJECT                                                                
004670                                                                          
004700 01  ERRTEXT.                                                             
004800     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
004900     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
005000 01  KDRC-DISPLAY                PIC Z(5).                                
005400     EJECT                                                                
005410                                                                          
005500 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
005600 01  FILLER REDEFINES TODAYS-DATE.                                        
005700     03  TODAYS-DATE-YEAR        PIC 9(2).                                
005800     03  TODAYS-DATE-MONTH       PIC 9(2).                                
005900     03  TODAYS-DATE-DAY         PIC 9(2).                                
006000     EJECT                                                                
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
009310                                                                          
009313 01  FILLER                      PIC X(10)   VALUE 'SYSIN '.              
009314 01  INAREA.                                                              
009315     03  PRM-IDLAND              PIC X(2).                                
009316     03  FILLER                  PIC X(78).                               
009317     EJECT                                                                
009318                                                                          
009320 01  UT00-AREA-START             PIC X(24)   VALUE                        
009330                                             'UT00-AREA-START'.           
009340 01  UT00-AREA.                                                           
009360*    03  -COPY WF1030           -PRE UT00-                                
009370*                                                                         
009380     EJECT                                                                
009400                                                                          
010100 LINKAGE SECTION.                                                         
010300*01  -COPY W0009   -PRE MSG-                                              
010700     EJECT                                                                
010800                                                                          
010801 PROCEDURE DIVISION  USING MSG-PCB.                                       
010802                                                                          
010803 MAIN SECTION.                                                            
010810     ENTRY 'DLITCBL' USING MSG-PCB.                                       
010900                                                                          
011200     PERFORM A-INIT                                                       
011201                                                                          
011210     PERFORM B-EXECUTE                                                    
011212                                                                          
012510     PERFORM Z-FINIT                                                      
012700     MOVE ZERO TO RETURN-CODE                                             
012800     GOBACK                                                               
012900     .                                                                    
013000     EJECT                                                                
013010                                                                          
013100 A-INIT SECTION.                                                          
013200     OPEN INPUT INDATA                                                    
013300     READ INDATA NEXT RECORD INTO INAREA                                  
013400       AT END MOVE 'J'              TO SYSIN-EOF                          
013500     END-READ                                                             
013600     CLOSE INDATA                                                         
013700     DISPLAY PRM-IDLAND                                                   
013800                                                                          
014300     OPEN INPUT WF1018                                                    
014310     .                                                                    
014350     EJECT                                                                
014360                                                                          
014425 B-EXECUTE SECTION.                                                       
014427     PERFORM S01-READ-WF1018                                              
014428                                                                          
014430     PERFORM UNTIL END-OF-WF1018                                          
014439       IF UT00-IDLANDX3 = PRM-IDLAND AND                                  
014440          UT00-KDPARTTY = 'EXT'      AND                                  
014441          UT00-KDPARTGR = 'DEALERS'                                       
014442         IF FIRST-TIME                                                    
014443           PERFORM S11-CUST-OPEN                                          
014444           MOVE 'N' TO FIRST-TIME-SW                                      
014445         END-IF                                                           
014446         PERFORM S12-CUST-PUT                                             
014447         PERFORM S01-READ-WF1018                                          
014448       ELSE                                                               
014449         PERFORM S01-READ-WF1018                                          
014450       END-IF                                                             
014451     END-PERFORM                                                          
014452     .                                                                    
014453     EJECT                                                                
014460                                                                          
014500 Z-FINIT SECTION.                                                         
014510     IF NOT FIRST-TIME                                                    
014600       PERFORM S13-CUST-CLOSE                                             
014601     END-IF                                                               
014610                                                                          
014700     CLOSE WF1018                                                         
015000     .                                                                    
015101     EJECT                                                                
015102                                                                          
015446 S01-READ-WF1018  SECTION.                                                
015447     READ WF1018          INTO UT00-WF1030                                
015448     AT END                                                               
015449        MOVE HIGH-VALUE   TO   UT00-WF1030                                
015450        SET END-OF-WF1018 TO   TRUE                                       
015451     END-READ                                                             
015452     .                                                                    
015453     EJECT                                                                
015454                                                                          
015535 S11-CUST-OPEN SECTION.                                                   
015543     MOVE 'N'                TO OK-SW                                     
015544     PERFORM UNTIL OK-SW = 'J' OR                                         
015545                   TAB-IDLAND(IX) = SPACE                                 
015546       IF TAB-IDLAND(IX) = PRM-IDLAND                                     
015547         MOVE 'J'            TO   OK-SW                                   
015548       END-IF                                                             
015549     END-PERFORM                                                          
015550                                                                          
015551     IF OK-SW = 'J'                                                       
015552       MOVE TAB-ADDRESS(IX)  TO SEND-ADDISPABS                            
015553       MOVE 'OPEN'           TO SEND-KDFUNC                               
015554       CALL WZ01SEND USING SEND-CONTROL-AREA                              
015555                           SEND-OPEN-AREA                                 
015556       IF SEND-KDRC > ZERO                                                
015557         MOVE SEND-KDRC      TO KDRC-DISPLAY                              
015558         STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                    
015559         DELIMITED BY SIZE INTO ERRTEXT-STR                               
015560         CALL ABEND USING RKOD-ABEND-WITH-DUMP                            
015561       END-IF                                                             
015562     ELSE                                                                 
015563       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
015564     END-IF                                                               
015565     .                                                                    
015566                                                                          
015567 S12-CUST-PUT SECTION.                                                    
015568     MOVE 'PUT'                           TO SEND-KDFUNC                  
015569     MOVE LENGTH OF UT00-AREA             TO SEND-KVDLEN                  
015570     CALL WZ01SEND USING SEND-CONTROL-AREA                                
015571                         SEND-KVDLEN                                      
015572                         UT00-AREA                                        
015573     IF SEND-KDRC > 1                                                     
015574       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
015575       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
015576       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
015577       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
015578     END-IF                                                               
015579     .                                                                    
015580                                                                          
015581 S13-CUST-CLOSE SECTION.                                                  
015590     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
015600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
015700     .                                                                    
