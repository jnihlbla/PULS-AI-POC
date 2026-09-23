000100 ID DIVISION.                                                             
000300 PROGRAM-ID.     W0153700.                                                
000400 AUTHOR.         LASSI OLGRENER.                                          
000500 DATE-WRITTEN.   02/07/05.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNKTION:                                                            
001000*        RENSNING AV HÄNDELSE-TRANSAR                                     
001100*        GENERELLT PROGRAM FÖR ATT TA BORT TRANSAKTIONER FRÅN EN          
001200*        FIL-DATABAS (WDR7)                                               
001300*        PROGRAMMET LÄSER EN FIL MED NYCKLAR FÖR DE TRANSAR SOM           
001400*        SKA TAS BORT, OCH SOM SKAPATS AV ETT TIDIGARE PROGRAM            
001500*        SOM LÄST UT TRANSARNA TILL EN FIL.                               
001600*                                                                         
002000*        PROGRAMMET UPPDATERAR WDR7                                       
002100*                                                                         
002500                                                                          
002700 ENVIRONMENT DIVISION.                                                    
002800                                                                          
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003200                                                                          
003300*          --- FIL MED TRANSAR SOM SKA RENSAS                             
003400     SELECT W01537                     ASSIGN TO W01537D1.                
003500                                                                          
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000                                                                          
004100 FILE SECTION.                                                            
004200                                                                          
004300 FD  W01537                                                               
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600                                                                          
004700*01  -COPY WDR701        -L.                                              
004800                                                                          
005500     EJECT                                                                
005600 WORKING-STORAGE SECTION.                                                 
005701                                                                          
005710*    -- CHECKED BY WY2000                                                 
005720                                                                          
005800 77  IDPGM                       PIC X(8)    VALUE 'W0153700'.            
005900 01  CHKP-VAR.                                                            
006000     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
006100     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
006200     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
006300     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
006400     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
006500     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
006600 77  JA                          PIC X       VALUE 'J'.                   
006700 77  NEJ                         PIC X       VALUE 'N'.                   
006710 77  W-ANT-IN                    PIC S9(7)   VALUE ZERO COMP-3.           
006720 77  W-ANT-DEL                   PIC S9(7)   VALUE ZERO COMP-3.           
006800                                                                          
006810                                                                          
006900 01  FELTEXT.                                                             
007000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007200                                                                          
007300 77  W01537-EOF-SW               PIC X       VALUE 'N'.                   
007400     88  END-OF-W01537                       VALUE 'J'.                   
007410                                                                          
007500                                                                          
007600 01  DYNAMISKA-SUBPROGRAM.                                                
007800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008200                                                                          
009000     EJECT                                                                
009100 01  RENS-AREA-START             PIC X(16)   VALUE                        
009200                                             'RENS-AREA-START'.           
009300                                                                          
009400*01  AREA -COPY WDR701  -L   -PRE RENS-                                   
009500*                                                                         
009900                                                                          
010300     EJECT                                                                
010400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010500                                                                          
010600 01  NYCKLAR-TILL-DLI.                                                    
010700     03  W-WDR701KY-X.                                                    
010800         05  W-WDR701KY          PIC X(27)    VALUE SPACE.                
010900                                                                          
010910                                                                          
011000*    --- STATUS-KOD FRÅN IMS                                              
011100 01  STATUS-WS                   PIC XX.                                  
011200     88  SEGMENT-FINNS                       VALUE '  '.                  
011600     88  IMS-EJ-OK                           VALUE 'XD'.                  
011700                                                                          
011710                                                                          
011800 01  GODK-STATUSKODER.                                                    
011900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012000                                                                          
012010                                                                          
012100 01  SSA1                        PIC X(64).                               
012300     EJECT                                                                
012400*    --- IMS FUNKTIONSKODER                                               
012500*01  -COPY W0003                                                          
012600     EJECT                                                                
012700*    ---  DLI INPUT-OUTPUT AREA                                           
012800                                                                          
012900 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDR701'.                      
013000 01  DLI-IO-WDR701.                                                       
013100*    03  -COPY WDR701                                                     
013300                                                                          
013400     EJECT                                                                
013500 LINKAGE SECTION.                                                         
013700*01  -COPY W0009  -PRE MSG-                                               
013800                                                                          
013900*01  -COPY W0008  -PRE WDR7-                                              
014000     05  FILLER                  PIC X.                                   
014100     EJECT                                                                
014200 PROCEDURE DIVISION  USING MSG-PCB WDR7-PCB.                              
014300 MAIN SECTION.                                                            
014400     ENTRY 'DLITCBL' USING MSG-PCB WDR7-PCB.                              
014500                                                                          
014700     PERFORM A-INIT                                                       
014800     PERFORM S01-LAES-W01537                                              
014900     PERFORM UNTIL END-OF-W01537                                          
015000       IF CHKP-ANT > CHKP-MAX                                             
015100         PERFORM X-TAG-CHECKPOINT                                         
015200       END-IF                                                             
015300       PERFORM B-LAES-RENSA-TRANS                                         
015400                                                                          
015500       PERFORM S01-LAES-W01537                                            
015600     END-PERFORM                                                          
015700                                                                          
015800                                                                          
015900     PERFORM Z-FINIT                                                      
016000                                                                          
016100     MOVE ZERO TO RETURN-CODE                                             
016200     GOBACK                                                               
016300     .                                                                    
016400     EJECT                                                                
016500 A-INIT SECTION.                                                          
016600                                                                          
016700     PERFORM IMS-RESTART                                                  
016800                                                                          
016900     OPEN INPUT W01537                                                    
018500     .                                                                    
018600                                                                          
018610                                                                          
018620                                                                          
018700 B-LAES-RENSA-TRANS  SECTION.                                             
018800                                                                          
018900     MOVE RENS-AREA TO W-WDR701KY                                         
019000     PERFORM IMS-GHU-WDR701                                               
019100     IF SEGMENT-FINNS                                                     
019200       PERFORM IMS-DLET-WDR701                                            
019300       ADD 1 TO CHKP-ANT                                                  
019310                W-ANT-DEL                                                 
019900     END-IF                                                               
020000     .                                                                    
020100                                                                          
020110                                                                          
020200 Z-FINIT SECTION.                                                         
020300                                                                          
020400     CLOSE W01537                                                         
020500                                                                          
020600     DISPLAY 'ANTAL LÄSTA POSTER = ' W-ANT-IN                             
020610     DISPLAY 'ANTAL DELETE       = ' W-ANT-DEL                            
020800     .                                                                    
020900     EJECT                                                                
021000 S01-LAES-W01537  SECTION.                                                
021100                                                                          
021200     READ W01537 INTO RENS-AREA                                           
021300     AT END                                                               
021400        MOVE HIGH-VALUE TO RENS-AREA                                      
021500        MOVE JA TO W01537-EOF-SW                                          
021700     NOT AT END                                                           
021800        ADD +1 TO W-ANT-IN                                                
022400     END-READ                                                             
022500     .                                                                    
022510                                                                          
022520                                                                          
022600                                                                          
022700 X-TAG-CHECKPOINT   SECTION.                                              
022800                                                                          
022900     PERFORM IMS-CHECKPOINT                                               
023000     MOVE ZERO TO CHKP-ANT                                                
023100     .                                                                    
023200     EJECT                                                                
023300* --- IMS SEKTIONER ---                                                   
023400                                                                          
023500 IMS-GHU-WDR701 SECTION.                                                  
023600                                                                          
023700     STRING 'WDR701  (WDR701KY =' W-WDR701KY-X ')'                        
023800          DELIMITED BY SIZE INTO SSA1                                     
023900     MOVE '  GE' TO GODK-STATUSKODER                                      
024000     CALL CBLTDLI USING GHU WDR7-PCB DLI-IO-WDR701 SSA1                   
024100     MOVE WDR7-STATUS-CODE TO STATUS-WS                                   
024200     PERFORM IMS-STATUSKONTROLL                                           
024300     .                                                                    
024310                                                                          
024400                                                                          
024500 IMS-DLET-WDR701 SECTION.                                                 
024600                                                                          
024700     MOVE '  ' TO GODK-STATUSKODER                                        
024800     CALL CBLTDLI USING DLET WDR7-PCB DLI-IO-WDR701                       
024900     MOVE WDR7-STATUS-CODE TO STATUS-WS                                   
025000     PERFORM IMS-STATUSKONTROLL                                           
025100     .                                                                    
025200                                                                          
025201     EJECT                                                                
025300 IMS-RESTART SECTION.                                                     
025400                                                                          
025500     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
025600     MOVE '  ' TO GODK-STATUSKODER                                        
025700     CALL CBLTDLI USING XRST MSG-PCB                                      
025800                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
025900                        CHKP-AREA-LENGTH CHKP-AREA                        
026000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
026100     PERFORM IMS-STATUSKONTROLL                                           
026200     .                                                                    
026300                                                                          
026310                                                                          
026400 IMS-CHECKPOINT SECTION.                                                  
026500                                                                          
026600     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
026700     MOVE '  XD' TO GODK-STATUSKODER                                      
026800     CALL CBLTDLI USING CHKP MSG-PCB                                      
026900                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
027000                        CHKP-AREA-LENGTH CHKP-AREA                        
027100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
027200     PERFORM IMS-STATUSKONTROLL                                           
027300                                                                          
027400     IF IMS-EJ-OK                                                         
027500       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
027600       DISPLAY FELTEXT                                                    
027700       CALL FELLOG                                                        
027800     END-IF                                                               
027900     .                                                                    
028000     EJECT                                                                
028100 IMS-STATUSKONTROLL SECTION.                                              
028200                                                                          
028300     SET STATUS-IX TO 1                                                   
028400     SEARCH GODK-STATUS                                                   
028500       AT END                                                             
028600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
028700           DELIMITED BY SIZE INTO FELTEXT                                 
028800         DISPLAY FELTEXT                                                  
028900         CALL FELLOG                                                      
029000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
029100         CONTINUE                                                         
029200     END-SEARCH                                                           
029300     .                                                                    
