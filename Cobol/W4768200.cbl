000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4768200.                                                
000400*AUTHOR.         CAMELIA OLGRENER.                                        
000500*DATE-WRITTEN.   03/05/15.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET RENSAR TULLHUVUDBASEN(WDM7) OCH TULLRADBASEN          
001100*        (WDM8) MED HJÄLP AV NYCKLAR SOM LIGGER I INFILEN(W4768A).        
001200*        INFILEN BESTÅR AV DE KONKATENERADE UTFILERNA UR PROGRAM          
001300*        W4768000, SOM GÅR ETT ANTAL GGR. PER DAG I RUTIN W476SA.         
001400*                                                                         
001500*        PROGRAMMET UPPDATERAR WDM7                                       
001600*        PROGRAMMET UPPDATERAR WDM8                                       
001700*                                                                         
001800*    ABENDKODER:                                                          
001900*        U0016 -  . . . .                                                 
002000*        U1000 -  . . . .                                                 
002100*                                                                         
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     SKIP2                                                                
003000*          --- RENSNINGSFIL                                               
003100     SELECT W4768A                     ASSIGN TO W47682D1.                
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP3                                                                
003500 FILE SECTION.                                                            
003600     SKIP3                                                                
003700 FD  W4768A                                                               
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000     SKIP2                                                                
004100*01  -COPY W476TU3      -L.                                               
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400     SKIP2                                                                
004401                                                                          
004410*    -- CHECKED BY WY2000                                                 
004500 77  IDPGM                       PIC X(8)    VALUE 'W4768200'.            
004600 01  CHKP-VAR.                                                            
004700 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
004800 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
004900 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
005000 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
005100 03  CHKP-ANT                    PIC S9(5)   VALUE +0.                    
005200 03  CHKP-MAX                    PIC S9(5)   VALUE +1500.                 
005300 77  JA                          PIC X       VALUE 'J'.                   
005400 77  NEJ                         PIC X       VALUE 'N'.                   
005500     SKIP2                                                                
005600 01  FELTEXT.                                                             
005700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005900*                                                                         
006000 77  W4768A-EOF-SW               PIC X       VALUE 'N'.                   
006100     88  END-OF-W4768A                       VALUE 'J'.                   
007100     EJECT                                                                
007200 01  DYNAMISKA-SUBPROGRAM.                                                
007300*                                                                         
007400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007700     EJECT                                                                
007800*    --- PARAMETRAR TILL POSTSUM                                          
007900*                                                                         
008000*01  -COPY W0005   -PRE  POSTSUM-                                         
008100     EJECT                                                                
008200 01  IN-AREA-START               PIC X(24)   VALUE                        
008300                                             'IN-AREA-START'.             
008400     SKIP2                                                                
008500                                                                          
008600*01  AREA -COPY W476TU3     -PRE IN-                                      
008700*                                                                         
008800     EJECT                                                                
008900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009000     SKIP3                                                                
009100 01  NYCKLAR-TILL-DLI.                                                    
009200     03  W-WDM701KY-X.                                                    
009300         05  W-M7-IDFAKT         PIC S9(7)    VALUE ZERO COMP-3.          
009400         05  W-M7-IDORDNR7       PIC S9(7)    VALUE ZERO COMP-3.          
009500         05  W-M7-IDKOLLI        PIC S9(5)    VALUE ZERO COMP-3.          
009510         05  W-M7-IDPRODNR       PIC S9(7)    VALUE ZERO COMP-3.          
009600*                                                                         
009700     03  W-WDM801KY-MIN-X.                                                
009800         05  W-M8-IDFAKT-MIN     PIC S9(7)    VALUE ZERO COMP-3.          
009900         05  W-M8-IDPRODNR-MIN   PIC S9(7)    VALUE ZERO COMP-3.          
010000         05  W-M8-IDKOLLI-MIN    PIC S9(5)    VALUE ZERO COMP-3.          
010100         05  FILLER              PIC X(8)     VALUE LOW-VALUE.            
010300*                                                                         
010400     03  W-WDM801KY-MAX-X.                                                
010500         05  W-M8-IDFAKT-MAX     PIC S9(7)    VALUE ZERO COMP-3.          
010600         05  W-M8-IDPRODNR-MAX   PIC S9(7)    VALUE ZERO COMP-3.          
010700         05  W-M8-IDKOLLI-MAX    PIC S9(5)    VALUE ZERO COMP-3.          
010710         05  FILLER              PIC X(8)     VALUE HIGH-VALUE.           
011000     SKIP2                                                                
011100*    --- STATUS-KOD FRÅN IMS                                              
011200 01  STATUS-WS                   PIC XX.                                  
011300     88  SEGMENT-FINNS                       VALUE '  '.                  
011400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
011700     88  IMS-EJ-OK                           VALUE 'XD'.                  
011800     SKIP2                                                                
011900 01  GODK-STATUSKODER.                                                    
012000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012100     SKIP3                                                                
012200 01  SSA1                        PIC X(192).                              
012400     EJECT                                                                
012500*    --- IMS FUNKTIONSKODER                                               
012600*01  -COPY W0003                                                          
012700     EJECT                                                                
012800*    ---  DLI INPUT-OUTPUT AREA                                           
012900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
013000 01  DLI-IO-AREA.                                                         
013100     03  IO-AREA                 PIC X(200)  VALUE SPACE.                 
013200     SKIP3                                                                
013300     03  WDM701 REDEFINES IO-AREA.                                        
013400*        05  -COPY WDM701                                                 
013500     EJECT                                                                
014200     03  WDM801 REDEFINES IO-AREA.                                        
014300*        05  -COPY WDM801                                                 
014400     EJECT                                                                
014500 LINKAGE SECTION.                                                         
014600                                                                          
014700*01  -COPY W0009   -PRE MSG-                                              
014800     EJECT                                                                
014900*01  -COPY W0008  -PRE WDM7-                                              
015000     05  FILLER                  PIC X.                                   
015100     EJECT                                                                
015200*01  -COPY W0008  -PRE WDM8-                                              
015300     05  FILLER                  PIC X.                                   
015400     EJECT                                                                
015500 PROCEDURE DIVISION  USING MSG-PCB WDM7-PCB WDM8-PCB.                     
015600     ENTRY 'DLITCBL' USING MSG-PCB WDM7-PCB WDM8-PCB.                     
015700                                                                          
015800     PERFORM A-INIT                                                       
015900     PERFORM S01-LAES-W4768A                                              
016000                                                                          
016100     PERFORM UNTIL END-OF-W4768A                                          
016310                                                                          
016320       PERFORM IMS-GHU-WDM801                                             
016400       PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                       
016500         IF CHKP-ANT > CHKP-MAX                                           
016600           PERFORM X-TAG-CHECKPOINT                                       
016700         END-IF                                                           
016800                                                                          
016900         PERFORM B-TAG-BORT-WDM8-SEG                                      
016910         PERFORM IMS-GHN-WDM801                                           
017000       END-PERFORM                                                        
017100                                                                          
017200       PERFORM C-TAG-BORT-WDM7-SEG                                        
017300                                                                          
017400       PERFORM S01-LAES-W4768A                                            
017500     END-PERFORM                                                          
017600                                                                          
017700     PERFORM Z-FINIT                                                      
017800                                                                          
017900     MOVE ZERO TO RETURN-CODE                                             
018000     GOBACK                                                               
018100     .                                                                    
018200     EJECT                                                                
018300 A-INIT SECTION.                                                          
018400     SKIP2                                                                
018500                                                                          
018600     PERFORM IMS-RESTART                                                  
018700                                                                          
018800     OPEN INPUT W4768A                                                    
018900                                                                          
019000     MOVE IDPGM                TO POSTSUM-PROGNAMN                        
019100     MOVE +0                   TO CHKP-ANT                                
019500     .                                                                    
019600     EJECT                                                                
021500 B-TAG-BORT-WDM8-SEG SECTION.                                             
021696                                                                          
021900     ADD +1                    TO CHKP-ANT                                
022000     PERFORM IMS-DLET-WDM801                                              
022200     .                                                                    
022300     EJECT                                                                
022400 C-TAG-BORT-WDM7-SEG           SECTION.                                   
022420                                                                          
022430     PERFORM IMS-GHU-WDM701                                               
022440     IF SEGMENT-FINNS                                                     
022460       ADD +1                  TO CHKP-ANT                                
022470       PERFORM IMS-DLET-WDM7                                              
022490     END-IF                                                               
022500                                                                          
023100     .                                                                    
023200     EJECT                                                                
023210 X-TAG-CHECKPOINT   SECTION.                                              
023220                                                                          
023250     PERFORM IMS-CHECKPOINT                                               
023260     MOVE ZERO                 TO CHKP-ANT                                
023261                                                                          
023270     PERFORM IMS-GHU-WDM801                                               
023280     .                                                                    
023290     EJECT                                                                
023300 Z-FINIT SECTION.                                                         
023400                                                                          
023500     CLOSE W4768A                                                         
023600     SKIP2                                                                
023700     MOVE 'S' TO POSTSUM-OPKOD                                            
023800     CALL POSTSUM USING POSTSUM-PARM                                      
023900     .                                                                    
024000     EJECT                                                                
024100 S01-LAES-W4768A  SECTION.                                                
024200     SKIP2                                                                
024300     READ W4768A INTO IN-AREA                                             
024400     AT END                                                               
024600        SET END-OF-W4768A TO TRUE                                         
024700                                                                          
024800     NOT AT END                                                           
024900        MOVE 'W4768A' TO POSTSUM-FDNAMN                                   
025000        MOVE 'W47682D1' TO POSTSUM-DDNAMN2                                
025100        MOVE IN-TU3-IDPTYP TO POSTSUM-TRANSTYP                            
025101        MOVE IN-TU3-IDFAKT        TO W-M7-IDFAKT                          
025102                                     W-M8-IDFAKT-MIN                      
025103                                     W-M8-IDFAKT-MAX                      
025104        MOVE IN-TU3-IDORDNR7      TO W-M7-IDORDNR7                        
025105        MOVE IN-TU3-IDKOLLI       TO W-M7-IDKOLLI                         
025106                                     W-M8-IDKOLLI-MIN                     
025107                                     W-M8-IDKOLLI-MAX                     
025108        MOVE IN-TU3-IDPRODNR      TO W-M7-IDPRODNR                        
025109                                     W-M8-IDPRODNR-MIN                    
025110                                     W-M8-IDPRODNR-MAX                    
025200        CALL POSTSUM USING POSTSUM-PARM                                   
025500     END-READ                                                             
025600     .                                                                    
025700     EJECT                                                                
026700* --- IMS SEKTIONER ---                                                   
026800     SKIP3                                                                
026900     EJECT                                                                
027000 IMS-GHU-WDM701 SECTION.                                                  
027100     STRING 'WDM701  (WDM701KY =' W-WDM701KY-X ')'                        
027200          DELIMITED BY SIZE INTO SSA1                                     
027300     MOVE '  GE' TO GODK-STATUSKODER                                      
027400     CALL CBLTDLI USING GHU WDM7-PCB DLI-IO-AREA SSA1                     
027500     MOVE WDM7-STATUS-CODE TO STATUS-WS                                   
027600     PERFORM IMS-STATUSKONTROLL                                           
027700     .                                                                    
027800     SKIP3                                                                
027900 IMS-DLET-WDM7 SECTION.                                                   
028000                                                                          
028100     MOVE '  ' TO GODK-STATUSKODER                                        
028200     CALL CBLTDLI USING DLET WDM7-PCB DLI-IO-AREA                         
028300     MOVE WDM7-STATUS-CODE TO STATUS-WS                                   
028400     PERFORM IMS-STATUSKONTROLL                                           
028500     .                                                                    
028600     EJECT                                                                
028700 IMS-GHU-WDM801 SECTION.                                                  
028800     STRING 'WDM801  (WDM801KY >' W-WDM801KY-MIN-X                        
028900                    '&WDM801KY <' W-WDM801KY-MAX-X ')'                    
029000          DELIMITED BY SIZE INTO SSA1                                     
029100     MOVE '  GE' TO GODK-STATUSKODER                                      
029200     CALL CBLTDLI USING GHU WDM8-PCB DLI-IO-AREA SSA1                     
029300     MOVE WDM8-STATUS-CODE TO STATUS-WS                                   
029400     PERFORM IMS-STATUSKONTROLL                                           
029500     .                                                                    
029600     SKIP3                                                                
029610 IMS-GHN-WDM801 SECTION.                                                  
029620     STRING 'WDM801  (WDM801KY >' W-WDM801KY-MIN-X                        
029630                    '&WDM801KY <' W-WDM801KY-MAX-X ')'                    
029640          DELIMITED BY SIZE INTO SSA1                                     
029650     MOVE '  GEGB' TO GODK-STATUSKODER                                    
029660     CALL CBLTDLI USING GHN WDM8-PCB DLI-IO-AREA SSA1                     
029670     MOVE WDM8-STATUS-CODE TO STATUS-WS                                   
029680     PERFORM IMS-STATUSKONTROLL                                           
029690     .                                                                    
029691     SKIP3                                                                
029700 IMS-DLET-WDM801 SECTION.                                                 
029800                                                                          
029900     MOVE '  ' TO GODK-STATUSKODER                                        
030000     CALL CBLTDLI USING DLET WDM8-PCB DLI-IO-AREA                         
030100     MOVE WDM8-STATUS-CODE TO STATUS-WS                                   
030200     PERFORM IMS-STATUSKONTROLL                                           
030300     .                                                                    
030400     EJECT                                                                
030500 IMS-RESTART SECTION.                                                     
030600     SKIP2                                                                
030700     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
030800     MOVE '  ' TO GODK-STATUSKODER                                        
030900     CALL CBLTDLI USING XRST MSG-PCB                                      
031000                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
031100                        CHKP-AREA-LENGTH CHKP-AREA                        
031200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031300     PERFORM IMS-STATUSKONTROLL                                           
031400     .                                                                    
031500     EJECT                                                                
031600 IMS-CHECKPOINT SECTION.                                                  
031700     SKIP2                                                                
031800     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
031900     MOVE '  XD' TO GODK-STATUSKODER                                      
032000     CALL CBLTDLI USING CHKP MSG-PCB                                      
032100                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
032200                        CHKP-AREA-LENGTH CHKP-AREA                        
032300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
032400     PERFORM IMS-STATUSKONTROLL                                           
032500                                                                          
032600     IF IMS-EJ-OK                                                         
032700       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
032800       DISPLAY FELTEXT                                                    
032900       CALL FELLOG                                                        
033000     END-IF                                                               
033100     .                                                                    
033200     EJECT                                                                
033300 IMS-STATUSKONTROLL SECTION.                                              
033400     SKIP2                                                                
033500     SET STATUS-IX TO 1                                                   
033600     SEARCH GODK-STATUS                                                   
033700       AT END                                                             
033800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
033801         DELIMITED BY SIZE INTO FELTEXT-STR                               
033900         DISPLAY FELTEXT                                                  
034000         CALL FELLOG                                                      
034100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
034200         CONTINUE                                                         
034300     END-SEARCH                                                           
034400     .                                                                    
